module TinyML.TypeInferencing

open Ast
open Utilities
open TypeInferencingUtils

let gamma0: scheme env = []

let (++) = compose_subst

let rec typeinfer_expr (env: scheme env) (e: expr) : ty * subst =
    match e with
    | Lit (LInt _) -> TyInt, []
    | Lit (LBool _) -> TyBool, []
    | Lit (LFloat _) -> TyFloat, []
    | Lit (LString _) -> TyString, []
    | Lit (LChar _) -> TyChar, []
    | Lit LUnit -> TyUnit, []

    | Var x ->
        try

            let _, Forall (tv, ty) = List.find (fun (y, _) -> x = y) env
            ty, []
        with
        | _ -> throw_formatted UndefinedError "The value or constructor '%s' is not defined." x

    | Let (x, tyo, e1, e2) ->
        let t1, s1 = typeinfer_expr env e1
        let gamma1 = apply_subst_env env s1
        let sigma = generalize gamma1 t1
        let gamma2 = (x, sigma) :: gamma1
        let t2, s2 = typeinfer_expr gamma2 e2
        let s3 = s2 ++ s1

        match tyo with
        | Some ty when t2 <> ty ->
            type_error "Type annotation %s is incompatible with type %s" (pretty_ty ty) (pretty_ty t2)
        | _ -> t2, s3

    | IfThenElse (e1, e2, e3o) ->
        let t1, s1 = typeinfer_expr env e1
        let s2 = unify t1 TyBool

        let s3 = s2 ++ s1
        let gamma1 = apply_subst_env env s3

        let t2, s4 = typeinfer_expr gamma1 e2
        let s5 = s4 ++ s3
        let gamma2 = apply_subst_env gamma1 s5

        if e3o.IsSome then
            let t3, s6 = typeinfer_expr gamma2 e3o.Value
            let s7 = s6 ++ s5
            let s8 = unify (apply_subst t2 s7) (apply_subst t3 s7)
            apply_subst t2 s8, s8 ++ s7
        else
            apply_subst t2 s5, s5


    | Lambda (x, tyo, e) ->
        let alpha = fresh_tyvar ()
        let scheme = Forall(Set.empty, alpha)
        let t2, s1 = typeinfer_expr ((x, scheme) :: env) e
        let t1 = apply_subst alpha s1
        let res = TyArrow(t1, t2), s1

        match tyo with
        | Some ty when ty <> fst res ->
            type_error "Type annotation '%s' is not compatible with type '%s'" (pretty_ty ty) (pretty_ty (fst res))
        | _ -> res

    | App (e1, e2) ->
        let t1, s1 = typeinfer_expr env e1
        let t2, s2 = typeinfer_expr (apply_subst_env env s1) e2
        let alpha = fresh_tyvar ()
        // WHY FLIPPED
        let s3 = unify (TyArrow(t2, alpha)) t1
        let t = apply_subst alpha s3
        let s4 = s3 ++ s2
        t, s4

    | Tuple tu ->
        let (ty, subst) =
            List.fold
                (fun acc e ->
                    let ti, si = typeinfer_expr env e
                    acc @ [ (ti, si) ])
                []
                tu
            |> List.unzip

        TyTuple(ty), List.last subst

    | LetRec (f, tyo, e1, e2) ->
        let alpha = fresh_tyvar ()
        let rec_binding = Forall(Set.empty, alpha)
        let gamma1 = (f, rec_binding) :: env
        let t1, s1 = typeinfer_expr gamma1 e1

        let gamma2 = apply_subst_env gamma1 s1
        let sigma = generalize gamma2 t1
        let gamma3 = (f, sigma) :: gamma2

        let t2, s2 = typeinfer_expr gamma3 e2

        let s3 = s2 ++ s1
        t2, s3

    // Math operators
    | BinOp (e1,
             ("+"
             | "-"
             | "*"
             | "/"
             | "%"),
             e2) ->
        //let t1, s1 = typeinfer_expr env e1
        //let t2, s2 = typeinfer_expr env e2
        //let s = unify t1 t2 ++ s2 ++ s1
        //let t = apply_subst t1 s
        //t, s
        let t1, s1 = typeinfer_expr env e1
        let s2 = unify t1 TyInt
        let s3 = s2 ++ s1
        let gamma1 = apply_subst_env env s3
        let t2, s4 = typeinfer_expr gamma1 e2
        let s5 = unify t2 TyInt
        let s6 = s5 ++ s4 ++ s3

        TyInt, s6

    | BinOp (e1,
             ("<"
             | "<="
             | "="
             | ">="
             | ">"
             | "<>"),
             e2) ->
        let t1, s1 = typeinfer_expr env e1
        let s2 = unify t1 TyInt
        let t2, s3 = typeinfer_expr env e2
        let s4 = unify t2 TyInt
        let s = s4 ++ s3 ++ s2 ++ s1
        TyBool, s

    | BinOp (e1,
             ("and"
             | "or" as op),
             e2) ->
        let t1, s1 = typeinfer_expr env e1
        let t2, s2 = typeinfer_expr env e2
        let s3 = unify t1 TyBool
        let s4 = unify t2 TyBool
        let s = s4 ++ s3 ++ s2 ++ s1
        TyBool, s

    | BinOp (_, op, _) -> unexpected_error "typecheck_expr: unsupported binary operator (%s)" op

    | UnOp ("not", e) ->
        let t, s1 = typeinfer_expr env e
        let s2 = unify t TyBool ++ s1
        TyBool, s2

    | UnOp ("-", e) ->
        let t, s1 = typeinfer_expr env e
        printfn "Unifying first %O with %O, then %O with %O" t TyInt t TyFloat
        let s2 = unify t TyInt
        let s3 = unify t TyFloat
        let s = s3 ++ s2 ++ s1
        apply_subst t s, s

    | UnOp (op, _) -> unexpected_error "typecheck_expr: unsupported unary operator (%s)" op


    | e -> failwithf "Expression %O is not yet implemented" e
