module TinyML.TypeInferencing

open Ast
open Printers
open Exceptions
open TypeInferencingUtils
open Init

let gamma0: scheme env = ops_types

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
        let findo = List.tryFind (fun (id, _) -> id = x) env

        match findo with
        | Some (_, scheme) -> instantiate scheme, []
        | None -> throw_formatted UndefinedError "The value or constructor '%s' is not defined." x

    | Let (x, tyo, e1, e2) ->
        let t1, s1 = typeinfer_expr env e1
        let gamma1 = apply_subst_env env s1
        let sigma = generalize gamma1 t1
        let gamma2 = (x, sigma) :: gamma1
        let t2, s2 = typeinfer_expr gamma2 e2
        let s3 = s2 ++ s1

        match tyo with
        | Some ty ->
            let s4 = unify t2 ty ++ s3
            apply_subst t2 s4, s4
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

        match tyo with
        | Some ty ->
            let s2 = unify ty t1 ++ s1
            TyArrow(apply_subst t1 s2, apply_subst t2 s2), s2
        | _ -> TyArrow(t1, t2), s1

    | App (e1, e2) ->
        let t1, s1 = typeinfer_expr env e1
        let t2, s2 = typeinfer_expr (apply_subst_env env s1) e2
        let alpha = fresh_tyvar ()
        let s3 = unify t1 (TyArrow(t2, alpha))
        let t = apply_subst alpha s3
        let s4 = s3 ++ s2 ++ s1
        t, s4

    | Tuple tu ->
        let (ty, subst) =
            List.fold
                (fun acc e ->
                    let si_1 =
                        match List.tryLast acc with
                        | Some s -> snd s
                        | None -> []

                    let ti, si = typeinfer_expr (apply_subst_env env si_1) e
                    acc @ [ (ti, si ++ si_1) ])

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

        match tyo with
        | Some ty ->
            let s4 = unify ty t2 ++ s3
            let t3 = apply_subst t2 s4
            t3, s4
        | None ->

            t2, s3

    | BinOp (e1, op, e2) -> typeinfer_expr env (App(App(Var op, e1), e2))

    | UnOp (op, e) -> typeinfer_expr env (App(Var op, e))

    | _ -> unexpected_error "typecheck_expr: unsupported expression: %s [AST: %A]" (pretty_expr e) e
