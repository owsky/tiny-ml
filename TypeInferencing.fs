module TinyML.TypeInferencing

open Ast
open Utilities

type subst = (tyvar * ty) list

// TODO implement this
let compose_subst (s1: subst) (s2: subst) : subst = s1 @ s2

// TODO implement this
let rec unify (t1: ty) (t2: ty) : subst =
    match (t1, t2) with
    | TyName s1, TyName s2 when s1 = s2 -> []

    | TyVar tv, t
    | t, TyVar tv -> [ tv, t ]

    | TyArrow (t1, t2), TyArrow (t3, t4) -> compose_subst (unify t1 t3) (unify t2 t4)

    | TyTuple ts1, TyTuple ts2 when List.length ts1 = List.length ts2 ->
        List.zip ts1 ts2
        |> List.fold (fun s (t1, t2) -> compose_subst s (unify t1 t2)) []
    | _ -> type_error "Cannot unify types %O and %O" t1 t2

// TODO implement this
let rec apply_subst (t: ty) (s: subst) : ty =
    match t with
    | TyName _ -> t
    | TyArrow (t1, t2) -> TyArrow(apply_subst t1 s, apply_subst t2 s)
    | TyVar tv ->
        try
            let _, t1 = List.find (fun (tv1, _) -> tv1 = tv) s
            t1
        //with KeyNotFoundException -> t
        with
        | _ -> t
    | TyTuple ts -> TyTuple(List.map (fun t -> apply_subst t s) ts)


let rec freevars_ty t =
    match t with
    | TyName s -> Set.empty
    | TyArrow (t1, t2) -> (freevars_ty t1) + (freevars_ty t2)
    | TyVar tv -> Set.singleton tv
    | TyTuple ts -> List.fold (fun r t -> r + freevars_ty t) Set.empty ts

let freevars_scheme (Forall (tvs, t)) = freevars_ty t - tvs

let freevars_scheme_env env =
    List.fold (fun r (_, sch) -> r + freevars_scheme sch) Set.empty env

// TODO continue implementing this
let rec typeinfer_expr (env: scheme env) (e: expr) : ty * subst =
    match e with
    | Lit (LInt _) -> TyInt, []
    | Lit (LBool _) -> TyBool, []
    | Lit (LFloat _) -> TyFloat, []
    | Lit (LString _) -> TyString, []
    | Lit (LChar _) -> TyChar, []
    | Lit LUnit -> TyUnit, []

    | Let (x, tyo, e1, e2) ->
        let t1, s1 = typeinfer_expr env e1
        let tvs = freevars_ty t1 - freevars_scheme_env env
        let sch = Forall(tvs, t1)
        let t2, s2 = typeinfer_expr ((x, sch) :: env) e2
        t2, compose_subst s2 s1

    | _ -> failwithf "not implemented"

// basic environment: add builtin operators at will
let gamma0 =
    [ ("+", TyArrow(TyInt, TyArrow(TyInt, TyInt)))
      ("-", TyArrow(TyInt, TyArrow(TyInt, TyInt))) ]
