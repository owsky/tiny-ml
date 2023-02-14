module TinyML.TypeInferencingUtils

open Ast
open Utilities

let mutable tyvar_counter = 0

/// Returns a globally unused type variable
let fresh_tyvar () =
    tyvar_counter <- tyvar_counter + 1
    TyVar tyvar_counter

/// Given a type and a substitution it applies the substitution and returns a new type
let rec apply_subst (t: ty) (s: subst) : ty =
    match t with
    | TyName _ -> t
    | TyVar tv ->
        try
            let _, t1 = List.find (fun (tv1, _) -> tv1 = tv) s

            t1
        with
        | _ -> t
    | TyArrow (t1, t2) -> TyArrow(apply_subst t1 s, apply_subst t2 s)
    | TyTuple ts -> TyTuple(List.map (fun t -> apply_subst t s) ts)

/// Given a type scheme and a substitution it applies the substitution and returns a new type scheme
let apply_subst_scheme (Forall (alpha_signed, ty)) (s: subst) : scheme =
    let subst_prime = List.filter (fun (tv, _) -> not (Set.contains tv alpha_signed)) s
    Forall(alpha_signed, apply_subst ty subst_prime)

/// Given an environment scheme and a substitution it applies the substitution to the environment and produces a new scheme environment
let apply_subst_env (env: scheme env) (subst: subst) : scheme env =
    List.map (fun (id, scheme) -> (id, apply_subst_scheme scheme subst)) env

/// Checks whether a given type variable belongs to a type
let rec belongs (tv: tyvar) (ty: ty) : bool =
    match ty with
    | TyArrow (t1, t2) -> belongs tv t1 || belongs tv t2
    | TyVar t when t = tv -> true
    | TyTuple tu -> List.fold (fun acc ty -> acc || belongs tv ty) false tu
    | _ -> false

/// Checks whether a substitution has domain conflicts
let rec check_domain (s: subst) =
    match s with
    | [] -> ()
    | (tv, ty) :: xs ->
        let findo = List.tryFind (fun (tvf, tyf) -> tvf = tv) xs

        match findo with
        | Some (tvf, tyf) ->
            if tyf <> ty then
                type_error "Domain not disjointed"
        | None -> check_domain xs

/// Given two substitutions it produces a single, composed substitution TODO HANDLE ERRORS STATES
let compose_subst (s1: subst) (s2: subst) : subst =
    let merged =
        List.map
            (fun (tv, ty) ->
                let r = (tv, apply_subst ty s1)

                if belongs tv (snd r) then
                    type_error "Circularity not allowed"
                else
                    r)
            s2
        @ s1

    check_domain merged

    merged

/// Given two types it produces a substitution by unifying them
let rec unify (t1: ty) (t2: ty) : subst =
    match (t1, t2) with
    | TyName s1, TyName s2 when s1 = s2 -> []

    | TyVar tv, t
    | t, TyVar tv ->
        if belongs tv t then
            type_error "Circularity not allowed"
        else
            [ tv, t ]

    | TyArrow (t1, t2), TyArrow (t3, t4) -> compose_subst (unify t1 t3) (unify t2 t4)

    | TyTuple ts1, TyTuple ts2 when List.length ts1 = List.length ts2 ->
        List.zip ts1 ts2
        |> List.fold (fun s (t1, t2) -> compose_subst s (unify t1 t2)) []

    | TyArrow (_, _), w
    | w, TyArrow (_, _) -> type_error "expected type function, got %s instead" (pretty_ty w)

    | _ -> type_error "Cannot unify types %s and %s" (pretty_ty t1) (pretty_ty t2)

/// Returns the free type variables contained in a type
let rec freevars_ty t =
    match t with
    | TyName _ -> Set.empty
    | TyVar tv -> Set.singleton tv
    | TyArrow (t1, t2) -> (freevars_ty t1) + (freevars_ty t2)
    | TyTuple ts -> List.fold (fun r t -> r + freevars_ty t) Set.empty ts

/// Returns the free type variables contained in a type scheme
let freevars_scheme (Forall (tvs, t)) = freevars_ty t - tvs

/// Returns the free type variables contained in an environment
let freevars_scheme_env env =
    List.fold (fun r (_, sch) -> r + freevars_scheme sch) Set.empty env

/// Given an environment and a type, it produces a type scheme by generalizing the type
let generalize (env: scheme env) ty : scheme =
    let alpha = freevars_ty ty - freevars_scheme_env env
    Forall(alpha, ty)

/// Refreshes the type variables bound to the type scheme
let rec refresh (Forall (sch, ty)) =
    let subs: subst = Set.fold (fun acc tv -> (tv, (fresh_tyvar ())) :: acc) [] sch

    apply_subst ty subs

/// Instantiates a type scheme into a concrete type by refreshing the type variables
let instantiate (sch: scheme) = refresh sch
