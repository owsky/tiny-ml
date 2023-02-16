module TinyML.Printers

open Ast
open Exceptions

/// utility function for printing lists by flattening strings with a separator
let rec flatten p sep es =
    match es with
    | [] -> ""
    | [ e ] -> p e
    | e :: es -> sprintf "%s%s %s" (p e) sep (flatten p sep es)

/// print pairs within the given env using p as printer for the elements bound within
let pretty_env p env =
    sprintf "[%s]" (flatten (fun (x, o) -> sprintf "%s=%s" x (p o)) ";" env)

/// print any tuple given a printer p for its elements
let pretty_tupled p l = flatten p ", " l

let rec pretty_ty t =
    match t with
    | TyName s -> s
    | TyArrow (t1, t2) -> sprintf "%s -> %s" (pretty_ty t1) (pretty_ty t2)
    | TyVar n -> sprintf "'%d" n
    | TyTuple ts -> sprintf "(%s)" (pretty_tupled pretty_ty ts)

let pretty_lit lit =
    match lit with
    | LInt n -> sprintf "%d" n
    | LFloat n -> sprintf "%g" n
    | LString s -> sprintf "\"%s\"" s
    | LChar c -> sprintf "%c" c
    | LBool true -> "true"
    | LBool false -> "false"
    | LUnit -> "()"

let rec pretty_expr e =
    match e with
    | Lit lit -> pretty_lit lit

    | Lambda (x, None, e) -> sprintf "fun %s -> %s" x (pretty_expr e)
    | Lambda (x, Some t, e) -> sprintf "fun (%s : %s) -> %s" x (pretty_ty t) (pretty_expr e)

    | App (e1, e2) ->
        match e1, e2 with
        | Lambda _, _
        | App _, _
        | Var _, _ -> sprintf "(%s) %s" (pretty_expr e1) (pretty_expr e2)
        | _ -> unexpected_error "Cannot apply expression: %s to expression: %s" (pretty_expr e1) (pretty_expr e2)

    | Var x -> x

    | Let (x, None, e1, e2) -> sprintf "let %s = %s in %s" x (pretty_expr e1) (pretty_expr e2)

    | Let (x, Some t, e1, e2) -> sprintf "let %s : %s = %s in %s" x (pretty_ty t) (pretty_expr e1) (pretty_expr e2)

    | LetRec (x, None, e1, e2) -> sprintf "let rec %s = %s in %s" x (pretty_expr e1) (pretty_expr e2)

    | LetRec (x, Some tx, e1, e2) ->
        sprintf "let rec %s : %s = %s in %s" x (pretty_ty tx) (pretty_expr e1) (pretty_expr e2)

    | IfThenElse (e1, e2, e3o) ->
        let s = sprintf "if %s then %s" (pretty_expr e1) (pretty_expr e2)

        match e3o with
        | None -> s
        | Some e3 -> sprintf "%s else %s" s (pretty_expr e3)

    | Tuple es -> sprintf "(%s)" (pretty_tupled pretty_expr es)

    | BinOp (e1, op, e2) -> sprintf "%s %s %s" (pretty_expr e1) op (pretty_expr e2)

    | UnOp (op, e) -> sprintf "%s %s" op (pretty_expr e)

    | _ -> unexpected_error "pretty_expr: %s" (pretty_expr e)

let rec pretty_value v =
    match v with
    | VLit lit -> pretty_lit lit

    | VTuple vs -> pretty_tupled pretty_value vs

    | Closure (env, x, e) -> sprintf "<|%s;%s;%s|>" (pretty_env pretty_value env) x (pretty_expr e)

    | RecClosure (env, f, x, e) -> sprintf "<|%s;%s;%s;%s|>" (pretty_env pretty_value env) f x (pretty_expr e)
