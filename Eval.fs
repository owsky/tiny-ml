(*
* TinyML
* Eval.fs: evaluator
*)

module TinyML.Eval

open Ast
open TypeInferencingUtils
open Utilities

let get_f op l =
    snd (List.find (fun (op_name, _) -> op_name = op) l)

let rec eval_expr (env: value env) (e: expr) : value =
    match e with
    | Lit lit -> VLit lit

    | Lambda (x, _, e) -> Closure(env, x, e)

    | App (e1, e2) ->
        let v1 = eval_expr env e1
        let v2 = eval_expr env e2

        match v1 with
        | Closure (env1, x, e) -> eval_expr ((x, v2) :: env1) e
        | RecClosure (env1, f, x, e) -> eval_expr ((x, v2) :: (f, v1) :: env1) e
        | _ -> unexpected_error "non-closure in left side of application: %s" (pretty_value v1)

    | Var x ->
        let _, v = List.find (fun (y, _) -> x = y) env
        v

    | IfThenElse (e1, e2, None) ->
        let v1 = eval_expr env e1

        match v1 with
        | VLit (LBool true) -> eval_expr env e2
        | VLit (LBool false) -> VLit LUnit
        | _ -> unexpected_error "non-boolean in if guard: %s" (pretty_value v1)


    | IfThenElse (e1, e2, Some e3) ->
        let v1 = eval_expr env e1

        eval_expr
            env
            (match v1 with
             | VLit (LBool true) -> e2
             | VLit (LBool false) -> e3
             | _ -> unexpected_error "non-boolean in if guard: %s" (pretty_value v1))

    | Let (x, _, e1, e2) ->
        let v1 = eval_expr env e1
        eval_expr ((x, v1) :: env) e2

    | LetRec (f, _, e1, e2) ->
        let v1 = eval_expr env e1

        let recclo =
            match v1 with
            | Closure (venv1, x, e) -> RecClosure(venv1, f, x, e)
            | _ -> unexpected_error "expected closure in rec binding but got: %s" (pretty_value v1)

        let env2 = (f, recclo) :: env
        eval_expr env2 e2

    | Tuple tu -> VTuple(List.map (fun x -> eval_expr env x) tu)

    // Arithmetic int operators
    | BinOp (e1, op, e2) when List.contains op (fst_l int_ops) -> binop_int env op (get_f op int_ops) e1 e2

    // Arithmetic float operators
    | BinOp (e1, op, e2) when List.contains op (fst_l float_ops) -> binop_float env op (get_f op float_ops) e1 e2

    // Unary int operators
    | UnOp (op, e) when List.contains op (fst_l int_uops) -> unop_int env op (get_f op int_uops) e

    // Int comp operators
    | BinOp (e1, op, e2) when List.contains op (fst_l int_comp_ops) ->
        binop_comp_int env op (get_f op int_comp_ops) e1 e2

    // Float comp operators
    | BinOp (e1, op, e2) when List.contains op (fst_l float_comp_ops) ->
        binop_comp_float env op (get_f op float_comp_ops) e1 e2

    // Bool ops
    | BinOp (e1, op, e2) when List.contains op (fst_l bool_ops) -> binop_bool env op (get_f op bool_ops) e1 e2

    // Bool unary ops
    | UnOp (op, e) when List.contains op (fst_l bool_uops) -> unop_bool env op (get_f op bool_uops) e

    | _ -> unexpected_error "eval_expr: unsupported expression: %s [AST: %A]" (pretty_expr e) e

and eval2 env e1 e2 = (eval_expr env e1, eval_expr env e2)

and binop_int env (op_s: string) (op) (e1: expr) (e2: expr) =
    let v1, v2 = eval2 env e1 e2

    match v1, v2 with
    | VLit (LInt x), VLit (LInt y) -> VLit(LInt(op x y))
    | _ ->
        unexpected_error
            "eval_expr: operator %s requires int values, got: %s, %s"
            op_s
            (pretty_value v1)
            (pretty_value v2)

and binop_float env (op_s: string) (op: float -> float -> float) (e1: expr) (e2: expr) =
    let v1, v2 = eval2 env e1 e2

    match v1, v2 with
    | VLit (LFloat x), VLit (LFloat y) -> VLit(LFloat(op x y))
    | _ ->
        unexpected_error
            "eval_expr: operator %s requires float values, got: %s, %s"
            op_s
            (pretty_value v1)
            (pretty_value v2)

and unop_int env (op_s: string) (op) (e: expr) =
    let v = eval_expr env e

    match v with
    | VLit (LInt x) -> VLit(LInt(op x))
    | _ -> unexpected_error "eval_expr: operator %s requires int values, got: %s" op_s (pretty_value v)

and binop_comp_int env (op_s: string) (op: int -> int -> bool) (e1: expr) (e2: expr) =
    let v1, v2 = eval2 env e1 e2

    match v1, v2 with
    | VLit (LInt x), VLit (LInt y) -> VLit(LBool(op x y))
    | _ ->
        unexpected_error
            "eval_expr: operator %s requires int values, got: %s, %s"
            op_s
            (pretty_value v1)
            (pretty_value v2)

and binop_comp_float env (op_s: string) (op: float -> float -> bool) (e1: expr) (e2: expr) =
    let v1, v2 = eval2 env e1 e2

    match v1, v2 with
    | VLit (LFloat x), VLit (LFloat y) -> VLit(LBool(op x y))
    | _ ->
        unexpected_error
            "eval_expr: operator %s requires float values, got: %s, %s"
            op_s
            (pretty_value v1)
            (pretty_value v2)

and binop_bool env (op_s: string) (op: bool -> bool -> bool) (e1: expr) (e2: expr) =
    let v1, v2 = eval2 env e1 e2

    match v1, v2 with
    | VLit (LBool x), VLit (LBool y) -> VLit(LBool(op x y))
    | _ ->
        unexpected_error
            "eval_expr: operator %s requires bool values, got: %s, %s"
            op_s
            (pretty_value v1)
            (pretty_value v2)

and unop_bool env (op_s: string) (op: bool -> bool) (e: expr) =
    let v1 = eval_expr env e

    match v1 with
    | VLit (LBool x) -> VLit(LBool(op x))
    | _ -> unexpected_error "eval_expr: operator %s requires bool values, got: %s" op_s (pretty_value v1)
