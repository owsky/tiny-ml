(*
* TinyML
* Ast.fs: abstract syntax tree
*)

// AST type definitions
module TinyML.Ast

type tyvar = int

type ty =
    | TyName of string
    | TyArrow of ty * ty
    | TyVar of tyvar
    | TyTuple of ty list

type subst = (tyvar * ty) list

// pseudo data constructors for literal types
let TyFloat = TyName "float"
let TyInt = TyName "int"
let TyChar = TyName "char"
let TyString = TyName "string"
let TyBool = TyName "bool"
let TyUnit = TyName "unit"

// active pattern for literal types
let private (|TyLit|_|) name =
    function
    | TyName s when s = name -> Some()
    | _ -> None

let (|TyFloat|_|) = (|TyLit|_|) "float"
let (|TyInt|_|) = (|TyLit|_|) "int"
let (|TyChar|_|) = (|TyLit|_|) "char"
let (|TyString|_|) = (|TyLit|_|) "string"
let (|TyBool|_|) = (|TyLit|_|) "bool"
let (|TyUnit|_|) = (|TyLit|_|) "unit"

type scheme = Forall of tyvar Set * ty

type lit =
    | LInt of int
    | LFloat of float
    | LString of string
    | LChar of char
    | LBool of bool
    | LUnit

type binding = bool * string * ty option * expr // (is_recursive, id, optional_type_annotation, expression)

and expr =
    | Lit of lit
    | Lambda of string * ty option * expr
    | App of expr * expr
    | Var of string
    | LetIn of binding * expr
    | IfThenElse of expr * expr * expr option
    | Tuple of expr list
    | BinOp of expr * string * expr
    | UnOp of string * expr

let fold_params parms e0 =
    List.foldBack (fun (id, tyo) e -> Lambda(id, tyo, e)) parms e0

let (|Let|_|) =
    function
    | LetIn ((false, x, tyo, e1), e2) -> Some(x, tyo, e1, e2)
    | _ -> None

let (|LetRec|_|) =
    function
    | LetIn ((true, x, tyo, e1), e2) -> Some(x, tyo, e1, e2)
    | _ -> None

type 'a env = (string * 'a) list

type value =
    | VLit of lit
    | VTuple of value list
    | Closure of value env * string * expr
    | RecClosure of value env * string * string * expr

type interactive =
    | IExpr of expr
    | IBinding of binding
