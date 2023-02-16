(*
* TinyML
* Ast.fs: abstract syntax tree
*)

module TinyML.Ast

open Printf

// errors
exception SyntaxError of string * FSharp.Text.Lexing.LexBuffer<char>
exception TypeError of string
exception UnexpectedError of string
exception UndefinedError of string

let throw_formatted exnf fmt = ksprintf (fun s -> raise (exnf s)) fmt

let unexpected_error fmt = throw_formatted UnexpectedError fmt

// AST type definitions
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

// pretty printers

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

    // TODO pattern-match sub-application cases
    | App (e1, e2) -> sprintf "%s %s" (pretty_expr e1) (pretty_expr e2)

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

// The function that normalizes the tyvars in a type with a counter starting from 0 TODO REVIEW THIS
let normalize_tyvars ty =
    let counter = ref -1 // create a local counter

    let fresh_tyvar () = // create a local fresh_tyvar function
        counter := !counter + 1
        !counter
    // A helper function that creates a mapping from old tyvars to new tyvars with the updated counter
    let rec create_mapping ty (map: Map<tyvar, tyvar>) =
        match ty with
        | TyName _ -> map // no tyvars in TyName
        | TyArrow (t1, t2) -> // create mapping for both subtypes
            let map1 = create_mapping t1 map
            create_mapping t2 map1
        | TyVar v -> // check if the tyvar is already in the map
            if map.ContainsKey v then
                map
            else
                map.Add(v, fresh_tyvar ()) // otherwise, add a new mapping
        | TyTuple ts -> // create mapping for each subtype in the list
            List.fold (fun m t -> create_mapping t m) map ts

    let map = create_mapping ty Map.empty // create the mapping

    let rec apply_mapping ty = // a helper function that applies the mapping
        match ty with
        | TyName _ -> ty // no change for TyName
        | TyArrow (t1, t2) -> // apply mapping for both subtypes
            TyArrow(apply_mapping t1, apply_mapping t2)
        | TyVar v -> // look up the new tyvar in the map
            TyVar(map.[v])
        | TyTuple ts -> // apply mapping for each subtype in the list
            TyTuple(List.map apply_mapping ts)

    apply_mapping ty // apply the mapping to the type

// A helper function that converts an integer to a letter or a letter-number combination
let int_to_letter (n: int) : string =
    let alphabet = "abcdefghijklmnopqrstuvwxyz"
    let len = alphabet.Length

    if n < len then // if n is less than 26, use a single letter
        alphabet.[n].ToString()
    else // otherwise, use a letter-number combination
        let div = n / len // the quotient of n divided by 26
        let rem = n % len // the remainder of n divided by 26
        alphabet.[div - 1].ToString() + rem.ToString() // use the letter at the (div - 1)th position and the remainder as a number

// The main function that replaces integers in a string with letters or letter-number combinations
let replace_integers (s: string) : string =
    let regex = System.Text.RegularExpressions.Regex("[0-9]+") // a regular expression that matches one or more digits

    let replacer =
        fun (m: System.Text.RegularExpressions.Match) -> // a function that takes a match and returns a replacement
            let n = int m.Value // convert the matched string to an integer
            int_to_letter n // convert the integer to a letter or a letter-number combination

    regex.Replace(s, replacer) // replace all matches in the string with the replacer function
