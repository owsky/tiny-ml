module TinyML.Utilities

open Ast
open Exceptions

let type_error fmt = throw_formatted TypeError fmt

/// Given a triple, returns a pair made of the first and second items
let pick_a_b (a, b, _) = (a, b)
/// Given a triple. returns a pair mde of the first and third items
let pick_a_c (a, _, c) = (a, c)
/// Given a list of triples, returns a list of pairs made of the first and second items for each triple
let pick_a_b_list l = List.map pick_a_b l
/// Given a list of triples, returns a list of pairs made of the first and third items for each triple
let pick_a_c_list l = List.map pick_a_c l
/// Given a list of pairs, return a list made of the first element of each pair
let fst_l l = List.map fst l
/// Given a list of pairs, return a list made of the second element of each pair
let snd_l l = List.map snd l

/// Given a type, returns a new type with all the type variables normalized
let normalize_tyvars ty =
    let mutable counter = -1

    let fresh_tyvar () =
        counter <- counter + 1
        counter

    // Creates a mapping from old tyvars to new tyvars
    let rec create_mapping ty (map: Map<tyvar, tyvar>) =
        match ty with
        | TyName _ -> map
        | TyArrow (t1, t2) -> create_mapping t2 (create_mapping t1 map)
        | TyVar v ->
            if map.ContainsKey v then
                map
            else
                map.Add(v, fresh_tyvar ())
        | TyTuple ts -> List.fold (fun acc ty -> create_mapping ty acc) map ts

    let map = create_mapping ty Map.empty

    let rec apply_mapping ty =
        match ty with
        | TyName _ -> ty
        | TyArrow (t1, t2) -> TyArrow(apply_mapping t1, apply_mapping t2)
        | TyVar v -> TyVar(map.[v])
        | TyTuple ts -> TyTuple(List.map apply_mapping ts)

    apply_mapping ty

// Given an integer, converts it to the respective alphabet letter
let int_to_letter n : string =
    let alphabet = "abcdefghijklmnopqrstuvwxyz"
    let len = alphabet.Length

    if n < len then // if n is less than 26, use a single letter
        string alphabet.[n]
    else // otherwise, use a letter-number combination
        let div = n / len
        let rem = n % len
        string alphabet.[div - 1] + string rem // use the letter at the (div - 1)th position and the remainder as a number

/// Given a string, it returns a new string constructed by substituting all integers with letter-numbers combinations
let replace_integers (s: string) : string =
    let regex = System.Text.RegularExpressions.Regex("[0-9]+") // regex that matches one or more digits
    regex.Replace(s, (fun (m: System.Text.RegularExpressions.Match) -> int_to_letter (int m.Value)))
