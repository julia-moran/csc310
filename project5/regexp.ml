(*
    Author:         Julia Moran
    Major:          Computer Science
    Creation Date:  March 21, 2022
    Due Date:       April 3rd, 2022
    Course:         CSC310 010
    Professor Name: Dr. Schwesinger
    Assignment:     #5
    Filename:       regexp.ml
    Purpose:        This program defines a regular expression data type and
                    provides various functions for regular expressions, such
                    as converting a regular expression to a NFA and converting
                    a string to a regular expression.
*)

open Fa
open Sets

(*********)
(* Types *)
(*********)

type regexp =
  | Empty_String
  | Char of char
  | Union of regexp * regexp
  | Concat of regexp * regexp
  | Star of regexp

(***********)
(* Utility *)
(***********)

let fresh =
  let cntr = ref 0 in
  fun () ->
    cntr := !cntr + 1 ;
    !cntr

(*******************************)
(* Part 3: Regular Expressions *)
(*******************************)
(*
    Function Name:  regexp_to_nfa
    Description:    Converts a regular expression to a NFA
    Parameters:     re: regexp - the regular expression to convert to an NFA
    Return Value:   (int, char) fa - the NFA the regular expression was 
                                     converted to
*)
let rec regexp_to_nfa (re: regexp) : (int, char) fa =
    match re with
    | Empty_String -> 
        let s0 = fresh() in
        let s1 = fresh() in
        let nfa = {
            sigma = [];
            qs = [s0; s1];
            q0 = s0;
            fs = [s1];
            delta = [(s0, None, s1)]
        } in nfa
    | Char (c) -> 
        let s0 = fresh() in
        let s1 = fresh() in
        let nfa = {
            sigma = [c];
            qs = [s0; s1];
            q0 = s0;
            fs = [s1];
            delta = [(s0, Some c, s1)]
        } in nfa
    | Union (r1, r2) ->
        let newStart = fresh() in
        let newAccept = fresh() in
        let nfa1 = regexp_to_nfa r1 in
        let nfa2 = regexp_to_nfa r2 in
        let finals1 =
            (List.fold_right (fun final acc -> (final, None, newAccept)::acc) nfa1.fs []) in
        let finals2 = 
            (List.fold_right (fun final acc -> (final, None, newAccept)::acc) nfa2.fs []) in
        let nfa = {
            sigma = union nfa1.sigma nfa2.sigma;
            qs = union (union nfa1.qs nfa2.qs) [newStart; newAccept];
            q0 = newStart;
            fs = [newAccept];
            delta = union (union nfa1.delta nfa2.delta) 
                ([(newStart, None, nfa1.q0);(newStart, None, nfa2.q0)]@
                finals1@finals2) 
        } in nfa
    | Concat (r1, r2) ->
        let nfa1 = regexp_to_nfa r1 in
        let nfa2 = regexp_to_nfa r2 in
        let newFinals =
            (List.fold_left (fun acc final -> (final, None, nfa2.q0)::acc) [] nfa1.fs) in
        let nfa = {
            sigma = union nfa1.sigma nfa2.sigma;
            qs = union nfa1.qs nfa2.qs;
            q0 = nfa1.q0;
            fs = nfa2.fs;
            delta = union (union nfa1.delta nfa2.delta) newFinals
        } in nfa
    | Star r1 ->
        let newStart = fresh() in
        let newAccept = fresh() in
        let nfa1 = regexp_to_nfa r1 in
        let newFinals = 
            (List.fold_left (fun acc final -> (final, None, newStart)::acc) [] nfa1.fs) in
        let nfa = {
            sigma = nfa1.sigma;
            qs = union nfa1.qs [newStart; newAccept];
            q0 = newStart;
            fs = [newAccept];
            delta = union nfa1.delta [
                (newStart, None, nfa1.q0)]@newFinals
                @[(newStart, None, newAccept)]
        } in nfa



(*****************************************************************)
(* Below this point is parser code that YOU DO NOT NEED TO TOUCH *)
(*****************************************************************)

exception IllegalExpression of string

(* Scanner *)
type token =
  | Tok_Char of char
  | Tok_Epsilon
  | Tok_Union
  | Tok_Star
  | Tok_LParen
  | Tok_RParen
  | Tok_END

let tokenize str =
  let re_var = Str.regexp "[a-z]" in
  let re_epsilon = Str.regexp "E" in
  let re_union = Str.regexp "|" in
  let re_star = Str.regexp "*" in
  let re_lparen = Str.regexp "(" in
  let re_rparen = Str.regexp ")" in
  let rec tok pos s =
    if pos >= String.length s then [Tok_END]
    else if Str.string_match re_var s pos then
      let token = Str.matched_string s in
      Tok_Char token.[0] :: tok (pos + 1) s
    else if Str.string_match re_epsilon s pos then
      Tok_Epsilon :: tok (pos + 1) s
    else if Str.string_match re_union s pos then Tok_Union :: tok (pos + 1) s
    else if Str.string_match re_star s pos then Tok_Star :: tok (pos + 1) s
    else if Str.string_match re_lparen s pos then Tok_LParen :: tok (pos + 1) s
    else if Str.string_match re_rparen s pos then Tok_RParen :: tok (pos + 1) s
    else raise (IllegalExpression ("tokenize: " ^ s))
  in
  tok 0 str

let tok_to_str t =
  match t with
  | Tok_Char v -> Char.escaped v
  | Tok_Epsilon -> "E"
  | Tok_Union -> "|"
  | Tok_Star -> "*"
  | Tok_LParen -> "("
  | Tok_RParen -> ")"
  | Tok_END -> "END"

(*
   S -> A Tok_Union S | A
   A -> B A | B
   B -> C Tok_Star | C
   C -> Tok_Char | Tok_Epsilon | Tok_LParen S Tok_RParen

   FIRST(S) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(A) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(B) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(C) = Tok_Char | Tok_Epsilon | Tok_LParen
 *)

let parse_regexp (l : token list) =
  let lookahead tok_list =
    match tok_list with
    | [] -> raise (IllegalExpression "lookahead")
    | h :: t -> (h, t)
  in
  let rec parse_S l =
    let a1, l1 = parse_A l in
    let t, n = lookahead l1 in
    match t with
    | Tok_Union ->
        let a2, l2 = parse_S n in
        (Union (a1, a2), l2)
    | _ -> (a1, l1)
  and parse_A l =
    let a1, l1 = parse_B l in
    let t, n = lookahead l1 in
    match t with
    | Tok_Char c ->
        let a2, l2 = parse_A l1 in
        (Concat (a1, a2), l2)
    | Tok_Epsilon ->
        let a2, l2 = parse_A l1 in
        (Concat (a1, a2), l2)
    | Tok_LParen ->
        let a2, l2 = parse_A l1 in
        (Concat (a1, a2), l2)
    | _ -> (a1, l1)
  and parse_B l =
    let a1, l1 = parse_C l in
    let t, n = lookahead l1 in
    match t with Tok_Star -> (Star a1, n) | _ -> (a1, l1)
  and parse_C l =
    let t, n = lookahead l in
    match t with
    | Tok_Char c -> (Char c, n)
    | Tok_Epsilon -> (Empty_String, n)
    | Tok_LParen ->
        let a1, l1 = parse_S n in
        let t2, n2 = lookahead l1 in
        if t2 = Tok_RParen then (a1, n2)
        else raise (IllegalExpression "parse_C 1")
    | _ -> raise (IllegalExpression "parse_C 2")
  in
  let rxp, toks = parse_S l in
  match toks with
  | [Tok_END] -> rxp
  | _ -> raise (IllegalExpression "parse didn't consume all tokens")


let string_to_regexp str = parse_regexp @@ tokenize str

let string_to_nfa str = regexp_to_nfa @@ string_to_regexp str
