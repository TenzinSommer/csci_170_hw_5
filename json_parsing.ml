(*****************************************************************************
 * JSON parsing: lexer and parser for a subset of JSON.
 *****************************************************************************)

(*****************************************************************************
 * Token representation for the lexer.
 *****************************************************************************)

type token =
  | NumLit of string    (* e.g., 3.14 represented as "3.14" *)
  | StringLit of string (* e.g., "foo" *)
  | FalseTok            (* false *)
  | TrueTok             (* true *)
  | NullTok             (* null *)
  | LBrace              (* { *)
  | RBrace              (* } *)
  | LBracket            (* [ *)
  | RBracket            (* ] *)
  | Comma               (* , *)
  | Colon               (* : *)

(*****************************************************************************
 * Basic token-printing support.
 *****************************************************************************)

(* copied over from hw3 *)
(** [quote_string s] is the string [s] surrounded by additional quote characters 
at the beginning and end. *)
let quote_string s =
  "\"" ^ s ^ "\""

(**
[string_of_token  t] returns a string representation of the token [t].
Note that string literals are returned with quotes.
*)
let string_of_token t =
  match t with
  | NumLit s -> s
  | StringLit s -> quote_string s
  | FalseTok -> "false"
  | TrueTok -> "true"
  | NullTok -> "null"
  | LBrace -> "{"
  | RBrace -> "}"
  | LBracket -> "["
  | RBracket -> "]"
  | Comma -> ","
  | Colon -> ":"

(*****************************************************************************
 * Small helper functions used by both the lexer and the parser.
 *****************************************************************************)

(**[char_to_string c] is the one-character string containing [c]. *)
let char_to_string = String.make 1

(** [string_of_char_list xs] is the string formed by the characters in [xs] 
in order. *)
let string_of_char_list xs = String.concat "" (List.map char_to_string xs)

(** [char_list_of_string s] is the list of characters occurring in [s], 
in order. *)
let char_list_of_string s = List.of_seq (String.to_seq s)

(** [is_alpha c] is true exactly when [c] is an uppercase or lowercase letter.
*)
let is_alpha c =
  match c with
    'a' .. 'z' -> true
  | 'A' .. 'Z' -> true
  | _ -> false

(** [is_digit c] is true exactly when [c] is one of '0' through '9'. *)
let is_digit c =
  match c with
    '0' .. '9' -> true
  | _ -> false

(*****************************************************************************
 * lexical error handling.
 *****************************************************************************)
exception LexicalError of string

(** [lexical_error msg] raises a LexicalError exception with the [msg]
  prefixed by "Lexical error: ". *)
let lexical_error msg = raise (LexicalError ("Lexical error: " ^ msg))

(*****************************************************************************
 * Part 1: lexer / tokenizer.
 *****************************************************************************)



(*1. add comments*)
let consume_string_literal cs =
  (* TODO: add about 10 lines of code to this function, and replace the
     failwith below with a helper-based implementation. *)
  match cs with
    '"' :: _t ->
      failwith
        "to implement and call a helper function to consume the rest of string"
  | _ -> lexical_error "Expecting string literal. No opening quote."


(*2. add comments *)
let consume_keyword _cs =
  (* TODO: implement, about 15 lines. *)
  failwith "consume_keyword unimplemented"



(**
  [consume_num cs] takes a char list [cs], beginning at the start of a JSON 
  number or at least at a position where the tokenizer has decided to attempt
  number consumption. it returns a pair [(s, rest)], where [s] is the maximal
  prefix (i.e. begining sublist) of [cs] that forms a valid JSON number 
  literal, and [rest] is the remaining sublist of [cs] after that number.

  NOTE:
    - this function is provided.
    - It'd be helpful to study every helper function defined inside it and
      understand how the number consumer works internally.
*)
let consume_num cs =
  (*
    [consume_minus xs] returns a pair [(m, rest)], where:
    - [m] is ['-'] if [xs] begins with '-', and [] otherwise
    - [rest] is the remaining list after removing the leading minus if present
  *)
  let consume_minus xs =
    match xs with
      '-' :: cs -> (['-'], cs)
    | cs -> ([], cs)
  in

  (*
    [consume_digit_list cs] returns a pair [(digits, rest)], where:
    - [digits] is the maximal prefix of [cs] consisting of digit characters
    - [rest] is the remaining list after those digits
  *)
  let consume_digit_list cs =
    let rec loop pr =
      match pr with
        (acc, []) -> (List.rev acc, [])
      | (acc, c :: cs) ->
          if is_digit c then
            loop (c :: acc, cs)
          else
            (List.rev acc, c :: cs)
    in
    loop ([], cs)
  in

  (*
    [consume_decimal xs] returns a pair [(d, rest)], where:
    - [d] is the maximal prefix of [xs] representing a decimal part starting with '.',
      or [] if no such prefix exists
    - [rest] is the remaining list after that prefix
  *)
  let consume_decimal xs =
    match xs with
      '.' :: cs ->
        let (l, cs) = consume_digit_list cs in
        ('.' :: l, cs)
    | cs -> ([], cs)
  in

  (*
    [consume_exp_sign xs] returns a pair [(s, rest)], where:
    - [s] is ['-'] or ['+'] if [xs] begins with a sign, and [] otherwise
    - [rest] is the remaining list after removing the sign if present
  *)
  let consume_exp_sign xs =
    match xs with
      '-' :: cs -> (['-'], cs)
    | '+' :: cs -> (['+'], cs)
    | cs -> ([], cs)
  in

  (*
    [consume_exp xs] returns a pair [(e, rest)], where:
    - [e] is the maximal prefix of [xs] representing an exponent part beginning
      with 'e' or 'E', or [] if no such prefix exists
    - [rest] is the remaining list after that prefix
  *)
  let consume_exp xs =
    match xs with
      c :: cs ->
        if c = 'e' || c = 'E' then
          let (sign, cs) = consume_exp_sign cs in
          let (l, cs) = consume_digit_list cs in
          (c :: sign @ l, cs)
        else
          ([], c :: cs)
    | [] -> ([], [])
  in
  let (minus, cs) = consume_minus cs in
  let (int, cs) = consume_digit_list cs in
  let (decimal, cs) = consume_decimal cs in
  let (exp, cs) = consume_exp cs in
  (string_of_char_list (minus @ int @ decimal @ exp), cs)

(**3. add comments *)
let tokenize_char_list cs =
  let rec go (cs, acc) =
    match cs with
      [] -> List.rev acc
    | '\n' :: cs -> go (cs, acc) (* ignore newlines *)
    | '{' :: cs -> go (cs, LBrace :: acc)
    (* TODO: add several more token cases here, including other single-
       character punctuation tokens and whitespace that should be
       skipped. *)
    | c :: cs ->
        if is_digit c || c = '-' then
          let (s, cs) = consume_num (c :: cs) in
          go (cs, NumLit s :: acc)
        else
          (* TODO: check for string literals and keywords here by calling
             the appropriate consumers. Otherwise, report a lexical
             error as below. *)
          lexical_error ("Unknown character " ^ char_to_string c)
  in
  go (cs, [])

(**4. add comments
*)
let tokenize (_s : string) : token list =
  (* TODO: implement in one line using char_list_of_string and
     tokenize_char_list. *)
  failwith "tokenize unimplemented"

