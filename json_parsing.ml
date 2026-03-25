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

 (* Helper function for slicing strings *)
let rec get_string acc str =
  match str with
  | [] ->
      lexical_error "Unterminated string literal."
  | h :: t ->
    match h with
    | '"' -> (acc, t)
    | _ -> get_string (acc ^ String.make 1 h) t

(**[consume_string_literal] takes a string and returns the Stringlit [s] and the remaining input.*)
let consume_string_literal cs =
  match cs with
    '"' :: t ->
      let (s, rest) = get_string "" t in
      (StringLit s, rest)
  | _ -> lexical_error "Expecting string literal. No opening quote."


(**[consume_keyword] takes a char array and matches it with either true, false, or null.*)
let consume_keyword cs =
  match cs with
  | 't' :: 'r' :: 'u' :: 'e' :: t ->
      (TrueTok, t)
  | 'f' :: 'a' :: 'l' :: 's' :: 'e' :: t ->
      (FalseTok, t)
  | 'n' :: 'u' :: 'l' :: 'l' :: t ->
      (NullTok, t)
  | _ ->
      lexical_error "Expecting keyword of true, false, or null."


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


  (*
  | NumLit of string    (* e.g., 3.14 represented as "3.14" *)
  | StringLit of string (* e.g., "foo" *)
  | FalseTok            (* false *)
  | TrueTok             (* true *)
  | NullTok             (* null *)*)
  
  (* [tokenize_char_list cs] converts a char list into a list of JSON tokens.
    If the input is empty, it returns an empty list.
    Otherwise, it skips leading whitespace. If the next character begins a valid
    token, it consumes that token.
    Then it continues tokenizing the remaining input.
    Finally, it returns the list of tokens in order.
    [tokenize_char_list cs] raises [LexicalError] if the next character that
    isn't whitespace doesn't start a valid JSON token.
  *)

  let tokenize_char_list cs =
  let rec go (cs, acc) =
    match cs with
      [] -> List.rev acc
    | '\n' :: cs -> go (cs, acc) (* ignore newlines *)
    | '{' :: cs -> go (cs, LBrace :: acc)
    (* TODO: add several more token cases here, including other single-
       character punctuation tokens and whitespace that should be
       skipped. *)
    | ' ' :: cs -> go (cs, acc)
    | '}' :: cs -> go (cs, RBrace :: acc)
    | '[' :: cs -> go (cs, LBracket :: acc)
    | ']' :: cs -> go (cs, RBracket :: acc)
    | ',' :: cs -> go (cs, Comma :: acc)
    | ':' :: cs -> go (cs, Colon :: acc)
    | c :: cs ->
        if is_digit c || c = '-' then
          let (s, cs) = consume_num (c :: cs) in
          go (cs, NumLit s :: acc)
          (* TODO: check for string literals and keywords here by calling
             the appropriate consumers. Otherwise, report a lexical
             error as below. *)
        else if c = '"' then
          let (tok, cs) = consume_string_literal(c :: cs) in go (cs, tok :: acc)
        else if is_alpha c then
          let (tok, cs) = consume_keyword (c :: cs) in
          go (cs, tok :: acc);

        else
          lexical_error ("Unknown character " ^ char_to_string c)
  in
  go (cs, [])

(**4. [tokenize (s : string) : token list] converts an input string into a list
  of tokens.
  It repeatedly skips whitespace and then consumes the next token until
  the input is exhausted.
  It raises [LexicalError] if it finds invalid input.
*)
let tokenize (s : string) : token list =
  tokenize_char_list (char_list_of_string s)

