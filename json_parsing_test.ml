open OUnit2
open Json_parsing


(* This file provides some example tests, and you need to add more. *)

(* [make_test_1arg test_info f argument expected] is a helper function that
   creates a test case for function [f] with the input [argument] and
   the [expected] output. [test_info] gives a test description, useful
   for identifying the test case in the error message if the test is
   not passed. *)
let make_test_1arg test_info f argument expected =
  test_info >:: (fun _ -> assert_equal expected (f argument))

let make_exn_test_1arg test_info f argument expected_exn =
  test_info >:: (fun _ -> assert_raises expected_exn (fun () -> f argument))


(* 1 *)
let tests_consume_string_literal = "test suite for consume_string_literal" >::: [
  make_test_1arg
    "consume_string_literal: hello"
    consume_string_literal
    (char_list_of_string "\"hello\"")
    (StringLit "hello", []);
  make_test_1arg
    "consume_string_literal: hello! "
    consume_string_literal
    (char_list_of_string "\"hello\"! ")
    (StringLit "hello", ['!'; ' ']);
] (* add more tests *)


let tests_consume_string_literal_exceptions =
  "test suite for consume_string_literal exceptions" >::: [
    make_exn_test_1arg
      "no opening quote"
      consume_string_literal
      (char_list_of_string "hello")
      (LexicalError "Lexical error: Expecting string literal. No opening quote.");
  ]


(* 2 *)
let tests_consume_keyword = "test suite for consume_keyword" >::: [
  make_test_1arg
    "consume_keyword: true; "
    consume_keyword
    (char_list_of_string "true; ")
    (TrueTok, [';';' ']);
] (* add more tests *)



(* 4 *)
let tests_tokenize = "test suite for tokenize" >::: [
  make_test_1arg
    "tokenize: simple object"
    tokenize
    "{ \"x\" : true }"
    [LBrace; StringLit "x"; Colon; TrueTok; RBrace];
] (* add more tests *)


let all_tests = "all tests" >::: [
  tests_consume_string_literal;
  tests_consume_string_literal_exceptions;
  tests_consume_keyword;
  tests_tokenize;
]

(* Run all tests *)
let () =
  run_test_tt_main all_tests