open OUnit2
open MessageTransformer

(*******************************************************************)
(* Tests for word_is_valid*)
(*******************************************************************)

let word_is_valid_tests = [
  "apple" >:: (fun _ -> assert_equal true  (word_is_valid "apple" d));
  "banana" >:: (fun _ -> assert_equal true  (word_is_valid "banana" d));
  "weird fruit" >:: (fun _ -> assert_equal true  (word_is_valid "applebanana" d));
]


let tests =
  "test suite for messageTransformer"  >::: List.flatten [
    word_is_valid_tests; ]

let _ = run_test_tt_main tests
