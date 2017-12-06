open OUnit2
open MessageTransformer

(*******************************************************************)
(* Tests for word_is_valid*)
(*******************************************************************)

let send_tests = [
  "apple" >:: (fun _ -> assert_equal "apple"  (send "apple"));
  "banana" >:: (fun _ -> assert_equal "banana"  (send "banana"));
]


let tests =
  "test suite for messageTransformer"  >::: List.flatten [
    send_tests; ]

let _ = run_test_tt_main tests
