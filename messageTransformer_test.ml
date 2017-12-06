open OUnit2
open MessageTransformer

(*******************************************************************)
(* Tests for word_is_valid*)
(*******************************************************************)

let send_tests = [
  "apple" >:: (fun _ -> assert_equal "apple"  (send "apple"));
  "banana" >:: (fun _ -> assert_equal "banana"  (send "banana"));
  "rotten banana" >:: (fun _ -> assert_equal "banana"  (send "nanana"));
  "run fast" >:: (fun _ -> assert_equal "got to go"  (send "gtg"));
  "awesome!" >:: (fun _ -> assert_equal "don't forget to be awesome"  (send "dftba"));
]


let tests =
  "test suite for messageTransformer"  >::: List.flatten [
    send_tests; ]

let _ = run_test_tt_main tests
