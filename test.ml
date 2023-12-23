open Experiment
open OUnit2

(** The vast majority of our program, with the exception of the experiment 
    module, could not be tested with ounit, and were all tested manually by 
    running the program. All non-helper functions in experiment module are 
    tested, with all possible types of input and output combinations covered. 
    Test cases were developed with the black box method.*)

(** [make_empty_test name input expected_output] constructs an OUnit
    test named [name] that asserts the equality of [expected_output]
    with [is_empty input]. *)
let make_empty_test 
    (name : string) 
    (input: int option list) 
    (expected_output : bool) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (is_empty input))

(** [make_compatible_test name answer input expected_output] constructs an OUnit
    test named [name] that asserts the equality of [expected_output]
    with [is_compatible answer input]. *)
let make_compatible_test 
    (name : string) 
    (answer : int option list)
    (input: int option list) 
    (expected_output : bool) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (is_compatible answer input))

(** [make_complete_test name input expected_output] constructs an OUnit
    test named [name] that asserts the equality of [expected_output]
    with [is_complete input]. *)
let make_complete_test 
    (name : string) 
    (input: int option list) 
    (expected_output : bool) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (is_complete input))

(** [make_build_test name clues gaps length expected_output] constructs an OUnit
    test named [name] that asserts the equality of [expected_output]
    with [build_line input], which is made to return [None] 
    if an error is caught. *)
let make_build_test 
    (name : string) 
    (clues: int list)
    (gaps: int list)
    (length: int) 
    (expected_output : int option list) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (try build_line clues gaps length with 
          | _ -> [None]))

(** [make_match_test name answer input expected_output] constructs an OUnit
    test named [name] that asserts the equality of [expected_output]
    with [match_line input1 input2]. *)
let make_match_test 
    (name : string) 
    (input1 : int option list)
    (input2: int option list) 
    (expected_output : int option list) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (match_lines input1 input2))

(** [make_fill_test name clues input expected_output] constructs an OUnit
    test named [name] that asserts the equality of [expected_output]
    with [fill_line clues input], which returns 
    [Some 911] if an error is caught *)
let make_fill_test 
    (name : string) 
    (clues : int list)
    (input: int option list) 
    (expected_output : int option list) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (try fill_line clues input with 
          | _ -> [Some 911]))

let empty_tests = [
  make_empty_test "Nones" [None; None; None] true;
  make_empty_test "More Nones" [None; None; None; None; None; None; None; None; 
                                None] 
    true;
  make_empty_test "Some 0s" [Some (-1); Some (-1); Some (-1)] true;
  make_empty_test "Mixed" [Some (-1); None; Some (-1)] true;
  make_empty_test "Not empty" [Some (-1); None; Some 1] false;
]

let compatible_tests = [
  make_compatible_test "Same" [Some (-1); Some 1; Some (-1)] 
    [Some (-1); Some 1; Some (-1)] true;
  make_compatible_test "Different" [Some 1; Some 1; Some (-1)] 
    [Some (-1); Some 1; Some (-1)] false;
  make_compatible_test "Matched to empty" [Some (-1); Some 1; Some (-1)] 
    [None; None; None] true;
  make_compatible_test "Mixed matches" [Some (-1); Some 1; Some (-1)] 
    [None; Some 1; Some (-1)] true;
]

let complete_tests = [
  make_complete_test "Empty" [None; None; None] false;
  make_complete_test "Complete" [Some (-1); Some 1; Some (-1)] true;
  make_complete_test "Incomplete" [None; Some 1; Some (-1)] false;
]

let build_tests = [
  make_build_test "Error1" [1;2] [1;1] 4 [None];
  make_build_test "PassFull1" [1;2] [1;1] 5 
    [Some (-1); Some 1; Some (-1); Some 1; Some 1];
  make_build_test "PassExtra1" [1;2] [1;1] 8 
    [Some (-1); Some 1; Some (-1); Some 1; 
     Some 1; Some(-1); Some(-1); Some(-1)];
  make_build_test "Error2" [2;1;3] [1;2;1] 5 [None];
  make_build_test "PassFull2" [2;1;3] [1;2;1] 10 
    [Some (-1); Some 1; Some 1; Some (-1); Some (-1); 
     Some 1; Some (-1); Some 1; Some 1; Some 1];
  make_build_test "PassExtra2" [2;1;3] [1;2;1] 12 
    [Some (-1); Some 1; Some 1; Some (-1); Some (-1); 
     Some 1; Some (-1); Some 1; Some 1; Some 1; Some(-1); Some(-1)];
]

let match_tests = [
  make_match_test "Same" [Some (-1); Some 1; Some (-1)] 
    [Some (-1); Some 1; Some (-1)] [Some (-1); Some 1; Some (-1)];
  make_match_test "Different" [Some 1; Some 1; Some (-1)] 
    [Some (-1); Some 1; Some (-1)] [None; Some 1; Some (-1)];
  make_match_test "Matched to empty" [Some (-1); Some 1; Some (-1)] 
    [None; None; None] [None; None; None];
  make_match_test "Mixed matches" [Some (-1); Some 1; Some (-1)] 
    [None; Some 1; Some 1] [None; Some 1; None];
]

let fill_line_tests = [
  make_fill_test "Full" [5] [None;None;None;None;None] 
    [Some 1; Some 1; Some 1; Some 1; Some 1];
  make_fill_test "1 clue 1" [4] [None;None;None;None;None] 
    [None; Some 1; Some 1; Some 1; None];
  make_fill_test "1 clue 2" [3] [None;None;None;None;None] 
    [None; None; Some 1; None; None];
  make_fill_test "1 clue 3" [4] [None;Some (-1);None;None;None] 
    [Some 911];
  make_fill_test "1 clue 4" [3] [None;Some (-1);None;None;None] 
    [Some (-1); Some (-1); Some 1; Some 1; Some 1];
  make_fill_test "1 clue 5" [3] [None;Some (1);None;None;None] 
    [None; Some 1; Some 1; None; Some (-1)];
  make_fill_test "Invalid 1" [5] [None;Some (-1);None;None;None] 
    [Some 911];
  make_fill_test "Invalid 2" [6] [None;None;None;None;None] 
    [Some 911];
  make_fill_test "Invalid 3" [1; 3] [None;Some 1;None;None;None] 
    [Some 911];
  make_fill_test "Invalid 4" [2; 1] [None;Some (-1);None;None;None] 
    [Some 911];
  make_fill_test "Invalid 5" [2; 3] [None;None;None;None;None] 
    [Some 911];  
]
let tests =
  "test suite for project"  >::: List.flatten [
    empty_tests;
    compatible_tests;
    complete_tests;
    build_tests;
    match_tests;
    fill_line_tests;
  ]
let _ = run_test_tt_main tests