(*
 * CS 3110 Fall 2017 Final Project
 *)
open Command
open State
open Unix
open MessageTransformer
open Networking2
open Lwt

(* [talking_repl state] is the repl that the user enters when they are
 * currently in a conversation. The user can leave the conversation, add a
 * shortcut, and define a new word, but none of the other functions will
 * do anything
 *)
let rec talking_repl state messages = 
  let input = read_line () in
  let new_state = do' (parse input) state in
  match parse input with
  | Quit -> return_unit
  | Leave_conversation -> 
    print_endline ("Leave_conversation");
    repl new_state
  | Add_shortcut intended -> 
    print_endline ("Add_shortcut");
    talking_repl new_state messages
  | Define intended -> 
    print_endline ("Define");
    talking_repl new_state messages
  | Error -> 
    print_endline ("Error");
    talking_repl state messages
  | _ -> talking_repl state messages

(* [repl state] is the main repl that the user enters when they are
 * not in a conversation. The user can do any of the commmands presented.
 *)

and repl state =
  let input = read_line () in
  let new_state = do' (parse input) state in
    match parse input with
    | Talk intended -> 
      print_endline ("talking to" ^ intended);
      talking_repl new_state ""
    | Add_friend intended -> 
      print_endline ("Friend");
      repl new_state
    | Quit -> return_unit
    | Friends_list -> 
      print_endline ("Friends_list");
      repl new_state
    | Leave_conversation -> 
      print_endline ("Leave_conversation");
      repl new_state
    | Unfriend intended -> 
      print_endline ("Unfriend");
      repl new_state
    | Add_shortcut intended -> 
      print_endline ("Add_shortcut");
      repl new_state
    | Define intended -> 
      print_endline ("Define");
      repl new_state
    | Setstatus intended -> 
      print_endline ("Setstatus");
      repl new_state
    | View_requests -> 
      print_endline ("View_requests");
      repl new_state
    | Error -> 
      print_endline ("Error");
      repl new_state
    | Help -> 
      print_endline command_help_message;
      repl state
    | _ -> repl new_state

(* (* [make_password f] creates a password [f] for this user.
 *)
let make_password f =
  let initial_state = State.init_state "" (*IP address*)in
  try
  (*save password f*) 
  let () = Lwt_io.printl ("\n\nType /help to get a list of commands\n") in
  repl initial_state
  with
  | _ -> Lwt_io.printl "There was an error in the program \n"

(* [check_password f] checks the given password against the
 * data currently in store.
 *)

let rec dir_helper (file : string) (dir : string) (handler : dir_handle) =
  try
    let next_file = handler |> Unix.readdir in
    if next_file = file then true
    else dir_helper file dir handler
  with
  | _ -> Unix.closedir handler; false

let password_exists =
  let d_handle = Unix.getcwd () |> Unix.opendir in
  dir_helper ("login.txt") (Unix.getcwd ()) d_handle

let rec check_password_helper (password : string) (file : string) (dir : string) (handler : dir_handle) : bool =
  try
    let next_file = handler |> Unix.readdir in
    if next_file = file then let l1 = words_in_file next_file in
      (List.hd l1 = password) || (List.hd (List.tl l1) = password)
    else check_password_helper file dir handler
  with
  | _ -> Unix.closedir handler; false *)

(* let check_password f =
  let initial_state = State.init_state "" (*IP address*) in
  let d_handle = Unix.getcwd () |> Unix.opendir in
    let password_matches = check_password_helper f ("login.txt") (Unix.getcwd ()) d_handle in
    if password_matches then 
      try
      (*check password f*)
      let () = Lwt_io.printl ("\n\nType /help to get a list of commands\n") in
      repl initial_state
      with
      | _ -> Lwt_io.printl "There was an error in the program \n"
    else () *)

(* [main ()] starts the REPL, which prompts for a game to play.
 * You are welcome to improve the user interface, but it must
 * still prompt for a game to play rather than hardcode a game file. *)
let main () =
  print_endline ("\n\nWelcome to CamlMsg!\n");
  (* if password_exists
  then begin
    print_endline "Please enter your password.\n";
    Lwt_io.printl  "> ";
    match read_line () with
    | input -> check_password input
    | _ -> failwith "should never get here"
    end
  else begin
    print_endline "This is your first time. Please enter a password.\n";
    Lwt_io.printl  "> ";
    match read_line () with
    | input -> make_password input
    | _ -> failwith "should never get here"
  end *)
  Lwt_main.run (Lwt.join [start_server (); repl (init_state "testUsr")])




let () = main ()