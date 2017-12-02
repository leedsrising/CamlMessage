(*
 * CS 3110 Fall 2017 Final Project
 *)
open Command
open State

(* [talking_repl state] is the repl that the user enters when they are
 * currently in a conversation. The user can leave the conversation, add a
 * shortcut, and define a new word, but none of the other functions will
 * do anything
 *)
let rec talking_repl state messages = 
  let input = read_line () in
  let new_state = do' (parse input) state in
  match parse input with
  | Quit -> ()
  | Leave_conversation -> 
    let () = print_string ("Leave_conversation") in
    repl new_state
  | Add_shortcut intended -> 
    let () = print_string ("Add_shortcut") in
    talking_repl new_state messages
  | Define intended -> 
    let () = print_string ("Define") in
    talking_repl new_state messages
  | Error -> 
    let () = print_string ("Error") in
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
      let () = print_string ("talking to" ^ intended) in
      talking_repl new_state ""
    | Add_friend intended -> 
      let () = print_string ("Friend") in
      repl new_state
    | Quit -> ()
    | Friends_list -> 
      let () = print_string ("Friends_list") in
      repl new_state
    | Leave_conversation -> 
      let () = print_string ("Leave_conversation") in
      repl new_state
    | Unfriend intended -> 
      let () = print_string ("Unfriend") in
      repl new_state
    | Add_shortcut intended -> 
      let () = print_string ("Add_shortcut") in
      repl new_state
    | Define intended -> 
      let () = print_string ("Define") in
      repl new_state
    | Setstatus intended -> 
      let () = print_string ("Setstatus") in
      repl new_state
    | View_requests -> 
      let () = print_string ("View_requests") in
      repl new_state
    | Error -> 
      let () = print_string ("Error") in
      repl new_state
    | Help -> 
      let () = print_string ("Error") in
      repl state
    | _ -> repl new_state

(* [make_password f] creates a password [f] for this user.
 *)
let make_password f =
  let initial_state = State.init_state "" (*IP address*)in
  try
  (*save password f*) 
  let () = print_string ("\n\nType /help to get a list of commands\n") in
  repl initial_state
  with
  | _ -> print_string "There was an error in the program \n"

(* [check_password f] checks the given password against the
 * data currently in store.
 *)
let check_password f =
  let initial_state = State.init_state "" (*IP address*) in
  try
  (*check password f*) 
  let () = print_string ("\n\nType /help to get a list of commands\n") in
  repl initial_state
  with
  | _ -> print_string "There was an error in the program \n"

(* [main ()] starts the REPL, which prompts for a game to play.
 * You are welcome to improve the user interface, but it must
 * still prompt for a game to play rather than hardcode a game file. *)
let main () =
  if (*has password*) true
  then begin
    print_string ("\n\nWelcome to the 3110 Text Adventure Game engine.\n");
    print_endline "Please enter your password.\n";
    print_string  "> ";
    match read_line () with
    | input -> check_password input
    | _ -> failwith "should never get here"
    end
  else begin
    print_string ("\n\nWelcome to the 3110 Text Adventure Game engine.\n");
    print_endline "This is your first time. Please enter a password.\n";
    print_string  "> ";
    match read_line () with
    | input -> make_password input
    | _ -> failwith "should never get here"
  end

let () = main ()