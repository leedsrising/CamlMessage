(*
 * CS 3110 Fall 2017 Final Project
 *)
open Command
open State
open Unix
open MessageTransformer
open Networking2
open Lwt
open Printf

(* [talking_repl state] is the repl that the user enters when they are
 * currently in a conversation. The user can leave the conversation, add a
 * shortcut, and define a new word, but none of the other functions will
 * do anything
 *)
(* let rec talking_repl  messages = 
  let input = read_line () in
  let new_state = do' (parse input) ! in
  match parse input with
  | Quit -> return_unit
  | Leave_conversation -> 
    print_endline ("Leave_conversation");
    repl  ()
  | Add_shortcut intended -> 
    print_endline ("Add_shortcut");
    talking_repl new_state messages
  | Define intended -> 
    print_endline ("Define");
    talking_repl new_state messages
  | Error -> 
    print_endline ("Error");
    talking_repl state messages
  | _ -> talking_repl state messages *)

(* [repl state] is the main repl that the user enters when they are
 * not in a conversation. The user can do any of the commmands presented.
 *)

let rec repl () =
  Lwt_io.print "> " >>=
  fun () -> Lwt_io.read_line Lwt_io.stdin >>= 
  fun input ->
   state_ref := do' (parse input) !state_ref;
    match parse input with
    | Talk intended -> 
      print_endline ("talking to " ^ intended);
      repl ()
    | Friend intended -> 
      print_endline ("Friend");
      repl ()
    | Quit -> return_unit
    | Friends_list -> 
      print_endline ("Friends_list");
      repl ()
    | Leave_conversation -> 
      print_endline ("Leave_conversation");
      repl ()
    | Unfriend intended -> 
      print_endline ("Unfriend");
      repl ()
    | Add_shortcut intended -> 
      print_endline ("Add_shortcut");
      repl ()
    | Define intended -> 
      print_endline ("Define");
      repl ()
    | Setstatus intended -> 
      print_endline ("Setstatus");
      repl ()
    | View_requests -> 
      print_endline ("View_requests");
      print_endline (current_requests !state_ref);
      repl ()
    | Error -> 
      print_endline ("Error");
      repl ()
    | Help -> 
      print_endline command_help_message;
      repl ()

  (* [make_password f] creates a password [f] for this user.
   *)
   let make_password username password =
      let () =
        let oc = open_out "login.txt" in   
        fprintf oc "%s\n" (username);  
        fprintf oc "%s\n" (password);  
        close_out oc in 
    try
    (*save password f*) 
    let () = print_string ("\n\nType /help to get a list of commands\n") in
      state_ref := {!state_ref with username = "test_user"};
      Lwt_main.run (Lwt.join [(start_server ()); repl ()]) 
    with
    | _ ->           
      state_ref := {!state_ref with username = "test_user"};
      Lwt_main.run (Lwt.join [(start_server ()); repl ()]) 
  
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
      if next_file = file then let ic = open_in next_file in
          try 
            let line1 = input_line ic in  
              let line2 = input_line ic in 
              let password_correct = password = line2 in 
              close_in ic; password_correct
  
          with e ->
              close_in_noerr ic;
              raise e            
      else check_password_helper password file dir handler
    with
    | _ -> Unix.closedir handler; false
  
  let rec prompt_for_password () = 
    begin
      print_string ("\n\nWelcome to the 3110 Text Adventure Game engine.\n");
      print_endline "Please enter your password.\n";
      print_string  "> ";
      match read_line () with
      | input -> let check_password f =
        let d_handle = Unix.getcwd () |> Unix.opendir in
          let password_matches = check_password_helper f ("login.txt") (Unix.getcwd ()) d_handle in
          if password_matches then 
            try
            let () = print_string ("\n\nType /help to get a list of commands\n") in
            state_ref := {!state_ref with username = "test_user"};
            Lwt_main.run (Lwt.join [(start_server ()); repl ()]) 
            with
            | _ ->         
              state_ref := {!state_ref with username = "test_user"};
              Lwt_main.run (Lwt.join [(start_server ()); repl ()]) 
          else prompt_for_password () in 
          check_password input
          | _ -> failwith "should never get here"
      end

(* [main ()] starts the REPL, which prompts for a game to play.
 * You are welcome to improve the user interface, but it must
 * still prompt for a game to play rather than hardcode a game file. *)
let main () = 
  print_endline ("\n\nWelcome to CamlMsg!\n");
  if password_exists
  then prompt_for_password ()
  else begin
    print_string ("\n\nWelcome to the 3110 Text Adventure Game engine.\n");
    print_endline "This is your first time. Please enter a username.\n";
    print_string  "> ";
    match read_line () with
    | username -> print_endline "Please enter a password.\n";
      print_string  "> ";
      match read_line () with 
      | password -> make_password username password
    | _ -> failwith "should never get here"
  end

let () =  main ()