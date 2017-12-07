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
      repl ()
    | Friend intended -> 
      print_endline ("Friend");
      repl ()
    | Quit -> return_unit
    | Friends_list -> 
      print_endline ("Friends_list");
      print_endline (current_friends !state_ref);
      repl  ()
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
    | Accept _ -> 
      print_endline ("Accept Req");
      repl ()
    | Message m ->
      repl ()
    | Error -> 
      print_endline ("Error");
      repl ()
    | Help -> 
      print_endline command_help_message;
      repl ()

(* [make_password username password] stores the users username and password in a text file "login.txt"
 * and sets the state username to [username]
 *)
let make_password username password =
  let () =
    let oc = open_out "login.txt" in   
      fprintf oc "%s\n" (username);  
      fprintf oc "%s\n" (password);  
      close_out oc in 
    try
    let () = print_string ("\n\nType /help to get a list of commands\n") in
      state_ref := {!state_ref with username = username };
      Lwt_main.run (Lwt.join [(start_server ()); repl ()]) 
    with
    | _ ->           
      state_ref := {!state_ref with username = username };
      Lwt_main.run (Lwt.join [(start_server ()); repl ()]) 
  
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

let rec get_username file dir handler = 
  try
    let next_file = handler |> Unix.readdir in
      if next_file = file then let ic = open_in next_file in
        try 
          let line1 = input_line ic in  
            let username = line1 in
            close_in ic; username
        with e ->
          close_in_noerr ic;
          raise e            
      else get_username file dir handler
  with
  | e -> Unix.closedir handler; "bob_invalid"

  (* [prompt_for_password] prompts the user for their password then 
   * checks the given password against the data stored in "login.txt"
   *)
let rec prompt_for_password () = 
  begin
    print_endline "Please enter your password.\n";
    print_string  "> ";
    match read_line () with
    | input -> let d_handle = Unix.getcwd () |> Unix.opendir in
      let password_matches = check_password_helper input ("login.txt") (Unix.getcwd ()) d_handle in
        if password_matches then let username = get_username "login.txt" (Unix.getcwd ()) d_handle in 
          try
            let () = print_string ("\n\nType /help to get a list of commands\n") in
            state_ref := {!state_ref with username = username};
            Lwt_main.run (Lwt.join [(start_server ()); repl ()]) 
          with
          | _ ->         
            state_ref := {!state_ref with username = username};
            Lwt_main.run (Lwt.join [(start_server ()); repl ()]) 
        else print_endline "Incorrect password.\n";
          prompt_for_password ()
          | _ -> failwith "should never get here"
  end

(* [main ()] starts the REPL, which prompts for a user to log in.
*)
let main () = 
  print_endline ("\n\nWelcome to CamlMsg!\n");
  if password_exists
  then prompt_for_password ()
  else begin
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