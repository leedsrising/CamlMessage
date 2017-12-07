open Lwt

(* Type command represents a command by the user to do some action while not in conversation.
 * If the user's request is not read as being one of the assigned commands, then it is
 * seen and treated as an error. 
 *)
type command = Talk of string | Friend of (string*int) | Quit | Friends_list | Help | Message_history of string |
                Leave_conversation | Unfriend of string| Add_shortcut of (string*string) | 
                Define of string | Setstatus of string | View_requests | Accept of string | Error

(* spec command_type
 *)             
type command_type = { name:string; min_args:int; desc:string; usage:string; 
aliases:string list; builder:(string list -> command)  }

let commands = [

{name = "/talk"; min_args = 1; desc = "Starts a conversation with a friend";
  usage = "/talk <friend>"; aliases = [];
  builder = (fun args -> Talk (List.nth args 1))};
  
{name = "/friend"; min_args = 2; desc = "Add a new friend";
  usage = "/friend <ip address> <port>"; aliases = [];
  builder = (fun args -> let port = int_of_string (List.nth args 2) 
  in Friend ((List.nth args 1), port))};
  
{name = "/quit"; min_args = 0; desc = "Quit the program";
  usage = "/quit"; aliases = [];
  builder = (fun args -> Quit)};

{name = "/friends"; min_args = 0; desc = "View your friends list";
  usage = "/friends"; aliases = [];
  builder = (fun args -> Friends_list)};

{name = "/help"; min_args = 0; desc = "Display the help message";
  usage = "/help"; aliases = [];
  builder = (fun args -> Help)};

{name = "/leave"; min_args = 0; desc = "Leave the conversation, if any";
  usage = "/leave"; aliases = [];
  builder = (fun args -> Leave_conversation)};

{name = "/unFriend"; min_args = 1; desc = "Remove someone from your friends list";
  usage = "/unFriend <friend>"; aliases = [];
  builder = (fun args -> Unfriend (List.nth args 1))};

{name = "/define"; min_args = 1; desc = "Add a word to the dictionary";
  usage = "/define <word>"; aliases = [];
  builder = (fun args -> Define (List.nth args 1))};

  (*TODO: Fix args *)
{name = "/setStatus"; min_args = 1; desc = "Set your status message";
  usage = "/setStatus <new status>"; aliases = [];
  builder = (fun args -> Setstatus (List.nth args 1))};

{name = "/requests"; min_args = 0; desc = "View your friend requests";
  usage = "/requests"; aliases = [];
  builder = (fun args -> View_requests)};

{name = "/accept"; min_args = 1; desc = "Accept a pending friend request";
  usage = "/accept <username>"; aliases = [];
  builder = (fun args -> Accept (List.nth args 1))};

{name = "/addShortcut"; min_args = 2; desc = "Add a shortcut (abbrev.) for a word";
    usage = "/addShortcut <shortcut> <replacement>"; aliases = [];
    builder = (fun args -> Add_shortcut ((List.nth args 1), (List.nth args 2)))};
]

let command_help_message = "\n--- CamlMsg Help ---\n\n" ^ (List.fold_left (^) "" 
  (List.mapi (fun i cmd -> (string_of_int (i + 1)) ^ ". " ^
  cmd.usage ^ "\n" ^ cmd.desc ^ "\n\n") commands)) ^ "--- End of Help ---"
  
let invalid_usage cmd = 
  (ignore (Lwt_io.printl ("Usage: " ^ cmd.usage));  Error)

(* [parse str] takes in a string from this user and assigns it to either a valid
 * command as described above or to an error. 
 *)
let parse str =
  let trimmed = String.trim str in
  let split = Str.split (Str.regexp " ") trimmed in
  let usr_cmd = List.hd split in
  (*TODO: aliases *)
  match List.find_opt (fun cmd -> (String.lowercase_ascii cmd.name) 
    = (String.lowercase_ascii usr_cmd)) commands with
  | Some cmd -> 
    if List.length split - 1 >= cmd.min_args then
      try (cmd.builder split) with e -> invalid_usage cmd
    else 
      if String.get trimmed 0 = '/' then
        invalid_usage cmd (*TODO: handle message *)
      else  invalid_usage cmd
  | None -> Error