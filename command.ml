(* A type exposed and defined in a .mli file must also be defined in
 * the corresponding .ml file.  So you must repeat the definition
 * of [command] here.  This helps OCaml achieve something called
 * "separate compilation", which you could google for.  Yes,
 * it's a little bit annoying, but there is a good reason for it. *)
type command = Talk of string | Friend of string | Quit | Friends_list | Help |
                Leave_conversation | Add_friend of string | Unfriend of string| Add_shortcut of (string*string) | 
                Define of string | Setstatus of string | View_requests | Error

type command_type = { name:string; min_args:int; usage:string; 
aliases:string list; builder:(string list -> command)  }


let commands = [

{name = "/talk"; min_args = 1; 
  usage = "/talk <friend>"; aliases = [];
  builder = (fun args -> Talk (List.nth args 1))};
  
{name = "/friend"; min_args = 1; 
  usage = "/friend <friend>"; aliases = [];
  builder = (fun args -> Talk (List.nth args 1))};
  
{name = "/quit"; min_args = 0; 
  usage = "/quit"; aliases = [];
  builder = (fun args -> Quit)};

{name = "/friends"; min_args = 0; 
  usage = "/friends"; aliases = [];
  builder = (fun args -> Friends_list)};

{name = "/help"; min_args = 0; 
  usage = "/help"; aliases = [];
  builder = (fun args -> Help)};

{name = "/leave"; min_args = 0; 
  usage = "/leave"; aliases = [];
  builder = (fun args -> Leave_conversation)};

{name = "/unFriend"; min_args = 1; 
  usage = "/unFriend <friend>"; aliases = [];
  builder = (fun args -> Unfriend (List.nth args 1))};

{name = "/define"; min_args = 1; 
  usage = "/define <word>"; aliases = [];
  builder = (fun args -> Define (List.nth args 1))};

  (*TODO: Fix args *)
{name = "/setStatus"; min_args = 1; 
  usage = "/setStatus <new status>"; aliases = [];
  builder = (fun args -> Setstatus (List.nth args 1))};

{name = "/requests"; min_args = 0; 
  usage = "/requests"; aliases = [];
  builder = (fun args -> View_requests)};

{name = "/addShortcut"; min_args = 2; 
    usage = "/addshortcut <shortcut> <replacement>"; aliases = [];
    builder = (fun args -> Add_shortcut ((List.nth args 1), (List.nth args 2)))};

]
  
let parse str =
  let trimmed = String.trim str in
  let split = Str.split (Str.regexp " ") trimmed in
  let usr_cmd = List.hd split in
  (*TODO: aliases *)
  match List.find_opt (fun cmd -> (String.lowercase_ascii cmd.name) 
    = (String.lowercase_ascii usr_cmd)) commands with
  | Some cmd -> 
    if List.length split - 1 >= cmd.min_args then
    (cmd.builder split) else (print_endline ("Usage: " ^ cmd.usage); Error)
  | None -> Error