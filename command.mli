type command = Talk of string | Friend of (string*int) | Quit | Friends_list | Help |
Leave_conversation | Unfriend of string| Add_shortcut of (string*string) | 
Define of string | Setstatus of string | View_requests | Accept  of string | Error

type command_type = { name:string; min_args:int; desc:string; usage:string; 
aliases:string list; builder:(string list -> command)  }

val command_help_message : string

(* [interpret_command c s] calls the command represented
 *  by the users input command [c] which returns a new system [state].
 *  - If c is an unrecognized command call [handle_unknown_command c]. *)
(* val interpret_usage : command_type -> command *)

(* [quit s] is the state with a flag that terminates the program. *)
val parse : string -> command

