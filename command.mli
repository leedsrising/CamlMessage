type command = Talk of string | Friend of (string*int) | Quit | Friends_list 
| Help | Message_history of string | Clear_history of string | Leave_conversation | Unfriend of string 
| Add_shortcut of (string*string) | Define of string | Setstatus of string 
| View_requests | Accept of string | Message of string | Error | Encrypt_messages of string * string 

type command_type = { name:string; min_args:int; desc:string; usage:string; 
aliases:string list; builder:(string list -> command)  }

(* [command_help_message] is the message that the user receives when they do 
 * /help. It is a string that clearly shows all the possible commands that the user
 * can do while in camlmsg
 *)
val command_help_message : string

(* [quit s] is the state with a flag that terminates the program. *)
val parse : string -> command

