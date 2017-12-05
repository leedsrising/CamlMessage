type person = {
  id : string;
  name : string;
  port : int;
  }
  
(* type state represents the current state of this user *)
type state = {
  username: string;
  status : string;
  friends_list : person list;
  messages : (person * string list) list;
  current_person_being_messaged : person option;
  requests: person list;
  dictionary : string list;
  shortcut_list : (string * string) list
  }

val current_friends : state -> string

val current_requests : state -> string
  
val state_ref : state ref

val do': Command.command -> state -> state

val state_ref : state ref

val get_friend_by_name : string -> state -> person option

val get_friend_by_ip : string -> state -> person option

val current_friends : state -> string

val current_requests : state -> string

val chat_history : state -> string

val shortcuts : state -> string

val request_friend : string -> int -> state -> state

val accept_friend_req : string -> state -> state

val add_friend_req : string -> string -> int -> state -> state

val add_friend : string -> string -> int -> state -> state

val remove_friend : string -> state -> state

val clear_messages : state -> state

val set_status : string -> state -> state

val add_shortcut : string -> string -> state -> state

val define : string -> state -> state

val handle_remote_cmd : Networking2.net_state ref -> string -> unit











