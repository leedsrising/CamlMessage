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

