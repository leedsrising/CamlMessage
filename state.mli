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
  requests: person list
  }

(* [current_friends s] takes in the current state of this user and returns
 * their friendlist in string version
 *) 
val current_friends : state -> string

(* [current_requests s] takes in the current state of this user and returns
 * their requests in string version
 *) 
val current_requests : state -> string
  
(* [state_ref] represents a state reference of the initial state
 *)
val state_ref : state ref

(* [do' cmd st] changes the state according to a command. See details in
 * state.mli 
 *)
val do': Command.command -> state -> state

(* [get_friend_by_name name st] is the friend (option) named [name] according
 * to st. Returns None if no friend is found
 *) 
val get_friend_by_name : string -> state -> person option

(* [get_friend_by_name ip st] is the friend (option) with ip [ip] according
 * to st. Returns None if no friend is found
 *) 
val get_friend_by_ip : string -> state -> person option

(* [current_friends s] takes in the current state of this user and returns
 * their friendlist in string version
 *) 
val current_friends : state -> string

(* [current_requests s] takes in the current state of this user and returns
 * their requests in string version
 *) 
val current_requests : state -> string

(* [chat_history s] takes in the current state of this user and returns
 * their chat history in string version
 *) 
val chat_history : state -> string

(* [shortcuts s] takes in the state list of this user and returns the 
 * string version of their current shortcuts
 *) 
(* val shortcuts : state -> string *)

(* [add_friend friend st] returns the new state with [friend] added onto
 * this user's friends list
 *)
val request_friend : string -> int -> state -> state

(* [accept_friend_req name st] accepts the friend request from the user
 * with name [name]. If there is not a request from the user [name], then
 * the user receieves a message saying that no request was found.
 *)
val accept_friend_req : string -> state -> state

(* [add_friend_req name ip port st] changes this user's state to include the
 * new friend request from outside user with name [name]
 *)
val add_friend_req : string -> string -> int -> state -> state

(* [add_friend name ip port st] changes this user's friends list in state to 
 * include the new friend from outside user with name [name]
 *)
val add_friend : string -> string -> int -> state -> state

(* [remove_friend friend st] returns the new state with [friend] taken off 
 * this user's friends list
 *)
val remove_friend : string -> state -> state

(* [clear_messages st] returns the new state with the current user's messages
 * list cleared
 *)
val clear_messages : state -> state

(* [set_status intended st] returns the new state with the current user's status
 * set to intended
 *)
val set_status : string -> state -> state

(* [add_shortcut shortcut word st] returns the new state with the current user's status
 * set to intended
 *)
(* val add_shortcut : string -> string -> state -> state *)

(* [add_shortcut shortcut word st] returns the new state with the current user's status
 * set to intended
 *)
(* val define : string -> state -> state *)

(* [handle_remote_cmd net_state msg] handles a command from a remote client so
 * that it can read what other users are doing (i.e. telling when another user
 * has accepted this user's friend request)
 *)
val handle_remote_cmd : Networking2.net_state ref -> string -> unit

val print_messages : string list -> string

val get_messages_for_friend : string -> string list










