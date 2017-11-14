open State

(* [interpret_command c s] calls the command represented
 *  by the users input command [c] which returns a new system [state].
 *  - If c is an unrecognized command call [handle_unknown_command c]. *)
val interpret_command : string -> state -> state

(* [quit s] is the state with a flag that terminates the program. *)
val quit : state -> state

(* [start_convo c s] alters the state by creating a new conversation.
 *  - If c does not contain a valid user identifier return the same state
 *    with the side effect of printing "Unknown User ID" to the
 *    user interface. *)
val start_convo : string -> state -> state

(* [display_friends_list s] returns the same state with the side effect
 * of printing the friends list to the user interface. *)
val display_friends_list : state -> state

(* [add_friend f s] creates a new friends list from the
 *  old friends list with f appended to it. *)
val add_friend : string -> state -> state

(* [remove_friend f s] creates a new friends list from the
 *  old friends list by removing f. *)
val remove_friend : string -> state -> state

(* [handle_unknown_command c s] returns the same state with the side effect
 * of printing "Unrecognized command " to the user interface. *)
val handle_unknown_command : string -> state -> state

(* [leave_convo s] alters the state by leaving the current conversation.
 * This also disconnects from the remote server. *)
val leave_convo : state -> state
