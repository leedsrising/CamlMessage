(* Because all the functions in userInterface are printing side effects,
 * it will return unit for all valid functions *)

(* [login] will first verify whether or not the inputted login
 * information is valid, and - if it is - will return the starting
 * interface that the user sees upon logging in *)
val login : command -> unit

(* [display_friends] displays the interface of friends that the
 * currently logged in user has logged in on their friends list *)
val display_friends : command -> unit

(* [display_conversation] displays the conversation interface that
 * corresponds to [person] *)
val display_conversation : command -> person -> unit

(* [close_system] closes the interface and quits the program *)
val close_system : command -> unit
