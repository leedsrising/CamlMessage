  (* [state] is an abstract type that represents the state of the program *)
  type state

  (* [person] is an abstract type that represents a person *)
  type person

  (* [user s] returns Some user that is currently “logged in” the state.
   *  If no one has logged in yet, return None. *)
  val user : state -> person option

  (* [friends s] returns the list of the friends of the person that has logged in.
   * If no one has logged in yet, throw a “Not Logged In” exception. *)
  val friends : state -> person list

  (* [conversation s] returns the messages between the person logged in and
   * another person. If no one is logged in, throw a “Not Logged In” exception.
   * If no messages are between the two people, throw a “No Conversation”
   * exception. *)
  val conversation : state -> person -> string list
