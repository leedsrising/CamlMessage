open Command

(* [id] represents the identification of someone through their IP *)

type person = {
  id : string;
  name : string;
}

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

(* The following function converts a json into a OCaml type that is defined
 * above. *)

(* The following functions returns information in a state. Details are in
 * state.mli *)

let win_score s = s.username

let score s = s.friends_list

let turns s = s.messages

let current_room_id s = s.current_person_being_messaged

(* [init_state j] takes in the initial login information of the user and 
 * initilizes the program based on that information *)
let init_state (name: string) : state =
  {
    username = name;
    status = "";
    friends_list = [(*access from txt file stored in computer*)];
    messages = [(*access from txt file stored in computer*)];
    current_person_being_messaged = None;
    requests = [(*access from networking*)];
    dictionary = [(*access*)];
    shortcut_list = []
  }

(* [add_friend friend st] returns the new state with [friend] added onto
 * this user's friends list
 *)
let add_friend (friend:string) (st:state) : state =
  let friend_person = {
    id = "";
    name = friend
  } in
  { st with
    username = st.username;
    friends_list = friend_person::st.friends_list;
    messages = st.messages;
    current_person_being_messaged = st.current_person_being_messaged
  }

(* [friend_removed friend friends accum] is a helper for [remove_friend]
 * that removes [friend] from [friends]
 *)
let rec friend_removed (friend: person) (friends: person list) (accum: person list) =
  match friends with
  | [] -> accum
  | p::xs -> 
    if p = friend then friend_removed friend xs accum
    else friend_removed friend xs (p::accum)

(* [remove_friend friend st] returns the new state with [friend] taken off 
 * this user's friends list
 *)
let remove_friend (friend:string) (st:state) : state =
  let friend_person = {
    id = "";
    name = friend
  } in
  { st with
    username = st.username;
    friends_list = friend_removed friend_person st.friends_list [];
    messages = st.messages;
    current_person_being_messaged = st.current_person_being_messaged
  }

(* [add_message_to_list friend message message_list] adds [message] just sent to
 * [friend] to the list of messages for this user
 *)  
let rec add_message_to_list (friend: person) (message: string) (message_list: (person * string list) list) accum =
  match message_list with
  | [] -> (friend, [message])::message_list
  | (p, sl)::xs -> 
    if p = friend then accum@[(p, message::sl)]@message_list
    else add_message_to_list friend message xs ((p, sl)::accum)

(* [pre_message_friend friend st] returns the new state with [friend] added as
 * this user's current person being messaged
 *)
let pre_message_friend (friend:string) (st:state) : state =
  let friend_person = {
    id = "";
    name = friend
  } in
  { st with
    username = st.username;
    friends_list = st.friends_list;
    messages = st.messages;
    current_person_being_messaged = Some friend_person
  }

(* [post_message_friend friend st] returns the new state with [friend] added onto
 * this user's friends list
 *)
 let post_message_friend (friend:person) (message: string) (st:state) : state =
  { st with
    username = st.username;
    friends_list = st.friends_list;
    messages = add_message_to_list friend message st.messages [];
    current_person_being_messaged = Some friend
  }

(* [clear_messages st] returns the new state with the current user's messages
 * list cleared
 *)
 let clear_messages (st:state) : state =
  { st with
    username = st.username;
    friends_list = st.friends_list;
    messages = [];
    current_person_being_messaged = st.current_person_being_messaged
  }

(* [set_status intended st] returns the new state with the current user's status
 * set to intended
 *)
 let set_status (intended:string) (st:state) : state =
  { st with
    status = intended
  }

(* [add_shortcut shortcut word st] returns the new state with the current user's status
 * set to intended
 *)
 let add_shortcut (shortcut:string) (word:string) (st:state) : state =
  { st with
    shortcut_list = ((shortcut, word)::st.shortcut_list)
  }

(* [add_shortcut shortcut word st] returns the new state with the current user's status
 * set to intended
 *)
 let define (word:string) (st:state) : state =
  { st with
    dictionary = ((word)::st.dictionary)
  }

(* [do' cmd st] changes the state according to a command. See details in
 * state.mli *)
let do' cmd st =
  (* if st.current_person_being_messaged = None then *)
    match cmd with
    | Talk intended -> pre_message_friend intended st
    | Friend intended -> add_friend intended st
    | Quit -> st
    | Friends_list -> st
    | Leave_conversation -> st
    | Unfriend intended -> remove_friend intended st
    | Add_shortcut (shortcut, word) -> add_shortcut shortcut word st
    | Define intended -> define intended st
    | Setstatus intended -> set_status intended st
    | View_requests -> st
    | Error -> st
    | _ -> st
  (* else 
    match cmd with
    | Leave_conversation messages -> st
    | Message ->  *)
