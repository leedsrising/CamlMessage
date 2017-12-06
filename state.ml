open Command
open Networking2

(* [id] represents the identification of someone through their IP *)


type person = {
  name : string;
  id : string;
  port : int;
  }
  
type state = {
  username: string;
  status : string;
  friends_list : person list;
  messages : (person * string list) list;
  convo_requests : person list;
  current_person_being_messaged : person option;
  requests: person list;
  dictionary : string list;
  shortcut_list : (string * string) list
  }

(* [init_state j] takes in the initial login information of the user and 
 * initilizes the program based on that information *)
let init_state (name: string) : state =
  {
    username = name;
    status = "";
    friends_list = [(*access from txt file stored in computer*)];
    messages = [(*access from txt file stored in computer*)];
    convo_requests = [];
    current_person_being_messaged = None;
    requests = [(*access from networking*)];
    dictionary = [(*access*)];
    shortcut_list = []
  }

let state_ref = ref (init_state "")
    
(* The following functions returns information in a state. Details are in
 * state.mli *)

(* [get_friend_by_name name st] is the friend (option) named [name] according
   to st. Returns None if no friend is found
*) 
let rec get_friend_by_name name st = 
  List.find_opt (fun friend -> name = friend.name) st.friends_list

(* [get_friend_by_name ip st] is the friend (option) with ip [ip] according
   to st. Returns None if no friend is found
*) 
let rec get_friend_by_ip ip st = 
  List.find_opt (fun friend -> ip = friend.id) st.friends_list

(* [current_friends_to_string st] returns the string version of
    the user's friends list
 *) 
 let current_friends_to_string st = 
  List.fold_left (^) "" 
    (List.mapi (fun i person -> (string_of_int (i + 1)) ^ ". " ^ person.name 
    ^ "  " ^ person.id ^ ":" ^ (string_of_int person.port) ^ "\n") 
  st.friends_list)

(* [current_friends_to_string st] returns the string version of
    the user's conversation request list
 *) 
 let current_convo_reqs_to_string st = 
  List.fold_left (^) "" 
    (List.mapi (fun i person -> (string_of_int (i + 1)) ^ ". " ^ person.name 
    ^ "\n") 
  st.convo_requests)

(* [current_friends s] takes in the current state of this user and returns
 * their friendlist in string version
 *) 
let current_friends s = 
  "\nYour current friends are: \n\n" ^ current_friends_to_string s

(* [current_requests_to_string st] returns the string version of
    the user's requests list
 *) 
 let current_friend_reqs_to_string st = 
  List.fold_left (^) "" 
    (List.mapi (fun i person -> (string_of_int (i + 1)) ^ ". " ^ person.name) 
    st.requests)

(* [current_requests s] takes in the current state of this user and returns
 * their requests in string version
 *) 
let current_requests st = 
  "\nPending Friend Requests : \n\n" ^ current_friend_reqs_to_string st
  ^ "\n\nPending Conversation Requests: \n\n" ^ current_convo_reqs_to_string st

(* [chat_history_names] takes in the current messages list of this 
 * user and returns the string version of the people that they have a chat
 * history with
 *) 
 let rec chat_history_names messages accum = 
  match messages with
  | [] -> accum
  | (x, _)::xs -> chat_history_names xs (accum ^ " " ^ x.name)

(* [chat_history s] takes in the current state of this user and returns
 * their chat history in string version
 *) 
let chat_history s = 
  "You have chat history with \n\n" ^ chat_history_names s.messages ""

(* [current_friends_to_string frnds accum] takes in the friends list of this 
 * user and returns the string version of their friends list
 *) 
 let rec current_shortcuts_to_string shrtcuts accum = 
  match shrtcuts with
  | [] -> accum
  | (shrtcut, wrd)::xs -> 
    current_shortcuts_to_string xs (accum ^ "\n" ^ shrtcut ^ "shortcuts to" ^ wrd)

let shortcuts s = 
  "Your current shortcuts are \n\n" ^ current_shortcuts_to_string s.shortcut_list ""

let get_friend_req name st = 
  List.find_opt (fun friend -> friend.name = name) st.requests

(* [add_friend friend st] returns the new state with [friend] added onto
 * this user's friends list
 *)
let request_friend (ip:string) (port:int) (st:state) : state =
  ignore (send_friend_req ip port st.username); st

let remove_friend_req name st = 
  {st with requests = 
    List.filter (fun person -> person.name <> name) st.requests}

let add_friend name ip port st = 
  {st with friends_list = {id=ip; port=port; name=name} :: st.friends_list }

let accept_friend_req name st =
  match get_friend_req name st with
  | None -> 
    print_endline ("Sorry, but you have no pending friend request from " 
      ^ name); st
  | Some friend -> begin
    ignore (send_uni_cmd friend.id friend.port ("friendaccept " ^ st.username 
    ^ " " ^ (string_of_int get_running_port))); 
    st |> add_friend friend.name friend.id friend.port 
       |> remove_friend_req name end
    
let add_friend_req name ip port st =
  {st with requests = {id=ip; port=port; name=name;} :: st.requests}

let add_convo_req name st =
  {st with convo_requests = name :: st.convo_requests}

(* [friend_removed friend friends accum] is a helper for [remove_friend]
 * that removes [friend] from [friends]
 *)
let rec friend_removed (name:string) (list: person list) =
  List.filter (fun friend -> friend.name <> name) list

(* [remove_friend friend st] returns the new state with [friend] taken off 
 * this user's friends list
 *)
let remove_friend (friend_name:string) (st:state) : state =
  { st with
    friends_list = friend_removed friend_name st.friends_list;
    current_person_being_messaged = st.current_person_being_messaged (*TODO: leave convo if talking to this friend *)
  }

let request_convo name st = 
  let friend_opt = get_friend_by_name name st in
  match friend_opt with
  | None -> print_endline "Sorry but you don't have a friend by that name."; st
  | Some friend -> ignore (send_uni_cmd friend.id friend.port "convoreq"); st

let accept_convo_req friend st = 
  ignore (send_uni_cmd friend.id friend.port "convoaccept"); 
  {st with requests = friend_removed friend.name st.requests}

let handle_talk name st = 
  let friend_opt = List.find_opt 
  (fun friend -> friend.name = name) st.convo_requests in
    match friend_opt with   
    | Some friend -> accept_convo_req friend st
    | None -> request_convo name st

let set_in_convo_with friend st = 
  {st with current_person_being_messaged = Some friend}

let confirm_convo_with friend st = 
  ignore (send_uni_cmd friend.id friend.port "convoconfirm"); 
  {st with requests = friend_removed friend.name st.requests} 
  |> set_in_convo_with friend


(* [add_message_to_list friend message message_list] adds [message] just sent to
 * [friend] to the list of messages for this user
 *)  
 let rec add_message_to_list (friend: person) (message: string) (message_list: (person * string list) list) accum =
  match message_list with
  | [] -> (friend, [message])::message_list
  | (p, sl)::xs -> 
    if p = friend then accum@[(p, message::sl)]@message_list
    else add_message_to_list friend message xs ((p, sl)::accum)

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
    | Talk username -> handle_talk username st
    | Friend (ip, port) -> request_friend ip port st
    | Quit -> st
    | Friends_list -> st
    | Leave_conversation -> st
    | Unfriend intended -> remove_friend intended st
    | Add_shortcut (shortcut, word) -> add_shortcut shortcut word st
    | Define intended -> define intended st
    | Setstatus intended -> set_status intended st
    | View_requests -> st
    | Accept username -> accept_friend_req username st
    | Error -> st
    | Help -> st

let definite opt = 
  match opt with 
  | Some thing -> thing
  | None -> failwith "Error: it wasn't definite"
  
let handle_remote_cmd net_state msg =
  let split = Str.split (Str.regexp " ") msg in
  let cmd = List.hd split in
  match cmd with 
  | "friendreq" -> 
    let name = (List.nth split 1) in
    state_ref := add_friend_req name !net_state.addr.ip 
      (int_of_string (List.nth split 2)) !state_ref;
    print_endline ("You have received a friend request from " ^ name);
    net_state := {!net_state with do_close = true};
  | "friendaccept" -> 
    let name = (List.nth split 1) in 
    let port = (List.nth split 2) in
    state_ref := add_friend name !net_state.addr.ip (int_of_string port)
      !state_ref |> remove_friend_req name;
    print_endline (name ^ " has accepted your friend request!");
    net_state := {!net_state with do_close = true};
  | "convoreq" -> (*sent by user 1 *) (* TODO: this isn't definite *)
    let friend = (definite (get_friend_by_ip !net_state.addr.ip !state_ref)) in
    state_ref := add_convo_req friend !state_ref;
    print_endline (friend.name ^ " would like to start a conversation.");
    net_state := {!net_state with do_close = true};
  | "convoaccept" -> (* TODO: fix if already in conversation *) (* sent by user 2 *)
    let friend = (definite (get_friend_by_ip !net_state.addr.ip !state_ref)) in
    state_ref := confirm_convo_with friend !state_ref;
    print_endline (friend.name ^ " has accepted your conversation request.");
  | "convoconfirm" ->  (* (auto if user not in convo) sent by user 1 *)
    let friend = (definite (get_friend_by_ip !net_state.addr.ip !state_ref)) in
    print_endline (" You are now in a conversation with " ^ friend.name);
    state_ref := set_in_convo_with friend !state_ref
  | _ -> failwith "Unexpected Remote Command: Use the latest version."

(* register listeners in networking *)
let () = 
  print_endline "registering...";
  register_read_listener handle_remote_cmd