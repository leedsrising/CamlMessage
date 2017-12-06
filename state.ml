open Command
open Networking2
open MessageTransformer

(* [id] represents the identification of someone through their IP *)

type person = {
  id : string;
  name : string;
  port : int;
  }
  
type state = {
  username: string;
  status : string;
  friends_list : person list;
  messages : (person * string list) list;
  current_person_being_messaged : person option;
  requests: person list
  }

let rec lines_in_file_helper (c : in_channel) (acc : string list) =
  try
    lines_in_file_helper c ((c |> input_line) :: acc)
  with
  | _ -> close_in c; acc

let lines_in_file (file : string) =
  let channel = file |> open_in in
  lines_in_file_helper channel []

let person_to_string (p: person) =
  p.name ^ p.id ^ (string_of_int p.port) ^ "\n"

(* adds a list of persons to the text file *)
let rec adding_friends (c:out_channel) = function
| [] -> close_out c
| x :: xs -> output_string c (person_to_string x); flush c; adding_friends c xs

(*String is of the form name ip port*)
let string_to_friend str =
  let s = String.trim str in
  let first_space = String.index s ' ' in
  let name = Str.string_before s first_space in
  let after_name = Str.string_after s first_space in
  let second_space = String.index after_name ' ' in
  let ip = Str.string_before s second_space in
  let port = Str.string_after s second_space in
  {id = ip; name = name; port = int_of_string port}

let friends_in_file (file : string) : person list=
  let line_lst = lines_in_file file in 
  let string_version = List.fold_left (fun lst elt -> elt :: lst) [] line_lst in
  List.map (string_to_friend) string_version

(* [add_shortcut sc phrase] adds a user defined shortcut
* into the file "shortcut.txt" *)
let add_friend_to_txt (friend : string) (id: string) (port: int) =
  let friend_person = {name = friend; id = id; port = port} in
  let orig_lst = "friends.txt" |> friends_in_file in
  let new_list = (friend_person)::orig_lst in
  let c = "friends.txt" |> open_out in
  adding_friends c new_list

let rec lst_remove ele lst accum =
  match lst with
  | [] -> accum
  | x::xs -> 
    if x.name = ele then lst_remove ele lst accum
    else lst_remove ele lst (x::accum)

(* [add_shortcut sc phrase] adds a user defined shortcut
* into the file "shortcut.txt" *)
let remove_friend (friend : string) =
  let orig_lst = "friends.txt" |> friends_in_file in
  let new_list = lst_remove friend orig_lst [] in
  let c = "friends.txt" |> open_out in
  adding_friends c new_list

let print_messages line_lst =
  List.fold_left (fun acc ele -> ele ^ "\n" ^ acc) "" line_lst

let get_messages_for_friend (friend : string) =
  lines_in_file (friend ^ ".txt") 

let rec get_total_messages_lst friends_list accum =
  match friends_list with
  | [] -> accum
  | x::xs -> get_total_messages_lst xs ((x, (get_messages_for_friend x.name))::accum)

(* [init_state j] takes in the initial login information of the user and 
 * initilizes the program based on that information *)
let init_state (name: string) : state =
  {
    username = name;
    status = "";
    friends_list = friends_in_file "friends.txt";
    messages = get_total_messages_lst (friends_in_file "friends.txt") [];
    current_person_being_messaged = None;
    requests = []
  }

let state_ref = ref (init_state "")
    
(* The following functions returns information in a state. Details are in
 * state.mli *)

(* [get_friend_by_name name st] is the friend (option) named [name] according
 * to st. Returns None if no friend is found
 *) 
let rec get_friend_by_name name st = 
  List.find_opt (fun friend -> name = friend.name) st.friends_list

(* [get_friend_by_name ip st] is the friend (option) with ip [ip] according
 * to st. Returns None if no friend is found
 *) 
let rec get_friend_by_ip ip st = 
  List.find_opt (fun friend -> ip = friend.id) st.friends_list

(* [current_friends_to_string st] returns the string version of
 * the user's friends list
 *) 
 let current_friends_to_string st = 
  List.fold_left (^) "" 
    (List.mapi (fun i person -> (string_of_int (i + 1)) ^ ". " ^ person.name) 
    st.friends_list)

(* [current_friends s] takes in the current state of this user and returns
 * their friendlist in string version
 *) 
let current_friends s = 
  "\nYour current friends are: \n\n" ^ current_friends_to_string s

(* [current_requests_to_string st] returns the string version of
    the user's requests list
 *) 
 let current_requests_to_string st = 
  List.fold_left (^) "" 
    (List.mapi (fun i person -> (string_of_int (i + 1)) ^ ". " ^ person.name) 
    st.requests)

(* [current_requests s] takes in the current state of this user and returns
 * their requests in string version
 *) 
let current_requests s = 
  "\nYour current requests are: \n\n" ^ current_requests_to_string s

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
 (* let rec current_shortcuts_to_string shrtcuts accum = 
  match shrtcuts with
  | [] -> accum
  | (shrtcut, wrd)::xs -> 
    current_shortcuts_to_string xs (accum ^ "\n" 
      ^ shrtcut ^ "shortcuts to" ^ wrd)

(* [shortcuts s] takes in the state list of this user and returns the 
 * string version of their current shortcuts
 *) 
let shortcuts s = 
  "Your current shortcuts are \n\n" ^ 
    current_shortcuts_to_string s.shortcut_list "" *)

let get_friend_req name st = 
  List.find_opt (fun friend -> friend.name = name) st.requests

(* [request_friend ip int state] sends a friend request to the user with
 * ip address [ip]
 *)
let request_friend (ip:string) (port:int) (st:state) : state =
  ignore (send_friend_req ip port st.username); st

(* [accept_friend_req name st] accepts the friend request from the user
 * with name [name]. If there is not a request from the user [name], then
 * the user receieves a message saying that no request was found.
 *)
let accept_friend_req name st =
  match get_friend_req name st with
  | None -> 
    print_endline ("Sorry, but you have no pending 
      friend request from " ^ name); st
  | Some friend -> begin
    ignore (send_friend_accpt friend.id friend.port st.username); 
    {st with friends_list = 
    {id = friend.id; port = friend.port; name = name} :: st.friends_list} end

(* [add_friend_req name ip port st] changes this user's friends requests list 
 * in state to include the new friend request from outside user with name 
 * [name]
 *)
let add_friend_req name ip port st =
  {st with requests = {id=ip; port=port; name=name;} :: st.requests}

(* [add_friend name ip port st] changes this user's friends list in state to 
 * include the new friend from outside user with name [name]
 *)
let add_friend name ip port st = 
  {st with friends_list = {id=ip; port=port; name=name} :: st.friends_list }

(* [friend_removed friend friends accum] is a helper for [remove_friend]
 * that removes [friend] from [friends]
 *)
let rec friend_removed (name:string) (friends: person list) =
  List.filter (fun friend -> friend.name != name) friends

(* [remove_friend friend st] returns the new state with [friend] taken off 
 * this user's friends list
 *)
let remove_friend (friend_name:string) (st:state) : state =
  { st with
    friends_list = friend_removed friend_name st.friends_list;
    current_person_being_messaged = st.current_person_being_messaged (*TODO: leave convo if talking to this friend *)
  }

(* [pre_message_friend friend st] returns the new state with [friend] added as
 * this user's current person being messaged
 *)
let pre_message_friend (friend:person) (st:state) : state =
 { st with
    username = st.username;
    friends_list = st.friends_list;
    messages = st.messages;
    current_person_being_messaged = Some friend 
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
 (* let add_shortcut (shortcut:string) (word:string) (st:state) : state =
  { st with
    shortcut_list = ((shortcut, word)::st.shortcut_list)
  } *)

(* [add_shortcut shortcut word st] returns the new state with the current user's status
 * set to intended
 *)
 (* let define (word:string) (st:state) : state =
  { st with
    dictionary = ((word)::st.dictionary)
  } *)

(* [do' cmd st] changes the state according to a command. See details in
 * state.mli 
 *)
let do' cmd st =
  (* if st.current_person_being_messaged = None then *)
    match cmd with
    | Talk intended -> (* pre_message_friend intended st *) st
    | Friend (ip, port) -> request_friend ip port st
    | Message_history friend -> st
    | Quit -> st
    | Friends_list -> st
    | Leave_conversation -> st
    | Unfriend intended -> remove_friend intended st
    | Add_shortcut (shortcut, word) -> failwith "todo"(*add_shortcut shortcut word st*)
    | Define intended -> failwith "todo"
    | Setstatus intended -> set_status intended st
    | View_requests -> st
    | Accept username -> accept_friend_req username st
    | Error -> st
    | Help -> st

(* [handle_remote_cmd net_state msg] handles a command from a remote client so
 * that it can read what other users are doing (i.e. telling when another user
 * has accepted this user's friend request)
 *)
let handle_remote_cmd net_state msg =
  let split = Str.split (Str.regexp " ") msg in
  let cmd = List.hd split in
  match cmd with 
  | "friendreq" -> 
    print_endline msg;
    let name = (List.nth split 1) in
    let ip = !net_state.addr.ip in
    let port = int_of_string (List.nth split 2) in
    state_ref := add_friend_req name !net_state.addr.ip 
      (int_of_string (List.nth split 2)) !state_ref;
    print_endline ("You have received a friend request from " ^ name);
    net_state := {!net_state with do_close = true};
  | "friendaccept" -> 
    let name = (List.nth split 1) in
    let ip = !net_state.addr.ip in
    let port = !net_state.addr.port in
    state_ref := add_friend name !net_state.addr.ip !net_state.addr.port !state_ref;
    add_friend_to_txt name ip port;
    print_endline (name ^ " has accepted your friend request!");
    net_state := {!net_state with do_close = true};
  | _ -> failwith "Unexpected Remote Command: Use the latest version."

(* register listeners in networking *)
let () = 
  print_endline "registering...";
  register_read_listener handle_remote_cmd