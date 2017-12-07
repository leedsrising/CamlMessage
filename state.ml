open Command
open Networking2
open MessageTransformer

(* [id] represents the identification of someone through their IP *)

type talk_status = None | One_to_one | GroupClient | GroupServer

(* [person] is the type of a person containing their name, ip address and port *)
type person = {
  name : string;
  id : string;
  port : int;
}

(* [state] holds all of the information about an instance of the program *)
type state = {
  username: string;
  status : string;
  friends_list : person list;
  messages : (person * string list) list;
  convo_requests : person list;
  talk_status : talk_status;
  current_person_being_messaged : person option;
  group_invites : person list;
  group_host_remote : person option;
  group_clients : person list;
  friend_requests: person list;
  encrypt: bool;
  encrypt_key : int;
  spellcheck : bool
  }

(* [lines_in_file_helper c acc] is a helper method that accesses each line in a .txt file *)
let rec lines_in_file_helper (c : in_channel) (acc : string list) =
  try
    lines_in_file_helper c ((c |> input_line) :: acc)
  with
  | _ -> close_in c; acc

(* [lines_in_file file] is a string list containing each line of a file 
 * as an entry in the list 
 *)
let lines_in_file (file : string) =
  let channel = file |> open_in in
  lines_in_file_helper channel []

(* [person_to_string p] represents a person object as a string *)
let person_to_string (p: person) =
  p.name ^ " " ^ p.id ^ " " ^(string_of_int p.port) ^ "\n"

(* [print_message_formatted from msg] formats a message [msg] from sender [from] and prints it*)
let print_message_formatted from msg = 
  print_endline ("[" ^ from ^ "]: " ^ msg)

(* [adding_friends c] adds a list of persons to the text file *)
let rec adding_friends (c:out_channel) = function
| [] -> close_out c
| x :: xs -> output_string c (person_to_string x); flush c; adding_friends c xs

(* [string_to_friend str] is a string of the form name ip port*)
let string_to_friend str =
  let str_list = String.split_on_char ' ' str in
  {id = List.nth str_list 1; name = List.hd str_list; port = int_of_string (List.nth str_list 2)}
(* [friends_in_file file] is a person list of all friends in a file*)
let rec friends_in_file (file : string) : person list=
  let line_lst = lines_in_file file in
  let rec print_lines lines acc =
  match lines with
  | [] -> acc
  | x::xs -> print_lines xs (string_to_friend x :: acc)
  in print_lines line_lst []

(* [add_shortcut sc phrase] adds a user defined shortcut
* into the file "shortcut.txt" *)
let add_friend_to_txt (friend : string) (id: string) (port: int) =
  let friend_person = {name = friend; id = id; port = port} in
  let orig_lst = "friends.txt" |> friends_in_file in
  let new_list = (friend_person)::orig_lst in
  let c = "friends.txt" |> open_out in
  adding_friends c new_list

(* [lst_remove ele lst accum] is lst without ele. *)
let rec lst_remove ele lst accum =
  match lst with
  | [] -> accum
  | x::xs ->
    if x.name = ele then lst_remove ele xs accum
    else lst_remove ele xs (x::accum)

(* [add_shortcut sc phrase] adds a user defined shortcut
* into the file "shortcut.txt" *)
let remove_friend_txt (friend : string) =
  let orig_lst = "friends.txt" |> friends_in_file in
  let new_list = lst_remove friend orig_lst [] in
  let c = "friends.txt" |> open_out in
  adding_friends c new_list

(* [print_messages line_lst] combines the lines in line_lst to one string *)
let print_messages line_lst =
  List.fold_left (fun acc ele -> ele ^ "\n" ^ acc) "" line_lst

(* [get_messages_for_friend friend] is all of the messages in the conversation with 
   friend. *)
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
    friends_list = (try friends_in_file "friends.txt"; with e -> []);
    messages = [(*access from txt file stored in computer*)];
    talk_status = None;
    convo_requests = [];
    current_person_being_messaged = None;
    group_invites = [];
    group_host_remote = None;
    group_clients = [];
    friend_requests = [];
    encrypt = false;
    encrypt_key = 0;
    spellcheck = true
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
   current_friends_to_string s

(* [current_requests_to_string st] returns the string version of
    the user's requests list
 *)
 let current_friend_reqs_to_string st =
  List.fold_left (^) ""
    (List.mapi (fun i person -> (string_of_int (i + 1)) ^ ". " ^ person.name)
    st.friend_requests)

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

let get_friend_req name st =
  List.find_opt (fun friend -> friend.name = name) st.friend_requests

(* [request_friend ip int state] sends a friend request to the user with
 * ip address [ip]
 *)
let request_friend (ip:string) (port:int) (st:state) : state =
  ignore (send_friend_req ip port st.username); st

(* [remove_friend_req name st] st without any friend requests from 
 * a person with name [name] *)
let remove_friend_req name st =
  {st with friend_requests =
    List.filter (fun person -> person.name <> name) st.friend_requests}

(* [add_friend name ip port st] adds a friend to the friends list *)
let add_friend name ip port st =
  {st with friends_list = {id=ip; port=port; name=name} :: st.friends_list }

(* [accept_friend_req name st] accepts a friend request from [name] and returns a new state *)
let accept_friend_req name st =
  match get_friend_req name st with
  | None ->
    print_endline ("Sorry, but you have no pending friend request from "
      ^ name); st
  | Some friend -> begin
    ignore (send_cmd friend.id friend.port ("friendaccept " ^ st.username
    ^ " " ^ (string_of_int (get_running_port ()))));
    add_friend_to_txt friend.name friend.id friend.port;
    st |> add_friend friend.name friend.id friend.port
       |> remove_friend_req name end

(* [add_friend_req name ip port st] adds the incoming friend request to the state *)
let add_friend_req name ip port st =
  {st with friend_requests = {id=ip; port=port; name=name;} 
    :: st.friend_requests}

(* [encrypt_messages boolean key st] toggles the encrypting of messages by updating state *)
let encrypt_messages (boolean: string ) (key: string ) (st:state) = 
  try 
    { st with
    encrypt = bool_of_string boolean;
    encrypt_key = int_of_string key
    }
  with 
  | e -> st
(*[encrypt msg] encrypts a message msg*)
let encrypt msg = 
  if !state_ref.encrypt then MessageTransformer.pm msg else msg

let add_convo_req name st =
  {st with convo_requests = name :: st.convo_requests}

(* [friend_removed friend friends accum] is a helper for [remove_friend]
 * that removes [friend] from [friends]
 *)
let rec friend_removed (name:string) (list: person list) =
  let rec friend_removed_helper name acc = 
  match acc with 
  | [] -> print_endline (name ^ " is not a friend"); List.filter (fun friend -> friend.name <> name) list
  | h::t -> if h.name != name then friend_removed_helper name t 
    else let () = print_endline ("Removed " ^ name) in 
    List.filter (fun friend -> friend.name <> name) list 
  in friend_removed_helper name list

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
  | Some friend -> ignore (send_cmd friend.id friend.port "convoreq"); st

let accept_convo_req friend st =
  ignore (send_cmd friend.id friend.port "convoaccept");
  {st with convo_requests = friend_removed friend.name st.convo_requests}

let handle_talk name st =
  let friend_opt = List.find_opt
  (fun friend -> friend.name = name) st.convo_requests in
    match friend_opt with
    | Some friend -> accept_convo_req friend st
    | None -> request_convo name st

let set_in_convo_with friend st =
  {st with talk_status = One_to_one;
    current_person_being_messaged = Some friend}

let check_confirm_convo_with friend st =
  if st.talk_status = None then
    (ignore (send_cmd friend.id friend.port "convoconfirm");
    print_endline (friend.name ^ " has accepted your conversation request.");
    print_endline ("You are now in a conversation with " ^ friend.name);
    {st with friend_requests = friend_removed friend.name st.friend_requests}
    |> set_in_convo_with friend)
  else 
    (ignore (send_cmd friend.id friend.port "convobusy"); st)

let clean_up_after_convo st = 
  {st with talk_status = None;
    current_person_being_messaged = None;
    group_host_remote = None}

let disconnect_from (person_opt: person option) st =
  match person_opt with
  | None -> st
  | Some person -> close person.id person.port;
    clean_up_after_convo st

let terminate_group_server st = 
  (List.fold_left (fun () client ->
    close client.id client.port) () st.group_clients);
    {st with talk_status = None; group_clients = [] }

(* TODO: Handle group *)
let leave_convo st =
  match st.talk_status with
  | One_to_one -> 
    disconnect_from st.current_person_being_messaged st
  | GroupClient -> 
    disconnect_from st.group_host_remote st
  | GroupServer -> terminate_group_server st
  | None -> st
  

(* [add_message_to_list friend message message_list] adds [message] just sent to
 * [friend] to the list of messages for this user
 *)
 let rec add_message_to_list (friend: person) (message: string) (message_list: (person * string list) list) accum =
  match message_list with
  | [] -> (friend, [message])::message_list
  | (p, sl)::xs ->
    if p = friend then accum@[(p, message::sl)]@message_list
    else add_message_to_list friend message xs ((p, sl)::accum)

let spellcheck msg = 
    if !state_ref.spellcheck then (msg |> send) else msg

let send_to_all_clients message st = 
  (List.fold_left (fun () client -> 
    ignore (send_cmd client.id client.port message)) () st.group_clients);
    st
  
let send_chat_to_all_clients username message st = 
  send_to_all_clients ("gmsg:[" ^ username ^ "]: " ^ message) st

let send_message message st =
  match st.talk_status with
  | None -> print_endline ("Error: You aren't in a conversation.\n"
    ^ "Type /help for commands."); st
  | One_to_one -> 
    (match st.current_person_being_messaged with
    | None -> print_endline ("Error: Missing peer"); st
    | Some friend -> (* TODO: Update state with message *)
      print_message_formatted st.username message;
      ignore(send_cmd friend.id friend.port ("msg:" ^ (message|> spellcheck |> encrypt))); st)
  | GroupClient -> 
    (match st.group_host_remote with
    | None -> print_endline "Error: Missing group host"; st
    | Some host -> 
      (* print_message_formatted st.username message; *)
      ignore(send_cmd host.id host.port ("gmsg:[" ^ st.username ^ "]: " 
      ^ (message|> spellcheck |> encrypt))); st)
  | GroupServer -> print_message_formatted st.username message;
    send_chat_to_all_clients st.username message st
    
    

(* invite someone to group convo *)
let do_invite name st = 
  match st.talk_status with
  | GroupClient | One_to_one -> 
    print_endline ("Error: You must be the host of a group conversation"
    ^ " to do that. Please leave this conversation first."); st
  | GroupServer | None -> match get_friend_by_name name st with
    | None -> print_endline "Sorry but you don't have a friend by that name."; st
    | Some friend -> 
      ignore (send_cmd friend.id friend.port "groupinv");
      {st with talk_status = GroupServer}
  
let add_group_inv name st =
  {st with group_invites = name :: st.group_invites}

let accept_group_inv friend st =
  ignore (send_cmd friend.id friend.port "groupaccept");
  print_endline "postSendAccept";
  {st with group_invites = 
    friend_removed friend.name st.group_invites}

(* try to join a group convo *)
let do_join name st = 
  match st.talk_status with
  | None -> begin let person_opt = List.find_opt
    (fun person -> person.name = name) st.group_invites in
      match person_opt with
      | None -> 
        print_endline "Sorry but you don't have an invite from that person."; st
      | Some friend -> accept_group_inv friend st
    end
  | _ -> print_endline ("Error: Please leave this conversation first."); st

let check_confirm_group_with friend st =
  if st.talk_status = GroupServer then
    begin (ignore (send_cmd friend.id friend.port "groupconfirm"));
    print_endline (friend.name ^ " has joined the group."); (* TODO: send to all *)
    print_endline "PostSendConfirm";
    {st with group_clients = friend::st.group_clients} end
  else 
    (ignore (send_cmd friend.id friend.port "groupbusy"); st)

(* [post_message_friend friend st] returns the new state with [friend] added onto
 * this user's friends list
 *)
 (**let post_message_friend (friend:person) (message: string) (st:state) : state =
  { st with
    username = st.username;
    friends_list = st.friends_list;
    messages = add_message_to_list friend message st.messages [];
    current_person_being_messaged = Some friend
  } **)

  

  (*** let send_message message st =
    match st.current_person_being_messaged with
    | None -> print_endline ("Error: You aren't in a conversation.\n"
      ^ "Type /help for commands."); st
    | Some friend -> (* TODO: Update state with message *)
      ignore(send_cmd friend.id friend.port ("msg:" ^ (message|> spellcheck |> encrypt))); st ***)

(* joining as client *)
let set_in_group_with friend st =
  print_endline "SetInGroupWith";
  {st with talk_status = GroupClient;
    group_host_remote = Some friend; }

(* [clear_messages st] returns the new state with the current user's messages
 * list cleared
 *)
 let clear_messages name  =
  let () = (name ^ ".txt") |> open_out |> close_out in ()

(* [set_status intended st] returns the new state with the current user's status
 * set to intended
 *)
 let set_status (intended:string) (st:state) : state =
  { st with
    status = intended
  }

let toggle_spellcheck st = 
  { st with 
    spellcheck = not (st.spellcheck)
  }

let do' cmd st =
  (* if st.current_person_being_messaged = None then *)
    match cmd with
    | Talk username -> handle_talk username st
    | Friend (ip, port) -> request_friend ip port st
    | Message_history friend -> 
      print_endline (print_messages (get_messages_for_friend friend)); st
    | Clear_history friend -> clear_messages friend; st
    | Quit -> st
    | Friends_list -> st
    | Encrypt_messages (boolean, key) -> encrypt_messages boolean key st
    | Toggle_spellcheck -> toggle_spellcheck st
    | Leave_conversation -> leave_convo st
    | Unfriend intended ->remove_friend_txt intended; remove_friend intended st
    | Add_shortcut (shortcut, word) -> st
    | Define intended -> st
    | Setstatus intended -> set_status intended st
    | View_requests -> st
    | Accept username -> accept_friend_req username st
    | Message m -> send_message m st
    | Invite name -> do_invite name st 
    | Join name -> do_join name st
    | Error -> st
    | Help -> st

let rec adding_messages (c:out_channel) = function
| [] -> close_out c
| x :: xs -> output_string c (x^"\n"); flush c; adding_messages c xs
let rec messages_in_file (file : string) : string list=
  let line_lst = lines_in_file file in
  let rec print_lines lines acc =
  match lines with
  | [] -> acc
  | x::xs -> print_lines xs (x :: acc)
  in print_lines line_lst []

(* [add_shortcut sc phrase] adds a user defined shortcut
* into the file "shortcut.txt" *)
let add_message_to_txt (msg: string) (name:string) =
  let orig_lst = name ^ ".txt" |> messages_in_file in
  let new_list = msg::orig_lst in
  let c = name ^ ".txt" |> open_out in
  adding_messages c new_list

let handle_message msg ip =
  let st = !state_ref in
  match st.talk_status with
  | None -> ()
  | One_to_one -> 
    (match st.current_person_being_messaged with
    | None -> () (* #ignored *)
    | Some person ->
      if person.id = ip then (*TODO: better auth. *)
        (
          (* add_message_to_txt ("[" ^ person.name ^ "]: " ^ msg) person.name; *)
        print_message_formatted person.name msg) else
        ())
  | GroupClient ->
    (match st.group_host_remote with
    | None -> () (* #ignored *)
    | Some person ->
      if person.id = ip then (*TODO: better auth. *)
        (print_endline msg) else
        ())
  | GroupServer -> 
    (match List.find_opt (fun c -> c.id = ip) st.group_clients with
    | None -> ()
    | Some person -> print_endline msg; 
    state_ref := (send_to_all_clients ("gmsg:" ^ msg) st))

(* [check_friends name ip port acc] checks to see if this exact user is already in the friends list *)
let rec check_friends name ip port acc = 
  match acc with 
  | [] -> false 
  | h::t -> if (String.equal h.name name && String.equal h.id ip && h.port == port) then true 
    else check_friends name ip port t

(*[check_friends_name name acc] checks if the user already has a friend of the same name*)
let rec check_friends_name name acc = 
  match acc with 
  | [] -> false 
  | h :: t -> if (String.equal h.name name) then true else check_friends_name name t

 (* [rename_helper name split_name acc] returns the number of occurances of a name
  * in the friends list
  *)
let rec rename_helper name split_name acc = 
  match split_name with 
    | [] -> acc
    | h::t -> if String.equal h name then acc + 1 else (rename_helper name t acc)

(* [rename name lst acc] returns the int to be appended to the name so that 
 * it is not duplicated in friends_list
 *)
let rec rename_int name lst acc = 
  match lst with 
  | [] -> acc 
  | h::t -> rename_int name t (rename_helper name (String.split_on_char '_' h.name ) acc)

(*TODO: remove definite *)
let definite opt =
  match opt with
  | Some thing -> thing
  | None -> failwith "Error: it wasn't definite"

let handle_remote_cmd net_state msg =
  print_endline msg;
  let trimmed = String.trim msg in
  let length = String.length trimmed in
  if length > 4 && (String.sub trimmed 0 4) = "msg:" then
      (handle_message (String.sub trimmed 4 (length - 4)) !net_state.addr.ip) else
  if length > 5 && (String.sub trimmed 0 5) = "gmsg:" then
      (handle_message (String.sub trimmed 5 (length - 5)) !net_state.addr.ip) else
  let split = Str.split (Str.regexp " ") trimmed in
  let cmd = List.hd split in
  match cmd with
  | "friendreq" ->
    let name = (List.nth split 1) in
    let ip = !net_state.addr.ip in
    let port = int_of_string (List.nth split 2) in
    state_ref := add_friend_req name ip port !state_ref;
    print_endline ("You have received a friend request from " ^ name);
    net_state := {!net_state with do_close = true};
  | "friendaccept" ->
    let name = (List.nth split 1) in
    let ip = !net_state.addr.ip in 
    let port = (List.nth split 2) in
    if (check_friends name ip (int_of_string port) !state_ref.friends_list) then 
    let new_state = (remove_friend_req name !state_ref) in state_ref := new_state;
    let () = print_endline (name ^ " is already your friend"); in ()
    else 
    let new_name = (if check_friends_name name !state_ref.friends_list 
      then let () = print_endline "Hi";in name ^ "_" ^ (string_of_int (rename_int name !state_ref.friends_list 0)) else let () = print_endline "Hi1";in name) in 
    add_friend_to_txt new_name !net_state.addr.ip (int_of_string port);
    state_ref := add_friend new_name !net_state.addr.ip (int_of_string port)
      !state_ref |> remove_friend_req name;
    print_endline (new_name ^ " has accepted your friend request!");
    net_state := {!net_state with do_close = true};
  | "convoreq" -> (*sent by user 1 *) (* TODO: this isn't definite *)
    let friend = (definite (get_friend_by_ip !net_state.addr.ip !state_ref)) in
    state_ref := add_convo_req friend !state_ref;
    print_endline (friend.name ^ " would like to start a conversation.");
    net_state := {!net_state with do_close = true};
  | "convoaccept" -> (* TODO: fix if already in conversation *) (* sent by user 2 *)
    let friend = (definite (get_friend_by_ip !net_state.addr.ip !state_ref)) in
    state_ref := check_confirm_convo_with friend !state_ref;
  | "convoconfirm" ->  (* (auto if user not in convo) sent by user 1 *)
    let friend = (definite (get_friend_by_ip !net_state.addr.ip !state_ref)) in
    print_endline ("You are now in a conversation with " ^ friend.name);
    state_ref := set_in_convo_with friend !state_ref
  | "convobusy" ->  (* accepted convo but other user is busy *)
    let friend = (definite (get_friend_by_ip !net_state.addr.ip !state_ref)) in
    print_endline ("Unfortunately, " ^ friend.name ^ " is already in a 
      conversation.");
  | "groupinv" ->  (* invited to group convo *)
    let friend = (definite (get_friend_by_ip !net_state.addr.ip !state_ref)) in
    state_ref := add_group_inv friend !state_ref;
    print_endline (friend.name ^ " has invited you to a group conversation.");
    net_state := {!net_state with do_close = true};
  | "groupaccept" ->
    let friend = (definite (get_friend_by_ip !net_state.addr.ip !state_ref)) in
    state_ref := check_confirm_group_with friend !state_ref;
  | "groupconfirm" -> 
    let friend = (definite (get_friend_by_ip !net_state.addr.ip !state_ref)) in
    print_endline ("You have joined the group hosted by " ^ friend.name);
    state_ref := set_in_group_with friend !state_ref
  | "groupbusy" ->  (* accepted convo but host is gone *)
    let friend = (definite (get_friend_by_ip !net_state.addr.ip !state_ref)) in
    print_endline ("Unfortunately, " ^ friend.name ^ " is no longer hosting" ^ 
    "that conversation.");
  | _ -> failwith "Unexpected Remote Command: Use the latest version."

let handle_disconnect net_state =
  let st = !state_ref in
  match st.talk_status with
  | GroupClient -> (let host = st.group_host_remote in
    match host with
    | None -> ()
    | Some person -> if person.id = !net_state.addr.ip
      then print_endline ("You have been disconnected from the group.");
      state_ref := clean_up_after_convo st)
  | One_to_one ->  
    (let talking_with_opt = st.current_person_being_messaged in
    match talking_with_opt with
    | None -> ()
    | Some person -> if person.id = !net_state.addr.ip
      then print_endline (person.name ^ " has disconnected.");
      state_ref := clean_up_after_convo st)
  | _ -> ()


(* register listeners in networking *)
let () =
  print_endline "registering...";
  register_read_listener handle_remote_cmd;
  register_disconnect_listener handle_disconnect;
