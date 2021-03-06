open Lwt
open Str

let () = Lwt_log.add_rule "*" Lwt_log.Info

let listen_address = Unix.inet_addr_loopback
let desired_port = 9000
let running_port = ref desired_port
let backlog = 10

type conn_addr = {ip:string; port:int; fd:Lwt_unix.file_descr}
type net_state = {out_buffer:string option ; do_close:bool; addr:conn_addr}

let connections = ref []

let message_listeners = ref []

let disconnect_listeners = ref []

let get_running_port () = !running_port

let to_ip_port saddr =
  match saddr with
  | Lwt_unix.ADDR_INET (ip, port) -> (Unix.string_of_inet_addr ip, port)
  | _ -> failwith "bad saddr type"

(* [handle_write ic oc st_r] writes messages stored in its write buffer
 * which is [st_r] to [oc] until it is signalled to stop with do_close = true. *)
let rec handle_write ic oc st_r () = 
  (match !st_r.out_buffer with
  | None -> return_unit
  | Some s -> 
    begin st_r := {!st_r with out_buffer = None};
    Lwt_io.write_line oc s >>= fun () -> Lwt_io.flush oc end) >>=
    fun () -> Lwt_unix.sleep 0.5 >>= fun () ->
    match !st_r.do_close with
    | true -> connections := List.remove_assoc (!st_r.addr.ip, !st_r.addr.port)
     !connections; 
      return (Lwt_unix.shutdown !st_r.addr.fd Lwt_unix.SHUTDOWN_ALL)
    | false -> handle_write ic oc st_r () (*connection remains open *)

(* [handle_message msg st_r] calls the registered listeners in [listeners] with
 * the received message [msg]. *)
let handle_message msg st_r =
  ignore (List.map (fun (listener_fun:(net_state ref -> string -> unit)) ->
    listener_fun st_r msg) !message_listeners); return_unit

(* [handle_disconnect st_r] calls the registered listeners in 
  [disconnect_listeners] after a disconnect has occurred. *)
let handle_disconnect st_r =
  ignore (List.map (fun (listener_fun:(net_state ref -> unit)) ->
    listener_fun st_r) !disconnect_listeners); return_unit
    
(* [handle_read ic oc st_r] handle_read loops for as long as messages
 * can be received from ic. Once ~EOF is received, it terminates and calls
 * the registered disconnect listeners *)
let rec handle_read ic oc st_r () =
  Lwt_io.read_line_opt ic >>=
  (fun msg ->
      match msg with
      | Some msg -> handle_message msg st_r >>= handle_read ic oc st_r
      | None -> st_r := {!st_r with do_close = true};
        handle_disconnect st_r >>= fun () -> 
          Lwt_log.info "RConnection closed" >>= return)

(* [handle_sock_error ns err] handles a socket error [err] associated with
 * [ns] and requests it to disconnect. *)
let handle_sock_error ns err = 
  Lwt_log.ign_error (Printexc.to_string err);
  ns := {!ns with do_close = true }

(* [accept_connection conn] handles (for the server) an incoming 
 * socket connection. It returns a net_state, used for dealing with the 
 * socket in the future. *)
let accept_connection conn =
  let open Lwt_unix in
  let fd, saddr = conn in
  let (ip, port) = to_ip_port saddr in
  let ic = Lwt_io.of_fd Lwt_io.Input fd in
  let oc = Lwt_io.of_fd Lwt_io.Output fd in
  let net_state = ref {out_buffer = None; do_close = false; addr = {ip; port; fd=fd}} in
  Lwt.on_failure (handle_read ic oc net_state ()) (handle_sock_error net_state);
  Lwt.on_failure (handle_write ic oc net_state ()) (handle_sock_error net_state);
  connections := ((ip, port), net_state) :: !connections;
  Lwt_log.info "New connection" >>= fun() -> return net_state

(* [create_server sock] creates a server socket which accepts (loops)
 * client sockets indefinitely. *)
let create_server sock =
  let rec serve () =
      Lwt_unix.accept sock >>= accept_connection >>= fun _ -> serve ()
  in serve

(* [create_socket ip_addr port] creates a server socket on the lowest port
 * which is free and >= [port]. It returns the file descriptor and the port. *)
let rec create_socket ip_addr port () =
  let open Lwt_unix in
  try let sock = socket PF_INET SOCK_STREAM 0 in
  ignore (bind sock @@ ADDR_INET(ip_addr, port));
  listen sock backlog;
  (sock, port) with e -> create_socket ip_addr (port + 1) ()

(* [make_connection conn addr] connects with the address specified in [addr]
 * using the socket conn. A net_state representing the new client is returned. *)
let rec make_connection conn addr () =
  let fd = conn in
  let ic = Lwt_io.of_fd Lwt_io.Input fd in
  let oc = Lwt_io.of_fd Lwt_io.Output fd in
  let net_state = ref {out_buffer = None; do_close = false; addr=addr} in
  Lwt.on_failure (handle_read ic oc net_state ()) (handle_sock_error net_state);
  Lwt.on_failure (handle_write ic oc net_state ()) (handle_sock_error net_state);
  connections := ((addr.ip, addr.port), net_state) :: !connections;
  Lwt_log.info "Connected to remote" >>= fun () -> return net_state

(* [do_connect ip port] makes a socket connection to the address [ip]:[port] with
 * a seven second timeout. The resulting net_state, if any, is returned. *)
let rec do_connect ip port =
  let open Lwt_unix in
  let addr = Unix.inet_addr_of_string ip in
  let sock = socket PF_INET SOCK_STREAM 0 in
  let timeout = Lwt_timeout.create 7 (fun () -> 
  print_endline ("Error: Failed to connect to " ^ ip 
  ^ ":" ^ (string_of_int port))) in
    Lwt_timeout.start timeout;
    connect sock (Lwt_unix.ADDR_INET (addr, port)) >>=
    fun () -> return (Lwt_timeout.stop timeout) >>=
    make_connection sock {ip=ip; port=port;fd=sock}
  
(* [send_uni_cmd ip port cmd_msg] connects to the socket at
 * [ip], [port] and sends the string [cmd_msg]*)
 let send_uni_cmd ip port cmd_msg =
  do_connect ip port >>= 
    fun net_state -> 
      return (net_state := {!net_state with out_buffer = Some (cmd_msg);})

(* [send_cmd ip port cmd_msg] uses an existing connection to
 * [ip], [port], or creates a new one if one does not exist, 
 * and sends the string [cmd_msg]*)
let send_cmd ip port cmd_msg =
  let conn_opt = List.assoc_opt (ip,port) !connections in
    match conn_opt with
    | Some net_state -> 
      return (net_state := {!net_state with out_buffer = Some (cmd_msg);})
    | None -> send_uni_cmd ip port cmd_msg

(* [send_friend_req ip port from_name] sends a friend request to 
* the address specified, where [from_name] is the user sending it. *)
let send_friend_req ip port from_name =
  send_uni_cmd ip port 
    ("friendreq " ^ from_name ^ " " ^ (string_of_int !running_port))

(* [close ip port] asks any existing net_state with a connection to
 * [ip]:[port] to disconnect. *)
let close ip port = 
  let ns_opt = List.assoc_opt (ip, port) !connections in
    match ns_opt with
    | Some ns -> ns := {!ns with do_close = true}
    | None -> ()

(* [register_read_listener listener] stores [listener], which will be
 * called when a message is received from a client. *)
let register_read_listener listener =
  message_listeners := listener :: !message_listeners

(* [register_disconnect_listener listener] stores [listener], which will be
 * called when a client disconnects. *)
let register_disconnect_listener listener =
  disconnect_listeners := listener :: !disconnect_listeners

(* [start_server ()] starts the server, which listens for TCP connections from
 * clients. *)
let start_server () =
  let sock, port = create_socket listen_address desired_port () in
  let serve = create_server sock in
  print_endline ("Started server on port " ^ string_of_int port);
  running_port := port;
  serve ()