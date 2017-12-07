open Lwt
open Str

(* module Networking2 = struct *)

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

let get_running_port = !running_port

let to_ip_port saddr =
  match saddr with
  | Lwt_unix.ADDR_INET (ip, port) -> (Unix.string_of_inet_addr ip, port)
  | _ -> failwith "bad saddr type"

(* universal writer *)
let rec handle_write ic oc st_r () = 
  (match !st_r.out_buffer with
  | None -> return_unit
  | Some s -> print_endline ("writing: " ^ s);
    begin st_r := {!st_r with out_buffer = None};
    Lwt_io.write_line oc s >>= fun () -> Lwt_io.flush oc end) >>=
    fun () -> Lwt_unix.sleep 0.5 >>= fun () ->
    match !st_r.do_close with
    | true -> connections := List.remove_assoc (!st_r.addr.ip, !st_r.addr.port)
     !connections; 
      return (Lwt_unix.shutdown !st_r.addr.fd Lwt_unix.SHUTDOWN_ALL)
    | false -> handle_write ic oc st_r () (*connection remains open *)

  (* TODO: check for thread problems here *)
let handle_message msg st_r =
  ignore (List.map (fun (listener_fun:(net_state ref -> string -> unit)) ->
    listener_fun st_r msg) !message_listeners); return_unit
    (* listener_fun !st_r.addr.ip !st_r.addr.port msg) !listeners); return_unit *)

let handle_disconnect st_r =
  ignore (List.map (fun (listener_fun:(net_state ref -> unit)) ->
    listener_fun st_r) !disconnect_listeners); return_unit
    (* listener_fun !st_r.addr.ip !st_r.addr.port msg) !listeners); return_unit *)
    
(* universal reader *)
let rec handle_read ic oc st_r () =
  Lwt_io.read_line_opt ic >>=
  (fun msg ->
      match msg with
      | Some msg -> handle_message msg st_r >>= handle_read ic oc st_r
      | None -> st_r := {!st_r with do_close = true};
        handle_disconnect st_r >>= fun () -> 
          Lwt_log.info "RConnection closed" >>= return)

let handle_sock_error ns err = 
  Lwt_log.ign_error (Printexc.to_string err);
  ns := {!ns with do_close = true }

(* accept incoming connecting to server *)
let accept_connection conn =
  let open Lwt_unix in
  print_endline "connection accepted";
  let fd, saddr = conn in
  let (ip, port) = to_ip_port saddr in
  let ic = Lwt_io.of_fd Lwt_io.Input fd in
  let oc = Lwt_io.of_fd Lwt_io.Output fd in
  let net_state = ref {out_buffer = None; do_close = false; addr = {ip; port; fd=fd}} in
  Lwt.on_failure (handle_read ic oc net_state ()) (handle_sock_error net_state);
  Lwt.on_failure (handle_write ic oc net_state ()) (handle_sock_error net_state);
  connections := ((ip, port), net_state) :: !connections;
  Lwt_log.info "New connection" >>= fun() -> return net_state

let create_server sock =
  print_endline "server created";
  let rec serve () =
      print_endline "serve";
      Lwt_unix.accept sock >>= accept_connection >>= fun _ -> serve ()
  in serve

let rec create_socket ip_addr port () =
  let open Lwt_unix in
  try let sock = socket PF_INET SOCK_STREAM 0 in
  ignore (bind sock @@ ADDR_INET(ip_addr, port));
  listen sock backlog;
  (sock, port) with e -> create_socket ip_addr (port + 1) ()

(* create client handlers for outgoing connection *)
let rec make_connection conn addr () =
  print_endline "connectingasda..";
  let fd = conn in
  let ic = Lwt_io.of_fd Lwt_io.Input fd in
  let oc = Lwt_io.of_fd Lwt_io.Output fd in
  let net_state = ref {out_buffer = None; do_close = false; addr=addr} in
  Lwt.on_failure (handle_read ic oc net_state ()) (handle_sock_error net_state);
  Lwt.on_failure (handle_write ic oc net_state ()) (handle_sock_error net_state);
  connections := ((addr.ip, addr.port), net_state) :: !connections;
  Lwt_log.info "Connected to remote" >>= fun () -> return net_state

(* connects to remote *)
let rec do_connect ip port =
  let open Lwt_unix in
  print_endline ("doc: " ^ ip ^ ":" ^ (string_of_int port));
  let addr = Unix.inet_addr_of_string ip in
  print_endline "doc2";
  let sock = socket PF_INET SOCK_STREAM 0 in
  print_endline "doc3";
  connect sock (Lwt_unix.ADDR_INET (addr, port)) >>=
  make_connection sock {ip=ip; port=port;fd=sock}

(* [send_uni_cmd ip port cmd_msg] connects to the socket at
 * [ip], [port] and sends the string [cmd_msg]*)
 let send_uni_cmd ip port cmd_msg =
  do_connect ip port >>=
  fun net_state ->
    return (net_state := {!net_state with out_buffer = Some (cmd_msg);})

let send_cmd ip port cmd_msg =
  let conn_opt = List.assoc_opt (ip,port) !connections in
    match conn_opt with
    | Some net_state -> print_endline "sendingCmdExistingNetState";
      return (net_state := {!net_state with out_buffer = Some (cmd_msg);})
    | None -> send_uni_cmd ip port cmd_msg

let send_friend_req ip port from_name =
  do_connect ip port >>=
  fun net_state ->
    return (net_state := {!net_state with out_buffer =
    Some ("friendreq " ^ from_name ^ " " ^ (string_of_int !running_port));})

let close ip port = 
  let ns_opt = List.assoc_opt (ip, port) !connections in
    match ns_opt with
    | Some ns -> ns := {!ns with do_close = true}
    | None -> ()

let register_read_listener listener =
  message_listeners := listener :: !message_listeners

let register_disconnect_listener listener =
  disconnect_listeners := listener :: !disconnect_listeners

let start_server () =
  print_endline ("Starting server...");
  let sock, port = create_socket listen_address desired_port () in
  let serve = create_server sock in
  print_endline ("Started server on port " ^ string_of_int port);
  running_port := port;
  serve ()

(* end *)
