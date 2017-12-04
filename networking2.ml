open Lwt
open Str

let () = Lwt_log.add_rule "*" Lwt_log.Info

let listen_address = Unix.inet_addr_loopback
let listen_port = 9000
let backlog = 10

type net_state = {out_buffer:string option; do_close:bool}

let rec handle_write ic oc st_r () =

  print_endline "handle";
  (match !st_r.out_buffer with
  | None -> return_unit
  | Some s -> 
    begin st_r := {!st_r with out_buffer = None}; Lwt_io.write_line oc s end >>=

    (fun () -> Lwt_unix.sleep 0.5 >>= fun () -> 
    match !st_r.do_close with
    | true -> Lwt_io.close ic >>= fun () -> Lwt_io.close oc
    | false -> handle_write ic oc st_r ()) (*connection remains open *)

let handle_message msg =
  Lwt_io.printl msg

let rec handle_read ic oc () =
  Lwt_io.read_line_opt ic >>=
  (fun msg ->
      match msg with
      | Some msg -> handle_message msg >>= handle_read ic oc
      | None -> Lwt_log.info "Connection closed" >>= return)

(* accept incoming connecting to server *)
let accept_connection conn =
  print_endline "connection accepted";
  let fd, _ = conn in
  let ic = Lwt_io.of_fd Lwt_io.Input fd in
  let oc = Lwt_io.of_fd Lwt_io.Output fd in
  Lwt.on_failure (handle_read ic oc ()) (fun e -> Lwt_log.ign_error (Printexc.to_string e));

  let net_state = ref {out_buffer = None; do_close = false} in 
  Lwt.on_failure (handle_write ic oc net_state ()) (fun e -> Lwt_log.ign_error (Printexc.to_string e));
  Lwt_log.info "New connection" >>= fun() -> return net_state

let create_server sock =
  print_endline "server created";
  let rec serve () =
      print_endline "serve";
      Lwt_unix.accept sock >>= accept_connection >>= fun _ -> serve ()
  in serve

let create_socket addr port () =
  let open Lwt_unix in
  let sock = socket PF_INET SOCK_STREAM 0 in
  ignore (bind sock @@ ADDR_INET(addr, port));
  listen sock backlog;
  sock

(* create client handlers for outgoing connection *)
let rec make_connection conn () =
  print_endline "connectingasda..";
  let fd = conn in
  let ic = Lwt_io.of_fd Lwt_io.Input fd in
  let oc = Lwt_io.of_fd Lwt_io.Output fd in
  Lwt.on_failure (handle_read ic oc ()) (fun e -> Lwt_log.ign_error (Printexc.to_string e));

  let net_state = ref {out_buffer = None; do_close = false} in
  Lwt.on_failure (handle_write ic oc net_state ()) (fun e -> Lwt_log.ign_error (Printexc.to_string e));
  Lwt_log.info "Connected to remote" >>= fun () -> return net_state

(* connects to remote *)
let rec do_connect ip port = 
  let open Lwt_unix in
  print_endline "doc";
  let addr = Unix.inet_addr_of_string ip in
  print_endline "doc2";
  let sock = socket PF_INET SOCK_STREAM 0 in  
  print_endline "doc3";
  connect sock (Lwt_unix.ADDR_INET (addr, port)) >>=
  make_connection sock

let send_friend_req ip port from_name = 
  do_connect ip port >>=
  fun net_state -> 
    return (net_state := {!net_state with out_buffer = Some ("friendreq " ^ from_name)})

let start_server () =
  print_endline ("Starting server on port " ^ string_of_int listen_port);
  let sock = create_socket listen_address listen_port () in 
  let serve = create_server sock in
  serve ()

