open Lwt
open Str

type state = {username: string; out_buffer: string option}

let st = ref {username= "brian"; out_buffer = None}

let () = Lwt_log.add_rule "*" Lwt_log.Info

let counter = ref 0

let listen_address = Unix.inet_addr_loopback
let listen_port = 9000
let backlog = 10

let rec handle_write_server ic oc () =
  (match !st.out_buffer with
  | None -> return_unit
  | Some s -> st := {!st with out_buffer = None}; Lwt_io.write_line oc s)
  >>= (fun () -> Lwt_unix.sleep 0.5) >>=
  handle_write_server ic oc


let handle_message msg =
  Lwt_io.printl msg

let rec handle_read_server ic oc () =
  Lwt_io.read_line_opt ic >>=
  (fun msg ->
      match msg with
      | Some msg -> handle_message msg >>= handle_read_server ic oc
      | None -> Lwt_log.info "Connection closed" >>= return)

let accept_connection conn =
  let fd, _ = conn in
  let ic = Lwt_io.of_fd Lwt_io.Input fd in
  let oc = Lwt_io.of_fd Lwt_io.Output fd in
  Lwt.on_failure (handle_read_server ic oc ()) (fun e -> Lwt_log.ign_error (Printexc.to_string e));
  Lwt.on_failure (handle_write_server ic oc ()) (fun e -> Lwt_log.ign_error (Printexc.to_string e));
  Lwt_log.info "New connection" >>= return

let create_server sock =
  let rec serve () =
      Lwt_unix.accept sock >>= accept_connection >>= serve
  in serve

let create_socket addr port () =
  let open Lwt_unix in
  let sock = socket PF_INET SOCK_STREAM 0 in
  ignore (bind sock @@ ADDR_INET(addr, port));
  listen sock backlog;
  sock

(* spawn client handlers *)
let rec handle_client conn () =
  print_endline "connectingasda..";
  let fd = conn in
  let ic = Lwt_io.of_fd Lwt_io.Input fd in
  let oc = Lwt_io.of_fd Lwt_io.Output fd in
  Lwt.on_failure (handle_read_server ic oc ()) (fun e -> Lwt_log.ign_error (Printexc.to_string e));
  Lwt.on_failure (handle_write_server ic oc ()) (fun e -> Lwt_log.ign_error (Printexc.to_string e));
  Lwt_log.info "Connected to remote" >>= return

(* connects to remote *)
let rec do_connect ip port = 
  let open Lwt_unix in
  print_endline "doc";
  let addr = Unix.inet_addr_of_string ip in
  print_endline "doc2";
  let sock = socket PF_INET SOCK_STREAM 0 in  
  print_endline "doc3";
  connect sock (Lwt_unix.ADDR_INET (addr, port)) >>=
  handle_client sock

let rec handle_connect splitcmd = 
  let usage = "Usage: /connect <ipAddress> <port>" in 
  match splitcmd with
  | [] -> Lwt_io.printl usage
  | lst -> 
    match lst with 
    | ip::port_s::[] -> 
      begin match int_of_string_opt port_s with
      | None -> Lwt_io.printl usage
      | Some port -> print_endline ("connecting to: " ^ ip ^ ":" ^ port_s); do_connect ip port
      end
    | _ -> Lwt_io.printl usage
  
let rec handle_command str = 
  let split = Str.split (Str.regexp " ") str in
  if List.length split <= 0 then
    Lwt_io.printl "Error: Unrecognized Command."
  else
    let cmd = List.hd split in
      match cmd with
      | "/connect" -> handle_connect (List.tl split)
      | s ->  st := {!st with out_buffer = Some str}; return_unit
  
let rec repl () = 
  Lwt_io.read_line_opt Lwt_io.stdin >>= function
  | None -> return_unit
  | Some s -> handle_command s >>= repl

let start_server () =
    let sock = create_socket listen_address listen_port () in 
    let serve = create_server sock in
    serve ()