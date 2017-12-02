open Core.Std
open Async.Std

type state = {username: string; out_buffer: string}

(* Credit: This file is inspired by partially taken from 
 https://realworldocaml.org/v1/en/html/concurrent-programming-with-async.html *)

(* Copy data from the reader to the writer, using the provided buffer
   as scratch space *)
   
let rec do_read r w = 
let buffer = Bytes.create (16 * 1024) in
  Reader.read r buffer
>>= function
| `Eof -> return ()
| `Ok bytes_read ->
  (* Writer.write w buffer ~len:bytes_read; *)
  let trimmed = String.strip buffer in
  
  print_endline ("buffer: " ^ trimmed);
  
  (* msgref := trimmed; *)
  (* print_endline ("msgref: " ^ !msgref); *)
  Writer.flushed w
  >>= fun () ->
  do_read r w

let do_write r w msgref =

  let poller = fun () -> 
  begin match !msgref with
    | "" -> `Continue_polling
    | s -> (Writer.write w !msgref; msgref := ""); `Continue_polling
    end in Scheduler.add_busy_poller poller

(** Starts a TCP server, which listens on the specified port, invoking
    copy_blocks every time a client connects. *)
    let run port st =
      print_endline "Server started!";
      let host_and_port =
        Tcp.Server.create
          ~on_handler_error:`Raise
          (Tcp.on_port port)
          (fun _addr r w ->
              (* ignore (do_write r w msgref2); *)
              do_read r w
             )
      in
      ignore (host_and_port : (Socket.Address.Inet.t, int) Tcp.Server.t Deferred.t)



let do_connect ip port st =

  let rec client socket reader writer = 
    Clock.after (sec 0.25) >>= fun () -> 
    begin 
      (match ((!st).out_buffer) with
      | "" -> ()
      | s -> Writer.write_line writer s; st := {!st with out_buffer = ""}
      );
      Writer.flushed writer >>= fun () -> client socket reader writer
    end
    in 
  ignore (Tcp.with_connection (Tcp.to_host_and_port ip port)
   client)

let rec handle_connect splitcmd st = 
  let usage = "Usage: /connect <ipAddress> <port>" in 
  match splitcmd with
  | None -> print_endline usage
  | Some lst -> 
    match lst with 
    | ip::port_s::[] -> print_endline ("connecting to: " ^ ip ^ ":" ^ port_s);
      begin match int_of_string_opt port_s with
      | None -> print_endline ""
      | Some port -> do_connect ip port st
      end
    | _ -> print_endline usage

let rec handle_command str st = 
  let split = Str.split (Str.regexp " ") str in
  if List.length split <= 0 then
    print_endline "Error: Unrecognized Command." 
  else
    let cmd = List.hd_exn split in 
      match cmd with
      | "/connect" -> handle_connect (List.tl split) st
      | s -> st := {!st with out_buffer = s}

let rec repl reader st = 
  Reader.read_line reader >>= 
    fun result -> 
      match result with
      | `Eof -> exit 0
      | `Ok s -> handle_command s st; repl reader st


let () =
  let st = ref {username= "brian"; out_buffer = ""} in
    print_endline "Welcome to CamlMsg!";
    ignore (repl (Lazy.force Reader.stdin) st);
    run 3111 st;
    never_returns (Scheduler.go ())