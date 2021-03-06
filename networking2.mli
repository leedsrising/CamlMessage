type conn_addr = {ip:string; port:int; fd:Lwt_unix.file_descr}
type net_state = {out_buffer:string option ; do_close:bool; addr:conn_addr}

(* A [NetworkEngine] facilitates communication among several clients. Handles    
 * the sending of messages and statuses to and from clients. *)


val get_running_port : unit -> int

val to_ip_port : Lwt_unix.sockaddr -> string * int

val handle_write: 'a Lwt_io.channel ->
Lwt_io.output_channel -> net_state ref -> unit -> unit Lwt.t
 
val handle_message: string -> net_state ref -> unit Lwt.t
 
val handle_read: Lwt_io.input_channel -> 'a -> net_state ref -> unit -> unit Lwt.t

val accept_connection: Lwt_unix.file_descr * Lwt_unix.sockaddr -> net_state ref Lwt.t

val create_server: Lwt_unix.file_descr -> unit -> 'a Lwt.t

val create_socket: Lwt_unix.inet_addr -> int -> unit -> Lwt_unix.file_descr * int

val make_connection: Lwt_unix.file_descr -> conn_addr -> unit -> net_state ref Lwt.t

(* val send_uni_cmd: string -> int -> string -> unit Lwt.t*)

val close: string -> int -> unit

val send_cmd: string -> int -> string -> unit Lwt.t

val send_friend_req: string -> int -> string -> unit Lwt.t

val register_read_listener: (net_state ref -> string -> unit) -> unit

val register_disconnect_listener : (net_state ref -> unit) -> unit

val start_server: unit -> 'a Lwt.t



