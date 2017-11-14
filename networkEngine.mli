(* A [NetworkEngine] facilitates communication among several clients. Handles    
 * the sending of messages and statuses to and from clients. *)
module type NetworkEngine = sig

    (* [send_friend_request ip_address st] sends a conversation request to the client at [ip_address]. *)
    val send_friend_request: string -> state

    (* [send_message msg st] sends [msg] to the user who we are currently conversing with according to [st]. *)
    val send_message: string -> state -> state

   (* [update_status status st] sends the new status [status] to all online friends who are listed in the friends list according to [st] *)
    val update_status: string -> state -> state

end