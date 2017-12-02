(* A type exposed and defined in a .mli file must also be defined in
 * the corresponding .ml file.  So you must repeat the definition
 * of [command] here.  This helps OCaml achieve something called
 * "separate compilation", which you could google for.  Yes,
 * it's a little bit annoying, but there is a good reason for it. *)
type command = Talk of string | Friend of string | Quit | Friends_list |
                Leave_conversation | Add_friend of string | Unfriend of string| Add_shortcut of (string*string) | 
                Define of string | Setstatus of string | View_requests | Error

let parse str =
  let trimmed_str = String.trim str in
  if String.contains trimmed_str ' ' 
    then 
    let space_index = String.index trimmed_str ' ' in
    let first = String.sub trimmed_str 0 space_index in
    let second = (String.sub trimmed_str (space_index + 1) (String.length trimmed_str - 1)) in
    if String.contains second ' '
      then
      let second_space = String.index second ' ' in
      let second_word = (String.sub second 0 second_space) in
      let third_word = (String.sub second (second_space + 1) (String.length second_word - 1)) in
      match (String.uppercase_ascii first) with
      | "/ADDSHORTCUT" -> Add_shortcut (second_word, third_word)
      | _ -> Error
    else
    match (String.uppercase_ascii first) with
    | "/TALK" -> Talk second
    | "/FRIEND" -> Friend second
    | "/QUIT" -> Quit
    | "/FRIENDS" -> Friends_list
    | "/LEAVE" -> Leave_conversation
    | "/ADDFRIEND" -> Add_friend second
    | "/UNFRIEND" -> Unfriend second
    | "/DEFINE" -> Define second
    | "/SETSTATUS" -> Setstatus second
    | "/REQUESTS" -> View_requests
    | _ -> Error
  else 
    failwith "Should never get here"
