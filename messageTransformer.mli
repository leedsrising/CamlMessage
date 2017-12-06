(* An [MessageTransformer] takes in messages and does something to the message,
 * including spellchecking and replacing shortcuts *)

    (* The type of a message *)
    type msg
    (* The type of a word *)
    type word
    (* The type of a dictionary *)
    type dict

    (* [send] checks whether all the words in a message is "valid",
     * i.e., it passes the spellcheck test and is not a defined shortcut.
     * It alters the message until all the words are valid. *)
    val send : msg -> msg

    (* [def] defines a user defined shortcut *)
    val def : word -> word -> unit

    (* [pm] encyrpts a message *)
    val pm : msg -> msg

    (* [depm] decyrpts a message *)
    val depm : msg -> msg

    val lines_in_file : string -> string list

    val make_dict : string -> dict
