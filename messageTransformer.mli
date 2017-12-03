(* An [MessageTransformer] takes in messages and does something to the message,
 * including spellchecking and replacing shortcuts *)
module type MessageTransformer = sig

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

end
