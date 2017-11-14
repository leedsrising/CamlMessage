(* An [MessageTransformer] takes in messages and does something to the message,
 * including spellchecking and replacing shortcuts *)
module type MessageTransformer = sig

    (* The type of a message *)
    type msg

    (* The type of a word *)
    type word

    (* [split msg] splits a message into distinct words.
     * example: [split "Hello there, friend."] = ["hello";"there";"friend"] *)
    val split : msg -> word list

    (* [is_valid word] checks whether a word is "valid", i.e., it passes
     * the spellcheck test and is not a defined shortcut.
     * example: [is_valid "hello"] = true
     *          [is_valid "hallo"] = false *)
    val is_valid : word -> bool

    (* [validify word] adds the word to the dictionary such that it passes
     * is_valid in subsequent checks. *)
    val validify : word -> unit

    (* [suggest word] gives suggestions for a word that is invalid.
     * example: [suggest "hallo"] = ["hello"] *)
    val suggest : word -> word list

    (* [replace msg] replaces a word that is a shortcut with the actual word(s).
     * example: [replace "omg"] = ["oh my gawd"] *)
    val replace : word -> word

end
