open Str
open Unix
open Suggest
open Hashtbl

(* represents a message *)
type msg = string
(* represents a word. NOTE: prewords and words are defined differently below. *)
type word = string
(* represents a dictionary.
 * Hash Table used for quickly searching for words and list dictionary used for encryption*)
type dict =  string list 
type hashDict = (string, unit) Hashtbl.t
let dictionary = Hashtbl.create 1500

(* [find_preword msg] finds the prewords in a given message. A preword
 * is any maximal length sequence of characters in a that does not contain
 * any whitespace.
 * credit: in order to figure out the correct regular expression, we looked at
 * the Str module and this stackoverflow URL for help - goo.gl/GhdqCk
 *
 * Example: msg = "hello there."
 * [find_preword msg] = ["hello" ; "there"]
 *
 * Example: msg = "Hello, hello there."
 * [find_preword msg] = ["Hello," ; "hello" ; "there."]*)
let find_preword msg =
  let regexp_space = Str.regexp "[ \n\t\r\x0c]+" in
  Str.split regexp_space msg

(* [find_word msg] finds the words in a given preword. A word is any
 * maximal length subsequence of a preword that begins and ends
 * with a boundary character. In between those there may be any
 * number of any other characters. A preword that contains no boundary
 * characters does not correspond to any word.
 * precondition: the string passed in is a preword.
 *
 * Example: msg = "Hello, hello there."
 * [find_preword msg] = ["Hello" ; "hello" ; "there"]*)
let find_word msg =
  let regexp_boundary = Str.regexp "[^A-Za-z0-9]+" in
  Str.split regexp_boundary msg

(* [split_shortcut msg] splits a string of format "shortcut->phrase" to
 * a tuple (shortcut,phrase).
 * Example: "gtg->got to go" becomes ("gtg", "got to go") *)
let split_shortcut string =
  let regexp_space = Str.regexp "[->]+" in
  let shortcut_phrase_lst = Str.split regexp_space string in
  ((List.nth shortcut_phrase_lst 0), (List.nth shortcut_phrase_lst 1))

(* [split msg] splits a message into distinct words.
 * The order of the words in the list is the order of the words that
 * appear in the message.
 * The case of the words are preserved.
 *
 * Example:
 * [split "hello world!"] = ["hello"; "world"]
 * [split "Taylor Swift, 1989."] = ["Taylor"; "Swift"; "1989"] *)
let split msg =
  let preword_lst = find_preword msg in
  (List.fold_left (fun lst elt -> (find_word elt) @ lst) [] preword_lst) |> List.rev

let rec add_to_hashtble lst = 
  match lst with 
  | [] -> ()
  | h::t -> Hashtbl.add dictionary h (); add_to_hashtble t

(* [lines_in_file file] finds all the lines in a given file and
 * returns them as a string list. The newline characters at the end of each
 * line is already stripped out. *)
let rec lines_in_file_helper (c : in_channel) (acc : string list) =
  try
    lines_in_file_helper c ((c |> input_line) :: acc)
  with
  | _ -> close_in c; acc

let lines_in_file (file : string) =
  let channel = file |> open_in in
  lines_in_file_helper channel []

(* [words_in_file file] finds all the words in any given file and returns a
 * string list.
 * returns: a list that contains all the words in a file that is:
 * 1) all in lowercase
 * 2) sorted in the order as you would find in a dictionary. Words that begin
 *    with digits come before words that begin with letters. *)
let words_in_file (file : string) =
  let line_lst = lines_in_file file in
  List.fold_left (fun lst elt -> split
                     (elt |> String.lowercase_ascii) @ lst) [] line_lst |>
  List.sort_uniq Pervasives.compare

(* [shortcuts_in_file file] finds all the words in any given file and returns a
 * (shortcut:string * phrase:string) list.
 * returns: a list that contains all the words in a file that is:
 * 1) all in lowercase
 * 2) sorted in the order as you would find in a dictionary. Words that begin
 *    with digits come before words that begin with letters. *)
let shortcuts_in_file (file : string) =
  let line_lst = lines_in_file file in
  List.fold_left (fun lst elt -> split_shortcut elt :: lst) [] line_lst |>
    List.sort_uniq Pervasives.compare

let rec dir_helper (file : string) (dir : string) (handler : dir_handle) =
  try
    let next_file = handler |> Unix.readdir in
    if next_file = file then let listDict = (words_in_file next_file) in 
      add_to_hashtble listDict; listDict
    else dir_helper file dir handler
  with
  | _ -> Unix.closedir handler; []

(* [make_dict filename] checks if the filename is under the current directory.
 * If it is, make a dictionary of all the words in the filename provided.
 * If not, an empty list is returned. *)
let make_dict (file : string) : dict =
  let d_handle = Unix.getcwd () |> Unix.opendir in
  dir_helper (file ^ ".txt") (Unix.getcwd ()) d_handle

(* The main dictionary to be used for reference. *)
let (d:dict) = make_dict "dict"
(* A smaller dictionary to be used for testing. *)
(*let (sd:dict) = make_dict "smalldict"*)
(* A reversed dictionary for decryption purposes. *)
let (drev:dict) = List.rev d
(* The references for shortcuts. *)
let sc = shortcuts_in_file "shortcut.txt"

(* [flush_list c] is a helper for add_word *)
let rec flush_list (c:out_channel) = function
  | [] -> close_out c
  | h :: t -> output_string c (h ^ "\n"); flush c; flush_list c t

(* [add_word w] adds a user defined word into the file "userdef.txt" *)
let add_word (w:word) =
  Hashtbl.add dictionary w ();
  let orig_lst = "userdef.txt" |> words_in_file in
  let new_list = w::orig_lst in
  let c = "userdef.txt" |> open_out in
  flush_list c new_list

(* [flush_list' c] is a helper for add_shortcut *)
let rec flush_list' (c:out_channel) = function
  | [] -> close_out c
  | (sc,phr) :: t -> output_string c (sc ^ "->" ^ phr ^ "\n"); flush c; flush_list' c t

(* [add_shortcut sc phrase] adds a user defined shortcut
 * into the file "shortcut.txt" *)
let def (sc : word) (phrase : string) =
  let orig_lst = "shortcut.txt" |> shortcuts_in_file in
  let new_list = (sc,phrase)::orig_lst in
  let c = "shortcut.txt" |> open_out in
  flush_list' c new_list

(* [ignore_word w] adds a word to be ignored into the file "ignore.txt"
 * That file is cleaned every time a new message is entered, so invalid
 * words are only ignored once. *)
let ignore_word (w:word) =
  let c = "ignore.txt" |> open_out in
  flush_list c [w]

(* [word_is_valid word] checks whether a word is "valid", i.e., it passes
 * the spellcheck test and is not a defined shortcut.
 * example: [is_valid "hello"] = true
 *          [is_valid "hallo"] = false *)
let word_is_valid word (dict:dict) =
  let word' = String.lowercase_ascii word in
  try 
    Hashtbl.find dictionary word'; true
  with 
  | e -> false

(* [suggest word] gives suggestions for a word that is invalid.
 * example: [suggest "hallo"] = ["hello"] *)
let suggest (w:word) =
  Suggest.suggest2 w d

let suggest' (w:word) =
  Suggest.suggest1 w d

let cmd_add (w:word) =
  print_endline ("The word [" ^ w ^ "] is added successfully.");
  add_word w

(* [replace w sub msg] replaces the first instance of the word [w] and
 * replaces it with the word [sub].
 * All other parts of the message is identical.*)
let replace (w:word) (sub:word) msg : string =
  let reg_expr = Str.regexp w in
  Str.replace_first reg_expr sub msg

(* [is_valid_shortcut sc ref_table] checks if the shortcut is defined in
 * a shortcut reference table created by [shortcuts_in_file].
 * If it is, return true .
 * Otherwise, return false *)
let rec is_valid_shortcut sc ref_table =
  match ref_table with
  | [] -> false
  | (short,refer) :: t -> if sc = short then true
    else is_valid_shortcut sc t

(* [try_shortcut sc ref_table] checks if the shortcut is defined in
 * a shortcut reference table created by [shortcuts_in_file].
 * If it is, return [the full length of the phrase].
 * Otherwise, throw an exception *)
let rec try_shortcut sc ref_table =
  match ref_table with
  | [] -> failwith "shortcut does not exist"
  | (short,refer) :: t -> if sc = short then refer
    else try_shortcut sc t

(* [list_to_msg lst] changes a list or words into a message.
 * example: [list_to_msg ["hello"; "there" ; "hallo"] =
 *          ["hello there hallo"]*)
let rec list_to_msg = function
  | [] -> ""
  | h :: t -> h ^ " " ^ list_to_msg t

let cmd_replace w sub msg : msg =
  replace w sub msg

let cmd_manual_suggest w msg =
  print_endline ("Enter the word that you meant to type: ");
  let input = read_line() in begin
    replace w input msg
  end

let cmd_ignore (w:word) =
  print_endline ("Overlooking invalid word [" ^ w ^ "].");
  ignore_word w

let print_instructions (w:word) : unit =
  let s (* suggested_word *) = suggest w in
  let s' (* suggested word 2*) = suggest' w in
  print_endline ("\n[" ^ w ^ "] is not a valid word.\n\
                 Type a following number to proceed.\n\
                 ___________________________________\n\
                 0. add the word [" ^ w ^ "] to the dictionary.\n\
                 1. suggestion: replace word with [" ^ s ^ "].\n\
                 2. suggestion: replace word with [" ^ s' ^ "].\n\
                 3. manually type in the correct word.\n\
                 4. ignore spellcheck.\n")

(* [send msg] checks whether all the words in a message is "valid",
 * i.e., it passes the spellcheck test and is not a defined shortcut.
 * It alters the message until all the words are valid. *)
let rec send (msg : msg) : msg =
  let word_lst = split msg in
  let wait4response (w:word) (msg:msg) : msg =
    let input = read_int() in begin
      if input = 0 then (cmd_add w; send msg)
      else if input = 1 then (cmd_replace w (suggest w) msg) |> send
      else if input = 2 then (cmd_replace w (suggest' w) msg) |> send
      else if input = 3 then (cmd_manual_suggest w msg) |> send
      else (cmd_ignore w; send msg)
    end in
  let rec send_helper dict word_lst =
    match word_lst with
    | [] -> (print_endline "No spellcheck errors found."; msg)
    | h :: t -> if (word_is_valid h dict) then send_helper dict t
      else if (is_valid_shortcut h (shortcuts_in_file "shortcut.txt"))
      then send (cmd_replace h (try_shortcut h (shortcuts_in_file "shortcut.txt")) msg)
      else (print_instructions h; wait4response h msg)
  in send_helper (d @ make_dict "userdef" @ make_dict "ignore") word_lst

(* exactly the same as [send], except it prints the message in a cute way. *)
let send' msg : unit =
print_endline ("\n\n

                         ''~``\n
                        ( o o )\n
+------------------.oooO--(_)--Oooo.-----------------+\n
"
^ send msg ^ "\n" ^
"                    .oooO\n
                     (   )   Oooo.\n
+--------------------\\  (----(   )-------------------+\n
                      \\_)     ) /\n
                              (_/\n

")
(* represents a key for encryption and decryption. *)
type key = int list

(* [encyrpt_keygen msg] creates a key for a certain message that is encyrpted.*)
let encyrpt_keygen msg =
  let lst = split msg in
  let rec keygen_helper = function
  | [] -> []
  | h :: t -> begin
      if (word_is_valid h d)
      then String.length h :: keygen_helper t
      else 0 :: keygen_helper t
    end
  in
  keygen_helper lst

(* [flush_list'' c] is a helper for generate_keygen *)
let rec flush_list (c:out_channel) = function
  | [] -> close_out c
  | h :: t -> output_string c ((string_of_int h) ^ "\n"); flush c; flush_list c t

(* [encyrpt_keygen msg] creates "key.txt" that is generated onto the computer. *)
let generate_keygen msg =
  flush_list ("key.txt" |> open_out) (encyrpt_keygen msg)

let rec encyrpt_helper_recurse i (rep : int) = function
  | [] -> failwith "the dictionary reached its end"
  | h :: t -> if i = rep then h else encyrpt_helper_recurse i (rep+1) t

let rec encyrpt_helper (i : int) (w : word) = function
  | [] -> failwith "something went wrong with encyrption"
  | h::t -> if h = (w |> String.lowercase_ascii) then encyrpt_helper_recurse i 1 t
    else encyrpt_helper i w t

(* [encyrpt_word w] finds the word in the dictionary that is n words below the
 * word w, n being the length of w. If w is not a valid word, w is returned. *)
let encyrpt_word w =
  if (word_is_valid w d) then
    let key = encyrpt_keygen w in
    let lst_to_int lst=
      match lst with
      | i :: [] -> i
      | _  -> failwith "something went wrong with encyrption'"
    in
    encyrpt_helper (lst_to_int key) w d
  else w

(* [encyrpt_msg msg] encyrpts a message and returns a word list of the
 * encyrpted message *)
let encyrpt_msg msg =
  let corrected_msg = send msg in generate_keygen msg;
  let word_lst = split corrected_msg in
  let rec encyrpt_helper = function
    | [] -> []
    | h::t -> encyrpt_word h :: encyrpt_helper t
  in encyrpt_helper word_lst

let pm msg : msg =
  let word_lst = encyrpt_msg msg in
  let rec helper = function
    | [] -> ""
    | h::t -> h ^ " " ^ helper t
  in (helper word_lst) |> String.trim

(* [decyrpt_word w] finds the word in the dictionary that is n words above the
 * word w, n being a corresponding number from "key.txt". *)
let decyrpt_word w (i:int) =
  if i = 0 then w
  else encyrpt_helper i w drev

(* [decyrpt_msg msg] encyrpts a message and returns a word list of the
 * decyrpted message *)
let decyrpt_msg msg =
  let num_lst = (lines_in_file "key.txt") |> List.rev in
  let word_lst = split msg in
  let rec decyrpt_helper (count : int) = function
    | [] -> []
    | h :: t -> (decyrpt_word h ((List.nth num_lst count) |> int_of_string)) ::
                decyrpt_helper (count+1) t
  in decyrpt_helper 0 word_lst

let depm msg =
  let word_lst = decyrpt_msg msg in
  let rec helper = function
    | [] -> ""
    | h::t -> h ^ " " ^ helper t
  in (helper word_lst) |> String.trim

(* NOTE: [ignore] doesn't work as intended. only one word can be ignored
   (because of how the file is flushed every time a new word is ignored), and
   the same word is ignored until a new word is ignored. *)
