open Char

type letter = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z

let letter_to_value = function
  |Q -> (1,1)
  |W -> (2,1)
  |E -> (3,1)
  |R -> (4,1)
  |T -> (5,1)
  |Y -> (6,1)
  |U -> (7,1)
  |I -> (8,1)
  |O -> (9,1)
  |P -> (10,1)
  |A -> (1,2)
  |S -> (2,2)
  |D -> (3,2)
  |F -> (4,2)
  |G -> (5,2)
  |H -> (6,2)
  |J -> (7,2)
  |K -> (8,2)
  |L -> (9,2)
  |Z -> (1,3)
  |X -> (2,3)
  |C -> (3,3)
  |V -> (4,3)
  |B -> (5,3)
  |N -> (6,3)
  |M -> (7,3)

let char_to_letter = function
  |'q' -> Q
  |'w' -> W
  |'e' -> E
  |'r' -> R
  |'t' -> T
  |'y' -> Y
  |'u' -> U
  |'i' -> I
  |'o' -> O
  |'p' -> P
  |'a' -> A
  |'s' -> S
  |'d' -> D
  |'f' -> F
  |'g' -> G
  |'h' -> H
  |'j' -> J
  |'k' -> K
  |'l' -> L
  |'z' -> Z
  |'x' -> X
  |'c' -> C
  |'v' -> V
  |'b' -> B
  |'n' -> N
  |'m' -> M
  | _ -> failwith "Something went wrong with spellcheck."

(* [distance x y] computes the relative distance of the two letters. *)
let distance (x:letter) (y:letter) =
  let (a,b) = letter_to_value x in
  let (c,d) = letter_to_value y in
  ((a-c) |> abs) + ((b-d) |> abs)

(* [string_to_charlst s] strips a string down into its characters and puts them
 * into a list.
 * Example : [string_to_char "hey"] = ['h';'e';'y'] *)
let string_to_charlst s =
  let rec helper i lst =
    if i < 0 then lst
    else helper (i - 1) (s.[i] :: lst)
  in helper ((String.length s)-1) []

(* [combine lst1 lst2] combines two lists of the same length into one list
 * whose elements are pairs.
 * Example: [combine ['a';'b'] ['c';'d']] = [[('a','c');('b','d')]] *)
let combine lst1 lst2 =
  let rec helper i lst1 lst2 lst =
    if i < 0 then lst
    else
      let elt1 = List.nth lst1 i in
      let elt2 = List.nth lst2 i in
      helper (i-1) lst1 lst2 ((elt1,elt2)::lst)
  in helper ((List.length lst1)-1) lst1 lst2 []

(* [compute_distance lst] computes the distance of a combined list.
 * Example: [compute_distance [('a','c');('b','d')]] = [6] *)
let rec compute_distance = function
  | [] -> 0
  | (elt1,elt2)::t -> begin
    (distance (lowercase_ascii elt1 |> char_to_letter)
    (lowercase_ascii elt2 |> char_to_letter)) + compute_distance t
  end

(* precondition: requires the two words to have the same length. *)
let total_distance w1 w2 =
  let lst1 = string_to_charlst w1 in
  let lst2 = string_to_charlst w2 in
  let comb_lst = combine lst1 lst2 in
  compute_distance comb_lst

let has_length w i = (w |> String.length) = i

(* This suggestion method requires the misspelling to be the same length as the
 * correct spelling. This suggestion method is meant to catch "hand-slipping"
 * typos, where you mean to type to type one letter but instead typed another
 * letter that is in close proximity to the former on the keyboard. *)
let rec suggest1_helper (word:string) (length:int) (sugg:string) (range:int) = function
  | [] -> sugg
  | h::t -> begin
      if (has_length h length) then
        let new_range = total_distance word h in
        if new_range <= range then suggest1_helper word length h new_range t
        else suggest1_helper word length sugg range t
      else suggest1_helper word length sugg range t
    end

let suggest1 word dict =
  let length = String.length word in
  suggest1_helper word length "no_suggestions" 30 dict

let has_close_length w1 w2 =
  (((w1 |> String.length)-(w2 |> String.length)) |> abs) < 3 (* Can change this number *)


(* [compare_front lst1 lst2] compares from the front of two words to see how
 * many same consecutive characters there are.
 * Example: "definitely" and "definately" has 5, and
 *          "argument" and "arguement" has 4. *)
let compare_front lst1 lst2 =
  let smaller = min (List.length lst1) (List.length lst2) in
  let rec helper i lst1 lst2 acc =
    if i >= smaller then acc
    else
      let elt1 = List.nth lst1 i in
      let elt2 = List.nth lst2 i in
      if elt1 = elt2 then helper (i+1) lst1 lst2 (acc+1)
      else acc
  in
  helper 0 lst1 lst2 0

(* [compare_back lst1 lst2] compares from the front of two words to see how
 * many same consecutive characters there are.
 * Example: "definitely" and "definately" has 4, and
 *          "argument" and "arguement" has 4. *)
let compare_back lst1 lst2 =
  let rev_lst1 = List.rev lst1 in
  let rev_lst2 = List.rev lst2 in
  compare_front rev_lst1 rev_lst2

let degree_of_relatedness (w1:string) (w2:string) =
  let lst1 = string_to_charlst w1 in
  let lst2 = string_to_charlst w2 in
  (compare_front lst1 lst2 ) + (compare_back lst1 lst2)

(* This suggestion method assumes the person kind of knows the word but misspelt
 * and minor portion of it, for example: believe -> beleive,
 * environment -> enviroment. This algortihm computes, both from the front and the
 * back, how many identical letters the misspelt word has with another word. We
 * call this the "degree of relatedness". The word with the highest "degree of
 * relatedness" is returned. *)
let rec suggest2_helper word sugg degree = function
  | [] -> sugg
  | h::t -> begin
      if (has_close_length word h) then
        let deg = degree_of_relatedness word h in
        if deg >= degree then suggest2_helper word h deg t
        else suggest2_helper word sugg degree t
      else suggest2_helper word sugg degree t
  end

let suggest2 word dict =
  suggest2_helper word "no_suggestions" 0 dict
