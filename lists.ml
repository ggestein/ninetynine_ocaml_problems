(*
  Tail of a List
  Write a function last : 'a list -> 'a option that returns the last element of a list

  # last ["a" ; "b" ; "c" ; "d"];;
  - : string option = Some "d"
  # last [];;
  - : 'a option = None
 *)
let rec last = function
  | [] -> None
  | [x] -> Some x
  | x :: rest -> last rest
;;

(*
  Last Two Elements of a List
  Find the last two (last and penultimate) elements of a list.

  # last_two ["a"; "b"; "c"; "d"];;
  - : (string * string) option = Some ("c", "d")
  # last_two ["a"];;
  - : (string * string) option = None
 *)
let rec last_two = function
  | [] -> None
  | [x] -> None
  | [x; y] -> Some (x, y)
  | x :: rest -> last_two rest
;;

(*
  N'th element of a List
  Find the N'th element of a list.

  # at 2 ["a"; "b"; "c"; "d"; "e"];;
  - : string option = Some "c"
  # at 2 ["a"];;
  - : string option = None
 *)
let rec at n (xs: 'a list) =
  if n < 0 then None
  else if n > 0 then
      match xs with
      | [] -> None
      | x :: rest -> at (n - 1) rest
  else match xs with
       | [] -> None
       | x :: _ -> Some x
;;

(*
  Length of a List
  Find the number of elements of a list.

  # length ["a"; "b"; "c"];;
  - : int = 3
  # length [];;
  - : int = 0
 *)
let rec length = function
  | [] -> 0
  | x :: rest -> 1 + (length rest)
;;

(*
  Reverse a List
  Reverse a list.
  OCaml standard library has List.rev but we ask that you reimplement it.

  # rev ["a"; "b"; "c"];;
  - : string list = ["c"; "b"; "a"]
 *)
let rec rev = function
  | [] -> []
  | x :: rest -> (rev rest) @ [x]
;;

(*
  Palindrome
  Find out whether a list is a palindrome.
  Hint: A palindrome is its own reverse.

  # is_palindrome ["x"; "a"; "m"; "a"; "x"];;
  - : bool = true
  # not (is_palindrome ["a"; "b"]);;
  - : bool = true
 *)
let rec is_palindrome (xs: 'a list) =
  xs = (List.rev xs)
;;

(*
  Flatten a List
  Flatten a nested list structure.

  type 'a node =
  | One of 'a 
  | Many of 'a node list
  # flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]];;
  - : string list = ["a"; "b"; "c"; "d"; "e"]
 *)
type 'a node =
  | One of 'a
  | Many of 'a node list
;;
let rec flatten = function
  | [] -> []
  | [x] ->
     (
         match x with
         | One i -> [i]
         | Many il -> flatten il 
     )
  | x :: rest -> (flatten [x]) @ (flatten rest)
;;

(*
  Eliminate Duplicates
  Eliminate consecutive duplicates of list elements.

  # compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
  - : string list = ["a"; "b"; "c"; "a"; "d"; "e"]
 *)
let rec compress = function
  | [] -> []
  | [x] -> [x]
  | x :: y :: rest ->
     let yrest = y :: rest in
     if x = y then (compress yrest) else (x :: (compress yrest))
;;

(*
  Pack Consecutive Duplicates
  Pack consecutive duplicates of list elements into sublists.

  # pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"];;
  - : string list list =
  [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
  ["e"; "e"; "e"; "e"]]
 *)
let rec pack = function
  | [] -> []
  | [x] -> [[x]]
  | x :: rest ->
     let pr = pack rest in
     match pr with
     | [] -> [[x]]
     | y :: rest -> if (List.hd y) = x then (x :: y) :: rest else [x] :: y :: rest
;;

(*
  Run-Length Encoding
  If you need so, refresh your memory about run-length encoding.
  [http://en.wikipedia.org/wiki/Run-length_encoding]

  # encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
  - : (int * string) list =
  [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]
 *)
let encode_0 xs =
  let inner_fold_func acc x =
    match acc with
    | [] -> [(1, x)]
    | a :: rest ->
       let (cnt, letter) = a in
       if letter = x then (cnt + 1, letter) :: rest
       else (1, x) :: a :: rest in
  List.rev (List.fold_left inner_fold_func [] xs)
;;
(*
  Modified Run-Length Encoding
  Modify the result of the previous problem in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.
  Since OCaml lists are homogeneous, one needs to define a type to hold both single elements and sub-lists.

  type 'a rle =
  | One of 'a
  | Many of int * 'a
  # encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
  - : string rle list =
  [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
  Many (4, "e")]
 *)
type 'a rle =
  | One of 'a
  | Many of int * 'a
;;
let encode_1 xs =
  let mff x =
    let (cnt, letter) = x in
    if cnt = 1 then One letter else Many (cnt, letter) in
  let mfb x =
    match x with
    | One letter -> (1, letter)
    | Many (cnt, letter) -> (cnt, letter) in
  let encode_1_fold acc x =
    match acc with
    | [] -> [One x]
    | a :: rest ->
       let (cnt, letter) = (mfb a) in
       if letter = x then mff ((cnt + 1), letter) :: rest
       else mff (1, x) :: a :: rest in
  List.rev (List.fold_left encode_1_fold [] xs)
;;

(*
  Decode a Run-Length Encoded List
  Given a run-length code list generated as specified in the previous problem, construct its uncompressed version.

  #  decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")];;
  - : string list =
  ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
 *)
let rec decode xs =
  let rec dup n e =
    if n = 0 then []
    else e :: (dup (n - 1) e) in
  let dup_expand = function
    | One a -> [a]
    | Many (cnt, elem) -> dup cnt elem in
  match xs with
  | [] -> []
  | a :: rest -> (dup_expand a) @ (decode rest)
;;
(*
  Run-Length Encoding of a List (Direct Solution)
  Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem "Pack consecutive duplicates of list elements into sublists", but only count them. As in problem "Modified run-length encoding", simplify the result list by replacing the singleton lists (1 X) by X.

  # encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
  - : string rle list =
  [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
  Many (4, "e")]
 *)
(* TODO *)

(*
  Duplicate the Elements of a List
  Duplicate the elements of a list.

  # duplicate ["a"; "b"; "c"; "c"; "d"];;
  - : string list = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"]
 *)
let rec duplicate = function
  | [] -> []
  | a :: rest -> a :: a :: (duplicate rest)
;;

(*
  Replicate the Elements of a List a Given Number of Times
  Replicate the elements of a list a given number of times.

  # replicate ["a"; "b"; "c"] 3;;
  - : string list = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"]
 *)
let rec replicate xs n =
  let rec dup_n x n = if n = 0 then [] else x :: (dup_n x (n - 1)) in
  match xs with
  | [] -> []
  | a :: rest -> (dup_n a n) @ (replicate rest n)
;;

(*
  Drop Every N'th Element From a List
  Drop every N'th element from a list.

  # drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;;
  - : string list = ["a"; "b"; "d"; "e"; "g"; "h"; "j"]
 *)
let rec drop xs n =
  let rec drop_n acc xs =
    match xs with
    | [] -> []
    | a :: rest -> if acc mod n = 0 then drop_n (acc + 1) rest else a :: (drop_n (acc + 1) rest)
  in drop_n 1 xs
;;
(*
  Split a List Into Two Parts; The Length of the First Part Is Given
  Split a list into two parts; the length of the first part is given.

  If the length of the first part is longer than the entire list, then the first part is the list and the second part is empty.

  # split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;;
  - : string list * string list =
  (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"])
  # split ["a"; "b"; "c"; "d"] 5;;
  - : string list * string list = (["a"; "b"; "c"; "d"], [])
 *)
let rec split xs n =
  if n = 0 then ([], xs)
  else
    match xs with
    | [] -> ([],[])
    | h :: tail ->
       let (a, b) = (split tail (n - 1)) in
       ((h :: a), b)
;;
(*
  Extract a Slice From a List
  Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included).
  Start counting the elements with 0 (this is the way the List module numbers elements).

  # slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6;;
  - : string list = ["c"; "d"; "e"; "f"; "g"]
 *)
let slice xs i0 i1 =
  let rec slice0 l s0 s1 acc =
    if s1 < 0 then acc
    else if s0 > 0 then
      match l with
      | [] -> acc
      | h :: tail -> slice0 tail (s0 - 1) (s1 - 1) acc
    else
      match l with
      | [] -> acc
      | h :: tail -> slice0 tail (s0 - 1) (s1 - 1) (acc @ [h])
  in slice0 xs i0 i1 []
;;
(*
  Rotate a List N Places to the Left
  Rotate a list N places to the left.

  # rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;
  - : string list = ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"]
 *)
let rec rotate xs n =
  if n = 0 then xs
  else
    match xs with
    | [] -> []
    | h :: tail ->
       rotate (tail @ [h]) (n - 1)
;;
(*
  Remove the K'th Element From a List
  Remove the K'th element from a list.

  The first element of the list is numbered 0, the second 1,...

  # remove_at 1 ["a"; "b"; "c"; "d"];;
  - : string list = ["a"; "c"; "d"]
 *)
let rec remove_at n xs =
  match xs with
  | [] -> []
  | h :: tail ->
     if n = 0 then tail
     else h :: remove_at (n - 1) tail
;;
(*
  Insert an Element at a Given Position Into a List
  Start counting list elements with 0.
  If the position is larger or equal to the length of the list, insert the element at the end. (The behavior is unspecified if the position is negative.)

  # insert_at "alfa" 1 ["a"; "b"; "c"; "d"];;
  - : string list = ["a"; "alfa"; "b"; "c"; "d"]
 *)
let rec insert_at n x xs =
  match xs with
  | [] -> [x]
  | h :: tail ->
     if n = 0 then x :: xs
     else h :: (insert_at (n - 1) x tail)
;;
