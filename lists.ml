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
(* TODO *)

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
(* TODO *)

(*
    Decode a Run-Length Encoded List
    Given a run-length code list generated as specified in the previous problem, construct its uncompressed version.

    #  decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")];;
    - : string list =
    ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
*)
(* TODO *)

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
(* TODO *)