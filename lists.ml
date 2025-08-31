(* Tail of a List *)
let rec last = function
    | [] -> None
    | [x] -> Some x
    | x :: rest -> last rest
;;

(* Last Two Elements of a List *)
let rec last_two = function
    | [] -> None
    | [x] -> None
    | [x; y] -> Some (x, y)
    | x :: rest -> last_two rest
;;

(* N'th element of a List *)
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

(* Length of a List *)
let rec length = function
    | [] -> 0
    | x :: rest -> 1 + (length rest)
;;

(* Reverse a List *)
let rec rev = function
    | [] -> []
    | x :: rest -> (rev rest) @ [x]
;;

(* Palindrome *)
let rec is_palindrome (xs: 'a list) =
    xs = (List.rev xs)
;;

(* Flatten a List *)
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
