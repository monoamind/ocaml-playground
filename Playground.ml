(* ============================================================================================= *)

print_int 100;; (* int -> unit *)
print_string "Sample string";; (* a la printf() *)
print_endline "Sample string";; (* a la puts() *)
print_newline ();; (* unit -> unit *)

let print_dict_entry (k, v) =
  print_int k ; print_newline () ; print_string v ; print_newline ()
;;

(* ============================================================================================= *)

let isvowel c = 
  c = 'a' || c = 'i' || c = 'e' || c = 'o' || c = 'u'

let isconsonant c = not (isvowel c)

(* ============================================================================================= *)

let rec factorial a = 
  if a = 1 then 1 else a * factorial (a - 1)
;;

let rec gcd a b = 
  if b = 0 then a else gcd b (a mod b)
;;

(* ============================================================================================= *)

let rec factorial a =
  match a with
    1 -> 1
    | _ -> a * factorial (a - 1)

let isvowel c = 
  match c with
    'a' | 'e' | 'i' | 'o' | 'u' -> true
    | _ -> false

let rec pow x n = 
  match n with
    0 -> 1
    | 1 -> x
    | _ -> x * pow x (n - 1)

let isupper c =
  match c with
    'A'..'Z' -> true
    | _ -> false

(* ============================================================================================= *)

let rec length l =
  match l with
    [] -> 0
    | hd::tl -> 1 + length tl
    (* _::tl -> 1 + length tl *)

let rec destutter l =
  match l with
    [] -> []
    | [hd] -> [hd]
    | x::y::rest ->
      if x = y then destutter (y::rest)
      else x::destutter (y::rest)

let rec only_even l =
  match l with
    [] -> []
    | hd::tl -> 
      if hd mod 2 = 0 then hd::(only_even tl)
      else only_even tl

(* ============================================================================================= *)

let rec length_inner l n =
  match l with
    [] -> n
    | _::tl -> length_inner tl (n + 1)
;;

let listlen l = length_inner l 0
;;

(* a' list -> a' list *)
let rec odd_elements l =
  match l with
    [] -> []
    | [hd] -> [hd]
    | hd::_::tl -> hd::odd_elements(tl)
;;

let rec odd_elements2 l =
  match l with
    hd::_::tl -> hd::odd_elements(tl)
    | _ -> l
;;

(* ============================================================================================= *)

let rec list_append lhs rhs =
  match lhs with
    [] -> rhs
    | hd::tl -> hd::list_append tl rhs
;;

let rec list_reverse l =
  match l with
    [] -> []
    | hd::tl -> (list_reverse tl) @ [hd]
;;

let rec take l n =
  if n = 0 then []
  else match l with
    [] -> take [] 0
    | hd::tl -> hd::take tl (n - 1)
;;

let rec drop l n =
  if n = 0 then l
  else match l with
    [] -> drop [] 0
    | hd::tl -> drop tl (n - 1)
;;

let x : (int list) list = [[1]; [2; 3]; [4; 5; 6]];;

(* INSERTION SORT ============================================================================== *)

let rec insert x cmp l =
  match l with
    [] -> [x]
    | hd::tl -> 
      if cmp x hd
        then x::hd::tl
        else hd::(insert x cmp tl)
;;

let rec insert_sort cmp l =
  match l with
    [] -> []
    | hd::tl -> insert hd cmp (insert_sort cmp tl)
;;

let rec insert_sort_in_one_func l =
  let rec insert x l =
    match l with
      [] -> [x]
      | hd::tl -> 
        if x <= hd
          then x::hd::tl
          else hd::insert x tl
  in match l with
    [] -> []
    | hd::tl -> insert hd (sort tl);;
;;  

let rec sort l = insert_sort (fun x y -> x < y) l ;;

let my_intlist = [3;9;7;1;2;4];;
sort my_intlist;;
insert_sort ( > ) my_intlist;;

(* MERGE SORT ================================================================================== *)

let rec merge x y =
  match x, y with
    [], l -> l
    | l, [] -> l
    | hx::tx, hy::ty ->
      if hx < hy then hx::merge tx (hy::ty)
      else hy::merge (hx::tx) ty
;;

let rec merge_sort l =
  match l with
    [] -> []
    | [x] -> [x]
    | _ ->
      let half = (length l) / 2 in
        let left = take l half in
          let right = drop l half in
            merge (merge_sort left) (merge_sort right)
;;

let rec is_sorted l =
  match l with
    [] -> true
    | [x] -> true
    | x::y::tl ->
      if x < y 
        then is_sorted (y::tl)
        else false
;;

(* FILTER ====================================================================================== *)

let rec filter func l =
  match l with
    [] -> []
    | hd::tl ->
      if func hd then hd::filter func tl
      else filter func tl
;;

let even x = x mod 2 = 0;;
let odd x = not (even x);;
let pos x = x > 0;;
let lst = [1; 2; -2; 4; 3; -3; -10; 8];;

filter even lst;;
filter pos (filter odd lst);;
filter (fun x -> x < 0) lst;;

(* MAP ========================================================================================= *)

let rec map func l =
  match l with
    [] -> []
    | hd::tl -> (func hd)::map func tl
;;

let strlist = ["OCaml";"C";"C++";"ASM";"LISP"];;
map String.length strlist;;

(* REDUCE ====================================================================================== *)

let rec reduce l func s =
  match l with
    [] -> s
    | hd::tl -> reduce tl func (func s hd)
;;

let add x y = x + y;;
let mul x y = x * y;;

reduce [1;2;3;4;5] add 0;;
reduce [1;2;3;4;5] mul 1;;

let upper_only chlist = reduce (map isupper chlist) (fun a b -> a && b) true ;;
upper_only ['O';'C';'a';'m';'l'] ;;
upper_only ['A';'S';'M'] ;;

(* ============================================================================================= *)

let rec apply func n def =
  match n with
    0 -> def
    | _ -> func (apply func (n - 1) def)
;;

apply (fun x -> x + 1) 0 5;;
apply (fun x -> x + 1) 2 5;;

(* ============================================================================================= *)

let rec factorial n =
  if n <= 0 then raise (Invalid_argument "factorial") else
    if n = 1 then 1 else n * factorial (n - 1)
;;

factorial 5;;

let rec factorial n =
  if n <= 0 then raise (Invalid_argument "factorial") 
  else match n with
    1 -> 1
    | _ -> n * factorial (n - 1)
;;

factorial 10;;

try factorial 0 with Invalid_argument opt -> 0;;

let rec find x l =
  match l with
    [] -> raise Not_found
    | hd::tl -> 
      if hd = x 
        then hd::tl
        else find x tl
;;
     
let rec find_or x l def =
  try find x l with
    Not_found -> def
;;

find 5 [1;3;5;6;0;1];;
find_or 2 [1;3;5;6;0;1] [];;

(* ============================================================================================= *)

let fst p = match p with (x, _) -> x;;
let snd p = match p with (_, y) -> y;;

let first (x, _) = x;;
let second (_, y) = y;;

let rec lookup l x =
  match l with
    [] -> raise Not_found
    | (k, v)::tl -> 
      if k = x then v else lookup tl x
;;

let rec add k v d =
  match d with
    [] -> [(k, v)]
    | (lk, lv)::tl -> 
      if k = lk 
        then (k, v)::tl
        else (lk, lv)::(add k v tl)
;;

let rec remove k d =
  match d with
    [] -> []
    | (hk, hv)::tl ->
      if k = hk
        then tl
        else (hk, hv)::(remove k tl)
;;

let rec key_exists k d =
  match d with
    [] -> false
    | (hk, hv)::tl ->
      if k = hk then true else key_exists k tl
;;

let lookuplist = [(1, 4); (2, 2); (3, 2); (4, 3); (5, 1); (6, 2)];;

(* ============================================================================================= *)

let add x y = x + y;;
let add_5 x = add x 5;; (* currying ? *)
add_5 10 = 15

let add5 = add 5;; (* partial application *)
add5 10 = 15;;

map (add 5) [0; 3; 8];;
map (( * ) 2) [10; 12; 9]

let rec mapl func l =
  match l with
    [] -> []
    | hd::tl -> (map func hd)::(mapl func tl)

let mapl_1 f l = map (map f) l
let mapl_2 f = map (map f)

let plus = fun x -> fun y -> x + y

let rec even x = if x = 0 then true else odd (x - 1)
    and odd x = if x = 0 then false else even (x - 1)

(* ============================================================================================= *)

type colour = 
  Red
  | Green
  | Blue 
  | Yellow
  | RGB of int * int * int

let cols = [Red; Red; Green; Yellow; RGB (150, 0, 255)]

type 'a option = None | Some of 'a

let nothing = None
let number = Some 50
let numbers = [Some 2; None; nothing; number]
let word = Some ['c'; 'a'; 'k'; 'e']

let rec lookup_opt x d =
  match d with
    [] -> None
    | (hk, hv)::tl ->
      if hk = x 
        then Some (hk, hv)
        else lookup_opt x tl
;;

type 'a sequence = Nil | Cons of 'a * 'a sequence;;

let myseq = Cons (1, Cons (2, Cons (5, Nil)));;

let rec seqlength s =
  match s with
    Nil -> 0
    | Cons (_, tl) -> 1 + seqlength tl
;;

let rec seqappend s x =
  match s with
    Nil -> Cons (x, Nil)
    | Cons (hd, tl) -> Cons(hd, seqappend tl x)

type expr =
  Num of int
  | Add of expr * expr
  | Subtract of expr * expr
  | Multiply of expr * expr
  | Divide of expr * expr

let rec evaluate e =
  match e with
    Num x -> x
    | Add (lhs, rhs) -> evaluate lhs + evaluate rhs
    | Subtract (lhs, rhs) -> evaluate lhs - evaluate rhs
    | Multiply (lhs, rhs) -> evaluate lhs * evaluate rhs
    | Divide (lhs, rhs) -> evaluate lhs / evaluate rhs
;;

(* ============================================================================================= *)

type 'a tree =  Branch of 'a * 'a tree * 'a tree | Leaf
;;

let mytree = Branch (1, Branch (2, Branch (5, Leaf, Leaf), Leaf), Branch(3, Leaf, Leaf));;

(* 
                  1
                /   \
              2       3
            /   \   /   \
           5 
         /   \
 *)

let rec size tr =
  match tr with
    Leaf -> 0
    | Branch (_, lhs, rhs) -> 1 + size lhs + size rhs
;;

let rec total tr =
  match tr with
    Leaf -> 0
    | Branch (num, lhs, rhs) -> num + total lhs + total rhs
;;

let max x y = if x > y then x else y
;;

let rec maxdepth tr =
  match tr with
    Leaf -> 0
    | Branch (_, l, r) -> 1 + max (maxdepth l) (maxdepth r)
;;

let maxdepth2 tr =
  match tr with
    Leaf -> 0
    | Branch (_, l, r) -> 1 + max (size l) (size r)
;;

let rec list_of_tree tr =
  match tr with
    Leaf -> []
    | Branch (x, l, r) -> list_of_tree l @ [x] @ (list_of_tree r)
;;

let rec tree_map f tr =
  match tr with
    Leaf -> Leaf
    | Branch (x, l, r) -> Branch (f x, tree_map f l, tree_map f r)
;;

tree_map (fun x -> Int.of_float (Float.of_int x ** 2.)) mytree
;;

let rec tree_lookup tr k =
  match tr with
    Leaf -> None
    | Branch ((bk, bv), l, r) ->
      if k = bk then Some bv
      else if k < bk then tree_lookup l k 
      else tree_lookup r k
;;

(* 
             (3, "three")
             /          \
        (1, "one")   (4, "four")
        /        \   /         \
    (2, "two")
    /        \
 *)


let mydict =
  Branch ((3, "three"),
    Branch ((1, "one"), 
      Leaf, 
      Branch ((2, "two"), 
        Leaf, 
        Leaf)),
    Branch ((5, "five"), 
      Leaf, 
      Leaf))
;;

let rec tree_insert tr k v =
  match tr with
    Leaf -> Branch ((k, v), Leaf, Leaf)
    | Branch ((bk, bv), l, r) ->
      if k = bk then Branch ((k, v), l, r)
      else if k < bk then Branch ((bk, bv), tree_insert l k v, r)
      else Branch ((bk, bv), l, tree_insert r k v)
;;

tree_insert (tree_insert (tree_insert mydict 6 "six") 4 "four") 0 "zero";;

(* ============================================================================================= *)

