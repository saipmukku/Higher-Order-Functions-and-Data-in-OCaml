open Funs

(*************************************)
(* Part 2: Three-Way Search Tree *)
(*************************************)

type int_tree =
  | IntLeaf
  | IntNode of int * int option * int_tree * int_tree * int_tree 

let empty_int_tree = IntLeaf

let rec int_insert x t = match t with
  | IntLeaf -> IntNode(x, None, IntLeaf, IntLeaf, IntLeaf)
  | IntNode(a, None, one, two, three) -> if x < a then IntNode(x, Some(a), one, two, three) 
    else if x > a then IntNode(a, Some(x), one, two, three) else t
  | IntNode(a, Some b, one, two, three) -> if x < a then 
    IntNode(a, Some(b), int_insert x one, two, three) else if b < x then 
    IntNode(a, Some(b), one, two, int_insert x three) else if x > a && x < b then
    IntNode(a, Some(b), one, int_insert x two, three) else t

let rec int_mem x t = match t with 
    | IntLeaf -> false
    | IntNode(a, None, one, two, three) when x = a -> true
    | IntNode(a, Some(b), one, two, three) when x = a || x = b -> true
    | IntNode(a, None, one, two, three) -> (int_mem x one || int_mem x two ||int_mem x three)
    | IntNode(a, Some(b), one, two, three) -> (int_mem x one || int_mem x two || int_mem x three)

let rec int_size t = match t with
  | IntLeaf -> 0
  | IntNode(a, None, one, two, three) -> 1
  | IntNode(a, Some b, one, two, three) -> 2 + (int_size one) + (int_size two) + (int_size three)

let rec int_max t = match t with
  | IntLeaf -> raise (Invalid_argument("int_max"))
  | IntNode(a, None, one, two, three) -> a
  | IntNode(a, Some b, one, two, three) -> if three = IntLeaf then b else int_max three

(*******************************)
(* Part 3: Three-Way Search Tree-Based Map *)
(*******************************)

type 'a tree_map =
  | MapLeaf
  | MapNode of (int * 'a) * (int * 'a) option * 'a tree_map * 'a tree_map * 'a tree_map

let empty_tree_map = MapLeaf

let rec map_put k v t = match t with
  | MapLeaf -> MapNode((k, v), None, MapLeaf, MapLeaf, MapLeaf)
  | MapNode((a, b), None, one, two, three) -> if k < a then MapNode((k, v), Some((a, b)), MapLeaf, MapLeaf, MapLeaf)
    else if k > a then MapNode((a, b), Some((k, v)), one, two, three) else raise (Invalid_argument "map_put")
  | MapNode((a, b), Some (c, d), one, two, three) -> if k < a then MapNode((a,b), Some((c, d)), map_put k v one, two, three)
    else if k > a && k < c then MapNode((a,b), Some((c, d)), one, map_put k v two, three) 
    else if k > c then MapNode((a,b), Some((c, d)), one, two, map_put k v three) else raise (Invalid_argument "map_put")

let rec map_contains k t = match t with
  | MapLeaf -> false
  | MapNode((a, b), None, one, two, three) -> if a = k then true else false
  | MapNode((a, b), Some (c, d), one, two, three) -> if a = k || c = k then true else if k < a then
    map_contains k one else if k > a && k < c then map_contains k two else map_contains k three

let rec map_get k t = match t with
  | MapLeaf -> raise(Invalid_argument("map_get"))
  | MapNode ((a, b), None, one, two, three) -> if k = a then b else raise (Invalid_argument("map_get"))
  | MapNode ((a, b), Some (c, d), one, two, three) -> if k = a then b else if k = c then d else if k < a 
    then map_get k one else if k > a && k < c then map_get k two else map_get k three

(***************************)
(* Part 4: Variable Lookup *)
(***************************)

(* Modify the next line to your intended type *)
type lookup_table = 
| Empty_table
| TableNode of (string * int) list * lookup_table

let empty_table : lookup_table = Empty_table

let push_scope (table : lookup_table) : lookup_table = TableNode ([], table)

let pop_scope (table : lookup_table) : lookup_table = match table with 
  | TableNode(_, table) -> table
  | Empty_table -> failwith "No scopes remain!"

let rec get_num name lst = match lst with 
  | [] -> 0
  | (a, b) :: t -> if a = name then b else get_num name t

let add_var name value (table : lookup_table) : lookup_table = match table with 
  | Empty_table -> failwith "There are no scopes to add a variable to!"
  | TableNode(a, b) -> if get_num name a = 0 then TableNode((name, value) :: a, b)
    else failwith "Duplicate variable binding in scope!"

let rec lookup name (table : lookup_table) = match table with 
  | Empty_table -> failwith "Variable not found!"
  | TableNode(a, b) -> if get_num name a = 0 then failwith "Variable not found!"
    else get_num name a