open Funs

(********************************)
(* Part 1: High Order Functions *)
(********************************)

let contains_elem lst e = fold (fun a b -> a || b = e) false lst

let is_present lst x = match lst with
    | [] -> []
    | _ -> map (fun num -> if num = x then 1 else 0) lst;;

let count_occ lst target = match lst with
    | [] -> 0
    | _ -> fold_right (fun num acc -> if num = target then acc + 1 else acc) lst 0;;

let uniq lst = match lst with
    | [] -> []
    | h :: [] -> lst
    | h :: t -> fold (fun the_list x -> if ((contains_elem the_list x) == true) then the_list else x :: the_list) [] lst;;

let assoc_list lst = let dupeless = uniq lst in map (fun num -> (num, count_occ lst num)) dupeless;;

let ap fns args = fold (fun a x -> a @ (map (fun z -> (x z)) args)) [] fns;;