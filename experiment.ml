let is_empty (line : int option list) = 
  List.for_all (fun a -> match a with Some x -> x = -1 | None -> true) line

let is_compatible (answer : int option list) (line : int option list) =
  List.for_all2 
    (fun a b -> a = b || a = None || b = None ) line answer

let is_complete line =
  List.for_all (fun a -> a <> None) line

let sum a = 
  List.fold_left (fun x y -> x + y) 0 a

let rec append value times list =
  match times with
  | 0 -> list
  | _ -> append value (times-1) (value::list)


let rec build_line_helper clues gaps length line = 
  if ((List.length line) > length) then failwith "Length too short" else
    match gaps with
    | [] -> append (Some (-1)) (length - List.length line) line
    | h::t -> 
      let line2= line |> append (Some (-1)) h 
                 |> append (Some 1) (List.hd clues) in
      build_line_helper (List.tl clues) t length line2

let build_line clues gaps length = 
  List.rev (build_line_helper clues gaps length [])

let match_lines line1 line2= 
  List.map2 (fun a b -> if a = b then a else None) line1 line2

let rec drop_last_helper in_list out_list = 
  match in_list with
  | [] -> out_list
  | [h] -> out_list
  | h::t -> drop_last_helper t (h::out_list)

(**[drop_last in_list] is [in_list] without the last elem*)
let drop_last in_list = List.rev (drop_last_helper in_list [])

(**[get_last in_list] is the last elem a list*)
let rec get_last in_list = 
  match in_list with
  | [] -> failwith("Invalid last")
  | [h] -> h
  | h::t -> get_last t

(**[next_gap_helper clues in_gaps length out_gaps] is a helper method for 
   [next_gap]*)
let rec next_gap_helper clues in_gaps length (out_gaps : int list) = 
  if sum clues + sum in_gaps < length then 
    match in_gaps with 
    | [] -> failwith("Invalid gaps")
    | [h] -> (h+1)::out_gaps
    | h::t -> next_gap_helper clues t length (h::out_gaps)
  else 
    let new_lng = length - (get_last clues + 1) in
    1::(next_gap_helper (drop_last clues) (drop_last in_gaps) new_lng out_gaps)

(**[next_gap clues gaps length] determines the next hole in clues *)
let next_gap clues gaps length = List.rev (next_gap_helper clues gaps length [])

(**[fill_line_helper clues line gaps filled_line] is a helper method for [fill
   line]*)
let rec fill_line_helper clues line gaps filled_line = 
  let max_gap = (List.length line) - (sum clues + (List.length clues) - 1) in
  if (List.hd gaps = max_gap) then filled_line
  else 
    let length = List.length line in
    let new_gaps = next_gap clues gaps length in
    let next_line = build_line clues new_gaps length in
    if (is_compatible next_line line) then  
      let updated = match_lines next_line filled_line in
      fill_line_helper clues line new_gaps updated else
      fill_line_helper clues line new_gaps filled_line

(**[first_valid_line clues gaps line] first line that satisfyies the given 
   clue and line based on order of gaps*)
let rec first_valid_line clues gaps line= 
  let length = List.length line in
  let initial_line = build_line clues gaps length in
  if (is_compatible line initial_line) then (initial_line, gaps) else
    first_valid_line clues (next_gap clues gaps length) line

(**[fill_line clues line] fills up a given line with the correct *)
let fill_line clues line = 
  let initial_gaps = 0::(append 1 (List.length clues - 1) []) in
  if (is_complete line) then line 
  else
    let first_valid = first_valid_line clues initial_gaps line in
    fill_line_helper clues line (snd first_valid) (fst first_valid)

(**[give_hint clues line_array] provides a clue to the user*)
let give_hint clues line_array =
  let line = Array.to_list line_array in
  let filled_line = fill_line clues line in
  let rec find_dif fld_line index=
    match fld_line with
    | [] -> failwith ("Logically Complete Line")
    | h1::t1 -> begin 
        if (h1 <> line_array.(index)) then (index, h1) 
        else find_dif t1 (index + 1)
      end in
  find_dif filled_line 0



