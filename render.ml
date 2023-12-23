(**[is_single] determines whether or not to single space or double space the
   puzzle*)
let is_single = ref false 
(**[changing_spacing] modified the [is_single] flag*)
let change_spacing () =
  is_single:=not!is_single

(**[print_normal str] prints [str] to the console in white*)
let print_normal str =
  ANSITerminal.(print_string [white] str)

(**[print_cursor str] prints [str] to the console in blue*)
let print_cursor str =
  ANSITerminal.(print_string [blue] str)

(**[print_error str] prints [str] to the console in red*)
let print_error str =
  ANSITerminal.(print_string [red] str)

(**[max_size_top lst cur] determines the offset for the top of the grid*)
let rec max_size_top lst cur : int =
  match lst with
  | h::t -> if (List.length h > cur) then max_size_top t (List.length h) 
    else max_size_top t cur
  | [] -> cur

(**[max_size_side lst cur] determines the offset for the side of the grid*)
let rec max_size_side lst cur : int =

  let row_max lst = List.fold_left (fun a b -> if b < 10 then a+2 else a+3) 0 
      lst in     
  let rec get_largest lst cur =
    match lst with
    | h::t -> if (row_max h) > cur then get_largest t (row_max h) else 
        get_largest t cur
    | [] -> (cur-1) in 
  get_largest lst 0

(**[horz_print offset horz] transforms the top clues into a printable form *)
let horz_print offset horz =

  let rec pad offset lst =
    if List.length lst < offset then pad offset (0::lst) else lst in
  let rec pad_lst offset olst nlst =
    match olst with
    | h::t -> pad_lst offset t (nlst@[(pad offset h)])
    | [] -> nlst in
  let rec get_string (padded_lst:int list list) temp_lst str to_get offset 
      slst =
    match temp_lst with
    | h::t -> if (to_get <= (offset-1)) then get_string padded_lst t 
          (if (not !is_single) then
             (String.concat " " [str; if (List.nth h to_get) = 0 then "  " 
                                 else if (List.nth h to_get) < 10 then 
                                   " "^(Int.to_string (List.nth h to_get)) 
                                 else Int.to_string (List.nth h to_get)]) else
             (String.concat "" [str; if (List.nth h to_get) = 0 then " " 
                                else if (List.nth h to_get) < 10 then 
                                  " "^(Int.to_string (List.nth h to_get)) 
                                else Int.to_string (List.nth h to_get)]))
          to_get offset slst else (List.rev slst)
    | [] -> if (to_get < offset) then 
        get_string padded_lst padded_lst ""
          (to_get+1) offset (str::slst) else (List.rev slst) in
  let padded = (pad_lst offset horz []) in
  get_string padded padded "" 0 offset []

(**[vert_print offset horz] transforms the side clues into a printable form *)
let vert_print offset vert =

  let rec pad oset lst =
    let rec list_to_string_size lst str =
      match lst with 
      | 0::t -> list_to_string_size t (str^" ")
      | h::t -> list_to_string_size t (str^(Int.to_string h)^" ")
      | [] -> String.length str 
    in
    if list_to_string_size lst "" < oset then pad oset (0::lst) else lst in
  let rec convert_hlp lst str =
    match lst with 
    | h::t -> convert_hlp t (String.concat " " [str; if (h = 0) then 
                                                  "" else Int.to_string h])
    | [] -> str in
  let convert lst = 
    if (List.exists (fun a -> a = 0) lst) then convert_hlp lst " " 
    else convert_hlp lst "" in
  let rec get_string olst nlst =
    match olst with
    | h::t -> get_string t ((convert (pad offset h))::nlst)
    | [] -> List.rev nlst in
  get_string vert []

(**[grid_print_row grid str_clues str_offset] prints the [grid] and side clues 
   to console *)
let grid_print_row grid str_clues str_offset =

  let print_top_double opt =
    match opt with
    | Some a -> begin match a with 
        | -1 -> ignore (print_normal "\\/ ")
        | 1 ->  ignore (print_normal "▄▄ ")
        | 2 -> ignore (print_cursor "██ ")
        | 3 -> ignore (print_cursor "\\/ ")
        | 4 -> ignore (print_cursor "▄▄ ")
        | _ -> () end
    | None -> ignore (print_normal"   ") 
  in

  let print_bot_double opt =
    match opt with
    | Some a -> begin match a with 
        | -1 -> ignore (print_normal "/\\ ")
        | 1 ->  ignore (print_normal "▀▀ ")
        | 2 -> ignore (print_cursor "██ ")
        | 3 -> ignore (print_cursor "/\\ ")
        | 4 -> ignore (print_cursor "▀▀ ")
        | _ -> () end 
    | None -> ignore (print_normal "   ") 
  in

  let print_single opt =
    match opt with
    | Some a -> begin match a with 
        | -1 -> ignore (print_normal "X")
        | 1 ->  ignore (print_normal "■")
        | 2 -> ignore (print_cursor "█")
        | 3 -> ignore (print_cursor "X")
        | 4 -> ignore (print_cursor "■")
        | _ -> () end 
    | None -> ignore (print_normal "□") 
  in

  let rec print_row_single lst = 
    match lst with
    | h::t -> print_single h; print_row_single t
    | [] -> print_normal "\n"
  in

  let rec print_row_top lst =
    match lst with
    | h::t -> print_top_double h; print_row_top t
    | [] -> print_normal "\n"
  in

  let rec print_row_bot lst =
    match lst with
    | h::t -> print_bot_double h; print_row_bot t
    | [] -> print_normal "\n"
  in

  let print lst offset clues : unit =
    if (!is_single) then 
      (print_normal (clues^" "); (print_row_single lst))
    else (print_normal offset);
    (print_row_top lst); print_normal (clues^" "); (print_row_bot lst) in

  print grid str_clues str_offset

(**[render vert horz offset state] draws the currently selected puzzle *)
let rec render vert horz offset state: unit =

  let grid_string_lst =
    let height grid = (Array.length grid.(0)) in
    let transpose grid = List.init (height grid) 
        (fun i -> grid |> Array.map (fun arr -> arr.(i)) |> Array.to_list) in
    transpose state
  in
  let rec make_string hor vrt grd hor_off  =
    match hor with
    | h::t -> print_normal ((String.concat "" [hor_off;h])^("\n"));
      make_string t vrt grd hor_off
    | [] -> begin match vrt with
        | a::b -> begin match grd with
            | c::d ->  grid_print_row c (a^" ") hor_off; 
              make_string hor b d hor_off
            | [] -> () end
        | [] -> () end
  in
  (make_string horz vert grid_string_lst offset)

(**[init_drawing puzzle] initalizes the printable form of the puzzle*)
let init_drawing puzzle =

  let horz = Puzzle.get_vclues puzzle |> Array.to_list 
             |> List.map (fun a -> Array.to_list a) in
  let vert = Puzzle.get_hclues puzzle |> Array.to_list 
             |> List.map (fun a -> Array.to_list a) in
  let state = State.init_state puzzle in
  let offset = (max_size_top horz 0),(max_size_side vert 0) in
  let horz_string_lst =
    match offset with
    | (a,b) -> horz_print a horz in
  let vert_string_lst =
    match offset with
    | (a,b) -> vert_print b vert in
  let rec horizontal_offset ofs str =
    match ofs with
    | (a,b) ->
      if b > 0 then horizontal_offset (a,b-1) (str^" ") else (str^" ") 
  in
  (vert_string_lst, horz_string_lst, (horizontal_offset offset ""), state)
