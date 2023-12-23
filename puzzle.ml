
open Yojson.Basic.Util
open Printf


(** Type to represent a puzzle. 
      [grid] is list all columns
      [top_clues] is list of all vertical clues**)
type puzzle = {
  grid : (int option array) array;
  top_clues: (int array) array;
  side_clues: (int array) array;
}
let optionify ar =
  ar |> Array.map (fun x -> match x with
      | 0 -> None 
      | h -> Some h )
let un_optionify ar =
  ar |> Array.map (fun x -> match x with
      | None -> 0 
      | Some h -> h )
let op ls =
  Array.map Array.of_list (Array.of_list ls)

let inverse_op ar = 
  List.map (Array.to_list) (Array.to_list ar)


let stringify ls =
  "["^String.concat ", " ( List.map Int.to_string ls)^"]";;
let big_stringify ls =
  "["^String.concat ", " (List.map stringify ls)^"]";;
let total_string puzzle =
  "{ \"grid\" :"^ (puzzle.grid|> Array.map un_optionify
                   |>inverse_op|>big_stringify) ^",
     \"vclues\" :"^ (puzzle.top_clues|>inverse_op|>big_stringify)^",
     \"hclues\" :"^ (puzzle.side_clues|>inverse_op|>big_stringify)^"}"


let puzzle_from_json j = {
  grid  = j |> member "grid" |> to_list |> filter_list 
          |> List.map filter_int |> op |> Array.map optionify;
  top_clues = j |> member "vclues" |> to_list |> filter_list 
              |> List.map filter_int |> op;
  side_clues  = j |> member "hclues" |> to_list |> filter_list 
                |> List.map filter_int|> op;
}

let get_grid pzl =
  pzl.grid

let get_vclues pzl = 
  pzl.top_clues

let get_hclues pzl =
  pzl.side_clues

(** [read str] is the puzzle corrosponding to file in given string*)
let read str =
  let open Yojson in
  puzzle_from_json (Yojson.Basic.from_file str) 

let write puzzle file_name =
  let oc = open_out file_name in (* create or truncate file, return channel *)
  Printf.fprintf oc "%s" (total_string puzzle) ;(* write something *)   
  close_out oc

let width grid = Array.length grid

let height grid = Array.length (grid.(0))

let get_row grid index = 
  grid |> Array.to_list |> List.map (fun arr -> arr.(index)) |> Array.of_list

let solve_line grid is_col index clues_arr  =
  let line = (if (is_col) then grid.(index) else get_row grid index)
             |> Array.to_list in
  let clues = clues_arr |> Array.to_list in 

  let output =  line |> Experiment.fill_line clues in

  if List.for_all2 (fun a b -> a = b) output line then begin
    true end
  else if (is_col) then begin
    List.iteri (fun i a -> Array.set grid.(index) i a) output;
    false
  end
  else 
    begin 
      for i = 0 to (width grid) - 1 do
        grid.(i).(index) <- (List.nth output i)
      done;
      false
    end

let duplicate grid = 
  Array.init (width grid) (fun n -> Array.copy grid.(n))

let is_complete grid = 
  Array.for_all (fun a -> Array.for_all (fun b -> b <> None) a) grid 

let grid_print grid =
  let width = width grid in
  let height = height grid in
  let convert_top opt =
    match opt with
    | Some a -> if (a=(-1)) then "\\/" else "▄▄"
    | None -> "  " in
  let convert_bot opt =
    match opt with
    | Some a -> if (a=(-1)) then "/\\" else "▀▀"
    | None -> "  " in
  let rec get_string_row grid row column str_top str_bot lst width height = 
    if (column < width && row != height) then get_string_row grid row (column+1) 
        (String.concat " " 
           [str_top; (convert_top (grid.(column).(row)))]) 
        (String.concat " " 
           [str_bot; (convert_bot (grid.(column).(row)))]) 
        lst width height
    else if (row < height) then get_string_row grid (row+1) 0 "" ""
        (str_bot::str_top::lst) width height
    else List.rev lst in
  get_string_row grid 0 0 "" "" [] width height

let solve pzl = 

  let grid = duplicate pzl.grid in

  let bln = ref true in

  let passthrough () = 
    bln:= true;
    for x = 0 to ((width grid) - 1) do
      bln := (solve_line grid true x pzl.top_clues.(x))&&(!bln);
    done;
    for x = 0 to ((height grid) - 1) do
      bln := (solve_line grid false x pzl.side_clues.(x))&&(!bln);
    done;
  in

  passthrough ();

  while not !bln do
    passthrough ();
  done;

  if (is_complete grid) then grid else failwith ("Unsolveable")

let write_sol puzzle file_name =
  let grid = solve puzzle in
  let sol_pzl = {grid = grid; top_clues = [||]; side_clues = [||]} in
  write sol_pzl file_name;
  grid

let save grid puzzle file_name = 
  let sol_pzl = {grid = grid; top_clues = puzzle.top_clues;
                 side_clues = puzzle.side_clues} in
  write sol_pzl file_name;
  grid
