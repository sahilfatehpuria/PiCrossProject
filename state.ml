
type t = (int option) array array

let init_state pzl = Puzzle.get_grid pzl

let get_square row col grid =
  grid.(col).(row)

let edit_square quantity row col grid= 
  grid.(col).(row) <- quantity

let fill_square = edit_square (Some 1)

let cross_square = edit_square (Some (-1))

let empty_square = edit_square (None : int option)

let give_hint grid pzl =
  let rec hint_helper index =
    let len = Array.length grid in
    if (index < len) then
      let clues = (Puzzle.get_vclues pzl).(index) |> Array.to_list in
      try 
        let x = Experiment.give_hint clues grid.(index) in
        edit_square (snd x) (fst x) index grid
      with
      | _ -> hint_helper (index + 1)
    else if (index < (len + Array.length (grid.(0)))) then
      let clues = (Puzzle.get_hclues pzl).(index-len) |> Array.to_list in
      try 
        let x = Experiment.give_hint clues (Puzzle.get_row grid (index - len)) 
        in
        edit_square (snd x) (index - len) (fst x) grid
      with
      | _ -> hint_helper (index + 1)
    else failwith ("No Hints Possible")
  in
  hint_helper 0




