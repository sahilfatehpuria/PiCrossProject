
(** [print_red str] prints [str] to console in red*)
let print_red str =
  ANSITerminal.(print_string [red] str)
(** [print_red str] prints [str] to console in green*)
let print_green str =
  ANSITerminal.(print_string [green] str)

(** [filename] is the filename that needs to be loaded*)
let filename = ref ""

(** [read] takes input from the user*)
let read () = read_line () |> String.trim

(**[get_puzzle a] converts the file [a] from json into the puzzle*)
let get_puzzle a = a |> Yojson.Basic.from_file |> Puzzle.puzzle_from_json

(**[jsonify a] takes a filename [a] and appends the .json suffix if needed*)
let jsonify a = 
  if String.length a > 3 then 
    if (String.sub a ((String.length a) - 4) (String.length a) = ".json") 
    then a else a^".json" else a^".json"

(**[take_input pzl sol state] takes user input and gives hints if needed*)
let rec take_input pzl sol state =
  let rec get_row () =
    print_red "\nEnter a row"; print_string "\n> ";
    try read () |> int_of_string with
    | _ -> print_red "\nUnsuppoted input,try again"; get_row ()
  in
  let rec get_column () =
    print_red "\nEnter a column"; print_string "\n> ";
    try read ()|> int_of_string with
    | _ -> print_red "\nUnsuppoted input,try again"; get_row ()
  in
  let rec coords () =
    (get_row ()), (get_column ())
  in

  print_red "\nEnter either 'fill' 'empty', 'mark', 'quit', 
  'hint', 'check' or 'save'";
  print_string "\n> ";
  match read () with
  | "fill" -> begin match coords () with
      | (a,b) -> State.fill_square a b state; state
    end
  | "empty" -> begin match coords () with
      | (a,b) -> State.empty_square a b state; state
    end
  | "mark" -> begin match coords () with 
      | (a,b) -> State.cross_square a b state; state end
  | "check" ->  
    begin match !sol with 
      | None ->  begin print_red "Solution not loaded yet";
          state end
      | Some el -> begin
          match coords () with 
          | (a,b) -> State.edit_square (State.get_square a b el) a b state;
            state end
    end
  | "save" -> print_red "\nEnter the file name you wish to save to"; 
    print_string "\n>"; Puzzle.save state pzl (read () |> jsonify)
  | "hint" -> State.give_hint state pzl; state
  | "quit" -> exit 0
  | _ -> begin print_red "\nNot supported input, try again"; 
      take_input pzl sol state 
    end

(** [game_loop plz v_clues_printable h_clues_printable offset grid] runs the
    main game loop*)
let rec game_loop pzl sol v_clues_printable h_clues_printable offset grid  =
  ANSITerminal.erase Screen;
  Render.render v_clues_printable h_clues_printable offset grid; 
  game_loop pzl sol v_clues_printable h_clues_printable offset 
    (take_input pzl sol grid)

(**[init_game_loop plz sol params] starts the game loop for the user*)
let init_game_loop pzl sol params =
  match params with
  | (a,b,c,d) -> game_loop pzl sol a b c d

(**[load_puzzle] loads the specified puzzle for the user*)
let rec load_puzzle () =
  print_red "Specify the file that you wish to load";
  print_string "\n> ";
  match (read () |> jsonify) with
  | exception e -> begin print_red "File not found, try again\n"; 
      load_puzzle () end
  | file_name -> filename:= (file_name); 
    file_name |> get_puzzle 

(**[load] is a function that creates solutions when loading puzzles *)
let load () = 
  let pzl = load_puzzle () in
  let sol_file_name = ("sol_"^(!filename)) in
  let sol = ref None in
  let solve () = sol := Some (if (Sys.file_exists sol_file_name) then 
                                sol_file_name |> get_puzzle |> State.init_state
                              else Puzzle.write_sol pzl sol_file_name) in
  ignore (Thread.create solve ());
  pzl |> Render.init_drawing |> init_game_loop pzl sol


(**Transposes a list of lists *)
let rec transpose list = match list with
  | []             -> []
  | []   :: xss    -> transpose xss
  | (x::xs) :: xss ->
    (x :: List.map List.hd xss) :: transpose (xs :: List.map List.tl xss)

(**Transposes a list of lists *)
let rec divide lst temp sub rslt =
  match lst with 
  |[]   -> temp::rslt
  |h::t -> if ((List.length temp) = sub)then (divide t ([h]) sub (temp::rslt))
    else divide t (h::temp) sub rslt

(**Transposes a list of lists *)
let sub lst n = divide (List.rev lst) [] ((List.length lst)/n) []
                |> List.map (List.filter (fun x -> x != 0 ))

(**Transposes a list of lists *)
let sub2 lst n = (divide (List.rev lst) [] (n) [])
                 |>transpose 
                 |> List.map (List.filter (fun x -> x != 0))

(**Turns List to String*)
let listize x= List.map int_of_string (Str.split (Str.regexp "[^0-9]+") x)

let stringify ls =
  "["^String.concat ", " ( List.map Int.to_string ls)^"]"

let big_stringify ls =
  "["^String.concat ", " (List.map stringify ls)^"]"

let prep_grid lst = 
  let col = int_of_string (List.nth lst 1) in
  let row = int_of_string (List.nth lst 2) in
  Batteries.List.make col (Batteries.List.make row (0)) 

let total_string lst =
  "{ \"grid\" :"^ (lst
                   |>prep_grid
                   |>big_stringify) ^",
     \"vclues\" :"^ (sub2 (listize (List.nth  lst 4)) 
                       (int_of_string ( List.nth lst 2))
                     |>big_stringify)^",
     \"hclues\" :"^ (sub (listize (List.nth  lst 3)) 
                       (int_of_string (List.nth  lst 1))
                     |>big_stringify)^"}"

let data = Csv.load "puzzle_data";;

let get_puzzle_string a = a 
                          |> Yojson.Basic.from_string   
                          |> Puzzle.puzzle_from_json
let load_csv () name str = 
  let pzl = get_puzzle_string str in
  let sol_file_name = (("sol_"^(name)) |> jsonify) in
  let sol = ref None in
  let solve () = sol := Some (if (Sys.file_exists sol_file_name) then 
                                sol_file_name |> get_puzzle |> State.init_state
                              else Puzzle.write_sol pzl sol_file_name) in
  ignore (Thread.create solve ());
  pzl |> Render.init_drawing |> init_game_loop pzl sol

let unify a = ()
let rec load_temp () str =
  str 
  |> get_puzzle_string
  |> Render.init_drawing
  |> unify


let contains s2 s1 =
  let re = Str.regexp_string s2
  in
  try ignore (Str.search_forward re s1 0); Some s1
  with Not_found -> None

let rec special_load bol () = 
  if bol then print_green "Welcome to Our Pictogram Collection!\n";
  print_green "Type the number of the option you wish to choose:
1. View All Puzzle
2. Search Puzzles
3. Select Puzzle
4. Return to Main Menue
>>";
  match read () |> int_of_string  with
  | 1 ->  (data
           |>List.map (List.hd)
           |>List.map print_endline
           |>List.hd;
           special_load false ())
  | 2 -> (print_green "Please Enter a keyword to search puzzles for\n>>";
          try 
            let query = read () in
            data
            |>List.map (List.hd)
            |>List.filter_map (contains query)
            |>List.map print_endline
            |>List.hd;
            special_load false ()
          with Failure _ -> (print_red "There are no puzzles with that keyword,
           Please take a look at our base again\n");
            special_load false ())
  | 3 -> (print_green "Please Enter The Name of the Puzzle \n>>";
          let query = read () in
          data
          |>List.filter_map (fun x ->
              match contains query (List.hd x) with
              |Some a-> Some x
              |None -> None)
          |>List.hd
          |>total_string
          |>load_csv () query)
  | 4 -> ( print_red ("Welcome to the Picross!"^
                      "\nType the number of the option you wish to select."^
                      "\n1. Create Puzzle\n2. Load Your Puzzle\n3. Choose A puzzle from our Collection \n4. Quit");
           let rec user_input () = 
             print_string  "\n> ";
             match read () |> int_of_string with
             | 1 -> load ()
             | 2 -> special_load true ()
             | 3 -> ()
             | _ -> begin print_red "Invalid input, try again"; user_input () 
               end in 
           user_input ())
  | _ -> begin print_red "Invalid input, try again\n"; special_load false () end

(**[init] starts the main game loop*)
let main () = 
  print_red ("Welcome to the Picros!"^
             "\nType the number of the option you wish to select."^
             "\n1. Load Your Puzzle\n2. Quit");
  let rec user_input () = 
    print_string  "\n> ";
    match read () |> int_of_string with
    | 1 -> load ()
    | 2 -> ()
    | _ -> begin print_red "Invalid input, try again"; user_input () end in 
  user_input ()

let () = main () 
