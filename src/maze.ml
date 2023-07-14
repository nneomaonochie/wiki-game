open! Core

module Coord = struct
  module T = struct
    (* row * column *)
    type t = int * int [@@deriving compare, sexp, hash]

    let get_r (r, _) = r
    let get_c (_, c) = c
    let add (r1, c1) (r2, c2) = r1 + r2, c1 + c2
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)
end

(* east, south, west, north *)
let directions = [ 0, 1; 1, 0; 0, -1; -1, 0 ]

(* locates where the starting index is *)
let find_start_cell (maze_matrix : char array array) =
  let start_set = Coord.Hash_set.create () in
  (* traverses 2d array to find the start index and adds the coordinates into
     the Hash_set *)
  Array.iteri maze_matrix ~f:(fun r_index ch_array ->
    Array.iteri ch_array ~f:(fun c_index elem ->
      if Char.equal elem 'S' then Hash_set.add start_set (r_index, c_index)));
  List.hd_exn (Hash_set.to_list start_set)
;;

(* recursively traverses the maze to find the correct solution *)
let rec traverse_maze
  (maze_matrix : char array array)
  current_cell
  has_visited
  current_path
  =
  (* returns true if a cell is in bounds *)
  let in_bounds new_cell : bool =
    Coord.get_r new_cell >= 0
    && Coord.get_r new_cell < Array.length maze_matrix
    && Coord.get_c new_cell >= 0
    && Coord.get_c new_cell < Array.length (Array.get maze_matrix 0)
  in
  (* returns true if the cell we are in is not a wall *)
  let open_path new_cell =
    not
      (Char.equal
         maze_matrix.(Coord.get_r new_cell).(Coord.get_c new_cell)
         '#')
  in
  (* we add the current cell to the current_path stack *)
  Stack.push current_path current_cell;
  (* check if we hit the exit - base case and we win! *)
  (* let ref x =[] !x (this makes this a reference list) x := [5; 5;5] *)
  let found_exit =
    Char.equal
      maze_matrix.(Coord.get_r current_cell).(Coord.get_c current_cell)
      'E'
  in
  if found_exit
  then
    ()
    (* current path is mutable so we will already have the solve_command
       function *)
  else (
    (* if we eventually hit base case it will return a current_ath*)

    (* change to fold in order to stop the endless popping *)
    let traverse_tracker = 0 in
    let traverse_tracker =
      List.fold
        directions
        ~init:(traverse_tracker, found_exit)
        ~f:(fun (traverse_tracker, found_exit) dir ->
        [%message
          ""
            (current_path : Coord.t Stack.t)
            (traverse_tracker : int)
            (dir : Coord.t)]
        |> Sexp.to_string_hum
        |> print_endline;
        print_endline "-------------------------";
        if not found_exit
        then (
          let new_cell = Coord.add current_cell dir in
          if in_bounds new_cell
             && open_path new_cell
             && not (Hash_set.mem has_visited new_cell)
          then (
            let traverse_tracker = traverse_tracker + 1 in
            Hash_set.add has_visited new_cell;
            traverse_maze maze_matrix new_cell has_visited current_path;
            (* its not stopping after we find the solution *)
            traverse_tracker, found_exit)
          else traverse_tracker, found_exit)
        else traverse_tracker, found_exit)
    in
    (* if we reach the end of directions all of it sucked and we must
       backtrack *)

    (* i didnt really back track, i just kept popping off *)
    if traverse_tracker >= List.length directions
    then (
      let pop_cell = Stack.pop_exn current_path in
      ignore pop_cell;
      ()))
;;

(* takes in a maze represented as a string and returns the solution set as a
   string *)
(* this method works with the assumption that the maze is not jagged and
   there are uniform number of rows and columns *)
let solve_maze (maze : string) =
  let maze_matrix =
    String.split maze ~on:'\n'
    |> List.filter ~f:(fun str -> not (String.is_empty str))
    |> List.map ~f:(fun line -> String.to_array line)
    |> List.to_array
  in
  let start_coord = find_start_cell maze_matrix in
  let has_visited = Coord.Hash_set.create () in
  let current_path = Stack.create () in
  (* pushed the starting cell in the has_visited set *)
  Hash_set.add has_visited start_coord;
  traverse_maze maze_matrix start_coord has_visited current_path;
  current_path
;;

let solve_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file containing a maze and find a solution"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:"FILE a file containing a maze"
      in
      fun () ->
        let maze_text =
          In_channel.read_all (File_path.to_string input_file)
        in
        let solution_stack = solve_maze maze_text in
        print_s [%message "" (solution_stack : Coord.t Base.Stack.t)]]
;;

let command =
  Command.group ~summary:"maze commands" [ "solve", solve_command ]
;;

let dfs root =
  let rec f node path =
    if is_end
         node (* doesnt have current node so put it in before returning *)
    then Some path
    else
      (* make sure get children ONLY returns valid cells (in bounds, not
         visited [things in the path so we dont go backwards], and not a
         wall)*)
      List.map (get_children node) ~f:(fun child -> f child (node :: path))
      |> List.find_map ~f:(fun path -> path)
  in
  f root []
;;
