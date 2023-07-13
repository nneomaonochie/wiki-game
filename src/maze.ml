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
  (* we add the current cell to the has_visited set and the current_path
     stack *)
  Stack.push current_path current_cell;
  (* check if we hit the exit - base case and we win! *)
  if Char.equal
       maze_matrix.(Coord.get_r current_cell).(Coord.get_c current_cell)
       'E'
  then current_path
  else (
    (* we are testing one cell *)
    let new_cell = Coord.add current_cell (List.hd_exn directions) in
    Hash_set.add has_visited new_cell;
    if in_bounds new_cell && not (Hash_set.mem has_visited new_cell)
    then traverse_maze maze_matrix new_cell has_visited current_path
    else
      (* choose the next index in directions *)
      current_path
        (* if we try all 4 directions and we are flopping, we need to
           backtrack *)
        Stack.pop_exn
        current_path)
;;

(* should i return a correct path? *)

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
  let solution_path =
    traverse_maze maze_matrix start_coord has_visited current_path
  in
  ""
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
        let solution_text = solve_maze maze_text in
        ignore solution_text;
        print_s [%message ""]]
;;

let command =
  Command.group ~summary:"maze commands" [ "solve", solve_command ]
;;
