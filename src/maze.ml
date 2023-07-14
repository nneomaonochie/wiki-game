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

(* a list of cells that will change a cell's current position to be one of
   the directions listed: east, south, west, north *)
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

(* returns true if a cell is in bounds *)
let in_bounds maze_matrix new_cell : bool =
  Coord.get_r new_cell >= 0
  && Coord.get_r new_cell < Array.length maze_matrix
  && Coord.get_c new_cell >= 0
  && Coord.get_c new_cell < Array.length (Array.get maze_matrix 0)
;;

(* returns true if the cell we are in is not a wall *)
let open_path maze_matrix new_cell =
  not
    (Char.equal
       maze_matrix.(Coord.get_r new_cell).(Coord.get_c new_cell)
       '#')
;;

(* recursively traverse through maze to find the path that connects us from
   start to end *)
let dfs maze_matrix root =
  let rec traverse curr_cell path =
    (* returns a list of other positions we can traverse from the current
       cell *)
    let get_children parent_cell =
      (* creates an initial list of cells in the E, S, W, N direction of the
         current cell *)
      List.map directions ~f:(fun dir -> Coord.add parent_cell dir)
      (* narrows down list to ensure other cells are in bounds, are not a
         wall, and have not been previously visted*)
      |> List.filter ~f:(fun child_cell ->
           in_bounds maze_matrix child_cell
           && open_path maze_matrix child_cell
           && not
                (List.exists path ~f:(fun cell ->
                   Coord.equal cell child_cell)))
    in
    (* if this cell is an exit cell *)
    if Char.equal
         maze_matrix.(Coord.get_r curr_cell).(Coord.get_c curr_cell)
         'E'
    then (
      let path = curr_cell :: path in
      Some path)
    else
      (* returns a list of children we can recurse through *)
      List.map (get_children curr_cell) ~f:(fun child ->
        traverse child (curr_cell :: path))
      |> List.find_map ~f:(fun path -> path)
  in
  traverse root []
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
  let path_list : Coord.t list =
    match dfs maze_matrix start_coord with
    | None -> []
    | Some path -> List.rev path
  in
  path_list
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
        print_s [%message "" (solution_stack : Coord.t list)]]
;;

let command =
  Command.group ~summary:"maze commands" [ "solve", solve_command ]
;;
