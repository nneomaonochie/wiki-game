open! Core

let get_row (r, _) = r
let get_column (_, c) = c

(* locates where the starting index is *)
let find_start_cell maze_matrix =
  (* traverse the list to find the start of the maze *)
  let line_indexes = List.range 0 (List.length maze_matrix) in
  let start_row =
    List.find_exn line_indexes ~f:(fun index ->
      String.contains (List.nth_exn maze_matrix index) 'S')
  in
  start_row, String.index_exn (List.nth_exn maze_matrix start_row) 'S'
;;

(* recursively traverses the maze to find the correct solution *)
let rec traverse_maze maze_matrix current_cell =
  (* if we visited a cell, we are going to put a '+' character *)

  (* we grab the string, replace with the '+' character, then replace this
     NEW string in the prev. list *)
  List.nth_exn maze_matrix (get_row current_cell)
  |> String.substr_index ~pos:(get_column current_cell)
;;

(* should we change the path or should we store visited cells? bc when we
   find a path, how will we know to turn the +'s back into #?*)

(* takes in a maze represented as a string and returns the solution set as a
   string *)
(* this method works with the assumption that the maze is not jagged and
   there are uniform number of rows and columns *)
let solve_maze (maze : string) =
  let maze_matrix = String.split maze ~on:'\n' in
  (* in the int * int tuple, the first int is the index in the maze_matrix
     where the start is and the second int is the index of the string where
     the int is *)
  let start_coord = find_start_cell maze_matrix in
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
        print_s [%message maze_text]]
;;

let command =
  Command.group ~summary:"maze commands" [ "solve", solve_command ]
;;
