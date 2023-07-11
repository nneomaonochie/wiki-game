open! Core
module City = String

(* We separate out the [Highway] module to represent our highway system *)
module Highway = struct
  (* We can represent our highway system graph as a set of connections, where
     a connection represents a friendship between two people. *)
  module Connection = struct
    module T = struct
      type t =
        { name : string
        ; start : City.t
        ; dest : City.t
        }
      [@@deriving compare, sexp]
      (* the first index is the highway *)
    end

    (* This funky syntax is necessary to implement sets of [Connection.t]s.
       This is needed to defined our [Network.t] type later. Using this
       [Comparable.Make] functor also gives us immutable maps, which might
       come in handy later. *)
    include Comparable.Make (T)

    let of_string s =
      let cities = String.split s ~on:',' in
      let highway = List.hd_exn cities in
      (* trying to remove head from rest of the city list *)
      let cities =
        match cities with
        | [] -> []
        | _ :: city -> Some (City.of_string city)
      in
      (* return back after you figure out the implementaoin*)
      let tuple_connections : string * string =
        List.map cities ~f:(fun city1 ->
          List.iter cities ~f:(fun city2 ->
            if !(String.equal city1 city2) then highway, city1, city2))
      in
      tuple_connections
    ;;
  end

  type t = Connection.Set.t [@@deriving sexp_of]

  let of_file input_file =
    let connections =
      In_channel.read_lines (File_path.to_string input_file)
      |> List.concat_map ~f:(fun s ->
           match Connection.of_string s with
           | Some (a, b) ->
             (* Friendships are mutual; a connection between a and b means we
                should also consider the connection between b and a. *)
             [ a, b; b, a ]
           | None ->
             printf
               "ERROR: Could not parse line as connection; dropping. %s\n"
               s;
             [])
    in
    Connection.Set.of_list connections
  ;;
end

let load_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file listing interstates and serialize graph as a sexp"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:
            "FILE a file listing interstates and the cities they go through"
      in
      fun () ->
        ignore (input_file : File_path.t);
        failwith "TODO"]
;;

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing \
       the highway network"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:
            "FILE a file listing all interstates and the cities they go \
             through"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        ignore (input_file : File_path.t);
        ignore (output_file : File_path.t);
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

let command =
  Command.group
    ~summary:"interstate highway commands"
    [ "load", load_command; "visualize", visualize_command ]
;;
