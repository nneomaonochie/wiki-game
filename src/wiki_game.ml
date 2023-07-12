open! Core

(* [get_linked_articles] should return a list of wikipedia article lengths
   contained in the input.

   Note that [get_linked_articles] should ONLY return things that look like
   wikipedia articles. In particular, we should discard links that are: -
   Wikipedia pages under special namespaces that are not articles (see
   https://en.wikipedia.org/wiki/Wikipedia:Namespaces) - other Wikipedia
   internal URLs that are not articles - resources that are external to
   Wikipedia - page headers

   One nice think about Wikipedia is that stringent content moderation
   results in uniformity in article format. We can expect that all Wikipedia
   article links parsed from a Wikipedia page will have the form
   "/wiki/<TITLE>". *)
let get_linked_articles contents : string list =
  let open Soup in
  let all_links = parse contents $$ "a" |> to_list in
  (* ensures all links have hrefs as attributes *)
  let all_links =
    List.filter_map all_links ~f:(fun a -> attribute "href" a)
  in
  let wiki_links =
    List.filter all_links ~f:(String.is_prefix ~prefix:"/wiki/")
  in
  (* removes duplicate links and ensures that only links that are NOT
     namespaces are included *)
  let wiki_links =
    List.dedup_and_sort
      (List.filter wiki_links ~f:(fun string ->
         match Wikipedia_namespace.namespace string with
         | None -> true
         | Some _ -> false))
      ~compare:String.compare
  in
  wiki_links
;;

let print_links_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Print all of the valid wiki page links on a page"
    [%map_open
      let how_to_fetch, resource = File_fetcher.param in
      fun () ->
        let contents = File_fetcher.fetch_exn how_to_fetch ~resource in
        List.iter (get_linked_articles contents) ~f:print_endline]
;;

(* In order to visualize the social network, we use the ocamlgraph library to
   create a [Graph] structure whose vertices are of type [Person.t].

   The ocamlgraph library exposes lots of different ways to construct
   different types of graphs. Take a look at
   https://github.com/backtracking/ocamlgraph/blob/master/src/imperative.mli
   for documentation on other types of graphs exposed by this API. *)
module G = Graph.Imperative.Graph.Concrete (String)

(* We extend our [Graph] structure with the [Dot] API so that we can easily
   render constructed graphs. Documentation about this API can be found here:
   https://github.com/backtracking/ocamlgraph/blob/master/src/dot.mli *)
module Dot = Graph.Graphviz.Dot (struct
  include G

  (* These functions can be changed to tweak the appearance of the generated
     graph. Check out the ocamlgraph graphviz API
     (https://github.com/backtracking/ocamlgraph/blob/master/src/graphviz.mli)
     for examples of what values can be set here. *)
  let edge_attributes _ = [ `Dir `None ]
  let default_edge_attributes _ = []
  let get_subgraph _ = None
  let vertex_attributes v = [ `Shape `Box; `Label v; `Fillcolor 1000 ]
  let vertex_name v = v
  let default_vertex_attributes _ = []
  let graph_attributes _ = []
end)

(* [visualize] should explore all linked articles up to a distance of
   [max_depth] away from the given [origin] article, and output the result as
   a DOT file. It should use the [how_to_fetch] argument along with
   [File_fetcher] to fetch the articles so that the implementation can be
   tested locally on the small dataset in the ../resources/wiki directory. *)
let visualize ?(max_depth = 3) ~origin ~output_file ~how_to_fetch () : unit =
  (*let contents = File_fetcher.fetch_exn how_to_fetch ~resource:origin in *)
  let graph = G.create () in
  let has_visited = String.Hash_set.create () in
  let rec traverse_links ~d ~origin =
    (* we add the given origin link to a set of links that have been
       visited *)
    Hash_set.add has_visited origin;
    (* these are a list of all the wiki links in the origin article *)
    let link_descendents =
      get_linked_articles
        (File_fetcher.fetch_exn how_to_fetch ~resource:origin)
    in
    List.iter link_descendents ~f:(fun link ->
      if (not (Hash_set.mem has_visited link)) && d > 0
      then (
        let get_link_name str =
          let name = Lambda_soup_utilities.get_title str in
          String.substr_replace_all name ~pattern:"- Wikipedia" ~with_:""
          |> String.substr_replace_all ~pattern:"(biology)" ~with_:""
        in
        let a =
          get_link_name
            (File_fetcher.fetch_exn how_to_fetch ~resource:origin)
        in
        let b =
          get_link_name (File_fetcher.fetch_exn how_to_fetch ~resource:link)
        in
        (* look at this later *)
        print_s [%message "" (a : string)];
        print_s [%message "" (b : string)];
        G.add_edge_e graph (a, b);
        traverse_links ~d:(d - 1) ~origin:link))
  in
  traverse_links ~d:max_depth ~origin;
  Dot.output_graph
    (Out_channel.create (File_path.to_string output_file))
    graph
;;

(* visualize command isnt working as intended, doesnt like () or other
   importnat info, work on later *)

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing \
       the highway network"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        visualize ~max_depth ~origin ~output_file ~how_to_fetch ();
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

(* [find_path] should attempt to find a path between the origin article and
   the destination article via linked articles.

   [find_path] should use the [how_to_fetch] argument along with
   [File_fetcher] to fetch the articles so that the implementation can be
   tested locally on the small dataset in the ../resources/wiki directory.

   [max_depth] is useful to limit the time the program spends exploring the
   graph. *)
let find_path ?(max_depth = 3) ~origin ~destination ~how_to_fetch () =
  ignore (max_depth : int);
  ignore (origin : string);
  ignore (destination : string);
  ignore (how_to_fetch : File_fetcher.How_to_fetch.t);
  failwith "TODO"
;;

let find_path_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "Play wiki game by finding a link between the origin and destination \
       pages"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and destination =
        flag "destination" (required string) ~doc:" the destination page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      in
      fun () ->
        match find_path ~max_depth ~origin ~destination ~how_to_fetch () with
        | None -> print_endline "No path found!"
        | Some trace -> List.iter trace ~f:print_endline]
;;

let command =
  Command.group
    ~summary:"wikipedia game commands"
    [ "print-links", print_links_command
    ; "visualize", visualize_command
    ; "find-path", find_path_command
    ]
;;
