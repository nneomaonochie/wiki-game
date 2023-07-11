open! Core

(* [get_credits] should take the contents of an IMDB page for an actor and
   return a list of strings containing that actor's main credits. *)
let get_credits contents : string list =
  let open Soup in
  let movie_titles =
    parse contents $$ "a[class]" |> to_list
    (*ß |> List.filter ~f:(fun a -> String.equal (to_string a)
      "ipc-primary-image-list-card__title") *)
  in
  let movie_titles = List.map movie_titles ~f:to_string in
  (*let movie_titles : string list list = List.map (movie_titles : element
    node list) ~f:texts in [[move1 namel; movie 1 name 2]; [m2n1; m2n2]]
    concatenate within the stuff bro *)
  print_s [%message "" (movie_titles : string list)];
  [ "" ]
;;

(* |> List.dedup_and_sort (List.filter ~f:(fun string -> String.is_prefix
   string ~prefix:"Known for:")) ~compare:String.compare*)

let print_credits_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "given an IMDB page for an actor, print out a list of their main \
       credits"
    [%map_open
      let how_to_fetch, resource = File_fetcher.param in
      fun () ->
        let contents = File_fetcher.fetch_exn how_to_fetch ~resource in
        List.iter (get_credits contents) ~f:print_endline]
;;

let command =
  Command.group
    ~summary:"imdb commands"
    [ "print-credits", print_credits_command ]
;;
