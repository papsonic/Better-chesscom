
open Opium
open Yojson
open Lwt.Syntax

let get_json link =
  link
  |> Request.get
  |> Request.to_json_exn

let get_last_from_json_list json key =
  json
  |> Safe.Util.member key |> Safe.Util.to_list
  |> (fun lst -> List.nth lst ((List.length lst) - 1))

let get_last_game_json username =
  let* archives = get_json ("http://api.chess.com/pub/player/" ^ username ^ "/games/archives") in
  let* games = get_last_from_json_list archives "archives" |> Safe.Util.to_string
  |> get_json in
  get_last_from_json_list games "games" |> Lwt.return

let get_pgn json = json |> Safe.Util.member "pgn" |> Safe.Util.to_string

let is_black json username =
  let black = json |> Safe.Util.member "black" |> Safe.Util.member "username" |> Safe.Util.to_string in
  black = username

let get_lichess_url pgn color =
  let* json = Request.of_json ~body:(`Assoc [ "pgn", `String pgn ]) "https://lichess.org/api/import" `POST |>
    Request.to_json_exn in
  let str = json |> Safe.Util.member "url" |> Safe.Util.to_string in
    if color then Lwt.return @@ str ^ "/black"
    else Lwt.return str

let main username =
  let* last_game = get_last_game_json username in
  get_lichess_url (get_pgn last_game) (is_black last_game username)

let () =
  Printexc.record_backtrace true;
  Printexc.print_backtrace stdout;
  Lwt_main.run (main "hikaru") |> print_string
