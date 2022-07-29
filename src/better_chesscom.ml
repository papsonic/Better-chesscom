
open Yojson
open Lwt.Syntax
open Lwt
open Cohttp_lwt_unix
open Cohttp_lwt

let json_of_body body =
  let* str = body |> Cohttp_lwt.Body.to_string in
  str |> Safe.from_string |> Lwt.return

let get_json link =
  link
  |> Uri.of_string
  |> Client.get
  >>= fun (_, body) ->
  json_of_body body

let post_urlencoded url key content =
  Client.post
  ~body: (Body.of_string (key ^ "=" ^ content))
  ~headers:
  ((Cohttp.Header.init()) |>
  (fun h -> Cohttp.Header.add h "Content-Type" "application/x-www-form-urlencoded; charset=UTF-8"))
  (Uri.of_string url)
  >>= fun (_, body) ->
    json_of_body body

let get_last_from_json_list json key =
  json
  |> Safe.Util.member key |> Safe.Util.to_list
  |> (fun lst -> List.nth lst ((List.length lst) - 1))

let get_last_game_json username =
  let* archives = get_json ("https://api.chess.com/pub/player/" ^ username ^ "/games/archives") in
  let* games = get_last_from_json_list archives "archives" |> Safe.Util.to_string
  |> get_json in
  get_last_from_json_list games "games" |> Lwt.return

let get_pgn json = json |> Safe.Util.member "pgn" |> Safe.Util.to_string

let is_black json username =
  let black = json |> Safe.Util.member "black" |> Safe.Util.member "username" |> Safe.Util.to_string in
  black = username

let format_pgn pgn =
  pgn
  |> (fun str -> Str.global_replace (Str.regexp "%") "" str)
  |> (fun str -> Str.global_replace (Str.regexp {|\\"|}) {|"|} str)
  |> (fun str -> Str.global_replace (Str.regexp "\\n") "" str)

let get_lichess_url pgn is_black =
  let* json = post_urlencoded "https://lichess.org/api/import" "pgn" (format_pgn pgn) in
  let str = json |> Safe.Util.member "url" |> Safe.Util.to_string in
  if is_black then Lwt.return @@ str ^ "/black"
  else Lwt.return str

let main username =
  let* last_game = get_last_game_json username in
  get_lichess_url (get_pgn last_game) (is_black last_game username)

let () =
  Printexc.record_backtrace true;
  Printexc.print_backtrace stdout;
  Lwt_main.run (main "hikaru") |> print_string
