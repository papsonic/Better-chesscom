[@@@warning "-32"]

open Yojson
open Lwt.Syntax
open Lwt
open Cohttp_lwt
open Cohttp_lwt_jsoo
open Js_of_ocaml

type user = {
  username: string;
}

type game = {
  white: user;
  black: user;
  link: string;
}

let doc = Dom_html.window##.document
let ( <+> ) = Dom.appendChild
let ( <-> ) = Dom.removeChild

let json_of_body body =
  let* str = body |> Body.to_string in
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

let get_last_game_json username =
  let get_last_from_json_list json key =
    json
    |> Safe.Util.member key |> Safe.Util.to_list
    |> (fun lst -> List.nth (List.rev lst) 0) in
  let* archives = get_json ("https://api.chess.com/pub/player/" ^ username ^ "/games/archives") in
  let* games = get_last_from_json_list archives "archives" |> Safe.Util.to_string
  |> get_json in
  get_last_from_json_list games "games" |> Lwt.return

let get_pgn json = json |> Safe.Util.member "pgn" |> Safe.Util.to_string

let get_user json color =
  let profile = Safe.Util.member color json in
  {username = profile |> Safe.Util.member "username" |> Safe.Util.to_string}

let is_black json username =
  let black = json |> Safe.Util.member "black" |> Safe.Util.member "username" |> Safe.Util.to_string in
  black = username

let format_pgn pgn =
  pgn
  |> Str.global_replace (Str.regexp "%") ""
  |> Str.global_replace (Str.regexp {|\\"|}) {|"|}
  |> Str.global_replace (Str.regexp "\\n") ""

let get_lichess_url pgn is_black =
  let* json = post_urlencoded "https://lichess.org/api/import" "pgn" (format_pgn pgn) in
  let str = json |> Safe.Util.member "url" |> Safe.Util.to_string in
  if is_black then Lwt.return @@ str ^ "/black"
  else Lwt.return str

let main username =
  let* last_game = get_last_game_json username in
  let* link = get_lichess_url (get_pgn last_game) (is_black last_game username) in
  {
    white = get_user last_game "white";
    black = get_user last_game "black";
    link = link
  } |> Lwt.return

let get_search callback =
  let search_bar =
    Option.get
      (Dom_html.getElementById_coerce "search-bar" Dom_html.CoerceTo.input)
  in
  search_bar##.onkeypress :=
    Dom_html.handler (fun event ->
        if event##.keyCode = 13 then (
          callback search_bar##.value;
          Js._false)
        else Js._true)

let games_screen page game =
  let button = Dom_html.createP doc in
  button##.innerHTML :=
    Js.string ("<a class='button' href='" ^ game.link ^ "'>Analyse</a>");
  let overview = Dom_html.createP doc in
  overview##.innerHTML :=
    Js.string ("<a>" ^ game.white.username ^ "-" ^ game.black.username ^ "</a>");
  doc##.body <+> overview;
  doc##.body <+> button;
  page <+> doc

let onload _ =
  get_search (fun text ->
  let page = Dom_html.getElementById "main" in
  let login = Dom_html.getElementById "login-screen" in
  page <-> login;
  (let* gm = main @@ Js.to_string text in
  games_screen page gm
  |> Lwt.return)
  |> Lwt.ignore_result);
  Js._false

let () = Dom_html.window##.onload := Dom_html.handler onload

(* let () =
  Printexc.record_backtrace true;
  Printexc.print_backtrace stdout;
  Lwt_main.run (main "hikaru") |> print_string *)
