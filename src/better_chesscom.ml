[@@@warning "-32"]

open Yojson
open Lwt.Syntax
open Lwt
open Cohttp_lwt
open Cohttp_lwt_jsoo
open Js_of_ocaml

type user = {
  username: string;
  elo: string
}

type game = {
  white: user;
  black: user;
  pgn: string;
  is_black: bool;
  has_won: bool
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

let get_monthly_games username =
  let get_last_from_json_list json key =
    json
    |> Safe.Util.member key |> Safe.Util.to_list
    |> (fun lst -> List.nth (List.rev lst) 0) in
  let* archives = get_json ("https://api.chess.com/pub/player/" ^ username ^ "/games/archives") in
  let* games = get_last_from_json_list archives "archives" |> Safe.Util.to_string
  |> get_json in
  games |> Safe.Util.member "games" |> Safe.Util.to_list |> Lwt.return

let get_pgn json = json |> Safe.Util.member "pgn" |> Safe.Util.to_string

let get_user json color =
  let profile = Safe.Util.member color json in
  {username = profile |> Safe.Util.member "username" |> Safe.Util.to_string;
   elo = profile |> Safe.Util.member "rating" |> Safe.Util.to_int |> string_of_int}

let has_black_won json =
  (json |> Safe.Util.member "black" |> Safe.Util.member "result" |> Safe.Util.to_string) = "win"

let user_black json username =
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
  let* games = get_monthly_games username in
  let rec get_games = function
    | hd :: tl ->
      let is_black = user_black hd username in
      let black_won = has_black_won hd in
      {
      white = get_user hd "white";
      black = get_user hd "black";
      pgn = get_pgn hd;
      is_black = is_black;
      has_won = (black_won && is_black) || (not black_won && not is_black)
      } :: (get_games tl)
    | _ -> [] in
  get_games games |> Lwt.return

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

let overview_string white black =
  let user_format username elo =
    username ^ " (" ^ elo ^ ")" in
  (user_format white.username white.elo) ^ " - " ^ (user_format black.username black.elo) 

let games_screen page games =
  Lwt_list.iter_p
    (fun game ->
      let div = Dom_html.createDiv doc in
      if game.has_won
        then div##.className := Js.string "overview_won"
        else div##.className := Js.string "overview_lost";
      let button = Dom_html.createButton doc in
      button##.innerHTML := Js.string "Analyse";
      let overview = Dom_html.createP doc in
      overview##.innerHTML :=
        Js.string
          ("<a>" ^ (overview_string game.white game.black) ^ "</a>");
      div <+> overview;
      div <+> button;
      page <+> div;
      let* _event =
        Js_of_ocaml_lwt.Lwt_js_events.(
          seq_loop click button (fun _ _ ->
              let* link = get_lichess_url game.pgn game.is_black in
              let bt = Dom_html.createP doc in
              bt##.innerHTML :=
                Js.string ("<a class='button' href='" ^ link ^ "'>Analyse</a>");
              div <-> button;
              div <+> bt;
              Lwt.return_unit))
      in

      div <+> button;
      doc##.body <+> div;
      page <+> doc;
      Lwt.return_unit)
    games

let onload _ =
  get_search (fun text ->
  let page = Dom_html.getElementById "main" in
  let login = Dom_html.getElementById "login-screen" in
  page <-> login;
  (let* gm = main @@ Js.to_string text in
  games_screen page gm |> Lwt.ignore_result
  |> Lwt.return)
  |> Lwt.ignore_result);
  Js._false

let () = Dom_html.window##.onload := Dom_html.handler onload
