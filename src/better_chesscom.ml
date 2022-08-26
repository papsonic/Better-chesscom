[@@@warning "-32"]

open Yojson
open Lwt.Syntax
open Lwt
open Cohttp_lwt
open Cohttp_lwt_jsoo
open Js_of_ocaml

type user = { username : string; elo : string }

type game = {
  white : user;
  black : user;
  pgn : string;
  opening : string;
  time_control : string;
  time_class : string;
  is_black : bool;
  has_won : bool;
}

let doc = Dom_html.window##.document
let append_first n c = Dom.insertBefore n c n##.firstChild
let ( <++> ) = append_first
let ( <+> ) = Dom.appendChild
let ( <-> ) = Dom.removeChild

let json_of_body body =
  let* str = body |> Body.to_string in
  str |> Safe.from_string |> Lwt.return

let get_json link =
  link |> Uri.of_string |> Client.get >>= fun (_, body) -> json_of_body body

let post_urlencoded url key content =
  Client.post
    ~body:(Body.of_string (key ^ "=" ^ content))
    ~headers:
      ( Cohttp.Header.init () |> fun h ->
        Cohttp.Header.add h "Content-Type"
          "application/x-www-form-urlencoded; charset=UTF-8" )
    (Uri.of_string url)
  >>= fun (_, body) -> json_of_body body

let get_monthly_games username =
  let get_last_from_json_list json key =
    json |> Safe.Util.member key |> Safe.Util.to_list |> fun lst ->
    List.nth (List.rev lst) 0
  in
  let* archives =
    get_json ("https://api.chess.com/pub/player/" ^ username ^ "/games/archives")
  in
  let* games =
    get_last_from_json_list archives "archives"
    |> Safe.Util.to_string |> get_json
  in
  games |> Safe.Util.member "games" |> Safe.Util.to_list |> Lwt.return

let get_pgn json = json |> Safe.Util.member "pgn" |> Safe.Util.to_string

let get_user json color =
  let profile = Safe.Util.member color json in
  {
    username = profile |> Safe.Util.member "username" |> Safe.Util.to_string;
    elo =
      profile |> Safe.Util.member "rating" |> Safe.Util.to_int |> string_of_int;
  }

let has_black_won json =
  json |> Safe.Util.member "black" |> Safe.Util.member "result"
  |> Safe.Util.to_string = "win"

let user_black json username =
  let black =
    json |> Safe.Util.member "black"
    |> Safe.Util.member "username"
    |> Safe.Util.to_string
  in
  black = username

let get_time_class json =
  json |> Safe.Util.member "time_class" |> Safe.Util.to_string

let get_opening pgn =
  let re =
    Re.seq [ Re.str "openings/"; Re.group (Re.rep Re.any); Re.str {|[UTCD|} ]
    |> Re.compile
    |> fun e -> Re.exec e pgn
  in
  re
  |> (fun e -> Re.Group.get e 1)
  |> Str.split (Str.regexp {|"|})
  |> (fun lst -> List.nth lst 0)
  |> Str.global_replace (Str.regexp "-") " "

let get_time_control json =
  let time_and_increment =
    json |> Safe.Util.member "time_control" |> Safe.Util.to_string |> fun str ->
    Str.split (Str.regexp "+") str
  in
  let time_in_min =
    List.nth time_and_increment 0
    |> int_of_string
    |> (fun n -> n / 60)
    |> string_of_int
  in
  match time_and_increment with
  | [ _; increment ] -> time_in_min ^ "+" ^ increment
  | _ -> time_in_min ^ "+0"

let format_pgn pgn =
  pgn
  |> Str.global_replace (Str.regexp "%") ""
  |> Str.global_replace (Str.regexp {|\\"|}) {|"|}
  |> Str.global_replace (Str.regexp "\\n") ""

let get_lichess_url pgn is_black =
  let* json =
    post_urlencoded "https://lichess.org/api/import" "pgn" (format_pgn pgn)
  in
  let str = json |> Safe.Util.member "url" |> Safe.Util.to_string in
  if is_black then Lwt.return @@ str ^ "/black" else Lwt.return str

let get_infos username game =
  let is_black = user_black game username in
  let black_won = has_black_won game in
  let pgn = get_pgn game in
  {
    white = get_user game "white";
    black = get_user game "black";
    pgn;
    opening = get_opening pgn;
    time_control = get_time_control game;
    time_class = get_time_class game;
    is_black;
    has_won = (black_won && is_black) || ((not black_won) && not is_black);
  }

let main username max =
  let* games = get_monthly_games username in
  let rec get_games acc = function
    | hd :: tl ->
        if acc = max then []
        else get_infos username hd :: get_games (acc + 1) tl
    | _ -> []
  in
  get_games 0 (List.rev games) |> List.rev |> Lwt.return

let get_last_game username =
  let* games = get_monthly_games username in
  games |> List.rev
  |> (fun lst -> List.nth lst 0)
  |> get_infos username |> Lwt.return

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

let overview_string white black time_control time_class opening =
  let user_format username elo = username ^ " (" ^ elo ^ ")" in
  time_control ^ " (" ^ time_class ^ ") : "
  ^ user_format white.username white.elo
  ^ " - "
  ^ user_format black.username black.elo
  ^ " : " ^ opening

let games_screen page games username =
  let refresh = Dom_html.createButton doc in
  refresh##.innerHTML := Js.string "Refresh";
  page <+> refresh;
  let games_div = Dom_html.createDiv doc in
  page <+> games_div;
  let display_game game =
    let div = Dom_html.createDiv doc in
    if game.has_won then div##.className := Js.string "overview_won"
    else div##.className := Js.string "overview_lost";
    let button = Dom_html.createButton doc in
    button##.innerHTML := Js.string "Analyse";
    button##.className := Js.string "analyse-button";
    let overview = Dom_html.createP doc in
    overview##.innerHTML :=
      Js.string
        ("<a>"
        ^ overview_string game.white game.black game.time_control
            game.time_class game.opening
        ^ "</a>");
    div <+> overview;
    div <+> button;
    games_div <++> div;
    let* _event =
      Js_of_ocaml_lwt.Lwt_js_events.(
        seq_loop click button (fun _ _ ->
            let* link = get_lichess_url game.pgn game.is_black in
            let bt = Dom_html.createP doc in
            bt##.innerHTML :=
              Js.string ("<a class='link' href='" ^ link ^ "'>Analyse</a>");
            div <-> button;
            div <+> bt;
            Lwt.return_unit))
    in
    games_div <++> div;
    Lwt.return_unit
  in

  let* () =
    Lwt_list.iter_p (fun game -> display_game game) games
    |> Lwt.ignore_result |> Lwt.return
  in

  let rec event () : 'a Lwt.t =
    Js_of_ocaml_lwt.Lwt_js_events.(
      seq_loop click refresh (fun _ _ ->
          let* game = get_last_game username in
          display_game game |> Lwt.ignore_result;
          event ()))
  in
  event () |> Lwt.ignore_result;
  Lwt.return_unit

let onload _ =
  get_search (fun text ->
      let page = Dom_html.getElementById "main" in
      let login = Dom_html.getElementById "login-screen" in
      page <-> login;
      (let username = Js.to_string text in
       let* gm = main username 60 in
       games_screen page gm username |> Lwt.ignore_result |> Lwt.return)
      |> Lwt.ignore_result);
  Js._false

let () = Dom_html.window##.onload := Dom_html.handler onload
