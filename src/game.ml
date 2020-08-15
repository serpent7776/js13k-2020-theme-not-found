open Js_of_ocaml
module Html = Js_of_ocaml.Dom_html

let log x = Firebug.console##log x
let id = Fun.id
let nullstr () = Js.string ""

let theme = "Theme not found"
let get_board () = Option.get (Html.getElementById_coerce "puzzle" Html.CoerceTo.element)

let todo s = failwith s

let char_of_string s def =
        if String.length s = 1 then s.[0]
        else def

let char_of_js_string s def =
        char_of_string (Js.to_string s) def

let get_text elem =
        Js.Opt.case elem##.textContent nullstr id

let reset board theme =
        let f = function
                | ' ' as c -> c
                | _ -> '_'
        in
        let pattern = String.map f theme in
        board##.innerHTML := Js.string pattern

let reveal_letter str letter =
        let lo = Char.lowercase_ascii in
        let f idx c =
                if lo theme.[idx] = lo letter then theme.[idx]
                else c
        in
        String.mapi f str

let reveal_board (board: Html.element Js.t) letter =
        let s = get_text board in
        let new_s = reveal_letter (Js.to_string s) letter in
        board##.innerHTML := Js.string new_s

let theme_found board =
        let text = get_text board in
        Js.to_string text = theme

let keypressed (board: Html.element Js.t) ev =
        let key = ev##.key in
        let str = Js.Optdef.case key nullstr id in
        let letter = char_of_js_string str ' ' in
        reveal_board board letter;
        Js._true

let load _ =
        let board = get_board () in
        reset board theme;
        Html.document##.onkeydown := Html.handler (keypressed board);
        Js._false

let _ =
        Html.window##.onload := Html.handler load;
