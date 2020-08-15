open Js_of_ocaml
module Html = Js_of_ocaml.Dom_html

type state = {
        mutable n: int;
        mutable theme: string;
        board: Html.element Js.t;
        pick: Html.element Js.t;
}

let themes = [|
        "Theme not found";
        "Bring me back";
|]

let log x = Firebug.console##log x
let id = Fun.id
let nullstr () = Js.string ""

let get_board () = Option.get (Html.getElementById_coerce "puzzle" Html.CoerceTo.element)

let get_pick () = Option.get (Html.getElementById_coerce "pick" Html.CoerceTo.element)

let create_state board pick =
        {
                n = 0;
                theme = themes.(0);
                board = board;
                pick = pick;
        }

let todo s = failwith s

let char_of_string s def =
        if String.length s = 1 then s.[0]
        else def

let char_of_js_string s def =
        char_of_string (Js.to_string s) def

let with_char_of_js_string s f =
        let nul = Char.chr 0 in
        let c = char_of_js_string s nul in
        if c != nul then f c

let get_text elem =
        Js.Opt.case elem##.textContent nullstr id

let reset state =
        let f = function
                | ' ' as c -> c
                | _ -> '_'
        in
        let pattern = String.map f state.theme in
        state.board##.innerHTML := Js.string pattern;
        state.pick##.innerHTML := Js.string ""

let reveal_letter str letter theme =
        let lo = Char.lowercase_ascii in
        let f idx c =
                if lo theme.[idx] = lo letter then theme.[idx]
                else c
        in
        String.mapi f str

let reveal_board state letter =
        let s = get_text state.board in
        let new_s = reveal_letter (Js.to_string s) letter state.theme in
        state.board##.innerHTML := Js.string new_s

let update_pick pick letter =
        pick##.innerHTML := Js.string (String.make 1 letter)

let theme_found state =
        let text = get_text state.board in
        text = Js.string state.theme

let theme_found state =
        let text = get_text state.board in
        Js.to_string text = state.theme

let if_theme_found state f =
        if theme_found state then f()

let next_theme state =
        let next = state.n + 1 in
        state.n <- next;
        state.theme <- themes.(next);
        reset state

let keypressed state ev =
        let key = ev##.key in
        let str = Js.Optdef.case key nullstr id in
        with_char_of_js_string str (fun c ->
                reveal_board state c;
                update_pick state.pick c;
                if_theme_found state (fun () ->
                        next_theme state
                )
        );
        Js._true

let load _ =
        let board = get_board () in
        let pick = get_pick () in
        let state = create_state board pick in
        reset state;
        Html.document##.onkeydown := Html.handler (keypressed state);
        Js._false

let _ =
        Html.window##.onload := Html.handler load;
