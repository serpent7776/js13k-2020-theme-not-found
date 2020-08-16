open Js_of_ocaml
module Html = Js_of_ocaml.Dom_html

type state = {
        mutable n: int;
        mutable theme: string;
        mutable complete: state -> (unit -> unit) -> unit;
        container: Html.element Js.t;
        board: Html.element Js.t;
        pick: Html.element Js.t;
        note: Html.element Js.t;
}

type theme = {
        text: string;
        complete: state -> (unit -> unit) -> unit;
}

let todo s = failwith s

let log x = Firebug.console##log x
let id = Fun.id
let nullstr () = Js.string ""

let get_element eid = Option.get (Html.getElementById_coerce eid Html.CoerceTo.element)

let with_element eid f =
        let elem = get_element eid in
        f elem

let setClass elem cls =
        elem##setAttribute (Js.string "class") (Js.string cls)

let removeClass elem =
        elem##removeAttribute (Js.string "class")

let delayed time f =
        ignore (Html.setTimeout f time)

let get_container () = get_element "container"

let get_board () = get_element "puzzle"

let get_pick () = get_element "pick"

let get_note () = get_element "note"

let themes = [|
        {text = "Theme not found"; complete = (fun state cont ->
                state.board##.innerHTML := Js.string "<span>Theme</span> <span id=\"not\">not</span> <span>found</span>";
                with_element "not" (fun elem ->
                        delayed 250.0 (fun () ->
                                setClass elem "vanish"
                        );
                        delayed 1500.0 cont
                )
        )};
        {text = "Bring me back to life"; complete = (fun state cont ->
                state.note##.innerHTML := (Js.string "<p>My previous game: <a target=\"_blank\" href=\"https://js13kgames.com/entries/back-to-life\">play it here</a></p><div><button id=\"continue\">Continue</button></div>");
                with_element "continue" (fun button ->
                        button##.onclick := Html.handler (fun _ -> cont (); Js._true)
                );
                delayed 250.0 (fun () ->
                        setClass state.note "visible"
                )
        )};
        {text = "Turn it off and on again"; complete = (fun state cont ->
                todo ""
        )};
        {text = "Everything is lost"; complete = (fun state cont ->
                todo ""
        )};
        {text = "It is gilchted"; complete = (fun state cont ->
                todo ""
        )};
        {text = "desrever"; complete = (fun state cont ->
                todo ""
        )};
        {text = "water earth wind fire"; complete = (fun state cont ->
                todo ""
        )};
        {text = "bad day"; complete = (fun state cont ->
                todo ""
        )};
        {text = "That's all Folks!"; complete = (fun state cont ->
                todo ""
        )};
|]

let create_state container board pick note =
        let n = 0 in
        let theme = themes.(n) in
        {
                n = n;
                theme = theme.text;
                complete = theme.complete;
                container = container;
                board = board;
                pick = pick;
                note = note;
        }

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
        setClass state.container ("th" ^ string_of_int state.n);
        state.board##.innerHTML := Js.string pattern;
        state.pick##.innerHTML := Js.string "";
        state.note##.innerHTML := Js.string "";
        removeClass state.note

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
        let theme = themes.(next) in
        state.n <- next;
        state.theme <- theme.text;
        state.complete <- theme.complete;
        reset state

let keypressed (state: state) ev =
        let key = ev##.key in
        let str = Js.Optdef.case key nullstr id in
        with_char_of_js_string str (fun c ->
                reveal_board state c;
                update_pick state.pick c;
                if_theme_found state (fun () ->
                        state.complete state (fun () ->
                                next_theme state
                        )
                )
        );
        Js._true

let load _ =
        let container = get_container () in
        let board = get_board () in
        let pick = get_pick () in
        let note = get_note () in
        let state = create_state container board pick note in
        reset state;
        Html.document##.onkeydown := Html.handler (keypressed state);
        Js._false

let _ =
        Html.window##.onload := Html.handler load;
