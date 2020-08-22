open Js_of_ocaml
module Html = Js_of_ocaml.Dom_html

type state = {
        mutable n: int;
        mutable theme: string;
        mutable complete: state -> (unit -> unit) -> unit;
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
let jstr s = Js.string s
let jtrue = Js._true
let jfalse = Js._false
let nullstr () = jstr ""

let get_element eid = Option.get (Html.getElementById_coerce eid Html.CoerceTo.element)

let with_element eid f =
        let elem = get_element eid in
        f elem

let with_queried_element q f =
        (Html.document##querySelector (jstr q) |> Js.Opt.iter) f

let setClass elem cls =
        elem##setAttribute (jstr "class") (jstr cls)

let removeClass elem =
        elem##removeAttribute (jstr "class")

let iteri_children (elem: Html.element Js.t) f =
        let children = elem##.childNodes in
        let size = children##.length in
        for i = 0 to size-1 do
                let node = children##item i in
                (Js.Opt.bind node Html.CoerceTo.element |> Js.Opt.iter) (f i)
        done

let delayed time f =
        ignore (Html.setTimeout f time)

let get_board () = get_element "puzzle"

let get_pick () = get_element "pick"

let get_note () = get_element "note"

let themes = [|
        {text = "Theme not found"; complete = (fun state cont ->
                state.board##.innerHTML := jstr {|<span class="box c">T</span><span class="box c">h</span><span class="box c">e</span><span class="box c">m</span><span class="box c">e</span><span id="not"><span class="box"> </span><span class="box c">n</span><span class="box c">o</span><span class="box c">t</span></span><span class="box"> </span><span class="box c">f</span><span class="box c">o</span><span class="box c">u</span><span class="box c">n</span><span class="box c">d</span>|};
                with_element "not" (fun elem ->
                        delayed 250.0 (fun () ->
                                setClass elem "vanish"
                        );
                        delayed 1500.0 cont
                )
        )};
        {text = "Bring me back to life"; complete = (fun state cont ->
                state.note##.innerHTML := (jstr "<p>My previous game: <a target=\"_blank\" href=\"https://js13kgames.com/entries/back-to-life\">play it here</a></p><div><button id=\"continue\">Continue</button></div>");
                with_element "continue" (fun button ->
                        button##.onclick := Html.handler (fun _ -> cont (); jtrue)
                );
                delayed 250.0 (fun () ->
                        setClass state.note "visible"
                )
        )};
        {text = "Turn it off and on again"; complete = (fun _ cont ->
                with_queried_element "body" (fun body ->
                        delayed 500.0 (fun () ->
                                setClass body "th2 off"
                        );
                        delayed 2000.0 (fun () ->
                                setClass body "th2"
                        );
                        delayed 3500.0 cont
                )
        )};
        {text = "Everything is lost"; complete = (fun state cont ->
                state.board##.innerHTML := jstr {|<span id="all"><span class="box c">E</span><span class="box c">v</span><span class="box c">e</span><span class="box c">r</span><span class="box c">y</span><span class="box c">t</span><span class="box c">h</span><span class="box c">i</span><span class="box c">n</span><span class="box c">g</span><span class="box"> </span><span class="box c">i</span><span class="box c">s</span><span class="box"> </span></span><span class="box c">l</span><span class="box c">o</span><span class="box c">s</span><span class="box c">t</span>|};
                with_element "all" (fun elem ->
                        delayed 500.0 (fun () ->
                                setClass elem "rot"
                        );
                        delayed 1550.0 (fun () ->
                                setClass elem "rot drop"
                        );
                        delayed 2000.0 cont
                )
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

let create_state board pick note =
        let n = 0 in
        let theme = themes.(n) in
        {
                n = n;
                theme = theme.text;
                complete = theme.complete;
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

let js_string_of_char c =
        jstr (String.make 1 c)

let get_text elem =
        Js.Opt.case elem##.textContent nullstr id

let make_board_html theme =
        let conv = function
                | ' ' -> "<span class=\"box\"> </span>"
                | _ -> "<span class=\"box c\">_</span>"
        in
        String.to_seq theme |> Seq.map conv |> Seq.fold_left (^) ""

let reset state =
        with_queried_element "body" (fun body ->
                setClass body ("th" ^ string_of_int state.n)
        );
        state.board##.innerHTML := jstr (make_board_html state.theme);
        state.pick##.innerHTML := jstr "";
        state.note##.innerHTML := jstr "";
        removeClass state.note

let reveal_board state letter =
        let lo = Char.lowercase_ascii in
        let lo_letter = lo letter in
        iteri_children state.board (fun idx elem ->
                if lo state.theme.[idx] = lo_letter then elem##.innerHTML := js_string_of_char state.theme.[idx]
        )

let update_pick pick letter =
        pick##.innerHTML := js_string_of_char letter

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
        jtrue

let load _ =
        let board = get_board () in
        let pick = get_pick () in
        let note = get_note () in
        let state = create_state board pick note in
        reset state;
        Html.document##.onkeydown := Html.handler (keypressed state);
        jfalse

let _ =
        Html.window##.onload := Html.handler load;
