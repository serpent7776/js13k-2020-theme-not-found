open Js_of_ocaml
module Html = Js_of_ocaml.Dom_html

type state = {
        mutable n: int;
        mutable typos: int;
        mutable theme: string;
        mutable complete: state -> (unit -> unit) -> unit;
        board: Html.element Js.t;
        pick: Html.element Js.t;
        note: Html.element Js.t;
        counter: Html.element Js.t;
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

let get_element eid = Option.get (Html.getElementById_coerce eid Html.CoerceTo.element)

let with_parent_element elem f = ((Js.Opt.bind elem##.parentNode Html.CoerceTo.element) |> Js.Opt.iter) f

let with_element eid f =
        let elem = get_element eid in
        f elem

let with_queried_element q f =
        (Html.document##querySelector (jstr q) |> Js.Opt.iter) f

let setClass elem cls =
        elem##setAttribute (jstr "class") (jstr cls)

let addClass (elem: Html.element Js.t) cls =
        let class_str = (jstr "class") in
        let current_cls = (elem##getAttribute class_str |> Js.Opt.get) (fun () -> jstr "") in
        let new_cls = current_cls##concat_2 (jstr " ") (jstr cls) in
        elem##setAttribute class_str new_cls

let removeClass elem =
        elem##removeAttribute (jstr "class")

let setStyle elem style =
        elem##setAttribute (jstr "style") (jstr style)

let with_nth_child parent n f =
        let child_node = parent##.childNodes##item n in
        (Js.Opt.bind child_node Html.CoerceTo.element |> Js.Opt.iter) f

let iteri_children (elem: Html.element Js.t) f =
        let children = elem##.childNodes in
        let size = children##.length in
        for i = 0 to size-1 do
                with_nth_child elem i (f i)
        done

let delayed time f =
        ignore (Html.setTimeout f time)

type delay_t = {
        delay: float;
        proc: unit -> unit;
}

let run (d: delay_t) =
        delayed d.delay d.proc

let (@>>) d1 d2 = {
        delay = d1.delay;
        proc = (fun () -> d1.proc (); run d2)
}

let delay_mk time f = {
        delay = time;
        proc = f;
}

let get_board () = get_element "puzzle"

let get_pick () = get_element "pick"

let get_note () = get_element "note"

let get_counter () = get_element "counter"

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
        {text = "It is gilchtyd"; complete = (fun state cont ->
                state.board##.innerHTML := jstr {|<span class="box c">I</span><span class="box c">t</span><span class="box"> </span><span class="box c">i</span><span class="box c">s</span><span class="box"> </span><span class="box c">g</span><span class="box c">i</span><span class="box c">l</span><span class="box c">c</span><span class="box c">h</span><span class="box c">t</span><span class="box c" id="y">y</span><span class="box c">d</span>|};
                with_element "y" (fun elem ->
                        delay_mk 400.0 (fun () ->
                                addClass elem "glitch"
                        ) @>> delay_mk 600.0 (fun () ->
                                setClass state.board "burn"
                        ) @>> delay_mk 500.0 (fun () ->
                                let yidx = 12 in
                                iteri_children state.board (fun idx box ->
                                        let delay = abs (idx - yidx) in
                                        let distf = ((float_of_int idx) -. 7.5) *. 2.0 in
                                        let dist = int_of_float distf in
                                        addClass box "glitch";
                                        setStyle box ("--del: " ^ (string_of_int delay) ^ "s; --dist: " ^ (string_of_int dist) ^ "em")
                                )
                        ) @>> delay_mk 2000.0 (fun () ->
                                addClass state.board "boom"
                        ) @>> delay_mk 2000.0 cont |> run
                )
        )};
        {text = "desrever"; complete = (fun state cont ->
                iteri_children state.board (fun idx elem ->
                        let delta = (3.5 -. (float_of_int idx)) *. 2.0 |> int_of_float in
                        setStyle elem ("--d: " ^ (string_of_int delta) ^ "em")
                );
                addClass state.board "rev";
                delayed 5500.0 cont
        )};
        {text = "water earth wind fire"; complete = (fun state cont ->
                state.board##.innerHTML := jstr {|<span id="wa"><span class="box c">w</span><span class="box c">a</span><span class="box c">t</span><span class="box c">e</span><span class="box c">r</span></span><span class="box"> </span><span id="ea"><span class="box c">e</span><span class="box c">a</span><span class="box c">r</span><span class="box c">t</span><span class="box c">h</span></span><span class="box"> </span><span id="wi"><span class="box c">w</span><span class="box c">i</span><span class="box c">n</span><span class="box c">d</span></span><span class="box"> </span><span id="fi"><span class="box c">f</span><span class="box c">i</span><span class="box c">r</span><span class="box c">e</span></span>|};
                delayed 1000.0 (fun () ->
                        with_element "wa" (fun e -> addClass e "tr")
                );
                delayed 2000.0 (fun () ->
                        with_element "ea" (fun e -> addClass e "tr")
                );
                delayed 3000.0 (fun () ->
                        with_element "wi" (fun e -> addClass e "tr")
                );
                delayed 4000.0 (fun () ->
                        with_element "fi" (fun e -> addClass e "tr")
                );
                delayed 6000.0 cont
        )};
        {text = "That's all Folks!"; complete = (fun state cont ->
                with_queried_element "body" (fun body ->
                        delayed 1000.0 (fun () ->
                                addClass body "black";
                                delayed 2000.0 (fun () ->
                                        addClass body "done";
                                        delayed 5000.0 cont
                                )
                        )
                )
        )};
        {text = "Thanks for playing!"; complete = (fun _ _ ->
                ()
        )};
|]

let create_state board pick note counter =
        let n = 0 in
        let theme = themes.(n) in
        {
                n = n;
                typos = 0;
                theme = theme.text;
                complete = theme.complete;
                board = board;
                pick = pick;
                note = note;
                counter = counter;
        }

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

let handle_user_pick state letter =
        reveal_board state letter;
        if not (String.contains state.theme letter) then
                state.typos <- state.typos + 1

let update_pick pick letter =
        pick##.innerHTML := js_string_of_char letter

let update_typos counter count =
        let typos = string_of_int count in
        counter##.innerHTML := jstr typos

let theme_found state =
        let text = get_text state.board in
        Js.to_string text = state.theme

let if_theme_found state f =
        if theme_found state then f()

let last n =
        let last_idx = Array.length themes - 1 in
        last_idx = n

let end_game state =
        String.iteri (fun idx c ->
                with_nth_child state.board idx (fun box ->
                        box##.innerHTML := js_string_of_char c
                )
        ) state.theme;
        let lcg n = (3 * n) mod 19 in
        let r = ref 1 in
        let n = String.length state.theme in
        let cb = Js.wrap_callback (fun () ->
                incr r;
                let nth = lcg !r mod n in
                with_nth_child state.board nth (fun elem ->
                        delayed 250.0 (fun () -> elem##.style##.color := jstr "black");
                        delayed 1000.0 (fun () -> elem##.style##.color := jstr "transparent")
                )
        ) in
        Html.window##setInterval cb 100.0 |> ignore

let next_theme state =
        let next = state.n + 1 in
        let theme = themes.(next) in
        state.n <- next;
        state.theme <- theme.text;
        state.complete <- theme.complete;
        reset state;
        if last state.n then
                end_game state

let keypressed (state: state) ev =
        let key = ev##.key in
        let str = Js.Optdef.case key nullstr id in
        with_char_of_js_string str (fun c ->
                handle_user_pick state c;
                update_pick state.pick c;
                update_typos state.counter state.typos;
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
        let counter = get_counter () in
        let state = create_state board pick note counter in
        reset state;
        Html.document##.onkeydown := Html.handler (keypressed state);
        jfalse

let _ =
        Html.window##.onload := Html.handler load;
