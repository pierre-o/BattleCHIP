open Util

(** Bindings between the CHIP-8 keys and the actual keyboard keys. *)
let bindings = [
  (0x0, Sdlkey.KEY_KP0);
  (0x1, Sdlkey.KEY_KP7);
  (0x2, Sdlkey.KEY_KP8);
  (0x3, Sdlkey.KEY_KP9);
  (0x4, Sdlkey.KEY_KP4);
  (0x5, Sdlkey.KEY_KP5);
  (0x6, Sdlkey.KEY_KP6);
  (0x7, Sdlkey.KEY_KP1);
  (0x8, Sdlkey.KEY_KP2);
  (0x9, Sdlkey.KEY_KP3);
  (0xA, Sdlkey.KEY_KP_DIVIDE);
  (0xB, Sdlkey.KEY_KP_MULTIPLY);
  (0xC, Sdlkey.KEY_KP_MINUS);
  (0xD, Sdlkey.KEY_KP_PLUS);
  (0xE, Sdlkey.KEY_KP_ENTER);
  (0xF, Sdlkey.KEY_KP_PERIOD);
]

(** Find the keyboard key associated to a given CHIP-8 key. *)
let keysym_of_int n =
  assert (in_range n (0x0, 0xF));
  List.assoc n bindings

(** Find the CHIP-8 key associated to a given keyboard key. *)
let int_of_keysym keysym =
  let (n, _) = List.find (fun (_, k) -> k = keysym) bindings in
  n

(** Process the events in the event queue until there are no more. *)
let rec process_events () =
  match Sdlevent.poll () with
  | None -> () (* If no event, do nothing. *)
  | Some Sdlevent.QUIT -> exit 0 (* If quit event, close the program. *)
  | Some _ -> process_events () (* If some random event, ignore and continue. *)

(** Return true if a given key is pressed, false if not. *)
let is_pressed (n : int) : bool =
  assert (in_range n (0x0, 0xF));
  Sdlkey.is_key_pressed (keysym_of_int n)

(** Wait for a keypress to happen and return the key. It disregards presses of
    keys not in the bindings. *)
let rec wait () : int =
  match Sdlevent.wait_event () with
  | Sdlevent.QUIT -> exit 0
  | Sdlevent.KEYDOWN keyboard_event -> begin
      (* If a key was pressed, look that it is one of the keys in the bindings.
         If so, return the key, if not, wait some more. *)
      try int_of_keysym keyboard_event.Sdlevent.keysym
      with Not_found -> wait ()
    end
  | _ -> wait () (* If some misc event happened, wait some more. *)
