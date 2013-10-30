open Util

let beep = ref None (** The beep sound. *)


(** Initialize the Audio module. *)
let init () =
  (* Open Sdlmixer. *)
  Sdlmixer.open_audio ();
  at_exit Sdlmixer.close_audio;

  (* Load the beep sound. *)
  beep := Some (Sdlmixer.loadWAV "beep.wav");
  at_exit (fun () -> Sdlmixer.free_chunk !?beep);

  (* Allocate a channel for the beep sound. *)
  let _ = Sdlmixer.allocate_channels 1 in
  assert (Sdlmixer.num_channels () = 1)


(** Play sound for (sound_timer / 60) seconds. This function is non-blocking.
    If the function is called again before a previous call is finished playing,
    the previous call is aborted and the latest call continues the beeping. *)
let play_sound sound_timer =
  assert (in_range sound_timer (0x00, 0xFF));

  Sdlmixer.play_channel
    ~channel:0 (* The first and only channel. *)
    ~loops:(-1) (* Repeat the sound indefinitely. *)
    ~ticks:(float_of_int sound_timer *. (1. /. 60.)) (* Sound duration in ms. *)
    !?beep (* The beep sound. *)
