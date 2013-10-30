open Memory
open Util

(** The current cycle's beginning time. *)
let cycle_start = ref 0

(** A cycle's length in ms. *)
let cycle_length = 2

(** Mark the beginning of a cycle. *)
let begin_cycle () = cycle_start := Sdltimer.get_ticks ()

(** Wait until the cycle is over. *)
let end_cycle () =
  while Sdltimer.get_ticks () - !cycle_start < cycle_length do
    () (* Busy waiting because Sdltimer.delay has unacceptable granularity. *)
  done

(** The delay timer. It is decremented 60 times per second. *)
let delay_timer = ref 0x00

(** The last time the delay timer was set. *)
let last_update = ref 0

(** Calculate the current value of the delay timer. *)
let get_delay_timer () =
  let num_decrements = (Sdltimer.get_ticks () - !last_update) / (1000 / 60) in
  let delay_timer' = max 0 (!delay_timer - num_decrements) in
  assert (in_range delay_timer' (0x00, 0xFF));
  delay_timer'

(** Set the value of the delay timer. *)
let set_delay_timer p_delay_timer =
  assert (in_range p_delay_timer (0x00, 0xFF));
  delay_timer := p_delay_timer;
  last_update := Sdltimer.get_ticks ()
