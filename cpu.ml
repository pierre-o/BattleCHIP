open Memory
open Util

(* Below are functions that implement the more complicated instructions. *)

let call_subroutine nnn =
  Stack.push !pc;
  pc := nnn

let add_vy_to_vx x y =
  let carry = v.{x} + v.{y} > 0xFF in
  v.{0xF} <- int_of_bool carry;
  v.{x} <- v.{x} + v.{y}

let shift_right x =
  let lsb = v.{x} land 0b00000001 in
  v.{0xF} <- lsb;
  v.{x} <- v.{x} >> 1

let shift_left x =
  let msb = (v.{x} land 0b10000000) >> 7 in
  v.{0xF} <- msb;
  v.{x} <- v.{x} << 1

let skip_if_pressed x =
  if Input.is_pressed v.{x} then
    skip ()

let skip_if_not_pressed x =
  if not (Input.is_pressed v.{x}) then
    skip ()

let draw_sprite x y n =
  let (x, y) = (v.{x}, v.{y}) in
  let nibble_length = n in
  let nibble = Bigarray.Array1.sub ram !i nibble_length in
  v.{0xF} <- int_of_bool (Video.draw_sprite x y nibble)

let add_vx_to_i x =
  v.{0xF} <- int_of_bool (!i + v.{x} > 0xFFF);
  i := !i + v.{x}

let store_bcd x =
  let bcd = Printf.sprintf "%03d" v.{x} in
  assert (String.length bcd = 3);
  ram.{!i + 0} <- int_of_char bcd.[0] - int_of_char '0'; (* Hundreds, *)
  ram.{!i + 1} <- int_of_char bcd.[1] - int_of_char '0'; (* tens, *)
  ram.{!i + 2} <- int_of_char bcd.[2] - int_of_char '0'; (* ones. *)
  assert (in_range ram.{!i + 0} (0, 9));
  assert (in_range ram.{!i + 1} (0, 9));
  assert (in_range ram.{!i + 2} (0, 9))

let store_registers x =
  for j = 0 to x do
    ram.{!i + j} <- v.{j}
  done

let load_registers x =
  for j = 0 to x do
    v.{j} <- ram.{!i + j}
  done

let sub x b a =
  let borrow = v.{a} > v.{b} in
  v.{0xF} <- int_of_bool (not borrow);
  v.{x} <- v.{b} - v.{a}

(** Execute opcode. It is assumed that the program counter was incremented
    beforehand. *)
let decode_execute opcode =
  let nnn = opcode land 0x0FFF in
  let kk = opcode land 0x00FF in
  let get_nibble i = (opcode >> (i * 4)) land 0x000F in
  let nibbles = Array.init 4 (fun i -> get_nibble (3 - i)) in
  match nibbles with
  | [|0x0; 0x0; 0xE; 0x0|] -> Video.clear_screen ()            (* Clear the screen.             *)
  | [|0x0; 0x0; 0xE; 0xE|] -> pc := Stack.pop ()               (* Return from subroutine.       *)
  | [|0x1;   _;   _;   _|] -> pc := nnn                        (* Jump to some adress.          *)
  | [|0x2;   _;   _;   _|] -> call_subroutine nnn              (* Call a subroutine.            *)
  | [|0x3;   x;   _;   _|] -> if v.{x} = kk then skip ()       (* Skip next instr if VX == kk.  *)
  | [|0x4;   x;   _;   _|] -> if v.{x} <> kk then skip ()      (* Skip next instr if VX != kk.  *)
  | [|0x5;   x;   y; 0x0|] -> if v.{x} = v.{y} then skip ()    (* Skip next instr if VX == VY.  *)
  | [|0x6;   x;   _;   _|] -> v.{x} <- kk                      (* Set VX to kk.                 *)
  | [|0x7;   x;   _;   _|] -> v.{x} <- v.{x} + kk              (* Add kk to VX.                 *)
  | [|0x8;   x;   y; 0x0|] -> v.{x} <- v.{y}                   (* Set VX to VY.                 *)
  | [|0x8;   x;   y; 0x1|] -> v.{x} <- v.{x} lor v.{y}         (* Set VX to VX OR VY.           *)
  | [|0x8;   x;   y; 0x2|] -> v.{x} <- v.{x} land v.{y}        (* Set VX to VX AND VY.          *)
  | [|0x8;   x;   y; 0x3|] -> v.{x} <- v.{x} lxor v.{y}        (* Set VX to VX XOR VY.          *)
  | [|0x8;   x;   y; 0x4|] -> add_vy_to_vx x y                 (* Add VY to VX.                 *)
  | [|0x8;   x;   y; 0x5|] -> sub x x y                        (* Subtract VY from VX.          *)
  | [|0x8;   x;   _; 0x6|] -> shift_right x                    (* Shift VX right by one.        *)
  | [|0x8;   x;   y; 0x7|] -> sub x y x                        (* Set VX to VY - VX.            *)
  | [|0x8;   x;   _; 0xE|] -> shift_left x                     (* Shift VX left by one.         *)
  | [|0x9;   x;   y;   _|] -> if v.{x} <> v.{y} then skip ()   (* Skip next instr if VX != VY.  *)
  | [|0xA;   _;   _;   _|] -> i := nnn                         (* Set I to nnn.                 *)
  | [|0xB;   _;   _;   _|] -> pc := nnn + v.{0x0}              (* Jump to some adress.          *)
  | [|0xC;   x;   _;   _|] -> v.{x} <- Random.int 256 land kk  (* Set VX to some random int.    *)
  | [|0xD;   x;   y;   n|] -> draw_sprite x y n                (* Draw a sprite.                *)
  | [|0xE;   x; 0x9; 0xE|] -> skip_if_pressed x                (* Skip if a key is pressed.     *)
  | [|0xE;   x; 0xA; 0x1|] -> skip_if_not_pressed x            (* Skip if a key is not pressed. *)
  | [|0xF;   x; 0x0; 0x7|] -> v.{x} <- Time.get_delay_timer () (* Set VX to delay timer.        *)
  | [|0xF;   x; 0x0; 0xA|] -> v.{x} <- Input.wait ()           (* Wait for a keypress.          *)
  | [|0xF;   x; 0x1; 0x5|] -> Time.set_delay_timer v.{x}       (* Set delay timer to VX.        *)
  | [|0xF;   x; 0x1; 0x8|] -> Audio.play_sound v.{x}           (* Set sound timer to VX.        *)
  | [|0xF;   x; 0x1; 0xE|] -> add_vx_to_i x                    (* Add VX to I.                  *)
  | [|0xF;   x; 0x2; 0x9|] -> i := char_addr v.{x}             (* Set I to sprite of char VX.   *)
  | [|0xF;   x; 0x3; 0x3|] -> store_bcd x                      (* Store BCD repr of VX.         *)
  | [|0xF;   x; 0x5; 0x5|] -> store_registers x                (* Store registers into memory.  *)
  | [|0xF;   x; 0x6; 0x5|] -> load_registers x                 (* Load registers from memory.   *)
  | _ ->                                                       (* Unknown opcode.               *)
      let pc = !pc - 2 in (* Remember that we assume that PC was incremented. *)
      prerr_endline (Printf.sprintf "Unknown opcode 0x%.4X at 0x%.3X." opcode pc);
      exit 1
