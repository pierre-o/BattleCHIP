open Util

(** Main memory; the RAM. *)
let (ram : u8_array) = u8_array (0xFFF + 1)

(** General purpose 8-bit registers. VF is the carry flag. *)
let (v : u8_array) = u8_array (0xF + 1)

(** 16-bit adress register. *)
let i = ref 0x000

(** The program counter. *)
let pc = ref 0x200

(** The stack. *)
module Stack : sig
  val push : int -> unit
  val pop : unit -> int
  val clear : unit -> unit
end = struct
  let stack = Stack.create ()
  let push x = Stack.push x stack
  let pop () = Stack.pop stack
  let clear () = Stack.clear stack
end

(** Check that all memory values make sense. It checks ranges. *)
let sanity_check () =
  assert (in_range !pc (0x200, 0xFFF));
  assert (in_range !i (0x000, 0xFFF))

(** Increment the program counter. *)
let skip () = pc := !pc + 2

(** Fetch opcode. It does not modify the program counter. *)
let fetch () =
  let msb = ram.{!pc} in
  let lsb = ram.{!pc + 1} in
  (msb lsl 8) lor lsb

(** Return the location in memory of a given character's font sprite. *)
let char_addr n =
  assert (in_range n (0x0, 0xF));
  5 * n


(** Load the font and ROM into memory. *)
let init (rom : string) =
  (* Initialize memory values. *)
  Bigarray.Array1.fill ram 0x00;
  Bigarray.Array1.fill v 0x00;
  i := 0x000;
  pc := 0x200;
  Stack.clear ();

  (* Load the font into RAM. *)
  for n = 0x0 to 0xF do
    let sprite = Font.sprite.(n) in
    let sprite_height = Array.length sprite in
    assert (sprite_height = 5);
    for i = 0 to sprite_height - 1 do
      assert (in_range sprite.(i) (0b0000, 0b1111));
      let row = sprite.(i) lsl 4 in

      let i' = char_addr n + i in
      assert (in_range i' (0x000, 0x200 - 1));
      ram.{i'} <- row
    done
  done;

  (* Load the ROM into RAM. *)
  let rom = open_in_bin rom in
  let rom_length = in_channel_length rom in
  if rom_length > 0xFFF - 0x200 + 1 then (
    prerr_endline "The given file is too big to be a CHIP-8 ROM.";
    exit 1
  );
  for i = 0 to rom_length - 1 do
    ram.{0x200 + i} <- input_byte rom
  done;
  close_in rom
