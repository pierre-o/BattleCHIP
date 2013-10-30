open Bigarray

let (<<) = (lsl)
let (>>) = (lsr)
let int_of_bool x = if x then 1 else 0
let in_range n (a, b) = a <= n && n <= b

let (!?) x =
  match !x with
  | None -> assert false
  | Some x -> x

(** Unsigned byte array type. *)
type u8_array = (int, int8_unsigned_elt, c_layout) Array1.t

(** Create a zero-initialized unsigned byte array of size n. *)
let u8_array (n : int) : u8_array =
  let a = Array1.create int8_unsigned c_layout n in
  Array1.fill a 0x00;
  a
