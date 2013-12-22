open Util

let screen_width = 64 (* Screen width in pixels. *)
let screen_height = 32 (* Screen height in pixels. *)
let k = 10 (* Size multiplier. *)

(** SDL surface representing the screen. *)
let screen = ref None

type color =
| Black
| White

let int32_of_color = function
| Black -> Sdlvideo.map_RGB !?screen Sdlvideo.black
| White -> Sdlvideo.map_RGB !?screen Sdlvideo.white

let color_of_int32 (color : int32) =
  if color = int32_of_color Black then Black
  else if color = int32_of_color White then White
  else assert false


(** Get the color of a virtual pixel. *)
let get_pixel (x, y) : color =
  (* Check that the coords are in range. *)
  assert (in_range x (0, screen_width - 1));
  assert (in_range y (0, screen_height - 1));

  let color = Sdlvideo.get_pixel !?screen ~x:(x * k) ~y:(y * k) in

  (* Ensure that the actual pixels are all the same color. *)
  for x = x * k to (x + 1) * k - 1 do
    for y = y * k to (y + 1) * k - 1 do
      assert (Sdlvideo.get_pixel !?screen ~x ~y = color)
    done
  done;

  color_of_int32 color


(** Set the color of a virtual pixel. *)
let set_pixel (x, y) (color : color) =
  (* Check that the coords are in range. *)
  assert (in_range x (0, screen_width - 1));
  assert (in_range y (0, screen_height - 1));

  (* Color the pixel. *)
  let rect = Sdlvideo.rect ~x:(x * k) ~y:(y * k) ~w:k ~h:k in
  Sdlvideo.fill_rect ~rect !?screen (int32_of_color color);

  (* Check that the virtual pixel is indeed the right color. *)
  assert (get_pixel (x, y) = color)


(** Flip the double buffer screens. *)
let update_screen () = Sdlvideo.flip !?screen


(** Erase the screen. *)
let clear_screen () =
  Sdlvideo.fill_rect !?screen (int32_of_color Black);
  update_screen ();

  (* Check that the screen is inded totally black. *)
  for x = 0 to screen_width - 1 do
    for y = 0 to screen_height - 1 do
      assert (get_pixel (x, y) = Black)
    done
  done


(** Init the Video module. *)
let init () =
  Sdl.init_subsystem [`VIDEO];
  screen := Some (
    Sdlvideo.set_video_mode
      ~w:(screen_width * k)
      ~h:(screen_height * k)
      [`DOUBLEBUF; `HWSURFACE]
  );
  clear_screen ()


(** Draw a sprite on screen. *)
let draw_sprite x y (sprite : u8_array) =
  (* Check that the coords are in range. *)
  assert (in_range x (0, screen_width - 1));
  assert (in_range y (0, screen_height - 1));

  let sprite_width = 8 in
  let sprite_height = Bigarray.Array1.dim sprite in
  assert (in_range sprite_height (0x0, 0xF));

  let collision = ref false in
  for dy = 0 to sprite_height - 1 do
    let line = sprite.{dy} in
    for dx = 0 to sprite_width - 1 do
      let x = x + dx in
      let y = y + dy in
      if in_range x (0, screen_width - 1)
      && in_range y (0, screen_height - 1)
      && line land (0b10000000 >> dx) <> 0b00000000 then (
        (* The sprite pixel is white. *)
        match get_pixel (x, y) with
        | Black -> set_pixel (x, y) White
        | White -> set_pixel (x, y) Black; collision := true
      )
    done
  done;
  update_screen ();
  !collision
