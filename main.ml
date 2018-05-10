let () =
  (* Initialization of the SDL. *)
  Sdl.init [];
  at_exit Sdl.quit;

  (* Initialization of the Audio and Video modules. *)
  Video.init ();
  Audio.init ();

  (* If there's not exactly one command line argument, error. *)
  if Array.length Sys.argv - 1 <> 1 then (
    prerr_endline "Usage: ./battlechip path/to/rom";
    exit 1
  );

  (* We load the ROM into memory. *)
  let rom = Sys.argv.(1) in
  Memory.init rom;

  (* Setting the window title. *)
  let title = Filename.basename rom ^ " < BattleCHIP" in
  Sdlwm.set_caption ~title ~icon:"";

  (* Emulation loop. *)
  while true do
    Time.begin_cycle (); (* Begin timing cycle. *)
    Memory.sanity_check (); (* Ensure that the memory content makes sense. *)

    let opcode = Memory.fetch () in (* Fetch the next opcode. *)
    Memory.skip (); (* Increment the program counter. *)
    (* Note that we increment the PC before we execute the opcode. *)
    Cpu.decode_execute opcode; (* Execute the opcode. *)

    Input.process_events (); (* Process window events. *)
    Time.end_cycle () (* Wait until cycle is over. *)
  done
