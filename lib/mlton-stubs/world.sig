signature MLTON_WORLD =
   sig
      val load: string -> 'a
      (* Save the world to resume with the current thread. *)
      datatype status = Clone | Original
      val save: string -> status
      (* Save the world to resume with the given thread. *)
      val saveThread: string * unit MLtonThread.t -> unit
   end
