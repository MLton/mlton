signature MLTON_WORLD =
   sig
      datatype status = Clone | Original

      val load: string -> 'a
      (* Save the world to resume with the current thread. *)
      val save: string -> status
      (* Save the world to resume with the given thread. *)
      val saveThread: string * unit Thread.t -> unit
   end
