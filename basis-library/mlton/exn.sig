signature MLTON_EXN =
   sig
      val history: exn -> string list
      val topLevelHandler: exn -> unit (* does not return *)
   end
