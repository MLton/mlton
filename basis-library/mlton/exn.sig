signature MLTON_EXN =
   sig
      val history: exn -> string list
      val topLevelHandler: exn -> 'a (* does not return *)
   end
