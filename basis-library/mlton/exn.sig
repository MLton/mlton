signature MLTON_EXN =
   sig
      val addExnMessager: (exn -> string option) -> unit
      val history: exn -> string list
      val topLevelHandler: exn -> 'a (* does not return *)
   end
