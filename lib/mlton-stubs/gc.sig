signature MLTON_GC =
   sig
      val collect: unit -> unit
      val setMessages: bool -> unit
      val setSummary: bool -> unit
   end
