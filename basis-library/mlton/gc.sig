signature MLTON_GC =
   sig
      val collect: unit -> unit
      val pack: unit -> unit
      val setMessages: bool -> unit
      val setSummary: bool -> unit
      val unpack: unit -> unit
   end
