signature MLTON_CALL_STACK =
   sig
      type t

      val keep: bool
      val current: unit -> t
      val toStrings: t -> string list
   end
