signature MLTON_SIGNAL =
   sig
      type t
      type signal = t

      val prof: t
      val vtalrm: t

      structure Handler:
	 sig
	    type t

	    val default: t
	    val handler: (unit MLtonThread.t -> unit MLtonThread.t) -> t
	    val ignore: t
	    val isDefault: t -> bool
	    val isIgnore: t -> bool
	 end

      structure Mask:
	 sig
	    type t
	       
	    val all: t
	    val allBut: signal list -> t
	    val block: t -> unit
	    val none: t
	    val set: t -> unit
	    val some: signal list -> t
	    val unblock: t -> unit
	 end

      val getHandler: t -> Handler.t
      val handleDefault: t -> unit
      (*
       * It is an error for a handler to raise an exception.
       * It is an error to Thread.switch' to an interrupted thread
       * with a thunk that raises an exception (either directly, or via
       * Thread.prepend).  This is to avoid the possibility of
       * aynchronous exceptions.
       *)
      val handleWith: t * (unit -> unit) -> unit
      val handleWith': t * (unit MLtonThread.t -> unit MLtonThread.t) -> unit
      val ignore: t -> unit
      val isHandledDefault: t -> bool
      val isIgnored: t -> bool
      val setHandler: t * Handler.t -> unit
      (* suspend m temporarily sets the signal mask to m and suspends until an
       * unmasked signal is received and handled, and then resets the mask.
       *)
      val suspend: Mask.t -> unit
   end

signature MLTON_SIGNAL_EXTRA =
   sig
      include MLTON_SIGNAL

      val handleGC: (unit -> unit) -> unit
   end
