signature MLTON_SIGNAL =
   sig
      include POSIX_SIGNAL

      type t
      sharing type t = signal

      val prof: t
      val vtalrm: t

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

      structure Handler:
	 sig
	    type t

	    val default: t
	    val handler: (unit MLtonThread.t -> unit MLtonThread.t) -> t
	    val ignore: t
	    val isDefault: t -> bool
	    val isIgnore: t -> bool
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
      val handleWith': t * (unit MLtonThread.t -> unit MLtonThread.t) -> unit
      val handleWith: t * (unit -> unit) -> unit
      val ignore: t -> unit
      val setHandler: t * Handler.t -> unit
      (* suspend m temporarily sets the signal mask to m and suspends until an
       * unmasked signal is received and handled, and then resets the mask.
       *)
      val suspend: Mask.t -> unit
   end
