signature MLTON_SIGNAL =
   sig
      include POSIX_SIGNAL

      val prof: signal
      val vtalrm: signal

      structure Mask:
	 sig
	    type t
	       
	    val all: t
	    val some: signal list -> t
	    val block: t -> unit
	    val unblock: t -> unit
	    val set: t -> unit
	 end

      structure Handler:
	 sig
	    datatype t =
	       Default
	     | Ignore
	       (*
		* It is an error for a handler to raise an exception.
		* It is an error to Thread.switch' to an interrupted thread
		* with a thunk that raises an exception (either directly, or via
		* Thread.prepend).  This is to avoid the possibility of
		* aynchronous exceptions.
		*)
	     | Handler of unit Thread.t -> unit Thread.t

	    (* Get the current handler for a signal. *)
	    val get: signal -> t
	 
	    (* Set the handler for a signal. *)
	    val set: signal * t -> unit

	    (* simple f = Handler (fn t => (f (); t)) *)
	    val simple: (unit -> unit) -> t
	 end
   end
