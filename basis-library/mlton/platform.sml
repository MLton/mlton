structure MLtonPlatform: MLTON_PLATFORM =
   struct
      open Primitive.MLton.Platform

      fun peek (l, f) = List.find f l
      fun omap (opt, f) = Option.map f opt
	 
      structure Arch =
	 struct
	    open Arch

	    val all = [(Sparc, "Sparc"), (X86, "X86")]

	    fun fromString s =
	       let
		  val s = String.toLower s
	       in
		  omap (peek (all, fn (_, s') => s = String.toLower s'), #1)
	       end

	    fun toString a = #2 (valOf (peek (all, fn (a', _) => a = a')))
	 end

      structure OS =
	 struct
	    open OS

	    val all = [(Cygwin, "Cygwin"),
		       (FreeBSD, "FreeBSD"),
		       (Linux, "Linux"),
		       (MinGW, "MinGW"),
		       (NetBSD, "NetBSD"),
		       (OpenBSD, "OpenBSD"),
		       (Solaris, "Solaris")]
	       
	    fun fromString s =
	       let
		  val s = String.toLower s
	       in
		  omap (peek (all, fn (_, s') => s = String.toLower s'), #1)
	       end

	    fun toString a = #2 (valOf (peek (all, fn (a', _) => a = a')))
	 end
   end
