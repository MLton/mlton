structure MLtonPlatform: MLTON_PLATFORM =
   struct
      open Primitive.MLton.Platform

      fun peek (l, f) = List.find f l
      fun omap (opt, f) = Option.map f opt
	 
      structure Arch =
	 struct
	    open Arch

	    val all = [(Sparc, "sparc"), (X86, "x86")]

	    fun fromString s = omap (peek (all, fn (_, s') => s = s'), #1)

	    fun toString a = #2 (valOf (peek (all, fn (a', _) => a = a')))
	 end

      structure OS =
	 struct
	    open OS

	    val all = [(Cygwin, "cygwin"),
		       (FreeBSD, "freebsd"),
		       (Linux, "linux"),
		       (NetBSD, "netbsd"),
		       (Solaris, "solaris")]
	       
	    fun fromString s = omap (peek (all, fn (_, s') => s = s'), #1)

	    fun toString a = #2 (valOf (peek (all, fn (a', _) => a = a')))
	 end
   end
