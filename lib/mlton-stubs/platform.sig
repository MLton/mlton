signature MLTON_PLATFORM =
   sig
      structure Arch:
	 sig
	    datatype t = Sparc | X86

	    val fromString: string -> t option
	    val host: t
	    val toString: t -> string
	 end
	 
      structure OS:
	 sig
	    datatype t = Cygwin | FreeBSD | Linux | NetBSD | Solaris

	    val fromString: string -> t option
	    val host: t
	    val toString: t -> string
	 end
   end
