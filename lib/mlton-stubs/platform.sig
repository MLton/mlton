signature MLTON_PLATFORM =
   sig
      structure Arch:
	 sig
	    datatype t = Sparc | X86

	    val host: t
	    val fromString: string -> t option
	    val toString: t -> string
	 end
	 
      structure OS:
	 sig
	    datatype t = Cygwin | FreeBSD | Linux | NetBSD | Solaris

	    val host: t
	    val fromString: string -> t option
	    val toString: t -> string
	 end
   end
