(* Copyright (C) 2003-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

signature MLTON_PLATFORM =
   sig
      structure Arch:
	 sig
            datatype t = Alpha | AMD64 | ARM | HPPA | IA64 | m68k |
                         MIPS | PowerPC | S390 | Sparc | X86

	    val fromString: string -> t option
	    val host: t
	    val toString: t -> string
	 end
	 
      structure OS:
	 sig
	    datatype t =
	       Cygwin
	     | Darwin
	     | FreeBSD
	     | Linux
	     | MinGW
	     | NetBSD
	     | OpenBSD
	     | Solaris

	    val fromString: string -> t option
	    val host: t
	    val toString: t -> string
	 end
   end
