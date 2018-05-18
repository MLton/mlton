(* Copyright (C) 2003-2009 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature MLTON_PLATFORM =
   sig
      structure Arch:
         sig
            datatype t = Alpha | AMD64 | ARM | ARM64 | HPPA | IA64 | m68k |
                         MIPS | PowerPC | PowerPC64 | RISCV | S390 | Sparc | X86

            val fromString: string -> t option
            val host: t
            val toString: t -> string
         end

      structure OS:
         sig
            datatype t = AIX | Cygwin | Darwin | FreeBSD | HPUX | Hurd
                       | Linux | MinGW | NetBSD | OpenBSD | Solaris

            val fromString: string -> t option
            val host: t
            val toString: t -> string
         end
   end
