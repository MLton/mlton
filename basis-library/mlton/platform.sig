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
            datatype t = Alpha | AMD64 | ARM | ARM64 | HPPA | IA64 | LoongArch64 | m68k |
                         MIPS | PowerPC | PowerPC64 | RISCV | S390 | Sparc | Wasm32 | X86

            val fromString: string -> t option
            val host: t
            val toString: t -> string
         end

      structure Format:
         sig
            datatype t = Archive | Executable | LibArchive | Library

            val fromString: string -> t option
            val host: t
            val toString: t -> string
         end

      structure OS:
         sig
            datatype t = AIX | Cygwin | Darwin | FreeBSD | Hurd | HPUX
                       | Linux | MinGW | NetBSD | OpenBSD | Solaris | WASI

            val fromString: string -> t option
            val host: t
            val toString: t -> string
         end
   end
