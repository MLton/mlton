(* Copyright (C) 2022 Matthew Fluet.
 * Copyright (C) 2003-2009 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure MLtonPlatform: MLTON_PLATFORM =
   struct
      local
         val toLower = CharVector.map Char.toLower
         fun peek (l, f) = List.find f l
         fun omap (opt, f) = Option.map f opt
      in
         fun fromString_toString all =
            let
               fun fromString s =
                  let
                     val s = toLower s
                  in
                     omap (peek (all, fn (_, s') => s = toLower s'), #1)
                  end
               fun toString a = #2 (valOf (peek (all, fn (a', _) => a = a')))
            in
               (fromString, toString)
            end
      end

      structure Arch =
         struct
            datatype t =
               Alpha
             | AMD64
             | ARM
             | ARM64
             | HPPA
             | IA64
             | m68k
             | MIPS
             | PowerPC
             | PowerPC64
             | RISCV
             | S390
             | Sparc
             | X86

            val all =
               (Alpha, "Alpha")::
               (AMD64, "AMD64")::
               (ARM, "ARM")::
               (ARM64, "ARM64")::
               (HPPA, "HPPA")::
               (IA64, "IA64")::
               (m68k, "m68k")::
               (MIPS, "MIPS")::
               (PowerPC, "PowerPC")::
               (PowerPC64, "PowerPC64")::
               (RISCV, "RISCV")::
               (S390, "S390")::
               (Sparc, "Sparc")::
               (X86, "X86")::
               nil

            val (fromString, toString) = fromString_toString all

            (* Can't use `fromString`, because won't constant fold. *)
            val host: t =
               case Primitive.MLton.Platform.Arch.host of
                  "alpha" => Alpha
                | "amd64" => AMD64
                | "arm" => ARM
                | "arm64" => ARM64
                | "hppa" => HPPA
                | "ia64" => IA64
                | "m68k" => m68k
                | "mips" => MIPS
                | "powerpc" => PowerPC
                | "powerpc64" => PowerPC64
                | "riscv" => RISCV
                | "s390" => S390
                | "sparc" => Sparc
                | "x86" => X86
                | _ => raise Fail "strange MLton_Platform_Arch_host"
         end

      structure Format =
         struct
            datatype t =
               Archive
             | Executable
             | LibArchive
             | Library

            val all =
               (Archive, "Archive")::
               (Executable, "Executable")::
               (LibArchive, "LibArchive")::
               (Library, "Library")::
               nil

            val (fromString, toString) = fromString_toString all

            (* Can't use `fromString`, because won't constant fold. *)
            val host: t =
               case Primitive.MLton.Platform.Format.host of
                  "archive" => Archive
                | "executable" => Executable
                | "libarchive" => LibArchive
                | "library" => Library
                | _ => raise Fail "strange MLton_Platform_Format_host"
         end

      structure OS =
         struct
            datatype t =
               AIX
             | Cygwin
             | Darwin
             | FreeBSD
             | Hurd
             | HPUX
             | Linux
             | MinGW
             | NetBSD
             | OpenBSD
             | Solaris

            val all =
               (AIX, "AIX")::
               (Cygwin, "Cygwin")::
               (Darwin, "Darwin")::
               (FreeBSD, "FreeBSD")::
               (HPUX, "HPUX")::
               (Hurd, "Hurd")::
               (Linux, "Linux")::
               (MinGW, "MinGW")::
               (NetBSD, "NetBSD")::
               (OpenBSD, "OpenBSD")::
               (Solaris, "Solaris")::
               nil

            val (fromString, toString) = fromString_toString all

            (* Can't use `fromString`, because won't constant fold. *)
            val host: t =
               case Primitive.MLton.Platform.OS.host of
                  "aix" => AIX
                | "cygwin" => Cygwin
                | "darwin" => Darwin
                | "freebsd" => FreeBSD
                | "hpux" => HPUX
                | "hurd" => Hurd
                | "linux" => Linux
                | "mingw" => MinGW
                | "netbsd" => NetBSD
                | "openbsd" => OpenBSD
                | "solaris" => Solaris
                | _ => raise Fail "strange MLton_Platform_OS_host"
         end
   end
