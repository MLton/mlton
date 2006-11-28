(* Copyright (C) 2003-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure MLtonPlatform: MLTON_PLATFORM =
   struct
      open Primitive.MLton.Platform

      fun peek (l, f) = List.find f l
      fun omap (opt, f) = Option.map f opt

      structure Arch =
         struct
            open Arch

            val all = [
                (Alpha, "Alpha"),
                (AMD64, "AMD64"),
                (ARM, "ARM"),
                (HPPA, "HPPA"),
                (IA64, "IA64"),
                (m68k, "m68k"),
                (MIPS, "MIPS"),
                (PowerPC, "PowerPC"), 
                (S390, "S390"),
                (Sparc, "Sparc"), 
                (X86, "X86")]

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

            val all = [(AIX, "AIX"),
                       (Cygwin, "Cygwin"),
                       (Darwin, "Darwin"),
                       (FreeBSD, "FreeBSD"),
                       (HPUX, "HPUX"),
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
