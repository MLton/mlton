(* Copyright (C) 2013,2019,2022 Matthew Fluet.
 * Copyright (C) 1999-2009 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor MkIO (S : sig
                     type outstream
                     val openOut: string -> outstream
                  end) =
   struct
      open S

      fun mkstemps {prefix, suffix} =
         let
            val name = concat [prefix, MLtonRandom.alphaNumString 6, suffix]
         in
            (* Make sure the temporary file name doesn't already exist. *)
            if OS.FileSys.access (name, [])
                then mkstemps {prefix = prefix, suffix = suffix}
                else (name, openOut name)
         end
      fun mkstemp s = mkstemps {prefix = s, suffix = ""}
      fun tempPrefix _ = raise Fail "IO.tempPrefix"
   end

(* This file is just a dummy provided in place of the structure that MLton
 * supplies so that we can compile under non-MLton SML compilers.
 *)
structure MLton: MLTON =
   struct
      val debug = false
      val eq = fn _ => raise Fail "MLton.eq"
      val equal = fn _ => raise Fail "MLton.equal"
      val hash = fn _ => raise Fail "MLton.hash"
      val isMLton = MLton.isMLton
      val safe = true
      val share = fn _ => raise Fail "MLton.share"
      val shareAll = fn _ => raise Fail "MLton.shareAll"
      val size: 'a -> IntInf.int = fn _ => ~1
      val sizeAll: 'a -> IntInf.int = fn _ => ~1

      structure Array =
         struct
            open Array

            fun unfoldi (n, a, f) =
               let
                  val r = ref a
                  val a =
                     tabulate (n, fn i =>
                               let
                                  val (b, a') = f (i, !r)
                                  val _ = r := a'
                               in
                                  b
                               end)
               in
                  (a, !r)
               end
         end

      structure Exn =
         struct
            val history = MLton.Exn.history
         end

      structure GC =
         struct
            val collect = MLton.GC.collect
            val pack = MLton.GC.pack
         end

      structure Platform =
         struct
            fun peek (l, f) = List.find f l
            fun omap (opt, f) = Option.map f opt

            structure String =
               struct
                  open String

                  val toLower = CharVector.map Char.toLower
               end

            structure Arch =
               struct
                  datatype t = Alpha | AMD64 | ARM | ARM64 | HPPA | IA64 |
                               m68k | MIPS | PowerPC | PowerPC64 | RISCV |
                               S390 | Sparc | X86

                  val all = [(Alpha, "Alpha"),
                             (AMD64, "AMD64"),
                             (ARM, "ARM"),
                             (ARM64, "ARM64"),
                             (HPPA, "HPPA"),
                             (IA64, "IA64"),
                             (m68k, "m68k"),
                             (MIPS, "MIPS"),
                             (PowerPC, "PowerPC"),
                             (PowerPC64, "PowerPC64"),
                             (RISCV, "RISCV"),
                             (S390, "S390"),
                             (Sparc, "Sparc"),
                             (X86, "X86")]

                  fun fromString s =
                     let
                        val s = String.toLower s
                     in
                        omap (peek (all, fn (_, s') => s = String.toLower s'),
                              #1)
                     end

                  fun toString a = #2 (valOf (peek (all, fn (a', _) => a = a')))

                  val host: t =
                     case fromString (MLton.Platform.Arch.toString MLton.Platform.Arch.host) of
                        NONE => raise Fail "MLton.Platform.Arch.host: strange arch"
                      | SOME host => host
               end

            structure OS =
               struct
                  datatype t =
                     AIX
                   | Cygwin
                   | Darwin
                   | FreeBSD
                   | HPUX
                   | Hurd
                   | Linux
                   | MinGW
                   | NetBSD
                   | OpenBSD
                   | Solaris

                  val all = [(AIX, "AIX"),
                             (Cygwin, "Cygwin"),
                             (Darwin, "Darwin"),
                             (FreeBSD, "FreeBSD"),
                             (HPUX, "HPUX"),
                             (Hurd, "Hurd"),
                             (Linux, "Linux"),
                             (MinGW, "MinGW"),
                             (NetBSD, "NetBSD"),
                             (OpenBSD, "OpenBSD"),
                             (Solaris, "Solaris")]

                  fun fromString s =
                     let
                        val s = String.toLower s
                     in
                        omap (peek (all, fn (_, s') => s = String.toLower s'),
                              #1)
                     end

                  fun toString a = #2 (valOf (peek (all, fn (a', _) => a = a')))

                  val host: t =
                     case fromString (MLton.Platform.OS.toString MLton.Platform.OS.host) of
                        NONE => raise Fail "MLton.Platform.OS.host: strange os"
                      | SOME os => os
               end
         end

      structure Process =
         struct
            type pid = Posix.Process.pid

            fun spawne {path, args, env} =
               case Posix.Process.fork () of
                  NONE => Posix.Process.exece (path, args, env)
                | SOME pid => pid

            fun spawn {path, args} =
               spawne {path = path, args = args, env = Posix.ProcEnv.environ ()}

            fun spawnp {file, args} =
               case Posix.Process.fork () of
                  NONE => Posix.Process.execp (file, args)
                | SOME pid => pid
         end

      structure Profile =
         struct
            structure Data =
               struct
                  type t = unit

                  val equals = fn _ => raise Fail "Profile.Data.equals"
                  val free = fn _ => raise Fail "Profile.Data.free"
                  val malloc = fn _ => raise Fail "Profile.Data.malloc"
                  val write = fn _ => raise Fail "Profile.Data.write"
               end
            val isOn = false
            val withData = fn _ => raise Fail "Profile.withData"
         end

      structure Random = MLtonRandom

      structure Rusage =
         struct
           type t = {stime: Time.time, utime: Time.time}

           fun measureGC _ = ()

           (* Fake it with Posix.ProcEnv.times
            * and Timer.totalCPUTimer and Timer.checkCPUTimes.
            *)
           fun rusage () =
              let
                 val zero = {utime = Time.zeroTime, stime = Time.zeroTime}
              in
                 let
                    val {gc = {usr = gcutime, sys = gcstime}, ...} =
                       Timer.checkCPUTimes (Timer.totalCPUTimer ())
                    val {utime, stime, cutime, cstime, ...} =
                       Posix.ProcEnv.times ()
                 in
                    {self = {utime = utime, stime = stime},
                     children = {utime = cutime, stime = cstime},
                     gc = {utime = gcutime, stime = gcstime}}
                 end handle Time => {children = zero, gc = zero, self = zero}
                 (* The handle Time is there because of a bug in SML/NJ that
                  * causes a Time exception to be raised on machines with a
                  * large uptime (enough that the number of clock ticks is
                  * >= 2^31).
                  *)
              end
         end

      structure TextIO = MkIO (TextIO)

      structure Vector =
         struct
            open Vector

            fun create n =
               let
                  val r = ref (Array.fromList [])
                  val subLim = ref 0
                  fun sub i =
                     if 0 <= i andalso i < !subLim
                        then Array.sub (!r, i)
                     else raise Subscript
                  val updateLim = ref 0
                  fun update (i, x) =
                     if 0 <= i andalso i < !updateLim
                        then if i = !updateLim andalso i < n
                                then (r := (Array.tabulate (i + 1, fn j =>
                                                            if i = j
                                                               then x
                                                            else Array.sub (!r, j)));
                                      subLim := i + 1;
                                      updateLim := i + 1)
                             else raise Subscript
                     else
                        Array.update (!r, i, x)
                  val gotIt = ref false
                  fun done () =
                     if !gotIt then
                        raise Fail "already got vector"
                     else
                        if n = !updateLim then
                           (gotIt := true;
                            updateLim := 0;
                            Array.vector (!r))
                        else
                           raise Fail "vector not full"
               in
                  {done = done,
                   sub = sub,
                   update = update}
               end

            fun unfoldi (n, a, f) =
               let
                  val r = ref a
                  val v =
                     tabulate (n, fn i =>
                               let
                                  val (b, a') = f (i, !r)
                                  val _ = r := a'
                               in
                                  b
                               end)
               in
                  (v, !r)
               end
         end
   end
