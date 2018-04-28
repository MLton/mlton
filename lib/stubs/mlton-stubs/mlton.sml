(* Copyright (C) 2013 Matthew Fluet.
 * Copyright (C) 1999-2009 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor MkIO (S : sig
                     type instream
                     type outstream
                     val openOut: string -> outstream
                  end) =
   struct
      open S

      fun inFd _ = raise Fail "IO.inFd"
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
      fun newIn _ = raise Fail "IO.newIn"
      fun newOut _ = raise Fail "IO.newOut"
      fun outFd _ = raise Fail "IO.outFd"
      fun setIn _ = raise Fail "IO.setIn"
      fun tempPrefix _ = raise Fail "IO.tempPrefix"
   end

functor MkWord(W : WORD) : MLTON_WORD =
   struct
      open W
      type t = word

      val wordSize = Word.fromInt wordSize

      val bswap = fn _ => raise Fail "Word.bswap"
      fun rol (w: word, w': Word.word): word =
         let
            val w' = Word.mod (w', wordSize)
         in
            orb (>> (w, Word.- (wordSize, w')),
                 << (w, w'))
         end
      fun ror (w: word, w': Word.word): word =
         let
            val w' = Word.mod (w', wordSize)
         in
            orb (>> (w, w'),
                 << (w, Word.- (wordSize, w')))
         end

   end

(* This file is just a dummy provided in place of the structure that MLton
 * supplies so that we can compile under SML/NJ.
 *)
structure MLton: MLTON =
   struct
      val debug = false
      val eq = fn _ => raise Fail "eq"
      val equal = fn _ => raise Fail "equal"
      val hash = fn _ => raise Fail "hash"
      val isMLton = MLton.isMLton
      val safe = true
      val share = fn _ => raise Fail "share"
      val shareAll = fn _ => raise Fail "shareAll"
      val size = MLton.size

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

      structure BinIO = MkIO (BinIO)

      structure Exn =
         struct
            val addExnMessager = fn _ => raise Fail "Exn.addExnMessager"
            val history = MLton.Exn.history

            val defaultTopLevelHandler = fn _ => raise Fail "Exn.defaultTopLevelHandler"
            val getTopLevelHandler = fn _ => raise Fail "Exn.getTopLevelHandler"
            val setTopLevelHandler = fn _ => raise Fail "Exn.setTopLevelHandler"
            val topLevelHandler = fn _ => raise Fail "Exn.topLevelHandler"
         end

      structure Finalizable =
         struct
            type 'a t = 'a

            fun addFinalizer _ = ()
            fun finalizeBefore _ = ()
            fun new x = x
            fun touch _ = ()
            fun withValue (x, f) = f x
         end

      structure GC =
         struct
            val collect = MLton.GC.collect
            val pack = MLton.GC.pack
            val setMessages = MLton.GC.setMessages
            fun setSummary _ = ()
            fun unpack _ = ()

            structure Statistics =
               struct
                  val bytesAllocated = fn _ => raise Fail "GC.Statistics.bytesAllocated"
                  val lastBytesLive = fn _ => raise Fail "GC.Statistics.lastBytesLive"
                  val numCopyingGCs = fn _ => raise Fail "GC.Statistics.numCopyingGCs"
                  val numMarkCompactGCs = fn _ => raise Fail "GC.Statistics.numMarkCompactGCs"
                  val numMinorGCs = fn _ => raise Fail "GC.Statistics.numMinorGCs"
                  val maxBytesLive = fn _ => raise Fail "GC.Statistics.maxBytesLive"
               end
         end

      structure Itimer =
         struct
            datatype t = Prof | Real | Virtual

            fun signal _ = Posix.Signal.alrm
            fun set _ = raise Fail "Itimer.set"
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

      structure ProcEnv =
         struct
            type gid = Posix.ProcEnv.gid

            fun setenv _ = raise Fail "setenv"
            fun setgroups _ = raise Fail "setgroups"
         end

      structure Process =
         struct
            type ('stdin, 'stdout, 'stderr) t = unit
            type input = unit
            type output = unit
            type none = unit
            type chain = unit
            type any = unit

            exception MisuseOfForget
            exception DoublyRedirected

            structure Child =
               struct
                  type ('use, 'dir) t = unit

                  val binIn = fn _ => raise Fail "Child.binIn"
                  val binOut = fn _ => raise Fail "Child.binOut"
                  val fd = fn _ => raise Fail "Child.fd"
                  val remember = fn _ => raise Fail "Child.remember"
                  val textIn = fn _ => raise Fail "Child.textIn"
                  val textOut = fn _ => raise Fail "Child.textOut"
               end

            structure Param =
               struct
                  type ('use, 'dir) t = unit

                  val child = fn _ => raise Fail "Param.child"
                  val fd = fn _ => raise Fail "Param.fd"
                  val file = fn _ => raise Fail "Param.file"
                  val forget = fn _ => raise Fail "Param.forget"
                  val null = ()
                  val pipe = ()
                  val self = ()
               end

            val create = fn _ => raise Fail "Process.create"
            val getStderr = fn _ => raise Fail "Process.getStderr"
            val getStdin  = fn _ => raise Fail "Process.getStdin"
            val getStdout = fn _ => raise Fail "Process.getStdout"
            val kill = fn _ => raise Fail "Process.kill"
            val reap = fn _ => raise Fail "Process.reap"

            type pid = Posix.Process.pid

            val atExit = OS.Process.atExit

            fun exit n =
               let
                  open OS.Process
               in
                  exit (if n = 0 then success else failure)
               end

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

      structure Signal =
         struct
            open Posix.Signal

            type t = signal

            val prof = alrm
            val vtalrm = alrm

            structure Handler =
               struct
                  type t = unit

                  val default = ()
                  val handler = fn _ => ()
                  val ignore = ()
                  val isDefault = fn _ => raise Fail "Signal.Handler.isDefault"
                  val isIgnore = fn _ => raise Fail "Signal.Handler.isIgnore"
                  fun simple _ = ()
               end

            structure Mask =
               struct
                  type t = unit

                  val all = ()
                  fun allBut _ = ()
                  fun block _ = raise Fail "Signal.Mask.block"
                  fun getBlocked _ = ()
                  fun isMember _ = raise Fail "Signal.Mask.isMember"
                  val none = ()
                  fun setBlocked _ = raise Fail "Signal.Mask.setBlocked"
                  fun some _ = ()
                  fun unblock _ = raise Fail "Signal.Mask.unblock"
               end

            fun getHandler _ = raise Fail "Signal.getHandler"
            fun handled _ = raise Fail "Signal.handled"
            val restart = ref true
            fun setHandler _ = raise Fail "Signal.setHandler"
            fun suspend _ = raise Fail "Signal.suspend"
         end

      structure TextIO = MkIO (TextIO)

      structure Thread = MLtonThread

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

      structure Weak =
         struct
            type 'a t = 'a

            val get = SOME
            fun new x = x
         end

      structure World =
         struct
            datatype status = Clone | Original
            fun load _ = raise Fail "World.load"
            fun save _ = raise Fail "World.save"
            fun saveThread _ = raise Fail "World.saveThread"
         end

      structure Word = MkWord(Word)
      structure Word8 = MkWord(Word8)
   end
