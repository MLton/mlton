(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor IO (S : sig
                   type instream
                   type outstream
                   val openOut: string -> outstream
                end) =
   struct
      open S

      fun inFd _ = raise Fail "inFd"
      fun mkstemps {prefix, suffix} =
         let
            val name = concat [prefix, Random.alphaNumString 6, suffix]
         in (name, openOut name)
         end
      fun mkstemp s = mkstemps {prefix = s, suffix = ""}
      fun newIn _ = raise Fail "newIn"
      fun newOut _ = raise Fail "newOut"
      fun outFd _ = raise Fail "outFd"
      fun setIn _ = raise Fail "setIn"
      fun tempPrefix _ = raise Fail "tempPrefix"
   end

(* This file is just a dummy provided in place of the structure that MLton
 * supplies so that we can compile under SML/NJ.
 *) 
structure MLton: MLTON =
   struct
      type int = Int.int
      type word = Word.word

      type pointer = Word32.word

      val cleanAtExit = fn _ => raise Fail "cleanAtExit"
      val debug = false
      val deserialize = fn _ => raise Fail "deserialize"
      val eq = fn _ => false
      val errno = fn _ => raise Fail "errno"
      (* Using Array.maxLen will make isMLton true when being compiled by MLton
       * and false when being compiled by SML/NJ.
       *)
      val isMLton = Array.maxLen = 0x7FFFFFFF
      val safe = true
      val serialize = fn _ => raise Fail "serialize"
      val share = fn _ => raise Fail "share"
      val shareAll = fn _ => raise Fail "shareAll"
      val size = fn _ => ~1: int

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

      structure BinIO =
         struct
            type instream = unit
            type outstream = unit

            fun inFd _ = raise Fail "inFd"
            fun mkstemps _ = raise Fail "mkstemps"
            fun mkstemp _ = raise Fail "mkstemp"
            fun newIn _ = raise Fail "newIn"
            fun newOut _ = raise Fail "newOut"
            fun outFd _ = raise Fail "outFd"
            fun setIn _ = raise Fail "setIn"
            fun tempPrefix _ = raise Fail "tempPrefix"
         end

      structure CallStack =
         struct
            type t = unit

            val keep = false
            fun current () = ()
            fun toStrings () = []
         end

      structure Cont =
         struct
            type 'a t = unit

            val callcc = fn _ => raise Fail "Cont.callcc"
            val prepend = fn _ => raise Fail "Cont.prepend"
            val throw = fn _ => raise Fail "Cont.throw"
            val throw' = fn _ => raise Fail "Cont.throw'"
         end

      structure Exn =
         struct
            val history = fn _ => []

            val addExnMessager = fn _ => raise Fail "Exn.addExnMessager"
            val topLevelHandler = fn _ => raise Fail "Exn.topLevelHandler"
         end

      structure FFI =
         struct
            val handleCallFromC = fn _ => raise Fail "FFI.handleCallFromC"
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
            fun collect _ = ()
            val pack = MLton.GC.pack
            fun setMessages _ = ()
            fun setSummary _ = ()
            fun time _ = Time.zeroTime
            fun unpack _ = ()
         end

      structure IntInf =
         struct
            open IntInf

            type t = IntInf.int

            datatype rep =
               Big of Word.word Vector.vector
             | Small of Int.int

            val areSmall =
               fn _ => raise Fail "MLton.IntInf.areSmall unimplemented"
            val gcd = fn _ => raise Fail "MLton.IntInf.gcd unimplemented"
            val isSmall = fn _ => raise Fail "MLton.IntInf.isSmall unimplemented"
            val rep = fn _ => raise Fail "MLton.IntInf.rep unimplemented"
            val size = fn _ => raise Fail "MLton.IntInf.size unimplemented"
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

                  val toLower = translate (str o Char.toLower)
               end

            structure Arch =
               struct
                  datatype t = Alpha | AMD64 | ARM | HPPA | IA64 | m68k |
                               MIPS | PowerPC | S390 | Sparc | X86

                  val host: t = X86

                  val all = [(Alpha, "Alpha"),
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
                        omap (peek (all, fn (_, s') => s = String.toLower s'),
                              #1)
                     end

                  fun toString a = #2 (valOf (peek (all, fn (a', _) => a = a')))
               end

            structure OS =
               struct
                  datatype t =
                     AIX
                   | Cygwin
                   | Darwin
                   | FreeBSD
                   | HPUX
                   | Linux
                   | MinGW
                   | NetBSD
                   | OpenBSD
                   | Solaris

                  val host: t = Linux

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
                        omap (peek (all, fn (_, s') => s = String.toLower s'),
                              #1)
                     end

                  fun toString a = #2 (valOf (peek (all, fn (a', _) => a = a')))
               end
         end

      structure Pointer =
         struct
            type t = unit

            val add = fn _ => raise Fail "Pointer.add"
            val compare = fn _ => raise Fail "Pointer.compare"
            val diff = fn _ => raise Fail "Pointer.diff"
            val free = fn _ => raise Fail "Pointer.free"
            val getInt8 = fn _ => raise Fail "Pointer.getInt8"
            val getInt16 = fn _ => raise Fail "Pointer.getInt16"
            val getInt32 = fn _ => raise Fail "Pointer.getInt32"
            val getInt64 = fn _ => raise Fail "Pointer.getInt64"
            val getPointer = fn _ => raise Fail "Pointer.getPointer"
            val getReal32 = fn _ => raise Fail "Pointer.getReal32"
            val getReal64 = fn _ => raise Fail "Pointer.getReal64"
            val getWord8 = fn _ => raise Fail "Pointer.getWord8"
            val getWord16 = fn _ => raise Fail "Pointer.getWord16"
            val getWord32 = fn _ => raise Fail "Pointer.getWord32"
            val getWord64 = fn _ => raise Fail "Pointer.getWord64"
            val isNull = fn _ => raise Fail "Pointer.isNull"
            val null = ()
            val setInt8 = fn _ => raise Fail "Pointer.setInt8"
            val setInt16 = fn _ => raise Fail "Pointer.setInt16"
            val setInt32 = fn _ => raise Fail "Pointer.setInt32"
            val setInt64 = fn _ => raise Fail "Pointer.setInt64"
            val setPointer = fn _ => raise Fail "Pointer.setPointer"
            val setReal32 = fn _ => raise Fail "Pointer.setReal32"
            val setReal64 = fn _ => raise Fail "Pointer.setReal64"
            val setWord8 = fn _ => raise Fail "Pointer.setWord8"
            val setWord16 = fn _ => raise Fail "Pointer.setWord16"
            val setWord32 = fn _ => raise Fail "Pointer.setWord32"
            val setWord64 = fn _ => raise Fail "Pointer.setWord64"
            val sub = fn _ => raise Fail "Pointer.sub"
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
            val profile = false

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

      structure Ptrace =
         struct
            type pid = Posix.Process.pid
            fun attach _ = raise Fail "attach"
            fun cont _ = raise Fail "cont"
            fun detach _ = raise Fail "detach"
            fun kill _ = raise Fail "kill"
            fun peekText _ = raise Fail "peekText"
            fun singleStep _ = raise Fail "singleStep"
            fun sysCall _ = raise Fail "sysCall"
         end

      structure Random = Random

      structure Rlimit =
         struct
            structure RLim =
               struct
                  type t = SysWord.word
                  val castFromSysWord = fn w => w
                  val castToSysWord = fn w => w
               end

            val infinity: RLim.t = 0w0

            type t = int

            val coreFileSize: t = 0
            val cpuTime: t = 0
            val dataSize: t = 0
            val fileSize: t = 0
            val numFiles: t = 0
            val stackSize: t = 0
            val virtualMemorySize: t = 0

(* NOT STANDARD
            val lockedInMemorySize: t = 0
            val numProcesses: t = 0
            val residentSetSize: t = 0
*)

            fun get _ = raise Fail "Rlimit.get"
            fun set _ = raise Fail "Rlimit.set"
         end

      structure Rusage =
         struct
           type t = {stime: Time.time, utime: Time.time}

           fun measureGC _ = ()

           (* Fake it with Posix.ProcEnv.times *)
           fun rusage () =
              let
                 val zero = {utime = Time.zeroTime, stime = Time.zeroTime}
              in
                 let
                    val {utime, stime, cutime, cstime, ...} =
                       Posix.ProcEnv.times ()
                 in
                    {self = {utime = utime, stime = stime},
                     children = {utime = cutime, stime = cstime},
                     gc = zero}
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
                  val isDefault = fn _ => raise Fail "isDefault"
                  val isIgnore = fn _ => raise Fail "isIgnore"
                  fun simple _ = ()
               end

            structure Mask =
               struct
                  type t = unit

                  val all = ()
                  fun allBut _ = ()
                  fun block _ = raise Fail "block"
                  fun getBlocked _ = ()
                  fun isMember _ = raise Fail "isMember"
                  val none = ()
                  fun setBlocked _ = raise Fail "setBlocked"
                  fun some _ = ()
                  fun unblock _ = raise Fail "unblock"
               end

            fun getHandler _ = raise Fail "getHandler"
            fun handled _ = raise Fail "handled"
            val restart = ref true
            fun setHandler _ = raise Fail "setHandler"
            fun suspend _ = raise Fail "suspend"
         end

      structure Socket =
         struct
            structure Address =
               struct
                  type t = word
               end

            structure Ctl =
               struct
                  fun getERROR _ = NONE
               end

            structure Host =
               struct
                  type t = {name: string}

                  fun getByAddress _ = raise Fail "Socket.Host.getByAddress"
                  fun getByName _ = raise Fail "Socket.Host.getByName"
               end

            structure Port =
               struct
                  type t = int
               end

            type t = unit

            fun accept _ = raise Fail "Socket.accept"
            fun connect _ = raise Fail "Socket.connect"
            fun fdToSock _ = raise Fail "Socket.fdToSock"
            fun listen _ = raise Fail "Socket.listen"
            fun listenAt _ = raise Fail "Socket.listenAt"
            fun shutdownRead _ = raise Fail "Socket.shutdownWrite"
            fun shutdownWrite _ = raise Fail "Socket.shutdownWrite"
         end

      (* From Tom 7 <twm@andrew.cmu.edu>. *)
      (* Implementation of Syslog which doesn't log anything. *)

      structure Syslog =
         struct

            type openflag = unit

            val CONS = ()
            val NDELAY = ()
            val PERROR = ()
            val PID = ()

            type facility = unit

            val AUTHPRIV = ()
            val CRON = ()
            val DAEMON = ()
            val KERN = ()
            val LOCAL0 = ()
            val LOCAL1 = ()
            val LOCAL2 = ()
            val LOCAL3 = ()
            val LOCAL4 = ()
            val LOCAL5 = ()
            val LOCAL6 = ()
            val LOCAL7 = ()
            val LPR = ()
            val MAIL = ()
            val NEWS = ()
            val SYSLOG = ()
            val USER = ()
            val UUCP = ()

            type loglevel = unit

            val EMERG = ()
            val ALERT = ()
            val CRIT = ()
            val ERR = ()
            val WARNING = ()
            val NOTICE = ()
            val INFO = ()
            val DEBUG = ()

            val closelog = fn _ => raise Fail "Syslog.closelog"
            val log = fn _ => raise Fail "Syslog.log"
            val openlog = fn _ => raise Fail "Syslog.openlog"
         end

      structure TextIO = IO (TextIO)

      structure Thread = MLtonThread

      structure Vector =
         struct
            open Vector

            fun create (n, f) =
               let
                  val r = ref (Array.fromList [])
                  val lim = ref 0
                  fun check i =
                     if 0 <= i andalso i < !lim then () else raise Subscript
                  val sub = fn i => (check i; Array.sub (!r, i))
                  val update = fn (i, x) => (check i; Array.update (!r, i, x))
                  val (tab, finish) = f {sub = sub, update = update}
               in
                  if 0 = n then
                     (finish (); Vector.fromList [])
                  else
                     let
                        val init = tab 0
                        val a = Array.array (n, init)
                        val () = r := a
                        val () =
                           Array.modifyi (fn (i, _) =>
                                          let
                                             val res =
                                                if i = 0 then
                                                   init
                                                else
                                                   tab i
                                             val () = lim := i + 1
                                          in
                                             res
                                          end)
                           a
                        val () = finish ()
                     in
                        Array.vector a
                     end
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
            datatype status = Original | Clone

            fun load _ = raise Fail "World.load"
            fun save _ = raise Fail "World.save"
            fun saveThread _ = raise Fail "World.saveThread"
         end

      structure Word =
         struct
            open Word

            type t = word

            fun rol (w, w') =
               let
                  val w' = w' mod (fromInt wordSize)
               in
                  orb (>> (w, fromInt wordSize - w'),
                       << (w, w'))
               end
            fun ror (w, w') =
               let
                  val w' = w' mod (fromInt wordSize)
               in
                  orb (>> (w, w'),
                       << (w, fromInt wordSize - w'))
               end
            local
               val max =    Word.toLargeInt 0wxFFFFFFFF
               val maxInt = Word.toLargeInt 0wx7FFFFFFF
               fun make (f: IntInf.int * IntInf.int -> IntInf.int)
                  (w: word, w': word): word =
                  let
                     val res = f (Word.toLargeInt w, Word.toLargeInt w')
                  in
                     if IntInf.> (res, max)
                        then raise Overflow
                     else Word.fromLargeInt res
                  end
            in
               val addCheck = make IntInf.+
               val mulCheck = make IntInf.*
            end
         end

      structure Word8 =
         struct
            open Word8

            type t = word

            val _ = >> : word * Word.word -> word
            fun rol (w: word, w': Word.word): word =
               let
                  val w' = Word.mod (w', Word.fromInt wordSize)
               in
                  orb (>> (w, Word.- (Word.fromInt wordSize, w')),
                       << (w, w'))
               end
            fun ror (w, w') =
               let
                  val w' = Word.mod (w', Word.fromInt wordSize)
               in
                  orb (>> (w, w'),
                       << (w, Word.- (Word.fromInt wordSize, w')))
               end
         end
   end
