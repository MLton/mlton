(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 2002-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure MLtonProcess =
   struct
      structure Prim = PrimitiveFFI.MLton.Process
      structure MLton = Primitive.MLton
      local
         open Posix
      in
         structure FileSys = FileSys
         structure IO = IO
         structure ProcEnv = ProcEnv
         structure Process = Process
         structure FileDesc = PrePosix.FileDesc
         structure PId = PrePosix.PId
         structure Signal = PrePosix.Signal
      end
      structure Mask = MLtonSignal.Mask
      structure SysCall = PosixError.SysCall

      type pid = PId.t

      exception MisuseOfForget
      exception DoublyRedirected

      type input = unit
      type output = unit

      type none = unit
      type chain = unit
      type any = unit

      val useWindowsProcess = MLton.Platform.OS.useWindowsProcess

      val readWrite =
        let
           open FileSys.S
        in
           flags [irusr, iwusr, irgrp, iwgrp, iroth, iwoth]
        end

      structure Child =
         struct
            datatype 'use childt =
               FileDesc of FileSys.file_desc
             | Stream of 'use * ('use -> unit)
             | Term
            type ('use, 'dir) t = 'use childt ref

            (* This is _not_ the identity; by rebuilding it we get type
             * ('a, 'b) t -> ('c, 'd) t
             *)
            fun remember x =
               case !x of 
                  FileDesc f =>
                     (x := Stream ((), fn () => ())
                      ; ref (FileDesc f))
                | Stream _ => raise MisuseOfForget (* remember twice = bad *)
                | Term => ref Term

            local
               fun convert (new, close) p =
                  case !p of
                     FileDesc fd =>
                        let
                           val str = new (fd, "<process>")
                           val () = p := Stream (str, close)
                        in
                           str
                        end
                   | Stream (str, _) => str
                   | Term => raise MisuseOfForget
            in
               val binIn = convert (BinIO.newIn, BinIO.closeIn)
               val binOut = convert (BinIO.newOut, BinIO.closeOut)
               val textIn = convert (TextIO.newIn, TextIO.closeIn)
               val textOut = convert (TextIO.newOut, TextIO.closeOut)
            end

            fun fd p =
               case !p of
                  FileDesc fd => fd
                | _ => raise MisuseOfForget

            fun close ch =
               case ch of
                  FileDesc fd => IO.close fd
                | Stream (str, close) => close str
                | Term => ()

            val close =
               fn (stdin, stdout, stderr) => 
               (close stdin; close stdout; close stderr)
         end

      structure Param =
         struct
            datatype ('use, 'dir) t =
               File of string
             | FileDesc of FileSys.file_desc
             | Pipe
             | Self

            (* This is _not_ the identity; by rebuilding it we get type
             * ('a, 'b) t -> ('c, 'd) t
             *)
            val forget = fn 
               File x => File x
             | FileDesc f => FileDesc f
             | Pipe => Pipe
             | Self => Self

            val pipe = Pipe
            local
               val null = if useWindowsProcess then "nul" else "/dev/null"
            in
               val null = File null
            end
            val self = Self
            fun file f = File f
            fun fd f = FileDesc f

            fun child c =
               FileDesc
               (case !c of 
                   Child.FileDesc f => (c := Child.Stream ((), fn () => ()); f)
                 | Child.Stream _ => raise DoublyRedirected
                 | Child.Term  => raise MisuseOfForget)

            fun setCloseExec fd =
               if useWindowsProcess
                  then ()
               else IO.setfd (fd, IO.FD.flags [IO.FD.cloexec])

            local
               fun openOut std p =
                  case p of 
                     File s => (FileSys.creat (s, readWrite), Child.Term)
                   | FileDesc f => (f, Child.Term)
                   | Pipe =>
                        let
                           val {infd, outfd} = IO.pipe ()
                           val () = setCloseExec infd
                        in
                           (outfd, Child.FileDesc infd)
                        end
                   | Self => (std, Child.Term)
            in
               fun openStdout p = openOut FileSys.stdout p
               fun openStderr p = openOut FileSys.stderr p
            end

            fun openStdin p =
               case p of
                  File s =>
                     (FileSys.openf (s, FileSys.O_RDONLY, FileSys.O.flags []),
                      Child.Term)
                | FileDesc f => (f, Child.Term)
                | Pipe =>
                     let
                        val {infd, outfd} = IO.pipe ()
                        val () = setCloseExec outfd
                     in
                        (infd, Child.FileDesc outfd)
                     end
                | Self => (FileSys.stdin, Child.Term)

            fun close p fd =
               case p of
                  File _ => IO.close fd
                | FileDesc _ => IO.close fd
                | Pipe => IO.close fd
                | _ => ()
        end

      datatype ('stdin, 'stdout, 'stderr) t =
         T of {pid: Process.pid, (* if useWindowsProcess, 
                                  * then this is a Windows process handle 
                                  * and can't be passed to
                                  * Posix.Process.* functions. 
                                  *)
               status: Posix.Process.exit_status option ref,
               stderr: ('stderr, input) Child.t,
               stdin:  ('stdin, output) Child.t,
               stdout: ('stdout, input) Child.t}

      local
         fun make f (T r) = f r
      in
         val getStderr = fn z => make #stderr z
         val getStdin = fn z => make #stdin z
         val getStdout = fn z => make #stdout z
      end

      fun ('a, 'b) protect (f: 'a -> 'b, x: 'a): 'b =
         let
            val () = Mask.block Mask.all
         in
            DynamicWind.wind (fn () => f x, fn () => Mask.unblock Mask.all)
         end

      local
         fun reap reapFn (T {pid, status, stderr, stdin, stdout, ...}) =
            case !status of
               NONE =>
                  let
                     val _ = Child.close (!stdin, !stdout, !stderr)
                     val st = reapFn pid
                  in
                     status := SOME st
                     ; st
                  end
             | SOME st => st
      in
         fun reapForFork p =
            reap (fn pid =>
                  let
                     (* protect is probably too much; typically, one
                      * would only mask SIGINT, SIGQUIT and SIGHUP.
                      *)
                     val (_, st) =
                        protect (Process.waitpid, (Process.W_CHILD pid, []))
                  in
                     st
                  end) 
                  p
         fun reapForCreate p =
            reap (fn pid =>
                  let
                     val pid' = PId.toRep pid
                     val status' = ref (C_Status.fromInt 0)
                     val () =
                        SysCall.simple
                        (fn () =>
                         PrimitiveFFI.Windows.Process.getexitcode 
                         (pid', status'))
                  in
                     Process.fromStatus' (!status')
                  end)
                 p
      end
      val reap = fn p =>
         (if useWindowsProcess then reapForCreate else reapForFork) p

      local
         fun kill killFn (p as T {pid, status, ...}, signal) =
            case !status of
               NONE =>
                  let
                     val () = killFn (pid, signal)
                  in
                     ignore (reap p)
                  end
             | SOME _ => ()
      in
         fun killForFork p =
            kill (fn (pid, signal) =>
                  Process.kill (Process.K_PROC pid, signal))
                 p
         fun killForCreate p =
            kill (fn (pid, signal) =>
                  SysCall.simple
                  (fn () =>
                   PrimitiveFFI.Windows.Process.terminate 
                   (PId.toRep pid, Signal.toRep signal)))
                 p
      end
      val kill = fn (p, signal) =>
         (if useWindowsProcess then killForCreate else killForFork) (p, signal)

      fun launchWithFork (path, args, env, stdin, stdout, stderr) =
         case protect (Process.fork, ()) of
            NONE => (* child *)
               let 
                  fun dup2 (old, new) =
                     if old = new
                        then ()
                     else (IO.dup2 {old = old, new = new}; IO.close old)
                  val args = path :: args
                  val execTh =
                     case env of
                        NONE =>
                           (fn () => Process.exec (path, args))
                      | SOME env =>
                           (fn () => Process.exece (path, args, env))
               in
                  dup2 (stdin, FileSys.stdin)
                  ; dup2 (stdout, FileSys.stdout)
                  ; dup2 (stderr, FileSys.stderr)
                  ; ignore (execTh ())
                  ; Process.exit 0w127 (* just in case *)
               end
          | SOME pid => pid (* parent *)

      fun strContains seps s =
        CharVector.exists (Char.contains seps) s
      (* In MinGW, a string must be escaped if it contains " \t" or is "".
       * Escaping means adds "s on the front and end. Any quotes inside
       * must be escaped with \. Any \s already in the string must be
       * doubled ONLY when they precede a " or the end of string.
       *)
      fun mingwEscape (l, 0) = l
        | mingwEscape (l, i) = mingwEscape (#"\\"::l, i-1)
      fun mingwFold (#"\\", (l, escapeCount)) = (#"\\"::l, escapeCount+1)
        | mingwFold (#"\"", (l, escapeCount)) = 
            (#"\"" :: mingwEscape (#"\\"::l, escapeCount), 0)
        | mingwFold (x, (l, _)) = (x :: l, 0)
      val mingwQuote = mingwEscape o CharVector.foldl mingwFold ([#"\""], 0)
      fun mingwEscape y =
         if not (strContains " \t\"" y) andalso y<>"" then y else
         String.implode (List.rev (#"\"" :: mingwQuote y))

      fun cygwinEscape y = 
         if not (strContains " \t\"\r\n\f'" y) andalso y<>"" then y else
         concat ["\"",
                 String.translate
                 (fn #"\"" => "\\\"" | #"\\" => "\\\\" | x => String.str x) y,
                 "\""]

      val cmdEscapeCreate = 
         if MLton.Platform.OS.host = MLton.Platform.OS.MinGW
         then mingwEscape else cygwinEscape

      val cmdEscapeSpawn = 
         if MLton.Platform.OS.host = MLton.Platform.OS.MinGW
         then mingwEscape else (fn s => s)

      fun launchWithCreate (path, args, env, stdin, stdout, stderr) =
         let
            val path' = 
               NullString.nullTerm
               (let 
                   open MLton.Platform.OS
                in
                   case host of
                      Cygwin => Cygwin.toFullWindowsPath path
                    | MinGW => path
                    | _ => raise Fail "MLton.Process.launchWithCreate: path'"
                end)
            val args' =
               NullString.nullTerm 
               (String.concatWith " " (List.map cmdEscapeCreate (path :: args)))
            val env' =
               Option.map 
               (fn env =>
                NullString.nullTerm 
                ((String.concatWith "\000" env) ^ "\000"))
               env
            val stdin' = FileDesc.toRep stdin
            val stdout' = FileDesc.toRep stdout
            val stderr' = FileDesc.toRep stderr
            val createTh =
               case env' of
                  NONE =>
                     (fn () =>
                      PrimitiveFFI.Windows.Process.createNull
                      (path', args', stdin', stdout', stderr'))
                | SOME env' =>
                     (fn () =>
                      PrimitiveFFI.Windows.Process.create
                      (path', args', env', stdin', stdout', stderr'))
            val pid' =
               SysCall.simpleResult'
               ({errVal = C_PId.castFromFixedInt ~1}, fn () =>
                createTh ())
            val pid = PId.fromRep pid'
         in
            pid
         end

      val launch =
         fn z =>
         (if useWindowsProcess then launchWithCreate else launchWithFork) z

      fun create {args, env, path, stderr, stdin, stdout} =
         if not (FileSys.access (path, [FileSys.A_EXEC]))
            then PosixError.raiseSys PosixError.noent
         else
            let
               val () = TextIO.flushOut TextIO.stdOut
               val (fstdin, cstdin) = Param.openStdin stdin
               val (fstdout, cstdout) = Param.openStdout stdout
               val (fstderr, cstderr) = Param.openStderr stderr
               val closeStdio =
                  fn () => (Param.close stdin fstdin
                            ; Param.close stdout fstdout
                            ; Param.close stderr fstderr)
               val pid =
                  launch (path, args, env, fstdin, fstdout, fstderr)
                  handle ex => (closeStdio ()
                                ; Child.close (cstdin, cstdout, cstderr)
                                ; raise ex)
               val () = closeStdio ()
            in
               T {pid = pid,
                  status = ref NONE,
                  stderr = ref cstderr,
                  stdin = ref cstdin,
                  stdout = ref cstdout}
            end

      fun spawne {path, args, env} =
         if useWindowsProcess
            then
               let
                  val args = List.map cmdEscapeSpawn args
                  val path = NullString.nullTerm path
                  val args = CUtil.C_StringArray.fromList args
                  val env = CUtil.C_StringArray.fromList env
               in
                  (PId.fromRep o SysCall.simpleResult')
                  ({errVal = C_PId.castFromFixedInt ~1}, fn () =>
                   Prim.spawne (path, args, env))
               end
         else
            case Posix.Process.fork () of
               NONE => (Posix.Process.exece (path, args, env) handle _ => ()
                        ; Posix.Process.exit 0w127)
             | SOME pid => pid

      fun spawn {args, path}= 
         spawne {args = args,
                 env = ProcEnv.environ (),
                 path = path}

      fun spawnp {args, file} =
         if useWindowsProcess
            then
               let
                  val file = NullString.nullTerm file
                  val args = List.map cmdEscapeSpawn args
                  val args = CUtil.C_StringArray.fromList args
               in
                  (PId.fromRep o SysCall.simpleResult')
                  ({errVal = C_PId.castFromFixedInt ~1}, fn () =>
                   Prim.spawnp (file, args))
               end
         else    
            case Posix.Process.fork () of
               NONE => (Posix.Process.execp (file, args) handle _ => ()
                        ; Posix.Process.exit 0w127)
             | SOME pid => pid

      open Exit
   end
