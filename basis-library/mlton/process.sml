(* Copyright (C) 2002-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
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
         structure Process = Posix.Process
      end
      structure Mask = MLtonSignal.Mask
      structure SysCall = PosixError.SysCall

      type pid = C_PId.t

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
         T of {pid: Process.pid,
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
         if useWindowsProcess then f x
         else
            let
               val () = Mask.block Mask.all
            in
               DynamicWind.wind (fn () => f x, fn () => Mask.unblock Mask.all)
            end

      fun reap (T {pid, status, stderr, stdin, stdout}) =
         case !status of
            NONE => 
               let
                  val _ = Child.close (!stdin, !stdout, !stderr)
                  (* protect is probably too much; typically, one
                   * would only mask SIGINT, SIGQUIT and SIGHUP
                   *)
                  val (_, st) =
                     protect (Process.waitpid, (Process.W_CHILD pid, []))
                  val () = status := SOME st
               in
                  st
               end
          | SOME status => status

      fun kill (p as T {pid, status, ...}, signal) =
        case !status of
           NONE =>
              let
                 val () =
                    if useWindowsProcess
                       then
                          SysCall.simple
                          (fn () =>
                           PrimitiveFFI.Windows.Process.terminate (pid, signal))
                    else Process.kill (Process.K_PROC pid, signal)
              in
                 ignore (reap p)
              end
         | SOME _ => ()

      fun launchWithFork (path, args, env, stdin, stdout, stderr) =
         case protect (Process.fork, ()) of
            NONE => (* child *)
               let 
                  val base =
                     Substring.string
                     (Substring.taker (fn c => c <> #"/") (Substring.full path))
                  fun dup2 (old, new) =
                     if old = new
                        then ()
                     else (IO.dup2 {old = old, new = new}; IO.close old)
               in
                  dup2 (stdin, FileSys.stdin)
                  ; dup2 (stdout, FileSys.stdout)
                  ; dup2 (stderr, FileSys.stderr)
                  ; ignore (Process.exece (path, base :: args, env))
                  ; Process.exit 0w127 (* just in case *)
               end
          | SOME pid => pid (* parent *)

      val dquote = "\""
      fun cmdEscape y = 
         concat [dquote,
                 String.translate
                 (fn #"\"" => "\\\"" | #"\\" => "\\\\" | x => String.str x) y,
                 dquote]

      fun create (cmd, args, env, stdin, stdout, stderr) =
         SysCall.simpleResult'
         ({errVal = C_PId.castFromFixedInt ~1}, fn () =>
          let
             val cmd =
                let
                   open MLton.Platform.OS
                in
                   case host of
                      Cygwin => Cygwin.toExe cmd
                    | MinGW => cmd
                    | _ => raise Fail "create"
                end
          in
             PrimitiveFFI.Windows.Process.create
             (NullString.nullTerm cmd, args, env, stdin, stdout, stderr)
          end)

      fun launchWithCreate (path, args, env, stdin, stdout, stderr) =
         create 
         (path,
          NullString.nullTerm (String.concatWith " "
                               (List.map cmdEscape (path :: args))),
          NullString.nullTerm (String.concatWith "\000" env ^ "\000"),
          stdin, stdout, stderr)

      val launch =
         fn z =>
         (if useWindowsProcess then launchWithCreate else launchWithFork) z

      fun create {args, env, path, stderr, stdin, stdout} =
         if not (FileSys.access (path, [FileSys.A_EXEC]))
            then PosixError.raiseSys PosixError.noent
         else
            let
               val () = TextIO.flushOut TextIO.stdOut
               val env =
                  case env of
                     NONE => ProcEnv.environ ()
                   | SOME x => x
               val (fstdin, cstdin) = Param.openStdin stdin
               val (fstdout, cstdout) = Param.openOut FileSys.stdout stdout
               val (fstderr, cstderr) = Param.openOut FileSys.stderr stderr
               val closeStdio =
                  fn () => (Param.close stdin  fstdin
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
                  val path = NullString.nullTerm path
                  val args = CUtil.StringVector.fromList args
                  val env = CUtil.StringVector.fromList env
               in
                  SysCall.simpleResult'
                  ({errVal = C_PId.castFromFixedInt ~1}, fn () =>
                   Prim.spawne (path, 
                                #1 args, #2 args, #3 args,
                                #1 env, #2 env, #3 env))
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
                  val args = CUtil.StringVector.fromList args
               in
                  SysCall.simpleResult'
                  ({errVal = C_PId.castFromFixedInt ~1}, fn () =>
                   Prim.spawnp (file, 
                                #1 args, #2 args, #3 args))
               end
         else    
            case Posix.Process.fork () of
               NONE => (Posix.Process.execp (file, args) handle _ => ()
                        ; Posix.Process.exit 0w127)
             | SOME pid => pid

      open Exit
   end

