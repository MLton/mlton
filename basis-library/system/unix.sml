(* Copyright (C) 2004-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(* Rewritten by wesley@terpstra.ca on 2004-11-23 to use MLtonProcess for the
 * implementation.
 *)

structure Unix: UNIX =
struct

structure Status = OS_Process.Status
structure Process = MLtonProcess
local
   open Process
in
   structure Child = Child
   structure Param = Param
end

type signal = Posix.Signal.signal
datatype exit_status = datatype Posix.Process.exit_status

val fromStatus = Posix.Process.fromStatus

type ('in, 'out) proc = ('out, 'in, Process.none) Process.t

local
   fun create {args, env, path} =
      Process.create {args = args,
                      env = env,
                      path = path,
                      stderr = Param.self,
                      stdin = Param.pipe,
                      stdout = Param.pipe}
in
   fun execute (path, args) =
      create {args = args, env = NONE, path = path}
   fun executeInEnv (path, args, env) =
      create {args = args, env = SOME env, path = path}
end

fun binInstreamOf proc = Child.binIn (Process.getStdout proc)
fun binOutstreamOf proc = Child.binOut (Process.getStdin proc)
fun textInstreamOf proc = Child.textIn (Process.getStdout proc)
fun textOutstreamOf proc = Child.textOut (Process.getStdin proc)

fun streamsOf pr = (textInstreamOf pr, textOutstreamOf pr)

val kill = Process.kill

fun reap z = Status.fromPosix (Process.reap z)

fun exit (w: Word8.word): 'a =
   OS.Process.exit (Status.fromInt (Word8.toInt w))

end
