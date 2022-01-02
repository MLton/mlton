(* Copyright (C) 2009,2022 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure CommandLine: COMMAND_LINE =
struct

open CommandLine

structure Status =
   struct
      type t = OS.Process.status
   end

fun makeMain (commandLine: string * string list -> unit) args =
   ((commandLine args; OS.Process.success)
    handle e =>
       let
          val out = Out.error
       in
          Out.output (out, concat ["unhandled exception: ", Exn.toString e, "\n"])
          ; (case Exn.history e of
                [] => ()
              | l => (Out.output (out, "with history: \n")
                      ; List.foreach
                        (l, fn s =>
                         Out.output (out, concat ["\t", s, "\n"]))))
          ; OS.Process.failure
       end)

fun wrapMain main (): 'a =
   OS.Process.exit (main (CommandLine.name(), CommandLine.arguments ()))

fun make commandLine = wrapMain (makeMain commandLine)

fun usage {usage: string, msg: string}: 'a =
   let
      val commandName = #file (OS.Path.splitDirFile (CommandLine.name ()))
   in
      Process.fail (concat [msg, "\n", "Usage: ", commandName, " ", usage])
   end

end
