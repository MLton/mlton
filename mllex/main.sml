(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Main =
struct

fun usage s =
   Process.usage {usage = "file.lex ...",
                  msg = s}

fun main args =
   let
      val rest =
         let open Popt
         in parse {switches = args, opts = []}
         end
   in
      case rest of
         Result.No msg => usage msg
       | Result.Yes [] => usage "no files"
       | Result.Yes files => List.foreach (files, LexGen.lexGen)
   end

val main = Process.makeMain main

end
