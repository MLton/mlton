(* Copyright (C) 2022 Matthew Fluet
 * Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Main =
struct

fun usage s =
   CommandLine.usage {usage = "file.grm", msg = s}

fun main (_, args) =
   let
      val rest =
         let open Popt
         in parse {switches = args,
                   opts = []}
         end
   in case rest of
      Result.No msg => usage msg
    | Result.Yes [file] => ParseGen.parseGen file
    | _ => usage "too many files"
   end

val main = CommandLine.make main

end
