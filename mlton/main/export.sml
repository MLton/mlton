(* Copyright (C) 2009 Matthew Fluet.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Export : EXPORT =
   struct

      fun exportNJ (file: File.t): unit =
         SMLofNJ.exportFn (file, Main.main)

      fun exportMLton (): unit =
         case CommandLine.arguments () of
            [worldFile] =>
               SMLofNJ.exportFn (worldFile, Main.main)
          | _ => Error.bug "usage: exportMLton worldFile"
   end
