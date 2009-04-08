(* Copyright (C) 2009 Matthew Fluet.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure ExportMLton =
   struct
      fun exportMLton (): unit =
         case CommandLine.arguments () of
            [worldFile] =>
               let
                  open MLton.World OS.Process
               in
                  case save (worldFile ^ ".mlton") of
                     Original => exit success
                   | Clone => Main.mainWrapped ()
               end
          | _ => Error.bug "usage: exportMLton worldFile"
   end
