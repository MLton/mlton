(* Copyright (C) 2009 Matthew Fluet.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure ExportSMLNJ =
   struct
      fun exportSMLNJ (file: string): unit =
         SMLofNJ.exportFn (file, Main.main)
   end
