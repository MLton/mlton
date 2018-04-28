(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure MyDirs: MY_DIRS =
   struct
      open Dir

      fun dirs() =
         let
            val home = fromString "/home/sweeks/"
            val sml = relative(home, "sml")
            val smlnj = relative(home, "sml/sml-nj-root")
            val bin = relative(smlnj, "bin")
            val binFiles = relative(smlnj, "bin.x86-unix")
            val heap = relative(bin, ".heap")
            val src = relative(smlnj, "src")
            val compiler = relative(src, "sml-nj")
         in {home = home, sml = sml, smlnj = smlnj, bin = bin,
             binFiles = binFiles, heap = heap, src = src, compiler = compiler}
         end

      fun exportFn(name, f) =
         SMLofNJ.exportFn(File.toString(File.relative(#heap(dirs()),name)),
                          f)
      fun exportML name =
         SMLofNJ.exportML(File.toString(File.relative(#heap(dirs()),name)))
   end
