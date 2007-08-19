(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure OS =
   struct
      open Pervasive.OS

      structure FileSys =
         struct
            open FileSys

            val fileSize = Pervasive.Int32.fromInt o fileSize
            val hash = Pervasive.Word32.fromLargeWord o Pervasive.Word.toLargeWord o hash
         end
   end
