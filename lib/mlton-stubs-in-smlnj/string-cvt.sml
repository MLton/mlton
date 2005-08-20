(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure StringCvt =
   struct
      open StringCvt

      open OpenInt32

      datatype realfmt =
         EXACT
       | FIX of Pervasive.Int32.int option 
       | GEN of Pervasive.Int32.int option 
       | SCI of Pervasive.Int32.int option 

      fun padLeft c i = StringCvt.padLeft c (toInt i)
      fun padRight c i = StringCvt.padRight c (toInt i)
   end
