(* Copyright (C) 2005-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure IEEEReal =
   struct
      open IEEEReal

      datatype float_class =
         NAN
       | INF
       | ZERO
       | NORMAL
       | SUBNORMAL
   end
