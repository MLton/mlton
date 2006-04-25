(* Copyright (C) 2002-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure OS =
   struct
      structure Process =
         struct
            type status = C_Status.t
         end
      structure IO :> sig
                         eqtype iodesc

                         val fromFD: C_Fd.t -> iodesc
                         val toFD: iodesc -> C_Fd.t
                      end = 
         struct
            type iodesc = C_Fd.t

            val fromFD = fn z => z
            val toFD = fn z => z
         end
   end

structure PreOS = OS
