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
            type status = C.Status.t
         end
      structure IO :> sig
                         eqtype iodesc

                         val fromFD: C.Fd.t -> iodesc
                         val toFD: iodesc -> C.Fd.t
                      end = 
                      struct
                         type iodesc = C.Fd.t

                         val fromFD = fn z => z
                         val toFD = fn z => z
                      end
   end

structure PreOS = OS
