(* Copyright (C) 2002-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure OS =
   struct
      structure Process =
         struct
            structure Status =
               MkAbsRep(type rep = C_Status.t)
            structure Status =
               struct
                  open Status
                  fun equals (s1,s2) =
                     (toRep s1) = (toRep s2)
               end
            type status = Status.t
         end
      structure IO :
         sig
            eqtype iodesc
         end =
         struct
            type iodesc = C_Fd.t
         end
   end

structure PreOS = OS
