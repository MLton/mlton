(* Copyright (C) 2002-2006, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure PreOS =
   struct
      structure Status =
         MkAbsRep(type rep = C_Status.t)
      structure IODesc =
         MkAbsRepEq(type rep = C_Fd.t)
   end
structure OS =
   struct
      structure Process =
         struct
            type status = PreOS.Status.t
         end
      structure IO =
         struct
            type iodesc = PreOS.IODesc.t
         end
   end
