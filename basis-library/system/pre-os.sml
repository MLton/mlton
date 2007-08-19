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
            type status = C_Status.t
         end
      structure IO =
         struct
            type iodesc = C_Fd.t
         end
   end

structure PreOS = OS
