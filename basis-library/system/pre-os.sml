(* Copyright (C) 2002-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure OS =
   struct
      structure Process :> 
         sig
            type status
            structure Status :
               sig
                  type t = status
                  val equals: t * t -> bool
                  val fromC: C_Status.t -> t
                  val toC: t -> C_Status.t
               end
         end =
         struct
            structure Status =
               struct
                  type t = C_Status.t
                  fun equals (x1: t, x2: t) = x1 = x2
                  val fromC = fn x => x
                  val toC = fn x => x
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
