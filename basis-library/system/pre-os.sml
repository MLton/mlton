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
            type status = PosixPrimitive.Process.Status.t
         end
      structure IO :> sig
                         eqtype iodesc

                         val fromFD: PosixPrimitive.IO.file_desc -> iodesc
                         val toFD: iodesc -> PosixPrimitive.IO.file_desc
                      end = 
                      struct
                         type iodesc = PosixPrimitive.IO.file_desc

                         val fromFD = fn z => z
                         val toFD = fn z => z
                      end
   end

structure PreOS = OS
