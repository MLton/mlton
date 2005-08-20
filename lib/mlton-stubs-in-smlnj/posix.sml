(* Copyright (C) 2003-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Posix =
   struct
      open Posix

      structure ProcEnv =
         struct
            open ProcEnv

            (* SML/NJ times is broken.  So it's probably best to ignore what
             * it says and return zero.
             *)
            fun times () =
               {cstime = Time.zeroTime,
                cutime = Time.zeroTime,
                elapsed = Time.zeroTime,
                stime = Time.zeroTime,
                utime = Time.zeroTime}
         end
   end
