(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor StringComparisons (type t
                           val compare: t * t -> order) =
   struct
      fun < (x, y) = 
         (case compare (x, y) of
             LESS => true
           | _ => false)
      fun <= (x, y) = 
         (case compare (x, y) of
             GREATER => false
           | _ => true)
      fun > (x, y) =
         (case compare (x, y) of
             GREATER => true
           | _ => false)
      fun >= (x, y) = 
         (case compare (x, y) of
             LESS => false
           | _ => true)
   end
