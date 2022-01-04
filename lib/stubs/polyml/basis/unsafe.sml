(* Copyright (C) 2009 Matthew Fluet.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Unsafe : UNSAFE =
   struct
      structure Array =
         struct
            open Array
         end
      structure Vector =
         struct
            open Vector
         end
   end