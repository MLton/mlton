(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 2004-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure MLton =
   struct
      val isMLton = false
      val size : 'a -> int = fn _ => ~1
      structure Exn =
         struct
            val history : exn -> string list = fn _ => []
         end
      structure GC =
         struct
            fun collect () = PolyML.fullGC ()
            fun setMessages (b : bool) = ()
            fun pack () = collect ()
         end
   end
