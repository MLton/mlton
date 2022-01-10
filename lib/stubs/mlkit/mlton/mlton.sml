(* Copyright (C) 2022 Matthew Fluet.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure MLton =
   struct
      val isMLton = false
      structure Exn =
         struct
            val history : exn -> string list = fn _ => []
         end
      structure GC =
         struct
            fun collect () = ()
            fun setMessages (b : bool) = ()
            fun pack () = collect ()
         end
      structure Process = MLtonProcess
   end
