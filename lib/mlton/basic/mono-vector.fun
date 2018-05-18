(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor MonoVector (Elt: T) =
   struct
      open Vector
      type t = Elt.t t
      val equals = fn (a, a') => equals (a, a', Elt.equals)
      val layout = layout Elt.layout
   end
