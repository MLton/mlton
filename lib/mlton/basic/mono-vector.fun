(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor MonoVector (Elt: T) =
   struct
      open Vector
      type t = Elt.t t
      val equals = fn (a, a') => equals (a, a', Elt.equals)
      val layout = layout Elt.layout
   end
