(* Copyright (C) 2002-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

type int = Int.int

signature MLTON_VECTOR =
   sig
      val unfoldi: int * 'b * (int * 'b -> 'a * 'b) -> 'a vector
   end
   
