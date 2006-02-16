(* Copyright (C) 2002-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

type int = Int.int

signature MLTON_VECTOR =
   sig
      val unfoldi: int * 'b * (int * 'b -> 'a * 'b) -> 'a vector
   end
   
