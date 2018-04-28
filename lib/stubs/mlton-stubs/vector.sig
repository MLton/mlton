(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature MLTON_VECTOR =
   sig
      val create: int -> {done: unit -> 'a vector,
                          sub: int -> 'a,
                          update: int * 'a -> unit}
      val unfoldi: int * 'b * (int * 'b -> 'a * 'b) -> 'a vector * 'b
   end
