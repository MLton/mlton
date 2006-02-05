(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature MLTON_CONT =
   sig
      type 'a t

      val callcc: ('a t -> 'a) -> 'a
      val prepend: 'a t * ('b -> 'a) -> 'b t
      val throw: 'a t * 'a -> 'b
      val throw': 'a t * (unit -> 'a) -> 'b
   end
