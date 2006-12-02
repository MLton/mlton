(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor WordToBool (S : sig
                          eqtype t
                          val one: t
                          val zero: t
                        end) : sig
                                 eqtype t
                                 val fromBool: bool -> t
                                 val toBool: t -> bool
                               end =
   struct
      open S

      val fromBool: bool -> t = fn b => if b then zero else one
      val toBool: t -> bool = fn w => w <> zero
   end
