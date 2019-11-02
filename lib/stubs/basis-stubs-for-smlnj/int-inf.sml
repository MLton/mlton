(* Copyright (C) 2009,2019 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor FixIntInf(PIntInf: sig include INT_INF end) : INT_INF =
   struct
      open PIntInf

      local
         (* SML/NJ uses lower instead of upper case. *)
         val toUpper = String.translate (Char.toString o Char.toUpper)
      in
         fun fmt r w = toUpper (PIntInf.fmt r w)
      end
   end

structure IntInf = FixIntInf(struct open Pervasive.IntInf end)
structure LargeInt: INTEGER = IntInf
