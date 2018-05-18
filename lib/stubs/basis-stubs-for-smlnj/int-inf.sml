(* Copyright (C) 2009 Matthew Fluet.
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
         structure FIntInf = FixInt(struct open PIntInf end)
      in
         open FIntInf
      end

      (* SML/NJ doesn't properly shift IntInf.int values. *)
      local
         fun pow2 w =
            if w = 0wx0
               then 1
            else
               let
                  val p = pow2 (Pervasive.Word.>> (w, 0wx1))
                  val pp = p * p
               in
                  if 0wx1 = Pervasive.Word.andb (0wx1, w)
                     then 2 * pp
                  else pp
               end
      in
         val ~>> = fn (a, b) => a div (pow2 b)
      end
   end

structure IntInf = FixIntInf(struct open Pervasive.IntInf end)
