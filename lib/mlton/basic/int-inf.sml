(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure IntInf: INTEGER = Integer (open Pervasive.IntInf
                                     fun toIntInf x = x)

structure IntInf: INT_INF =
   struct
      open IntInf 

      val hash = String.hash o toString
         
      local
         open Pervasive.IntInf
      in
         val andb = andb
         val log2 = log2
         val notb = notb
         val orb = orb
         val xorb = xorb
         val op ~>> = ~>>
         val op << = <<
      end

      structure M = MaxPow2ThatDivides (open IntInf
                                        val andb = andb
                                        val orb = orb
                                        val << = <<
                                        val >> = ~>>)
      open M
   end

structure LargeInt = IntInf
