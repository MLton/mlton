(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure IntInf: INTEGER = Integer(open Pervasive.IntInf
				    fun toIntInf x = x)

structure IntInf: INT_INF =
   struct
      open IntInf 

      val hash = String.hash o toString
	 
      val andb = Pervasive.IntInf.andb
      val notb = Pervasive.IntInf.notb
      val orb = Pervasive.IntInf.orb
      val xorb = Pervasive.IntInf.xorb
      val (op ~>>) = Pervasive.IntInf.~>>
      val (op <<) = Pervasive.IntInf.<<
      val fromInt = Trace.trace("IntInf.fromInt", Int.layout, layout) fromInt
      val toInt = Trace.trace("IntInf.toInt", layout, Int.layout) toInt
   end

structure LargeInt = IntInf
