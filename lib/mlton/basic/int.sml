(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure Int:
   sig
      include INTEGER

      type int
	 
      val maxInt: t
      val minInt: t
      val toReal: t -> real
   end =
   struct
      structure Int = Pervasive.Int
      structure I = Integer(open Int
			    fun divMod(a, b) = (a div b, a mod b)
			    fun quotRem(a, b) = (quot(a, b), rem(a, b))
			    val toIntInf = Pervasive.IntInf.fromInt)
      open I

      type int = t
      val maxInt = valOf Int.maxInt
      val minInt = valOf Int.minInt
      val toReal = Pervasive.Real.fromInt
   end

structure Int32: INTEGER = Int
