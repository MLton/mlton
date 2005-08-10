(* Copyright (C) 2002-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

functor RealConvert
        (structure Real: REAL) :
        REAL_1997 =
  struct
     open Real

     val class = IEEEReal1997.>> o class
     val toDecimal = IEEEReal1997.>>> o toDecimal
     val fromDecimal = fromDecimal o IEEEReal1997.<<<
  end