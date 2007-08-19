(* Copyright (C) 2002-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
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
