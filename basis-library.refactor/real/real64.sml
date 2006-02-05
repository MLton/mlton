(* Copyright (C) 2003-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Real64 =
  Real
  (structure P = Primitive.Real64
   open P
   fun fromLarge _ r = P.fromLarge r
   val negInf = ~1.0 / 0.0
   val posInf = 1.0 / 0.0
   fun nextAfterDown r = nextAfter (r, negInf)
   fun nextAfterUp r = nextAfter (r, posInf)
  )
structure Real = Real64
val real = Real.fromInt
structure RealGlobal: REAL_GLOBAL = Real
open RealGlobal
structure LargeReal = Real64
