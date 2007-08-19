(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

local
   fun 'a check (_: 'a, _: 'a) : unit = ()

   local
      structure R1 = Primitive.Real32
      structure R2 = PrimitiveFFI.Real32
   in
      val () = check (R1.Math.acos, R2.Math.acos)
      val () = check (R1.Math.asin, R2.Math.asin)
      val () = check (R1.Math.atan, R2.Math.atan)
      val () = check (R1.Math.atan2, R2.Math.atan2)
      val () = check (R1.Math.cos, R2.Math.cos)
      val () = check (R1.Math.cosh, R2.Math.cosh)
      val () = check (fn () => R1.Math.e, R2.Math.eGet)
      val () = check (R1.Math.exp, R2.Math.exp)
      val () = check (R1.Math.ln, R2.Math.ln)
      val () = check (R1.Math.log10, R2.Math.log10)
      val () = check (fn () => R1.Math.pi, R2.Math.piGet)
      val () = check (R1.Math.pow, R2.Math.pow)
      val () = check (R1.Math.sin, R2.Math.sin)
      val () = check (R1.Math.sinh, R2.Math.sinh)
      val () = check (R1.Math.sqrt, R2.Math.sqrt)
      val () = check (R1.Math.tan, R2.Math.tan)
      val () = check (R1.Math.tanh, R2.Math.tanh)

      val () = check (R1.abs, R2.abs)
      val () = check (R1.+, R2.add)
      val () = check (R1.class, R2.class)
      val () = check (R1./, R2.div)
      val () = check (R1.==, R2.equal)
      val () = check (R1.frexp, R2.frexp)
      val () = check (R1.gdtoa, R2.gdtoa)
      val () = check (R1.ldexp, R2.ldexp)
      val () = check (R1.<=, R2.le)
      val () = check (R1.<, R2.lt)
      val () = check (fn () => R1.maxFinite, R2.maxFiniteGet)
      val () = check (fn () => R1.minNormalPos, R2.minNormalPosGet)
      val () = check (fn () => R1.minPos, R2.minPosGet)
      val () = check (R1.modf, R2.modf)
      val () = check (R1.*, R2.mul)
      val () = check (R1.*+, R2.muladd)
      val () = check (R1.*-, R2.mulsub)
      val () = check (R1.~, R2.neg)
      val () = check (R1.round, R2.round)
      val () = check (R1.signBit, R2.signBit)
      val () = check (R1.strto, R2.strto)
      val () = check (R1.-, R2.sub)
   end

   local
      structure R1 = Primitive.Real64
      structure R2 = PrimitiveFFI.Real64
   in
      val () = check (R1.Math.acos, R2.Math.acos)
      val () = check (R1.Math.asin, R2.Math.asin)
      val () = check (R1.Math.atan, R2.Math.atan)
      val () = check (R1.Math.atan2, R2.Math.atan2)
      val () = check (R1.Math.cos, R2.Math.cos)
      val () = check (R1.Math.cosh, R2.Math.cosh)
      val () = check (fn () => R1.Math.e, R2.Math.eGet)
      val () = check (R1.Math.exp, R2.Math.exp)
      val () = check (R1.Math.ln, R2.Math.ln)
      val () = check (R1.Math.log10, R2.Math.log10)
      val () = check (fn () => R1.Math.pi, R2.Math.piGet)
      val () = check (R1.Math.pow, R2.Math.pow)
      val () = check (R1.Math.sin, R2.Math.sin)
      val () = check (R1.Math.sinh, R2.Math.sinh)
      val () = check (R1.Math.sqrt, R2.Math.sqrt)
      val () = check (R1.Math.tan, R2.Math.tan)
      val () = check (R1.Math.tanh, R2.Math.tanh)

      val () = check (R1.abs, R2.abs)
      val () = check (R1.+, R2.add)
      val () = check (R1.class, R2.class)
      val () = check (R1./, R2.div)
      val () = check (R1.==, R2.equal)
      val () = check (R1.frexp, R2.frexp)
      val () = check (R1.gdtoa, R2.gdtoa)
      val () = check (R1.ldexp, R2.ldexp)
      val () = check (R1.<=, R2.le)
      val () = check (R1.<, R2.lt)
      val () = check (fn () => R1.maxFinite, R2.maxFiniteGet)
      val () = check (fn () => R1.minNormalPos, R2.minNormalPosGet)
      val () = check (fn () => R1.minPos, R2.minPosGet)
      val () = check (R1.modf, R2.modf)
      val () = check (R1.*, R2.mul)
      val () = check (R1.*+, R2.muladd)
      val () = check (R1.*-, R2.mulsub)
      val () = check (R1.~, R2.neg)
      val () = check (R1.round, R2.round)
      val () = check (R1.signBit, R2.signBit)
      val () = check (R1.strto, R2.strto)
      val () = check (R1.-, R2.sub)
   end

in

end
