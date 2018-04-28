(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 2004-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature REAL_X_STRUCTS = 
   sig
      structure RealSize: REAL_SIZE
      structure WordX: WORD_X
   end

signature REAL_X = 
   sig
      include REAL_X_STRUCTS

      (* reals of all RealSize.t sizes. *)
      type t

      datatype decon =
         NAN
       | ZERO of {signBit: bool}
       | ONE of {signBit: bool}
       | POW2 of {signBit: bool, exp: int} (* man = 0.5 *)
       | FIN of {signBit: bool, exp: int, man: t}
       | INF of {signBit: bool}

      val abs: t -> t option
      val acos: t -> t option
      val add: t * t -> t option
      val asin: t -> t option
      val atan2: t * t -> t option
      val atan: t -> t option
      val castFromWord: WordX.t -> t option
      val castToWord: t -> WordX.t option
      val cos: t -> t option
      val decon: t -> decon option
      val div: t * t -> t option
      val equal: t * t -> bool option
      val equals: t * t -> bool
      val exp: t -> t option
      val fromIntInf: IntInf.t * RealSize.t -> t option
      val hash: t -> word
      val layout: t -> Layout.t
      val le: t * t -> bool option
      val ln: t -> t option
      val log10: t -> t option
      val lt: t * t -> bool option
      val make: string * RealSize.t -> t option
      val mul: t * t -> t option
      val muladd: t * t * t -> t option
      val mulsub: t * t * t -> t option
      val neg: t -> t option
      val qequal: t * t -> bool option
      val sin: t -> t option
      val size: t -> RealSize.t
      val sqrt: t -> t option
      val sub: t * t -> t option
      val tan: t -> t option
      val toString: t -> string
      val zero: RealSize.t -> t
   end
