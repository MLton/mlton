(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 2004-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature WORD_X_STRUCTS = 
   sig
      structure WordSize: WORD_SIZE
   end

signature WORD_X = 
   sig
      include WORD_X_STRUCTS

      (* Words of all WordSize.t sizes. *)
      type t

      val add: t * t -> t
      val allOnes: WordSize.t -> t
      val andb: t * t -> t
      val compare: t * t * {signed: bool} -> order
      val div: t * t * {signed: bool} -> t
      val equals: t * t -> bool
      val fromChar: char -> t (* returns a word of size 8 *)
      val fromIntInf: IntInf.t * WordSize.t -> t
      val ge: t * t * {signed: bool} -> bool
      val gt: t * t * {signed: bool} -> bool
      val hash: t -> word
      val isAllOnes: t -> bool
      val isOne: t -> bool
      val isMax: t * {signed: bool} -> bool
      val isMin: t * {signed: bool} -> bool
      val isNegOne: t -> bool
      val isZero: t -> bool
      val layout: t -> Layout.t
      val le: t * t * {signed: bool} -> bool
      val lshift: t * t -> t
      val lt: t * t * {signed: bool} -> bool
      val max: WordSize.t * {signed: bool} -> t
      val min: WordSize.t * {signed: bool} -> t
      val mod: t * t * {signed: bool} -> t
      val mul: t * t * {signed: bool} -> t
      val neg: t -> t
      val notb: t -> t
      val one: WordSize.t -> t
      val orb: t * t -> t
      val quot: t * t * {signed: bool} -> t
      val rem: t * t * {signed: bool} -> t
      val resize: t * WordSize.t -> t
      val resizeX: t * WordSize.t -> t
      val rol: t * t -> t
      val ror: t * t -> t
      val rshift : t * t * {signed: bool} -> t
      val size: t -> WordSize.t
      val sub: t * t -> t
      val toChar: t -> char
      val toInt: t -> int
      val toIntInf: t -> IntInf.t
      val toIntInfX: t -> IntInf.t
      val toString: t -> string
      val xorb: t * t -> t
      val zero: WordSize.t -> t
   end
