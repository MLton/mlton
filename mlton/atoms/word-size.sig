(* Copyright (C) 2009,2019,2021 Matthew Fluet.
 * Copyright (C) 2004-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature WORD_SIZE_STRUCTS =
   sig
   end

signature WORD_SIZE =
   sig
      include WORD_SIZE_STRUCTS

      type t
      datatype prim = W8 | W16 | W32 | W64

      val all: t list
      val bits: t -> Bits.t
      val bigIntInfWord: unit -> t
      val bool: t
      val bytes: t -> Bytes.t
      val byte: t
      val cardinality: t -> IntInf.t
      val cint: unit -> t
      val compare: t * t -> Relation.t
      val compareRes: t
      val cpointer: unit -> t
      val cptrdiff: unit -> t
      val csigatomic: unit -> t
      val csize: unit -> t
      val equals: t * t -> bool
      val fromBits: Bits.t -> t
      val fromPrim: prim -> t
      val hash: t -> word
      val isInRange: t * IntInf.t * {signed: bool} -> bool
      val max: t * {signed: bool} -> IntInf.t
      val min: t * {signed: bool} -> IntInf.t
      val memoize: (t -> 'a) -> t -> 'a
      val objptr: unit -> t
      val objptrHeader: unit -> t
      val parse: t Parse.t
      val primOpt: t -> prim option
      val prim: t -> prim
      val prims: t list
      val roundUpToPrim: t -> t
      val seqIndex: unit -> t
      val shiftArg: t
      val smallIntInfWord: unit -> t
      val toString: t -> string
      val word8: t      
      val word16: t
      val word32: t
      val word64: t
   end
