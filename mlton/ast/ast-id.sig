(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature AST_ID_STRUCTS =
   sig
      structure Symbol: SYMBOL
   end

signature AST_ID =
   sig
      include AST_ID_STRUCTS
      include WRAPPED

      type t
      sharing type obj = t

      val bogus: t
      val compare: t * t -> Relation.t
      val equals: t * t -> bool
      val fromSymbol: Symbol.t * Region.t -> t
      val isSymbolic: t -> bool
      val layout: t -> Layout.t
      val toString: t -> string
      val toSymbol: t -> Symbol.t
   end
