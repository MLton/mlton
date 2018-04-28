(* Copyright (C) 2009,2017 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature SOURCE_POS_STRUCTS = 
   sig
   end

signature SOURCE_POS = 
   sig
      include SOURCE_POS_STRUCTS

      type t

      val bogus: t
      val column: t -> int
      val compare: t * t -> Relation.t
      val equals: t * t -> bool
      val file: t -> File.t
      val fileEquals: t * t -> bool
      val isBogus: t -> bool
      val line: t -> int
      val make: {column: int,
                 file: File.t,
                 line: int} -> t
      val posToString: t -> string
      val toString: t -> string
   end
