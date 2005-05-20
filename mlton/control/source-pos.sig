(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
type int = Int.t
   
signature SOURCE_POS_STRUCTS = 
   sig
   end

signature SOURCE_POS = 
   sig
      include SOURCE_POS_STRUCTS

      type t

      val bogus: t
      val compare: t * t -> Relation.t
      val equals: t * t -> bool
      val file: t -> File.t
      val isBasis: t -> bool
      val line: t -> int
      val make: {column: int,
		 file: File.t,
		 line: int} -> t
      val posToString: t -> string
      val toString: t -> string
   end
