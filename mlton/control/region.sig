(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature REGION_STRUCTS = 
   sig
   end

signature REGION = 
   sig
      include REGION_STRUCTS

      type t

      val <= : t * t -> bool
      val append: t * t -> t
      val bogus: t
      val compare: t * t -> Relation.t
      val equals: t * t -> bool
      val extendRight: t * SourcePos.t -> t
      val left: t -> SourcePos.t option
      val layout: t -> Layout.t
      val make: {left: SourcePos.t, right: SourcePos.t} -> t
      val right: t -> SourcePos.t option
      val toString: t -> string

      structure Wrap:
         sig
            type region
            type 'a t
            val region: 'a t -> region
            val node: 'a t -> 'a
            val makeRegion: 'a * region -> 'a t
            val makeRegion':  'a * SourcePos.t * SourcePos.t -> 'a t
(*          val make: 'a -> 'a t *)
            val dest: 'a t -> 'a * region
(*          val left: 'a t -> int *)
(*          val right: 'a t -> int *)
         end
      sharing type Wrap.region = t
   end
