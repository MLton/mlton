(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(* Relational Data Base *)
signature RDB =
   sig
      structure Value:
         sig
            type t

            val bool: bool -> t
            val compare: t * t -> Relation.t
            val int: int -> t
            val real: real -> t
            val string: string -> t
            val toString: t -> string
         end
      structure Domain:
         sig
            type t

            val bool: t
            val int: t
            val real: t
            val string: t
         end
      structure Attribute:
         sig
            type t

            val new: string -> t
         end

      type t

      val add: t * (Attribute.t * Value.t) list -> unit
      val cardinality: t -> int
      val degree: t -> int
      val new: {heading: (Attribute.t * Domain.t) list} -> t
      val printTable: {rdb: t,
                       row: Attribute.t,
                       col: Attribute.t,
                       entry: Attribute.t,
                       out: Out.t} -> unit
      val printTable': {rdb: t,
                        cols: Attribute.t list,
                        sortBy: Attribute.t,
                        out: Out.t} -> unit
   end
