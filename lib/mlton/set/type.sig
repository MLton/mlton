(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature TYPE =
    sig
        datatype set =
            EmptySet
          | Set of elt
        and elt = 
            Base
          | Pair of elt * elt
          | EltSet of set

        exception Incompatible

        structure Set :
            sig
                type t sharing type t = set
                val combine: t * t -> t
                val areCompatible: t * t -> bool
            end

        structure Elt :
            sig
                type t sharing type t = elt
                val combine: t * t -> t
                val areCompatible: t * t -> bool
            end

        val combineSetElt: set * elt -> set
        val areCompatibleSetElt: set * elt -> bool
    end
