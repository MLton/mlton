(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
signature SXML_TREE_STRUCTS =
  sig
    include XML_TREE
  end

signature SXML_TREE = XML_TREE

(*
 invariants:
 * no Exception decs
 * no PolyVal decs
 * hasPrimExns = false
 * tyvar lists in datatype, val, and fun decs are always empty
 * type lists are empty for variable args and for non-built-in-constructor args
 * no tyvars in types
 *)

