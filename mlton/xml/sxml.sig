(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
signature SXML = XML

(*
 invariants:
 * no Exception decs
 * no PolyVal decs
 * hasPrimExns = false
 * tyvar lists in datatype, val, and fun decs are always empty
 * type lists are empty for variable args and for non-built-in-constructor args
 * no tyvars in types
 *)
