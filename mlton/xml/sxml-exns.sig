(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
signature SXML_EXNS = XML

(*
 invariants:
 * no PolyVal decs
 * hasExns = true
 * tyvar lists in datatype, val, and fun decs are always empty
 * type lists are empty for variable args and for non-built-in-constructor args
 * no tyvars in types
 *)
