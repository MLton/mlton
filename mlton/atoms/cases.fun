(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Cases (S: CASES_STRUCTS): CASES =
struct

open S

datatype 'a t =
   Char of (char * 'a) vector
 | Con of (con * 'a) vector
 | Int of (IntInf.t * 'a) vector
 | Word of (word * 'a) vector



end
