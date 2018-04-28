(* Copyright (C) 2012,2017 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Tyvar (S: TYVAR_STRUCTS): TYVAR = 
struct

open S

structure V = Id (val noname = "'a")
open V

end
