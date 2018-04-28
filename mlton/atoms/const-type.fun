(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor ConstType (S: CONST_TYPE_STRUCTS): CONST_TYPE =
struct

open S

datatype t = Bool | Real of RealSize.t | String | Word of WordSize.t

end
