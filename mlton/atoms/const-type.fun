(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor ConstType (S: CONST_TYPE_STRUCTS): CONST_TYPE =
struct

open S

datatype t = Bool | Real | String | Word

val toString =
   fn Bool => "Bool"
    | Real => "Real"
    | String => "String"
    | Word => "Word"
	 
end
