(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature CONST_TYPE_STRUCTS =
   sig
   end

signature CONST_TYPE =
   sig
      include CONST_TYPE_STRUCTS
	 
      datatype t = Bool | Real | String | Word

      val toString: t -> string
   end
