(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
signature XML_TYPE =
   sig
      include HASH_TYPE
	 
      datatype dest =
	 Var of Tyvar.t
       | Con of Tycon.t * t vector
	 
      val dest: t -> dest
   end
