(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
signature EXTRACT_TARGS_STRUCTS = 
   sig
      structure Prim: PRIM
      structure Type:
	 sig
	    include TYPE_OPS
	    val layout: t -> Layout.t
	 end
   end

signature EXTRACT_TARGS = 
   sig
      include EXTRACT_TARGS_STRUCTS
      
   end
