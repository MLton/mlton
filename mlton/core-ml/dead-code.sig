(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
signature DEAD_CODE_STRUCTS = 
   sig
      structure CoreML: CORE_ML
   end

signature DEAD_CODE = 
   sig
      include DEAD_CODE_STRUCTS
      
      val deadCode:
	 {basis: CoreML.Dec.t list,
	  user: CoreML.Dec.t list} -> CoreML.Dec.t list (* basis *)
   end
