(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)

type int = Int.t
   
signature FIELD_STRUCTS = 
   sig
   end

signature FIELD = 
   sig
      include FIELD_STRUCTS
      
      datatype t =
	 String of string
       | Int of int

      val <= : t * t -> bool (* ordering used for sorting *)
      val equals: t * t -> bool
      val layout: t -> Layout.t
   end
