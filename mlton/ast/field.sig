(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
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
      val toString: t -> string
   end
