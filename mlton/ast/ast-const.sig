(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

signature AST_CONST_STRUCTS =
   sig
   end

signature AST_CONST =
   sig
      include AST_CONST_STRUCTS

      type t
      datatype node =
	 Bool of bool
       | Char of IntInf.t
       | Int of IntInf.t
       | Real of string
       | String of IntInf.t vector
       | Word of IntInf.t
      include WRAPPED sharing type node' = node
                      sharing type obj = t

      val equals: t * t -> bool
      val layout: t -> Layout.t
      val ordToString: IntInf.t -> string
      val toString: t -> string
   end
