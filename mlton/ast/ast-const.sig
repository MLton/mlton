(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)

type int = Int.t
type word = Word.t
   
signature AST_CONST_STRUCTS =
   sig
   end

signature AST_CONST =
   sig
      include AST_CONST_STRUCTS

      type t
      datatype node =
	 Char of char
       | Int of string
       | Real of string
       | String of string
       | Word of word
      include WRAPPED sharing type node' = node
                      sharing type obj = t

      val equals: t * t -> bool
      val layout: t -> Layout.t
      val toString: t -> string
   end
