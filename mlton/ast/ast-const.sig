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
	 
      datatype t =
	 Char of char
       | Int of string
       | Real of string
       | String of string
       | Word of word

      val equals: t * t -> bool
      val fromInt: int -> t
      val layout: t -> Layout.t
   end
