(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
type int = Int.t
type word = Word.t
   
signature CONST_STRUCTS = 
   sig
      structure Ast: AST
      structure Tycon: TYCON
      sharing Tycon.AstId = Ast.Tycon
   end

signature CONST = 
   sig
      include CONST_STRUCTS

      structure SmallIntInf:
	 sig
	    val isSmall: IntInf.t -> bool
	    val toWord: IntInf.t -> word
	    val fromWord: word -> IntInf.t
	 end

      type t

      structure Node:
	 sig
	    datatype t =
	       Char of char
	     | Int of int
	     | IntInf of IntInf.t
	     | Real of string
	     | String of string
	     | Word of word

	    val layout: t -> Layout.t
	 end

      val equals: t * t -> bool
      val fromChar: char -> t
      val fromInt: int -> t
      val fromIntInf: IntInf.t -> t
      val fromReal: string -> t
      val fromString: string -> t
      val fromWord: word -> t
      val fromWord8: Word8.t -> t
      val hash: t -> word
      val layout: t -> Layout.t
      val make: Node.t * Tycon.t -> t
      val node: t -> Node.t
      val toAstExp: t -> Ast.Exp.t
      val toAstPat: t -> Ast.Pat.t
      val toString: t -> string
      val tycon: t -> Tycon.t
   end
