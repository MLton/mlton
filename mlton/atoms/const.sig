(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
type int = Int.t
type word = Word.t
   
signature CONST_STRUCTS = 
   sig
      structure Ast: AST
      structure Tycon: TYCON
      sharing Tycon.AstId = Ast.Tycon
      sharing Tycon.AstId = Ast.Tycon
   end

signature CONST = 
   sig
      include CONST_STRUCTS

      structure Type:
	 sig
	    type t
	    val make: Tycon.t * Tycon.t vector -> t
	    val equals: t * t -> bool
	    val layout: t -> Layout.t
	    val toString: t -> string
	    val toType: t * (Tycon.t * 'a vector -> 'a) -> 'a
	    val bool: t
	    val char: t
	    val int: t
	    val intInf: t
	    val real: t
	    val string: t
	    val word: t
	    val word8: t
	 end

      structure SmallIntInf:
	 sig
	    val isSmall: IntInf.t -> bool
	    val toWord: IntInf.t -> word option
	    val fromWord: word -> IntInf.t option
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
      val make: Node.t * Type.t -> t
      val node: t -> Node.t
      val toAstExp: t -> Ast.Exp.t
      val toAstPat: t -> Ast.Pat.t
      val toString: t -> string
      val ty: t -> Type.t
   end
