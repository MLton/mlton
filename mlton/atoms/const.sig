(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
type word = Word.t
   
signature CONST_STRUCTS = 
   sig
      structure RealX: REAL_X
      structure WordX: WORD_X
      structure WordXVector: WORD_X_VECTOR
   end

signature CONST = 
   sig
      include CONST_STRUCTS

      structure ConstType: CONST_TYPE

      structure SmallIntInf:
	 sig
	    val fromWord: word -> IntInf.t
	    val isSmall: IntInf.t -> bool
	    val toWord: IntInf.t -> word option
	 end

      datatype t =
	 IntInf of IntInf.t
       | Real of RealX.t
       | Word of WordX.t
       | WordVector of WordXVector.t

      val equals: t * t -> bool
      val intInf: IntInf.t -> t
      val hash: t -> word
      val layout: t -> Layout.t
      (* lookup is for constants defined by _const, _build_const, and
       * _command_line_const.  It is set in main/compile.fun.
       *)
      val lookup: ({default: string option,
		    name: string} * ConstType.t -> t) ref
      val real: RealX.t -> t
      val string: string -> t
      val toString: t -> string
      val word: WordX.t -> t
      val word8: Word8.t -> t
      val wordVector: WordXVector.t -> t
   end
