(* Copyright (C) 2009,2014,2017 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature CONST_STRUCTS = 
   sig
      structure RealX: REAL_X
      structure WordX: WORD_X
      structure WordXVector: WORD_X_VECTOR
      sharing WordX = RealX.WordX = WordXVector.WordX
   end

signature CONST = 
   sig
      include CONST_STRUCTS

      structure ConstType: CONST_TYPE
      sharing ConstType.RealSize = RealX.RealSize
      sharing ConstType.WordSize = WordX.WordSize

      structure IntInfRep:
         sig
            datatype t = Big of WordXVector.t | Small of WordX.t
            val bigToIntInf: WordXVector.t -> IntInf.t option
            val fromIntInf: IntInf.t -> t
            val smallToIntInf: WordX.t -> IntInf.t option
         end

      datatype t =
         IntInf of IntInf.t
       | Null
       | Real of RealX.t
       | Word of WordX.t
       | WordVector of WordXVector.t

      val deWord: t -> WordX.t
      val deWordOpt: t -> WordX.t option
      val equals: t * t -> bool
      val intInf: IntInf.t -> t
      val hash: t -> word
      val layout: t -> Layout.t
      (* lookup is for constants defined by _const, _build_const, and
       * _command_line_const.  It is set in main/compile.fun.
       *)
      val lookup: ({default: string option,
                    name: string} * ConstType.t -> t) ref
      val null: t
      val real: RealX.t -> t
      val string: string -> t
      val toString: t -> string
      val word: WordX.t -> t
      val wordVector: WordXVector.t -> t
   end
