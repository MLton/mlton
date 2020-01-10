(* Copyright (C) 2020 Matthew Fluet.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature OBJECT_STRUCTS =
   sig
      structure ObjptrTycon: OBJPTR_TYCON
      structure ObjectType: OBJECT_TYPE
      structure Runtime: RUNTIME
      structure Type: REP_TYPE
      structure WordSize: WORD_SIZE
      structure WordX: WORD_X
      structure WordXVector: WORD_X_VECTOR
      sharing ObjptrTycon = ObjectType.ObjptrTycon = Type.ObjptrTycon
      sharing ObjectType = Type.ObjectType
      sharing Runtime = ObjectType.Runtime = ObjptrTycon.Runtime = Type.Runtime
      sharing WordSize = ObjptrTycon.WordSize = Type.WordSize = WordX.WordSize = WordXVector.WordSize
      sharing WordX = ObjptrTycon.WordX = Type.WordX = WordXVector.WordX
      sharing WordXVector = Type.WordXVector

      structure Use: sig
                        type t

                        val deWord: t -> WordX.t option
                        val layout: t -> Layout.t
                        val ty: t -> Type.t
                        val word: WordX.t -> t
                     end
   end

signature OBJECT =
   sig
      include OBJECT_STRUCTS

      datatype t =
         Normal of {init: {offset: Bytes.t,
                           src: Use.t} vector,
                    ty: Type.t,
                    tycon: ObjptrTycon.t}
       | Sequence of {elt: Type.t,
                      init: {offset: Bytes.t,
                             src: Use.t} vector vector,
                      tycon: ObjptrTycon.t}

      val deString: {elt: Type.t,
                     init: {offset: Bytes.t,
                            src: Use.t} vector vector,
                     tycon: ObjptrTycon.t} -> string option
      val foldUse: t * 'a * (Use.t * 'a -> 'a) -> 'a
      val foreachUse: t * (Use.t -> unit) -> unit
      val fromWordXVector: WordXVector.t -> t
      val isOk: t * {checkUse: Use.t -> unit,
                     tyconTy: ObjptrTycon.t -> ObjectType.t} -> bool
      val layout: t -> Layout.t
      val metaDataSize: t -> Bytes.t
      val replace: t * {use: Use.t -> Use.t} -> t
      val size: t -> Bytes.t
      val toString: t -> string
      val ty: t -> Type.t
   end
