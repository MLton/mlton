(* Copyright (C) 2014,2017 Matthew Fluet.
 * Copyright (C) 2004-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature REP_TYPE_STRUCTS =
   sig
      structure CFunction: C_FUNCTION
      structure CType: C_TYPE
      structure Label: LABEL
      structure ObjptrTycon: OBJPTR_TYCON
      structure Prim: PRIM
      structure RealSize: REAL_SIZE
      structure Runtime: RUNTIME
      structure Scale: SCALE
      structure WordSize: WORD_SIZE
      structure WordX: WORD_X
      structure WordXVector: WORD_X_VECTOR
      sharing CFunction = Prim.CFunction
      sharing RealSize = Prim.RealSize
      sharing WordSize = Prim.WordSize = WordX.WordSize
      sharing WordX = WordXVector.WordX
   end

signature REP_TYPE =
   sig
      include REP_TYPE_STRUCTS

      structure ObjectType: OBJECT_TYPE
      type t
      sharing type t = ObjectType.ty

      val bogusWord: t -> WordX.t
      val align: t * Bytes.t -> Bytes.t
      val arrayOffsetIsOk: {base: t,
                            index: t,
                            offset: Bytes.t,
                            tyconTy: ObjptrTycon.t -> ObjectType.t,
                            result: t,
                            scale: Scale.t} -> bool
      val bits: Bits.t -> t
      val bool: t
      val bytes: t -> Bytes.t
      val castIsOk: {from: t,
                     to: t,
                     tyconTy: ObjptrTycon.t -> ObjectType.t} -> bool
      val checkPrimApp: {args: t vector,
                         prim: t Prim.t,
                         result: t option} -> bool
      val cpointer: unit -> t
      val csize: unit -> t
      val cint: unit -> t
      val compareRes: t
      val deLabel: t -> Label.t option
      val deObjptr: t -> ObjptrTycon.t option
      val deReal: t -> RealSize.t option
      val deSeq: t -> t vector option
      val deWord: t -> WordSize.t option
      val equals: t * t -> bool
      val exnStack: unit -> t
      val gcState: unit -> t
      val exists: t * (t -> bool) -> bool
      val intInf: unit -> t
      val isCPointer: t -> bool
      val isObjptr: t -> bool
      val isUnit: t -> bool
      val isSubtype: t * t -> bool
      val label: Label.t -> t
      val layout: t -> Layout.t
      val name: t -> string (* simple one letter abbreviation *)
      val ofGCField: Runtime.GCField.t -> t
      val ofWordXVector: WordXVector.t -> t
      val ofWordX: WordX.t -> t
      val offsetIsOk: {base: t,
                       offset: Bytes.t,
                       tyconTy: ObjptrTycon.t -> ObjectType.t,
                       result: t} -> bool
      val objptr: ObjptrTycon.t -> t
      val objptrHeader: unit -> t
      val real: RealSize.t -> t
      val resize: t * Bits.t -> t
      val seq: t vector -> t
      val seqIndex: unit -> t
      val shiftArg: t
      val string: unit -> t
      val sum: t vector -> t
      val thread: unit -> t
      val toCType: t -> CType.t
      val unit: t
      val width: t -> Bits.t
      val word: WordSize.t -> t
      val wordVector: WordSize.t -> t
      val zero: Bits.t -> t

      structure BuiltInCFunction:
         sig
            val bug: unit -> t CFunction.t
            val gc: {maySwitchThreads: bool} -> t CFunction.t
         end
   end
