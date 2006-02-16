(* Copyright (C) 2004-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature REP_TYPE_STRUCTS =
   sig
      structure CFunction: C_FUNCTION
      structure CType: C_TYPE
      structure Label: LABEL
      structure PointerTycon: POINTER_TYCON
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
      (*
       * - Junk is used for padding.  You can stick any value in, but you
       *   can't get any value out.
       * - In Seq, the components are listed in increasing order of
       *   address.
       * - In Seq ts, length ts <> 1
       * - In Sum ts, length ts >= 2
       * - In Sum ts, all t in ts must have same width.
       * - In Sum ts, there are no duplicates, and the types are in order.
       *)
      type t
      sharing type t = ObjectType.ty

      val add: t * t -> t
      val bogusWord: t -> WordX.t
      val address: t -> t
      val align: t * Bytes.t -> Bytes.t
      val andb: t * t -> t option
      val arrayOffsetIsOk: {base: t,
                            index: t,
                            offset: Bytes.t,
                            pointerTy: PointerTycon.t -> ObjectType.t,
                            result: t,
                            scale: Scale.t} -> bool
      val arshift: t * t -> t
      val bool: t
      val bytes: t -> Bytes.t
      val castIsOk: {from: t,
                     to: t,
                     tyconTy: PointerTycon.t -> ObjectType.t} -> bool
      val checkPrimApp: {args: t vector,
                         prim: t Prim.t,
                         result: t option} -> bool
      val char: t
      val cPointer: unit -> t
      val constant: WordX.t -> t
      val deLabel: t -> Label.t option
      val dePointer: t -> PointerTycon.t option
      val deReal: t -> RealSize.t option
      val defaultWord: t
      val equals: t * t -> bool
      val exnStack: t
      val gcState: t
      val intInf: t
      val isCPointer: t -> bool
      val isPointer: t -> bool
      val isUnit: t -> bool
      val isReal: t -> bool
      val isSubtype: t * t -> bool
      val isZero: t -> bool
      val junk: Bits.t -> t
      val label: Label.t -> t
      val layout: t -> Layout.t
      val lshift: t * t -> t
      val mul: t * t -> t
      val name: t -> string (* simple one letter abbreviation *)
      val ofGCField: Runtime.GCField.t -> t
      val ofWordVector: WordXVector.t -> t
      val offsetIsOk: {base: t,
                       offset: Bytes.t,
                       pointerTy: PointerTycon.t -> ObjectType.t,
                       result: t} -> bool
      val orb: t * t -> t option
      val pointer: PointerTycon.t -> t
      val pointerHeader: PointerTycon.t -> t
      val real: RealSize.t -> t
      val resize: t * Bits.t -> t
      val rshift: t * t -> t
      val seq: t vector -> t
      val string: t
      val sum: t vector -> t
      val thread: t
      val toCType: t -> CType.t
      val toString: t -> string
      val unit: t
      val width: t -> Bits.t
      val word8: t
      val word: Bits.t -> t
      val wordVector: Bits.t -> t
      val words: t -> Words.t
      val zero: Bits.t -> t

      structure BuiltInCFunction:
         sig
            val bug: t CFunction.t
            val gc: {maySwitchThreads: bool} -> t CFunction.t
         end
   end
