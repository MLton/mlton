(* Copyright (C) 2009,2014 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature MACHINE_STRUCTS = 
   sig
      include ATOMS
   end

signature MACHINE = 
   sig
      include MACHINE_STRUCTS

      structure ObjectType: OBJECT_TYPE
      structure ObjptrTycon: OBJPTR_TYCON
      structure Runtime: RUNTIME
      structure Switch: SWITCH
      structure Type: REP_TYPE

      sharing Atoms = Type
      sharing Atoms = Switch
      sharing ObjectType = Type.ObjectType
      sharing ObjptrTycon = ObjectType.ObjptrTycon = Type.ObjptrTycon
      sharing Runtime = ObjectType.Runtime = Type.Runtime

      structure ChunkLabel: ID

      structure Register:
         sig
            type t

            val equals: t * t -> bool
            val index: t -> int
            val indexOpt: t -> int option
            val layout: t -> Layout.t
            val new: Type.t * int option -> t
            val setIndex: t * int -> unit
            val toString: t -> string
            val ty: t -> Type.t
         end

      structure Global:
         sig
            type t

            val equals: t * t -> bool
            val index: t -> int
            val isRoot: t -> bool
            val layout: t -> Layout.t
            val new: {isRoot: bool, ty: Type.t} -> t
            val numberOfNonRoot: unit -> int
            val numberOfType: CType.t -> int
            val ty: t -> Type.t
         end

      structure StackOffset:
         sig
            datatype t = T of {offset: Bytes.t,
                               ty: Type.t}

            val ty: t -> Type.t
         end

      structure Scale: SCALE
      sharing Scale = Type.Scale

      structure Operand:
         sig
            datatype t =
               ArrayOffset of {base: t,
                               index: t,
                               offset: Bytes.t,
                               scale: Scale.t,
                               ty: Type.t}
             | Cast of t * Type.t
             | Contents of {oper: t,
                            ty: Type.t}
             | Frontier
             | GCState
             | Global of Global.t
             | Label of Label.t
             | Null
             | Offset of {base: t,
                          offset: Bytes.t,
                          ty: Type.t}
             | Real of RealX.t
             | Register of Register.t
             | StackOffset of StackOffset.t
             | StackTop
             | Word of WordX.t

            val equals: t * t -> bool
            val interfere: t * t -> bool
            val isLocation: t -> bool
            val layout: t -> Layout.t
            val stackOffset: {offset: Bytes.t, ty: Type.t} -> t
            val toString: t -> string
            val ty: t -> Type.t
         end
      sharing Operand = Switch.Use

      structure Live:
         sig
            datatype t =
               Global of Global.t
             | Register of Register.t
             | StackOffset of StackOffset.t

            val equals: t * t -> bool
            val fromOperand: Operand.t -> t option
            val layout: t -> Layout.t
            val toOperand: t -> Operand.t
            val ty: t -> Type.t
         end

      structure Statement:
         sig
            datatype t =
             (* When registers or offsets appear in operands, there is an
              * implicit contents of.
              * When they appear as locations, there is not.
              *)
               Move of {dst: Operand.t,
                        src: Operand.t}
             | Noop
             | PrimApp of {args: Operand.t vector,
                           dst: Operand.t option,
                           prim: Type.t Prim.t}
             | ProfileLabel of ProfileLabel.t

            val foldOperands: t * 'a * (Operand.t * 'a -> 'a) -> 'a
            val layout: t -> Layout.t
            val move: {dst: Operand.t, src: Operand.t} -> t
            (* Error if dsts and srcs aren't of same length. *)
            val moves: {dsts: Operand.t vector,
                        srcs: Operand.t vector} -> t vector
            val object: {dst: Operand.t, header: word, size: Bytes.t} -> t vector
         end

      structure FrameInfo:
         sig
            datatype t = T of {frameLayoutsIndex: int}

            val equals: t * t -> bool
            val layout: t -> Layout.t
         end

      structure Transfer:
         sig
            datatype t =
               (* In an arith transfer, dst is modified whether or not the
                * prim succeeds.
                *)
               Arith of {args: Operand.t vector,
                         dst: Operand.t,
                         overflow: Label.t,
                         prim: Type.t Prim.t,
                         success: Label.t}
             | CCall of {args: Operand.t vector,
                         frameInfo: FrameInfo.t option,
                         func: Type.t CFunction.t,
                         (* return is NONE iff the func doesn't return.
                          * Else, return must be SOME l, where l is of CReturn
                          * kind with a matching func.
                          *)
                         return: Label.t option}
             | Call of {label: Label.t, (* label must be a Func *)
                        live: Live.t vector,
                        return: {return: Label.t,
                                 handler: Label.t option,
                                 size: Bytes.t} option}
             | Goto of Label.t (* label must be a Jump *)
             | Raise
             | Return
             | Switch of Switch.t

            val foldOperands: t * 'a * (Operand.t * 'a -> 'a) -> 'a
            val layout: t -> Layout.t
         end

      structure Kind:
         sig
            datatype t =
               Cont of {args: Live.t vector,
                        frameInfo: FrameInfo.t}
             | CReturn of {dst: Live.t option,
                           frameInfo: FrameInfo.t option,
                           func: Type.t CFunction.t}
             | Func
             | Handler of {frameInfo: FrameInfo.t,
                           handles: Live.t vector}
             | Jump

            val frameInfoOpt: t -> FrameInfo.t option
         end

      structure Block:
         sig
            datatype t =
               T of {kind: Kind.t,
                     label: Label.t,
                     (* Live registers and stack offsets at start of block. *)
                     live: Live.t vector,
                     raises: Live.t vector option,
                     returns: Live.t vector option,
                     statements: Statement.t vector,
                     transfer: Transfer.t}

            val foldDefs: t * 'a * (Operand.t * 'a -> 'a) -> 'a
            val label: t -> Label.t
         end

      structure Chunk:
         sig
            datatype t =
               T of {blocks: Block.t vector,
                     chunkLabel: ChunkLabel.t,
                     (* Register.index r
                      *    <= regMax (Type.toCType (Register.ty r))
                      * for all registers in the chunk.
                      *)
                     regMax: CType.t -> int}
         end

      structure ProfileInfo:
         sig
            datatype t =
               T of {(* For each frame, gives the index into sourceSeqs of the
                      * source functions corresponding to the frame.
                      *)
                     frameSources: int vector,
                     labels: {label: ProfileLabel.t,
                              sourceSeqsIndex: int} vector,
                     names: string vector,
                     (* Each sourceSeq describes a sequence of source functions,
                      * each given as an index into the source vector.
                      *)
                     sourceSeqs: int vector vector,
                     sources: {nameIndex: int,
                               successorsIndex: int} vector}

            val empty: t
            val modify: t -> {newProfileLabel: ProfileLabel.t -> ProfileLabel.t,
                              delProfileLabel: ProfileLabel.t -> unit,
                              getProfileInfo: unit -> t}
         end

      structure Program:
         sig
            datatype t =
               T of {chunks: Chunk.t list,
                     frameLayouts: {frameOffsetsIndex: int,
                                    isC: bool,
                                    size: Bytes.t} vector,
                     (* Each vector in frame Offsets specifies the offsets
                      * of live pointers in a stack frame.  A vector is referred
                      * to by index as the offsetsIndex in frameLayouts.
                      *)
                     frameOffsets: Bytes.t vector vector,
                     handlesSignals: bool,
                     main: {chunkLabel: ChunkLabel.t,
                            label: Label.t},
                     maxFrameSize: Bytes.t,
                     objectTypes: Type.ObjectType.t vector,
                     profileInfo: ProfileInfo.t option,
                     reals: (Global.t * RealX.t) list,
                     vectors: (Global.t * WordXVector.t) list}

            val frameSize: t * FrameInfo.t -> Bytes.t
            val clearLabelNames: t -> unit
            val layouts: t * (Layout.t -> unit) -> unit
            val typeCheck: t -> unit
         end
   end
