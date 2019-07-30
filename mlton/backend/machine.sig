(* Copyright (C) 2009,2014,2019 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature MACHINE_STRUCTS = 
   sig
      include BACKEND_ATOMS
   end

signature MACHINE = 
   sig
      include MACHINE_STRUCTS

      structure Switch: SWITCH

      sharing Atoms = Switch

      structure ChunkLabel: ID

      structure Temporary:
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
            val layout: t -> Layout.t
            val new: Type.t -> t
            val numberOfType: CType.t -> int
            val ty: t -> Type.t
         end

      structure StackOffset:
         sig
            datatype t = T of {offset: Bytes.t,
                               ty: Type.t}

            val ty: t -> Type.t
         end

      structure Operand:
         sig
            datatype t =
               Cast of t * Type.t
             | Frontier
             | GCState
             | Global of Global.t
             | Label of Label.t
             | Null
             | Offset of {base: t,
                          offset: Bytes.t,
                          ty: Type.t}
             | Real of RealX.t
             | SequenceOffset of {base: t,
                                  index: t,
                                  offset: Bytes.t,
                                  scale: Scale.t,
                                  ty: Type.t}
             | StackOffset of StackOffset.t
             | StackTop
             | Temporary of Temporary.t
             | Word of WordX.t

            val equals: t * t -> bool
            val interfere: t * t -> bool
            val isLocation: t -> bool
            val layout: t -> Layout.t
            val stackOffset: {offset: Bytes.t, ty: Type.t} -> t
            val gcField: Runtime.GCField.t -> t
            val toString: t -> string
            val ty: t -> Type.t
         end
      sharing Operand = Switch.Use

      structure Live:
         sig
            datatype t =
               Global of Global.t
             | StackOffset of StackOffset.t
             | Temporary of Temporary.t

            val equals: t * t -> bool
            val fromOperand: Operand.t -> t option
            val layout: t -> Layout.t
            val toOperand: t -> Operand.t
            val ty: t -> Type.t
         end

      structure Statement:
         sig
            datatype t =
             (* When temporaries or offsets appear in operands, there is an
              * implicit contents of.
              * When they appear as locations, there is not.
              *)
               Move of {dst: Operand.t,
                        src: Operand.t}
             | PrimApp of {args: Operand.t vector,
                           dst: Operand.t option,
                           prim: Type.t Prim.t}
             | ProfileLabel of ProfileLabel.t

            val foldOperands: t * 'a * (Operand.t * 'a -> 'a) -> 'a
            val layout: t -> Layout.t
            val move: {dst: Operand.t, src: Operand.t} -> t option
            val object: {dst: Operand.t, header: word, size: Bytes.t} -> t vector
         end

      structure Transfer:
         sig
            datatype t =
               CCall of {args: Operand.t vector,
                         func: Type.t CFunction.t,
                         return: {return: Label.t (* must be CReturn *),
                                  size: Bytes.t option} option}
             | Call of {label: Label.t, (* must be kind Func *)
                        live: Live.t vector,
                        return: {return: Label.t (* must be kind Cont *),
                                 handler: Label.t option (* must be kind Handler*),
                                 size: Bytes.t} option}
             | Goto of Label.t (* must be kind Jump *)
             | Raise of {raisesTo: Label.t list}
             | Return of {returnsTo: Label.t list}
             | Switch of Switch.t

            val foldOperands: t * 'a * (Operand.t * 'a -> 'a) -> 'a
            val layout: t -> Layout.t
         end

      structure FrameOffsets:
         sig
            type t

            val equals: t * t -> bool
            val hash: t -> word
            val index: t -> int
            val layout: t -> Layout.t
            val new: {index: int, offsets: Bytes.t vector} -> t
            val offsets: t -> Bytes.t vector
         end

      structure FrameInfo:
         sig
            structure Kind:
               sig
                  datatype t = C_FRAME | ML_FRAME
                  val equals: t * t -> bool
                  val hash: t -> word
                  val layout: t -> Layout.t
                  val toString: t -> string
               end

            type t

            val equals: t * t -> bool
            val frameOffsets: t -> FrameOffsets.t
            val index: t -> int
            val kind: t -> Kind.t
            val layout: t -> Layout.t
            val new: {frameOffsets: FrameOffsets.t,
                      index: int,
                      kind: Kind.t,
                      size: Bytes.t,
                      sourceSeqIndex: int option} -> t
            val offsets: t -> Bytes.t vector
            val setIndex: t * int -> unit
            val size: t -> Bytes.t
            val sourceSeqIndex: t -> int option
         end

      structure Kind:
         sig
            datatype t =
               Cont of {args: Live.t vector,
                        frameInfo: FrameInfo.t}
             | CReturn of {dst: Live.t option,
                           frameInfo: FrameInfo.t option,
                           func: Type.t CFunction.t}
             | Func of {frameInfo: FrameInfo.t}
             | Handler of {args: Live.t vector,
                           frameInfo: FrameInfo.t}
             | Jump

            val isEntry: t -> bool
            val frameInfoOpt: t -> FrameInfo.t option
         end

      structure Block:
         sig
            datatype t =
               T of {kind: Kind.t,
                     label: Label.t,
                     (* Live temporaries and stack offsets at start of block. *)
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
                     (* Temporary.index r
                      *    <= tempsMax (Type.toCType (Temporary.ty r))
                      * for all temporaries in the chunk.
                      *)
                     tempsMax: CType.t -> int}
            val chunkLabel: t -> ChunkLabel.t
         end

      structure Program:
         sig
            datatype t =
               T of {chunks: Chunk.t list,
                     frameInfos: FrameInfo.t vector,
                     frameOffsets: FrameOffsets.t vector,
                     handlesSignals: bool,
                     main: {chunkLabel: ChunkLabel.t,
                            label: Label.t},
                     maxFrameSize: Bytes.t,
                     objectTypes: Type.ObjectType.t vector,
                     reals: (Global.t * RealX.t) list,
                     sourceMaps: SourceMaps.t option,
                     vectors: (Global.t * WordXVector.t) list}

            val clearLabelNames: t -> unit
            val layouts: t * (Layout.t -> unit) -> unit
            val layoutStats: t -> Layout.t
            val toFile: {display: t Control.display, style: Control.style, suffix: string}
            val typeCheck: t -> unit
         end

      val simplify: Program.t -> Program.t
   end
