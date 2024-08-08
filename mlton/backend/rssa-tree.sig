(* Copyright (C) 2009,2017,2019-2021,2024 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature RSSA_TREE_STRUCTS =
   sig
      include BACKEND_ATOMS
   end

signature RSSA_TREE =
   sig
      include RSSA_TREE_STRUCTS

      structure Operand:
         sig
            datatype t =
               Cast of t * Type.t
             | Const of Const.t
             | GCState
             | Offset of {base: t,
                          offset: Bytes.t,
                          ty: Type.t}
             | ObjptrTycon of ObjptrTycon.t
             | Runtime of Runtime.GCField.t
             | SequenceOffset of {base: t,
                                  index: t,
                                  offset: Bytes.t,
                                  scale: Scale.t,
                                  ty: Type.t}
             | Var of {ty: Type.t,
                       var: Var.t}

            val bool: bool -> t
            val cast: t * Type.t -> t
            val layout: t -> Layout.t
            val one: WordSize.t -> t
            val null: t
            val replace: t * {const: Const.t -> t,
                              var: {ty: Type.t, var: Var.t} -> t} -> t
            val ty: t -> Type.t
            val word: WordX.t -> t
            val zero: WordSize.t -> t
         end

      structure Object: OBJECT
      sharing Object = BackendAtoms
      sharing Object.Use = Operand

      structure Statement:
         sig
            datatype t =
               Bind of {dst: Var.t * Type.t,
                        pinned: bool,
                        src: Operand.t}
             | Move of {dst: Operand.t,
                        src: Operand.t}
             | Object of {dst: Var.t * Type.t,
                          obj: Object.t}
             | PrimApp of {args: Operand.t vector,
                           dst: (Var.t * Type.t) option,
                           prim: Type.t Prim.t}
             | Profile of ProfileExp.t
             | SetExnStackLocal
             | SetExnStackSlot
             | SetHandler of Label.t (* label must be of Handler kind. *)
             | SetSlotExnStack

            (* foldDef (s, a, f)
             * If s defines a variable x, then return f (x, a), else return a.
             *)
            val foldDef: t * 'a * (Var.t * Type.t * 'a -> 'a) -> 'a
            (* foreachDef (s, f) = foldDef (s, (), fn (x, ()) => f x) *)
            val foreachDef: t * (Var.t * Type.t -> unit) -> unit
            val foreachDefUse: t * {def: (Var.t * Type.t) -> unit,
                                    use: Var.t -> unit} -> unit
            val foldUse: t * 'a * (Var.t * 'a -> 'a) -> 'a
            val foreachUse: t * (Var.t -> unit) -> unit
            val layout: t -> Layout.t
            val replace: t * {const: Const.t -> Operand.t,
                              var: {var: Var.t, ty: Type.t} -> Operand.t} -> t
            val resize: Operand.t * Type.t -> Operand.t * t list
            val toString: t -> string
         end

      structure Switch: SWITCH
      sharing Switch = Atoms
      sharing Switch.Use = Operand

      structure Transfer:
         sig
            datatype t =
               CCall of {args: Operand.t vector,
                         func: Type.t CFunction.t,
                         (* return is NONE iff the CFunction doesn't return.
                          * Else, return must be SOME l, where l is of kind
                          * CReturn.  The return should be nullary if the C
                          * function returns void.  Else, it should be unary with
                          * a var of the appropriate type to accept the result.
                          *)
                         return: Label.t option}
             | Call of {args: Operand.t vector,
                        func: Func.t,
                        return: Return.t}
             | Goto of {args: Operand.t vector,
                        dst: Label.t}
             (* Raise implicitly raises to the caller.  
              * I.E. the local handler stack must be empty.
              *)
             | Raise of Operand.t vector
             | Return of Operand.t vector
             | Switch of Switch.t

            val bug: unit -> t
            val foreachLabelUse: t * {label: Label.t -> unit,
                                      use: Var.t -> unit} -> unit
            val foreachFunc: t * (Func.t -> unit) -> unit
            val foreachLabel: t * (Label.t -> unit) -> unit
            val foreachUse: t * (Var.t -> unit) -> unit
            val ifBool: Operand.t * {falsee: Label.t, truee: Label.t} -> t
            val ifBoolE: Operand.t * bool option * {falsee: Label.t, truee: Label.t} -> t
            (* in ifZero, the operand should be of type defaultWord *)
            val ifZero: Operand.t * {falsee: Label.t, truee: Label.t} -> t
            val layout: t -> Layout.t
            val replace: t * {const: Const.t -> Operand.t,
                              label: Label.t -> Label.t,
                              var: {var: Var.t, ty: Type.t} -> Operand.t} -> t
            val replaceLabels: t * (Label.t -> Label.t) -> t
         end

      structure Kind:
         sig
            datatype t =
               Cont of {handler: Handler.t}
             | CReturn of {func: Type.t CFunction.t}
             | Handler
             | Jump

            datatype frameStyle = None | OffsetsAndSize | SizeOnly
            val frameStyle: t -> frameStyle
            val isJump: t -> bool
            val layout: t -> Layout.t
         end

      structure Block:
         sig
            datatype t =
               T of {args: (Var.t * Type.t) vector,
                     kind: Kind.t,
                     label: Label.t,
                     statements: Statement.t vector,
                     transfer: Transfer.t}

            val clear: t -> unit
            val foreachDef: t * (Var.t * Type.t -> unit) -> unit
            val foreachUse: t * (Var.t -> unit) -> unit
            val kind: t -> Kind.t
            val label: t -> Label.t
            val layout: t -> Layout.t
         end

      structure Function:
         sig
            type t

            val blocks: t -> Block.t vector
            val clear: t -> unit
            val controlFlow:
               t -> {graph: unit DirectedGraph.t,
                     labelNode: Label.t -> unit DirectedGraph.Node.t,
                     nodeBlock: unit DirectedGraph.Node.t -> Block.t}
            val dest: t -> {args: (Var.t * Type.t) vector,
                            blocks: Block.t vector,
                            name: Func.t,
                            raises: Type.t vector option,
                            returns: Type.t vector option,
                            start: Label.t}
            (* dfs (f, v) visits the blocks in depth-first order, applying v b
             * for block b to yield v', then visiting b's descendents,
             * then applying v' ().
             *)
            val dfs: t * (Block.t -> unit -> unit) -> unit
            val dominatorTree: t -> Block.t Tree.t
            val foreachDef: t * (Var.t * Type.t -> unit) -> unit
            val foreachUse: t * (Var.t -> unit) -> unit
            val layout: t -> Layout.t
            val layoutDot:
               t -> {destroy: unit -> unit,
                     controlFlowGraph: Layout.t,
                     dominatorTree: unit -> Layout.t,
                     loopForest: unit -> Layout.t}
            val layoutHeader: t -> Layout.t
            (* Produce a loop forest, with an optional predicate;
             * the start node will be connected when
             * the predicate fails, to maintain connectedness *)
            val loopForest: t * (Block.t * Block.t -> bool) -> Block.t DirectedGraph.LoopForest.t
            val name: t -> Func.t
            val new: {args: (Var.t * Type.t) vector,
                      blocks: Block.t vector,
                      name: Func.t,
                      raises: Type.t vector option,
                      returns: Type.t vector option,
                      start: Label.t} -> t
         end

      structure Program:
         sig
            datatype t =
               T of {functions: Function.t list,
                     handlesSignals: bool,
                     main: Function.t,
                     objectTypes: ObjectType.t vector,
                     profileInfo: {sourceMaps: SourceMaps.t,
                                   getFrameSourceSeqIndex: Label.t -> int option} option,
                     statics: {dst: Var.t * Type.t, obj: Object.t} vector}

            val clear: t -> unit
            (* dfs (p, v) visits the functions in depth-first order, applying v f
             * for function f to yield v', then visiting b's descendents,
             * then applying v' ().
             *)
            val dfs: t * (Function.t -> unit -> unit) -> unit
            val dropProfile: t -> t
            val layouts: t * (Layout.t -> unit) -> unit
            val layoutStats: t -> Layout.t
            val orderFunctions: t -> t
            val rflow: t -> (Func.t -> {raisesTo: Label.t list,
                                        returnsTo: Label.t list})
            val shuffle: t -> t
            val toFile: {display: t Control.display, style: Control.style, suffix: string}
         end
   end
