(* Copyright (C) 2009,2017,2019,2025 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature SSA_TREE_STRUCTS = 
   sig
      include ATOMS
   end

signature SSA_TREE = 
   sig
      include SSA_TREE_STRUCTS

      structure Type:
         sig
            type t

            datatype dest =
               Array of t
             | CPointer
             | Datatype of Tycon.t
             | IntInf
             | Real of RealSize.t
             | Ref of t
             | Thread
             | Tuple of t vector
             | Vector of t
             | Weak of t
             | Word of WordSize.t

            val array: t -> t
            val bool: t
            val checkPrimApp: {targs: t vector,
                               args: t vector,
                               prim: t Prim.t,
                               result: t} -> bool
            val cpointer: t
            val datatypee: Tycon.t -> t
            val dest: t -> dest
            val deArray: t -> t
            val deDatatype: t -> Tycon.t
            val deRef: t -> t
            val deTuple: t -> t vector
            val deTupleOpt: t -> t vector option
            val deVector: t -> t
            val deWeak: t -> t
            val deWord: t -> WordSize.t
            val deWordOpt: t -> WordSize.t option
            val equals: t * t -> bool
            val hash: t -> word
            val intInf: t
            val isTuple: t -> bool
            val isUnit: t -> bool
            val layout: t -> Layout.t
            val ofConst: Const.t -> t
            val plist: t -> PropertyList.t
            val real: RealSize.t -> t
            val reff: t -> t
            val thread: t
            val tuple: t vector -> t
            val vector: t -> t
            val weak: t -> t
            val word: WordSize.t -> t
            val unit: t
         end

      structure Exp:
         sig
            datatype t =
               ConApp of {args: Var.t vector,
                          con: Con.t}
             | Const of Const.t
             | PrimApp of {args: Var.t vector,
                           prim: Type.t Prim.t,
                           targs: Type.t vector}
             | Profile of ProfileExp.t
             | Select of {offset: int,
                          tuple: Var.t}
             | Tuple of Var.t vector
             | Var of Var.t

            val equals: t * t -> bool
            val foreachVar: t * (Var.t -> unit) -> unit
            val hash: t -> Word.t
            val isProfile: t -> bool
            val layout: t -> Layout.t
            val maySideEffect: t -> bool
            val replaceVar: t * (Var.t -> Var.t) -> t
            val size: t -> int
            val unit: t
         end

      structure Statement:
         sig
            datatype t = T of {exp: Exp.t,
                               ty: Type.t,
                               var: Var.t option}

            val clear: t -> unit (* clear the var *)
            val exp: t -> Exp.t
            val isProfile: t -> bool
            val layout: t -> Layout.t
            val profile: ProfileExp.t -> t
            val var: t -> Var.t option
         end

      structure Transfer:
         sig
            datatype t =
               Bug  (* MLton thought control couldn't reach here. *)
             | Call of {args: Var.t vector,
                        func: Func.t,
                        return: Return.t}
             | Case of {cases: (Con.t, Label.t) Cases.t,
                        default: Label.t option, (* Must be nullary. *)
                        test: Var.t}
             | Goto of {args: Var.t vector,
                        dst: Label.t}
             (* Raise implicitly raises to the caller.
              * I.E. the local handler stack must be empty.
              *)
             | Raise of Var.t vector
             | Return of Var.t vector
             | Runtime of {args: Var.t vector,
                           prim: Type.t Prim.t,
                           return: Label.t}

            val equals: t * t -> bool
            val foreachFunc : t * (Func.t -> unit) -> unit
            val foreachLabel: t * (Label.t -> unit) -> unit
            val foreachLabelVar: t * (Label.t -> unit) * (Var.t -> unit) -> unit
            val foreachVar: t * (Var.t -> unit) -> unit
            val hash: t -> Word.t
            val layout: t -> Layout.t
            val replaceLabelVar: t * (Label.t -> Label.t) * (Var.t -> Var.t) -> t
            val replaceLabel: t * (Label.t -> Label.t) -> t
            val replaceVar: t * (Var.t -> Var.t) -> t
            val size: t -> int
         end

      structure Block:
         sig
            datatype t =
               T of {args: (Var.t * Type.t) vector,
                     label: Label.t,
                     statements: Statement.t vector,
                     transfer: Transfer.t}

            val args: t -> (Var.t * Type.t) vector
            val clear: t -> unit
            val label: t -> Label.t
            val layout: t -> Layout.t
            val sizeV: t vector * {sizeExp: Exp.t -> int, sizeTransfer: Transfer.t -> int} -> int
            val statements: t -> Statement.t vector
            val transfer: t -> Transfer.t
         end

      structure Datatype:
         sig
            datatype t =
               T of {cons: {args: Type.t vector,
                            con: Con.t} vector,
                     tycon: Tycon.t}

            val layout: t -> Layout.t
         end

      structure Function:
         sig
            type t

            val alphaRename: t -> t
            val blocks: t -> Block.t vector
            (* clear the plists for all bound variables and labels that appear
             * in the function, but not the function name's plist.
             *)
            val clear: t -> unit
            val controlFlow:
               t -> {graph: unit DirectedGraph.t,
                     labelNode: Label.t -> unit DirectedGraph.Node.t,
                     nodeBlock: unit DirectedGraph.Node.t -> Block.t}
            val dest: t -> {args: (Var.t * Type.t) vector,
                            blocks: Block.t vector,
                            mayInline: bool,
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
            val foreachVar: t * (Var.t * Type.t -> unit) -> unit
            val layout: t -> Layout.t
            val layoutDot:
               t * (Var.t -> Layout.t) -> {destroy: unit -> unit,
                                           controlFlowGraph: Layout.t,
                                           dominatorTree: unit -> Layout.t,
                                           loopForest: unit -> Layout.t}
            val mayInline: t -> bool
            val name: t -> Func.t
            val new: {args: (Var.t * Type.t) vector,
                      blocks: Block.t vector,
                      mayInline: bool,
                      name: Func.t,
                      raises: Type.t vector option,
                      returns: Type.t vector option,
                      start: Label.t} -> t
            val profile: t * SourceInfo.t -> t
            val size: t * {sizeExp: Exp.t -> int, sizeTransfer: Transfer.t -> int} -> int
            val sizeMax: t * {max: int option, sizeExp: Exp.t -> int, sizeTransfer: Transfer.t -> int} -> int option
         end

      structure Program:
         sig
            datatype t =
               T of {datatypes: Datatype.t vector,
                     functions: Function.t list,
                     globals: Statement.t vector,
                     main: Func.t (* Must be nullary. *)}

            val clear: t -> unit
            val clearTop: t -> unit
            (* dfs (p, v) visits the functions in depth-first order, applying v f
             * for function f to yield v', then visiting f's descendents,
             * then applying v' ().
             *)
            val dfs: t * (Function.t -> unit -> unit) -> unit
            val foreachPrim: t * (Type.t Prim.t -> unit) -> unit
            val foreachVar: t * (Var.t * Type.t -> unit) -> unit
            val hasPrim: t * (Type.t Prim.t -> bool) -> bool
            val layouts: t * (Layout.t -> unit) -> unit
            val layoutStats: t -> Layout.t
            val mainFunction: t -> Function.t
            val parse: unit -> t Parse.t
            val toFile: {style: Control.style, suffix: string, display: t Control.display}
         end
   end
