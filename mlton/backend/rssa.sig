(* Copyright (C) 2009,2017 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature RSSA_STRUCTS = 
   sig
      include ATOMS

      structure Handler: HANDLER
      structure ObjectType: OBJECT_TYPE
      structure ObjptrTycon: OBJPTR_TYCON
      structure Return: RETURN
      structure Runtime: RUNTIME
      structure Scale: SCALE
      structure Type: REP_TYPE

      sharing Atoms = Type
      sharing Handler = Return.Handler
      sharing Label = Handler.Label = Return.Label
      sharing ObjectType = Type.ObjectType
      sharing ObjptrTycon = ObjectType.ObjptrTycon = Type.ObjptrTycon
      sharing Runtime = Type.Runtime
      sharing Scale = Type.Scale
   end

signature RSSA = 
   sig
      include RSSA_STRUCTS

      structure Switch: SWITCH
      sharing Atoms = Switch

      structure Operand:
         sig
            datatype t =
               ArrayOffset of {base: t,
                               index: t,
                               offset: Bytes.t,
                               scale: Scale.t,
                               ty: Type.t}
             | Cast of t * Type.t
             | Const of Const.t
               (* EnsuresBytesFree is a pseudo-op used by C functions (like
                * GC_allocateArray) that take a number of bytes as an argument
                * and ensure that that number of bytes is free upon return.
                * EnsuresBytesFree is replaced by the limit check pass with
                * a real operand.
                *)
             | EnsuresBytesFree
             | GCState
             | Offset of {base: t,
                          offset: Bytes.t,
                          ty: Type.t}
             | ObjptrTycon of ObjptrTycon.t
             | Runtime of Runtime.GCField.t
             | Var of {ty: Type.t,
                       var: Var.t}

            val bool: bool -> t
            val cast: t * Type.t -> t
            val layout: t -> Layout.t
            val null: t
            val replaceVar: t * (Var.t -> t) -> t
            val ty: t -> Type.t
            val word: WordX.t -> t
            val zero: WordSize.t -> t
         end
      sharing Operand = Switch.Use

      structure Statement:
         sig
            datatype t =
               Bind of {dst: Var.t * Type.t,
                        isMutable: bool,
                        src: Operand.t}
             | Move of {dst: Operand.t,
                        src: Operand.t}
             | Object of {dst: Var.t * Type.t,
                          header: word,
                          size: Bytes.t (* including header *)}
             | PrimApp of {args: Operand.t vector,
                           dst: (Var.t * Type.t) option,
                           prim: Type.t Prim.t}
             | Profile of ProfileExp.t
             | ProfileLabel of ProfileLabel.t
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
            val replaceUses: t * (Var.t -> Operand.t) -> t
            val resize: Operand.t * Type.t -> Operand.t * t list
            val toString: t -> string
         end

      structure Transfer:
         sig
            datatype t =
               Arith of {args: Operand.t vector,
                         dst: Var.t,
                         overflow: Label.t, (* Must be nullary. *)
                         prim: Type.t Prim.t,
                         success: Label.t, (* Must be nullary. *)
                         ty: Type.t}
             | CCall of {args: Operand.t vector,
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
            (* foldDef (t, a, f)
             * If t defines a variable x, then return f (x, a), else return a.
             *)
            val foldDef: t * 'a * (Var.t * Type.t * 'a -> 'a) -> 'a
            (* foreachDef (t, f) = foldDef (t, (), fn (x, ()) => f x) *)
            val foreachDef: t * (Var.t * Type.t -> unit) -> unit
            val foreachDefLabelUse: t * {def: Var.t * Type.t -> unit,
                                         label: Label.t -> unit,
                                         use: Var.t -> unit} -> unit
            val foreachFunc: t * (Func.t -> unit) -> unit
            val foreachLabel: t * (Label.t -> unit) -> unit
            val foreachUse: t * (Var.t -> unit) -> unit
            val ifBool: Operand.t * {falsee: Label.t, truee: Label.t} -> t
            (* in ifZero, the operand should be of type defaultWord *)
            val ifZero: Operand.t * {falsee: Label.t, truee: Label.t} -> t
            val layout: t -> Layout.t
            val replaceUses: t * (Var.t -> Operand.t) -> t
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
            val kind: t -> Kind.t
            val label: t -> Label.t
            val layout: t -> Layout.t
         end

      structure Function:
         sig
            type t

            val blocks: t -> Block.t vector
            val clear: t -> unit
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
            val foreachDef: t * (Var.t * Type.t -> unit) -> unit
            val foreachUse: t * (Var.t -> unit) -> unit
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
                     objectTypes: ObjectType.t vector}

            val clear: t -> unit
            val checkHandlers: t -> unit
            (* dfs (p, v) visits the functions in depth-first order, applying v f
             * for function f to yield v', then visiting b's descendents,
             * then applying v' ().
             *)
            val dfs: t * (Function.t -> unit -> unit) -> unit
            val dropProfile: t -> t
            val layouts: t * (Layout.t -> unit) -> unit
            val layoutStats: t -> Layout.t
            val orderFunctions: t -> t
            val shrink: t -> t
            val typeCheck: t -> unit
         end
   end
