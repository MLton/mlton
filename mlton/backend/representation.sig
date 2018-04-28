(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature REPRESENTATION_STRUCTS = 
   sig
      structure Rssa: RSSA
      structure Ssa: SSA2
      sharing Rssa.RealSize = Ssa.RealSize
      sharing Rssa.WordSize = Ssa.WordSize
   end

signature REPRESENTATION = 
   sig
      include REPRESENTATION_STRUCTS

      val compute:
         Ssa.Program.t
         -> {diagnostic: unit -> unit,
             genCase: {cases: {con: Ssa.Con.t,
                               dst: Rssa.Label.t,
                               dstHasArg: bool} vector,
                       default: Rssa.Label.t option,
                       test: unit -> Rssa.Operand.t,
                       tycon: Ssa.Tycon.t} -> (Rssa.Statement.t list
                                               * Rssa.Transfer.t
                                               * Rssa.Block.t list),
             object: {args: 'a vector,
                      con: Ssa.Con.t option,
                      dst: Rssa.Var.t * Rssa.Type.t,
                      objectTy: Ssa.Type.t,
                      oper: 'a -> Rssa.Operand.t} -> Rssa.Statement.t list,
             objectTypes: (Rssa.ObjptrTycon.t * Rssa.ObjectType.t) vector,
             select: {base: Rssa.Operand.t Ssa.Base.t,
                      baseTy: Ssa.Type.t,
                      dst: Rssa.Var.t * Rssa.Type.t,
                      offset: int} -> Rssa.Statement.t list,
             toRtype: Ssa.Type.t -> Rssa.Type.t option,
             update: {base: Rssa.Operand.t Ssa.Base.t,
                      baseTy: Ssa.Type.t,
                      offset: int,
                      value: Rssa.Operand.t} -> Rssa.Statement.t list}
   end
