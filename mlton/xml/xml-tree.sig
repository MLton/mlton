(* Copyright (C) 2009,2017 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(* binding occurences:
 *   1. lambda arg
 *   2. pattern arg
 *   3. MonoVal dec
 *   4. PolyVal dec
 *   5. Fun dec
 *   6. Handle catch
 *)

signature XML_TREE_STRUCTS =
   sig
      include ATOMS
   end

signature XML_TREE =
   sig
      include XML_TREE_STRUCTS

      structure Type: XML_TYPE
      sharing Atoms = Type.Atoms

      structure Pat:
         sig
            datatype t = T of {arg: (Var.t * Type.t) option,
                               con: Con.t,
                               targs: Type.t vector}

            val falsee: t
            val truee: t
            val con: t -> Con.t
            val layout: t -> Layout.t
         end

      structure Cases:
         sig
            datatype 'a t =
               Con of (Pat.t * 'a) vector
             | Word of WordSize.t * (WordX.t * 'a) vector

            val fold: 'a t * 'b * ('a * 'b -> 'b) -> 'b
            val foreach: 'a t * ('a -> unit) -> unit
            val foreach': 'a t * ('a -> unit) * (Pat.t -> unit) -> unit
            val map: 'a t * ('a -> 'b) -> 'b t
         end

      structure Lambda:
         sig
            type exp
            type t

            val arg: t -> Var.t
            val body: t -> exp
            val dest: t -> {arg: Var.t,
                            argType: Type.t,
                            body: exp,
                            mayInline: bool}
            val equals: t * t -> bool
            val layout: t -> Layout.t
            val make: {arg: Var.t,
                       argType: Type.t,
                       body: exp,
                       mayInline: bool} -> t
            val mayInline: t -> bool
            val plist: t -> PropertyList.t
         end

      (* VarExp is a type application, variable applied to type args. *)
      structure VarExp:
         sig
            datatype t = T of {var: Var.t,
                               targs: Type.t vector}

            val equals: t * t -> bool
            val layout: t -> Layout.t
            val mono: Var.t -> t
            val var: t -> Var.t
         end

      structure PrimExp:
         sig
            type exp = Lambda.exp
            datatype t =
               App of {arg: VarExp.t,
                       func: VarExp.t}
             | Case of {cases: exp Cases.t,
                        default: (exp * Region.t) option,
                        test: VarExp.t}
             | ConApp of {arg: VarExp.t option,
                          con: Con.t,
                          targs: Type.t vector}
             | Const of Const.t
             | Handle of {(* catch binds the exception in the handler. *)
                          catch: Var.t * Type.t,
                          handler: exp,
                          try: exp}
             | Lambda of Lambda.t
             | PrimApp of {args: VarExp.t vector,
                           prim: Type.t Prim.t,
                           targs: Type.t vector}
             | Profile of ProfileExp.t
             | Raise of {exn: VarExp.t, extend: bool}
             | Select of {offset: int,
                          tuple: VarExp.t}
             | Tuple of VarExp.t vector
             | Var of VarExp.t

            val layout: t -> Layout.t
         end

      structure Dec:
         sig
            type exp = Lambda.exp

            datatype t =
               Exception of {arg: Type.t option,
                             con: Con.t}
             | Fun of {decs: {lambda: Lambda.t,
                              ty: Type.t,
                              var: Var.t} vector,
                       tyvars: Tyvar.t vector}
             | MonoVal of {exp: PrimExp.t,
                           ty: Type.t,
                           var: Var.t}
             | PolyVal of {exp: exp,
                           ty: Type.t,
                           tyvars: Tyvar.t vector,
                           var: Var.t}

            val layout: t -> Layout.t
         end

      structure Exp:
         sig
            type t = Lambda.exp

            val clear: t -> unit
            val decs: t -> Dec.t list
            val dest: t -> {decs: Dec.t list, result: VarExp.t}
            val enterLeave: t * Type.t * SourceInfo.t -> t
            (* foreach {exp, handleExp, handleBoundVar, handleVarExp}
             * applies handleExp to each subexpresison of e (including e)
             * applies handleBoundVar to each variable bound in e
             * applies handleVarExp to each variable expression in e
             * handleBoundVar will be called on a variable binding before
             * handleVarExp is called on any occurrences
             * handleExp is called on an expression after it is called on
             * all of its subexpressions
             *)
            val foreach:
               {exp: t,
                handleExp: t -> unit,
                handlePrimExp: Var.t * Type.t * PrimExp.t -> unit,
                handleBoundVar: Var.t * Tyvar.t vector * Type.t -> unit,
                handleVarExp: VarExp.t -> unit} -> unit
            val foreachBoundVar:
               t * (Var.t * Tyvar.t vector * Type.t -> unit) -> unit
            val foreachExp: t * (t -> unit) -> unit
            val foreachPrimExp: t * (Var.t * Type.t * PrimExp.t -> unit) -> unit
            val foreachVarExp: t * (VarExp.t -> unit) -> unit
            val fromPrimExp: PrimExp.t * Type.t -> t
            val hasPrim: t * (Type.t Prim.t -> bool) -> bool
            val layout: t -> Layout.t
            val make: {decs: Dec.t list, result: VarExp.t} -> t
            val prefix: t * Dec.t -> t
            val result: t -> VarExp.t
            val size: t -> int
         end

      structure DirectExp:
         sig
            type t

            val app: {func: t, arg: t, ty: Type.t} -> t
            val bug: string -> t
            val casee:
               {cases: t Cases.t,
                default: (t * Region.t) option,
                test: t,
                ty: Type.t} (* type of entire case expression *)
               -> t
            val conApp: {arg: t option,
                         con: Con.t,
                         targs: Type.t vector,
                         ty: Type.t} -> t
            val const: Const.t -> t
            val deref: t -> t
            val detuple: {tuple: t, body: (VarExp.t * Type.t) vector -> t} -> t
            val detupleBind: {tuple: t, components: Var.t vector, body: t} -> t
            val devector: {vector: t, length: int, body: (VarExp.t * Type.t) vector -> t} -> t
            val equal: t * t -> t
            val falsee: unit -> t
            val fromExp: Exp.t * Type.t -> t
            val fromLambda: Lambda.t * Type.t -> t
            val handlee: {catch: Var.t * Type.t,
                          handler: t,
                          try: t,
                          ty: Type.t} -> t
            val iff: {test: t, thenn: t, elsee: t, ty: Type.t} -> t
            val lambda: {arg: Var.t,
                         argType: Type.t,
                         body: t,
                         bodyType: Type.t,
                         mayInline: bool} -> t
            val let1: {var: Var.t, exp: t, body: t} -> t
            val lett: {decs: Dec.t list, body: t} -> t
            val monoVar: Var.t * Type.t -> t
            val primApp: {args: t vector,
                          prim: Type.t Prim.t,
                          targs: Type.t vector,
                          ty: Type.t} -> t
            val raisee: {exn: t, extend: bool, ty: Type.t} -> t
            val reff: t -> t
            val select: {tuple: t, offset: int, ty: Type.t} -> t
            val seq: t vector * (t vector -> t) -> t
            val sequence: t vector -> t
            val string: string -> t
            val toExp: t -> Exp.t
            val truee: unit -> t
            val tuple: {exps: t vector, ty: Type.t} -> t
            val unit: unit -> t
            val vall: {var: Var.t, exp: t} -> Dec.t list
            val var: {targs: Type.t vector,
                      ty: Type.t,
                      var: Var.t} -> t
            val varExp: VarExp.t * Type.t -> t
            val vectorLength: t -> t
         end

      structure Program:
         sig
            datatype t =
               T of {body: Exp.t,
                     datatypes: {cons: {arg: Type.t option,
                                        con: Con.t} vector,
                                 tycon: Tycon.t,
                                 tyvars: Tyvar.t vector} vector,
                     (* overflow is SOME only after exceptions have been
                      * implemented.
                      *)
                     overflow: Var.t option}

            val clear: t -> unit (* clear all property lists *)
            val layout: t -> Layout.t
            val layouts: t * (Layout.t -> unit) -> unit
            val layoutStats: t -> Layout.t
         end
   end
