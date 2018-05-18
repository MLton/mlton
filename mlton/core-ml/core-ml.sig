(* Copyright (C) 2015,2017 Matthew Fluet
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature CORE_ML_STRUCTS = 
   sig
      include ATOMS
      structure Type:
         sig
            type t

            val arrow: t * t -> t
            val bool: t
            val deConOpt: t -> (Tycon.t * t vector) option
            val deRecord: t -> (Record.Field.t * t) vector
            val isCharX: t -> bool
            val isInt: t -> bool
            val layout: t -> Layout.t
            val makeHom: {con: Tycon.t * 'a vector -> 'a,
                          var: Tyvar.t -> 'a} -> {destroy: unit -> unit,
                                                  hom: t -> 'a}
            val tuple: t vector -> t
            val unit: t
         end
   end

signature CORE_ML = 
   sig
      include CORE_ML_STRUCTS

      structure Pat:
         sig
            type t
            datatype node =
               Con of {arg: t option,
                       con: Con.t,
                       targs: Type.t vector}
             | Const of unit -> Const.t
             | Layered of Var.t * t
             | List of t vector
             | Or of t vector
             | Record of t Record.t
             | Var of Var.t
             | Vector of t vector
             | Wild

            val dest: t -> node * Type.t
            val falsee: t
            val foreachVar: t * (Var.t -> unit) -> unit
            (* true if pattern contains a constant, constructor or variable *)
            val isRefutable: t -> bool
            val isUnit: t -> bool
            val isWild: t -> bool
            val layout: t -> Layout.t
            val make: node * Type.t -> t
            val node: t -> node
            val var: Var.t * Type.t -> t
            val truee: t
            val tuple: t vector -> t
            val ty: t -> Type.t
            val wild: Type.t -> t
         end

      structure Exp:
         sig
            type dec
            type lambda
            type t
            datatype noMatch = Impossible | RaiseAgain | RaiseBind | RaiseMatch
            datatype node =
               App of t * t
             | Case of {ctxt: unit -> Layout.t,
                        kind: string * string,
                        nest: string list,
                        matchDiags: {nonexhaustiveExn: Control.Elaborate.DiagDI.t,
                                     nonexhaustive: Control.Elaborate.DiagEIW.t,
                                     redundant: Control.Elaborate.DiagEIW.t},
                        noMatch: noMatch,
                        region: Region.t,
                        rules: {exp: t,
                                layPat: (unit -> Layout.t) option,
                                pat: Pat.t,
                                regionPat: Region.t} vector,
                        test: t}
             | Con of Con.t * Type.t vector
             | Const of unit -> Const.t
             | EnterLeave of t * SourceInfo.t
             | Handle of {catch: Var.t * Type.t,
                          handler: t,
                          try: t}
             | Lambda of lambda
             | Let of dec vector * t
             | List of t vector
             | PrimApp of {args: t vector,
                           prim: Type.t Prim.t,
                           targs: Type.t vector}
             | Raise of t
             | Record of t Record.t
             | Seq of t vector
             | Var of (unit -> Var.t) * (unit -> Type.t vector)
             | Vector of t vector

            val andAlso: t * t -> t
            val casee: {ctxt: unit -> Layout.t,
                        kind: string * string,
                        nest: string list,
                        matchDiags: {nonexhaustiveExn: Control.Elaborate.DiagDI.t,
                                     nonexhaustive: Control.Elaborate.DiagEIW.t,
                                     redundant: Control.Elaborate.DiagEIW.t},
                        noMatch: noMatch,
                        region: Region.t,
                        rules: {exp: t,
                                layPat: (unit -> Layout.t) option,
                                pat: Pat.t,
                                regionPat: Region.t} vector,
                        test: t} -> t
            val dest: t -> node * Type.t
            val iff: t * t * t -> t
            val falsee: t
            val foreachVar: t * (Var.t -> unit) -> unit
            (* true if the expression may side-effect. See p 19 of Definition *)
            val isExpansive: t -> bool
            val lambda: lambda -> t
            val layout: t -> Layout.t
            val layoutWithType: t -> Layout.t
            val make: node * Type.t -> t
            val node: t -> node
            val orElse: t * t -> t
            val truee: t
            val tuple: t vector -> t
            val ty: t -> Type.t
            val unit: t
            val var: Var.t * Type.t -> t
            val whilee: {expr: t, test: t} -> t
         end

      structure Lambda:
         sig
            type t

            val bogus: t
            val dest: t -> {arg: Var.t,
                            argType: Type.t,
                            body: Exp.t,
                            mayInline: bool}
            val make: {arg: Var.t,
                       argType: Type.t,
                       body: Exp.t,
                       mayInline: bool} -> t
         end
      sharing type Exp.lambda = Lambda.t

      structure Dec:
         sig
            datatype t =
               Datatype of {cons: {arg: Type.t option,
                                   con: Con.t} vector,
                            tycon: Tycon.t,
                            tyvars: Tyvar.t vector} vector
             | Exception of {arg: Type.t option,
                             con: Con.t}
             | Fun of {decs: {lambda: Lambda.t,
                              var: Var.t} vector,
                       tyvars: unit -> Tyvar.t vector}
             | Val of {matchDiags: {nonexhaustiveExn: Control.Elaborate.DiagDI.t,
                                    nonexhaustive: Control.Elaborate.DiagEIW.t,
                                    redundant: Control.Elaborate.DiagEIW.t},
                       rvbs: {lambda: Lambda.t,
                              var: Var.t} vector,
                       tyvars: unit -> Tyvar.t vector,
                       vbs: {ctxt: unit -> Layout.t,
                             exp: Exp.t,
                             layPat: unit -> Layout.t,
                             nest: string list,
                             pat: Pat.t,
                             regionPat: Region.t} vector}

            val layout: t -> Layout.t
         end
      sharing type Exp.dec = Dec.t

      structure Program:
         sig
            datatype t = T of {decs: Dec.t vector}

            val layouts: t * (Layout.t -> unit) -> unit
         end
   end
