(* Copyright (C) 2017 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature ELABORATE_ENV_STRUCTS =
   sig
      structure Ast: AST
      structure CoreML: CORE_ML
      structure TypeEnv: TYPE_ENV
      sharing Ast.Record = CoreML.Record
      sharing Ast.SortedRecord = CoreML.SortedRecord
      sharing CoreML.Atoms = TypeEnv.Atoms
      sharing CoreML.Type = TypeEnv.Type
   end

signature ELABORATE_ENV =
   sig
      include ELABORATE_ENV_STRUCTS

      structure Decs: DECS
      sharing CoreML = Decs.CoreML

      structure Tyvar: TYVAR
      sharing Tyvar = TypeEnv.Tyvar
      structure TyvarEnv:
         sig
            val lookupTyvar: Ast.Tyvar.t -> Tyvar.t option
            val scope: Ast.Tyvar.t vector * (Tyvar.t vector -> 'a) -> 'a
            val makeLayoutPretty: unit -> {destroy: unit -> unit, layoutPretty: Tyvar.t -> Layout.t}
         end

      structure AdmitsEquality: ADMITS_EQUALITY
      sharing AdmitsEquality = TypeEnv.Tycon.AdmitsEquality
      structure Kind: TYCON_KIND
      sharing Kind = TypeEnv.Tycon.Kind
      structure Tycon: TYCON
      sharing Tycon = TypeEnv.Tycon

      structure Type:
         sig
            type t
         end
      sharing Type = TypeEnv.Type
      structure Scheme:
         sig
            type t
         end
      sharing Scheme = TypeEnv.Scheme

      (* The value of a vid.  This is used to distinguish between vids whose
       * status cannot be determined at parse time.
       *)
      structure Vid:
         sig
            datatype t =
               Con of CoreML.Con.t
             | Exn of CoreML.Con.t
             | Overload of Ast.Priority.t * (CoreML.Var.t * Scheme.t) vector
             | Var of CoreML.Var.t

            val layout: t -> Layout.t
         end

      structure TypeStr:
         sig
            structure Cons:
               sig
                  type t

                  val layout: t -> Layout.t
               end

            type t

            datatype node =
               Datatype of {cons: Cons.t,
                            tycon: Tycon.t}
             | Scheme of Scheme.t
             | Tycon of Tycon.t

            val abs: t -> t
            val admitsEquality: t -> AdmitsEquality.t
            val apply: t * Type.t vector -> Type.t
            val data: Tycon.t * Cons.t -> t
            val def: Scheme.t -> t
            val kind: t -> Kind.t
            val layout: t -> Layout.t
            val node: t -> node
            val toTyconOpt: t -> Tycon.t option (* NONE on Scheme *)
            val tycon: Tycon.t -> t
         end

      structure Interface: INTERFACE
      sharing Interface.Ast = Ast
      sharing Interface.AdmitsEquality = AdmitsEquality
      sharing Interface.Kind = Kind
      sharing Interface.EnvTycon = Tycon
      sharing Interface.EnvTypeStr = TypeStr
      sharing Interface.Tyvar = Tyvar

      structure Structure:
         sig
            type t

            (* ffi represents MLtonFFI, which is built by the basis library and
             * set via the special ffiStr MLB annotation.
             *)
            val ffi: t option ref
            val forceUsed: t -> unit
            val layout: t -> Layout.t
         end
      structure FunctorClosure:
         sig
            type t

            val apply: (t * Structure.t * string list
                        -> Decs.t * Structure.t option)
            val argInterface: t -> Interface.t
         end

      structure InterfaceEnv:
         sig
            structure FlexibleTycon:
               sig
                  type t
               end
            structure Scheme:
               sig
                  type t
               end
            structure Status:
               sig
                  type t
               end
            structure TypeStr:
               sig
                  type t
               end

            type t

            val extendCon: t * Ast.Con.t * Scheme.t -> unit
            val extendExn: t * Ast.Con.t * Scheme.t -> unit
            val extendStrid: t * Ast.Strid.t * Interface.t -> unit
            val extendTycon: t * Ast.Tycon.t * TypeStr.t -> unit
            val extendVid: t * Ast.Vid.t * Status.t * Scheme.t -> unit
            val lookupLongtycon: t * Ast.Longtycon.t -> TypeStr.t option
            val lookupSigid: t * Ast.Sigid.t -> Interface.t option
            val makeInterface:
               t * {isTop: bool} * (unit -> 'a) -> Interface.t * 'a
            val openInterface: t * Interface.t * Region.t -> unit
            val rebindTycon: t * Ast.Tycon.t * TypeStr.t -> unit
         end
      sharing Interface.FlexibleTycon = InterfaceEnv.FlexibleTycon
      sharing Interface.Scheme = InterfaceEnv.Scheme
      sharing Interface.Status = InterfaceEnv.Status
      sharing Interface.TypeStr = InterfaceEnv.TypeStr

      structure Basis:
         sig
            type t
            val layout: t -> Layout.t
         end

      type t

      val amInsideFunctor: unit -> bool
      (* cut keeps only those bindings in the structure that also appear
       * in the interface.  It proceeds recursively on substructures.
       *)
      val cut:
         t * Structure.t * Interface.t
         * {isFunctor: bool, opaque: bool, prefix: string} * Region.t
         -> Structure.t * Decs.t
      val empty: unit -> t
      val extendBasid: t * Ast.Basid.t * Basis.t -> unit
      val extendExn: t * Ast.Con.t * CoreML.Con.t * Scheme.t -> unit
      val extendFctid: t * Ast.Fctid.t * FunctorClosure.t -> unit
      val extendFix: t * Ast.Vid.t * Ast.Fixity.t -> unit
      val extendSigid: t * Ast.Sigid.t * Interface.t -> unit
      val extendStrid: t * Ast.Strid.t * Structure.t -> unit
      val extendTycon:
         t * Ast.Tycon.t * TypeStr.t * {forceUsed: bool, isRebind: bool} -> unit
      val extendVar:
         t * Ast.Var.t * CoreML.Var.t * Scheme.t * {isRebind: bool} -> unit
      val extendOverload:
         t * Ast.Priority.t * Ast.Var.t * (CoreML.Var.t * Scheme.t) vector
         * Scheme.t
         -> unit
      val forceUsed: t -> unit
      val forceUsedLocal: t * (unit -> 'a) -> 'a
      val functorClosure:
         t * Ast.Fctid.t * Interface.t
         * (Structure.t * string list -> Decs.t * Structure.t option)
         -> FunctorClosure.t
      val localAll: t * (unit -> 'a) * ('a -> 'b) -> 'b
      val localCore: t * (unit -> 'a) * ('a -> 'b) -> 'b
      val localModule: t * (unit -> 'a) * ('a -> 'b) -> 'b
      val lookupBasid: t * Ast.Basid.t -> Basis.t option
      val lookupFctid: t * Ast.Fctid.t -> FunctorClosure.t option
      val lookupLongcon: t * Ast.Longcon.t -> (CoreML.Con.t * Scheme.t) option
      val lookupLongexn: t * Ast.Longcon.t -> (CoreML.Con.t * Scheme.t) option
      val lookupLongstrid: t * Ast.Longstrid.t -> Structure.t option
      val lookupLongtycon: t * Ast.Longtycon.t -> TypeStr.t option
      val lookupLongvar: t * Ast.Longvar.t -> (CoreML.Var.t * Scheme.t) option
      val lookupLongvid: t * Ast.Longvid.t -> (Vid.t * Scheme.t) option
      val lookupSigid: t * Ast.Sigid.t -> Interface.t option
      val lookupStrid: t * Ast.Strid.t -> Structure.t option
      val makeBasis: t * (unit -> 'a) -> 'a * Basis.t
      val makeInterfaceEnv: t -> InterfaceEnv.t
      val makeStructure: t * (unit -> 'a) -> 'a * Structure.t
      val newCons:
         t * {con: CoreML.Con.t,
              name: Ast.Con.t,
              scheme: Scheme.t} vector
         -> TypeStr.Cons.t
      (* openStructure (E, S) opens S in the environment E. *) 
      val openStructure: t * Structure.t -> unit
      (* openBasis (E, B) opens B in the environment E. *) 
      val openBasis: t * Basis.t -> unit
      val output: t * Out.t * {compact: bool, def: bool, flat: bool, onlyCurrent: bool, prefixUnset: bool} -> unit
      val peekFix: t * Ast.Vid.t -> Ast.Fixity.t option
      val peekLongcon: t * Ast.Longcon.t -> (CoreML.Con.t * Scheme.t) option
      val processDefUse: t -> unit
      (* scope f evaluates f () in a new scope so that extensions that occur
       * during f () are forgotten afterwards.
       * scope works for infixes, types, values, and structures
       *)
      val scope: t * (unit -> 'a) -> 'a
      (* like scope, but works for bases, signatures and functors as well *)
      val scopeAll: t * (unit -> 'a) -> 'a
      val makeLayoutPrettyTycon:
         t * {prefixUnset: bool}
         -> {destroy: unit -> unit,
             layoutPrettyTycon: Tycon.t -> Layout.t,
             setLayoutPrettyTycon: Tycon.t * Layout.t -> unit,
             loopStr: Structure.t * int * Ast.Strid.t list -> unit}
      val makeLayoutPrettyTyconAndFlexTycon:
         t * InterfaceEnv.t * Interface.t option * {prefixUnset: bool}
         -> {destroy: unit -> unit,
             layoutPrettyTycon: Tycon.t -> Layout.t,
             layoutPrettyFlexTycon: Interface.FlexibleTycon.t -> Layout.t,
             setLayoutPrettyTycon: Tycon.t * Layout.t -> unit}
      val sizeMessage: t -> Layout.t
      val snapshot: t -> (unit -> 'a) -> 'a
   end
