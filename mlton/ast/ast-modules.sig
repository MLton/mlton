(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature AST_MODULES_STRUCTS =
   sig
      include AST_ATOMS_STRUCTS
   end

signature AST_MODULES =
   sig
      include AST_CORE

      structure Sigexp:
         sig
            type spec

            type t
            datatype node =
               Spec of spec
             | Var of Sigid.t
             | Where of t * {longtycon: Longtycon.t,
                             ty: Type.t,
                             tyvars: Tyvar.t vector} vector

            include WRAPPED sharing type node' = node
                            sharing type obj = t

            val var: Sigid.t -> t
            val wheree: t * {tyvars: Tyvar.t vector,
                             longtycon: Longtycon.t,
                             ty: Type.t} vector * Region.t -> t
            val spec: spec -> t

            val layout: t -> Layout.t
         end

      structure SigConst:
         sig
            datatype t =
               None
             | Opaque of Sigexp.t
             | Transparent of Sigexp.t
         end

      structure Equation:
         sig
            type t
            datatype node =
               Structure of Longstrid.t list
             | Type of Longtycon.t list
            include WRAPPED sharing type node' = node
                            sharing type obj = t
         end

      structure Spec:
         sig
            type t
            datatype node =
               Datatype of DatatypeRhs.t
             | Eqtype of {tycon: Tycon.t,
                          tyvars: Tyvar.t vector} vector
             | Empty
             | Exception of (Con.t * Type.t option) vector
             | IncludeSigexp of Sigexp.t
             | IncludeSigids of Sigid.t vector
             | Seq of t * t
             | Sharing of {equations: Equation.t vector,
                           spec: t}
             | Structure of (Strid.t * Sigexp.t) vector
             | Type of {tycon: Tycon.t,
                        tyvars: Tyvar.t vector} vector
             | TypeDefs of TypBind.t
             | Val of (Var.t * Type.t) vector

            include WRAPPED sharing type node' = node
                            sharing type obj = t

            val layout: t -> Layout.t
         end
      sharing type Spec.t = Sigexp.spec

      structure Strexp:
         sig
            type strdec

            type t
            datatype node =
               App of Fctid.t * t
             | Constrained of t * SigConst.t
             | Let of strdec * t
             | Struct of strdec
             | Var of Longstrid.t

            include WRAPPED sharing type node' = node
                            sharing type obj = t

            val var: Longstrid.t -> t
            val structt: strdec -> t
            val constrained: t * SigConst.t -> t
            val app: Fctid.t * t -> t
            val lett: strdec * t -> t
               
            val layout: t -> Layout.t
         end

      structure Strdec:
         sig
            type t
            datatype node =
               Core of Dec.t
             | Local of t * t
             | Seq of t list
             | Structure of {constraint: SigConst.t,
                             def: Strexp.t,
                             name: Strid.t} vector

            include WRAPPED sharing type node' = node
                            sharing type obj = t

            val coalesce: t -> t
            val core: Dec.t -> t
            val layout: t -> Layout.t
            val locall: t * t -> t
            val openn: Longstrid.t vector -> t
            val seq: t list -> t
            val structuree: {name: Strid.t,
                             def: Strexp.t,
                             constraint: SigConst.t} vector -> t
         end
      sharing type Strdec.t = Strexp.strdec

      structure FctArg:
         sig
            type t
            datatype node =
               Structure of Strid.t * Sigexp.t
             | Spec of Spec.t
            include WRAPPED sharing type node' = node
                            sharing type obj = t
         end
      
      structure Topdec:
         sig
            type t
            datatype node =
               Functor of {arg: FctArg.t,
                           body: Strexp.t,
                           name: Fctid.t,
                           result: SigConst.t} vector
             | Signature of (Sigid.t * Sigexp.t) vector
             | Strdec of Strdec.t

            include WRAPPED sharing type node' = node
                            sharing type obj = t

            val checkSyntax: t -> unit
            val fromExp: Exp.t -> t
            val functorr: {name: Fctid.t,
                           arg: FctArg.t,
                           result: SigConst.t,
                           body: Strexp.t} vector -> t
            val layout: t -> Layout.t
            val signaturee: (Sigid.t * Sigexp.t) vector -> t
            val strdec: Strdec.t -> t
         end
   end
