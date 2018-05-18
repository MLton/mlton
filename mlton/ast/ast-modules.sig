(* Copyright (C) 2017 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature AST_MODULES_STRUCTS =
   sig
      include AST_ATOMS_STRUCTS
   end

signature AST_MODULES =
   sig
      include AST_CORE

      structure WhereEquation:
         sig
            type t
            datatype node =
               Type of {longtycon: Longtycon.t,
                        tyvars: Tyvar.t vector,
                        ty: Type.t}
            include WRAPPED sharing type node' = node
                            sharing type obj = t
         end

      structure Sigexp:
         sig
            type spec

            type t
            datatype node =
               Spec of spec
             | Var of Sigid.t
             | Where of {equations: WhereEquation.t vector,
                         sigexp: t}

            include WRAPPED sharing type node' = node
                            sharing type obj = t

            val wheree: t * WhereEquation.t vector -> t
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

      structure SharingEquation:
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
             | Sharing of {equation: SharingEquation.t,
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

            val constrained: t * SigConst.t -> t
            val lett: strdec * t -> t
            val var: Longstrid.t -> t

            val layout: t -> Layout.t
         end

      structure Strdec:
         sig
            type t
            datatype node =
               Core of Dec.t
             | Local of t * t
             | Seq of t list
             | ShowBasis of File.t
             | Structure of {constraint: SigConst.t,
                             def: Strexp.t,
                             name: Strid.t} vector

            include WRAPPED sharing type node' = node
                            sharing type obj = t

            val coalesce: t -> t
            val core: Dec.t -> t
            val layout: t -> Layout.t
            val openn: Longstrid.t vector -> t
            val structuree: {constraint: SigConst.t,
                             def: Strexp.t,
                             name: Strid.t} -> t
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
            val layout: t -> Layout.t
         end
   end
