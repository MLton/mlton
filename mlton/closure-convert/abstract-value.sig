(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature ABSTRACT_VALUE_STRUCTS = 
   sig
      structure Sxml: SXML
      structure Ssa: SSA
   end

signature ABSTRACT_VALUE = 
   sig
      include ABSTRACT_VALUE_STRUCTS

      structure Lambda:
         sig
            type t

            val dest: t -> Sxml.Lambda.t
            val layout: t -> Layout.t
         end

      structure Lambdas:
         sig
            type t

            val equals: t * t -> bool
            val plist: t -> PropertyList.t
            val toList: t -> Lambda.t list
         end

      type t

      datatype dest =
         Array of t
       | Lambdas of Lambdas.t
       | Ref of t
       | Tuple of t vector
       | Type of Sxml.Type.t (* type doesn't contain any arrows *)
       | Vector of t
       | Weak of t

      val addHandler: t * (Lambda.t -> unit) -> unit
      val coerce: {from: t, to: t} -> unit
      val ssaType: t -> Ssa.Type.t option ref
      val deArray: t -> t
      val deRef: t -> t
      val deVector: t -> t
      val deWeak: t -> t
      val dest: t -> dest
      (* Destroy info associated with Sxml.Type used to keep track of arrows. *)
      val destroy: unit -> unit
      val equals: t * t -> bool
      val fromType: Sxml.Type.t -> t
      val isEmpty: t -> bool (* no possible values correspond to me *) 
      val lambda: Sxml.Lambda.t * Sxml.Type.t (* The type of the lambda. *) -> t
      val layout: t -> Layout.t
      val primApply: {prim: Sxml.Type.t Sxml.Prim.t,
                      args: t vector,
                      resultTy: Sxml.Type.t} -> t
      val select: t * int -> t
      val serialValue: Sxml.Type.t -> t
      (* In tuple vs, there must be one argument that is not Type _. *)
      val tuple: t vector -> t 
      val ty: t -> Sxml.Type.t
      val typeIsFirstOrder: Sxml.Type.t -> bool
      val unify: t * t -> unit
   end
