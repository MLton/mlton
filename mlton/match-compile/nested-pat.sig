(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature NESTED_PAT_STRUCTS = 
   sig
      include ATOMS
      structure Type:
         sig
            type t

            val layout: t -> Layout.t
            val tuple: t vector -> t
         end
   end

signature NESTED_PAT = 
   sig
      include NESTED_PAT_STRUCTS

      datatype t = T of {pat: node, ty: Type.t}
      and node =
         Con of {arg: t option,
                 con: Con.t,
                 targs: Type.t vector}
        | Const of {const: Const.t,
                    isChar: bool,
                    isInt: bool}
        | Layered of Var.t * t
        | Or of t vector
        | Tuple of t vector
        | Var of Var.t
        | Wild

      val flatten: t -> t vector
      (* isRefutable p iff p contains a constant, constructor or variable. *)
      val isRefutable: t -> bool
      val isVar: t -> bool
      val layout: t -> Layout.t
      val make: node * Type.t -> t
      val node: t -> node
      val removeOthersReplace: t * {new: Var.t, old: Var.t} -> t
      val removeVars: t -> t
      val replaceTypes: t * (Type.t -> Type.t) -> t
      val tuple: t vector -> t
      val ty: t -> Type.t
      (* varsAndTypes returns a list of the variables in the pattern, along with
       * their types.  It is used for match compilation in order to build a
       * function that abstracts over the expression of a case rule p => e.
       * See infer.fun.
       *)
      val varsAndTypes: t -> (Var.t * Type.t) list
   end
