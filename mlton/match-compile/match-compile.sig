(* Copyright (C) 2009,2015,2017 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature MATCH_COMPILE_STRUCTS =
   sig
      include ATOMS
      structure Type:
         sig
            type t

            val deTuple: t -> t vector
            val equals: t * t -> bool
            val layout: t -> Layout.t
            val unit: t
            val word: WordSize.t -> t
         end
      structure Cases:
         sig
            type exp
            type t

            val con: {arg: (Var.t * Type.t) option,
                      con: Con.t,
                      rhs: exp,
                      targs: Type.t vector} vector -> t
            val word: WordSize.t * (WordX.t * exp) vector -> t
         end
      structure Exp:
         sig
            type t

            val casee:
               {cases: Cases.t,
                default: (t * Region.t) option,
                test: t,
                ty: Type.t}  (* type of entire case expression *)
               -> t
            val const: Const.t -> t
            val deref: t -> t
            val detuple: {tuple: t,
                          body: (Var.t * Type.t) vector -> t} -> t
            val devector: {vector: t, length: int,
                           body: (Var.t * Type.t) vector -> t} -> t
            val equal: t * t -> t
            val iff: {test: t, thenn: t, elsee: t, ty: Type.t} -> t
            val lett: {var: Var.t, exp: t, body: t} -> t
            val var: Var.t * Type.t -> t
            val vectorLength: t -> t
         end
      sharing type Cases.exp = Exp.t
      structure NestedPat: NESTED_PAT
      sharing Atoms = NestedPat.Atoms
      sharing Type = NestedPat.Type
   end

signature MATCH_COMPILE =
   sig
      include MATCH_COMPILE_STRUCTS

      val matchCompile:
         {caseType: Type.t, (* type of entire expression *)
          cases: (NestedPat.t * (int -> (Var.t -> Var.t) -> Exp.t)) vector,
          conTycon: Con.t -> Tycon.t,
          region: Region.t,
          test: Var.t,
          testType: Type.t,
          tyconCons: Tycon.t -> {con: Con.t, hasArg: bool} vector}
         -> Exp.t * ({dropOnlyExns: bool} -> Layout.t option)
   end
