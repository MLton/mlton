(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature HASH_TYPE_STRUCTS = 
   sig
      include ATOMS
   end

signature HASH_TYPE = 
   sig
      include HASH_TYPE_STRUCTS
      include TYPE_OPS
      sharing type realSize = RealSize.t
      sharing type tycon = Tycon.t
      sharing type wordSize = WordSize.t

      structure Dest:
         sig
            datatype dest =
               Con of Tycon.t * t vector
             | Var of Tyvar.t
            val dest: t -> dest
         end

      val checkPrimApp: {args: t vector,
                         prim: t Prim.t,
                         result: t,
                         targs: t vector} -> bool
      val containsTycon: t * Tycon.t -> bool
      (* O(1) time *)
      val equals: t * t -> bool
      (* for reporting type errors *)
      val error: string * Layout.t -> 'a
      val hash: t -> Word.t
      val hom: {ty: t,
                var: Tyvar.t -> 'a,
                con: Tycon.t * 'a vector -> 'a} -> 'a
      val isUnit: t -> bool
      val layout: t -> Layout.t
      val layoutPretty: t -> Layout.t
      val makeHom:
         {var: t * Tyvar.t -> 'a,
          con: t * Tycon.t * 'a vector -> 'a}
         -> {hom: t -> 'a,
             destroy: unit -> unit}
      val makeMonoHom:
         {con: t * Tycon.t * 'a vector -> 'a}
         -> {hom: t -> 'a,
             destroy: unit -> unit}
      val ofConst: Const.t -> t
      val plist: t -> PropertyList.t
      val stats: unit -> Layout.t
      val string: t (* synonym for word8Vector *)
      (* substitute (t, [(a1, t1), ..., (an, tn)]) performs simultaneous
       * substitution of the ti for ai in t.
       * The ai's are not required to contain every free variable in t
       *)
      val substitute: t * (Tyvar.t * t) vector -> t
      val tycon: t -> Tycon.t
      val var: Tyvar.t -> t
   end
