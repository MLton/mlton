(* Copyright (C) 1999-2005, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature TYVAR_STRUCTS = 
   sig
   end

signature TYVAR = 
   sig
      include TYVAR_STRUCTS
      include T
      include WRAPPED sharing type obj = t

      val clear: t -> unit
      val hash: t -> Word.t
      val isEquality: t -> bool
      val layouts: t vector -> Layout.t
      val name: t -> string
      val newLike: t -> t
      (* newNoname creates a new type variable named a_n, where n is a
       * counter.
       *)
      val newNoname: {equality: bool} -> t
      (* newString "'a" creates a type variable named a
       * newString "''a" creates an equality type variable named a
       *)
      val newString: string * {left: SourcePos.t,
                               right: SourcePos.t} -> t
      val plist: t -> PropertyList.t
      (* reset the counter for new type variables *)
      val reset: unit -> unit 
      val sameName: t * t -> bool
      val toString: t -> string
   end
