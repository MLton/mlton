(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
type int = Int.t
   
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
      val new: {name: string, equality: bool} -> t
      (* newNoname creates a new type variable named a_n, where n is a
       * counter.
       *)
      val newNoname: {equality: bool} -> t
      (* newString "'a" creates a type variable named a
       * newString "''a" creates an equality type variable named a
       *)
      val newString: string * int * int -> t
      val plist: t -> PropertyList.t
      (* reset the counter for new type variables *)
      val reset: unit -> unit 
      val sameName: t * t -> bool
   end

