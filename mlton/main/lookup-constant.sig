(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

type word = Word.t
   
signature LOOKUP_CONSTANT_STRUCTS = 
   sig
      structure Const: CONST
      structure ConstType: CONST_TYPE
      structure Ffi: FFI
      sharing ConstType = Const.ConstType
   end

signature LOOKUP_CONSTANT = 
   sig
      include LOOKUP_CONSTANT_STRUCTS

      val build: (string * ConstType.t) list * Out.t -> unit
      val load:
	 In.t * {name: string, value: string} list
	 -> {default: string option, name: string} * ConstType.t -> Const.t
   end
