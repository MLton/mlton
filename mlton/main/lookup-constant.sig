(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

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
