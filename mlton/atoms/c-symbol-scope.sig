(* Copyright (C) 2019 Matthew Fluet.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature C_SYMBOL_SCOPE_STRUCTS =
   sig
   end

signature C_SYMBOL_SCOPE =
   sig
      include C_SYMBOL_SCOPE_STRUCTS

      datatype t = External | Private | Public

      val equals: t * t -> bool
      val layout: t -> Layout.t
      val toString: t -> string
      val parse: t Parse.t
   end
