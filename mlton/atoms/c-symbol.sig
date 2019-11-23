(* Copyright (C) 2019 Matthew Fluet.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature C_SYMBOL_STRUCTS =
   sig
      structure CSymbolScope: C_SYMBOL_SCOPE
      structure CType: C_TYPE
   end

signature C_SYMBOL =
   sig
      include C_SYMBOL_STRUCTS

      datatype t = T of {cty: CType.t option,
                         name: string,
                         symbolScope: CSymbolScope.t}

      val equals: t * t -> bool
      val hash: t -> word
      val layout: t -> Layout.t
      val toString: t -> string
      val parse: t Parse.t
   end
