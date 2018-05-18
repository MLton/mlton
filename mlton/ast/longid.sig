(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature LONGID_STRUCTS =
   sig
      structure Id: AST_ID
      structure Strid: AST_ID
      structure Symbol: SYMBOL
      sharing Symbol = Id.Symbol = Strid.Symbol
   end

signature LONGID =
   sig
      include LONGID_STRUCTS
      include T

      datatype node = T of {strids: Strid.t list,
                            id: Id.t}

      include WRAPPED sharing type node' = node
                      sharing type obj = t

      val fromSymbols: Symbol.t list * Region.t -> t
      val long: Strid.t list * Id.t -> t
      val short: Id.t -> t
      val split: t -> Strid.t list * Id.t
      val toString: t -> string
   end
