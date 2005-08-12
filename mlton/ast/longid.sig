(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
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

      val bogus: t
      val fromSymbols: Symbol.t list * Region.t -> t
      val isLong: t -> bool (* returns true if the list of strids is nonempty *)
      val long: Strid.t list * Id.t -> t
      (* prepend with a path: 
       * prepend (([B, C], x), A) = ([A, B, C], x)
       * prepends (([C, D], x), [A, B]) = ([A, B, C, D], x)
       *)
      val prepend: t * Strid.t -> t
      val prepends: t * Strid.t list -> t
      val short: Id.t -> t
      val split: t -> Strid.t list * Id.t
      val toId: t -> Id.t
      val toString: t -> string
   end
