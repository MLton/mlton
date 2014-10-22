(* Copyright (C) 2014 Matthew Fluet.
 * Copyright (C) 2004-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature WORD_X_VECTOR_STRUCTS =
   sig
      structure WordSize: WORD_SIZE
      structure WordX: WORD_X
      sharing WordSize = WordX.WordSize
   end

signature WORD_X_VECTOR =
   sig
      include WORD_X_VECTOR_STRUCTS

      type t

      val elementSize: t -> WordSize.t
      val equals: t * t -> bool
      val foldFrom: t * int * 'b * (WordX.t * 'b -> 'b) -> 'b
      val forall: t * (WordX.t -> bool) -> bool
      val fromString: string -> t
      val fromListRev: {elementSize: WordSize.t} * WordX.t list -> t
      val hash : t -> word
      val layout: t -> Layout.t
      val length: t -> int
      val sub: t * int -> WordX.t
      val tabulate: {elementSize: WordSize.t} * int * (int -> WordX.t) -> t
      val toListMap: t * (WordX.t -> 'a) -> 'a list
      val toString: t -> string
   end
