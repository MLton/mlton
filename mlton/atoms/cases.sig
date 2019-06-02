(* Copyright (C) 2009,2017,2019 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature CASES_STRUCTS =
   sig
      structure WordSize: WORD_SIZE
      structure WordX: WORD_X
      sharing WordSize = WordX.WordSize
   end

signature CASES =
   sig
      include CASES_STRUCTS

      datatype ('con, 'a) t =
         Con of ('con * 'a) vector
       | Word of WordSize.t * (WordX.t * 'a) vector

      val equals: ('con, 'a) t * ('con, 'a) t * ('con * 'con -> bool) * ('a * 'a -> bool) -> bool
      val fold: ('con, 'a) t * 'b * ('a * 'b -> 'b) -> 'b
      val forall: ('con, 'a) t * ('a -> bool) -> bool
      val foreach': ('con, 'a) t * ('a -> unit) * ('con -> unit) -> unit
      val foreach: ('con, 'a) t * ('a -> unit) -> unit
      val hd: ('con, 'a) t -> 'a
      val isEmpty: ('con, 'a) t -> bool
      val length: ('con, 'a) t -> int
      val map: ('con, 'a) t * ('a -> 'b) -> ('con, 'b) t
   end
