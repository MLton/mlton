(* Copyright (C) 2014 Rob Simmons.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)
 
signature PRIM_SEQUENCE =
   sig
      type 'a sequence
      type 'a elt

      structure Slice: PRIM_SLICE where type 'a sequence = 'a sequence
                                    and type 'a elt = 'a elt

      val maxLen: SeqIndex.int (* Must also be representable as an Int.int *)
      val tabulate: SeqIndex.int * (SeqIndex.int -> 'a elt) -> 'a sequence
      val length: 'a sequence -> SeqIndex.int
      val sub: 'a sequence * SeqIndex.int -> 'a elt
      val unsafeSub: 'a sequence * SeqIndex.int -> 'a elt
      (* updateMk,unsafeUpdateMk:
       * ('a sequence * SeqIndex.int * 'a elt -> unit) 
       * should be primitive unsafe update. 
       *)
      val updateMk: ('a sequence * SeqIndex.int * 'a elt -> unit) ->
                     ('a sequence * SeqIndex.int * 'a elt) -> unit
      val unsafeUpdateMk: ('a sequence * SeqIndex.int * 'a elt -> unit) ->
                           ('a sequence * SeqIndex.int * 'a elt) -> unit
      val appi: (SeqIndex.int * 'a elt -> unit) -> 'a sequence -> unit 
      val app: ('a elt -> unit) -> 'a sequence -> unit 
      val mapi: (SeqIndex.int * 'a elt -> 'b elt) -> 'a sequence -> 'b sequence 
      val map: ('a elt -> 'b elt) -> 'a sequence -> 'b sequence 
      val foldli: (SeqIndex.int * 'a elt * 'b -> 'b) -> 'b -> 'a sequence -> 'b 
      val foldl: ('a elt * 'b -> 'b) -> 'b -> 'a sequence -> 'b 
      val foldri: (SeqIndex.int * 'a elt * 'b -> 'b) -> 'b -> 'a sequence -> 'b 
      val foldr: ('a elt * 'b -> 'b) -> 'b -> 'a sequence -> 'b
      val findi: (SeqIndex.int * 'a elt -> bool) -> 'a sequence -> (SeqIndex.int * 'a elt) option
      val find: ('a elt -> bool) -> 'a sequence -> 'a elt option
      val existsi: (SeqIndex.int * 'a elt -> bool) -> 'a sequence -> bool
      val exists: ('a elt -> bool) -> 'a sequence -> bool
      val alli: (SeqIndex.int * 'a elt -> bool) -> 'a sequence -> bool
      val all: ('a elt -> bool) -> 'a sequence -> bool
      val collate: ('a elt * 'a elt -> order) -> 'a sequence * 'a sequence -> order




      (* Extra *)
      val append: 'a sequence * 'a sequence -> 'a sequence
      val duplicate: 'a sequence -> 'a sequence
      val generate:
         SeqIndex.int -> {done: unit -> 'a sequence,
                          sub: SeqIndex.int -> 'a elt, 
                          update: SeqIndex.int * 'a elt -> unit}
      val newUninit: SeqIndex.int -> 'a sequence
      val new: SeqIndex.int * 'a elt -> 'a sequence
      val unfoldi: SeqIndex.int * 'b * (SeqIndex.int * 'b -> 'a elt * 'b) -> 'a sequence * 'b
      val unfold: SeqIndex.int * 'b * ('b -> 'a elt * 'b) -> 'a sequence * 'b
   end
