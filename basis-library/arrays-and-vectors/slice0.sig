(* Copyright (C) 2017 Matthew Fluet.
 * Copyright (C) 2014 Rob Simmons.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature PRIM_SLICE = 
   sig
      type 'a sequence
      type 'a elt
      type 'a slice
      val length: 'a slice -> SeqIndex.int
      val sub: 'a slice * SeqIndex.int -> 'a elt
      val unsafeSub: 'a slice * SeqIndex.int -> 'a elt
      val update: 'a slice * SeqIndex.int * 'a elt -> unit
      val unsafeUpdate: 'a slice * SeqIndex.int * 'a elt -> unit
      val uninitIsNop: 'a slice -> bool
      val uninit: 'a slice * SeqIndex.int -> unit
      val unsafeUninit: 'a slice * SeqIndex.int -> unit
      val copy: {dst: 'a elt array, di: SeqIndex.int, src: 'a slice} -> unit
      val unsafeCopy: {dst: 'a elt array, di: SeqIndex.int, src: 'a slice} -> unit
      val full: 'a sequence -> 'a slice
      val slice: 'a sequence * SeqIndex.int * SeqIndex.int option -> 'a slice
      val unsafeSlice: 'a sequence * SeqIndex.int * SeqIndex.int option -> 'a slice
      val subslice: 'a slice * SeqIndex.int * SeqIndex.int option -> 'a slice
      val unsafeSubslice: 'a slice * SeqIndex.int * SeqIndex.int option -> 'a slice
      val base: 'a slice -> 'a sequence * SeqIndex.int * SeqIndex.int
      val isEmpty: 'a slice -> bool
      val getItem: 'a slice -> ('a elt * 'a slice) option
      val appi: (SeqIndex.int * 'a elt -> unit) -> 'a slice -> unit
      val app: ('a elt -> unit) -> 'a slice -> unit
      val mapi: (SeqIndex.int * 'a elt -> 'b elt) -> 'a slice -> 'b sequence
      val map: ('a elt -> 'b elt) -> 'a slice -> 'b sequence
      val foldli: (SeqIndex.int * 'a elt * 'b -> 'b) -> 'b -> 'a slice -> 'b
      val foldl: ('a elt * 'b -> 'b) -> 'b -> 'a slice -> 'b
      val foldri: (SeqIndex.int * 'a elt * 'b -> 'b) -> 'b -> 'a slice -> 'b
      val foldr: ('a elt * 'b -> 'b) -> 'b -> 'a slice -> 'b
      val findi: (SeqIndex.int * 'a elt -> bool) -> 'a slice -> (SeqIndex.int * 'a elt) option
      val find: ('a elt -> bool) -> 'a slice -> 'a elt option
      val existsi: (SeqIndex.int * 'a elt -> bool) -> 'a slice -> bool
      val exists: ('a elt -> bool) -> 'a slice -> bool
      val alli: (SeqIndex.int * 'a elt -> bool) -> 'a slice -> bool
      val all: ('a elt -> bool) -> 'a slice -> bool
      val collate: ('a elt * 'a elt -> order) -> 'a slice * 'a slice -> order

      val splitl: ('a elt -> bool) -> 'a slice -> 'a slice * 'a slice
      val splitr: ('a elt -> bool) -> 'a slice -> 'a slice * 'a slice 
      val splitAt: 'a slice * SeqIndex.int -> 'a slice * 'a slice
      val dropl: ('a elt -> bool) -> 'a slice -> 'a slice
      val dropr: ('a elt -> bool) -> 'a slice -> 'a slice
      val takel: ('a elt -> bool) -> 'a slice -> 'a slice
      val taker: ('a elt -> bool) -> 'a slice -> 'a slice
      val position: ('a elt * 'a elt -> bool) -> 
                    'a sequence -> 'a slice -> 'a slice * 'a slice
      val append: 'a slice * 'a slice -> 'a sequence
      val sequence: 'a slice -> 'a sequence

      (* span:
       * ('a sequence * 'a sequence -> bool)  should be polymorphic equality
       *)
      val span: ('a sequence * 'a sequence -> bool) -> 'a slice * 'a slice -> 'a slice
   end
