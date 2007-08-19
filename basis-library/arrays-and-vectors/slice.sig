(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature SLICE =
   sig
      type 'a sequence
      type 'a elt
      type 'a slice
      val length': 'a slice -> SeqIndex.int
      val length: 'a slice -> int
      val sub': 'a slice * SeqIndex.int -> 'a elt
      val sub: 'a slice * int -> 'a elt
      val unsafeSub': 'a slice * SeqIndex.int -> 'a elt
      val unsafeSub: 'a slice * int -> 'a elt
      (* updateMk',updateMk,unsafeUpdateMk',unsafeUpdateMk:
       * ('a sequence * SeqIndex.int * 'a elt -> unit)  should be an unsafe update. 
       *)
      val updateMk': ('a sequence * SeqIndex.int * 'a elt -> unit) ->
                     ('a slice * SeqIndex.int * 'a elt) -> unit
      val updateMk: ('a sequence * SeqIndex.int * 'a elt -> unit) ->
                    ('a slice * int * 'a elt) -> unit
      val unsafeUpdateMk': ('a sequence * SeqIndex.int * 'a elt -> unit) ->
                           ('a slice * SeqIndex.int * 'a elt) -> unit
      val unsafeUpdateMk: ('a sequence * SeqIndex.int * 'a elt -> unit) ->
                          ('a slice * int * 'a elt) -> unit
      val full: 'a sequence -> 'a slice
      val slice': 'a sequence * SeqIndex.int * SeqIndex.int option -> 'a slice
      val slice: 'a sequence * int * int option -> 'a slice
      val unsafeSlice': 'a sequence * SeqIndex.int * SeqIndex.int option -> 'a slice
      val unsafeSlice: 'a sequence * int * int option -> 'a slice
      val subslice': 'a slice * SeqIndex.int * SeqIndex.int option -> 'a slice
      val subslice: 'a slice * int * int option -> 'a slice
      val unsafeSubslice': 'a slice * SeqIndex.int * SeqIndex.int option -> 'a slice
      val unsafeSubslice: 'a slice * int * int option -> 'a slice
      val base': 'a slice -> 'a sequence * SeqIndex.int * SeqIndex.int
      val base: 'a slice -> 'a sequence * int * int
      val concat: 'a slice list -> 'a sequence
      val isEmpty: 'a slice -> bool
      val getItem: 'a slice -> ('a elt * 'a slice) option
      val appi': (SeqIndex.int * 'a elt -> unit) -> 'a slice -> unit
      val appi: (int * 'a elt -> unit) -> 'a slice -> unit
      val app: ('a elt -> unit) -> 'a slice -> unit
      val mapi': (SeqIndex.int * 'a elt -> 'b elt) -> 'a slice -> 'b sequence
      val mapi: (int * 'a elt -> 'b elt) -> 'a slice -> 'b sequence
      val map: ('a elt -> 'b elt) -> 'a slice -> 'b sequence
      val foldli': (SeqIndex.int * 'a elt * 'b -> 'b) -> 'b -> 'a slice -> 'b
      val foldli: (int * 'a elt * 'b -> 'b) -> 'b -> 'a slice -> 'b
      val foldl: ('a elt * 'b -> 'b) -> 'b -> 'a slice -> 'b
      val foldri': (SeqIndex.int * 'a elt * 'b -> 'b) -> 'b -> 'a slice -> 'b
      val foldri: (int * 'a elt * 'b -> 'b) -> 'b -> 'a slice -> 'b
      val foldr: ('a elt * 'b -> 'b) -> 'b -> 'a slice -> 'b
      val findi': (SeqIndex.int * 'a elt -> bool) -> 'a slice -> (SeqIndex.int * 'a elt) option
      val findi: (int * 'a elt -> bool) -> 'a slice -> (int * 'a elt) option
      val find: ('a elt -> bool) -> 'a slice -> 'a elt option
      val existsi': (SeqIndex.int * 'a elt -> bool) -> 'a slice -> bool
      val existsi: (int * 'a elt -> bool) -> 'a slice -> bool
      val exists: ('a elt -> bool) -> 'a slice -> bool
      val alli': (SeqIndex.int * 'a elt -> bool) -> 'a slice -> bool
      val alli: (int * 'a elt -> bool) -> 'a slice -> bool
      val all: ('a elt -> bool) -> 'a slice -> bool
      val collate: ('a elt * 'a elt -> order) -> 'a slice * 'a slice -> order

      (* Used to implement Substring/String functions *)
      val concatWith: 'a sequence -> 'a slice list -> 'a sequence
      val triml: int -> 'a slice -> 'a slice
      val trimr: int -> 'a slice -> 'a slice
      val isPrefix: ('a elt * 'a elt -> bool) -> 'a sequence -> 'a slice -> bool
      val isSubsequence: ('a elt * 'a elt -> bool) -> 'a sequence -> 'a slice -> bool
      val isSuffix: ('a elt * 'a elt -> bool) -> 'a sequence -> 'a slice -> bool
      val splitl: ('a elt -> bool) -> 'a slice -> 'a slice * 'a slice
      val splitr: ('a elt -> bool) -> 'a slice -> 'a slice * 'a slice
      val splitAt': 'a slice * SeqIndex.int -> 'a slice * 'a slice
      val splitAt: 'a slice * int -> 'a slice * 'a slice
      val dropl: ('a elt -> bool) -> 'a slice -> 'a slice
      val dropr: ('a elt -> bool) -> 'a slice -> 'a slice
      val takel: ('a elt -> bool) -> 'a slice -> 'a slice
      val taker: ('a elt -> bool) -> 'a slice -> 'a slice
      val position: ('a elt * 'a elt -> bool) -> 
                    'a sequence -> 'a slice -> 'a slice * 'a slice
      (* span:
       * ('a sequence * 'a sequence -> bool)  should be polymorphic equality
       *)
      val span: ('a sequence * 'a sequence -> bool) -> 'a slice * 'a slice -> 'a slice
      val translate: ('a elt -> 'b sequence) -> 'a slice -> 'b sequence
      val tokens: ('a elt -> bool) -> 'a slice -> 'a slice list
      val fields: ('a elt -> bool) -> 'a slice -> 'a slice list

      (* Extra *)
      val append: 'a slice * 'a slice -> 'a sequence
      (* createi',createi,create:
       * (SeqIndex.int * (SeqIndex.int -> 'b elt) -> 'c)  should be a tabulate' function.
       *)
      val createi': (SeqIndex.int * (SeqIndex.int -> 'b elt) -> 'c) ->
                    (SeqIndex.int * 'a elt -> 'b elt) -> 'a slice -> 'c
      val createi: (SeqIndex.int * (SeqIndex.int -> 'b elt) -> 'c) ->
                   (int * 'a elt -> 'b elt) -> 'a slice -> 'c
      val create: (SeqIndex.int * (SeqIndex.int -> 'b elt) -> 'c) ->
                  ('a elt -> 'b elt) -> 'a slice -> 'c
      val toList: 'a slice -> 'a elt list
      val sequence: 'a slice -> 'a sequence
   end
