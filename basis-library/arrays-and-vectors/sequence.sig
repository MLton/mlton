(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature SEQUENCE =
   sig
      type 'a sequence
      type 'a elt

      structure Slice : SLICE where type 'a sequence = 'a sequence
                                and type 'a elt = 'a elt

      val maxLen: int
      val fromList: 'a elt list -> 'a sequence
      val tabulate': SeqIndex.int * (SeqIndex.int -> 'a elt) -> 'a sequence
      val tabulate: int * (int -> 'a elt) -> 'a sequence
      val length': 'a sequence -> SeqIndex.int
      val length: 'a sequence -> int
      val sub': 'a sequence * SeqIndex.int -> 'a elt
      val sub: 'a sequence * int -> 'a elt
      val unsafeSub': 'a sequence * SeqIndex.int -> 'a elt
      val unsafeSub: 'a sequence * int -> 'a elt
      (* updateMk',updateMk,unsafeUpdateMk',unsafeUpdateMk:
       * ('a sequence * SeqIndex.int * 'a elt -> unit)  should be an unsafe update. 
       *)
      val updateMk': ('a sequence * SeqIndex.int * 'a elt -> unit) ->
                     ('a sequence * SeqIndex.int * 'a elt) -> unit
      val updateMk: ('a sequence * SeqIndex.int * 'a elt -> unit) ->
                    ('a sequence * int * 'a elt) -> unit
      val unsafeUpdateMk': ('a sequence * SeqIndex.int * 'a elt -> unit) ->
                           ('a sequence * SeqIndex.int * 'a elt) -> unit
      val unsafeUpdateMk: ('a sequence * SeqIndex.int * 'a elt -> unit) ->
                          ('a sequence * int * 'a elt) -> unit
      val concat: 'a sequence list -> 'a sequence 
      val appi': (SeqIndex.int * 'a elt -> unit) -> 'a sequence -> unit 
      val appi: (int * 'a elt -> unit) -> 'a sequence -> unit 
      val app: ('a elt -> unit) -> 'a sequence -> unit 
      val mapi' : (SeqIndex.int * 'a elt -> 'b elt) -> 'a sequence -> 'b sequence 
      val mapi : (int * 'a elt -> 'b elt) -> 'a sequence -> 'b sequence 
      val map: ('a elt -> 'b elt) -> 'a sequence -> 'b sequence 
      val foldli': (SeqIndex.int * 'a elt * 'b -> 'b) -> 'b -> 'a sequence -> 'b 
      val foldli: (int * 'a elt * 'b -> 'b) -> 'b -> 'a sequence -> 'b 
      val foldl: ('a elt * 'b -> 'b) -> 'b -> 'a sequence -> 'b 
      val foldri': (SeqIndex.int * 'a elt * 'b -> 'b) -> 'b -> 'a sequence -> 'b 
      val foldri: (int * 'a elt * 'b -> 'b) -> 'b -> 'a sequence -> 'b 
      val foldr: ('a elt * 'b -> 'b) -> 'b -> 'a sequence -> 'b
      val findi': (SeqIndex.int * 'a elt -> bool) -> 'a sequence -> (SeqIndex.int * 'a elt) option
      val findi: (int * 'a elt -> bool) -> 'a sequence -> (int * 'a elt) option
      val find: ('a elt -> bool) -> 'a sequence -> 'a elt option
      val existsi': (SeqIndex.int * 'a elt -> bool) -> 'a sequence -> bool
      val existsi: (int * 'a elt -> bool) -> 'a sequence -> bool
      val exists: ('a elt -> bool) -> 'a sequence -> bool
      val alli': (SeqIndex.int * 'a elt -> bool) -> 'a sequence -> bool
      val alli: (int * 'a elt -> bool) -> 'a sequence -> bool
      val all: ('a elt -> bool) -> 'a sequence -> bool
      val collate: ('a elt * 'a elt -> order) -> 'a sequence * 'a sequence -> order

      (* Used to implement Substring/String functions *)
      val concatWith: 'a sequence -> 'a sequence list -> 'a sequence
      val isPrefix: ('a elt * 'a elt -> bool) -> 'a sequence -> 'a sequence -> bool
      val isSubsequence: ('a elt * 'a elt -> bool) -> 'a sequence -> 'a sequence -> bool
      val isSuffix: ('a elt * 'a elt -> bool) -> 'a sequence -> 'a sequence -> bool
      val translate: ('a elt -> 'b sequence) -> 'a sequence -> 'b sequence
      val tokens: ('a elt -> bool) -> 'a sequence -> 'a sequence list
      val fields: ('a elt -> bool) -> 'a sequence -> 'a sequence list

      (* Extra *)
      val append: 'a sequence * 'a sequence -> 'a sequence
      (* createi',createi,create:
       * (SeqIndex.int * (SeqIndex.int -> 'b elt) -> 'c)  should be a tabulate' function.
       *)
      val createi': (SeqIndex.int * (SeqIndex.int -> 'b elt) -> 'c) ->
                    (SeqIndex.int * 'a elt -> 'b elt) -> 'a sequence -> 'c
      val createi: (SeqIndex.int * (SeqIndex.int -> 'b elt) -> 'c) ->
                   (int * 'a elt -> 'b elt) -> 'a sequence -> 'c
      val create: (SeqIndex.int * (SeqIndex.int -> 'b elt) -> 'c) ->
                  ('a elt -> 'b elt) -> 'a sequence -> 'c
      val duplicate: 'a sequence -> 'a sequence
      val generate':
         SeqIndex.int -> {done: unit -> 'a sequence,
                          sub: SeqIndex.int -> 'a elt, 
                          update: SeqIndex.int * 'a elt -> unit}
      val generate:
         int -> {done: unit -> 'a sequence,
                 sub: int -> 'a elt, 
                 update: int * 'a elt -> unit}
      val newUninit': SeqIndex.int -> 'a sequence
      val newUninit: int -> 'a sequence
      val new': SeqIndex.int * 'a elt -> 'a sequence
      val new: int * 'a elt -> 'a sequence
      val toList: 'a sequence -> 'a elt list
      val unfoldi': SeqIndex.int * 'b * (SeqIndex.int * 'b -> 'a elt * 'b) -> 'a sequence * 'b
      val unfoldi: int * 'b * (int * 'b -> 'a elt * 'b) -> 'a sequence * 'b
      val unfold: int * 'b * ('b -> 'a elt * 'b) -> 'a sequence * 'b
   end
