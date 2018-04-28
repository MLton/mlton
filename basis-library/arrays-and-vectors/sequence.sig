(* Copyright (C) 2017 Matthew Fluet.
 * Copyright (C) 2014 Rob Simmons.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature SEQUENCE =
   sig
      type 'a sequence
      type 'a elt

      structure Slice : SLICE where type 'a sequence = 'a sequence
                                and type 'a elt = 'a elt

      val maxLen: int
      val length: 'a sequence -> int
      val sub: 'a sequence * int -> 'a elt
      val unsafeSub: 'a sequence * int -> 'a elt 
      val update: 'a sequence * int * 'a elt -> unit
      val unsafeUpdate: 'a sequence * int * 'a elt -> unit
      val uninitIsNop: 'a sequence -> bool
      val uninit: 'a sequence * int -> unit
      val unsafeUninit: 'a sequence * int -> unit
      val copy: {dst: 'a elt Array.array, di: int, src: 'a sequence} -> unit
      val unsafeCopy: {dst: 'a elt Array.array, di: int, src: 'a sequence} -> unit
      val tabulate: int * (int -> 'a elt) -> 'a sequence
      val appi: (int * 'a elt -> unit) -> 'a sequence -> unit 
      val app: ('a elt -> unit) -> 'a sequence -> unit 
      val mapi : (int * 'a elt -> 'b elt) -> 'a sequence -> 'b sequence 
      val map: ('a elt -> 'b elt) -> 'a sequence -> 'b sequence 
      val foldli: (int * 'a elt * 'b -> 'b) -> 'b -> 'a sequence -> 'b 
      val foldl: ('a elt * 'b -> 'b) -> 'b -> 'a sequence -> 'b 
      val foldri: (int * 'a elt * 'b -> 'b) -> 'b -> 'a sequence -> 'b 
      val foldr: ('a elt * 'b -> 'b) -> 'b -> 'a sequence -> 'b
      val findi: (int * 'a elt -> bool) -> 'a sequence -> (int * 'a elt) option
      val find: ('a elt -> bool) -> 'a sequence -> 'a elt option
      val existsi: (int * 'a elt -> bool) -> 'a sequence -> bool
      val exists: ('a elt -> bool) -> 'a sequence -> bool
      val alli: (int * 'a elt -> bool) -> 'a sequence -> bool
      val all: ('a elt -> bool) -> 'a sequence -> bool
      val collate: ('a elt * 'a elt -> order) -> 'a sequence * 'a sequence -> order 
      val fromList: 'a elt list -> 'a sequence 
      val toList: 'a sequence -> 'a elt list 
      val concat: 'a sequence list -> 'a sequence 

      (* Extra *)
      val alloc: int -> 'a sequence
      val append: 'a sequence * 'a sequence -> 'a sequence 
      val create:
         int -> {done: unit -> 'a sequence,
                 sub: int -> 'a elt, 
                 update: int * 'a elt -> unit} 
      val duplicate: 'a sequence -> 'a sequence
      val new: int * 'a elt -> 'a sequence 
      val unfoldi: int * 'b * (int * 'b -> 'a elt * 'b) -> 'a sequence * 'b
      val unfold: int * 'b * ('b -> 'a elt * 'b) -> 'a sequence * 'b
      val unsafeAlloc: int -> 'a sequence
      val unsafeNew: int * 'a elt -> 'a sequence

      (* Used to implement Substring/String functions *)
      val isPrefix: ('a elt * 'a elt -> bool) -> 'a sequence -> 'a sequence -> bool
      val concatWith: 'a sequence -> 'a sequence list -> 'a sequence
      val isSubsequence: ('a elt * 'a elt -> bool) -> 'a sequence -> 'a sequence -> bool
      val isSuffix: ('a elt * 'a elt -> bool) -> 'a sequence -> 'a sequence -> bool 
      val translate: ('a elt -> 'b sequence) -> 'a sequence -> 'b sequence
      val tokens: ('a elt -> bool) -> 'a sequence -> 'a sequence list
      val fields: ('a elt -> bool) -> 'a sequence -> 'a sequence list 
   end
