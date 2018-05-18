(* Copyright (C) 2017 Matthew Fluet.
 * Copyright (C) 2014 Rob Simmons.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature SLICE =
   sig
      type 'a sequence
      type 'a elt
      type 'a slice 
      val length: 'a slice -> int
      val sub: 'a slice * int -> 'a elt
      val unsafeSub: 'a slice * int -> 'a elt
      val update: 'a slice * int * 'a elt -> unit
      val unsafeUpdate: 'a slice * int * 'a elt -> unit
      val uninitIsNop: 'a slice -> bool
      val uninit: 'a slice * int -> unit
      val unsafeUninit: 'a slice * int -> unit
      val copy: {dst: 'a elt Array.array, di: int, src: 'a slice} -> unit
      val unsafeCopy: {dst: 'a elt Array.array, di: int, src: 'a slice} -> unit
      val full: 'a sequence -> 'a slice
      val slice: 'a sequence * int * int option -> 'a slice
      val unsafeSlice: 'a sequence * int * int option -> 'a slice
      val subslice: 'a slice * int * int option -> 'a slice
      val unsafeSubslice: 'a slice * int * int option -> 'a slice
      val base: 'a slice -> 'a sequence * int * int 
      val isEmpty: 'a slice -> bool 
      val getItem: 'a slice -> ('a elt * 'a slice) option 
      val appi: (int * 'a elt -> unit) -> 'a slice -> unit
      val app: ('a elt -> unit) -> 'a slice -> unit
      val mapi: (int * 'a elt -> 'b elt) -> 'a slice -> 'b sequence
      val map: ('a elt -> 'b elt) -> 'a slice -> 'b sequence
      val foldli: (int * 'a elt * 'b -> 'b) -> 'b -> 'a slice -> 'b
      val foldl: ('a elt * 'b -> 'b) -> 'b -> 'a slice -> 'b
      val foldri: (int * 'a elt * 'b -> 'b) -> 'b -> 'a slice -> 'b
      val foldr: ('a elt * 'b -> 'b) -> 'b -> 'a slice -> 'b
      val findi: (int * 'a elt -> bool) -> 'a slice -> (int * 'a elt) option
      val find: ('a elt -> bool) -> 'a slice -> 'a elt option 
      val existsi: (int * 'a elt -> bool) -> 'a slice -> bool
      val exists: ('a elt -> bool) -> 'a slice -> bool
      val alli: (int * 'a elt -> bool) -> 'a slice -> bool
      val all: ('a elt -> bool) -> 'a slice -> bool 
      val collate: ('a elt * 'a elt -> order) -> 'a slice * 'a slice -> order 

      val splitl: ('a elt -> bool) -> 'a slice -> 'a slice * 'a slice
      val splitr: ('a elt -> bool) -> 'a slice -> 'a slice * 'a slice 
      val splitAt: 'a slice * int -> 'a slice * 'a slice 
      val dropl: ('a elt -> bool) -> 'a slice -> 'a slice
      val dropr: ('a elt -> bool) -> 'a slice -> 'a slice
      val takel: ('a elt -> bool) -> 'a slice -> 'a slice
      val taker: ('a elt -> bool) -> 'a slice -> 'a slice
      val position: ('a elt * 'a elt -> bool) -> 
                    'a sequence -> 'a slice -> 'a slice * 'a slice 
      val append: 'a slice * 'a slice -> 'a sequence
      val sequence: 'a slice -> 'a sequence 
      val toList: 'a slice -> 'a elt list 

      (* Used to implement Substring/String functions *)
      val concat: 'a slice list -> 'a sequence 
      val concatWith: 'a sequence -> 'a slice list -> 'a sequence 
      val triml: int -> 'a slice -> 'a slice
      val trimr: int -> 'a slice -> 'a slice 
      val isPrefix: ('a elt * 'a elt -> bool) -> 'a sequence -> 'a slice -> bool 
      val isSubsequence: ('a elt * 'a elt -> bool) -> 'a sequence -> 'a slice -> bool
      val isSuffix: ('a elt * 'a elt -> bool) -> 'a sequence -> 'a slice -> bool 
      val translate: ('a elt -> 'b sequence) -> 'a slice -> 'b sequence
      val tokens: ('a elt -> bool) -> 'a slice -> 'a slice list
      val fields: ('a elt -> bool) -> 'a slice -> 'a slice list 
   end
