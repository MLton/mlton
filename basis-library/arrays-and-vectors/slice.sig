(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

signature SLICE =
   sig
      type 'a sequence
      type 'a elt
      type 'a slice
      val length: 'a slice -> int
      val sub: 'a slice * int -> 'a elt
      val unsafeSub: 'a slice * int -> 'a elt
      (* ('a sequence * int * 'a elt -> unit  should be an unsafe update. 
       *)
      val update': ('a sequence * int * 'a elt -> unit) ->
                   ('a slice * int * 'a elt) -> unit
      val unsafeUpdate': ('a sequence * int * 'a elt -> unit) ->
                         ('a slice * int * 'a elt) -> unit
      val full: 'a sequence -> 'a slice
      val slice: 'a sequence * int * int option -> 'a slice
      val unsafeSlice: 'a sequence * int * int option -> 'a slice
      val subslice: 'a slice * int * int option -> 'a slice
      val unsafeSubslice: 'a slice * int * int option -> 'a slice
      val base: 'a slice -> 'a sequence * int * int
      val concat: 'a slice list -> 'a sequence
      val isEmpty: 'a slice -> bool
      val getItem: 'a slice -> ('a elt * 'a slice) option
      val appi: (int * 'a elt -> unit) -> 'a slice -> unit
      val app: ('a elt -> unit) -> 'a slice -> unit
      val mapi: (int * 'a elt -> 'b elt) -> 'a slice -> 'b sequence
      val map: ('a elt -> 'b elt) -> 'a slice -> 'b sequence
      val foldli: (int * 'a elt * 'b -> 'b) -> 'b -> 'a slice -> 'b
      val foldri: (int * 'a elt * 'b -> 'b) -> 'b -> 'a slice -> 'b
      val foldl: ('a elt * 'b -> 'b) -> 'b -> 'a slice -> 'b
      val foldr: ('a elt * 'b -> 'b) -> 'b -> 'a slice -> 'b
      val findi: (int * 'a elt -> bool) -> 'a slice -> (int * 'a elt) option
      val find: ('a elt -> bool) -> 'a slice -> 'a elt option
      val existsi: (int * 'a elt -> bool) -> 'a slice -> bool
      val exists: ('a elt -> bool) -> 'a slice -> bool
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
      val splitAt: 'a slice * int -> 'a slice * 'a slice
      val dropl: ('a elt -> bool) -> 'a slice -> 'a slice
      val dropr: ('a elt -> bool) -> 'a slice -> 'a slice
      val takel: ('a elt -> bool) -> 'a slice -> 'a slice
      val taker: ('a elt -> bool) -> 'a slice -> 'a slice
      val position: ('a elt * 'a elt -> bool) -> 
                    'a sequence -> 'a slice -> 'a slice * 'a slice
      (* span:
       * 'a sequence * 'a sequence -> bool should be polymorphic equality
       *)
      val span: ('a sequence * 'a sequence -> bool) -> 'a slice * 'a slice -> 'a slice
      val translate: ('a elt -> 'a sequence) -> 'a slice -> 'a sequence
      val tokens: ('a elt -> bool) -> 'a slice -> 'a slice list
      val fields: ('a elt -> bool) -> 'a slice -> 'a slice list

      (* Extra *)
      (* createi,create:
       * (int * (int -> 'b elt) -> 'c  should be a tabulate function.
       *)
      val createi: (int * (int -> 'b elt) -> 'c) ->
	           (int * 'a elt -> 'b elt) -> 'a slice -> 'c
      val create: (int * (int -> 'b elt) -> 'c) ->
	          ('a elt -> 'b elt) -> 'a slice -> 'c
      val toList: 'a slice -> 'a elt list
      val sequence: 'a slice -> 'a sequence
   end
