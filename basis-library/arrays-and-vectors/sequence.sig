(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

signature SEQUENCE =
   sig
      type 'a sequence
      type 'a elt

      structure Slice : SLICE where type 'a sequence = 'a sequence
	                        and type 'a elt = 'a elt

      val maxLen: int
      val fromList: 'a elt list -> 'a sequence
      val tabulate: int * (int -> 'a elt) -> 'a sequence
      val length: 'a sequence -> int
      val sub: 'a sequence * int -> 'a elt
      val unsafeSub: 'a sequence * int -> 'a elt
      (* ('a sequence * int * 'a elt -> unit  should be an unsafe update. 
       *)
      val update': ('a sequence * int * 'a elt -> unit) ->
                   ('a sequence * int * 'a elt) -> unit
      val unsafeUpdate': ('a sequence * int * 'a elt -> unit) ->
	                 ('a sequence * int * 'a elt) -> unit
      val concat: 'a sequence list -> 'a sequence 
      val appi: (int * 'a elt -> unit) -> 'a sequence -> unit 
      val app: ('a elt -> unit) -> 'a sequence -> unit 
      val mapi : (int * 'a elt -> 'b elt) -> 'a sequence -> 'b sequence 
      val map: ('a elt -> 'b elt) -> 'a sequence -> 'b sequence 
      val foldli: (int * 'a elt * 'b -> 'b) -> 'b -> 'a sequence -> 'b 
      val foldri: (int * 'a elt * 'b -> 'b) -> 'b -> 'a sequence -> 'b 
      val foldl: ('a elt * 'b -> 'b) -> 'b -> 'a sequence -> 'b 
      val foldr: ('a elt * 'b -> 'b) -> 'b -> 'a sequence -> 'b
      val findi: (int * 'a elt -> bool) -> 'a sequence -> (int * 'a elt) option
      val find: ('a elt -> bool) -> 'a sequence -> 'a elt option
      val existsi: (int * 'a elt -> bool) -> 'a sequence -> bool
      val exists: ('a elt -> bool) -> 'a sequence -> bool
      val alli: (int * 'a elt -> bool) -> 'a sequence -> bool
      val all: ('a elt -> bool) -> 'a sequence -> bool
      val collate: ('a elt * 'a elt -> order) -> 'a sequence * 'a sequence -> order

      (* Used to implement Substring/String functions *)
      val concatWith: 'a sequence -> 'a sequence list -> 'a sequence
      val isPrefix: ('a elt * 'a elt -> bool) -> 'a sequence -> 'a sequence -> bool
      val isSubsequence: ('a elt * 'a elt -> bool) -> 'a sequence -> 'a sequence -> bool
      val isSuffix: ('a elt * 'a elt -> bool) -> 'a sequence -> 'a sequence -> bool
      val translate: ('a elt -> 'a sequence) -> 'a sequence -> 'a sequence
      val tokens: ('a elt -> bool) -> 'a sequence -> 'a sequence list
      val fields: ('a elt -> bool) -> 'a sequence -> 'a sequence list

      (* Extra *)
      val createi: (int * (int -> 'b elt) -> 'c) ->
                   (int * 'a elt -> 'b elt) -> 'a sequence -> 'c
      val create: (int * (int -> 'b elt) -> 'c) ->
                  ('a elt -> 'b elt) -> 'a sequence -> 'c
      val duplicate: 'a sequence -> 'a sequence
      val new: int * 'a elt -> 'a sequence
      val toList: 'a sequence -> 'a elt list
      val unfoldi: int * 'a * (int * 'a -> 'b elt * 'a) -> 'b sequence

      (* Depreciated *)
      val checkSlice: 'a sequence * int * int option -> int
      (* Depreciated *)
      val checkSliceMax: int * int option * int -> int
      (* Depreciated *)
      val extract: 'a sequence * int * int option -> 'a sequence
   end
