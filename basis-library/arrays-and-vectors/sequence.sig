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

      val app: ('a elt -> unit) -> 'a sequence -> unit 
      val appi: (int * 'a elt -> unit) -> 'a sequence * int * int option -> unit 
      (* checkSlice returns max where the slice is from min (inclusive)
       * to max (exclusive).  Raises Subscript if invalid slice.
       *)
      val checkSlice: 'a sequence * int * int option -> int
      val checkSliceMax: int * int option * int -> int
      val concat: 'a sequence list -> 'a sequence
      val extract: 'a sequence * int * int option -> 'a sequence
      val find: 'a sequence * ('a elt -> bool) -> 'a elt option
      val foldl: ('a elt * 'b -> 'b) -> 'b -> 'a sequence -> 'b 
      val foldli:
	 (int * 'a elt * 'b -> 'b) -> 'b -> 'a sequence * int * int option -> 'b 
      val foldr: ('a elt * 'b -> 'b) -> 'b -> 'a sequence -> 'b
      val foldri:
	 (int * 'a elt * 'b -> 'b) -> 'b -> 'a sequence * int * int option -> 'b 
      val fromList: 'a elt list -> 'a sequence 
      val length: 'a sequence -> int 
      val map: ('a elt -> 'b elt) -> 'a sequence -> 'b sequence
      val mapi:
	 (int * 'a elt -> 'b elt)
	 -> 'a sequence * int * int option -> 'b sequence 
      val maxLen: int 
      val new: int * 'a elt -> 'a sequence
      val prefixToList: 'a sequence * int -> 'a elt list
      val sub: 'a sequence * int -> 'a elt 
      val tabulate: int * (int -> 'a elt) -> 'a sequence 
      val toList: 'a sequence -> 'a elt list
      val unfoldi: int * 'a * (int * 'a -> 'b elt * 'a) -> 'b sequence
      val update: 'a elt array * int * 'a elt -> unit
      val wholeSlice: 'a sequence -> 'a sequence * int * int option
   end
