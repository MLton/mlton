(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
type int = Int.t
   
signature RANDOM =
   sig
      val alphaNumString: int -> string
      val bool: unit -> bool
      val int: unit -> int
      (* intRange(i, j)() is uniform in [i, j]
       * It is curried because it does some computation with i and j.
       *)
      val intRange: int * int -> unit -> int
      val list: 'a list -> 'a option
      val nRandom: {list: 'a list, length: int, n: int} -> 'a list
      (* 0.0 <= real() <= 1.0 *)
      val real: unit -> real
      val seed: unit -> Word.t option
      val srand: Word.t -> unit
      val useed: unit -> Word.t option
      val word: unit -> Word.t
   end
