(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

type int = Int.t
type word = Word.t
   
signature RANDOM =
   sig
      val alphaNumString: int -> string
      val charFrom: string -> char
      val bool: unit -> bool
      val list: 'a list -> 'a option
      (* natLessThan n returns a random number uniform in [0, n). *)
      val natLessThan: int -> int
      val nRandom: {list: 'a list, length: int, n: int} -> 'a list
      (* 0.0 <= real() <= 1.0 *)
      val real: unit -> real
      val seed: unit -> Word.t option
      val srand: Word.t -> unit
      val useed: unit -> Word.t option
      val word: unit -> Word.t
      (* word w returns a random number uniform in [0, w). *)
      val wordLessThan: Word.t -> Word.t
   end
