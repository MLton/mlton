(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure Random: RANDOM =
struct

structure Array = Pervasive.Array

local
   open MLton.Random
in
   val alphaNumString = alphaNumString
   val seed = seed
   val useed = useed
   val word = rand
   val srand = srand
end

val word = Trace.trace ("Random.word", Unit.layout, Word.layout) word
   
local
   val ri: int ref = ref 0
   val rw = ref (word ())
   val max = Word.wordSize - 1
in
   fun bool () =
      let
	 val i = !ri
	 val b = 0w1 = Word.andb (0wx1, Word.>> (!rw, Word.fromInt i))
	 val _ =
	    if i = max
	       then (rw := word ()
		     ; ri := 0)
	    else ri := 1 + i
      in b
      end
end

fun int () = Word.toIntX (word ())

val int = Trace.trace ("Random.int", Unit.layout, Int.layout) int

val maxInt = Int.maxInt

fun nat () = Word.toInt (Word.andb (word (), Word.fromInt maxInt))

val int = Trace.trace ("Random.nat", Unit.layout, Int.layout) int

val maxIntR = Real.fromInt maxInt

fun scale r = r / maxIntR

val natReal = Real.fromInt o nat

fun real () = scale (natReal () + scale (natReal ()))

fun intRange (lo: int, hi: int): unit -> int =
   let val rlo = Real.fromInt lo
      val R = Real.fromInt hi - rlo + 1.0
   in
      fn () => Real.trunc (rlo + R * real ())
   end

val intRange =
   Assert.assertFun2
   ("Random.intRange", intRange,
    fn (lo, hi) =>
    (lo <= hi, fn () => (true, fn n => lo <= n andalso n <= hi)))

fun nRandom {list, length, n} =
   let
      fun loop (need: int, length: int, xs: 'a list, ac: 'a list): 'a list =
	 (Assert.assert ("Random.nRandom", fn () => need <= length)
	  ; if need <= 0
	       then ac
	    else (case xs of
		     [] => Error.bug "nRandom"
		   | x :: xs =>
			if intRange (0, length - 1) () < need
			   then loop (need - 1, length - 1, xs, x :: ac)
			else loop (need, length - 1, xs, ac)))
   in loop (n, length, list, [])
   end

val nRandom = fn x =>
   Assert.assertFun
   ("nRandom", nRandom,
    fn {list, length, n} => (length = List.length list
			     andalso 0 <= n
			     andalso n <= length,
			     fn l => n = List.length l))
   x

fun list l =
   let val n = List.length l
   in if n = 0
	 then NONE
      else SOME (List.nth (l, intRange (0, n - 1) ()))
   end

(* For now, the following is not used.  I just use MLton.Random.rand. *)
(* Page 302 of Numerical Recipes in C.
 * DES random word generator.
 *)
local
   val niter: int = 4
   open Word
   fun make (l: Word.t list) =
      let val a = Array.fromList l
      in fn i => Array.sub (a, i)
      end
   val c1 = make [0wxbaa96887, 0wx1e17d32c, 0wx03bdcd3c, 0wx0f33d1b2]
   val c2 = make [0wx4b0f3b58, 0wxe874f0c3, 0wx6955c5a6, 0wx55a7ca46]
   val half: Word.t = 0w16
   fun reverse w = orb (>> (w, half), << (w, half))
   fun psdes (lword: t, irword: t): t * t =
      Int.fold
      (0, niter, (lword, irword), fn (i, (lword, irword)) =>
       let
	  val ia = xorb (irword, c1 i)
	  val itmpl = andb (ia, 0wxffff)
	  val itmph = >> (ia, half)
	  val ib = itmpl * itmpl + notb (itmph * itmph)
       in (irword,
	   xorb (lword, itmpl * itmph + xorb (c2 i, reverse ib)))
       end)

   val zero: Word.t = 0wx13 
   val lword = ref zero
   val irword = ref zero
   val needTo = ref true
   fun word () =
      if !needTo
	 then
	    let
	       val (l, i) = psdes (!lword, !irword)
	       val _ = lword := l
	       val _ = irword := i
	       val _ = needTo := false
	    in
	       l
	    end
      else (needTo := true
	    ; !irword)
in
end

end
