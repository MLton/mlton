functor Cases (S: CASES_STRUCTS): CASES =
struct

open S

datatype 'a t =
   Char of (char * 'a) vector
 | Con of (con * 'a) vector
 | Int of (int * 'a) vector
 | Word of (word * 'a) vector
 | Word8 of (Word8.t * 'a) vector

fun equals (c1: 'a t, c2: 'a t, eq: 'a * 'a -> bool): bool =
   let
      fun doit (l1, l2, eq') = 
	 Vector.equals 
	 (l1, l2, fn ((x1, a1), (x2, a2)) =>
	  eq' (x1, x2) andalso eq (a1, a2))
   in case (c1, c2) of
      (Char l1, Char l2) => doit (l1, l2, Char.equals)
    | (Con l1, Con l2) => doit (l1, l2, conEquals)
    | (Int l1, Int l2) => doit (l1, l2, Int.equals)
    | (Word l1, Word l2) => doit (l1, l2, Word.equals)
    | (Word8 l1, Word8 l2) => doit (l1, l2, Word8.equals)
    | _ => false
   end

fun fold (c: 'a t, b: 'b, f: 'a * 'b -> 'b): 'b =
   let
      fun doit l = Vector.fold (l, b, fn ((_, a), b) => f (a, b))
   in case c of
      Char l => doit l
    | Con l => doit l
    | Int l => doit l
    | Word l => doit l
    | Word8 l => doit l
   end

fun map (c: 'a t, f: 'a -> 'b): 'b t =
   let
      fun doit l = Vector.map (l, fn (i, x) => (i, f x))
   in case c of
      Char l => Char (doit l)
    | Con l => Con (doit l)
    | Int l => Int (doit l)
    | Word l => Word (doit l)
    | Word8 l => Word8 (doit l)
   end

fun forall (c: 'a t, f: 'a -> bool): bool =
   let
      fun doit l = Vector.forall (l, fn (_, x) => f x)
   in case c of
      Char l => doit l
    | Con l => doit l
    | Int l => doit l
    | Word l => doit l
    | Word8 l => doit l
   end

fun isEmpty (c: 'a t): bool =
   let
      fun doit v = 0 = Vector.length v
   in case c of
      Char v => doit v
    | Con v => doit v
    | Int v => doit v
    | Word v => doit v
    | Word8 v => doit v
   end

fun hd (c: 'a t): 'a =
   let
      fun doit v =
	 if Vector.length v >= 1
	    then let val (_, a) = Vector.sub (v, 0)
		 in a
		 end
	 else Error.bug "Cases.hd"
   in case c of
      Char l => doit l
    | Con l => doit l
    | Int l => doit l
    | Word l => doit l
    | Word8 l => doit l
   end

fun length (c: 'a t): int = fold (c, 0, fn (_, i) => i + 1)

fun foreach (c, f) = fold (c, (), fn (x, ()) => f x)

fun foreach' (c: 'a t, f: 'a -> unit, fc: con -> unit): unit =
   let
      fun doit l = Vector.foreach (l, fn (_, a) => f a)
   in case c of
      Char l => doit l
    | Con l => Vector.foreach (l, fn (c, a) => (fc c; f a))
    | Int l => doit l
    | Word l => doit l
    | Word8 l => doit l
   end


end
