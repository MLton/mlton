(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
(* empty tuple is also a record *)

functor Record (S: RECORD_STRUCTS): RECORD = 
struct

open S

datatype 'a t =
   Tuple of 'a vector
 | Record of (Field.t * 'a) vector

val tuple = Tuple

val isTuple =
   fn Tuple _ => true
    | _ => false

fun toVector r =
   case r of
      Tuple v => Vector.mapi (v, fn (i, x) => (Field.Int i, x))
    | Record r => r
	 
fun detupleOpt (r: 'a t): 'a vector option =
   case r of
      Tuple t => SOME t
    | Record _ => NONE

fun sort v =
   QuickSort.sortVector (v, fn ((s, _), (s', _)) => Field.<= (s, s'))
   
fun fromVector v =
   let
      fun isTuple v : bool =
	 Vector.foralli
	 (v, fn (i, (f, _)) =>
	  case f of
	     Field.Int i' => Int.equals (i, i')
	   | _ => false)
      val v = if isSorted then sort v else v
   in
      if isTuple v
	 then Tuple (Vector.map (v, #2))
      else Record v
   end

fun equals (r, r', eq) =
   case (r, r') of
      (Tuple v, Tuple v') => Vector.equals (v, v', eq)
    | (Record fs, Record fs') =>
	 Vector.equals
	 (fs, sort fs', fn ((f, v), (f', v')) =>
	  Field.equals (f, f') andalso eq (v, v'))
    | _ => false

val peek: 'a t * Field.t -> 'a option =
   fn (r, f) =>
   case r of
      Record r =>
	 (case Vector.peek (r, fn (f', _) => Field.equals (f, f')) of
	     NONE => NONE
	   | SOME (_, x) => SOME x)
    | Tuple t =>
	 if Vector.isEmpty t
	    then NONE
	 else (case f of
		  Field.Int i =>
		     if 0 <= i andalso i < Vector.length t
			then SOME (Vector.sub (t, i))
		     else NONE
		| Field.String _ => NONE)

fun range r =
   case r of
      Tuple t => t
    | Record r => Vector.map (r, #2)

fun exists (r, p) =
   case r of
      Tuple xs => Vector.exists (xs, p)
    | Record r => Vector.exists (r, fn (_, x) => p x)

fun foldi (r, b, f) =
   case r of
      Tuple xs => Vector.foldi (xs, b, fn (i, x, b) => f (Field.Int i, x, b))
    | Record r => Vector.fold (r, b, fn ((i, x), b) => f (i, x, b))

fun fold (r, b, f) = foldi (r, b, fn (_, a, b) => f (a, b))
	  
fun map (r: 'a t, f: 'a -> 'b): 'b t =
   case r of
      Tuple xs => Tuple (Vector.map (xs, f))
    | Record r => Record (Vector.map (r, fn (field, a) => (field, f a)))

fun foreach (r: 'a t, f: 'a -> unit): unit =
   case r of
      Tuple xs => Vector.foreach (xs, f)
    | Record r => Vector.foreach (r, f o #2)

fun change (r: 'a t, f: 'a vector -> 'b vector * 'c): 'b t * 'c =
   case r of
      Tuple xs => let val (ys, c) = f xs
		  in (Tuple ys, c)
		  end
    | Record r => let val (fs, xs) = Vector.unzip r
		      val (ys, c) = f xs
		  in (Record (Vector.zip (fs, ys)), c)
		  end

fun zip z = fromVector (Vector.zip z)

fun unzip r =
   case r of
      Tuple v => (Vector.tabulate (Vector.length v, Field.Int),
		  v)
    | Record r => Vector.unzip r

fun layout {record, layoutTuple, separator, extra, layoutElt} =
   case (record, extra) of
      (Tuple xs, "") => layoutTuple xs
    | _ =>
	 let
	    val r = toVector record
	    open Layout
	 in seq [str "{",
		 mayAlign (separateRight (Vector.toListMap
					  (r, fn (f, x) =>
					   seq [Field.layout f,
						str separator,
						layoutElt x]),
					  ",")),
		 str extra,
		 str "}"]
	 end

end
