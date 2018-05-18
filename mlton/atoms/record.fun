(* Copyright (C) 2017 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(* empty tuple is also a record *)

functor Record (S: RECORD_STRUCTS): RECORD = 
struct

open S

datatype 'a t =
   Tuple of 'a vector
 | Record of (Field.t * 'a) vector

val tuple = Tuple

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
      if isTuple v andalso Vector.length v <> 1
         then Tuple (Vector.map (v, #2))
      else Record v
   end

fun unzip r = Vector.unzip (toVector r)
fun zip z = fromVector (Vector.zip z)

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
                | Field.Symbol _ => NONE)

fun domain r =
   case r of
      Tuple v => Vector.mapi (v, fn (i, _) => Field.Int i)
    | Record r => Vector.map (r, #1)

fun range r =
   case r of
      Tuple t => t
    | Record r => Vector.map (r, #2)

fun exists (r, p) =
   case r of
      Tuple xs => Vector.exists (xs, p)
    | Record r => Vector.exists (r, fn (_, x) => p x)

fun forall (r, p) = not (exists (r, not o p))

fun fold (r: 'a t, b: 'b, f: 'a * 'b -> 'b): 'b =
   case r of
      Tuple xs => Vector.fold (xs, b, f)
    | Record r => Vector.fold (r, b, fn ((_, x), b) => f (x, b))

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
