(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure AppendList: APPEND_LIST =
struct

(* We are careful to ensure that the empty list is always represented by
 * the Empty constructor.
 *)
datatype 'a t =
   Append of 'a t * 'a t (* Neither is empty. *)
 | Appends of 'a t list (* None is empty and list is nonempty. *)
 | AppendsV of 'a t vector (* None is empty and vector is nonempty. *)
 | Cons of 'a * 'a t (* Nonempty. *)
 | Empty
 | List of 'a list (* Nonempty. *)
 | Single of 'a
 | Snoc of 'a t (* Nonempty *) * 'a 
 | Vector of 'a vector (* Nonempty. *)

val isEmpty = fn Empty => true | _ => false

fun append (t1, t2) =
   if isEmpty t1
      then t2
   else if isEmpty t2
           then t1
        else Append (t1, t2)

fun appends l =
   let
      val l = List.keepAll (l, not o isEmpty)
   in
      if List.isEmpty l
         then Empty
      else Appends l
   end

fun appendsV v =
   let
      val v = Vector.keepAll (v, not o isEmpty)
   in
      if Vector.isEmpty v
         then Empty
      else AppendsV v
   end

fun cons (a, l) =
   case l of
      Empty => Single a
    | _ => Cons (a, l)

val empty = Empty

fun fromList l =
   if List.isEmpty l
      then Empty
   else List l

fun fromVector v =
   if Vector.isEmpty v
      then Empty
   else Vector v

val single = Single

fun snoc (l, a) =
   case l of
      Empty => Single a
    | _ => Snoc (l, a)

fun fold (l, b, f) =
   let
      fun loop (l, b) =
         case l of
            Append (l, l') => loop (l', loop (l, b))
          | Appends l => List.fold (l, b, loop)
          | AppendsV v => Vector.fold (v, b, loop)
          | Cons (x, l) => loop (l, f (x, b))
          | Empty => b
          | List l => List.fold (l, b, f)
          | Single x => f (x, b)
          | Snoc (l, x) => f (x, loop (l, b))
          | Vector v => Vector.fold (v, b, f)
   in loop (l, b)
   end

fun length l : int = fold (l, 0, fn (_, i) => i + 1)

fun foreach (l, f) = fold (l, (), fn (x, ()) => f x)

fun foldr (l, b, f) =
   let
      fun loop (l, b) =
         case l of
            Append (l, l') => loop (l, loop (l', b))
          | Appends l => List.foldr (l, b, loop)
          | AppendsV v => Vector.foldr (v, b, loop)
          | Cons (x, l) => f (x, loop (l, b))
          | Empty => b
          | List l => List.foldr (l, b, f)
          | Single x => f (x, b)
          | Snoc (l, x) => loop (l, f (x, b))
          | Vector v => Vector.foldr (v, b, f)
   in loop (l, b)
   end

fun map (l, f) =
   let
      val rec loop =
         fn Append (l, l') => Append (loop l, loop l')
          | Appends l => Appends (List.map (l, loop))
          | AppendsV v => AppendsV (Vector.map (v, loop))
          | Cons (x, l) => Cons (f x, loop l)
          | Empty => Empty
          | List l => List (List.map (l, f))
          | Single x => Single (f x)
          | Snoc (l, x) => Snoc (loop l, f x)
          | Vector v => Vector (Vector.map (v, f))
   in loop l
   end

fun toList ds = foldr (ds, [], op ::)

fun toListOnto (ds, l) = foldr (ds, l, op ::)

fun toVector ds = Vector.tabulator (length ds, fn call => foreach (ds, call))

fun layout layoutX l = List.layout layoutX (toList l)

fun push (r, x) = r := cons (x, !r)

end
