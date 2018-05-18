(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

local
   structure H = BinaryHeap(structure O = Int
                            open Int
                            fun inject x = x
                            fun project x = x
                            val largest = Int.maxInt
                            val smallest = Int.minInt)
   open H
   val h = new[(1, "1"), (2, "2"), (3, "3")]
   val _ =
      while not(isEmpty h) do
         (print(deleteMin h)
          ; print "\n")
in
end

structure IntegerHeap = FibonacciHeap(structure Key' = Integer)

open IntegerHeap ;

fun p h = output h Integer.output print

val h = new [(1, 1)] ;

val w = isWellFormed h ;

val _ = p h ;

fun i n = insert h n n ;

val _ = i 2 ;

val _ = i 3 ;

val h = new (ListUtil.reverse (ListUtil.map (ListUtil.fromTo 1 10)
                               (fn x => (x, x)))) ;

val _ = p h ;

val w = isWellFormed h ;

val a = min h ;

val a = deleteMin h ;

val b = deleteMin h ;

val c = deleteMin h ;

val l = ListUtil.map (ListUtil.fromTo 1 7) (fn _ => deleteMin h) ;

isEmpty h ;

insert h 100 100 ;

isEmpty h ;

min h ;

ListUtil.foreach (ListUtil.fromTo 1 10) (fn i =>
                                         (insert h (11 * i) (11 * i) ;
                                          ())) ;

min h ;

deleteMin h ;

isEmpty h ;

ListUtil.foreach (ListUtil.reverse (ListUtil.fromTo 1 1100))
(fn i => (insert h i i ; ())) ;

ListUtil.foreach (ListUtil.fromTo 1 1098) (fn _ => (deleteMin h ; ())) ;


insert h 11 11 ;

val _ = p h ;


val h: int t = new [] ;

val elts = ListUtil.map (ListUtil.fromTo 0 9) (fn x => insert h x x) ;

val _ = p h ;

fun dc i = decreaseKey h (ListUtil.nth elts i)
fun d i = delete h (ListUtil.nth elts i)

val _ = dc 6 ~1 ;

   dc 7 0 ;

val a = deleteMin h ;

val _ = d 4 ;
