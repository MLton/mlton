(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure S = UnorderedUniverse(Integer)


structure S = SetEqual(structure Element = Integer)

open S

val s = listTo[1, 2, 3, 4]

val ss = subsets s 3

structure D = DisjointMax(structure Label = struct open Int open IU end) ;

open D ;

val ss = LU.mapFromTo 0 10 singleton ;

val s = LU.nth ss ;

   LU.map ss eval ;

fun u i n = update (s i) n ;
fun l i j = link (s i) (s j) ;
fun e i = eval (s i) ;

   l 3 6 ;

   l 3 7 ;

   l 8 3 ;

   l 8 10 ;

   u 8 11 ;   


open DisjointCollection ;

val (c, ss) = new (LU.fromTo 0 10) ;

fun s i = LU.nth ss i ;
fun r() = randomSet c ;
fun u i j = union c (s i) (s j) ;
fun n() = numSets c ;
fun v i = value c (s i) ;

r() ;

n() ;

value c (r()) ;

LU.map (LU.fromTo 0 10) v ;

u 1 2 ;

u 2 3 ;

u 3 4 ;

u 1 3 ;

u 2 4 ;

open DisjointSet ;

val elts = LU.mapFromTo 0 9 singleton ;

val n = ListUtil.nth elts ;
fun e i j = areEquivalent (n i) (n j) ;
fun u i j = union (n i) (n j) ;

   u 1 3 ; u 2 3 ; e 1 2 ;

   u 1 3 ; e 1 3 ;

   u 2 3 ; e 1 3 ;

   u 2 4 ; u 1 4 ; u 1 3 ; e 1 2 ;

u 1 2 ;
e 1 2 ;
e 1 3 ;

u 3 4 ;

u 5 6 ;

u 1 4 ;

e 2 3 ;

u 5 1 ;

e 6 4 ;

u 7 8 ;
u 9 0 ;

u 8 9 ;

u 1 7 ;

elts ;
