(* Copyright (C) 2018 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor NPointLattice (S: N_POINT_LATTICE_STRUCTS): N_POINT_LATTICE = 
struct

open S

val N = List.length names - 1

structure Set = DisjointSet

type value = int * (unit -> unit) AppendList.t ref List.t
datatype t = T of value Set.t

fun value (T s) = Set.! s

fun toString e =
   case value e of
      (n, _) => List.nth (names, n)

val layout = Layout.str o toString

fun new (): t = 
   T (Set.singleton (0, List.duplicate (N, fn () => ref AppendList.empty)))

fun equals (T s, T s') = Set.equals (s, s')

fun whenN (s, n', h') =
   case value s of
      (n, hss) => if n' < 0 orelse n' > N
                     then Error.bug "NPointLattice.whenN"
                  else if n >= n'
                     then h' ()
                  else AppendList.push (List.nth (hss, n' - n - 1), h')

fun isN (s, n') =
   case value s of
      (n, _) => if n' < 0 orelse n' > N
                   then Error.bug "NPointLattice.isN"
                else n = n'

fun up (T s) =
   case Set.! s of
      (n, hss) => if n = N
                     then ()
                  else (Set.:= (s, (n + 1, tl hss)) ;
                        AppendList.foreach (!(hd hss), fn h => h ()))

fun makeN (s, n') =
   case value s of
      (n, _) => if n' < 0 orelse n' > N
                   then Error.bug "NPointLattice.makeN"
                else if n >= n'
                   then ()
                else (up s ; makeN (s, n'))

fun from <= to =
   if equals (from, to)
      then ()
   else
      case (value from, value to) of
         ((n,hss), (n',_)) => 
            (List.foreachi
             (hss, fn (i,hs) =>
              if n + i + 1 > n'
                then AppendList.push (hs, fn () => makeN (to, n + i + 1))
                else ());
             makeN (to, n))

fun == (T s, T s') =
   if Set.equals (s, s')
      then ()
   else
      let 
         val e = Set.! s
         val e' = Set.! s'
         val _ = Set.union (s, s')
      in
         case (e, e') of
            ((n,hss), (n',hss')) =>
               let
                 val n'' = Int.max (n, n')

                 val (gss, hss) =
                    List.splitAt (hss, n'' - n)
                 val (gss', hss') =
                    List.splitAt (hss', n'' - n')
                 val hss'' =
                    List.map2
                    (hss, hss', fn (hs, hs') =>
                     ref (AppendList.append (!hs, !hs')))
                 fun runHandlers fss =
                    List.foreach
                    (fss, fn fs =>
                     AppendList.foreach
                     (!fs, fn f => f ()))
               in
                  Set.:= (s, (n'', hss''));
                  runHandlers gss;
                  runHandlers gss'
               end
      end

end
