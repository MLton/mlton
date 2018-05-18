(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor DisjointSet ():> DISJOINT_SET =
struct

datatype 'a t = T of 'a parent ref
and 'a parent =
   Parent of 'a t
  | Root of {value: 'a, rank: int}

fun singleton v = T (ref (Root {value = v, rank = 0}))

val rank =
   fn T (ref (Root {rank, ...})) => rank
    | _ => Error.bug "DisjointSet.rank"

val setRank =
   fn (T (r as ref (Root {value, ...})), rank) =>
        r := Root {value = value, rank = rank}
    | _ => Error.bug "DisjointSet.setRootValue"

fun incrementRank r = setRank (r, rank r + 1)

val parent =
   fn T (ref (Parent p)) => p
    | _ => Error.bug "DisjointSet.parent"    

fun setParent (T r, p) = r := Parent p

val rootValue =
   fn T (ref (Root {value, ...})) => value
    | _ => Error.bug "DisjointSet.rootValue"

val setRootValue =
   fn (T (r as ref (Root {rank, ...})), v) => r := Root {value = v, rank = rank}
    | _ => Error.bug "DisjointSet.setRootValue"

fun equal (T r, T r') = r = r'

val isRoot  =
   fn T (ref (Root _)) => true
    | _ => false

val isRepresentative = isRoot

fun root s = if isRoot s then s
             else let val r = root (parent s)
                  in setParent (s, r)
                     ; r
                  end

val representative = root

fun ! s = rootValue (root s)

fun s := v = setRootValue (root s, v)

val equals = fn (s1, s2) => equal (root s1, root s2)

fun union (s, s') =
   let val r = root s
      val r' = root s'
   in if equal (r, r') then ()
      else let val n = rank r
               val n' = rank r'
           in if n < n' then setParent (r, r')
              else (setParent (r', r)
                    ; if Int.equals (n, n') then incrementRank r else ())
           end
   end

fun canUnion (s, s', f) =
   equals (s, s')
   orelse (case f (! s, ! s') of
              NONE => false
            | SOME v => (union (s, s')
                         ; s := v
                         ; true))

end

structure DisjointSet = DisjointSet ()
