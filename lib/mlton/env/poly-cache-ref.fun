(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor PolyCache(): POLY_CACHE =
struct

datatype ('a, 'b) t = T of {equal: ('a * 'a) -> bool,
                           elts: ('a * 'b ref) list ref}

fun fromList 

fun new equal = T{equal = equal, elts = ref []}

fun all(T{elts, ...}) = List.map(!elts, fn (x, y) => (x, !y))

fun peekR(T{equal, elts = ref l}, x) =
   case List.keepFirst(l, fn (x', _) => equal(x, x')) of
      NONE => NONE
    | SOME(_, y) => SOME y

fun peek(c, x) =
   case peekR(c, x) of
      NONE => NONE
    | SOME r => SOME(!r)

fun lookup cx = Option.project(peek cx)

fun addNew(T{elts = r as ref l, ...}, x, y) =
   r := (x, ref y) :: l

fun set(c, x, y) =
   case peekR(c, x) of
      NONE => addNew(c, x, y)
    | SOME r => r := y

fun getOrAdd(c, x, th) =
   case peek(c, x) of
      NONE => let val y = th()
              in addNew(c, x, y) ; y
              end
    | SOME y => y

fun eq(T{elts=r, ...}, T{elts=r', ...}) = r = r'

end 

structure PolyCache = PolyCache()
