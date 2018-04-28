(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor PolyCache(): POLY_CACHE =
struct

datatype ('a, 'b) t = T of {equal: ('a * 'a) -> bool,
                           elts: ('a * 'b) list ref}

fun fromList{equal, elements} = T{equal = equal, elts = ref elements}

fun new{equal} = T{equal = equal, elts = ref []}

fun toList(T{elts, ...}) = !elts

fun size c = List.length(toList c)
fun foreach(c, f) = List.foreach(toList c, f)

fun peek(T{equal, elts = ref l}, x) =
   case List.peek(l, fn (x', _) => equal(x, x')) of
      NONE => NONE
    | SOME(_, y) => SOME y

fun lookup cx = valOf(peek cx)

fun toFunction c a = lookup(c, a)

fun addNew(T{elts = r as ref l, ...}, x, y) = r := (x, y) :: l

fun getOrAdd(c, x, th) =
   case peek(c, x) of
      NONE => let val y = th()
              in addNew(c, x, y) ; y
              end
    | SOME y => y

fun eq(T{elts=r, ...}, T{elts=r', ...}) = r = r'

end 

structure PolyCache = PolyCache()
