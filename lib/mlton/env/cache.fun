(* Copyright (C) 1999-2006, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Cache(Domain: T): CACHE =
struct

structure Domain = Domain

open PolyCache

type 'a t = (Domain.t, 'a) t

fun new() = PolyCache.new{equal = Domain.equals}

end 
