(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

functor Cache(Domain: T): CACHE =
struct

structure Domain = Domain

open PolyCache
   
type 'a t = (Domain.t, 'a) t

fun new() = PolyCache.new{equal = Domain.equals}
   
end 

