functor Cache(Domain: T): CACHE =
struct

structure Domain = Domain

open PolyCache
   
type 'a t = (Domain.t, 'a) t

fun new() = PolyCache.new{equal = Domain.equals}
   
end 

