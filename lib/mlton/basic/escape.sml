structure Escape: ESCAPE =
struct

type 'a t = 'a -> exn

fun escape (e, a) = raise (e a)
   
fun new (f: 'a t -> 'a): 'a =
   let exception E of 'a
   in f E handle E a => a
   end
   
end
