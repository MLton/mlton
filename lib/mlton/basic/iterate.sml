(*-------------------------------------------------------------------*)
(*                              Iterate                              *)
(*-------------------------------------------------------------------*)

structure Iterate: ITERATE =
struct
   
fun iterate(start, isTerm,next) =
   let fun loop s = if isTerm s then s else loop(next s)
   in loop start
   end
   
fun whileDo(test, f) = iterate((),not o test, f)
   
fun repeatUntil(f, test) = (f() ; iterate((), test, f))
   
end
