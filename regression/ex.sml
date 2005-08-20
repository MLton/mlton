exception Foo of unit ref

fun f (x, r): int = if x then raise (Foo r) 
                   else (f (true, r); 1 + 2)
   
fun loop (r: unit ref): int =
   let val r' = ref ()
   in if r = r'
         then 13
      else f (false, r') handle Foo r => loop r
   end

val _ = loop (ref ())
