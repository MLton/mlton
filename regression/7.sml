fun h f = f 13
   
fun f x = let fun z x = z(x + 1)
          in h z
          end
   
val r : (int -> int) ref = ref f

val _ = h(fn x => x)
