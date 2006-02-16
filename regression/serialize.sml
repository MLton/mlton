open MLton

val l = [1, 2, 3, 4]

structure W = Word8
structure V = Word8Vector

val r = ref 0
val t = (r, r)

fun pv v = (V.app (fn w => (print(W.toString w); print " ")) v
            ; print "\n")

fun pr s = (print s; print "\n")

fun pi i = (print(Int.toString i); print " ")
fun pl l = List.app pi l

fun 'a ds(a: 'a): 'a = deserialize(serialize a)
val pb = pr o Bool.toString
   
val _ =
   (pb(serialize l = serialize l)
    ; pl(ds l) ; print "\n"
    ; pb(l = ds l)
    ; pb(let val t: int ref * int ref = ds t
         in #1 t = #2 t
         end)
    ; pi(ds (fn x => x) 13)
    ; pi(ds (fn x => x + 1) 14)
    ; print "\n")
