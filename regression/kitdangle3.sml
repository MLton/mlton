(*kitdangle3.sml*)

infix - + :: =
(*fun op = (x: ''a, y: ''a): bool =           prim("=", "=", (x, y)) *)

exception Hd
fun hd (x::l) = x
  | hd [] = raise Hd

fun mklist 0 = []
  | mklist n = n :: mklist(n-1)

fun cycle(p as (m,f)) = 
  if m=0 then p
  else cycle(m-1, 
         let val x = [(m, mklist 2000)]
         in fn () => #1(hd x) + f()
         end)

val r = ((cycle(1000, fn() => 0);()),
         (cycle(1000, fn() => 0);()),
         (cycle(1000, fn() => 0);()))
