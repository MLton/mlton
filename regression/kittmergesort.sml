(*kittmergesort.sml*)

(* This is tmergesort taken from Paulson's book , page 99 *)

(* The merge function has been modified slightly, to 
   traverse and rebuild both arguments fully, even when
   the one argument is empty. This ensures that both
   recursive calls of tmergesort in itself can put their 
   results in regions local to the body of tmergesort.

   One can show that the maximum number of live list elements
   is 3n, where n is the number of elements to be sorted.
   For n=50000 this should give an approximate memory usage of
   3 * 50.000 list elements * 5 words/list element * 4 bytes/word=
   3Mb. The actual memory usage (run24d) is 4.5Mb. The remaining
   1.5Mb is probably largely due to the fact that merge puts
   stuff on the stack (as it is not tail recursive).

*)

exception Take and Drop

fun take(0, _ ) = []
  | take(n, x::xs) = x::take(n-1, xs)
  | take(n, []) = raise Take

fun drop(0, l) = l
  | drop(n, x::xs) = drop(n-1, xs)
  | drop(n, []) = raise Drop

fun digit n = chr(ord #"0" + n)

fun digits(n,acc) =
      if n >=0 andalso n<=9 then digit n:: acc
      else digits (n div 10, digit(n mod 10) :: acc)
fun int_to_string(n) = implode(digits(n,[#"\n"]))

fun snd(x,y) = y

val a = 167
val m = 2147
fun nextrand(seed) =
   let val t = a*seed
   in t - (m*(t div m))
   end

fun randlist(n,seed,tail)=
    if n=0 then (seed,tail)
    else randlist(n-1, nextrand seed, seed::tail)


fun length [] = 0
  | length (_::xs) = 1+length xs

fun merge([], ys) = (ys:int list)@[]
  | merge(xs, []) = xs @[]
  | merge(l as x::xs, r as y:: ys) =
      if x<= y then x::merge(xs, r)
      else y:: merge(l, ys)

fun tmergesort [] = []
  | tmergesort [x] = [x]
  | tmergesort xs =
      let val k = length xs div 2
      in merge(tmergesort(take(k, xs)),
               tmergesort(drop(k, xs)))
      end
 

val result = 
let 
  val n = 50000
  val xs = snd(randlist(n,1,[]))
  val _ = print "\n List generated\n"
  fun report msg = print(msg^"\n")
in
  report "Doing tmergesort...";
  tmergesort xs;
  report("Sorted " ^ int_to_string n ^ " numbers\n")
end
    
