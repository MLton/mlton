(* parse the entire standard input *)
fun parse s =  Vector.fromList (String.fields (fn c => c = #";") s)
fun scanin h = 
   case TextIO.inputLine h of
      NONE => []
    | SOME s => s :: scanin h

fun hex2int s = valOf (StringCvt.scanString (Int.scan StringCvt.HEX) s)
val UnicodeData = List.map parse (scanin (TextIO.openIn "UnicodeData.txt"))
val CodePoints = Vector.fromList (List.map (fn v => hex2int (Vector.sub (v, 0))) UnicodeData)

fun hash (s, f, n) =
   let
      val a = Array.tabulate (n, fn _ => 0)
      val clashes = ref 0
      
      val sw = Word32.fromInt s
      val fw = Word32.fromInt f
      val nw = Word32.fromInt n
      
      fun inject x =
         let
            val xw = Word32.fromInt x
            val k = (Word32.>> (xw, sw) * fw) + xw (* 6..15 *)
            val k = Word32.toInt (Word32.mod (k, nw))
            val v = Unsafe.Array.sub (a, k) + 1
         in
            if v > 1 then clashes := !clashes + 1 else ();
            Unsafe.Array.update (a, k, v)
         end
   in
      Vector.app inject CodePoints;
      !clashes
   end

(* 21 bits of data, 14 bits used, 15+5 used *)

(* 10,  5919, 32768 -> 52, 2 *)
(* 12,  5537, 32768 -> 51, 2 *)
(* 14, 16837, 32768 ->  8, 2 *)
(* 14,  6162, 33739 ->  5, 2 *)
(* 13, 10941, 52007 -> 0 *)

fun better (best, clashes) = best < clashes

fun loopshift (n, f) (s, best) =
   if s = 17 then best else
   let val trial = hash (s, f, n) in
   if trial > best then loopshift (n, f) (s + 1, best) else
   (print (Int.toString s ^ ", " ^ Int.toString f ^ ", " ^ Int.toString n ^ " -> " ^ Int.toString best ^ "\n");
    loopshift (n, f) (s + 1, trial)) end
   
fun loopfact n (f, best) = 
   let val best = loopshift (n, f) (10, best) in
   if f = n then best else loopfact n (f + 1, best) end

fun loopmod best =
   let
      val n = Word.toInt (MLton.Random.rand () mod 0w31500) + 10000
      val best = loopfact n (1, best) 
   in
      loopmod best
   end

val () = MLton.Random.srand (valOf (MLton.Random.useed ()))
(*
val best = loopfact 32768 (1, 5000) (* a useful size due to bit arith *)
val best = loopfact 65536 (1, best) (* a useful size due to bit arith *)
*)
val () = loopmod 0
