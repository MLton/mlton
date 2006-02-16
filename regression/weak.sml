structure Weak = MLton.Weak

val w = Weak.new 13
val _ =
   if isSome (Weak.get w)
      then raise Fail "bug int"
   else ()

fun testIntInf (i: IntInf.int) =
   let
      val w = Weak.new i
      val _ =
         case Weak.get w of
            NONE => raise Fail "bug IntInf"
          | SOME i => print (concat [IntInf.toString i, "\n"])
   in
      ()
   end
val _ = testIntInf 13
val _ = testIntInf 12345678901234567890
      
val r = ref 13
val n = 2
val rs = Array.tabulate (n, ref)
val ws = Array.tabulate (n, fn i => Weak.new (Array.sub (rs, i)))
fun isAlive i = isSome (Weak.get (Array.sub (ws, i)))
val _ = MLton.GC.collect ()
val _ =
   if isAlive 0 andalso isAlive 1
      then ()
   else raise Fail "bug1"
fun clear i = Array.update (rs, i, r)
fun sub i = ! (Array.sub (rs, i))
fun pi x = print (concat [Int.toString x, "\n"])
val _ = pi (sub 0 + sub 1)
val _ = valOf (Weak.get (Array.sub (ws, 0))) := 12345
val _ = clear 1
val _ = MLton.GC.collect ()
val _ =
   if isAlive 0 andalso not (isAlive 1)
      then ()
   else raise Fail "bug2"
val _ = pi (sub 0)
val _ = clear 0
val _ = MLton.GC.collect ()
val _ =
   if not (isAlive 0) andalso not (isAlive 1)
      then ()
   else raise Fail "bug2"

