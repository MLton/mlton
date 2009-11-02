val version =
   case CommandLine.arguments () of
      [version] => version
    | _ => (print "Specific version as argument\n"; 
            OS.Process.exit OS.Process.failure)

fun hash x (c, w) = w * x + Word64.fromInt (Char.ord c)
fun escape x = hash x (#"\000", foldl (hash x) 0w0 (explode version))

val w32 = Word32.fromLarge o Word64.toLarge o escape
val w16 = Word16.fromLarge o Word64.toLarge o escape
val zero = "00000000"
fun pad i s = String.substring (zero, 0, i - String.size s) ^ s 
val w32 = pad 8 o Word32.toString o w32
val w16 = pad 4 o Word16.toString o w16

(* Some fat prime numbers *)
val a = 0w5746711073709751657
val b = 0w1892735987235987253
val c = 0w2098509180985089227
val d = 0w15198712489180714177
val e = 0w8904928971259057927
val f = 0w7690819081905790867

val () = 
   print (concat [ 
      w32 a, "-", w16 b, "-", w16 c, "-", w16 d, "-", w16 e, w32 f, "\n"
   ])
