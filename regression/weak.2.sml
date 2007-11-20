structure Weak = MLton.Weak

val x = (13, ref 5)
val wx = Weak.new x
fun isAlive () = isSome (Weak.get wx)
val _ = MLton.GC.collect ()
val _ = print (Bool.toString (isAlive ()) ^ "\n")

(*
fun sum (x : int * (int list ref vector * int * int) ref) =
   #1 x +
   #2 (! (#2 x)) +
   #3 (! (#2 x)) +
   Vector.foldr (fn (lr,s) => List.foldr (op +) s (!lr)) 0 (#1 (! (#2 x)))

val x = (13, ref (Vector.tabulate (10, fn n => ref (List.tabulate (n, fn i => i))),
                  4, 
                  5))
val wx = Weak.new x
fun isAlive () = isSome (Weak.get wx)
val _ = MLton.GC.collect ()
val _ = print (Bool.toString (isAlive ()) ^ "\n")
val s1 = sum x
*)