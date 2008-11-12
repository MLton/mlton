(* Access the current stack in the heap via a MLton.share object trace. *)
val rt : MLton.Thread.Runnable.t option ref = ref NONE

fun stats () =
   let
      val () = MLton.share rt
   in
      ()
   end

fun switcheroo () =
   MLton.Thread.switch
   (fn t => let
               val () = rt := SOME (MLton.Thread.prepare (t, ()))
               val () = stats ()
            in
               valOf (!rt)
            end)

(* tuple option array *)
val a = Array.tabulate (100, fn i => SOME (i mod 2, i mod 3))
val () = Array.update (a, 0, NONE)

fun touch () =
   let
      val size = MLton.size a
      val sum =
         Array.foldr (fn (NONE,sum) => sum
                       | (SOME (a, b),sum) => a + b + sum)
                     0 a
   in
      (size, sum)
   end

val (size1,sum1) = touch ()
val () = switcheroo ()
val (size2,sum2) = touch ()
val _ = print (concat ["size1 >= size2 = ", Bool.toString (size1 >= size2), "\n"])
val _ = print (concat ["sum1 = sum2 = ", Bool.toString (sum1 >= sum2), "\n"])
