(* Access the current stack in the heap via a MLton.size object trace. *)
val rt : MLton.Thread.Runnable.t option ref = ref NONE
val rs : int ref = ref 0

fun stats () =
   let
      val () = rs := MLton.size rt
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

val () = switcheroo ()
val _ = print (concat ["!rs > 0 = ", Bool.toString (!rs > 0), "\n"])
