(*
 * On my 400MhZ system, thread-switch 10000000 takes 4.98s, which comes out to
 * 2,008,032 switches per second.
 *)
   
structure Main =
struct

type int = Int.int
open MLton
open Thread

datatype t = T of (int * t) Thread.t

val done: Thread.Runnable.t option ref = ref NONE
   
fun loop (n: int, T t): unit =
   if n = 0
      then switch (fn _ => valOf (!done))
   else
      let
         val (n, t) = switch (fn t' => prepare (t, (n - 1, T t')))
      in
         loop(n, t)
      end
   
fun main () =
   let
      val numSwitches =
         case CommandLine.arguments () of
            [] => 1000
          | s :: _ => valOf (Int.fromString s)
   in
      switch (fn cur =>
              (done := SOME (prepare (cur, ()))
               ; prepare (new loop, (numSwitches, T (new loop)))))
      ; print "ok\n"
   end

end

val _ = Main.main ()
(*    SMLofNJ.exportFn
 *    ("thread-switch", fn _ => (Main.main(); OS.Process.success))
 *)
