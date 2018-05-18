(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Lines: LINES =
struct

fun startStop(ins, out, {start: int, stop: int}): unit =
   let
      val _ = Assert.assert("Lines.startStop", fn () => start <= stop)
      fun loop i =
         if i > stop
            then ()
         else (case In.inputLine ins of
                  NONE => ()
                | SOME l => 
                     (if i >= start
                         then Out.output(out, l)
                      else ();
                      loop(i + 1)))
   in loop 0
   end

fun dropLast (ins, out, {start: int, last: int}): unit =
   let
      val _ = Assert.assert ("Lines.dropLast", fn () =>
                             start >= 0 andalso last >= 0)
      fun line () = In.inputLine ins
      val _ = Int.for (0, start, fn _ => ignore (line ()))
   in
      if last = 0
         then In.outputAll (ins, out)
      else
         let
            val q =
               Int.fold (0, last, Queue.empty (), fn (_, q) =>
                         Queue.enque (q,
                                      case line () of
                                         NONE => ""
                                       | SOME l => l))
            fun loop (q: string Queue.t) =
               case line () of
                  NONE => ()
                | SOME l =>
                     let
                        val q = Queue.enque (q, l)
                        val (q, l') = valOf (Queue.deque q)
                        val _ = Out.output (out, l')
                    in
                       loop q
                    end
         in loop q
         end
   end


end
