structure Lines: LINES =
struct

fun startStop(ins, out, {start: int, stop: int}): unit =
   let
      val _ = Assert.assert("Lines.startStop", fn () => start <= stop)
      fun loop i =
	 if i > stop
	    then ()
	 else (case In.inputLine ins of
		  "" => ()
		| l => 
		     (if i >= start
			 then Out.output(out, l)
		      else ();
		      loop(i + 1)))
   in loop 0
   end

fun dropLast(ins, out, {start: int, last: int}): unit =
   let
      val _ = Assert.assert("Lines.dropLast", fn () =>
			    start >= 0 andalso last >= 0)
      fun line() = In.inputLine ins
      val _ = Int.for(0, start, fn _ => (line(); ()))
   in if last = 0
	 then In.outputAll(ins, out)
      else
	 let val q = Int.fold(0, last, Queue.empty(), fn (_, q) =>
			      Queue.enque(q, line()))
	    fun loop(q: string Queue.t) =
	       if In.endOf ins
		  then ()
	       else let val q = Queue.enque(q, line())
			val (l', q) = valOf(Queue.deque q)
			val _ = Out.output(out, l')
		    in loop q
		    end
	 in loop q
	 end
   end


end
