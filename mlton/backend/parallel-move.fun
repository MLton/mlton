(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor ParallelMove(S: PARALLEL_MOVE_STRUCTS): PARALLEL_MOVE = 
struct

open S

fun ('register, 'statement) move{moves, equals, move, interfere, temp}
   : 'statement list =
   let
      val mvs =
	 List.fold(moves, [], fn (mv as {src, dst}, mvs) =>
		   if equals(src, dst)
		      then mvs
		   else mv :: mvs)
      fun loopTop(mvs, moves) = loop(mvs, [], moves, false)
      and loop(mvs, hard, moves, changed) =
	 case mvs of
	    [] =>
	       (case hard of
		   [] => List.rev moves
		 | {src, dst} :: hard' => 
		      if changed
			 then loopTop(hard, moves)
		      else
			 let
			    val (hard, moves) =
			       List.fold
			       (hard', ([], moves),
				fn (mv as {src = s, dst = d}, (hard, moves)) =>
				if interfere{write = dst, read = s}
				   then let val temp = temp s
					in ({src = temp, dst = d} :: hard,
					    move{dst = temp, src = s}
					    :: moves)
					end
				else (mv :: hard, moves))
			    val moves = move{src = src, dst = dst} :: moves
			 in loopTop(hard, moves)
			 end)
	  | (mv as {src, dst}) :: mvs =>
	       let
		  fun isHard l =
		     List.exists(l, fn {src, dst = _} =>
				 interfere{write = dst, read = src})
	       in if isHard mvs orelse isHard hard
		     then loop(mvs, mv :: hard, moves, changed)
		  else loop(mvs, hard,
			    move{src = src, dst = dst} :: moves,
			    true)
	       end
   in loopTop(mvs, [])
   end

end
