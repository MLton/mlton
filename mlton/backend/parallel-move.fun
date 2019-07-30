(* Copyright (C) 2019 Matthew Fluet
 * Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor ParallelMove (S: PARALLEL_MOVE_STRUCTS): PARALLEL_MOVE = 
struct

open S

fun ('temporary, 'statement) move {moves, equals, move, interfere, temp}
   : 'statement list =
   let
      val mvs =
         List.fold (moves, [], fn (mv as {src, dst}, mvs) =>
                   if equals (src, dst)
                      then mvs
                   else mv :: mvs)
      fun loopTop (mvs, moves) = loop (mvs, [], moves, false)
      and loop (mvs, hard, moves, changed) =
         case mvs of
            [] =>
               (case hard of
                   [] => List.rev moves
                 | {src, dst} :: hard' => 
                      if changed
                         then loopTop (hard, moves)
                      else
                         let
                            val (hard, moves) =
                               List.fold
                               (hard', ([], moves),
                                fn (mv as {src = s, dst = d}, (hard, moves)) =>
                                if interfere (dst, s)
                                   then let val temp = temp s
                                        in ({src = temp, dst = d} :: hard,
                                            case move {dst = temp, src = s} of
                                               NONE => moves
                                             | SOME move => move :: moves)
                                        end
                                else (mv :: hard, moves))
                            val moves =
                               case move {src = src, dst = dst} of
                                  NONE => moves
                                | SOME move => move :: moves
                         in loopTop (hard, moves)
                         end)
          | (mv as {src, dst}) :: mvs =>
               let
                  fun isHard l =
                     List.exists (l, fn {src, dst = _} =>
                                 interfere (dst, src))
               in if isHard mvs orelse isHard hard
                     then loop (mvs, mv :: hard, moves, changed)
                  else loop (mvs, hard,
                             case move {src = src, dst = dst} of
                                NONE => moves
                              | SOME move => move :: moves,
                            true)
               end
   in loopTop (mvs, [])
   end

end
