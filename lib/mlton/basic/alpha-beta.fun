(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor AlphaBeta (S: ALPHA_BETA_STRUCTS): ALPHA_BETA = 
struct

open S

val traceAlphaBeta =
   Trace.trace3
   ("AlphaBeta.alphaBeta", State.layout, Value.layout, Value.layout, Value.layout)

fun messenger () =
   let
      val numCalls = ref 0
      val next = ref 1
      fun count f =
         (Int.inc numCalls
          ; if !numCalls = !next
               then
                  let
                     val _ = next := 2 * !next
                     open Layout
                  in
                     output (seq [str (Justify.justify (Int.toString (!numCalls),
                                                        10,
                                                        Justify.Left)),
                                  str " ",
                                  f ()],
                             Out.error)
                     ; Out.newline Out.error
                  end
            else ())
   in count
   end

fun alphaBeta arg =
   let
      val count = messenger ()
      fun alphaBeta arg : Value.t =
         traceAlphaBeta
         (fn (s: State.t, a: Value.t, b: Value.t) =>
          (count (fn () =>
                  let open Layout
                  in align [tuple [Value.layout a, Value.layout b],
                            State.layout s]
                  end)
           ; (case State.evaluate s of
                 State.Leaf v =>
                    if Value.<= (v, a)
                       then a
                    else if Value.<= (b, v)
                            then b
                         else v
               | State.NonLeaf {lower, upper} =>
                    if Value.<= (upper, a)
                       then a
                    else if Value.<= (b, lower)
                            then b
                         else
                            let
                               val a' = Value.move b
                               val b' = Value.move a
                               (* inv: a' <= b'' <= b' *)
                               fun loop (ss, b'') =
                                  if Value.equals (a', b'') then b''
                                  else
                                     case ss of
                                        [] => b''
                                      | s :: ss =>
                                           loop (ss, alphaBeta (s, a', b''))
                            in Value.unmove (loop (State.succ s, b'))
                            end))) arg
   in
      alphaBeta arg
   end

val alphaBetaNoCache = alphaBeta

structure Interval =
   struct
      datatype t = T of {lower: Value.t,
                         upper: Value.t}

      fun layout (T {lower, upper}) =
         if Value.equals (lower, upper)
            then Value.layout lower
         else let open Layout
              in seq [Value.layout lower, str "-", Value.layout upper]
              end

      val make = T

      local
         fun make f (T r) = f r
      in
         val lower = make #lower
         val upper = make #upper
      end

      val all = T {lower = Value.smallest,
                   upper = Value.largest}

      fun above (v: Value.t): t = T {lower = v, upper = Value.largest}

      fun below (v: Value.t): t = T {lower = Value.smallest, upper = v}

      fun isPoint (T {lower, upper}) = Value.equals (lower, upper)

      fun point v = T {lower = v, upper = v}

      fun closest (T {lower, upper}, v: Value.t): Value.t =
         if Value.<= (v, lower) then lower
         else if Value.>= (v, upper) then upper
              else v

      fun contains (T {lower, upper}, v: Value.t): bool =
         Value.<= (lower, v) andalso Value.<= (v, upper)

      fun move (T {lower, upper}): t =
         T {lower = Value.move upper,
           upper = Value.move lower}

      fun intersect (i: t, i': t): t =
         let val lower = Value.max (lower i, lower i')
            val upper = Value.min (upper i, upper i')
         in if Value.> (lower, upper)
               then Error.bug "AlphaBeta.Interval.intersect: empty intersection"
            else T {lower = lower, upper = upper}
         end
(*      val intersect = Trace.trace2 ("intersect", layout, layout, layout) intersect *)
   end

(* val trace =
 *    Trace.trace2 ("alphaBetaCache", State.layout, Interval.layout, Value.layout)
 * 
 * fun traceAlphaBeta f (s, i) =
 *    let val v = trace f (s, i)
 *       val v' = alphaBetaNoCache (s, Interval.lower i, Interval.upper i)
 *    in if Value.equals (v, v')
 *       then ()
 *       else Misc.bug (let open Layout
 *                  in align [str "v = ", Value.layout v,
 *                           str "v' = ", Value.layout v']
 *                  end);
 *       v
 *    end
 * 
 * val traceSearch = Trace.trace ("search", Interval.layout, Value.layout)
 *)

fun alphaBetaCache (s: State.t, i: Interval.t, c: Interval.t Cache.t): Value.t =
   let
      val count = messenger ()
      fun alphaBeta (s: State.t, i: Interval.t): Value.t =
         (count (fn () =>
                 let open Layout
                 in align [Interval.layout i, State.layout s]
                 end)
          ; (case State.evaluate s of
                State.Leaf v => Interval.closest (i, v)
              | State.NonLeaf {lower, upper} =>
                   if Value.<= (upper, Interval.lower i)
                      then Interval.lower i
                   else if Value.<= (Interval.upper i, lower)
                           then Interval.upper i
                        else
                           let
                              val {update, value} = Cache.peek (c, s)
                              fun search iKnown =
                                 let
                                    val iSearch = Interval.intersect (i, iKnown)
                                    val Interval.T {lower, upper} =
                                       Interval.move iSearch
                                    (* inv: lower <= v <= upper *)
                                    fun loop (ss, v) =
                                       if Value.equals (lower, v) then v
                                       else
                                          case ss of
                                             [] => v
                                           | s :: ss =>
                                                loop
                                                (ss,
                                                 alphaBeta
                                                 (s, Interval.T {lower = lower,
                                                                 upper = v}))
                                    val v =
                                       Value.unmove (loop (State.succ s, upper))
                                    val Interval.T {lower, upper} = iSearch
                                    val iKnown =
                                       Interval.intersect
                                       (iKnown,
                                        if Value.equals (v, upper)
                                           then Interval.above upper
                                        else if Value.equals (v, lower)
                                                then Interval.below lower
                                             else Interval.point v)
                                 in (*Misc.assert (fn () =>
                                     Interval.contains
                                     (iKnown,
                                     alphaBetaNoCache (s, Value.smallest,
                                     Value.largest))); *)
                                    update iKnown; v
                                 end
                           in
                              case value of
                                 SOME i' =>
                                    let
                                       val Interval.T {lower, upper} = i
                                       val Interval.T {lower = lower', upper = upper'} =
                                          i'
                                    in if Value.<= (upper', lower)
                                          then lower
                                       else if Value.>= (lower', upper)
                                               then upper
                                            else search i'
                                    end
                               | NONE => search Interval.all
                           end))
   in alphaBeta (s, i)
   end

end
