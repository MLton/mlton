(* fun-queue.sml
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

structure FunQueue : FUN_QUEUE =
   struct
      datatype 'a t = T of {front: 'a list, back: 'a list}

      local
         fun filterPrefix (xs, p) =
            case xs of
               [] => []
             | y::ys => if p y
                          then filterPrefix (ys, p)
                          else xs
         fun filter (xs, p) = List.filter (not o p) xs
         fun filterRevAcc ((xs, zs), p) =
            case xs of
               [] => zs
             | y::ys => if p y
                          then filterRevAcc ((ys, zs), p)
                          else filterRevAcc ((ys, y::zs), p)
         fun filterRev (xs, p) = filterRevAcc ((xs, []), p)
      in
         fun cleanPrefix (T {front, back}, p) =
            (case filterPrefix (front, p) of
                [] => T {front = filterPrefix (List.rev(back), p),
                         back = []}
              | front' =>  T {front = front',
                              back = back})
         fun clean (T {front, back}, p) =
            (case filter (front, p) of
                [] => T {front = filterRev (back, p),
                         back = []}
              | front' =>  T {front = front',
                              back = filter (back, p)})
         fun cleanAndDeque (T {front, back}, p) =
            (case filter (front, p) of
                [] => (case filterRev(back, p) of
                          [] => (NONE,
                                 T {front = [],
                                    back = []})
                        | x::front' => (SOME x,
                                        T {front = front',
                                           back = []}))
              | [x] => (SOME x,
                        T {front = filterRev (back, p),
                           back = []})
              | x::front' => (SOME x,
                              T {front = front',
                                 back = filter (back, p)}))
      end

      fun deque (T {front, back}) =
         (case front of
             [] => (case back of
                       [] => NONE
                     | l => let val l = List.rev l
                            in 
                               case l of
                                  [] => raise Fail "FunQueue.deque:impossible"
                                | x::front' => 
                                     SOME (x,
                                           T {front = front',
                                              back = []})
                            end)
           | x::front' => SOME (x, T {front = front', back = back}))
                          
      fun empty (T {front, back}) =
         (case front of
             [] => (case back of
                       [] => true
                     | _ => false)
           | _ => false)
             
      fun enque (T {front, back, ...}, x) = 
         T {front = front, back = x::back}

      fun enqueAndClean (q, y, p) =
         clean (enque (q, y), p)

      fun new () = T {front = [], back = []}

      fun peek (T {front, back}) =
         (case front of
             [] => (case back of
                       [] => NONE
                     | l => let val l = List.rev l
                            in 
                               case l of
                                  [] => raise Fail "FunQueue.peek:impossible"
                                | x::_ => SOME x
                            end)
           | x::_ => SOME x)
   end
