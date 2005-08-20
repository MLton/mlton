(* fun-queue.sml
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

functor FunPriorityQueue(S: FUN_PRIORITY_QUEUE_ARG) : 
   FUN_PRIORITY_QUEUE where type Key.t = S.Key.t =
   struct
      open S

      structure Elt =
         struct
            datatype 'a t = T of Key.t * 'a
            fun key (T (k, _)) = k
            fun value (T (_, v)) = v
         end

      datatype 'a t = T of 'a Elt.t list

      local
         fun filterPrefix (xs, p) =
            case xs of
               [] => []
             | y::ys => if p y
                          then filterPrefix (ys, p)
                          else xs
         fun filter (xs, p) = List.filter (not o p) xs
      in
         fun cleanPrefix (T xs, p) = T (filterPrefix (xs, p))
         fun clean (T xs, p) = T (filter (xs, p))
      end

      fun deque (T xs) =
         (case xs of
             [] => NONE
           | x::xs => SOME (x, T xs))

      fun cleanAndDeque (q, p) =
         let
            val q = clean (q, p)
         in
            case deque q of
               NONE => (NONE, q)
             | SOME (x, q) => (SOME x, q)
         end

      fun empty (T xs) = 
         (case xs of 
             [] => true
           | _ => false)

      fun enque (T xs, k', v') =
         let
            val x' = Elt.T (k', v')
            fun loop (xs, ys) =
               case xs of
                  [] => List.revAppend(ys, [x'])
                | (z as Elt.T (k, _))::zs => 
                     (case Key.compare (k, k') of
                         GREATER => List.revAppend(ys, x'::xs)
                       | _ => loop(zs, z::ys))
         in
            T (loop (xs, []))
         end

      fun enqueAndClean (q, k, v, p) =
         clean (enque (q, k, v), p)

      fun new () = T []

      fun peek (T xs) =
         (case xs of
             [] => NONE
           | elt::_ => SOME elt)
   end
