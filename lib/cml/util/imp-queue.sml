(* imp-queue.sml
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

structure ImpQueue : IMP_QUEUE =
   struct
      datatype 'a t = T of {front: 'a list ref, back: 'a list ref}

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
            (Assert.assertAtomic' ("ImpQueue.cleanPrefix", NONE)
             ; case filterPrefix (!front, p) of
                  [] => (front := filterPrefix (List.rev(!back), p)
                         ; back := [])
                | front' =>  front := front')
         fun clean (T {front, back}, p) =
            (Assert.assertAtomic' ("ImpQueue.clean", NONE)
             ; case filter (!front, p) of
                  [] => (front := filterRev (!back, p)
                         ; back := [])
                | front' =>  (front := front'
                              ; back := filter (!back, p)))
         fun cleanAndDeque (T {front, back}, p) =
            (Assert.assertAtomic' ("ImpQueue.cleanAndDeque", NONE)
             ; case filter (!front, p) of
                  [] => (case filterRev(!back, p) of
                            [] => (front := []
                                   ; back := []
                                   ; NONE)
                          | x::front' => (front := front'
                                          ; back := []
                                          ; SOME x))
                | [x] => (front := filterRev (!back, p)
                          ; back := []
                          ; SOME x)
                | x::front' => (front := front'
                                ; back := filter (!back, p)
                                ; SOME x))
      end

      fun deque (T {front, back}) =
         (Assert.assertAtomic' ("ImpQueue.deque", NONE)
          ; case !front of
               [] => (case !back of
                         [] => NONE
                       | l => let val l = List.rev l
                              in case l of
                                    [] => raise Fail "ImpQueue.deque:impossible"
                                  | x :: front' => 
                                       (front := front'
                                        ; back := []
                                        ; SOME x)
                              end)
             | x::front' => (front := front'; SOME x))

      fun empty (T {front, back}) =
         (Assert.assertAtomic' ("ImpQueue.empty", NONE)
          ; case !front of
               [] => (case !back of
                         [] => true
                       | _ => false)
             | _ => false)

      fun enque (T {back, ...}, x) = 
         (Assert.assertAtomic' ("ImpQueue.enque", NONE)
          ; back := x::(!back))

      fun enqueAndClean (q, y, p) =
         (enque (q, y); clean (q, p))

      fun new () = T {front = ref [], back = ref []}

      fun peek (T {front, back}) =
         (Assert.assertAtomic' ("ImpQueue.peek", NONE)
          ; case !front of
               [] => (case !back of
                         [] => NONE
                       | l => let val l = List.rev l
                              in case l of
                                    [] => raise Fail "ImpQueue.peek:impossible"
                                  | x::front' => 
                                       (front := x::front'
                                        ; back := []
                                        ; SOME x)
                              end)
             | x::_ => SOME x)

      fun reset (T {front, back}) =
         (Assert.assertAtomic' ("ImpQueue.reset", NONE)
          ; front := []
          ; back := [])

(*
      val clean = fn arg => TimeIt.timeit "ImpQueue.clean" clean arg
      val cleanAndDeque = fn arg => TimeIt.timeit "ImpQueue.cleanAndDeque" cleanAndDeque arg
      val cleanPrefix = fn arg => TimeIt.timeit "ImpQueue.cleanPrefix" cleanPrefix arg
      val deque = fn arg => TimeIt.timeit "ImpQueue.deque" deque arg
      val empty = fn arg => TimeIt.timeit "ImpQueue.empty" empty arg
      val enque = fn arg => TimeIt.timeit "ImpQueue.enque" enque arg
      val enqueAndClean = fn arg => TimeIt.timeit "ImpQueue.enqueAndClean" enqueAndClean arg
      val new = fn arg => TimeIt.timeit "ImpQueue.new" new arg
      val peek = fn arg => TimeIt.timeit "ImpQueue.peek" peek arg
      val reset = fn arg => TimeIt.timeit "ImpQueue.reset" reset arg
*)
   end
