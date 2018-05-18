(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor MergeSort (S:
                   sig
                      type 'a t
                      val make: ('a * 'a -> bool) -> {isSorted: 'a t -> bool,
                                                      merge: 'a t * 'a t -> 'a t,
                                                      sort: 'a t -> 'a t}
                   end): MERGE_SORT =
   struct
      open S

      fun isSorted (l, le) = #isSorted (make le) l
      fun merge (l, l', le) = #merge (make le) (l, l')
      fun sort (l, le) = #sort (make le) l
   end

structure MergeSortList: MERGE_SORT =
   MergeSort
   (type 'a t = 'a list

    (* This is a variant of mergesort that runs in O (n log n) time. *)
    fun make (op <= : 'a * 'a -> bool) =
       let
          fun assert f = Assert.assert ("MergeSort.assert", f)
          fun isSorted l =
             case l of
                [] => true
              | x :: l =>
                   let
                      fun loop (x, l) =
                         case l of
                            [] => true
                          | x' :: l => x <= x' andalso loop (x', l)
                   in loop (x, l)
                   end
          fun merge (l1, l2) =
             (assert (fn () => isSorted l1 andalso isSorted l2)
              ; (case (l1, l2) of
                    ([], _) => l2
                  | (_, []) => l1
                  | (x1 :: l1', x2 :: l2') =>
                       if x1 <= x2
                          then x1 :: merge (l1', l2)
                       else x2 :: merge (l1, l2')))
          fun sort l =
             let
                val numBuckets = 25
                val _ = assert (fn () => length l < Int.pow (2, numBuckets) - 1)
                val a: 'a list array = Array.new (numBuckets, [])
                fun invariant () =
                   assert (fn () => Array.foralli (a, fn (i, l) =>
                                                   case l of
                                                      [] => true
                                                    | _ => (length l = Int.pow (2, i)
                                                            andalso isSorted l)))
                fun mergeIn (i: int, l: 'a list): unit =
                   (assert (fn () => length l = Int.pow (2, i))
                    ; (case Array.sub (a, i) of
                          [] => Array.update (a, i, l)
                        | l' => (Array.update (a, i, [])
                                 ; mergeIn (i + 1, merge (l, l')))))
                val _ = List.foreach (l, fn x => mergeIn (0, [x]))
                val l = Array.fold (a, [], fn (l, l') =>
                                    case l of
                                       [] => l'
                                     | _ => merge (l, l'))
                val _ = assert (fn () => isSorted l)
             in l
             end
       in
          {isSorted = isSorted,
           merge = merge,
           sort = sort}
       end)

structure MergeSortVector: MERGE_SORT =
   MergeSort
   (type 'a t = 'a vector

    fun make (op <=) =
       let
          fun isSorted v = Vector.isSorted (v, op <=)
          fun merge (v, v') =
             let
                val _ = Assert.assert ("MergeSortVector.merge: pre", fn () =>
                                       isSorted (v, op <=)
                                       andalso isSorted (v', op <=))
                val n = length v
                val n' = length v'
                val r = ref 0
                val r' = ref 0
                fun next _ =
                   let
                      val i = !r
                      val i' = !r'
                   (* 0 <= i <= n andalso 0 <= i' <= n' *)
                   in
                      if i = n
                         then
                            let
                               val res = sub (v', i')
                               val _ = Int.inc r'
                            in res
                            end
                      else if i' = n'
                              then 
                                 let
                                    val res = sub (v, i)
                                    val _ = Int.inc r
                                 in res
                                 end
                           else
                              let
                                 val a = sub (v, i)
                                 val a' = sub (v', i')
                              in
                                 if a <= a'
                                    then (Int.inc r; a)
                                 else (Int.inc r'; a')
                              end
                   end
                val v = tabulate (n + n', fn _ => next ())
                val _ = Assert.assert ("MergeSortVector.merge: post", fn () =>
                                       isSorted (v, op <=))
             in
                v
             end

          fun sort v =
             let
                fun loop v =
                   if isSorted (v, op <=)
                      then v
                   else
                      let
                         val n = length v
                         val m = n div 2
                         val m' = n - m
                         fun get (m, start) =
                            loop
                            (tabulate (m, 
                                       let val r = ref start
                                       in fn _ =>
                                          let
                                             val i = !r
                                             val res = sub (v, i)
                                             val _ = r := 2 + i
                                          in res
                                          end
                                       end))
                      in merge (get (m', 0), get (m, 1), op <=)
                      end
                val v = loop v
                val _ = Assert.assert ("MergeSortVector.sort", fn () =>
                                       isSorted (v, op <=))
             in
                v
             end
       in
          {isSorted = isSorted,
           merge = merge,
           sort = sort}
       end)
