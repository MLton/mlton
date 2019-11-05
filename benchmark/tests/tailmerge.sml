(* Written by Stephen Weeks (sweeks@sweeks.com). *)
fun merge (l1: int list, l2) =
   let
      fun revapp (l, acc) =
         case l of
            [] => acc
          | h :: t => revapp (t, h :: acc)
      fun loop (l1, l2, acc) =
         case (l1, l2) of
            ([], _) => revapp (acc, l2)
          | (_, []) => revapp (acc, l1)
          | (x1 :: l1', x2 :: l2') =>
               if x1 <= x2
                  then loop (l1', l2, x1 :: acc)
                  else loop (l1, l2', x2 :: acc)
   in
      loop (l1, l2, [])
   end

structure Main =
   struct
      fun doit size =
         let
            val len = 100000
            val l1 = List.tabulate (len, fn i => i * 2)
            val l2 = List.tabulate (len, fn i => i * 2 + 1)

            fun test () =
               if 0 = hd (merge (l1, l2))
                  then ()
               else raise Fail "bug"

            fun loop n =
               if n = 0
                  then ()
               else (test (); loop (n - 1))
         in
            loop size
         end
   end
