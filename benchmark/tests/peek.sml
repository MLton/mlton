(* Written by Stephen Weeks (sweeks@sweeks.com). *)
structure Plist:
   sig
      type t

      val new: unit -> t
      val addPeek: unit -> {add: t * 'a -> unit,
                            peek: t -> 'a option}
   end =
   struct
      datatype t = T of exn list ref

      fun new () = T (ref [])

      fun addPeek () =
         let
            exception E of 'a
            fun add (T r, x) = r := E x :: !r
            fun peek (T r) =
               let
                  val rec loop =
                     fn [] => NONE
                      | E x :: _ => SOME x
                      | _ :: l => loop l
               in loop (!r)
               end
         in {add = add, peek = peek}
         end
   end

structure Main =
   struct
      fun inner () =
         let
            val l1 = Plist.new ()
            val l2 = Plist.new ()
            val {add = addA, peek = peekA} = Plist.addPeek ()
            val {add = addB, peek = peekB} = Plist.addPeek ()
            val {add = addC, peek = peekC} = Plist.addPeek ()
            val {add = addD, peek = peekD} = Plist.addPeek ()
            val _ = addA (l1, 13: Int32.int)
            val _ = addB (l1, 15: Int64.int)
            val _ = addC (l1, 17: Int32.int)
            val _ = addD (l1, 19: Int64.int)
            val _ = addA (l2, 19: Int32.int)
            val _ = addB (l2, 17: Int64.int)
            val _ = addC (l2, 15: Int32.int)
            val _ = addD (l2, 13: Int64.int)
            fun peek l =
               Int32.toInt (valOf (peekA l1)) + Int64.toInt (valOf (peekB l))
               + Int32.toInt (valOf (peekC l)) + Int64.toInt (valOf (peekD l))
            fun loop (i, ac1, ac2) =
               if i = 0
                  then (ac1, ac2)
               else loop (i - 1, ac1 + peek l1, ac2 + peek l2)
            val (n1, n2) = loop (10000000, 0, 0)
            val _ =
               if n1 <> 640000000 orelse n2 <> 580000000
                  then raise Fail "bug"
                  else ()
         in ()
         end

      fun doit size =
         let
            fun loop i =
               if i = 0
                  then ()
               else (inner (); loop (i - 1))
         in loop size
         end
   end
