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
            val l = Plist.new ()
            val {add, peek} = Plist.addPeek ()
            val _ = add (l, 13)
            fun loop (i, ac) =
               if i = 0
                  then ac
               else loop (i - 1, ac + valOf (peek l))
            val n = loop (10000000, 0)
            val _ = print (concat [Int.toString n, "\n"])
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
