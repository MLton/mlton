(* Written by Stephen Weeks (sweeks@sweeks.com). *)

structure Main =
   struct
      fun doit n =
         let
            open TextIO
            val f = OS.FileSys.tmpName ()
            val out = openOut f
            val _ =
               output (out,
                       String.implode
                       (List.tabulate (1000000, fn i =>
                                       if i mod 10 = 0 then #"\n" else #"a")))
            val _ = closeOut out
            fun wc f =
               let
                  val ins = openIn f
                  fun loop (i: int): int =
                     case input1 ins of
                        NONE => i
                      | SOME c => loop (if c = #"\n" then i + 1 else i)
                  val n = loop 0
                  val _ = if n <> 100000 then raise Fail "bug" else ()
                  val _ = closeIn ins
               in n
               end
            val rec loop =
               fn 0 => ()
                | n => (wc f; loop (n - 1))
            val _ = loop n
            val _ = OS.FileSys.remove f
         in ()
         end
   end
