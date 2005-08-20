
structure Main =
struct
   open CML

   fun pong ch =
      let
         fun loop () =
            let
               val () = recv ch
            in
               loop ()
            end
         val _ = spawn (fn () => loop ())
      in
         ()
      end

   fun ping ch n =
      let
         fun loop i =
            if i > n then RunCML.shutdown OS.Process.success
               else let
                       val () = send (ch, ())
                    in
                       loop (i + 1)
                    end
         val _ = spawn (fn () => loop 0)
      in
         ()
      end

   fun doit n =
      RunCML.doit
      (fn () =>
       let
          val ch = channel ()
          val () = pong ch
          val () = ping ch n
       in
          ()
       end,
       SOME (Time.fromMilliseconds 10))
end
