
structure Main =
struct
   open CML

   val print = TextIO.print

   fun makeNatStream c =
      let
         val ch = channel ()
         fun count i = (send(ch, i)
                        ; count(i+1))
         val _ = spawn (fn () => 
                        (print (concat ["makeNatStream: ", 
                                        tidToString (getTid ()), 
                                        "\n"])
                         ; count c))
      in
         ch
      end

   fun makeFilter (p, inCh) =
      let
         val outCh = channel ()
         fun loop () =
            let
               val i = sync (recvEvt inCh)
            in
               if ((i mod p) <> 0) 
                  then sync (sendEvt (outCh, i))
                  else ()
               ; loop ()
            end
         val _ = spawn loop
      in
         outCh
      end

   fun makePrimes () =
      let
         val primes = channel ()
         fun head ch =
            let val p = recv ch
            in
               send(primes, p)
               ; head (makeFilter (p, ch))
            end
         val _ = spawn (fn () => 
                        (print (concat ["makePrimes: ", 
                                        tidToString (getTid ()), 
                                        "\n"])
                         ; head (makeNatStream 2)))
      in
         primes
      end

   fun makeNatPrinter ch n =
      let
         fun loop i =
            if i > n then RunCML.shutdown OS.Process.success
               else let
                       val m = recv ch
                       val m' = Int.toString m
                       fun loop' j =
                          if j > m then ()
                          else (print (m' ^ "\n")
                                ; loop' (j + 1))
                    in
                       loop' m
                       ; loop (i + 1)
                    end
         val _ = spawn (fn () => 
                        (print (concat ["makeNatPrinter: ", 
                                        tidToString (getTid ()), 
                                        "\n"])
                         ; loop 0))
      in
         ()
      end

   fun doit' n =
      RunCML.doit
      (fn () =>
       let
          val ch = makePrimes ()
          val _ = makeNatPrinter ch n
       in
          ()
       end,
       SOME (Time.fromMilliseconds 10))

   fun doit n =
      let
         val x = doit' n
      in
         x
      end
end
