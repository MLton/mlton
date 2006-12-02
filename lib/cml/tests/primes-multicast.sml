
structure Main =
struct
   open CML
   structure MC = Multicast

   val print = TextIO.print

   fun makeNatStream c =
      let
         val mch = MC.mChannel ()
         fun count i = (MC.multicast(mch, i)
                        ; count(i+1))
         val _ = spawn (fn () => 
                        (print (concat ["makeNatStream: ", 
                                        tidToString (getTid ()), 
                                        "\n"])
                         ; count c))
      in
         mch
      end

   fun makeFilter (p, inMCh) =
      let
         val inP = MC.port inMCh
         val outMCh = MC.mChannel ()
         fun loop () =
            let
               val i = sync (MC.recvEvt inP)
            in
               if ((i mod p) <> 0) 
                  then MC.multicast(outMCh, i)
                  else ()
               ; loop ()
            end
         val _ = spawn loop
      in
         outMCh
      end

   fun makePrimes () =
      let
         val primes = MC.mChannel ()
         fun head mch =
            let
               val p = MC.recv (MC.port mch)
            in
               MC.multicast(primes, p)
               ; head (makeFilter (p, mch))
            end
         val _ = spawn (fn () => 
                        (print (concat ["makePrimes: ", 
                                        tidToString (getTid ()), 
                                        "\n"])
                         ; head (makeNatStream 2)))
      in
         primes
      end

   fun makeNatPrinter mch n =
      let
         val p = MC.port mch
         fun loop i =
            if i > n then RunCML.shutdown OS.Process.success
               else let
                       val m = MC.recv p
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
          val mch = makePrimes ()
          val _ = makeNatPrinter mch n
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
