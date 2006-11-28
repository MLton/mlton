signature CLIST =
   sig
      type t

      val cons: int * t -> t
      val sing: int -> t
      val sum: t -> int
   end

functor CList (structure F: MLTON_FINALIZABLE
               structure Prim:
                  sig
                     val cons: int * Word32.word -> Word32.word
                     val free: Word32.word -> unit
                     val sing: int -> Word32.word
                     val sum: Word32.word -> int
                  end): CLIST =
   struct
      type t = Word32.word F.t

      fun cons (n: int, l: t) =
         F.withValue
         (l, fn w' =>
          let
             val c = F.new (Prim.cons (n, w'))
             val _ = F.addFinalizer (c, Prim.free)
             val _ = F.finalizeBefore (c, l)
          in
             c
          end)

      fun sing n =
         let
            val c = F.new (Prim.sing n)
            val _ = F.addFinalizer (c, Prim.free)
         in
            c
         end

      fun sum c = F.withValue (c, Prim.sum)
   end

functor Test (structure CList: CLIST
              structure MLton: sig
                                  structure GC:
                                     sig
                                        val collect: unit -> unit
                                     end
                               end) =
   struct
      fun f n =
         if n = 1
            then ()
         else
            let
               val a = Array.tabulate (n, fn i => i)
               val _ = Array.sub (a, 0) + Array.sub (a, 1)
            in
               f (n - 1)
            end

      val l = CList.sing 2
      val l = CList.cons (2,l)
      val l = CList.cons (2,l)
      val l = CList.cons (2,l)
      val l = CList.cons (2,l)
      val l = CList.cons (2,l)
      val l = CList.cons (2,l)
      val _ = MLton.GC.collect ()
      val _ = f 100
      val _ = print (concat ["listSum(l) = ",
                             Int.toString (CList.sum l),
                             "\n"])
      val _ = MLton.GC.collect ()
      val _ = f 100
   end

structure CList =
   CList (structure F = MLton.Finalizable
          structure Prim =
             struct
                val cons = _import "listCons": int * Word32.word -> Word32.word;
                val free = _import "listFree": Word32.word -> unit;
                val sing = _import "listSing": int -> Word32.word;
                val sum = _import "listSum": Word32.word -> int;
             end)

structure S = Test (structure CList = CList
                    structure MLton = MLton)
