functor F (P: PACK_WORD) =
   struct
      val v = Word8Vector.tabulate (11, Word8.fromInt)
         
      fun p i = print (concat [LargeWord.toString (P.subVec (v, i)), "\n"])

      val _ = (p 0; p 1)

      val _ =
         List.app
         (fn i => p i handle Subscript => print "OK\n")
         [~1, 2, valOf Int.maxInt]
   end

structure S = F (PackWord32Little)
structure S = F (PackWord32Big)

functor F (P: PACK_WORD) =
   struct
      val a = Word8Array.tabulate (11, Word8.fromInt)

      val _ = P.update (a, 0, 0wxFFFEFDFC)

      fun p i = print (concat [LargeWord.toString (P.subArr (a, i)), "\n"])

      val _ = (p 0; p 1)

      val _ =
         List.app
         (fn i => p i handle Subscript => print "OK\n")
         [~1, 2, valOf Int.maxInt]
   end

structure S = F (PackWord32Little)
structure S = F (PackWord32Big)
