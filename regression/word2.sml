  
fun wc hi lo = Word32.orb (Word32.<< (hi, 0w16),  lo)

  val chr = fn c =>
      let in
          print ("chr " ^ (Int.toString c) ^ "\n");
          chr c
      end

  fun w2b w =
      let in
          print ("w2B " ^ (Word32.toString w) ^ "\n");

          map chr
          [Word32.toInt (Word32.andb( w                 , 0w255)),
           Word32.toInt (Word32.andb(Word32.>>(w, 0w8)  , 0w255)),
           Word32.toInt (Word32.andb(Word32.>>(w, 0w16) , 0w255)),
           Word32.toInt (Word32.andb(Word32.>>(w, 0w24) , 0w255))]

          before print "success\n"
      end

  val _ = w2b (wc 0wx6754 0wx2301)
