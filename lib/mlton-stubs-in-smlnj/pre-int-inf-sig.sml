structure Int =
   struct
      type int = Pervasive.Int32.int
   end
structure LargeInt =
   struct
      type int = Pervasive.IntInf.int
   end
structure Word =
   struct
      type word = Word32.word
   end
