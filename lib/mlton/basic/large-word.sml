structure LargeWord: WORD =
   struct
      open Pervasive.LargeWord

      structure Z = FixWord (Pervasive.LargeWord)
      open Z

      val equals: t * t -> bool = op =
   end
