structure BinIO =
   struct
      open OpenInt32 BinIO

      fun inputN (ins, n) = BinIO.inputN (ins, toInt n)
      fun canInput (ins, n) = BinIO.canInput (ins, toInt n)
   end
