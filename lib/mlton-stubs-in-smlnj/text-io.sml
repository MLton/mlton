structure TextIO =
   struct
      open OpenInt32 TextIO

      fun inputN(ins, n) = TextIO.inputN(ins, toInt n)
      fun canInput(ins, n) = TextIO.canInput(ins, toInt n)
   end
