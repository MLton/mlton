structure String =
   struct
      open String

      open OpenInt32
      val size = fromInt o size
      fun substring(s, i, j) = String.substring(s, toInt i, toInt j)
      val maxSize = fromInt maxSize
      fun sub(s, i) = String.sub(s, toInt i)
      fun extract(s, i, io) = String.extract(s, toInt i, toIntOpt io)
      val toCString = translate Char.toCString
   end
