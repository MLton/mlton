open Primitive

val printInt8 = _import "printInt8" : Int8.int -> unit;
val printInt16 = _import "printInt16" : Int16.int -> unit;
val printInt32 = _import "printInt32" : Int32.int -> unit;
val printInt64 = _import "printInt64" : Int64.int -> unit;

val printWord8 = _import "printWord8" : Word8.word -> unit;
val printWord16 = _import "printWord16" : Word16.word -> unit;
val printWord32 = _import "printWord32" : Word32.word -> unit;
val printWord64 = _import "printWord64" : Word64.word -> unit;

fun printString s =
   PrimitiveFFI.Stdio.printStdout s

fun printIntInf i =
   let
      val s = IntInf.toString8 i
   in
      printString s
      ; printString "\n"
   end

local
   open IntInf
in
   fun fact n =
      if n = 0 then 1 else n * (fact (n - 1))
end

val _ = (printString "Int8.maxInt' = \n"
          ; printInt8 Int8.maxInt')
val _ = (printString "IntInf.fromInt8 Int8.maxInt' = \n"
         ; printIntInf (IntInf.fromInt8 Int8.maxInt'))
val _ = (printString "IntInf.toInt8 (IntInf.fromInt8 Int8.maxInt') = \n"
         ; printInt8 (IntInf.toInt8 (IntInf.fromInt8 Int8.maxInt')))
val _ = (printString "Int16.maxInt' = \n"
          ; printInt16 Int16.maxInt')
val _ = (printString "IntInf.fromInt16 Int16.maxInt' = \n"
         ; printIntInf (IntInf.fromInt16 Int16.maxInt'))
val _ = (printString "IntInf.toInt16 (IntInf.fromInt16 Int16.maxInt') = \n"
         ; printInt16 (IntInf.toInt16 (IntInf.fromInt16 Int16.maxInt')))
val _ = (printString "Int32.maxInt' = \n"
          ; printInt32 Int32.maxInt')
val _ = (printString "IntInf.fromInt32 Int32.maxInt' = \n"
         ; printIntInf (IntInf.fromInt32 Int32.maxInt'))
val _ = (printString "IntInf.toInt32 (IntInf.fromInt32 Int32.maxInt') = \n"
         ; printInt32 (IntInf.toInt32 (IntInf.fromInt32 Int32.maxInt')))
val _ = (printString "Int64.maxInt' = \n"
          ; printInt64 Int64.maxInt')
val _ = (printString "IntInf.fromInt64 Int64.maxInt' = \n"
         ; printIntInf (IntInf.fromInt64 Int64.maxInt'))
val _ = (printString "IntInf.toInt64 (IntInf.fromInt64 Int64.maxInt') = \n"
         ; printInt64 (IntInf.toInt64 (IntInf.fromInt64 Int64.maxInt')))

val _ = (printString "Int8.minInt' = \n"
          ; printInt8 Int8.minInt')
val _ = (printString "IntInf.fromInt8 Int8.minInt' = \n"
         ; printIntInf (IntInf.fromInt8 Int8.minInt'))
val _ = (printString "IntInf.toInt8 (IntInf.fromInt8 Int8.minInt') = \n"
         ; printInt8 (IntInf.toInt8 (IntInf.fromInt8 Int8.minInt')))
val _ = (printString "Int16.minInt' = \n"
          ; printInt16 Int16.minInt')
val _ = (printString "IntInf.fromInt16 Int16.minInt' = \n"
         ; printIntInf (IntInf.fromInt16 Int16.minInt'))
val _ = (printString "IntInf.toInt16 (IntInf.fromInt16 Int16.minInt') = \n"
         ; printInt16 (IntInf.toInt16 (IntInf.fromInt16 Int16.minInt')))
val _ = (printString "Int32.minInt' = \n"
          ; printInt32 Int32.minInt')
val _ = (printString "IntInf.fromInt32 Int32.minInt' = \n"
         ; printIntInf (IntInf.fromInt32 Int32.minInt'))
val _ = (printString "IntInf.toInt32 (IntInf.fromInt32 Int32.minInt') = \n"
         ; printInt32 (IntInf.toInt32 (IntInf.fromInt32 Int32.minInt')))
val _ = (printString "Int64.minInt' = \n"
          ; printInt64 Int64.minInt')
val _ = (printString "IntInf.fromInt64 Int64.minInt' = \n"
         ; printIntInf (IntInf.fromInt64 Int64.minInt'))
val _ = (printString "IntInf.toInt64 (IntInf.fromInt64 Int64.minInt') = \n"
         ; printInt64 (IntInf.toInt64 (IntInf.fromInt64 Int64.minInt')))

val _ = (printString "(Int8.div (Int8.minInt', 2)) = \n"
          ; printInt8 (Int8.div (Int8.minInt', 2)))
val _ = (printString "IntInf.fromInt8 (Int8.div (Int8.minInt', 2)) = \n"
         ; printIntInf (IntInf.fromInt8 (Int8.div (Int8.minInt', 2))))
val _ = (printString "IntInf.toInt8 (IntInf.fromInt8 (Int8.div (Int8.minInt', 2))) = \n"
         ; printInt8 (IntInf.toInt8 (IntInf.fromInt8 (Int8.div (Int8.minInt', 2)))))
val _ = (printString "(Int16.div (Int16.minInt', 2)) = \n"
          ; printInt16 (Int16.div (Int16.minInt', 2)))
val _ = (printString "IntInf.fromInt16 (Int16.div (Int16.minInt', 2)) = \n"
         ; printIntInf (IntInf.fromInt16 (Int16.div (Int16.minInt', 2))))
val _ = (printString "IntInf.toInt16 (IntInf.fromInt16 (Int16.div (Int16.minInt', 2))) = \n"
         ; printInt16 (IntInf.toInt16 (IntInf.fromInt16 (Int16.div (Int16.minInt', 2)))))
val _ = (printString "(Int32.div (Int32.minInt', 2)) = \n"
          ; printInt32 (Int32.div (Int32.minInt', 2)))
val _ = (printString "IntInf.fromInt32 (Int32.div (Int32.minInt', 2)) = \n"
         ; printIntInf (IntInf.fromInt32 (Int32.div (Int32.minInt', 2))))
val _ = (printString "IntInf.toInt32 (IntInf.fromInt32 (Int32.div (Int32.minInt', 2))) = \n"
         ; printInt32 (IntInf.toInt32 (IntInf.fromInt32 (Int32.div (Int32.minInt', 2)))))
val _ = (printString "(Int64.div (Int64.minInt', 2)) = \n"
          ; printInt64 (Int64.div (Int64.minInt', 2)))
val _ = (printString "IntInf.fromInt64 (Int64.div (Int64.minInt', 2)) = \n"
         ; printIntInf (IntInf.fromInt64 (Int64.div (Int64.minInt', 2))))
val _ = (printString "IntInf.toInt64 (IntInf.fromInt64 (Int64.div (Int64.minInt', 2))) = \n"
         ; printInt64 (IntInf.toInt64 (IntInf.fromInt64 (Int64.div (Int64.minInt', 2)))))

val _ = (printString "(Int8.div (Int8.minInt', 4)) = \n"
          ; printInt8 (Int8.div (Int8.minInt', 4)))
val _ = (printString "IntInf.fromInt8 (Int8.div (Int8.minInt', 4)) = \n"
         ; printIntInf (IntInf.fromInt8 (Int8.div (Int8.minInt', 4))))
val _ = (printString "IntInf.toInt8 (IntInf.fromInt8 (Int8.div (Int8.minInt', 4))) = \n"
         ; printInt8 (IntInf.toInt8 (IntInf.fromInt8 (Int8.div (Int8.minInt', 4)))))
val _ = (printString "(Int16.div (Int16.minInt', 4)) = \n"
          ; printInt16 (Int16.div (Int16.minInt', 4)))
val _ = (printString "IntInf.fromInt16 (Int16.div (Int16.minInt', 4)) = \n"
         ; printIntInf (IntInf.fromInt16 (Int16.div (Int16.minInt', 4))))
val _ = (printString "IntInf.toInt16 (IntInf.fromInt16 (Int16.div (Int16.minInt', 4))) = \n"
         ; printInt16 (IntInf.toInt16 (IntInf.fromInt16 (Int16.div (Int16.minInt', 4)))))
val _ = (printString "(Int32.div (Int32.minInt', 4)) = \n"
          ; printInt32 (Int32.div (Int32.minInt', 4)))
val _ = (printString "IntInf.fromInt32 (Int32.div (Int32.minInt', 4)) = \n"
         ; printIntInf (IntInf.fromInt32 (Int32.div (Int32.minInt', 4))))
val _ = (printString "IntInf.toInt32 (IntInf.fromInt32 (Int32.div (Int32.minInt', 4))) = \n"
         ; printInt32 (IntInf.toInt32 (IntInf.fromInt32 (Int32.div (Int32.minInt', 4)))))
val _ = (printString "(Int64.div (Int64.minInt', 4)) = \n"
          ; printInt64 (Int64.div (Int64.minInt', 4)))
val _ = (printString "IntInf.fromInt64 (Int64.div (Int64.minInt', 4)) = \n"
         ; printIntInf (IntInf.fromInt64 (Int64.div (Int64.minInt', 4))))
val _ = (printString "IntInf.toInt64 (IntInf.fromInt64 (Int64.div (Int64.minInt', 4))) = \n"
         ; printInt64 (IntInf.toInt64 (IntInf.fromInt64 (Int64.div (Int64.minInt', 4)))))

val _ = (printString "(Int8.- (Int8.maxInt', 2)) = \n"
          ; printInt8 (Int8.- (Int8.maxInt', 2)))
val _ = (printString "IntInf.fromInt8 (Int8.- (Int8.maxInt', 2)) = \n"
         ; printIntInf (IntInf.fromInt8 (Int8.- (Int8.maxInt', 2))))
val _ = (printString "IntInf.toInt8 (IntInf.fromInt8 (Int8.- (Int8.maxInt', 2))) = \n"
         ; printInt8 (IntInf.toInt8 (IntInf.fromInt8 (Int8.- (Int8.maxInt', 2)))))
val _ = (printString "(Int16.- (Int16.maxInt', 2)) = \n"
          ; printInt16 (Int16.- (Int16.maxInt', 2)))
val _ = (printString "IntInf.fromInt16 (Int16.- (Int16.maxInt', 2)) = \n"
         ; printIntInf (IntInf.fromInt16 (Int16.- (Int16.maxInt', 2))))
val _ = (printString "IntInf.toInt16 (IntInf.fromInt16 (Int16.- (Int16.maxInt', 2))) = \n"
         ; printInt16 (IntInf.toInt16 (IntInf.fromInt16 (Int16.- (Int16.maxInt', 2)))))
val _ = (printString "(Int32.- (Int32.maxInt', 2)) = \n"
          ; printInt32 (Int32.- (Int32.maxInt', 2)))
val _ = (printString "IntInf.fromInt32 (Int32.- (Int32.maxInt', 2)) = \n"
         ; printIntInf (IntInf.fromInt32 (Int32.- (Int32.maxInt', 2))))
val _ = (printString "IntInf.toInt32 (IntInf.fromInt32 (Int32.- (Int32.maxInt', 2))) = \n"
         ; printInt32 (IntInf.toInt32 (IntInf.fromInt32 (Int32.- (Int32.maxInt', 2)))))
val _ = (printString "(Int64.- (Int64.maxInt', 2)) = \n"
          ; printInt64 (Int64.- (Int64.maxInt', 2)))
val _ = (printString "IntInf.fromInt64 (Int64.- (Int64.maxInt', 2)) = \n"
         ; printIntInf (IntInf.fromInt64 (Int64.- (Int64.maxInt', 2))))
val _ = (printString "IntInf.toInt64 (IntInf.fromInt64 (Int64.- (Int64.maxInt', 2))) = \n"
         ; printInt64 (IntInf.toInt64 (IntInf.fromInt64 (Int64.- (Int64.maxInt', 2)))))

val _ = (printString "(Int8.- (Int8.maxInt', 4)) = \n"
          ; printInt8 (Int8.- (Int8.maxInt', 4)))
val _ = (printString "IntInf.fromInt8 (Int8.- (Int8.maxInt', 4)) = \n"
         ; printIntInf (IntInf.fromInt8 (Int8.- (Int8.maxInt', 4))))
val _ = (printString "IntInf.toInt8 (IntInf.fromInt8 (Int8.- (Int8.maxInt', 4))) = \n"
         ; printInt8 (IntInf.toInt8 (IntInf.fromInt8 (Int8.- (Int8.maxInt', 4)))))
val _ = (printString "(Int16.- (Int16.maxInt', 4)) = \n"
          ; printInt16 (Int16.- (Int16.maxInt', 4)))
val _ = (printString "IntInf.fromInt16 (Int16.- (Int16.maxInt', 4)) = \n"
         ; printIntInf (IntInf.fromInt16 (Int16.- (Int16.maxInt', 4))))
val _ = (printString "IntInf.toInt16 (IntInf.fromInt16 (Int16.- (Int16.maxInt', 4))) = \n"
         ; printInt16 (IntInf.toInt16 (IntInf.fromInt16 (Int16.- (Int16.maxInt', 4)))))
val _ = (printString "(Int32.- (Int32.maxInt', 4)) = \n"
          ; printInt32 (Int32.- (Int32.maxInt', 4)))
val _ = (printString "IntInf.fromInt32 (Int32.- (Int32.maxInt', 4)) = \n"
         ; printIntInf (IntInf.fromInt32 (Int32.- (Int32.maxInt', 4))))
val _ = (printString "IntInf.toInt32 (IntInf.fromInt32 (Int32.- (Int32.maxInt', 4))) = \n"
         ; printInt32 (IntInf.toInt32 (IntInf.fromInt32 (Int32.- (Int32.maxInt', 4)))))
val _ = (printString "(Int64.- (Int64.maxInt', 4)) = \n"
          ; printInt64 (Int64.- (Int64.maxInt', 4)))
val _ = (printString "IntInf.fromInt64 (Int64.- (Int64.maxInt', 4)) = \n"
         ; printIntInf (IntInf.fromInt64 (Int64.- (Int64.maxInt', 4))))
val _ = (printString "IntInf.toInt64 (IntInf.fromInt64 (Int64.- (Int64.maxInt', 4))) = \n"
         ; printInt64 (IntInf.toInt64 (IntInf.fromInt64 (Int64.- (Int64.maxInt', 4)))))

val _ = (printString "(Int8.+ (Int8.minInt', 2)) = \n"
          ; printInt8 (Int8.+ (Int8.minInt', 2)))
val _ = (printString "IntInf.fromInt8 (Int8.+ (Int8.minInt', 2)) = \n"
         ; printIntInf (IntInf.fromInt8 (Int8.+ (Int8.minInt', 2))))
val _ = (printString "IntInf.toInt8 (IntInf.fromInt8 (Int8.+ (Int8.minInt', 2))) = \n"
         ; printInt8 (IntInf.toInt8 (IntInf.fromInt8 (Int8.+ (Int8.minInt', 2)))))
val _ = (printString "(Int16.+ (Int16.minInt', 2)) = \n"
          ; printInt16 (Int16.+ (Int16.minInt', 2)))
val _ = (printString "IntInf.fromInt16 (Int16.+ (Int16.minInt', 2)) = \n"
         ; printIntInf (IntInf.fromInt16 (Int16.+ (Int16.minInt', 2))))
val _ = (printString "IntInf.toInt16 (IntInf.fromInt16 (Int16.+ (Int16.minInt', 2))) = \n"
         ; printInt16 (IntInf.toInt16 (IntInf.fromInt16 (Int16.+ (Int16.minInt', 2)))))
val _ = (printString "(Int32.+ (Int32.minInt', 2)) = \n"
          ; printInt32 (Int32.+ (Int32.minInt', 2)))
val _ = (printString "IntInf.fromInt32 (Int32.+ (Int32.minInt', 2)) = \n"
         ; printIntInf (IntInf.fromInt32 (Int32.+ (Int32.minInt', 2))))
val _ = (printString "IntInf.toInt32 (IntInf.fromInt32 (Int32.+ (Int32.minInt', 2))) = \n"
         ; printInt32 (IntInf.toInt32 (IntInf.fromInt32 (Int32.+ (Int32.minInt', 2)))))
val _ = (printString "(Int64.+ (Int64.minInt', 2)) = \n"
          ; printInt64 (Int64.+ (Int64.minInt', 2)))
val _ = (printString "IntInf.fromInt64 (Int64.+ (Int64.minInt', 2)) = \n"
         ; printIntInf (IntInf.fromInt64 (Int64.+ (Int64.minInt', 2))))
val _ = (printString "IntInf.toInt64 (IntInf.fromInt64 (Int64.+ (Int64.minInt', 2))) = \n"
         ; printInt64 (IntInf.toInt64 (IntInf.fromInt64 (Int64.+ (Int64.minInt', 2)))))

val _ = (printString "(Int8.+ (Int8.minInt', 4)) = \n"
          ; printInt8 (Int8.+ (Int8.minInt', 4)))
val _ = (printString "IntInf.fromInt8 (Int8.+ (Int8.minInt', 4)) = \n"
         ; printIntInf (IntInf.fromInt8 (Int8.+ (Int8.minInt', 4))))
val _ = (printString "IntInf.toInt8 (IntInf.fromInt8 (Int8.+ (Int8.minInt', 4))) = \n"
         ; printInt8 (IntInf.toInt8 (IntInf.fromInt8 (Int8.+ (Int8.minInt', 4)))))
val _ = (printString "(Int16.+ (Int16.minInt', 4)) = \n"
          ; printInt16 (Int16.+ (Int16.minInt', 4)))
val _ = (printString "IntInf.fromInt16 (Int16.+ (Int16.minInt', 4)) = \n"
         ; printIntInf (IntInf.fromInt16 (Int16.+ (Int16.minInt', 4))))
val _ = (printString "IntInf.toInt16 (IntInf.fromInt16 (Int16.+ (Int16.minInt', 4))) = \n"
         ; printInt16 (IntInf.toInt16 (IntInf.fromInt16 (Int16.+ (Int16.minInt', 4)))))
val _ = (printString "(Int32.+ (Int32.minInt', 4)) = \n"
          ; printInt32 (Int32.+ (Int32.minInt', 4)))
val _ = (printString "IntInf.fromInt32 (Int32.+ (Int32.minInt', 4)) = \n"
         ; printIntInf (IntInf.fromInt32 (Int32.+ (Int32.minInt', 4))))
val _ = (printString "IntInf.toInt32 (IntInf.fromInt32 (Int32.+ (Int32.minInt', 4))) = \n"
         ; printInt32 (IntInf.toInt32 (IntInf.fromInt32 (Int32.+ (Int32.minInt', 4)))))
val _ = (printString "(Int64.+ (Int64.minInt', 4)) = \n"
          ; printInt64 (Int64.+ (Int64.minInt', 4)))
val _ = (printString "IntInf.fromInt64 (Int64.+ (Int64.minInt', 4)) = \n"
         ; printIntInf (IntInf.fromInt64 (Int64.+ (Int64.minInt', 4))))
val _ = (printString "IntInf.toInt64 (IntInf.fromInt64 (Int64.+ (Int64.minInt', 4))) = \n"
         ; printInt64 (IntInf.toInt64 (IntInf.fromInt64 (Int64.+ (Int64.minInt', 4)))))

val _ = (printString "Int8.zero = \n"
          ; printInt8 Int8.zero)
val _ = (printString "IntInf.fromInt8 Int8.zero = \n"
         ; printIntInf (IntInf.fromInt8 Int8.zero))
val _ = (printString "IntInf.toInt8 (IntInf.fromInt8 Int8.zero) = \n"
         ; printInt8 (IntInf.toInt8 (IntInf.fromInt8 Int8.zero)))
val _ = (printString "Int16.zero = \n"
          ; printInt16 Int16.zero)
val _ = (printString "IntInf.fromInt16 Int16.zero = \n"
         ; printIntInf (IntInf.fromInt16 Int16.zero))
val _ = (printString "IntInf.toInt16 (IntInf.fromInt16 Int16.zero) = \n"
         ; printInt16 (IntInf.toInt16 (IntInf.fromInt16 Int16.zero)))
val _ = (printString "Int32.zero = \n"
          ; printInt32 Int32.zero)
val _ = (printString "IntInf.fromInt32 Int32.zero = \n"
         ; printIntInf (IntInf.fromInt32 Int32.zero))
val _ = (printString "IntInf.toInt32 (IntInf.fromInt32 Int32.zero) = \n"
         ; printInt32 (IntInf.toInt32 (IntInf.fromInt32 Int32.zero)))
val _ = (printString "Int64.zero = \n"
          ; printInt64 Int64.zero)
val _ = (printString "IntInf.fromInt64 Int64.zero = \n"
         ; printIntInf (IntInf.fromInt64 Int64.zero))
val _ = (printString "IntInf.toInt64 (IntInf.fromInt64 Int64.zero) = \n"
         ; printInt64 (IntInf.toInt64 (IntInf.fromInt64 Int64.zero)))

val _ = (printString "Int8.one = \n"
          ; printInt8 Int8.one)
val _ = (printString "IntInf.fromInt8 Int8.one = \n"
         ; printIntInf (IntInf.fromInt8 Int8.one))
val _ = (printString "IntInf.toInt8 (IntInf.fromInt8 Int8.one) = \n"
         ; printInt8 (IntInf.toInt8 (IntInf.fromInt8 Int8.one)))
val _ = (printString "Int16.one = \n"
          ; printInt16 Int16.one)
val _ = (printString "IntInf.fromInt16 Int16.one = \n"
         ; printIntInf (IntInf.fromInt16 Int16.one))
val _ = (printString "IntInf.toInt16 (IntInf.fromInt16 Int16.one) = \n"
         ; printInt16 (IntInf.toInt16 (IntInf.fromInt16 Int16.one)))
val _ = (printString "Int32.one = \n"
          ; printInt32 Int32.one)
val _ = (printString "IntInf.fromInt32 Int32.one = \n"
         ; printIntInf (IntInf.fromInt32 Int32.one))
val _ = (printString "IntInf.toInt32 (IntInf.fromInt32 Int32.one) = \n"
         ; printInt32 (IntInf.toInt32 (IntInf.fromInt32 Int32.one)))
val _ = (printString "Int64.one = \n"
          ; printInt64 Int64.one)
val _ = (printString "IntInf.fromInt64 Int64.one = \n"
         ; printIntInf (IntInf.fromInt64 Int64.one))
val _ = (printString "IntInf.toInt64 (IntInf.fromInt64 Int64.one) = \n"
         ; printInt64 (IntInf.toInt64 (IntInf.fromInt64 Int64.one)))

val _ = (printString "(Int8.~ Int8.one) = \n"
          ; printInt8 (Int8.~ Int8.one))
val _ = (printString "IntInf.fromInt8 (Int8.~ Int8.one) = \n"
         ; printIntInf (IntInf.fromInt8 (Int8.~ Int8.one)))
val _ = (printString "IntInf.toInt8 (IntInf.fromInt8 (Int8.~ Int8.one)) = \n"
         ; printInt8 (IntInf.toInt8 (IntInf.fromInt8 (Int8.~ Int8.one))))
val _ = (printString "(Int16.~ Int16.one) = \n"
          ; printInt16 (Int16.~ Int16.one))
val _ = (printString "IntInf.fromInt16 (Int16.~ Int16.one) = \n"
         ; printIntInf (IntInf.fromInt16 (Int16.~ Int16.one)))
val _ = (printString "IntInf.toInt16 (IntInf.fromInt16 (Int16.~ Int16.one)) = \n"
         ; printInt16 (IntInf.toInt16 (IntInf.fromInt16 (Int16.~ Int16.one))))
val _ = (printString "(Int32.~ Int32.one) = \n"
          ; printInt32 (Int32.~ Int32.one))
val _ = (printString "IntInf.fromInt32 (Int32.~ Int32.one) = \n"
         ; printIntInf (IntInf.fromInt32 (Int32.~ Int32.one)))
val _ = (printString "IntInf.toInt32 (IntInf.fromInt32 (Int32.~ Int32.one)) = \n"
         ; printInt32 (IntInf.toInt32 (IntInf.fromInt32 (Int32.~ Int32.one))))
val _ = (printString "(Int64.~ Int64.one) = \n"
          ; printInt64 (Int64.~ Int64.one))
val _ = (printString "IntInf.fromInt64 (Int64.~ Int64.one) = \n"
         ; printIntInf (IntInf.fromInt64 (Int64.~ Int64.one)))
val _ = (printString "IntInf.toInt64 (IntInf.fromInt64 (Int64.~ Int64.one)) = \n"
         ; printInt64 (IntInf.toInt64 (IntInf.fromInt64 (Int64.~ Int64.one))))

val _ = (printString "IntInf.toWord8 0 = \n"
         ; printWord8 (IntInf.toWord8 0))
val _ = (printString "IntInf.toWord16 0 = \n"
         ; printWord16 (IntInf.toWord16 0))
val _ = (printString "IntInf.toWord32 0 = \n"
         ; printWord32 (IntInf.toWord32 0))
val _ = (printString "IntInf.toWord64 0 = \n"
         ; printWord64 (IntInf.toWord64 0))

val _ = (printString "IntInf.toWord8 1 = \n"
         ; printWord8 (IntInf.toWord8 1))
val _ = (printString "IntInf.toWord16 1 = \n"
         ; printWord16 (IntInf.toWord16 1))
val _ = (printString "IntInf.toWord32 1 = \n"
         ; printWord32 (IntInf.toWord32 1))
val _ = (printString "IntInf.toWord64 1 = \n"
         ; printWord64 (IntInf.toWord64 1))

val _ = (printString "IntInf.toWord8 ~1 = \n"
         ; printWord8 (IntInf.toWord8 ~1))
val _ = (printString "IntInf.toWord16 ~1 = \n"
         ; printWord16 (IntInf.toWord16 ~1))
val _ = (printString "IntInf.toWord32 ~1 = \n"
         ; printWord32 (IntInf.toWord32 ~1))
val _ = (printString "IntInf.toWord64 ~1 = \n"
         ; printWord64 (IntInf.toWord64 ~1))

val _ = (printString "IntInf.toWord8 (IntInf.fromInt8 Int8.minInt') = \n"
         ; printWord8 (IntInf.toWord8 (IntInf.fromInt8 Int8.minInt')))
val _ = (printString "IntInf.toWord16 (IntInf.fromInt8 Int8.minInt') = \n"
         ; printWord16 (IntInf.toWord16 (IntInf.fromInt8 Int8.minInt')))
val _ = (printString "IntInf.toWord32 (IntInf.fromInt8 Int8.minInt') = \n"
         ; printWord32 (IntInf.toWord32 (IntInf.fromInt8 Int8.minInt')))
val _ = (printString "IntInf.toWord64 (IntInf.fromInt8 Int8.minInt') = \n"
         ; printWord64 (IntInf.toWord64 (IntInf.fromInt8 Int8.minInt')))

val _ = (printString "IntInf.toWord8 (IntInf.fromInt16 Int16.minInt') = \n"
         ; printWord8 (IntInf.toWord8 (IntInf.fromInt16 Int16.minInt')))
val _ = (printString "IntInf.toWord16 (IntInf.fromInt16 Int16.minInt') = \n"
         ; printWord16 (IntInf.toWord16 (IntInf.fromInt16 Int16.minInt')))
val _ = (printString "IntInf.toWord32 (IntInf.fromInt16 Int16.minInt') = \n"
         ; printWord32 (IntInf.toWord32 (IntInf.fromInt16 Int16.minInt')))
val _ = (printString "IntInf.toWord64 (IntInf.fromInt16 Int16.minInt') = \n"
         ; printWord64 (IntInf.toWord64 (IntInf.fromInt16 Int16.minInt')))

val _ = (printString "IntInf.toWord8 (IntInf.fromInt32 Int32.minInt') = \n"
         ; printWord8 (IntInf.toWord8 (IntInf.fromInt32 Int32.minInt')))
val _ = (printString "IntInf.toWord16 (IntInf.fromInt32 Int32.minInt') = \n"
         ; printWord16 (IntInf.toWord16 (IntInf.fromInt32 Int32.minInt')))
val _ = (printString "IntInf.toWord32 (IntInf.fromInt32 Int32.minInt') = \n"
         ; printWord32 (IntInf.toWord32 (IntInf.fromInt32 Int32.minInt')))
val _ = (printString "IntInf.toWord64 (IntInf.fromInt32 Int32.minInt') = \n"
         ; printWord64 (IntInf.toWord64 (IntInf.fromInt32 Int32.minInt')))

val _ = (printString "IntInf.toWord8 (IntInf.fromInt64 Int64.minInt') = \n"
         ; printWord8 (IntInf.toWord8 (IntInf.fromInt64 Int64.minInt')))
val _ = (printString "IntInf.toWord16 (IntInf.fromInt64 Int64.minInt') = \n"
         ; printWord16 (IntInf.toWord16 (IntInf.fromInt64 Int64.minInt')))
val _ = (printString "IntInf.toWord32 (IntInf.fromInt64 Int64.minInt') = \n"
         ; printWord32 (IntInf.toWord32 (IntInf.fromInt64 Int64.minInt')))
val _ = (printString "IntInf.toWord64 (IntInf.fromInt64 Int64.minInt') = \n"
         ; printWord64 (IntInf.toWord64 (IntInf.fromInt64 Int64.minInt')))

val _ = (printString "IntInf.toWord8 (IntInf.fromInt8 Int8.maxInt') = \n"
         ; printWord8 (IntInf.toWord8 (IntInf.fromInt8 Int8.maxInt')))
val _ = (printString "IntInf.toWord16 (IntInf.fromInt8 Int8.maxInt') = \n"
         ; printWord16 (IntInf.toWord16 (IntInf.fromInt8 Int8.maxInt')))
val _ = (printString "IntInf.toWord32 (IntInf.fromInt8 Int8.maxInt') = \n"
         ; printWord32 (IntInf.toWord32 (IntInf.fromInt8 Int8.maxInt')))
val _ = (printString "IntInf.toWord64 (IntInf.fromInt8 Int8.maxInt') = \n"
         ; printWord64 (IntInf.toWord64 (IntInf.fromInt8 Int8.maxInt')))

val _ = (printString "IntInf.toWord8 (IntInf.fromInt16 Int16.maxInt') = \n"
         ; printWord8 (IntInf.toWord8 (IntInf.fromInt16 Int16.maxInt')))
val _ = (printString "IntInf.toWord16 (IntInf.fromInt16 Int16.maxInt') = \n"
         ; printWord16 (IntInf.toWord16 (IntInf.fromInt16 Int16.maxInt')))
val _ = (printString "IntInf.toWord32 (IntInf.fromInt16 Int16.maxInt') = \n"
         ; printWord32 (IntInf.toWord32 (IntInf.fromInt16 Int16.maxInt')))
val _ = (printString "IntInf.toWord64 (IntInf.fromInt16 Int16.maxInt') = \n"
         ; printWord64 (IntInf.toWord64 (IntInf.fromInt16 Int16.maxInt')))

val _ = (printString "IntInf.toWord8 (IntInf.fromInt32 Int32.maxInt') = \n"
         ; printWord8 (IntInf.toWord8 (IntInf.fromInt32 Int32.maxInt')))
val _ = (printString "IntInf.toWord16 (IntInf.fromInt32 Int32.maxInt') = \n"
         ; printWord16 (IntInf.toWord16 (IntInf.fromInt32 Int32.maxInt')))
val _ = (printString "IntInf.toWord32 (IntInf.fromInt32 Int32.maxInt') = \n"
         ; printWord32 (IntInf.toWord32 (IntInf.fromInt32 Int32.maxInt')))
val _ = (printString "IntInf.toWord64 (IntInf.fromInt32 Int32.maxInt') = \n"
         ; printWord64 (IntInf.toWord64 (IntInf.fromInt32 Int32.maxInt')))

val _ = (printString "IntInf.toWord8 (IntInf.fromInt64 Int64.maxInt') = \n"
         ; printWord8 (IntInf.toWord8 (IntInf.fromInt64 Int64.maxInt')))
val _ = (printString "IntInf.toWord16 (IntInf.fromInt64 Int64.maxInt') = \n"
         ; printWord16 (IntInf.toWord16 (IntInf.fromInt64 Int64.maxInt')))
val _ = (printString "IntInf.toWord32 (IntInf.fromInt64 Int64.maxInt') = \n"
         ; printWord32 (IntInf.toWord32 (IntInf.fromInt64 Int64.maxInt')))
val _ = (printString "IntInf.toWord64 (IntInf.fromInt64 Int64.maxInt') = \n"
         ; printWord64 (IntInf.toWord64 (IntInf.fromInt64 Int64.maxInt')))

