
fun printString s =
   PrimitiveFFI.Stdio.printStdout s

fun printIntInf i =
   let
      val s = IntInf.toString i
   in
      printString s
      ; printString "\n"
   end

fun printInt8 i = 
   let
      val s = Int8.toString i
   in
      printString s
      ; printString "\n"
   end
fun printInt16 i = 
   let
      val s = Int16.toString i
   in
      printString s
      ; printString "\n"
   end
fun printInt32 i = 
   let
      val s = Int32.toString i
   in
      printString s
      ; printString "\n"
   end
fun printInt64 i = 
   let
      val s = Int64.toString i
   in
      printString s
      ; printString "\n"
   end

fun printWord8 w = 
   let
      val s = Word8.toString w
   in
      printString s
      ; printString "\n"
   end
fun printWord16 w = 
   let
      val s = Word16.toString w
   in
      printString s
      ; printString "\n"
   end
fun printWord32 w = 
   let
      val s = Word32.toString w
   in
      printString s
      ; printString "\n"
   end
fun printWord64 w = 
   let
      val s = Word64.toString w
   in
      printString s
      ; printString "\n"
   end



structure Int8 = struct
   open Int8
   val zero : int = 0
   val one : int = 1
end
structure Int16 = struct
   open Int16
   val zero : int = 0
   val one : int = 1
end
structure Int32 = struct
   open Int32
   val zero : int = 0
   val one : int = 1
end
structure Int64 = struct
   open Int64
   val zero : int = 0
   val one : int = 1
end

local
   open IntInf
in
   fun fact n =
      if n = 0 then 1 else n * (fact (n - 1))
end

val _ = (printString "Int8.maxInt' = \n"
          ; printInt8 Int8.maxInt')
val _ = (printString "Int8.toLarge Int8.maxInt' = \n"
         ; printIntInf (Int8.toLarge Int8.maxInt'))
val _ = (printString "Int8.fromLarge (Int8.toLarge Int8.maxInt') = \n"
         ; printInt8 (Int8.fromLarge (Int8.toLarge Int8.maxInt')))
val _ = (printString "Int16.maxInt' = \n"
          ; printInt16 Int16.maxInt')
val _ = (printString "Int16.toLarge Int16.maxInt' = \n"
         ; printIntInf (Int16.toLarge Int16.maxInt'))
val _ = (printString "Int16.fromLarge (Int16.toLarge Int16.maxInt') = \n"
         ; printInt16 (Int16.fromLarge (Int16.toLarge Int16.maxInt')))
val _ = (printString "Int32.maxInt' = \n"
          ; printInt32 Int32.maxInt')
val _ = (printString "Int32.toLarge Int32.maxInt' = \n"
         ; printIntInf (Int32.toLarge Int32.maxInt'))
val _ = (printString "Int32.fromLarge (Int32.toLarge Int32.maxInt') = \n"
         ; printInt32 (Int32.fromLarge (Int32.toLarge Int32.maxInt')))
val _ = (printString "Int64.maxInt' = \n"
          ; printInt64 Int64.maxInt')
val _ = (printString "Int64.toLarge Int64.maxInt' = \n"
         ; printIntInf (Int64.toLarge Int64.maxInt'))
val _ = (printString "Int64.fromLarge (Int64.toLarge Int64.maxInt') = \n"
         ; printInt64 (Int64.fromLarge (Int64.toLarge Int64.maxInt')))

val _ = (printString "Int8.minInt' = \n"
          ; printInt8 Int8.minInt')
val _ = (printString "Int8.toLarge Int8.minInt' = \n"
         ; printIntInf (Int8.toLarge Int8.minInt'))
val _ = (printString "Int8.fromLarge (Int8.toLarge Int8.minInt') = \n"
         ; printInt8 (Int8.fromLarge (Int8.toLarge Int8.minInt')))
val _ = (printString "Int16.minInt' = \n"
          ; printInt16 Int16.minInt')
val _ = (printString "Int16.toLarge Int16.minInt' = \n"
         ; printIntInf (Int16.toLarge Int16.minInt'))
val _ = (printString "Int16.fromLarge (Int16.toLarge Int16.minInt') = \n"
         ; printInt16 (Int16.fromLarge (Int16.toLarge Int16.minInt')))
val _ = (printString "Int32.minInt' = \n"
          ; printInt32 Int32.minInt')
val _ = (printString "Int32.toLarge Int32.minInt' = \n"
         ; printIntInf (Int32.toLarge Int32.minInt'))
val _ = (printString "Int32.fromLarge (Int32.toLarge Int32.minInt') = \n"
         ; printInt32 (Int32.fromLarge (Int32.toLarge Int32.minInt')))
val _ = (printString "Int64.minInt' = \n"
          ; printInt64 Int64.minInt')
val _ = (printString "Int64.toLarge Int64.minInt' = \n"
         ; printIntInf (Int64.toLarge Int64.minInt'))
val _ = (printString "Int64.fromLarge (Int64.toLarge Int64.minInt') = \n"
         ; printInt64 (Int64.fromLarge (Int64.toLarge Int64.minInt')))

val _ = (printString "(Int8.div (Int8.minInt', 2)) = \n"
          ; printInt8 (Int8.div (Int8.minInt', 2)))
val _ = (printString "Int8.toLarge (Int8.div (Int8.minInt', 2)) = \n"
         ; printIntInf (Int8.toLarge (Int8.div (Int8.minInt', 2))))
val _ = (printString "Int8.fromLarge (Int8.toLarge (Int8.div (Int8.minInt', 2))) = \n"
         ; printInt8 (Int8.fromLarge (Int8.toLarge (Int8.div (Int8.minInt', 2)))))
val _ = (printString "(Int16.div (Int16.minInt', 2)) = \n"
          ; printInt16 (Int16.div (Int16.minInt', 2)))
val _ = (printString "Int16.toLarge (Int16.div (Int16.minInt', 2)) = \n"
         ; printIntInf (Int16.toLarge (Int16.div (Int16.minInt', 2))))
val _ = (printString "Int16.fromLarge (Int16.toLarge (Int16.div (Int16.minInt', 2))) = \n"
         ; printInt16 (Int16.fromLarge (Int16.toLarge (Int16.div (Int16.minInt', 2)))))
val _ = (printString "(Int32.div (Int32.minInt', 2)) = \n"
          ; printInt32 (Int32.div (Int32.minInt', 2)))
val _ = (printString "Int32.toLarge (Int32.div (Int32.minInt', 2)) = \n"
         ; printIntInf (Int32.toLarge (Int32.div (Int32.minInt', 2))))
val _ = (printString "Int32.fromLarge (Int32.toLarge (Int32.div (Int32.minInt', 2))) = \n"
         ; printInt32 (Int32.fromLarge (Int32.toLarge (Int32.div (Int32.minInt', 2)))))
val _ = (printString "(Int64.div (Int64.minInt', 2)) = \n"
          ; printInt64 (Int64.div (Int64.minInt', 2)))
val _ = (printString "Int64.toLarge (Int64.div (Int64.minInt', 2)) = \n"
         ; printIntInf (Int64.toLarge (Int64.div (Int64.minInt', 2))))
val _ = (printString "Int64.fromLarge (Int64.toLarge (Int64.div (Int64.minInt', 2))) = \n"
         ; printInt64 (Int64.fromLarge (Int64.toLarge (Int64.div (Int64.minInt', 2)))))

val _ = (printString "(Int8.div (Int8.minInt', 4)) = \n"
          ; printInt8 (Int8.div (Int8.minInt', 4)))
val _ = (printString "Int8.toLarge (Int8.div (Int8.minInt', 4)) = \n"
         ; printIntInf (Int8.toLarge (Int8.div (Int8.minInt', 4))))
val _ = (printString "Int8.fromLarge (Int8.toLarge (Int8.div (Int8.minInt', 4))) = \n"
         ; printInt8 (Int8.fromLarge (Int8.toLarge (Int8.div (Int8.minInt', 4)))))
val _ = (printString "(Int16.div (Int16.minInt', 4)) = \n"
          ; printInt16 (Int16.div (Int16.minInt', 4)))
val _ = (printString "Int16.toLarge (Int16.div (Int16.minInt', 4)) = \n"
         ; printIntInf (Int16.toLarge (Int16.div (Int16.minInt', 4))))
val _ = (printString "Int16.fromLarge (Int16.toLarge (Int16.div (Int16.minInt', 4))) = \n"
         ; printInt16 (Int16.fromLarge (Int16.toLarge (Int16.div (Int16.minInt', 4)))))
val _ = (printString "(Int32.div (Int32.minInt', 4)) = \n"
          ; printInt32 (Int32.div (Int32.minInt', 4)))
val _ = (printString "Int32.toLarge (Int32.div (Int32.minInt', 4)) = \n"
         ; printIntInf (Int32.toLarge (Int32.div (Int32.minInt', 4))))
val _ = (printString "Int32.fromLarge (Int32.toLarge (Int32.div (Int32.minInt', 4))) = \n"
         ; printInt32 (Int32.fromLarge (Int32.toLarge (Int32.div (Int32.minInt', 4)))))
val _ = (printString "(Int64.div (Int64.minInt', 4)) = \n"
          ; printInt64 (Int64.div (Int64.minInt', 4)))
val _ = (printString "Int64.toLarge (Int64.div (Int64.minInt', 4)) = \n"
         ; printIntInf (Int64.toLarge (Int64.div (Int64.minInt', 4))))
val _ = (printString "Int64.fromLarge (Int64.toLarge (Int64.div (Int64.minInt', 4))) = \n"
         ; printInt64 (Int64.fromLarge (Int64.toLarge (Int64.div (Int64.minInt', 4)))))

val _ = (printString "(Int8.- (Int8.maxInt', 2)) = \n"
          ; printInt8 (Int8.- (Int8.maxInt', 2)))
val _ = (printString "Int8.toLarge (Int8.- (Int8.maxInt', 2)) = \n"
         ; printIntInf (Int8.toLarge (Int8.- (Int8.maxInt', 2))))
val _ = (printString "Int8.fromLarge (Int8.toLarge (Int8.- (Int8.maxInt', 2))) = \n"
         ; printInt8 (Int8.fromLarge (Int8.toLarge (Int8.- (Int8.maxInt', 2)))))
val _ = (printString "(Int16.- (Int16.maxInt', 2)) = \n"
          ; printInt16 (Int16.- (Int16.maxInt', 2)))
val _ = (printString "Int16.toLarge (Int16.- (Int16.maxInt', 2)) = \n"
         ; printIntInf (Int16.toLarge (Int16.- (Int16.maxInt', 2))))
val _ = (printString "Int16.fromLarge (Int16.toLarge (Int16.- (Int16.maxInt', 2))) = \n"
         ; printInt16 (Int16.fromLarge (Int16.toLarge (Int16.- (Int16.maxInt', 2)))))
val _ = (printString "(Int32.- (Int32.maxInt', 2)) = \n"
          ; printInt32 (Int32.- (Int32.maxInt', 2)))
val _ = (printString "Int32.toLarge (Int32.- (Int32.maxInt', 2)) = \n"
         ; printIntInf (Int32.toLarge (Int32.- (Int32.maxInt', 2))))
val _ = (printString "Int32.fromLarge (Int32.toLarge (Int32.- (Int32.maxInt', 2))) = \n"
         ; printInt32 (Int32.fromLarge (Int32.toLarge (Int32.- (Int32.maxInt', 2)))))
val _ = (printString "(Int64.- (Int64.maxInt', 2)) = \n"
          ; printInt64 (Int64.- (Int64.maxInt', 2)))
val _ = (printString "Int64.toLarge (Int64.- (Int64.maxInt', 2)) = \n"
         ; printIntInf (Int64.toLarge (Int64.- (Int64.maxInt', 2))))
val _ = (printString "Int64.fromLarge (Int64.toLarge (Int64.- (Int64.maxInt', 2))) = \n"
         ; printInt64 (Int64.fromLarge (Int64.toLarge (Int64.- (Int64.maxInt', 2)))))

val _ = (printString "(Int8.- (Int8.maxInt', 4)) = \n"
          ; printInt8 (Int8.- (Int8.maxInt', 4)))
val _ = (printString "Int8.toLarge (Int8.- (Int8.maxInt', 4)) = \n"
         ; printIntInf (Int8.toLarge (Int8.- (Int8.maxInt', 4))))
val _ = (printString "Int8.fromLarge (Int8.toLarge (Int8.- (Int8.maxInt', 4))) = \n"
         ; printInt8 (Int8.fromLarge (Int8.toLarge (Int8.- (Int8.maxInt', 4)))))
val _ = (printString "(Int16.- (Int16.maxInt', 4)) = \n"
          ; printInt16 (Int16.- (Int16.maxInt', 4)))
val _ = (printString "Int16.toLarge (Int16.- (Int16.maxInt', 4)) = \n"
         ; printIntInf (Int16.toLarge (Int16.- (Int16.maxInt', 4))))
val _ = (printString "Int16.fromLarge (Int16.toLarge (Int16.- (Int16.maxInt', 4))) = \n"
         ; printInt16 (Int16.fromLarge (Int16.toLarge (Int16.- (Int16.maxInt', 4)))))
val _ = (printString "(Int32.- (Int32.maxInt', 4)) = \n"
          ; printInt32 (Int32.- (Int32.maxInt', 4)))
val _ = (printString "Int32.toLarge (Int32.- (Int32.maxInt', 4)) = \n"
         ; printIntInf (Int32.toLarge (Int32.- (Int32.maxInt', 4))))
val _ = (printString "Int32.fromLarge (Int32.toLarge (Int32.- (Int32.maxInt', 4))) = \n"
         ; printInt32 (Int32.fromLarge (Int32.toLarge (Int32.- (Int32.maxInt', 4)))))
val _ = (printString "(Int64.- (Int64.maxInt', 4)) = \n"
          ; printInt64 (Int64.- (Int64.maxInt', 4)))
val _ = (printString "Int64.toLarge (Int64.- (Int64.maxInt', 4)) = \n"
         ; printIntInf (Int64.toLarge (Int64.- (Int64.maxInt', 4))))
val _ = (printString "Int64.fromLarge (Int64.toLarge (Int64.- (Int64.maxInt', 4))) = \n"
         ; printInt64 (Int64.fromLarge (Int64.toLarge (Int64.- (Int64.maxInt', 4)))))

val _ = (printString "(Int8.+ (Int8.minInt', 2)) = \n"
          ; printInt8 (Int8.+ (Int8.minInt', 2)))
val _ = (printString "Int8.toLarge (Int8.+ (Int8.minInt', 2)) = \n"
         ; printIntInf (Int8.toLarge (Int8.+ (Int8.minInt', 2))))
val _ = (printString "Int8.fromLarge (Int8.toLarge (Int8.+ (Int8.minInt', 2))) = \n"
         ; printInt8 (Int8.fromLarge (Int8.toLarge (Int8.+ (Int8.minInt', 2)))))
val _ = (printString "(Int16.+ (Int16.minInt', 2)) = \n"
          ; printInt16 (Int16.+ (Int16.minInt', 2)))
val _ = (printString "Int16.toLarge (Int16.+ (Int16.minInt', 2)) = \n"
         ; printIntInf (Int16.toLarge (Int16.+ (Int16.minInt', 2))))
val _ = (printString "Int16.fromLarge (Int16.toLarge (Int16.+ (Int16.minInt', 2))) = \n"
         ; printInt16 (Int16.fromLarge (Int16.toLarge (Int16.+ (Int16.minInt', 2)))))
val _ = (printString "(Int32.+ (Int32.minInt', 2)) = \n"
          ; printInt32 (Int32.+ (Int32.minInt', 2)))
val _ = (printString "Int32.toLarge (Int32.+ (Int32.minInt', 2)) = \n"
         ; printIntInf (Int32.toLarge (Int32.+ (Int32.minInt', 2))))
val _ = (printString "Int32.fromLarge (Int32.toLarge (Int32.+ (Int32.minInt', 2))) = \n"
         ; printInt32 (Int32.fromLarge (Int32.toLarge (Int32.+ (Int32.minInt', 2)))))
val _ = (printString "(Int64.+ (Int64.minInt', 2)) = \n"
          ; printInt64 (Int64.+ (Int64.minInt', 2)))
val _ = (printString "Int64.toLarge (Int64.+ (Int64.minInt', 2)) = \n"
         ; printIntInf (Int64.toLarge (Int64.+ (Int64.minInt', 2))))
val _ = (printString "Int64.fromLarge (Int64.toLarge (Int64.+ (Int64.minInt', 2))) = \n"
         ; printInt64 (Int64.fromLarge (Int64.toLarge (Int64.+ (Int64.minInt', 2)))))

val _ = (printString "(Int8.+ (Int8.minInt', 4)) = \n"
          ; printInt8 (Int8.+ (Int8.minInt', 4)))
val _ = (printString "Int8.toLarge (Int8.+ (Int8.minInt', 4)) = \n"
         ; printIntInf (Int8.toLarge (Int8.+ (Int8.minInt', 4))))
val _ = (printString "Int8.fromLarge (Int8.toLarge (Int8.+ (Int8.minInt', 4))) = \n"
         ; printInt8 (Int8.fromLarge (Int8.toLarge (Int8.+ (Int8.minInt', 4)))))
val _ = (printString "(Int16.+ (Int16.minInt', 4)) = \n"
          ; printInt16 (Int16.+ (Int16.minInt', 4)))
val _ = (printString "Int16.toLarge (Int16.+ (Int16.minInt', 4)) = \n"
         ; printIntInf (Int16.toLarge (Int16.+ (Int16.minInt', 4))))
val _ = (printString "Int16.fromLarge (Int16.toLarge (Int16.+ (Int16.minInt', 4))) = \n"
         ; printInt16 (Int16.fromLarge (Int16.toLarge (Int16.+ (Int16.minInt', 4)))))
val _ = (printString "(Int32.+ (Int32.minInt', 4)) = \n"
          ; printInt32 (Int32.+ (Int32.minInt', 4)))
val _ = (printString "Int32.toLarge (Int32.+ (Int32.minInt', 4)) = \n"
         ; printIntInf (Int32.toLarge (Int32.+ (Int32.minInt', 4))))
val _ = (printString "Int32.fromLarge (Int32.toLarge (Int32.+ (Int32.minInt', 4))) = \n"
         ; printInt32 (Int32.fromLarge (Int32.toLarge (Int32.+ (Int32.minInt', 4)))))
val _ = (printString "(Int64.+ (Int64.minInt', 4)) = \n"
          ; printInt64 (Int64.+ (Int64.minInt', 4)))
val _ = (printString "Int64.toLarge (Int64.+ (Int64.minInt', 4)) = \n"
         ; printIntInf (Int64.toLarge (Int64.+ (Int64.minInt', 4))))
val _ = (printString "Int64.fromLarge (Int64.toLarge (Int64.+ (Int64.minInt', 4))) = \n"
         ; printInt64 (Int64.fromLarge (Int64.toLarge (Int64.+ (Int64.minInt', 4)))))

val _ = (printString "Int8.zero = \n"
          ; printInt8 Int8.zero)
val _ = (printString "Int8.toLarge Int8.zero = \n"
         ; printIntInf (Int8.toLarge Int8.zero))
val _ = (printString "Int8.fromLarge (Int8.toLarge Int8.zero) = \n"
         ; printInt8 (Int8.fromLarge (Int8.toLarge Int8.zero)))
val _ = (printString "Int16.zero = \n"
          ; printInt16 Int16.zero)
val _ = (printString "Int16.toLarge Int16.zero = \n"
         ; printIntInf (Int16.toLarge Int16.zero))
val _ = (printString "Int16.fromLarge (Int16.toLarge Int16.zero) = \n"
         ; printInt16 (Int16.fromLarge (Int16.toLarge Int16.zero)))
val _ = (printString "Int32.zero = \n"
          ; printInt32 Int32.zero)
val _ = (printString "Int32.toLarge Int32.zero = \n"
         ; printIntInf (Int32.toLarge Int32.zero))
val _ = (printString "Int32.fromLarge (Int32.toLarge Int32.zero) = \n"
         ; printInt32 (Int32.fromLarge (Int32.toLarge Int32.zero)))
val _ = (printString "Int64.zero = \n"
          ; printInt64 Int64.zero)
val _ = (printString "Int64.toLarge Int64.zero = \n"
         ; printIntInf (Int64.toLarge Int64.zero))
val _ = (printString "Int64.fromLarge (Int64.toLarge Int64.zero) = \n"
         ; printInt64 (Int64.fromLarge (Int64.toLarge Int64.zero)))

val _ = (printString "Int8.one = \n"
          ; printInt8 Int8.one)
val _ = (printString "Int8.toLarge Int8.one = \n"
         ; printIntInf (Int8.toLarge Int8.one))
val _ = (printString "Int8.fromLarge (Int8.toLarge Int8.one) = \n"
         ; printInt8 (Int8.fromLarge (Int8.toLarge Int8.one)))
val _ = (printString "Int16.one = \n"
          ; printInt16 Int16.one)
val _ = (printString "Int16.toLarge Int16.one = \n"
         ; printIntInf (Int16.toLarge Int16.one))
val _ = (printString "Int16.fromLarge (Int16.toLarge Int16.one) = \n"
         ; printInt16 (Int16.fromLarge (Int16.toLarge Int16.one)))
val _ = (printString "Int32.one = \n"
          ; printInt32 Int32.one)
val _ = (printString "Int32.toLarge Int32.one = \n"
         ; printIntInf (Int32.toLarge Int32.one))
val _ = (printString "Int32.fromLarge (Int32.toLarge Int32.one) = \n"
         ; printInt32 (Int32.fromLarge (Int32.toLarge Int32.one)))
val _ = (printString "Int64.one = \n"
          ; printInt64 Int64.one)
val _ = (printString "Int64.toLarge Int64.one = \n"
         ; printIntInf (Int64.toLarge Int64.one))
val _ = (printString "Int64.fromLarge (Int64.toLarge Int64.one) = \n"
         ; printInt64 (Int64.fromLarge (Int64.toLarge Int64.one)))

val _ = (printString "(Int8.~ Int8.one) = \n"
          ; printInt8 (Int8.~ Int8.one))
val _ = (printString "Int8.toLarge (Int8.~ Int8.one) = \n"
         ; printIntInf (Int8.toLarge (Int8.~ Int8.one)))
val _ = (printString "Int8.fromLarge (Int8.toLarge (Int8.~ Int8.one)) = \n"
         ; printInt8 (Int8.fromLarge (Int8.toLarge (Int8.~ Int8.one))))
val _ = (printString "(Int16.~ Int16.one) = \n"
          ; printInt16 (Int16.~ Int16.one))
val _ = (printString "Int16.toLarge (Int16.~ Int16.one) = \n"
         ; printIntInf (Int16.toLarge (Int16.~ Int16.one)))
val _ = (printString "Int16.fromLarge (Int16.toLarge (Int16.~ Int16.one)) = \n"
         ; printInt16 (Int16.fromLarge (Int16.toLarge (Int16.~ Int16.one))))
val _ = (printString "(Int32.~ Int32.one) = \n"
          ; printInt32 (Int32.~ Int32.one))
val _ = (printString "Int32.toLarge (Int32.~ Int32.one) = \n"
         ; printIntInf (Int32.toLarge (Int32.~ Int32.one)))
val _ = (printString "Int32.fromLarge (Int32.toLarge (Int32.~ Int32.one)) = \n"
         ; printInt32 (Int32.fromLarge (Int32.toLarge (Int32.~ Int32.one))))
val _ = (printString "(Int64.~ Int64.one) = \n"
          ; printInt64 (Int64.~ Int64.one))
val _ = (printString "Int64.toLarge (Int64.~ Int64.one) = \n"
         ; printIntInf (Int64.toLarge (Int64.~ Int64.one)))
val _ = (printString "Int64.fromLarge (Int64.toLarge (Int64.~ Int64.one)) = \n"
         ; printInt64 (Int64.fromLarge (Int64.toLarge (Int64.~ Int64.one))))

val _ = (printString "Word8.fromLargeInt 0 = \n"
         ; printWord8 (Word8.fromLargeInt 0))
val _ = (printString "Word16.fromLargeInt 0 = \n"
         ; printWord16 (Word16.fromLargeInt 0))
val _ = (printString "Word32.fromLargeInt 0 = \n"
         ; printWord32 (Word32.fromLargeInt 0))
val _ = (printString "Word64.fromLargeInt 0 = \n"
         ; printWord64 (Word64.fromLargeInt 0))

val _ = (printString "Word8.fromLargeInt 1 = \n"
         ; printWord8 (Word8.fromLargeInt 1))
val _ = (printString "Word16.fromLargeInt 1 = \n"
         ; printWord16 (Word16.fromLargeInt 1))
val _ = (printString "Word32.fromLargeInt 1 = \n"
         ; printWord32 (Word32.fromLargeInt 1))
val _ = (printString "Word64.fromLargeInt 1 = \n"
         ; printWord64 (Word64.fromLargeInt 1))

val _ = (printString "Word8.fromLargeInt ~1 = \n"
         ; printWord8 (Word8.fromLargeInt ~1))
val _ = (printString "Word16.fromLargeInt ~1 = \n"
         ; printWord16 (Word16.fromLargeInt ~1))
val _ = (printString "Word32.fromLargeInt ~1 = \n"
         ; printWord32 (Word32.fromLargeInt ~1))
val _ = (printString "Word64.fromLargeInt ~1 = \n"
         ; printWord64 (Word64.fromLargeInt ~1))

val _ = (printString "Word8.fromLargeInt (Int8.toLarge Int8.minInt') = \n"
         ; printWord8 (Word8.fromLargeInt (Int8.toLarge Int8.minInt')))
val _ = (printString "Word16.fromLargeInt (Int8.toLarge Int8.minInt') = \n"
         ; printWord16 (Word16.fromLargeInt (Int8.toLarge Int8.minInt')))
val _ = (printString "Word32.fromLargeInt (Int8.toLarge Int8.minInt') = \n"
         ; printWord32 (Word32.fromLargeInt (Int8.toLarge Int8.minInt')))
val _ = (printString "Word64.fromLargeInt (Int8.toLarge Int8.minInt') = \n"
         ; printWord64 (Word64.fromLargeInt (Int8.toLarge Int8.minInt')))

val _ = (printString "Word8.fromLargeInt (Int16.toLarge Int16.minInt') = \n"
         ; printWord8 (Word8.fromLargeInt (Int16.toLarge Int16.minInt')))
val _ = (printString "Word16.fromLargeInt (Int16.toLarge Int16.minInt') = \n"
         ; printWord16 (Word16.fromLargeInt (Int16.toLarge Int16.minInt')))
val _ = (printString "Word32.fromLargeInt (Int16.toLarge Int16.minInt') = \n"
         ; printWord32 (Word32.fromLargeInt (Int16.toLarge Int16.minInt')))
val _ = (printString "Word64.fromLargeInt (Int16.toLarge Int16.minInt') = \n"
         ; printWord64 (Word64.fromLargeInt (Int16.toLarge Int16.minInt')))

val _ = (printString "Word8.fromLargeInt (Int32.toLarge Int32.minInt') = \n"
         ; printWord8 (Word8.fromLargeInt (Int32.toLarge Int32.minInt')))
val _ = (printString "Word16.fromLargeInt (Int32.toLarge Int32.minInt') = \n"
         ; printWord16 (Word16.fromLargeInt (Int32.toLarge Int32.minInt')))
val _ = (printString "Word32.fromLargeInt (Int32.toLarge Int32.minInt') = \n"
         ; printWord32 (Word32.fromLargeInt (Int32.toLarge Int32.minInt')))
val _ = (printString "Word64.fromLargeInt (Int32.toLarge Int32.minInt') = \n"
         ; printWord64 (Word64.fromLargeInt (Int32.toLarge Int32.minInt')))

val _ = (printString "Word8.fromLargeInt (Int64.toLarge Int64.minInt') = \n"
         ; printWord8 (Word8.fromLargeInt (Int64.toLarge Int64.minInt')))
val _ = (printString "Word16.fromLargeInt (Int64.toLarge Int64.minInt') = \n"
         ; printWord16 (Word16.fromLargeInt (Int64.toLarge Int64.minInt')))
val _ = (printString "Word32.fromLargeInt (Int64.toLarge Int64.minInt') = \n"
         ; printWord32 (Word32.fromLargeInt (Int64.toLarge Int64.minInt')))
val _ = (printString "Word64.fromLargeInt (Int64.toLarge Int64.minInt') = \n"
         ; printWord64 (Word64.fromLargeInt (Int64.toLarge Int64.minInt')))

val _ = (printString "Word8.fromLargeInt (Int8.toLarge Int8.maxInt') = \n"
         ; printWord8 (Word8.fromLargeInt (Int8.toLarge Int8.maxInt')))
val _ = (printString "Word16.fromLargeInt (Int8.toLarge Int8.maxInt') = \n"
         ; printWord16 (Word16.fromLargeInt (Int8.toLarge Int8.maxInt')))
val _ = (printString "Word32.fromLargeInt (Int8.toLarge Int8.maxInt') = \n"
         ; printWord32 (Word32.fromLargeInt (Int8.toLarge Int8.maxInt')))
val _ = (printString "Word64.fromLargeInt (Int8.toLarge Int8.maxInt') = \n"
         ; printWord64 (Word64.fromLargeInt (Int8.toLarge Int8.maxInt')))

val _ = (printString "Word8.fromLargeInt (Int16.toLarge Int16.maxInt') = \n"
         ; printWord8 (Word8.fromLargeInt (Int16.toLarge Int16.maxInt')))
val _ = (printString "Word16.fromLargeInt (Int16.toLarge Int16.maxInt') = \n"
         ; printWord16 (Word16.fromLargeInt (Int16.toLarge Int16.maxInt')))
val _ = (printString "Word32.fromLargeInt (Int16.toLarge Int16.maxInt') = \n"
         ; printWord32 (Word32.fromLargeInt (Int16.toLarge Int16.maxInt')))
val _ = (printString "Word64.fromLargeInt (Int16.toLarge Int16.maxInt') = \n"
         ; printWord64 (Word64.fromLargeInt (Int16.toLarge Int16.maxInt')))

val _ = (printString "Word8.fromLargeInt (Int32.toLarge Int32.maxInt') = \n"
         ; printWord8 (Word8.fromLargeInt (Int32.toLarge Int32.maxInt')))
val _ = (printString "Word16.fromLargeInt (Int32.toLarge Int32.maxInt') = \n"
         ; printWord16 (Word16.fromLargeInt (Int32.toLarge Int32.maxInt')))
val _ = (printString "Word32.fromLargeInt (Int32.toLarge Int32.maxInt') = \n"
         ; printWord32 (Word32.fromLargeInt (Int32.toLarge Int32.maxInt')))
val _ = (printString "Word64.fromLargeInt (Int32.toLarge Int32.maxInt') = \n"
         ; printWord64 (Word64.fromLargeInt (Int32.toLarge Int32.maxInt')))

val _ = (printString "Word8.fromLargeInt (Int64.toLarge Int64.maxInt') = \n"
         ; printWord8 (Word8.fromLargeInt (Int64.toLarge Int64.maxInt')))
val _ = (printString "Word16.fromLargeInt (Int64.toLarge Int64.maxInt') = \n"
         ; printWord16 (Word16.fromLargeInt (Int64.toLarge Int64.maxInt')))
val _ = (printString "Word32.fromLargeInt (Int64.toLarge Int64.maxInt') = \n"
         ; printWord32 (Word32.fromLargeInt (Int64.toLarge Int64.maxInt')))
val _ = (printString "Word64.fromLargeInt (Int64.toLarge Int64.maxInt') = \n"
         ; printWord64 (Word64.fromLargeInt (Int64.toLarge Int64.maxInt')))

