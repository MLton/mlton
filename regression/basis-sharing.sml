val id = fn x => x

val _ = fn () => (()

; id: 'a Array.array -> 'a array
; id: 'a Array.vector -> 'a vector
; id: BinIO.StreamIO.elem -> Word8.word
; id: BinIO.StreamIO.pos -> BinPrimIO.pos
; id: BinIO.StreamIO.reader -> BinPrimIO.reader
; id: BinIO.StreamIO.vector -> Word8Vector.vector
; id: BinIO.StreamIO.writer -> BinPrimIO.writer
; id: BinPrimIO.array -> Word8Array.array
; id: BinPrimIO.elem -> Word8.word
; id: BinPrimIO.pos -> Position.int
; id: BinPrimIO.vector -> Word8Vector.vector
; id: Bool.bool -> bool
; id: BoolArray.elem -> Bool.bool
; id: BoolArray.vector -> BoolVector.vector
; id: BoolArray2.elem -> Bool.bool
; id: BoolArray2.vector -> BoolVector.vector
; id: BoolArraySlice.array -> BoolArray.array
; id: BoolArraySlice.elem -> Bool.bool
; id: BoolArraySlice.vector -> BoolVector.vector
; id: BoolArraySlice.vector_slice -> BoolVectorSlice.slice
; id: BoolVector.elem -> Bool.bool
; id: BoolVectorSlice.elem -> BoolVector.elem
; id: BoolVectorSlice.vector -> BoolVector.vector
; id: Char.char -> char
; id: Char.string -> String.string
; id: CharArray.elem -> Char.char
; id: CharArray.vector -> CharVector.vector
; id: CharArray2.elem -> Char.char
; id: CharArray2.vector -> CharVector.vector
; id: CharArraySlice.array -> CharArray.array
; id: CharArraySlice.elem -> Char.char
; id: CharArraySlice.vector -> CharVector.vector
; id: CharArraySlice.vector_slice -> CharVectorSlice.slice
; id: CharVector.elem -> Char.char
; id: CharVector.vector -> String.string
; id: CharVectorSlice.elem -> char
; id: CharVectorSlice.slice -> Substring.substring
; id: CharVectorSlice.vector -> String.string
; id: General.exn -> exn
; id: INetSock.dgram_sock -> Socket.dgram INetSock.sock
; id: INetSock.sock_addr -> INetSock.inet Socket.sock_addr
; id: 'sock_type INetSock.sock -> (INetSock.inet, 'sock_type) Socket.sock
; id: 'mode INetSock.stream_sock -> 'mode Socket.stream INetSock.sock
; id: Int16Array.elem -> Int16.int
; id: Int16Array.vector -> Int16Vector.vector
; id: Int16Array2.elem -> Int16.int
; id: Int16Array2.vector -> Int16Vector.vector
; id: Int16ArraySlice.array -> Int16Array.array
; id: Int16ArraySlice.elem -> Int16.int
; id: Int16ArraySlice.vector -> Int16Vector.vector
; id: Int16ArraySlice.vector_slice -> Int16VectorSlice.slice
; id: Int16Vector.elem -> Int16.int
; id: Int16VectorSlice.elem -> Int16Vector.elem
; id: Int16VectorSlice.vector -> Int16Vector.vector
; id: Int32Array.elem -> Int32.int
; id: Int32Array.vector -> Int32Vector.vector
; id: Int32Array2.elem -> Int32.int
; id: Int32Array2.vector -> Int32Vector.vector
; id: Int32ArraySlice.array -> Int32Array.array
; id: Int32ArraySlice.elem -> Int32.int
; id: Int32ArraySlice.vector -> Int32Vector.vector
; id: Int32ArraySlice.vector_slice -> Int32VectorSlice.slice
; id: Int32Vector.elem -> Int32.int
; id: Int32VectorSlice.elem -> Int32Vector.elem
; id: Int32VectorSlice.vector -> Int32Vector.vector
; id: Int64Array.elem -> Int64.int
; id: Int64Array.vector -> Int64Vector.vector
; id: Int64Array2.elem -> Int64.int
; id: Int64Array2.vector -> Int64Vector.vector
; id: Int64ArraySlice.array -> Int64Array.array
; id: Int64ArraySlice.elem -> Int64.int
; id: Int64ArraySlice.vector -> Int64Vector.vector
; id: Int64ArraySlice.vector_slice -> Int64VectorSlice.slice
; id: Int64Vector.elem -> Int64.int
; id: Int64VectorSlice.elem -> Int64Vector.elem
; id: Int64VectorSlice.vector -> Int64Vector.vector
; id: Int8Array.elem -> Int8.int
; id: Int8Array.vector -> Int8Vector.vector
; id: Int8Array2.elem -> Int8.int
; id: Int8Array2.vector -> Int8Vector.vector
; id: Int8ArraySlice.array -> Int8Array.array
; id: Int8ArraySlice.elem -> Int8.int
; id: Int8ArraySlice.vector -> Int8Vector.vector
; id: Int8ArraySlice.vector_slice -> Int8VectorSlice.slice
; id: Int8Vector.elem -> Int8.int
; id: Int8VectorSlice.elem -> Int8Vector.elem
; id: Int8VectorSlice.vector -> Int8Vector.vector
; id: IntArray.elem -> Int.int
; id: IntArray.vector -> IntVector.vector
; id: IntArray2.elem -> Int.int
; id: IntArray2.vector -> IntVector.vector
; id: IntArraySlice.array -> IntArray.array
; id: IntArraySlice.elem -> Int.int
; id: IntArraySlice.vector -> IntVector.vector
; id: IntArraySlice.vector_slice -> IntVectorSlice.slice
; id: IntVector.elem -> Int.int
; id: IntVectorSlice.elem -> IntVector.elem
; id: IntVectorSlice.vector -> IntVector.vector
; id: LargeIntArray.elem -> LargeInt.int
; id: LargeIntArray.vector -> LargeIntVector.vector
; id: LargeIntArray2.elem -> LargeInt.int
; id: LargeIntArray2.vector -> LargeIntVector.vector
; id: LargeIntArraySlice.array -> LargeIntArray.array
; id: LargeIntArraySlice.elem -> LargeInt.int
; id: LargeIntArraySlice.vector -> LargeIntVector.vector
; id: LargeIntArraySlice.vector_slice -> LargeIntVectorSlice.slice
; id: LargeIntVector.elem -> LargeInt.int
; id: LargeIntVectorSlice.elem -> LargeIntVector.elem
; id: LargeIntVectorSlice.vector -> LargeIntVector.vector
; id: LargeRealArray.elem -> LargeReal.real
; id: LargeRealArray.vector -> LargeRealVector.vector
; id: LargeRealArray2.elem -> LargeReal.real
; id: LargeRealArray2.vector -> LargeRealVector.vector
; id: LargeRealArraySlice.array -> LargeRealArray.array
; id: LargeRealArraySlice.elem -> LargeReal.real
; id: LargeRealArraySlice.vector -> LargeRealVector.vector
; id: LargeRealArraySlice.vector_slice -> LargeRealVectorSlice.slice
; id: LargeRealVector.elem -> LargeReal.real
; id: LargeRealVectorSlice.elem -> LargeRealVector.elem
; id: LargeRealVectorSlice.vector -> LargeRealVector.vector
; id: LargeWordArray.elem -> LargeWord.word
; id: LargeWordArray.vector -> LargeWordVector.vector
; id: LargeWordArray2.elem -> LargeWord.word
; id: LargeWordArray2.vector -> LargeWordVector.vector
; id: LargeWordArraySlice.array -> LargeWordArray.array
; id: LargeWordArraySlice.elem -> LargeWord.word
; id: LargeWordArraySlice.vector -> LargeWordVector.vector
; id: LargeWordArraySlice.vector_slice -> LargeWordVectorSlice.slice
; id: LargeWordVector.elem -> LargeWord.word
; id: LargeWordVectorSlice.elem -> LargeWordVector.elem
; id: LargeWordVectorSlice.vector -> LargeWordVector.vector
; id: 'a List.list -> 'a list
; id: Math.real -> Real.real
; id: 'a Option.option -> 'a option
; id: PackReal32Big.real -> Real32.real
; id: PackReal32Little.real -> Real32.real
; id: PackReal64Big.real -> Real64.real
; id: PackReal64Little.real -> Real64.real
; id: PackRealBig.real -> Real.real
; id: PackRealLittle.real -> Real.real
; id: Posix.Error.syserror -> OS.syserror
; id: Posix.FileSys.file_desc -> Posix.ProcEnv.file_desc
; id: Posix.FileSys.gid -> Posix.ProcEnv.gid
; id: Posix.FileSys.uid -> Posix.ProcEnv.uid
; id: Posix.IO.file_desc -> Posix.ProcEnv.file_desc
; id: Posix.IO.open_mode -> Posix.FileSys.open_mode
; id: Posix.IO.pid -> Posix.Process.pid
; id: Posix.ProcEnv.pid -> Posix.Process.pid
; id: Posix.Process.signal -> Posix.Signal.signal
; id: Posix.SysDB.gid -> Posix.ProcEnv.gid
; id: Posix.SysDB.uid -> Posix.ProcEnv.uid
; id: Posix.TTY.file_desc -> Posix.ProcEnv.file_desc
; id: Posix.TTY.pid -> Posix.Process.pid
; id: Real.real -> real
; id: Real32Array.elem -> Real32.real
; id: Real32Array.vector -> Real32Vector.vector
; id: Real32Array2.elem -> Real32.real
; id: Real32Array2.vector -> Real32Vector.vector
; id: Real32ArraySlice.array -> Real32Array.array
; id: Real32ArraySlice.elem -> Real32.real
; id: Real32ArraySlice.vector -> Real32Vector.vector
; id: Real32ArraySlice.vector_slice -> Real32VectorSlice.slice
; id: Real32Vector.elem -> Real32.real
; id: Real32VectorSlice.elem -> Real32Vector.elem
; id: Real32VectorSlice.vector -> Real32Vector.vector
; id: Real64Array.elem -> Real64.real
; id: Real64Array.vector -> Real64Vector.vector
; id: Real64Array2.elem -> Real64.real
; id: Real64Array2.vector -> Real64Vector.vector
; id: Real64ArraySlice.array -> Real64Array.array
; id: Real64ArraySlice.elem -> Real64.real
; id: Real64ArraySlice.vector -> Real64Vector.vector
; id: Real64ArraySlice.vector_slice -> Real64VectorSlice.slice
; id: Real64Vector.elem -> Real64.real
; id: Real64VectorSlice.elem -> Real64Vector.elem
; id: Real64VectorSlice.vector -> Real64Vector.vector
; id: RealArray.elem -> Real.real
; id: RealArray.vector -> RealVector.vector
; id: RealArray2.elem -> Real.real
; id: RealArray2.vector -> RealVector.vector
; id: RealArraySlice.array -> RealArray.array
; id: RealArraySlice.elem -> Real.real
; id: RealArraySlice.vector -> RealVector.vector
; id: RealArraySlice.vector_slice -> RealVectorSlice.slice
; id: RealVector.elem -> Real.real
; id: RealVectorSlice.elem -> RealVector.elem
; id: RealVectorSlice.vector -> RealVector.vector
; id: String.char -> Char.char
; id: String.string -> CharVector.vector
; id: String.string -> string
; id: Substring.char -> Char.char
; id: Substring.string -> String.string
; id: Substring.substring -> CharVectorSlice.slice
; id: Text.Char.char -> Char.char
; id: Text.Char.char -> Text.CharArray.elem
; id: Text.Char.char -> Text.CharArraySlice.elem
; id: Text.Char.char -> Text.CharVector.elem
; id: Text.Char.char -> Text.CharVectorSlice.elem
; id: Text.Char.char -> Text.String.char
; id: Text.Char.char -> Text.Substring.char
; id: Text.Char.string -> Text.CharArray.vector
; id: Text.Char.string -> Text.CharArraySlice.vector
; id: Text.Char.string -> Text.CharVector.vector
; id: Text.Char.string -> Text.CharVectorSlice.vector
; id: Text.Char.string -> Text.String.string
; id: Text.Char.string -> Text.Substring.string
; id: Text.CharArray.array -> CharArray.array
; id: Text.CharArray.array -> Text.CharArraySlice.array
; id: Text.CharArraySlice.array -> Text.CharArray.array
; id: Text.CharArraySlice.slice -> CharArraySlice.slice
; id: Text.CharArraySlice.vector_slice -> Text.CharVectorSlice.slice
; id: Text.CharVector.vector -> CharVector.vector
; id: Text.CharVectorSlice.slice -> CharVectorSlice.slice
; id: Text.CharVectorSlice.slice -> Text.CharArraySlice.vector_slice
; id: Text.String.string -> String.string
; id: Text.Substring.substring -> Substring.substring
; id: TextIO.StreamIO.pos -> TextPrimIO.pos
; id: TextIO.StreamIO.reader -> TextPrimIO.reader
; id: TextIO.StreamIO.writer -> TextPrimIO.writer
; id: TextPrimIO.array -> CharArray.array
; id: TextPrimIO.elem -> Char.char
; id: TextPrimIO.vector -> CharVector.vector
; id: UnixSock.dgram_sock -> Socket.dgram UnixSock.sock
; id: UnixSock.sock_addr -> UnixSock.unix Socket.sock_addr
; id: 'sock_type UnixSock.sock -> (UnixSock.unix, 'sock_type) Socket.sock
; id: 'mode UnixSock.stream_sock -> 'mode Socket.stream UnixSock.sock
; id: 'a Vector.vector -> 'a vector
; id: Word.word -> word
; id: Word16Array.elem -> Word16.word
; id: Word16Array.vector -> Word16Vector.vector
; id: Word16Array2.elem -> Word16.word
; id: Word16Array2.vector -> Word16Vector.vector
; id: Word16ArraySlice.array -> Word16Array.array
; id: Word16ArraySlice.elem -> Word16.word
; id: Word16ArraySlice.vector -> Word16Vector.vector
; id: Word16ArraySlice.vector_slice -> Word16VectorSlice.slice
; id: Word16Vector.elem -> Word16.word
; id: Word16VectorSlice.elem -> Word16Vector.elem
; id: Word16VectorSlice.vector -> Word16Vector.vector
; id: Word32Array.elem -> Word32.word
; id: Word32Array.vector -> Word32Vector.vector
; id: Word32Array2.elem -> Word32.word
; id: Word32Array2.vector -> Word32Vector.vector
; id: Word32ArraySlice.array -> Word32Array.array
; id: Word32ArraySlice.elem -> Word32.word
; id: Word32ArraySlice.vector -> Word32Vector.vector
; id: Word32ArraySlice.vector_slice -> Word32VectorSlice.slice
; id: Word32Vector.elem -> Word32.word
; id: Word32VectorSlice.elem -> Word32Vector.elem
; id: Word32VectorSlice.vector -> Word32Vector.vector
; id: Word64Array.elem -> Word64.word
; id: Word64Array.vector -> Word64Vector.vector
; id: Word64Array2.elem -> Word64.word
; id: Word64Array2.vector -> Word64Vector.vector
; id: Word64ArraySlice.array -> Word64Array.array
; id: Word64ArraySlice.elem -> Word64.word
; id: Word64ArraySlice.vector -> Word64Vector.vector
; id: Word64ArraySlice.vector_slice -> Word64VectorSlice.slice
; id: Word64Vector.elem -> Word64.word
; id: Word64VectorSlice.elem -> Word64Vector.elem
; id: Word64VectorSlice.vector -> Word64Vector.vector
; id: Word8Array.elem -> Word8.word
; id: Word8Array.vector -> Word8Vector.vector
; id: Word8Array2.elem -> Word8.word
; id: Word8Array2.vector -> Word8Vector.vector
; id: Word8ArraySlice.array -> Word8Array.array
; id: Word8ArraySlice.elem -> Word8.word
; id: Word8ArraySlice.vector -> Word8Vector.vector
; id: Word8ArraySlice.vector_slice -> Word8VectorSlice.slice
; id: Word8Vector.elem -> Word8.word
; id: Word8VectorSlice.elem -> Word8Vector.elem
; id: Word8VectorSlice.vector -> Word8Vector.vector
; id: WordArray.elem -> Word.word
; id: WordArray.vector -> WordVector.vector
; id: WordArray2.elem -> Word.word
; id: WordArray2.vector -> WordVector.vector
; id: WordArraySlice.array -> WordArray.array
; id: WordArraySlice.elem -> Word.word
; id: WordArraySlice.vector -> WordVector.vector
; id: WordArraySlice.vector_slice -> WordVectorSlice.slice
; id: WordVector.elem -> Word.word
; id: WordVectorSlice.elem -> WordVector.elem
; id: WordVectorSlice.vector -> WordVector.vector

   (* ; id: WideChar.string -> WideString.string
 * ; id: WideCharArray.elem -> WideChar.char
 * ; id: WideCharArray.vector -> WideCharVector.vector
 * ; id: WideCharArray2.elem -> WideChar.char
 * ; id: WideCharArray2.vector -> WideCharVector.vector
 * ; id: WideCharArraySlice.array -> WideCharArray.array
 * ; id: WideCharArraySlice.elem -> WideChar.char
 * ; id: WideCharArraySlice.vector -> WideCharVector.vector
 * ; id: WideCharArraySlice.vector_slice -> WideCharVectorSlice.slice
 * ; id: WideCharVector.elem -> WideChar.char
 * ; id: WideCharVector.vector -> WideString.string
 * ; id: WideCharVectorSlice.elem -> WideCharVector.elem
 * ; id: WideCharVectorSlice.vector -> WideCharVector.vector
 * ; id: WideString.string -> WideCharVector.vector
 * ; id: WideString.type char -> WideChar.char
 * ; id: WideSubstring.char -> WideChar.char
 * ; id: WideSubstring.string -> WideString.string
 * ; id: WideSubstring.substring -> WideCharVectorSlice.slice
 * ; id: WideText.Char.char -> WideChar.char
 * ; id: WideText.CharArray.array -> WideCharArray.array
 * ; id: WideText.CharArraySlice.slice -> WideCharArraySlice.slice
 * ; id: WideText.CharVector.vector -> WideCharVector.vector
 * ; id: WideText.CharVectorSlice.slice -> WideCharVectorSlice.slice
 * ; id: WideText.String.string -> WideString.string
 * ; id: WideText.Substring.substring -> WideSubstring.substring
 * ; id: WideTextPrimIO.array -> WideCharArray.array
 * ; id: WideTextPrimIO.elem -> WideChar.char
 * ; id: WideTextPrimIO.vector -> WideCharVector.vector
 *)

)
