signature BASIS_EXTRA =
   sig
      (* Top-level types *)
      eqtype 'a array
      datatype bool = datatype bool
      eqtype char
      type exn
      eqtype int 
      datatype 'a option = NONE | SOME of 'a 
      datatype order = LESS | EQUAL | GREATER 
      datatype list = datatype list
      datatype ref = datatype ref
      type real
      eqtype string
      type substring 
      eqtype unit
      eqtype 'a vector
      eqtype word 

      (* Top-level exceptions *)
      exception Bind 
      exception Chr
      exception Div
      exception Domain
      exception Empty
      exception Fail of string
      exception Match
      exception Option
      exception Overflow
      exception Size
      exception Span
      exception Subscript

      (* Top-level values *)
      val = : ''a * ''a -> bool
      val <> : ''a * ''a -> bool

      val ! : 'a ref -> 'a
      val := : 'a ref * 'a -> unit
      val @ : ('a list * 'a list) -> 'a list
      val ^ : string * string -> string
      val app : ('a -> unit) -> 'a list -> unit
      val before : 'a * unit -> 'a
      val ceil : real -> int 
      val chr : int -> char
      val concat : string list -> string
      val exnMessage : exn -> string
      val exnName : exn -> string
      val explode : string -> char list
      val floor : real -> int 
      val foldl : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
      val foldr : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b 
      val getOpt : ('a option * 'a) -> 'a
      val hd : 'a list -> 'a
      val ignore : 'a -> unit
      val isSome : 'a option -> bool
      val implode : char list -> string
      val length : 'a list -> int
      val map : ('a -> 'b) -> 'a list -> 'b list
      val not : bool -> bool
      val null : 'a list -> bool
      val o : ('a -> 'b) * ('c -> 'a) -> 'c -> 'b
      val ord : char -> int
      val print : string -> unit
      val real : int -> real
(*
      val ref : 'a -> 'a ref
*)
      val rev : 'a list -> 'a list
      val round : real -> int
      val size : string -> int
      val str : char -> string
      val substring : string * int * int -> string
      val tl : 'a list -> 'a list
      val trunc : real -> int 
(*
      val use : string -> unit
*)
      val valOf : 'a option -> 'a 
      val vector : 'a list -> 'a vector

      (* Required structures *)
      structure Array : ARRAY   
      structure ArraySlice : ARRAY_SLICE        
      structure BinIO : BIN_IO  
      structure BinPrimIO : PRIM_IO     
      structure Bool : BOOL     
      structure Byte : BYTE     
      structure Char : CHAR     
      structure CharArray : MONO_ARRAY  
      structure CharArraySlice : MONO_ARRAY_SLICE       
      structure CharVector : MONO_VECTOR        
      structure CharVectorSlice : MONO_VECTOR_SLICE     
      structure CommandLine : COMMAND_LINE      
      structure Date : DATE     
      structure General : GENERAL       
      structure IEEEReal : IEEE_REAL    
      structure Int : INTEGER   
      structure IO : IO 
      structure LargeInt : INTEGER      
      structure LargeReal : REAL        
      structure LargeWord : WORD        
      structure List : LIST     
      structure ListPair : LIST_PAIR    
      structure Math : MATH     
      structure Option : OPTION 
      structure OS : OS 
      structure Position : INTEGER      
      structure Real : REAL     
      structure StringCvt : STRING_CVT  
      structure String : STRING 
      structure Substring : SUBSTRING   
      structure TextIO : TEXT_IO        
      structure TextPrimIO : PRIM_IO    
      structure Text : TEXT     
      structure Time : TIME     
      structure Timer : TIMER   
      structure VectorSlice : VECTOR_SLICE      
      structure Vector : VECTOR 
      structure Word : WORD     
      structure Word8Array : MONO_ARRAY 
      structure Word8Array2 : MONO_ARRAY2       
      structure Word8ArraySlice : MONO_ARRAY_SLICE      
      structure Word8Vector : MONO_VECTOR       
      structure Word8VectorSlice : MONO_VECTOR_SLICE    

      (* Optional structures *)
      structure Array2 : ARRAY2
      structure BoolArray : MONO_ARRAY
      structure BoolArray2 : MONO_ARRAY2
      structure BoolArraySlice : MONO_ARRAY_SLICE
      structure BoolVector : MONO_VECTOR
      structure BoolVectorSlice : MONO_VECTOR_SLICE
      structure CharArray2 : MONO_ARRAY2
      structure FixedInt : INTEGER
      structure GenericSock : GENERIC_SOCK
      structure INetSock : INET_SOCK
      structure Int1: INTEGER
      structure Int2: INTEGER
      structure Int3: INTEGER
      structure Int4: INTEGER
      structure Int5: INTEGER
      structure Int6: INTEGER
      structure Int7: INTEGER
      structure Int8: INTEGER
      structure Int9: INTEGER
      structure Int10: INTEGER
      structure Int11: INTEGER
      structure Int12: INTEGER
      structure Int13: INTEGER
      structure Int14: INTEGER
      structure Int15: INTEGER
      structure Int16: INTEGER
      structure Int17: INTEGER
      structure Int18: INTEGER
      structure Int19: INTEGER
      structure Int20: INTEGER
      structure Int21: INTEGER
      structure Int22: INTEGER
      structure Int23: INTEGER
      structure Int24: INTEGER
      structure Int25: INTEGER
      structure Int26: INTEGER
      structure Int27: INTEGER
      structure Int28: INTEGER
      structure Int29: INTEGER
      structure Int30: INTEGER
      structure Int31: INTEGER
      structure Int32: INTEGER
      structure Int64: INTEGER
      structure Int8Array : MONO_ARRAY
      structure Int8Array2 : MONO_ARRAY2
      structure Int8ArraySlice : MONO_ARRAY_SLICE
      structure Int8Vector : MONO_VECTOR
      structure Int8VectorSlice : MONO_VECTOR_SLICE
      structure Int16Array : MONO_ARRAY
      structure Int16Array2 : MONO_ARRAY2
      structure Int16ArraySlice : MONO_ARRAY_SLICE
      structure Int16Vector : MONO_VECTOR
      structure Int16VectorSlice : MONO_VECTOR_SLICE
      structure Int32Array : MONO_ARRAY
      structure Int32Array2 : MONO_ARRAY2
      structure Int32ArraySlice : MONO_ARRAY_SLICE
      structure Int32Vector : MONO_VECTOR
      structure Int32VectorSlice : MONO_VECTOR_SLICE
      structure Int64Array : MONO_ARRAY
      structure Int64Array2 : MONO_ARRAY2
      structure Int64ArraySlice : MONO_ARRAY_SLICE
      structure Int64Vector : MONO_VECTOR
      structure Int64VectorSlice : MONO_VECTOR_SLICE
      structure IntArray : MONO_ARRAY
      structure IntArray2 : MONO_ARRAY2
      structure IntArraySlice : MONO_ARRAY_SLICE
      structure IntVector : MONO_VECTOR
      structure IntVectorSlice : MONO_VECTOR_SLICE
      structure IntInf : INT_INF
      structure LargeIntArray : MONO_ARRAY
      structure LargeIntArray2 : MONO_ARRAY2
      structure LargeIntArraySlice : MONO_ARRAY_SLICE
      structure LargeIntVector : MONO_VECTOR
      structure LargeIntVectorSlice : MONO_VECTOR_SLICE
      structure LargeRealArray : MONO_ARRAY
      structure LargeRealArray2 : MONO_ARRAY2
      structure LargeRealArraySlice : MONO_ARRAY_SLICE
      structure LargeRealVector : MONO_VECTOR
      structure LargeRealVectorSlice : MONO_VECTOR_SLICE
      structure LargeWordArray : MONO_ARRAY
      structure LargeWordArray2 : MONO_ARRAY2
      structure LargeWordArraySlice : MONO_ARRAY_SLICE
      structure LargeWordVector : MONO_VECTOR
      structure LargeWordVectorSlice : MONO_VECTOR_SLICE
      structure NetHostDB : NET_HOST_DB
      structure NetProtDB : NET_PROT_DB
      structure NetServDB : NET_SERV_DB
      structure PackReal32Big : PACK_REAL
      structure PackReal32Little : PACK_REAL
      structure PackReal64Big : PACK_REAL
      structure PackReal64Little : PACK_REAL
      structure PackRealBig : PACK_REAL
      structure PackRealLittle : PACK_REAL
      structure PackWord16Big : PACK_WORD
      structure PackWord16Little : PACK_WORD
      structure PackWord32Big : PACK_WORD
      structure PackWord32Little : PACK_WORD
      structure PackWord64Big : PACK_WORD
      structure PackWord64Little : PACK_WORD
      structure Posix : POSIX
      structure Real32 : REAL
      structure Real32Array : MONO_ARRAY
      structure Real32Array2 : MONO_ARRAY2
      structure Real32ArraySlice : MONO_ARRAY_SLICE
      structure Real32Vector : MONO_VECTOR
      structure Real32VectorSlice : MONO_VECTOR_SLICE
      structure Real64 : REAL
      structure Real64Array : MONO_ARRAY
      structure Real64Array2 : MONO_ARRAY2
      structure Real64ArraySlice : MONO_ARRAY_SLICE
      structure Real64Vector : MONO_VECTOR
      structure Real64VectorSlice : MONO_VECTOR_SLICE
      structure RealArray : MONO_ARRAY
      structure RealArray2 : MONO_ARRAY2
      structure RealArraySlice : MONO_ARRAY_SLICE
      structure RealVector : MONO_VECTOR
      structure RealVectorSlice : MONO_VECTOR_SLICE
      structure Socket : SOCKET
      structure SysWord : WORD
      structure Unix : UNIX
      structure UnixSock : UNIX_SOCK
      structure WideChar : CHAR
      structure WideCharArray : MONO_ARRAY
      structure WideCharArray2 : MONO_ARRAY2
      structure WideCharArraySlice : MONO_ARRAY_SLICE
      structure WideCharVector : MONO_VECTOR
      structure WideCharVectorSlice : MONO_VECTOR_SLICE
      structure WideString : STRING
      structure WideSubstring : SUBSTRING
      structure WideText : TEXT
(*
      structure WideTextIO : TEXT_IO
      structure WideTextPrimIO : PRIM_IO
*)
(*
      structure Windows : WINDOWS
*)
      structure Word1: WORD
      structure Word2: WORD
      structure Word3: WORD
      structure Word4: WORD
      structure Word5: WORD
      structure Word6: WORD
      structure Word7: WORD
      structure Word8: WORD
      structure Word9: WORD
      structure Word10: WORD
      structure Word11: WORD
      structure Word12: WORD
      structure Word13: WORD
      structure Word14: WORD
      structure Word15: WORD
      structure Word16: WORD
      structure Word17: WORD
      structure Word18: WORD
      structure Word19: WORD
      structure Word20: WORD
      structure Word21: WORD
      structure Word22: WORD
      structure Word23: WORD
      structure Word24: WORD
      structure Word25: WORD
      structure Word26: WORD
      structure Word27: WORD
      structure Word28: WORD
      structure Word29: WORD
      structure Word30: WORD
      structure Word31: WORD
      structure Word32: WORD
      structure Word64: WORD
      structure WordArray : MONO_ARRAY
      structure WordArray2 : MONO_ARRAY2
      structure WordArraySlice : MONO_ARRAY_SLICE
      structure WordVector : MONO_VECTOR
      structure WordVectorSlice : MONO_VECTOR_SLICE
      structure Word16Array : MONO_ARRAY
      structure Word16Array2 : MONO_ARRAY2
      structure Word16ArraySlice : MONO_ARRAY_SLICE
      structure Word16Vector : MONO_VECTOR
      structure Word16VectorSlice : MONO_VECTOR_SLICE
      structure Word32Array : MONO_ARRAY
      structure Word32Array2 : MONO_ARRAY2
      structure Word32ArraySlice : MONO_ARRAY_SLICE
      structure Word32Vector : MONO_VECTOR
      structure Word32VectorSlice : MONO_VECTOR_SLICE
      structure Word64Array : MONO_ARRAY
      structure Word64Array2 : MONO_ARRAY2
      structure Word64ArraySlice : MONO_ARRAY_SLICE
      structure Word64Vector : MONO_VECTOR
      structure Word64VectorSlice : MONO_VECTOR_SLICE

      (* Non-standard structures *)
      structure SML90: SML90
      structure MLton: MLTON
      structure SMLofNJ: SML_OF_NJ
      structure Unsafe: UNSAFE

      sharing type MLton.IntInf.t = IntInf.int
      sharing type MLton.Process.pid = Posix.Process.pid
      sharing type MLton.ProcEnv.gid = Posix.ProcEnv.gid
      sharing type MLton.LargeReal.t = LargeReal.real
      sharing type MLton.LargeWord.t = LargeWord.word
      sharing type MLton.Real.t = Real.real
      sharing type MLton.Real32.t = Real32.real
      sharing type MLton.Real64.t = Real64.real
      sharing type MLton.Signal.t = Posix.Signal.signal
      sharing type MLton.Word.t = Word.word
      sharing type MLton.Word8.t = Word8.word
      sharing type MLton.Word16.t = Word16.word
      sharing type MLton.Word32.t = Word32.word
      sharing type MLton.Word64.t = Word64.word
      sharing Unsafe.CharArray = CharArray
      sharing Unsafe.CharVector = CharVector
      sharing Unsafe.Real64Array = Real64Array
      sharing Unsafe.Word8Array = Word8Array
      sharing Unsafe.Word8Vector = Word8Vector

      (* ************************************************** *)
      (* ************************************************** *)

      (* Sharing constraints *)

      (* Top-level types *)
      sharing type unit = General.unit
      sharing type int = Int.int
      sharing type word = Word.word
      sharing type real = Real.real
      sharing type char = Char.char
      sharing type string = String.string
      sharing type substring = Substring.substring
      sharing type exn = General.exn
(* Can't use sharing on type array or vector, because they are rigid tycons.
 * Don't need it anyways, since it's built into the ARRAY and VECTOR signatures.
 *)
(*
      sharing type array = Array.array
      sharing type vector = Vector.vector 
*)
(*
      sharing type ref = General.ref
*)
(*
      sharing type bool = Bool.bool
*)
      sharing type option = Option.option
      sharing type order = General.order
(*
      sharing type list = List.list
*)

      (* Required structures *)
(*
      sharing type BinIO.StreamIO.elem = Word8.word 
*)
      sharing type BinIO.StreamIO.reader = BinPrimIO.reader
      sharing type BinIO.StreamIO.pos = BinPrimIO.pos
(*
      sharing type BinIO.StreamIO.vector = Word8Vector.vector 
*)
      sharing type BinIO.StreamIO.writer = BinPrimIO.writer
      sharing type BinPrimIO.array = Word8Array.array
      sharing type BinPrimIO.array_slice = Word8ArraySlice.slice
      sharing type BinPrimIO.elem = Word8.word
      sharing type BinPrimIO.pos = Position.int
      sharing type BinPrimIO.vector = Word8Vector.vector
      sharing type BinPrimIO.vector_slice = Word8VectorSlice.slice
      sharing type Char.char = char
      sharing type Char.string = String.string
      sharing type CharArray.elem = char
      sharing type CharArray.vector = CharVector.vector
      sharing type CharArraySlice.elem = char
      sharing type CharArraySlice.array = CharArray.array
      sharing type CharArraySlice.vector = CharVector.vector
      sharing type CharArraySlice.vector_slice = CharVectorSlice.slice
      sharing type CharVector.elem = char
      sharing type CharVector.vector = String.string
      sharing type CharVectorSlice.elem = char
      sharing type CharVectorSlice.vector = String.string
      sharing type CharVectorSlice.slice = Substring.substring
      sharing type Int.int = int
      sharing type Math.real = Real.real
      sharing type Real.real = real
      sharing type String.string = string
      sharing type String.string = CharVector.vector
      sharing type String.char = Char.char
      sharing type Substring.substring = CharVectorSlice.slice
      sharing type Substring.string = String.string
      sharing type Substring.char = Char.char
      sharing type Text.Char.char = Char.char
      sharing type Text.String.string = String.string
      sharing type Text.Substring.substring = Substring.substring
      sharing type Text.CharVector.vector = CharVector.vector
      sharing type Text.CharArray.array = CharArray.array
      sharing type Text.CharArraySlice.slice = CharArraySlice.slice
      sharing type Text.CharVectorSlice.slice = CharVectorSlice.slice
      (* redundant *)
(*
      sharing type TextIO.elem = char
      sharing type TextIO.vector = string 
*)
      sharing type TextPrimIO.array = CharArray.array
      sharing type TextPrimIO.array_slice = CharArraySlice.slice
      sharing type TextPrimIO.elem = Char.char
      sharing type TextPrimIO.pos = Position.int
      sharing type TextPrimIO.vector = CharVector.vector
      sharing type TextPrimIO.vector_slice = CharVectorSlice.slice
      sharing type Word.word = word
      sharing type Word8Array.elem = Word8.word
      sharing type Word8Array.vector = Word8Vector.vector
      sharing type Word8ArraySlice.elem = Word8.word
      sharing type Word8ArraySlice.array = Word8Array.array
      sharing type Word8ArraySlice.vector = Word8Vector.vector
      sharing type Word8ArraySlice.vector_slice = Word8VectorSlice.slice
      sharing type Word8Vector.elem = Word8.word
      sharing type Word8VectorSlice.elem = Word8.word
      sharing type Word8VectorSlice.vector = Word8Vector.vector
      sharing type Word8Array2.elem = Word8.word
      sharing type Word8Array2.vector = Word8Vector.vector

      (* Optional structures *)
      sharing type BoolArray.vector = BoolVector.vector
      sharing type BoolArraySlice.array = BoolArray.array
      sharing type BoolArraySlice.vector = BoolVector.vector
      sharing type BoolArraySlice.vector_slice = BoolVectorSlice.slice
      sharing type BoolVectorSlice.vector = BoolVector.vector
      sharing type BoolArray2.vector = BoolVector.vector
      sharing type CharArray2.elem = char
      sharing type CharArray2.vector = CharVector.vector
      sharing type IntArray.elem = int
      sharing type IntArray.vector = IntVector.vector
      sharing type IntArraySlice.elem = int
      sharing type IntArraySlice.array = IntArray.array
      sharing type IntArraySlice.vector = IntVector.vector
      sharing type IntArraySlice.vector_slice = IntVectorSlice.slice
      sharing type IntVector.elem = int
      sharing type IntVectorSlice.elem = int
      sharing type IntVectorSlice.vector = IntVector.vector
      sharing type IntArray2.elem = int
      sharing type IntArray2.vector = IntVector.vector
      sharing type Int8Array.elem = Int8.int
      sharing type Int8Array.vector = Int8Vector.vector
      sharing type Int8ArraySlice.elem = Int8.int
      sharing type Int8ArraySlice.array = Int8Array.array
      sharing type Int8ArraySlice.vector = Int8Vector.vector
      sharing type Int8ArraySlice.vector_slice = Int8VectorSlice.slice
      sharing type Int8Vector.elem = Int8.int
      sharing type Int8VectorSlice.elem = Int8.int
      sharing type Int8VectorSlice.vector = Int8Vector.vector
      sharing type Int8Array2.elem = Int8.int
      sharing type Int8Array2.vector = Int8Vector.vector
      sharing type Int16Array.elem = Int16.int
      sharing type Int16Array.vector = Int16Vector.vector
      sharing type Int16ArraySlice.elem = Int16.int
      sharing type Int16ArraySlice.array = Int16Array.array
      sharing type Int16ArraySlice.vector = Int16Vector.vector
      sharing type Int16ArraySlice.vector_slice = Int16VectorSlice.slice
      sharing type Int16Vector.elem = Int16.int
      sharing type Int16VectorSlice.elem = Int16.int
      sharing type Int16VectorSlice.vector = Int16Vector.vector
      sharing type Int16Array2.elem = Int16.int
      sharing type Int16Array2.vector = Int16Vector.vector
      sharing type Int32Array.elem = Int32.int
      sharing type Int32Array.vector = Int32Vector.vector
      sharing type Int32ArraySlice.elem = Int32.int
      sharing type Int32ArraySlice.array = Int32Array.array
      sharing type Int32ArraySlice.vector = Int32Vector.vector
      sharing type Int32ArraySlice.vector_slice = Int32VectorSlice.slice
      sharing type Int32Vector.elem = Int32.int
      sharing type Int32VectorSlice.elem = Int32.int
      sharing type Int32VectorSlice.vector = Int32Vector.vector
      sharing type Int32Array2.elem = Int32.int
      sharing type Int32Array2.vector = Int32Vector.vector
      sharing type Int64Array.elem = Int64.int
      sharing type Int64Array.vector = Int64Vector.vector
      sharing type Int64ArraySlice.elem = Int64.int
      sharing type Int64ArraySlice.array = Int64Array.array
      sharing type Int64ArraySlice.vector = Int64Vector.vector
      sharing type Int64ArraySlice.vector_slice = Int64VectorSlice.slice
      sharing type Int64Vector.elem = Int64.int
      sharing type Int64VectorSlice.elem = Int64.int
      sharing type Int64VectorSlice.vector = Int64Vector.vector
      sharing type Int64Array2.elem = Int64.int
      sharing type Int64Array2.vector = Int64Vector.vector
      sharing type LargeIntArray.elem = LargeInt.int
      sharing type LargeIntArray.vector = LargeIntVector.vector
      sharing type LargeIntArraySlice.elem = LargeInt.int
      sharing type LargeIntArraySlice.array = LargeIntArray.array
      sharing type LargeIntArraySlice.vector = LargeIntVector.vector
      sharing type LargeIntArraySlice.vector_slice = LargeIntVectorSlice.slice
      sharing type LargeIntVector.elem = LargeInt.int
      sharing type LargeIntVectorSlice.elem = LargeInt.int
      sharing type LargeIntVectorSlice.vector = LargeIntVector.vector
      sharing type LargeIntArray2.elem = LargeInt.int
      sharing type LargeIntArray2.vector = LargeIntVector.vector
      sharing type LargeRealArray.elem = LargeReal.real
      sharing type LargeRealArray.vector = LargeRealVector.vector
      sharing type LargeRealArraySlice.elem = LargeReal.real
      sharing type LargeRealArraySlice.array = LargeRealArray.array
      sharing type LargeRealArraySlice.vector = LargeRealVector.vector
      sharing type LargeRealArraySlice.vector_slice = LargeRealVectorSlice.slice
      sharing type LargeRealVector.elem = LargeReal.real
      sharing type LargeRealVectorSlice.elem = LargeReal.real
      sharing type LargeRealVectorSlice.vector = LargeRealVector.vector
      sharing type LargeRealArray2.elem = LargeReal.real
      sharing type LargeRealArray2.vector = LargeRealVector.vector
      sharing type LargeWordArray.elem = LargeWord.word
      sharing type LargeWordArray.vector = LargeWordVector.vector
      sharing type LargeWordArraySlice.elem = LargeWord.word
      sharing type LargeWordArraySlice.array = LargeWordArray.array
      sharing type LargeWordArraySlice.vector = LargeWordVector.vector
      sharing type LargeWordArraySlice.vector_slice = LargeWordVectorSlice.slice
      sharing type LargeWordVector.elem = LargeWord.word
      sharing type LargeWordVectorSlice.elem = LargeWord.word
      sharing type LargeWordVectorSlice.vector = LargeWordVector.vector
      sharing type LargeWordArray2.elem = LargeWord.word
      sharing type LargeWordArray2.vector = LargeWordVector.vector
      sharing type PackRealBig.real = real
      sharing type PackRealLittle.real = real
      sharing type PackReal32Big.real = Real32.real
      sharing type PackReal32Little.real = Real32.real
      sharing type PackReal64Big.real = Real64.real
      sharing type PackReal64Little.real = Real64.real
      sharing type Posix.Error.syserror = OS.syserror
      sharing type Posix.IO.file_desc = Posix.ProcEnv.file_desc
      sharing type Posix.FileSys.dirstream = OS.FileSys.dirstream
      sharing type Posix.FileSys.access_mode = OS.FileSys.access_mode
      sharing type Posix.Process.exit_status = Unix.exit_status
      sharing type Posix.Signal.signal = Unix.signal
      sharing type RealArray.elem = real
      sharing type RealArray.vector = RealVector.vector
      sharing type RealArraySlice.elem = real
      sharing type RealArraySlice.array = RealArray.array
      sharing type RealArraySlice.vector = RealVector.vector
      sharing type RealArraySlice.vector_slice = RealVectorSlice.slice
      sharing type RealVector.elem = real
      sharing type RealVectorSlice.elem = real
      sharing type RealVectorSlice.vector = RealVector.vector
      sharing type RealArray2.elem = real
      sharing type RealArray2.vector = RealVector.vector
      sharing type Real32Array.elem = Real32.real
      sharing type Real32Array.vector = Real32Vector.vector
      sharing type Real32ArraySlice.elem = Real32.real
      sharing type Real32ArraySlice.array = Real32Array.array
      sharing type Real32ArraySlice.vector = Real32Vector.vector
      sharing type Real32ArraySlice.vector_slice = Real32VectorSlice.slice
      sharing type Real32Vector.elem = Real32.real
      sharing type Real32VectorSlice.elem = Real32.real
      sharing type Real32VectorSlice.vector = Real32Vector.vector
      sharing type Real32Array2.elem = Real32.real
      sharing type Real32Array2.vector = Real32Vector.vector
      sharing type Real64Array.elem = Real64.real
      sharing type Real64Array.vector = Real64Vector.vector
      sharing type Real64ArraySlice.elem = Real64.real
      sharing type Real64ArraySlice.array = Real64Array.array
      sharing type Real64ArraySlice.vector = Real64Vector.vector
      sharing type Real64ArraySlice.vector_slice = Real64VectorSlice.slice
      sharing type Real64Vector.elem = Real64.real
      sharing type Real64VectorSlice.elem = Real64.real
      sharing type Real64VectorSlice.vector = Real64Vector.vector
      sharing type Real64Array2.elem = Real64.real
      sharing type Real64Array2.vector = Real64Vector.vector
      sharing type Unix.exit_status = Posix.Process.exit_status
      sharing type WideChar.string = WideString.string
      sharing type WideCharArray.elem = WideChar.char
      sharing type WideCharArray.vector = WideCharVector.vector
      sharing type WideCharArray2.elem = WideChar.char
      sharing type WideCharArray2.vector = WideCharVector.vector
      sharing type WideCharArraySlice.elem = WideChar.char
      sharing type WideCharArraySlice.array = WideCharArray.array
      sharing type WideCharArraySlice.vector = WideCharVector.vector
      sharing type WideCharArraySlice.vector_slice = WideCharVectorSlice.slice
      sharing type WideCharVector.elem = WideChar.char
      sharing type WideCharVector.vector = WideString.string
      sharing type WideCharVectorSlice.elem = WideChar.char
      sharing type WideCharVectorSlice.slice = WideSubstring.substring
      sharing type WideCharVectorSlice.vector = WideString.string
      sharing type WideString.char = WideChar.char
      (* next two are redundant? basis & char both do it... *)
      sharing type WideString.string = WideCharVector.vector
      sharing type WideSubstring.substring = WideCharVectorSlice.slice
      sharing type WideSubstring.string = WideString.string
      sharing type WideSubstring.char = WideChar.char
      sharing type WideText.Char.char = WideChar.char
      sharing type WideText.String.string = WideString.string
      sharing type WideText.Substring.substring = WideSubstring.substring
      sharing type WideText.CharVector.vector = WideCharVector.vector
      sharing type WideText.CharArray.array = WideCharArray.array
      sharing type WideText.CharArraySlice.slice = WideCharArraySlice.slice
      sharing type WideText.CharVectorSlice.slice = WideCharVectorSlice.slice
(*
      sharing type WideTextIO.
      sharing type WideTextPrimIO.array = WideCharArray.array
      sharing type WideTextPrimIO.vector = WideCharVector.vector
      sharing type WideTextPrimIO.elem = WideChar.char
*)
      sharing type WordArray.elem = word
      sharing type WordArray.vector = WordVector.vector
      sharing type WordArraySlice.elem = word
      sharing type WordArraySlice.array = WordArray.array
      sharing type WordArraySlice.vector = WordVector.vector
      sharing type WordArraySlice.vector_slice = WordVectorSlice.slice
      sharing type WordVector.elem = word
      sharing type WordVectorSlice.elem = word
      sharing type WordVectorSlice.vector = WordVector.vector
      sharing type WordArray2.elem = word
      sharing type WordArray2.vector = WordVector.vector
      sharing type Word16Array.elem = Word16.word
      sharing type Word16Array.vector = Word16Vector.vector
      sharing type Word16ArraySlice.elem = Word16.word
      sharing type Word16ArraySlice.array = Word16Array.array
      sharing type Word16ArraySlice.vector = Word16Vector.vector
      sharing type Word16ArraySlice.vector_slice = Word16VectorSlice.slice
      sharing type Word16Vector.elem = Word16.word
      sharing type Word16VectorSlice.elem = Word16.word
      sharing type Word16VectorSlice.vector = Word16Vector.vector
      sharing type Word16Array2.elem = Word16.word
      sharing type Word16Array2.vector = Word16Vector.vector
      sharing type Word32Array.elem = Word32.word
      sharing type Word32Array.vector = Word32Vector.vector
      sharing type Word32ArraySlice.elem = Word32.word
      sharing type Word32ArraySlice.array = Word32Array.array
      sharing type Word32ArraySlice.vector = Word32Vector.vector
      sharing type Word32ArraySlice.vector_slice = Word32VectorSlice.slice
      sharing type Word32Vector.elem = Word32.word
      sharing type Word32VectorSlice.elem = Word32.word
      sharing type Word32VectorSlice.vector = Word32Vector.vector
      sharing type Word32Array2.elem = Word32.word
      sharing type Word32Array2.vector = Word32Vector.vector
      sharing type Word64Array.elem = Word64.word
      sharing type Word64Array.vector = Word64Vector.vector
      sharing type Word64ArraySlice.elem = Word64.word
      sharing type Word64ArraySlice.array = Word64Array.array
      sharing type Word64ArraySlice.vector = Word64Vector.vector
      sharing type Word64ArraySlice.vector_slice = Word64VectorSlice.slice
      sharing type Word64Vector.elem = Word64.word
      sharing type Word64VectorSlice.elem = Word64.word
      sharing type Word64VectorSlice.vector = Word64Vector.vector
      sharing type Word64Array2.elem = Word64.word
      sharing type Word64Array2.vector = Word64Vector.vector
      sharing type MLton.BinIO.instream = BinIO.instream
      sharing type MLton.BinIO.outstream = BinIO.outstream
      sharing type MLton.TextIO.instream = TextIO.instream
      sharing type MLton.TextIO.outstream = TextIO.outstream
      sharing type MLton.Word8Array.t = Word8Array.array
      sharing type MLton.Word8Vector.t = Word8Vector.vector
   end
   (* bool is already defined as bool and so cannot be shared.
    * So, we where these to get the needed sharing.
    *)
   where type BoolArray.elem = bool
   where type BoolArray2.elem = bool
   where type BoolArraySlice.elem = bool
   where type BoolVector.elem = bool
   where type BoolVectorSlice.elem = bool

   (* Top-level types.  These appear free in basis signatures and hence must be
    * the same in the basis as at the top level.
    *)
   where type 'a array = 'a array
   where type 'a option = 'a option
   where type 'a vector = 'a vector
   where type char = char
   where type exn = exn
   where type int = int
   where type order = order
   where type real = real
   where type string = string
   where type substring = substring
   where type unit = unit
   where type word = word

   (* Types referenced in signatures by structure name *)
(*
   where type 'a Array.array = 'a Array.array
*)
   where type Array2.traversal = Array2.traversal
   where type 'a ArraySlice.slice = 'a ArraySlice.slice
   where type BinIO.instream = BinIO.instream
   where type BinIO.outstream = BinIO.outstream
   where type BinPrimIO.reader = BinPrimIO.reader
   where type BinPrimIO.writer = BinPrimIO.writer
   where type FixedInt.int = FixedInt.int
   where type IO.buffer_mode = IO.buffer_mode
   where type LargeInt.int = LargeInt.int
   where type LargeReal.real = LargeReal.real
   where type LargeWord.word = LargeWord.word
   where type IEEEReal.real_order = IEEEReal.real_order
   where type IEEEReal.float_class = IEEEReal.float_class
   where type IEEEReal.rounding_mode = IEEEReal.rounding_mode
   where type NetHostDB.in_addr = NetHostDB.in_addr
   where type NetHostDB.addr_family = NetHostDB.addr_family
   where type OS.IO.iodesc = OS.IO.iodesc
   where type OS.Process.status = OS.Process.status (* UNIX *)
   where type Position.int = Position.int
   where type Posix.IO.file_desc = Posix.IO.file_desc
   where type Posix.Signal.signal = Posix.Signal.signal
   where type Socket.dgram = Socket.dgram
   where type ('a, 'b) Socket.sock = ('a, 'b) Socket.sock
   where type 'a Socket.sock_addr = 'a Socket.sock_addr
   where type Socket.SOCK.sock_type = Socket.SOCK.sock_type (* GENERIC_SOCK *)
   where type 'a Socket.stream = 'a Socket.stream
   where type StringCvt.radix = StringCvt.radix
   where type StringCvt.realfmt = StringCvt.realfmt
(*
   where type ('a, 'b) StringCvt.reader = ('a, 'b) StringCvt.reader
*)
   where type SysWord.word = SysWord.word
   where type TextIO.instream = TextIO.instream
   where type TextIO.outstream = TextIO.outstream
   where type TextPrimIO.reader = TextPrimIO.reader
   where type TextPrimIO.writer = TextPrimIO.writer
   where type Time.time = Time.time
(*
   where type 'a Vector.vector = 'a Vector.vector
*)
   where type 'a VectorSlice.slice = 'a VectorSlice.slice
(*
   where type WideTextIO.instream = WideTextIO.instream
   where type WideTextIO.outstream = WideTextIO.outstream
   where type WideTextPrimIO.reader = WideTextPrimIO.reader
   where type WideTextPrimIO.writer = WideTextPrimIO.writer
*)
   where type Word8Array.array = Word8Array.array
   where type Word8ArraySlice.slice = Word8ArraySlice.slice
   where type Word8ArraySlice.vector_slice = Word8ArraySlice.vector_slice
   where type Word8Vector.vector = Word8Vector.vector
   
   where type 'a MLton.Thread.t = 'a MLton.Thread.t
   where type MLton.Thread.Runnable.t = MLton.Thread.Runnable.t

   (* Types that must be exposed because constants denote them. *)
   where type Int1.int = Int1.int
   where type Int2.int = Int2.int
   where type Int3.int = Int3.int
   where type Int4.int = Int4.int
   where type Int5.int = Int5.int
   where type Int6.int = Int6.int
   where type Int7.int = Int7.int
   where type Int8.int = Int8.int
   where type Int9.int = Int9.int
   where type Int10.int = Int10.int
   where type Int11.int = Int11.int
   where type Int12.int = Int12.int
   where type Int13.int = Int13.int
   where type Int14.int = Int14.int
   where type Int15.int = Int15.int
   where type Int16.int = Int16.int
   where type Int17.int = Int17.int
   where type Int18.int = Int18.int
   where type Int19.int = Int19.int
   where type Int20.int = Int20.int
   where type Int21.int = Int21.int
   where type Int22.int = Int22.int
   where type Int23.int = Int23.int
   where type Int24.int = Int24.int
   where type Int25.int = Int25.int
   where type Int26.int = Int26.int
   where type Int27.int = Int27.int
   where type Int28.int = Int28.int
   where type Int29.int = Int29.int
   where type Int30.int = Int30.int
   where type Int31.int = Int31.int
   where type Int32.int = Int32.int
   where type Int64.int = Int64.int
   where type IntInf.int = IntInf.int
   where type Real32.real = Real32.real
   where type Real64.real = Real64.real
   where type WideChar.char = WideChar.char
   where type WideString.string = WideString.string
   where type Word1.word = Word1.word
   where type Word2.word = Word2.word
   where type Word3.word = Word3.word
   where type Word4.word = Word4.word
   where type Word5.word = Word5.word
   where type Word6.word = Word6.word
   where type Word7.word = Word7.word
   where type Word8.word = Word8.word
   where type Word9.word = Word9.word
   where type Word10.word = Word10.word
   where type Word11.word = Word11.word
   where type Word12.word = Word12.word
   where type Word13.word = Word13.word
   where type Word14.word = Word14.word
   where type Word15.word = Word15.word
   where type Word16.word = Word16.word
   where type Word17.word = Word17.word
   where type Word18.word = Word18.word
   where type Word19.word = Word19.word
   where type Word20.word = Word20.word
   where type Word21.word = Word21.word
   where type Word22.word = Word22.word
   where type Word23.word = Word23.word
   where type Word24.word = Word24.word
   where type Word25.word = Word25.word
   where type Word26.word = Word26.word
   where type Word27.word = Word27.word
   where type Word28.word = Word28.word
   where type Word29.word = Word29.word
   where type Word30.word = Word30.word
   where type Word31.word = Word31.word
   where type Word32.word = Word32.word
   where type Word64.word = Word64.word
