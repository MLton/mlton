signature BASIS =
   sig
      (* Top-level types *)
      eqtype unit
      eqtype int 
      eqtype word 
      type real
      eqtype char
      eqtype string
      type substring 
      type exn
      eqtype 'a array 
      eqtype 'a vector
(*
      eqtype 'a ref
*)
      datatype 'a ref = ref of 'a
      datatype bool = false | true
      datatype 'a option = NONE | SOME of 'a 
      datatype order = LESS | EQUAL | GREATER 
      datatype 'a list = nil | :: of ('a * 'a list)

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

      val <> : ''a * ''a -> bool
	
      (* Required structures *)
      structure Array : ARRAY	
      structure ArraySlice : ARRAY_SLICE	
      structure BinIO : BIN_IO	
      structure BinPrimIO : PRIM_IO	
      structure Bool : BOOL	
      structure Byte : BYTE	
      structure CharArray : MONO_ARRAY	
      structure CharArraySlice : MONO_ARRAY_SLICE	
      structure Char : CHAR	
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
      structure Word8Array2 : MONO_ARRAY2	
      structure Word8Array : MONO_ARRAY	
      structure Word8ArraySlice : MONO_ARRAY_SLICE	
      structure Word8Vector : MONO_VECTOR	
      structure Word8VectorSlice : MONO_VECTOR_SLICE	
      structure Word8 : WORD	
      structure Word : WORD	

      (* Optional structures *)
      structure Array2 : ARRAY2
      structure BoolArray : MONO_ARRAY
      structure BoolArray2 : MONO_ARRAY2
      structure BoolArraySlice : MONO_ARRAY_SLICE
      structure BoolVector : MONO_VECTOR
      structure BoolVectorSlice : MONO_VECTOR_SLICE
      structure CharArray2 : MONO_ARRAY2
      structure FixedInt : INTEGER
(*
      structure GenericSock : GENERIC_SOCK
      structure INetSock : INET_SOCK
*)
      structure IntArray : MONO_ARRAY
      structure IntArray2 : MONO_ARRAY2
      structure IntArraySlice : MONO_ARRAY_SLICE
      structure IntVector : MONO_VECTOR
      structure IntVectorSlice : MONO_VECTOR_SLICE
      structure Int32Array : MONO_ARRAY
      structure Int32Array2 : MONO_ARRAY2
      structure Int32ArraySlice : MONO_ARRAY_SLICE
      structure Int32 : INTEGER
      structure Int32Vector : MONO_VECTOR
      structure Int32VectorSlice : MONO_VECTOR_SLICE
      structure IntInf : INT_INF
(*
      structure NetHostDB : NET_HOST_DB
      structure NetProtDB : NET_PROT_DB
      structure NetServDB : NET_SERV_DB
*)
      structure Pack32Big : PACK_WORD
      structure Pack32Little : PACK_WORD
(*
      structure PackRealBig : PACK_REAL
*)
      structure PackRealLittle : PACK_REAL
(*
      structure PackReal64Big : PACK_REAL
*)
      structure PackReal64Little : PACK_REAL
      structure Posix : POSIX
      structure RealArray2 : MONO_ARRAY2
      structure RealArray : MONO_ARRAY
      structure RealArraySlice : MONO_ARRAY_SLICE
      structure RealVector : MONO_VECTOR
      structure RealVectorSlice : MONO_VECTOR_SLICE
      structure Real64Array : MONO_ARRAY
      structure Real64Array2 : MONO_ARRAY2
      structure Real64ArraySlice : MONO_ARRAY_SLICE
      structure Real64 : REAL
      structure Real64Vector : MONO_VECTOR
      structure Real64VectorSlice : MONO_VECTOR_SLICE
(*
      structure Socket : SOCKET
*)
      structure SysWord : WORD
(*
      structure UnixSock : UNIX_SOCK
*)
      structure Unix : UNIX
(*
      structure WideCharArray : MONO_ARRAY
      structure WideCharArray2 : MONO_ARRAY2
      structure WideCharArraySlice : MONO_ARRAY_SLICE
      structure WideChar : CHAR
      structure WideCharVector : MONO_VECTOR
      structure WideCharVectorSlice : MONO_VECTOR_SLICE
      structure WideString : STRING
      structure WideSubstring : SUBSTRING
      structure WideTextPrimIO : PRIM_IO
      structure WideText : TEXT
*)
(*
      structure Windows : WINDOWS
*)
      structure Word32Array : MONO_ARRAY
      structure Word32Array2 : MONO_ARRAY2
      structure Word32ArraySlice : MONO_ARRAY_SLICE
      structure Word32Vector : MONO_VECTOR
      structure Word32VectorSlice : MONO_VECTOR_SLICE
      structure Word32 : WORD

      (* Non-standard structures *)
      structure MLton : MLTON
      structure SMLofNJ : SML_OF_NJ
      structure Unsafe : UNSAFE
      structure SML90 : SML90

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
      sharing type array = Array.array
      sharing type vector = Vector.vector
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
      sharing type Array.vector = Vector.vector
      sharing type BinPrimIO.array = Word8Array.array
      sharing type BinPrimIO.vector = Word8Vector.vector
      sharing type BinPrimIO.elem = Word8.word
      sharing type BinPrimIO.pos = Position.int
      sharing type Char.string = string
      sharing type CharArray.vector = CharVector.vector
      sharing type CharArray.elem = char
      sharing type CharArraySlice.vector = CharVector.vector
      sharing type CharArraySlice.vector_slice = CharVectorSlice.slice
      sharing type CharArraySlice.array = CharArray.array
      sharing type CharArraySlice.elem = char
      sharing type CharVector.vector = String.string
      sharing type CharVector.elem = char
      sharing type CharVectorSlice.slice = Substring.substring
      sharing type CharVectorSlice.vector = String.string
      sharing type CharVectorSlice.elem = char
      sharing type Math.real = Real.real
      sharing type String.string = CharVector.vector
      sharing type String.char = char
      sharing type Substring.substring = CharVectorSlice.slice
      sharing type Substring.string = String.string
      sharing type Substring.char = char
(*
      sharing type Text.Char.char = char
      sharing type Text.String.string = string
*)
      sharing type TextPrimIO.array = CharArray.array
      sharing type TextPrimIO.vector = CharVector.vector
      sharing type TextPrimIO.elem = Char.char
      sharing type Word8Array2.vector = Word8Vector.vector
      sharing type Word8Array2.elem = Word8.word
      sharing type Word8Array.vector = Word8Vector.vector
      sharing type Word8Array.elem = Word8.word
      sharing type Word8ArraySlice.vector = Word8Vector.vector
      sharing type Word8ArraySlice.vector_slice = Word8VectorSlice.slice
      sharing type Word8ArraySlice.array = Word8Array.array
      sharing type Word8ArraySlice.elem = Word8.word
      sharing type Word8Vector.elem = Word8.word
      sharing type Word8VectorSlice.vector = Word8Vector.vector
      sharing type Word8VectorSlice.elem = Word8.word
	
      (* Optional structures *)
      sharing type BoolArray.vector = BoolVector.vector
      sharing type BoolArray.elem = bool
      sharing type BoolArray2.vector = BoolVector.vector
      sharing type BoolArray2.elem = bool
      sharing type BoolArraySlice.vector = BoolVector.vector
      sharing type BoolArraySlice.vector_slice = BoolVectorSlice.slice
      sharing type BoolArraySlice.array = BoolArray.array
      sharing type BoolArraySlice.elem = bool
      sharing type BoolVector.elem = bool
      sharing type BoolVectorSlice.vector = BoolVector.vector
      sharing type BoolVectorSlice.elem = bool
      sharing type CharArray2.vector = CharVector.vector
      sharing type CharArray2.elem = char
      sharing type IntArray.vector = IntVector.vector
      sharing type IntArray.elem = int
      sharing type IntArray2.vector = IntVector.vector
      sharing type IntArray2.elem = int
      sharing type IntArraySlice.vector = IntVector.vector
      sharing type IntArraySlice.vector_slice = IntVectorSlice.slice
      sharing type IntArraySlice.array = IntArray.array
      sharing type IntArraySlice.elem = int
      sharing type IntVector.elem = int
      sharing type IntVectorSlice.vector = IntVector.vector
      sharing type IntVectorSlice.elem = int
      sharing type Int32Array.vector = Int32Vector.vector
      sharing type Int32Array.elem = Int32.int
      sharing type Int32Array2.vector = Int32Vector.vector
      sharing type Int32Array2.elem = Int32.int
      sharing type Int32ArraySlice.vector = Int32Vector.vector
      sharing type Int32ArraySlice.vector_slice = Int32VectorSlice.slice
      sharing type Int32ArraySlice.array = Int32Array.array
      sharing type Int32ArraySlice.elem = Int32.int
      sharing type Int32Vector.elem = Int32.int
      sharing type Int32VectorSlice.vector = Int32Vector.vector
      sharing type Int32VectorSlice.elem = Int32.int
(*
      sharing type PackRealBig.real = real
*)
      sharing type PackRealLittle.real = real
(*
      sharing type PackReal64Big.real = Real64.real
*)
      sharing type PackReal64Little.real = Real64.real
      sharing type Posix.Error.syserror = OS.syserror
      sharing type Posix.Process.exit_status = Unix.exit_status
      sharing type Posix.FileSys.dirstream = OS.FileSys.dirstream
      sharing type Posix.FileSys.access_mode = OS.FileSys.access_mode
      sharing type RealArray.vector = RealVector.vector
      sharing type RealArray.elem = real
      sharing type RealArray2.vector = RealVector.vector
      sharing type RealArray2.elem = real
      sharing type RealArraySlice.vector = RealVector.vector
      sharing type RealArraySlice.vector_slice = RealVectorSlice.slice
      sharing type RealArraySlice.array = RealArray.array
      sharing type RealArraySlice.elem = real
      sharing type RealVector.elem = real
      sharing type RealVectorSlice.vector = RealVector.vector
      sharing type RealVectorSlice.elem = real
      sharing type Real64Array.vector = Real64Vector.vector
      sharing type Real64Array.elem = Real64.real
      sharing type Real64Array2.vector = Real64Vector.vector
      sharing type Real64Array2.elem = Real64.real
      sharing type Real64ArraySlice.vector = Real64Vector.vector
      sharing type Real64ArraySlice.vector_slice = Real64VectorSlice.slice
      sharing type Real64ArraySlice.array = Real64Array.array
      sharing type Real64ArraySlice.elem = Real64.real
      sharing type Real64Vector.elem = Real64.real
      sharing type Real64VectorSlice.vector = Real64Vector.vector
      sharing type Real64VectorSlice.elem = Real64.real
      sharing type Unix.exit_status = Posix.Process.exit_status
      sharing type Word32Array.vector = Word32Vector.vector
      sharing type Word32Array.elem = Word32.word
      sharing type Word32Array2.vector = Word32Vector.vector
      sharing type Word32Array2.elem = Word32.word
      sharing type Word32ArraySlice.vector = Word32Vector.vector
      sharing type Word32ArraySlice.vector_slice = Word32VectorSlice.slice
      sharing type Word32ArraySlice.array = Word32Array.array
      sharing type Word32ArraySlice.elem = Word32.word
      sharing type Word32Vector.elem = Word32.word
      sharing type Word32VectorSlice.vector = Word32Vector.vector
      sharing type Word32VectorSlice.elem = Word32.word
   end
   (* Top-level types *)
   where type unit = unit
   where type int = int
   where type word = word
   where type real = real
   where type char = char
   where type exn = exn
   where type 'a array = 'a array
   where type 'a vector = 'a vector
   where type 'a ref = 'a ref
   where type bool = bool
   where type 'a option = 'a option
   where type order = order
   where type 'a list = 'a list

   (* Types referenced in signatures by structure name *)
   where type BinPrimIO.reader = BinPrimIO.reader
   where type BinPrimIO.writer = BinPrimIO.writer
   where type Char.char = Char.char
   where type Int.int = Int.int
   where type LargeInt.int = LargeInt.int
   where type LargeReal.real = LargeReal.real
   where type LargeWord.word = LargeWord.word
   where type IEEEReal.real_order = IEEEReal.real_order
   where type IEEEReal.float_class = IEEEReal.float_class
   where type IEEEReal.rounding_mode = IEEEReal.rounding_mode
   where type OS.IO.iodesc = OS.IO.iodesc
   where type OS.Process.status = OS.Process.status
   where type Position.int = Position.int
   where type Posix.Process.pid = Posix.Process.pid
   where type StringCvt.radix = StringCvt.radix
   where type StringCvt.realfmt = StringCvt.realfmt
(*
   where type ('a, 'b) StringCvt.reader = ('a, 'b) StringCvt.reader
*)
   where type SysWord.word = SysWord.word
   where type TextPrimIO.reader = TextPrimIO.reader
   where type TextPrimIO.writer = TextPrimIO.writer
   where type Time.time = Time.time
   where type Word.word = Word.word
   where type Word8.word = Word8.word
   where type Word8Array.array = Word8Array.array
   where type Word8Vector.vector = Word8Vector.vector
