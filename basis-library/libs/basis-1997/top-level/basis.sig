signature BASIS_1997 =
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
      datatype ref = datatype ref
      datatype bool = datatype bool
      datatype 'a option = NONE | SOME of 'a
      datatype order = LESS | EQUAL | GREATER 
      datatype list = datatype list

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

      val = : ''a * ''a -> bool
      val <> : ''a * ''a -> bool

      (* Required structures *)
      structure Array : ARRAY_1997
      structure BinIO : BIN_IO_1997
(*
      structure BinPrimIO : PRIM_IO
*)
      structure Bool : BOOL
      structure Byte : BYTE
      structure Char : CHAR
      structure CharArray : MONO_ARRAY_1997
      structure CharVector : MONO_VECTOR_1997
      structure CommandLine : COMMAND_LINE
      structure Date : DATE
      structure General : GENERAL
      structure IEEEReal : IEEE_REAL_1997
      structure Int : INTEGER
      structure IO : IO_1997
      structure LargeInt : INTEGER
      structure LargeReal : REAL_1997
      structure LargeWord : WORD_1997
      structure List : LIST
      structure ListPair : LIST_PAIR
      structure Math : MATH
      structure Option : OPTION
      structure OS : OS_1997
(*
      structure OS.FileSys : OS_FILE_SYS_1997
      structure OS.Path : OS_PATH_1997
      structure OS.Process : OS_PROCESS_1997
      structure OS.IO : OS_IO
*)
      structure Position : INTEGER
      structure Real : REAL_1997
      structure SML90 : SML90
      structure String : STRING_1997
      structure StringCvt : STRING_CVT
      structure Substring : SUBSTRING_1997
      structure TextIO : TEXT_IO_1997
(*
      structure TextPrimIO : PRIM_IO
*)
      structure Time : TIME
      structure Timer : TIMER_1997
      structure Vector : VECTOR_1997
      structure Word : WORD_1997
      structure Word8 : WORD_1997
      structure Word8Array : MONO_ARRAY_1997
      structure Word8Vector : MONO_VECTOR_1997

      (* Optional structures *)
      structure Array2 : ARRAY2
      structure BoolArray : MONO_ARRAY_1997
      structure BoolArray2 : MONO_ARRAY2_1997
      structure BoolVector : MONO_VECTOR_1997
      structure CharArray2 : MONO_ARRAY2_1997
      structure FixedInt : INTEGER
      structure IntInf : INT_INF
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
      structure IntArray : MONO_ARRAY_1997
      structure Int32Array : MONO_ARRAY_1997
      structure IntArray2 : MONO_ARRAY2_1997
      structure Int32Array2 : MONO_ARRAY2_1997
      structure IntVector : MONO_VECTOR_1997
      structure Int32Vector : MONO_VECTOR_1997
(*
      structure Locale : LOCALE
      structure MultiByte : MULTIBYTE
*)
(*
      structure PackReal64Big : PACK_REAL
*)
      structure PackReal64Little : PACK_REAL
(*
      structure PackRealBig : PACK_REAL
*)
      structure PackRealLittle : PACK_REAL
      structure Pack32Big : PACK_WORD
      structure Pack32Little : PACK_WORD

      structure Posix : POSIX_1997
(*
      structure Posix.Error : POSIX_ERROR
      structure Posix.Signal : POSIX_SIGNAL
      structure Posix.Process : POSIX_PROCESS_1997
      structure Posix.ProcEnv : POSIX_PROC_ENV
      structure Posix.FileSys : POSIX_FILE_SYS_1997
      structure Posix.IO : POSIX_IO_1997
      structure Posix.SysDB : POSIX_SYS_DB
      structure Posix.TTY : POSIX_TTY_1997
*)
      structure RealArray : MONO_ARRAY_1997
      structure RealVector : MONO_VECTOR_1997
      structure Real64 : REAL_1997
      structure Real64Array : MONO_ARRAY_1997
      structure Real64Vector : MONO_VECTOR_1997
      structure RealArray2 : MONO_ARRAY2_1997
      structure Real64Array2 : MONO_ARRAY2_1997
      structure SysWord : WORD_1997
(*
      structure WideChar : CHAR
      structure WideCharArray : MONO_ARRAY_1997
      structure WideCharArray2 : MONO_ARRAY2_1997
      structure WideCharVector : MONO_VECTOR_1997
      structure WideString : STRING
      structure WideSubstring : SUBSTRING
      structure WideTextPrimIO : PRIM_IO
      structure WideTextIO : TEXT_IO
*)
      structure Word1: WORD_1997
      structure Word2: WORD_1997
      structure Word3: WORD_1997
      structure Word4: WORD_1997
      structure Word5: WORD_1997
      structure Word6: WORD_1997
      structure Word7: WORD_1997
      structure Word9: WORD_1997
      structure Word10: WORD_1997
      structure Word11: WORD_1997
      structure Word12: WORD_1997
      structure Word13: WORD_1997
      structure Word14: WORD_1997
      structure Word15: WORD_1997
      structure Word16: WORD_1997
      structure Word17: WORD_1997
      structure Word18: WORD_1997
      structure Word19: WORD_1997
      structure Word20: WORD_1997
      structure Word21: WORD_1997
      structure Word22: WORD_1997
      structure Word23: WORD_1997
      structure Word24: WORD_1997
      structure Word25: WORD_1997
      structure Word26: WORD_1997
      structure Word27: WORD_1997
      structure Word28: WORD_1997
      structure Word29: WORD_1997
      structure Word30: WORD_1997
      structure Word31: WORD_1997
      structure Word32: WORD_1997
      structure Word64: WORD_1997
      structure Word8Array2 : MONO_ARRAY2_1997
      structure Unix : UNIX_1997
   end
