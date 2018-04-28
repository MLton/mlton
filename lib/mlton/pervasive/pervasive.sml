(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature PERVASIVE_REAL = REAL
signature PERVASIVE_WORD = WORD
structure Pervasive =
   struct
      structure Array = Array
      structure Array2 = Array2
      structure Bool = Bool
      structure Byte = Byte
      structure Char = Char
      structure CharArray = CharArray
      structure CharVector = CharVector
      structure CommandLine = CommandLine
      structure Date = Date
      structure General = General
      structure IEEEReal = IEEEReal
      structure Int = Int
      structure IntInf = IntInf
      structure LargeInt = LargeInt
      structure LargeReal = LargeReal
      structure LargeWord = LargeWord
      structure ListPair = ListPair
      structure List = List
      structure Math = Math
      structure Option = Option
      structure OS = OS
      structure PackReal32Little = PackReal32Little
      structure PackReal64Little = PackReal64Little
      structure PackWord32Little = PackWord32Little
      structure PackWord64Little = PackWord64Little
      structure Position = Position
      structure Posix = Posix
      structure Real = Real
      structure Real32 = Real32
      structure Real64 = Real64
      structure Socket = Socket
      structure String = String
      structure StringCvt = StringCvt
      structure Substring = Substring
      structure SysWord = SysWord
      structure TextIO = TextIO
      structure Time = Time
      structure Unix = Unix
      structure Vector = Vector
      structure Word = Word
      structure Word8 = Word8
      structure Word8Array = Word8Array
      structure Word8Vector = Word8Vector
      structure Word32 = Word32
      structure Word64 = Word64

      type unit = General.unit
      type real = Real.real
      type char = Char.char
      type string = String.string
      type substring = Substring.substring
      type exn = General.exn
      type 'a array = 'a Array.array
      type 'a vector = 'a Vector.vector
      type 'a ref = 'a ref
      datatype bool = datatype bool
      datatype option = datatype option
      datatype order = datatype General.order
      datatype list = datatype list

      val ! = General.! 
      val op := = General.:= 
      val op @ = List.@ 
      val op ^ = String.^ 
      val app = List.app 
      val op before = General.before 
      val ceil = Real.ceil 
      val chr = Char.chr 
      val concat = String.concat 
      val exnMessage = General.exnMessage 
      val exnName = General.exnName 
      val explode = String.explode 
      val floor = Real.floor 
      val foldl = List.foldl 
      val foldr = List.foldr 
      val getOpt = Option.getOpt 
      val hd = List.hd 
      val ignore = General.ignore 
      val implode = String.implode 
      val isSome = Option.isSome 
      val length = List.length 
      val map = List.map 
      val not = Bool.not 
      val null = List.null 
      val op o = General.o 
      val ord = Char.ord 
      val print = TextIO.print 
      val real = Real.fromInt 
      val rev = List.rev 
      val round = Real.round 
      val size = String.size 
      val str = String.str 
      val substring = String.substring 
      val tl = List.tl 
      val trunc = Real.trunc 
      val valOf = Option.valOf 
      val vector = Vector.fromList
   end
