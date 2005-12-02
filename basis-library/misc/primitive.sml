(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(* Primitive names are special -- see atoms/prim.fun. *)

infix 4 = (* <> > >= < <= *)

val op = = fn z => _prim "MLton_equal": ''a * ''a -> bool; z

structure Array =
   struct
      type 'a array = 'a array
   end

type 'a array = 'a Array.array

structure Bool =
   struct
      datatype bool = datatype bool
   end

structure Char =
   struct
      type t = char8
      type char = t
   end
type char = Char.char
structure Char2 =
   struct
      type t = char16
      type char = t
   end
structure Char4 =
   struct
      type t = char32
      type char = t
   end

type exn = exn

structure Int8 =
   struct
      type t = int8
      type int = t
   end
structure Int16 =
   struct
      type t = int16
      type int = t
   end
structure Int32 =
   struct
      type t = int32
      type int = t
   end
structure Int = Int32
type int = Int.int
structure Int64 =
   struct
      type t = int64
      type int = t
   end
structure Position = Int64
structure IntInf =
   struct
      type t = intInf
      type int = t
   end
(*structure LargeInt = IntInf*)

structure Real32 =
   struct
      type t = real32
      type real = t
   end
structure Real64 =
   struct
      type t = real64
      type real = t
   end
structure Real = Real64
type real = Real.real

structure String =
   struct
      type t = char vector
      type string = t
   end
type string = String.string
structure String2 =
   struct
      type t = Char2.t vector
      type string = t
   end
structure String4 =
   struct
      type t = Char4.t vector
      type string = t
   end

structure PreThread :> sig type t end = struct type t = thread end
structure Thread :> sig type t end = struct type t = thread end

structure Word8 =
   struct
      type t = word8
      type word = t
   end
structure Word16 =
   struct
      type t = word16
      type word = t
   end
structure Word32 =
   struct
      type t = word32
      type word = t
   end
structure Word = Word32
type word = Word.word
structure Word64 =
   struct
      type t = word64
      type word = t
   end
structure LargeWord = Word64

type 'a vector = 'a vector
type 'a weak = 'a weak

(* NullString is used for strings that must be passed to C and hence must be
 * null terminated.  After the Primitive structure is defined,
 * NullString.fromString is replaced by a version that checks that the string
 * is indeed null terminated.  See the bottom of this file.
 *)
structure NullString :>
   sig
      type t

      val fromString: string -> t
   end =
   struct
      type t = string

      val fromString = fn s => s
   end

structure Pointer =
   struct
      type t = pointer
   end

structure GetSet =
   struct
      type 'a t = (unit -> 'a) * ('a -> unit)
   end

structure Pid :> sig
                    eqtype t

                    val fromInt: int -> t
                    val toInt: t -> int
                 end =
   struct
      type t = int

      val fromInt = fn i => i
      val toInt = fn i => i
      val _ = fromInt
   end
   
exception Fail of string
exception Match = Match
exception PrimitiveOverflow = Overflow
exception Overflow
exception Size

val wrapOverflow: ('a -> 'b) -> ('a -> 'b) =
   fn f => fn a => f a handle PrimitiveOverflow => raise Overflow

datatype 'a option = NONE | SOME of 'a

fun not b = if b then false else true
   
functor Comparisons (type t
                     val < : t * t -> bool) =
   struct
      fun <= (a, b) = not (< (b, a))
      fun > (a, b) = < (b, a)
      fun >= (a, b) = <= (b, a)
   end

functor RealComparisons (type t
                         val < : t * t -> bool
                         val <= : t * t -> bool) =
   struct
      fun > (a, b) = < (b, a)
      fun >= (a, b) = <= (b, a)
   end

structure Primitive =
   struct
      val bug = _import "MLton_bug": NullString.t -> unit;
      val debug = _command_line_const "MLton.debug": bool = false;
      val detectOverflow =
         _command_line_const "MLton.detectOverflow": bool = true;
      val eq = _prim "MLton_eq": 'a * 'a -> bool;
      (* val errno = _import "MLton_errno": unit -> int; *)
      val installSignalHandler =
         _prim "MLton_installSignalHandler": unit -> unit;
      val safe = _command_line_const "MLton.safe": bool = true;
      val touch = _prim "MLton_touch": 'a -> unit;
      val usesCallcc: bool ref = ref false;

      structure Stdio =
         struct
            val print = _import "Stdio_print": string -> unit;
            (* val sprintf = _import "Stdio_sprintf": char array * nullString * real -> int; *)
         end

      structure Array =
         struct
            val array0Const = _prim "Array_array0Const": unit -> 'a array;
            val length = _prim "Array_length": 'a array -> int;
            (* There is no maximum length on arrays, so maxLen = maxInt. *)
            val maxLen: int = 0x7FFFFFFF
            val sub = _prim "Array_sub": 'a array * int -> 'a;
            val update = _prim "Array_update": 'a array * int * 'a -> unit;
         end

      structure CString =
         struct
            type t = Pointer.t
         end
      structure CStringArray =
         struct
            type t = Pointer.t
         end

      structure GCState =
         struct
            type t = Pointer.t

            val gcState = #1 _symbol "gcStateAddress": t GetSet.t; ()
         end
      
      structure CallStack =
         struct
            (* The most recent caller is at index 0 in the array. *)
            datatype t = T of int array

            val callStack =
               _import "GC_callStack": GCState.t * int array -> unit;
            val frameIndexSourceSeq =
               _import "GC_frameIndexSourceSeq": GCState.t * int -> Pointer.t;
            val keep = _command_line_const "CallStack.keep": bool = false;
            val numStackFrames =
               _import "GC_numStackFrames": GCState.t -> int;
            val sourceName = _import "GC_sourceName": GCState.t * int -> CString.t;
         end

      structure Char =
         struct
            open Char
               
            val op < = _prim "WordU8_lt": char * char -> bool;
            val chr = _prim "WordS32_toWord8": int -> char;
            val ord = _prim "WordU8_toWord32": char -> int;
            val toInt8 = _prim "WordS8_toWord8": char -> Int8.int;
            val fromInt8 = _prim "WordS8_toWord8": Int8.int -> char;
            val toWord8 = _prim "WordU8_toWord8": char -> Word8.word;
            val fromWord8 = _prim "WordU8_toWord8": Word8.word -> char;
         end

      structure Char =
         struct
            open Char
            local
               structure S = Comparisons (Char)
            in
               open S
            end
         end

      structure Char2 =
         struct
            open Char2

            val op < = _prim "WordU16_lt": char * char -> bool;
            val chr = _prim "WordS32_toWord16": int -> char;
            val ord = _prim "WordU16_toWord32": char -> int;
            val toInt16 = _prim "WordS16_toWord16": char -> Int16.int;
            val fromInt16 = _prim "WordS16_toWord16": Int16.int -> char;
            (* val toWord16 = _prim "WordU16_toWord16": char -> Word16.word; *)
            (* val fromWord16 = _prim "WordU16_toWord16": Word16.word -> char; *)
         end
      
      structure Char4 =
         struct
            open Char4

            val op < = _prim "WordU32_lt": char * char -> bool;
            val chr = _prim "WordS32_toWord32": int -> char;
            val ord = _prim "WordU32_toWord32": char -> int;
            val toInt32 = _prim "WordS32_toWord32": char -> Int32.int;
            val fromInt32 = _prim "WordS32_toWord32": Int32.int -> char;
            (* val toWord32 = _prim "WordU32_toWord32": char -> Word32.word; *)
            (* val fromWord32 = _prim "WordU32_toWord32": Word32.word -> char; *)
         end

      structure CommandLine =
         struct
            val argc = #1 _symbol "CommandLine_argc": int GetSet.t;
            val argv = #1 _symbol "CommandLine_argv": CStringArray.t GetSet.t;
            val commandName = #1 _symbol "CommandLine_commandName": CString.t GetSet.t;
         end

      structure Date =
         struct
            type time = int
            type size = int

            structure Tm =
               struct
                  val sec = _import "Date_Tm_sec": unit -> int;
                  val min = _import "Date_Tm_min": unit -> int;
                  val hour = _import "Date_Tm_hour": unit -> int;
                  val mday = _import "Date_Tm_mday": unit -> int;
                  val mon = _import "Date_Tm_mon": unit -> int;
                  val year = _import "Date_Tm_year": unit -> int;
                  val wday = _import "Date_Tm_wday": unit -> int;
                  val yday = _import "Date_Tm_yday": unit -> int;
                  val isdst = _import "Date_Tm_isdst": unit -> int;

                  val setSec = _import "Date_Tm_setSec": int -> unit;
                  val setMin = _import "Date_Tm_setMin": int -> unit;
                  val setHour = _import "Date_Tm_setHour": int -> unit;
                  val setMday = _import "Date_Tm_setMday": int -> unit;
                  val setMon = _import "Date_Tm_setMon": int -> unit;
                  val setYear = _import "Date_Tm_setYear": int -> unit;
                  val setWday = _import "Date_Tm_setWday": int -> unit;
                  val setYday = _import "Date_Tm_setYday": int -> unit;
                  val setIsdst = _import "Date_Tm_setIsdst": int -> unit;
               end
               
            val gmTime = _import "Date_gmTime": time ref -> unit;
            val localOffset = _import "Date_localOffset": unit -> int;
            val localTime = _import "Date_localTime": time ref -> unit;
            val mkTime = _import "Date_mkTime": unit -> time;
            val strfTime =
               _import "Date_strfTime": char array * size * NullString.t -> size;
         end
    
      structure Exn =
         struct
            (* The polymorphism with extra and setInitExtra is because primitives
             * are only supposed to deal with basic types.  The polymorphism
             * allows the various passes like monomorphisation to translate
             * the types appropriately.
             *)
            type extra = CallStack.t option

            val extra = _prim "Exn_extra": exn -> 'a;
            val extra: exn -> extra = extra
            val name = _prim "Exn_name": exn -> string;
            val keepHistory =
               _command_line_const "Exn.keepHistory": bool = false;
            val setExtendExtra = _prim "Exn_setExtendExtra": ('a -> 'a) -> unit;
            val setExtendExtra: (extra -> extra) -> unit = setExtendExtra
            val setInitExtra = _prim "Exn_setInitExtra": 'a -> unit;
            val setInitExtra: extra -> unit = setInitExtra
         end

      structure FFI =
         struct
            val getOp = #1 _symbol "MLton_FFI_op": int GetSet.t;
            val int8Array = #1 _symbol "MLton_FFI_Int8": Pointer.t GetSet.t; ()
            val int16Array = #1 _symbol "MLton_FFI_Int16": Pointer.t GetSet.t; ()
            val int32Array = #1 _symbol "MLton_FFI_Int32": Pointer.t GetSet.t; ()
            val int64Array = #1 _symbol "MLton_FFI_Int64": Pointer.t GetSet.t; ()
            val numExports = _build_const "MLton_FFI_numExports": int;
            val pointerArray = #1 _symbol "MLton_FFI_Pointer": Pointer.t GetSet.t; ()
            val real32Array = #1 _symbol "MLton_FFI_Real32": Pointer.t GetSet.t; ()
            val real64Array = #1 _symbol "MLton_FFI_Real64": Pointer.t GetSet.t; ()
            val word8Array = #1 _symbol "MLton_FFI_Word8": Pointer.t GetSet.t; ()
            val word16Array = #1 _symbol "MLton_FFI_Word16": Pointer.t GetSet.t; ()
            val word32Array = #1 _symbol "MLton_FFI_Word32": Pointer.t GetSet.t; ()
            val word64Array = #1 _symbol "MLton_FFI_Word64": Pointer.t GetSet.t; ()
         end

      structure GC =
         struct
            val collect = _prim "GC_collect": unit -> unit;
            val pack = _import "MLton_GC_pack": unit -> unit;
            val setHashConsDuringGC =
               _import "GC_setHashConsDuringGC": bool -> unit;
            val setMessages = _import "GC_setMessages": bool -> unit;
            val setRusageMeasureGC = _import "GC_setRusageMeasureGC": bool -> unit;
            val setSummary = _import "GC_setSummary": bool -> unit;
            val unpack = _import "MLton_GC_unpack": unit -> unit;
         end
      
      structure IEEEReal =
         struct
            structure RoundingMode =
               struct
                  type t = int
                     
                  val toNearest = _const "FE_TONEAREST": t;
                  val downward = _const "FE_DOWNWARD": t;
                  val noSupport = _const "FE_NOSUPPORT": t;
                  val upward = _const "FE_UPWARD": t;
                  val towardZero = _const "FE_TOWARDZERO": t;
               end
            
            val getRoundingMode =
               _import "IEEEReal_getRoundingMode": unit -> int;
            val setRoundingMode =
               _import "IEEEReal_setRoundingMode": int -> unit;
         end

      structure Int1 =
         struct
            type big = Int8.int
            type int = int1
            val fromBigUnsafe = _prim "WordU8_toWord1": big -> int;
            val precision' = 1
            val toBig = _prim "WordU1_toWord8": int -> big;
         end
      structure Int2 =
         struct
            type big = Int8.int
            type int = int2
            val fromBigUnsafe = _prim "WordU8_toWord2": big -> int;
            val precision' = 2
            val toBig = _prim "WordU2_toWord8": int -> big;
         end
      structure Int3 =
         struct
            type big = Int8.int
            type int = int3
            val fromBigUnsafe = _prim "WordU8_toWord3": big -> int;
            val precision' = 3
            val toBig = _prim "WordU3_toWord8": int -> big;
         end
      structure Int4 =
         struct
            type big = Int8.int
            type int = int4
            val fromBigUnsafe = _prim "WordU8_toWord4": big -> int;
            val precision' = 4
            val toBig = _prim "WordU4_toWord8": int -> big;
         end
      structure Int5 =
         struct
            type big = Int8.int
            type int = int5
            val fromBigUnsafe = _prim "WordU8_toWord5": big -> int;
            val precision' = 5
            val toBig = _prim "WordU5_toWord8": int -> big;
         end
      structure Int6 =
         struct
            type big = Int8.int
            type int = int6
            val fromBigUnsafe = _prim "WordU8_toWord6": big -> int;
            val precision' = 6
            val toBig = _prim "WordU6_toWord8": int -> big;
         end
      structure Int7 =
         struct
            type big = Int8.int
            type int = int7
            val fromBigUnsafe = _prim "WordU8_toWord7": big -> int;
            val precision' = 7
            val toBig = _prim "WordU7_toWord8": int -> big;
         end
      structure Int8 =
         struct
            type t = Int8.int
            type int = t
               
            val precision' : Int.int = 8
            val maxInt' : int = 0x7f
            val minInt' : int = ~0x80

            val *? = _prim "WordS8_mul": int * int -> int;
            val * =
               if detectOverflow
                  then wrapOverflow (_prim "WordS8_mulCheck": int * int -> int;)
               else *?
            val +? = _prim "Word8_add": int * int -> int;
            val + =
               if detectOverflow
                  then wrapOverflow (_prim "WordS8_addCheck": int * int -> int;)
               else +?
            val -? = _prim "Word8_sub": int * int -> int;
            val - =
               if detectOverflow
                  then wrapOverflow (_prim "WordS8_subCheck": int * int -> int;)
               else -?
            val op < = _prim "WordS8_lt": int * int -> bool;
            val quot = _prim "WordS8_quot": int * int -> int;
            val rem = _prim "WordS8_rem": int * int -> int;
            val << = _prim "Word8_lshift": int * Word.word -> int;
            val >> = _prim "WordU8_rshift": int * Word.word -> int;
            val ~>> = _prim "WordS8_rshift": int * Word.word -> int;
            val ~? = _prim "Word8_neg": int -> int; 
            val ~ =
               if detectOverflow
                  then wrapOverflow (_prim "Word8_negCheck": int -> int;)
               else ~?
            val andb = _prim "Word8_andb": int * int -> int;
            val fromInt = _prim "WordS32_toWord8": Int.int -> int;
            val toInt = _prim "WordS8_toWord32": int -> Int.int;
         end
      structure Int8 =
         struct
            open Int8
            local
               structure S = Comparisons (Int8)
            in
               open S
            end
         end
      structure Int9 =
         struct
            type big = Int16.int
            type int = int9
            val fromBigUnsafe = _prim "WordU16_toWord9": big -> int;
            val precision' = 9
            val toBig = _prim "WordU9_toWord16": int -> big;
         end
      structure Int10 =
         struct
            type big = Int16.int
            type int = int10
            val fromBigUnsafe = _prim "WordU16_toWord10": big -> int;
            val precision' = 10
            val toBig = _prim "WordU10_toWord16": int -> big;
         end
      structure Int11 =
         struct
            type big = Int16.int
            type int = int11
            val fromBigUnsafe = _prim "WordU16_toWord11": big -> int;
            val precision' = 11
            val toBig = _prim "WordU11_toWord16": int -> big;
         end
      structure Int12 =
         struct
            type big = Int16.int
            type int = int12
            val fromBigUnsafe = _prim "WordU16_toWord12": big -> int;
            val precision' = 12
            val toBig = _prim "WordU12_toWord16": int -> big;
         end
      structure Int13 =
         struct
            type big = Int16.int
            type int = int13
            val fromBigUnsafe = _prim "WordU16_toWord13": big -> int;
            val precision' = 13
            val toBig = _prim "WordU13_toWord16": int -> big;
         end
      structure Int14 =
         struct
            type big = Int16.int
            type int = int14
            val fromBigUnsafe = _prim "WordU16_toWord14": big -> int;
            val precision' = 14
            val toBig = _prim "WordU14_toWord16": int -> big;
         end
      structure Int15 =
         struct
            type big = Int16.int
            type int = int15
            val fromBigUnsafe = _prim "WordU16_toWord15": big -> int;
            val precision' = 15
            val toBig = _prim "WordU15_toWord16": int -> big;
         end    
      structure Int16 =
         struct
            type t = Int16.int
            type int = t
               
            val precision' : Int.int = 16
            val maxInt' : int = 0x7fff
            val minInt' : int = ~0x8000

            val *? = _prim "WordS16_mul": int * int -> int;
            val * =
               if detectOverflow
                  then (wrapOverflow
                        (_prim "WordS16_mulCheck": int * int -> int;))
               else *?
            val +? = _prim "Word16_add": int * int -> int;
            val + =
               if detectOverflow
                  then (wrapOverflow
                        (_prim "WordS16_addCheck": int * int -> int;))
               else +?
            val -? = _prim "Word16_sub": int * int -> int;
            val - =
               if detectOverflow
                  then (wrapOverflow
                        (_prim "WordS16_subCheck": int * int -> int;))
               else -?
            val op < = _prim "WordS16_lt": int * int -> bool;
            val quot = _prim "WordS16_quot": int * int -> int;
            val rem = _prim "WordS16_rem": int * int -> int;
            val << = _prim "Word16_lshift": int * Word.word -> int;
            val >> = _prim "WordU16_rshift": int * Word.word -> int;
            val ~>> = _prim "WordS16_rshift": int * Word.word -> int;
            val ~? = _prim "Word16_neg": int -> int; 
            val ~ =
               if detectOverflow
                  then wrapOverflow (_prim "Word16_negCheck": int -> int;)
               else ~?
            val andb = _prim "Word16_andb": int * int -> int;
            val fromInt = _prim "WordS32_toWord16": Int.int -> int;
            val toInt = _prim "WordS16_toWord32": int -> Int.int;
         end
      structure Int16 =
         struct
            open Int16
            local
               structure S = Comparisons (Int16)
            in
               open S
            end
         end
      structure Int17 =
         struct
            type big = Int32.int
            type int = int17
            val fromBigUnsafe = _prim "WordU32_toWord17": big -> int;
            val precision' = 17
            val toBig = _prim "WordU17_toWord32": int -> big;
         end
      structure Int18 =
         struct
            type big = Int32.int
            type int = int18
            val fromBigUnsafe = _prim "WordU32_toWord18": big -> int;
            val precision' = 18
            val toBig = _prim "WordU18_toWord32": int -> big;
         end
      structure Int19 =
         struct
            type big = Int32.int
            type int = int19
            val fromBigUnsafe = _prim "WordU32_toWord19": big -> int;
            val precision' = 19
            val toBig = _prim "WordU19_toWord32": int -> big;
         end
      structure Int20 =
         struct
            type big = Int32.int
            type int = int20
            val fromBigUnsafe = _prim "WordU32_toWord20": big -> int;
            val precision' = 20
            val toBig = _prim "WordU20_toWord32": int -> big;
         end
      structure Int21 =
         struct
            type big = Int32.int
            type int = int21
            val fromBigUnsafe = _prim "WordU32_toWord21": big -> int;
            val precision' = 21
            val toBig = _prim "WordU21_toWord32": int -> big;
         end
      structure Int22 =
         struct
            type big = Int32.int
            type int = int22
            val fromBigUnsafe = _prim "WordU32_toWord22": big -> int;
            val precision' = 22
            val toBig = _prim "WordU22_toWord32": int -> big;
         end
      structure Int23 =
         struct
            type big = Int32.int
            type int = int23
            val fromBigUnsafe = _prim "WordU32_toWord23": big -> int;
            val precision' = 23
            val toBig = _prim "WordU23_toWord32": int -> big;
         end
      structure Int24 =
         struct
            type big = Int32.int
            type int = int24
            val fromBigUnsafe = _prim "WordU32_toWord24": big -> int;
            val precision' = 24
            val toBig = _prim "WordU24_toWord32": int -> big;
         end
      structure Int25 =
         struct
            type big = Int32.int
            type int = int25
            val fromBigUnsafe = _prim "WordU32_toWord25": big -> int;
            val precision' = 25
            val toBig = _prim "WordU25_toWord32": int -> big;
         end
      structure Int26 =
         struct
            type big = Int32.int
            type int = int26
            val fromBigUnsafe = _prim "WordU32_toWord26": big -> int;
            val precision' = 26
            val toBig = _prim "WordU26_toWord32": int -> big;
         end
      structure Int27 =
         struct
            type big = Int32.int
            type int = int27
            val fromBigUnsafe = _prim "WordU32_toWord27": big -> int;
            val precision' = 27
            val toBig = _prim "WordU27_toWord32": int -> big;
         end
      structure Int28 =
         struct
            type big = Int32.int
            type int = int28
            val fromBigUnsafe = _prim "WordU32_toWord28": big -> int;
            val precision' = 28
            val toBig = _prim "WordU28_toWord32": int -> big;
         end
      structure Int29 =
         struct
            type big = Int32.int
            type int = int29
            val fromBigUnsafe = _prim "WordU32_toWord29": big -> int;
            val precision' = 29
            val toBig = _prim "WordU29_toWord32": int -> big;
         end
      structure Int30 =
         struct
            type big = Int32.int
            type int = int30
            val fromBigUnsafe = _prim "WordU32_toWord30": big -> int;
            val precision' = 30
            val toBig = _prim "WordU30_toWord32": int -> big;
         end
      structure Int31 =
         struct
            type big = Int32.int
            type int = int31
            val fromBigUnsafe = _prim "WordU32_toWord31": big -> int;
            val precision' = 31
            val toBig = _prim "WordU31_toWord32": int -> big;
         end
      structure Int32 =
         struct
            type t = Int32.int
            type int = t

            val precision' : Int.int = 32
            val maxInt' : int = 0x7fffffff
            val minInt' : int = ~0x80000000

            val *? = _prim "WordS32_mul": int * int -> int;
            val * =
               if detectOverflow
                  then (wrapOverflow
                        (_prim "WordS32_mulCheck": int * int -> int;))
               else *?
            val +? = _prim "Word32_add": int * int -> int;
            val + =
               if detectOverflow
                  then (wrapOverflow
                        (_prim "WordS32_addCheck": int * int -> int;))
               else +?
            val -? = _prim "Word32_sub": int * int -> int;
            val - =
               if detectOverflow
                  then (wrapOverflow
                        (_prim "WordS32_subCheck": int * int -> int;))
               else -?
            val op < = _prim "WordS32_lt": int * int -> bool;
            val quot = _prim "WordS32_quot": int * int -> int;
            val rem = _prim "WordS32_rem": int * int -> int;
            val << = _prim "Word32_lshift": int * Word.word -> int;
            val >> = _prim "WordU32_rshift": int * Word.word -> int;
            val ~>> = _prim "WordS32_rshift": int * Word.word -> int;
            val ~? = _prim "Word32_neg": int -> int; 
            val ~ =
               if detectOverflow
                  then wrapOverflow (_prim "Word32_negCheck": int -> int;)
               else ~?
            val andb = _prim "Word32_andb": int * int -> int;
            val fromInt : int -> int = fn x => x
            val toInt : int -> int = fn x => x
         end
      structure Int32 =
         struct
            open Int32
            local
               structure S = Comparisons (Int32)
            in
               open S
            end
         end
      structure Int = Int32
      structure Int64 =
         struct
            type t = Int64.int
            type int = t

            val precision' : Int.int = 64
            val maxInt' : int = 0x7FFFFFFFFFFFFFFF
            val minInt' : int = ~0x8000000000000000

            val *? = _prim "WordS64_mul": int * int -> int;
            val * = fn _ => raise Fail "Int64.* unimplemented"
(*
            val * =
               if detectOverflow
                  then _prim "WordS64_mulCheck": int * int -> int;
               else *?
*)
            val +? = _prim "Word64_add": int * int -> int;
            val + =
               if detectOverflow
                  then (wrapOverflow
                        (_prim "WordS64_addCheck": int * int -> int;))
               else +?
            val -? = _prim "Word64_sub": int * int -> int;
            val - =
               if detectOverflow
                  then (wrapOverflow
                        (_prim "WordS64_subCheck": int * int -> int;))
               else -?
            val op < = _prim "WordS64_lt": int * int -> bool;
            val << = _prim "Word64_lshift": int * Word.word -> int;
            val >> = _prim "WordU64_rshift": int * Word.word -> int;
            val ~>> = _prim "WordS64_rshift": int * Word.word -> int;
            val quot = _prim "WordS64_quot": int * int -> int;
            val rem = _prim "WordS64_rem": int * int -> int;
            val ~? = _prim "Word64_neg": int -> int; 
            val ~ =
               if detectOverflow
                  then wrapOverflow (_prim "Word64_negCheck": int -> int;)
               else ~?
            val andb = _prim "Word64_andb": int * int -> int;
            val fromInt = _prim "WordS32_toWord64": Int.int -> int;
            val fromWord = _prim "WordU32_toWord64": word -> int;
            val toInt = _prim "WordU64_toWord32": int -> Int.int;
            val toWord = _prim "WordU64_toWord32": int -> word;
         end
      structure Int64 =
         struct
            open Int64
            local
               structure S = Comparisons (Int64)
            in
               open S
            end
         end

      structure Array =
         struct
            open Array

            val array = _prim "Array_array": int -> 'a array;
            val array =
               fn n => if safe andalso Int.< (n, 0)
                          then raise Size
                       else array n
         end

      structure IntInf =
         struct
            open IntInf

            val + = _prim "IntInf_add": int * int * word -> int;
            val andb = _prim "IntInf_andb": int * int * word -> int;
            val ~>> = _prim "IntInf_arshift": int * word * word -> int;
            val compare = _prim "IntInf_compare": int * int -> Int.int;
            val fromVector = _prim "WordVector_toIntInf": word vector -> int;
            val fromWord = _prim "Word_toIntInf": word -> int;
            val gcd = _prim "IntInf_gcd": int * int * word -> int;
            val << = _prim "IntInf_lshift": int * word * word -> int;
            val * = _prim "IntInf_mul": int * int * word -> int;
            val ~ = _prim "IntInf_neg": int * word -> int;
            val notb = _prim "IntInf_notb": int * word -> int;
            val orb = _prim "IntInf_orb": int * int * word -> int;
            val quot = _prim "IntInf_quot": int * int * word -> int;
            val rem = _prim "IntInf_rem": int * int * word -> int;
            val smallMul =
               _import "IntInf_smallMul": word * word * word ref -> word;
            val - = _prim "IntInf_sub": int * int * word -> int; 
            val toString
               = _prim "IntInf_toString": int * Int.int * word -> string;
            val toVector = _prim "IntInf_toVector": int -> word vector;
            val toWord = _prim "IntInf_toWord": int -> word;
            val xorb = _prim "IntInf_xorb": int * int * word -> int;
         end

      structure Itimer =
         struct
            type which = int
               
            val prof = _const "Itimer_prof": which;
            val real = _const "Itimer_real": which;
            val set =
               _import "Itimer_set": which * int * int * int * int -> unit;
            val virtual = _const "Itimer_virtual": which;
         end

      structure MLton =
         struct
            structure Codegen =
               struct
                  datatype t = Bytecode | C | Native

                  val codegen =
                     case _build_const "MLton_Codegen_codegen": int; of
                        0 => Bytecode
                      | 1 => C
                      | 2 => Native
                      | _ => raise Fail "MLton_Codegen_codegen"

                  val isBytecode = codegen = Bytecode
                  (* val isC = codegen = C *)
                  val isNative = codegen = Native
               end
            
            (* val deserialize = _prim "MLton_deserialize": Word8Vector.vector -> 'a ref; *)
            (* val serialize = _prim "MLton_serialize": 'a ref -> Word8Vector.vector; *)
            val share = _prim "MLton_share": 'a -> unit;
            val size = _prim "MLton_size": 'a ref -> int;

            structure Platform =
               struct
                  structure Arch =
                     struct
                        datatype t = Alpha | AMD64 | ARM | HPPA | IA64 | m68k |
                                     MIPS | PowerPC | S390 | Sparc | X86
                        
                        val host: t =
                           case _const "MLton_Platform_Arch_host": string; of
                              "alpha" => Alpha
                            | "amd64" => AMD64
                            | "arm" => ARM
                            | "hppa" => HPPA
                            | "ia64" => IA64
                            | "m68k" => m68k
                            | "mips" => MIPS
                            | "powerpc" => PowerPC
                            | "s390" => S390
                            | "sparc" => Sparc
                            | "x86" => X86
                            | _ => raise Fail "strange MLton_Platform_Arch_host"

                        val hostIsBigEndian =
                           _const "MLton_Platform_Arch_bigendian": bool;
                     end

                  structure OS =
                     struct
                        datatype t =
                           Cygwin
                         | Darwin
                         | FreeBSD
                         | Linux
                         | MinGW
                         | NetBSD
                         | OpenBSD
                         | Solaris

                        val host: t =
                           case _const "MLton_Platform_OS_host": string; of
                              "cygwin" => Cygwin
                            | "darwin" => Darwin
                            | "freebsd" => FreeBSD
                            | "linux" => Linux
                            | "mingw" => MinGW
                            | "netbsd" => NetBSD
                            | "openbsd" => OpenBSD
                            | "solaris" => Solaris
                            | _ => raise Fail "strange MLton_Platform_OS_host"

                        val forkIsEnabled =
                           case host of
                              Cygwin =>
                                 #1 _symbol "MLton_Platform_CygwinUseMmap": bool GetSet.t; ()
                            | MinGW => false
                            | _ => true

                        val useWindowsProcess = not forkIsEnabled
                     end
               end

            structure Process =
               struct
                  val spawne =
                     if let
                           open Platform.OS
                        in
                           case host of
                              Cygwin => true
                            | MinGW => true
                            | _ => false
                        end
                        then
                           _import "MLton_Process_spawne"
                           : (NullString.t 
                              * NullString.t array
                              * NullString.t array
                              -> Pid.t);
                        else fn _ => raise Fail "spawne not defined"
                  val spawnp =
                     if let
                           open Platform.OS
                        in
                           case host of
                              Cygwin => true
                            | MinGW => true
                            | _ => false
                        end
                        then
                           _import "MLton_Process_spawnp"
                           : (NullString.t 
                              * NullString.t array
                              -> Pid.t);
                        else fn _ => raise Fail "spawnp not defined"
               end
            
            structure Profile =
               struct
                  val isOn = _build_const "MLton_Profile_isOn": bool;
                  structure Data =
                     struct
                        type t = word

                        val dummy:t = 0w0
                        val free = _import "MLton_Profile_Data_free": t -> unit;
                        val malloc = _import "MLton_Profile_Data_malloc": unit -> t;
                        val write =
                           _import "MLton_Profile_Data_write"
                           : t * word (* fd *) -> unit;
                     end
                  val current = _import "MLton_Profile_current": unit -> Data.t;
                  val done = _import "MLton_Profile_done": unit -> unit;
                  val setCurrent =
                     _import "MLton_Profile_setCurrent": Data.t -> unit;
               end
            
            structure Rlimit =
               struct
                  type rlim = word
                     
                  val infinity = _const "MLton_Rlimit_infinity": rlim;

                  type t = int

                  val cpuTime = _const "MLton_Rlimit_cpuTime": t;
                  val coreFileSize = _const "MLton_Rlimit_coreFileSize": t;
                  val dataSize = _const "MLton_Rlimit_dataSize": t;
                  val fileSize = _const "MLton_Rlimit_fileSize": t;
                  val lockedInMemorySize =
                     _const "MLton_Rlimit_lockedInMemorySize": t;
                  val numFiles = _const "MLton_Rlimit_numFiles": t;
                  val numProcesses = _const "MLton_Rlimit_numProcesses": t;
                  val residentSetSize = _const "MLton_Rlimit_residentSetSize": t;
                  val stackSize = _const "MLton_Rlimit_stackSize": t;
                  val virtualMemorySize =
                     _const "MLton_Rlimit_virtualMemorySize": t;
                     
                  val get = _import "MLton_Rlimit_get": t -> int;
                  val getHard = _import "MLton_Rlimit_getHard": unit -> rlim;
                  val getSoft = _import "MLton_Rlimit_getSoft": unit -> rlim;
                  val set = _import "MLton_Rlimit_set": t * rlim * rlim -> int;
               end
            
            structure Rusage =
               struct
                 val ru = _import "MLton_Rusage_ru": unit -> unit;
                    
                 val self_utime_sec = _import "MLton_Rusage_self_utime_sec": unit -> int;
                 val self_utime_usec = _import "MLton_Rusage_self_utime_usec": unit -> int;
                 val self_stime_sec = _import "MLton_Rusage_self_stime_sec": unit -> int;
                 val self_stime_usec = _import "MLton_Rusage_self_stime_usec": unit -> int;
                 val children_utime_sec = _import "MLton_Rusage_children_utime_sec": unit -> int;
                 val children_utime_usec = _import "MLton_Rusage_children_utime_usec": unit -> int;
                 val children_stime_sec = _import "MLton_Rusage_children_stime_sec": unit -> int;
                 val children_stime_usec = _import "MLton_Rusage_children_stime_usec": unit -> int;
                 val gc_utime_sec = _import "MLton_Rusage_gc_utime_sec": unit -> int;
                 val gc_utime_usec = _import "MLton_Rusage_gc_utime_usec": unit -> int;
                 val gc_stime_sec = _import "MLton_Rusage_gc_stime_sec": unit -> int;
                 val gc_stime_usec = _import "MLton_Rusage_gc_stime_usec": unit -> int;
               end

            structure Syslog =
               struct
                  type openflag = int
                     
                  val CONS = _const "LOG_CONS": openflag;
                  val NDELAY = _const "LOG_NDELAY": openflag;
                  val PERROR = _const "LOG_PERROR": openflag;
                  val PID = _const "LOG_PID": openflag;
                     
                  type facility = int
                     
                  val AUTHPRIV = _const "LOG_AUTHPRIV": facility;
                  val CRON = _const "LOG_CRON": facility;
                  val DAEMON = _const "LOG_DAEMON": facility;
                  val KERN = _const "LOG_KERN": facility;
                  val LOCAL0 = _const "LOG_LOCAL0": facility;
                  val LOCAL1 = _const "LOG_LOCAL1": facility;
                  val LOCAL2 = _const "LOG_LOCAL2": facility;
                  val LOCAL3 = _const "LOG_LOCAL3": facility;
                  val LOCAL4 = _const "LOG_LOCAL4": facility;
                  val LOCAL5 = _const "LOG_LOCAL5": facility;
                  val LOCAL6 = _const "LOG_LOCAL6": facility;
                  val LOCAL7 = _const "LOG_LOCAL7": facility;
                  val LPR = _const "LOG_LPR": facility;
                  val MAIL = _const "LOG_MAIL": facility;
                  val NEWS = _const "LOG_NEWS": facility;
                  val SYSLOG = _const "LOG_SYSLOG": facility;
                  val USER = _const "LOG_USER": facility;
                  val UUCP = _const "LOG_UUCP": facility;
                     
                  type loglevel = int
                     
                  val EMERG = _const "LOG_EMERG": loglevel;
                  val ALERT = _const "LOG_ALERT": loglevel;
                  val CRIT = _const "LOG_CRIT": loglevel;
                  val ERR = _const "LOG_ERR": loglevel;
                  val WARNING = _const "LOG_WARNING": loglevel;
                  val NOTICE = _const "LOG_NOTICE": loglevel;
                  val INFO = _const "LOG_INFO": loglevel;
                  val DEBUG = _const "LOG_DEBUG": loglevel;
               end

            structure Weak =
               struct
                  type 'a t = 'a weak
                     
                  val canGet = _prim "Weak_canGet": 'a t -> bool;
                  val get = _prim "Weak_get": 'a t -> 'a;
                  val new = _prim "Weak_new": 'a -> 'a t;
               end
         end

      structure Net =
         struct
            (* val htonl = _import "Net_htonl": int -> int; *)
            (* val ntohl = _import "Net_ntohl": int -> int; *)
            val htons = _import "Net_htons": int -> int;
            val ntohs = _import "Net_ntohs": int -> int;
         end

      structure NetHostDB =
         struct
            (* network byte order (MSB) *)
            type pre_in_addr = word8 array
            type in_addr = word8 vector
            val inAddrLen = _const "NetHostDB_inAddrLen": int;
            val INADDR_ANY = _const "NetHostDB_INADDR_ANY": int;
            type addr_family = int
            val entryName = _import "NetHostDB_Entry_name": unit -> CString.t;
            val entryNumAliases = _import "NetHostDB_Entry_numAliases": unit -> int;
            val entryAliasesN = _import "NetHostDB_Entry_aliasesN": int -> CString.t;
            val entryAddrType = _import "NetHostDB_Entry_addrType": unit -> int;
            val entryLength = _import "NetHostDB_Entry_length": unit -> int;
            val entryNumAddrs = _import "NetHostDB_Entry_numAddrs": unit -> int;
            val entryAddrsN =
               _import "NetHostDB_Entry_addrsN": int * pre_in_addr -> unit;
            val getByAddress =
               _import "NetHostDB_getByAddress": in_addr * int -> bool;
            val getByName = _import "NetHostDB_getByName": NullString.t -> bool;
            val getHostName =
               _import "NetHostDB_getHostName": char array * int -> int;
         end

      structure NetProtDB =
         struct
            val entryName = _import "NetProtDB_Entry_name": unit -> CString.t;
            val entryNumAliases = _import "NetProtDB_Entry_numAliases": unit -> int;
            val entryAliasesN = _import "NetProtDB_Entry_aliasesN": int -> CString.t;
            val entryProtocol = _import "NetProtDB_Entry_protocol": unit -> int;
            val getByName = _import "NetProtDB_getByName": NullString.t -> bool;
            val getByNumber = _import "NetProtDB_getByNumber": int -> bool;
         end

      structure NetServDB =
         struct
            val entryName = _import "NetServDB_Entry_name": unit -> CString.t;
            val entryNumAliases = _import "NetServDB_Entry_numAliases": unit -> int;
            val entryAliasesN = _import "NetServDB_Entry_aliasesN": int -> CString.t;
            val entryPort = _import "NetServDB_Entry_port": unit -> int;
            val entryProtocol = _import "NetServDB_Entry_protocol": unit -> CString.t;
            val getByName = _import "NetServDB_getByName": NullString.t * NullString.t -> bool;
            val getByNameNull = _import "NetServDB_getByNameNull": NullString.t -> bool;
            val getByPort = _import "NetServDB_getByPort": int * NullString.t -> bool;
            val getByPortNull = _import "NetServDB_getByPortNull": int -> bool;
         end

      structure OS =
         struct
            structure IO =
               struct
                  val POLLIN = _const "OS_IO_POLLIN": word;
                  val POLLPRI = _const "OS_IO_POLLPRI": word;
                  val POLLOUT = _const "OS_IO_POLLOUT": word;
                  val poll = _import "OS_IO_poll": int vector * word vector * 
                                                int * int * word array -> int;
               end
         end

      structure PackReal32 =
         struct
            type real = Real32.real
               
            val subVec = _import "PackReal32_subVec": word8 vector * int -> real;
            val subVecRev =
               _import "PackReal32_subVecRev": word8 vector * int -> real;
            val update =
               _import "PackReal32_update": word8 array * int * real -> unit;
            val updateRev =
               _import "PackReal32_updateRev": word8 array * int * real -> unit;
         end

      structure PackReal64 =
         struct
            type real = Real64.real
               
            val subVec = _import "PackReal64_subVec": word8 vector * int -> real;
            val subVecRev =
               _import "PackReal64_subVecRev": word8 vector * int -> real;
            val update =
               _import "PackReal64_update": word8 array * int * real -> unit;
            val updateRev =
               _import "PackReal64_updateRev": word8 array * int * real -> unit;
         end

      structure Pointer =
         struct
            open Pointer

            val fromWord = _prim "WordU32_toWord32": word -> t;
            val toWord = _prim "WordU32_toWord32": t -> word;
               
            val null: t = fromWord 0w0

            fun isNull p = p = null

            (* val + = _prim "Pointer_add": t * t -> t; *)
            (* val op < = _prim "Pointer_lt": t * t -> bool; *)
            (* val - = _prim "Pointer_sub": t * t -> t; *)
(*            val free = _import "free": t -> unit; *)
            val getInt8 = _prim "Pointer_getWord8": t * int -> Int8.int;
            val getInt16 = _prim "Pointer_getWord16": t * int -> Int16.int;
            val getInt32 = _prim "Pointer_getWord32": t * int -> Int32.int;
            val getInt64 = _prim "Pointer_getWord64": t * int -> Int64.int;
            val getPointer = _prim "Pointer_getPointer": t * int -> 'a;
            val getReal32 = _prim "Pointer_getReal32": t * int -> Real32.real;
            val getReal64 = _prim "Pointer_getReal64": t * int -> Real64.real;
            val getWord8 = _prim "Pointer_getWord8": t * int -> Word8.word;
            val getWord16 = _prim "Pointer_getWord16": t * int -> Word16.word;
            val getWord32 = _prim "Pointer_getWord32": t * int -> Word32.word;
            val getWord64 = _prim "Pointer_getWord64": t * int -> Word64.word;
            val setInt8 = _prim "Pointer_setWord8": t * int * Int8.int -> unit;
            val setInt16 =
               _prim "Pointer_setWord16": t * int * Int16.int -> unit;
            val setInt32 =
               _prim "Pointer_setWord32": t * int * Int32.int -> unit;
            val setInt64 = 
               _prim "Pointer_setWord64": t * int * Int64.int -> unit;
            val setPointer = _prim "Pointer_setPointer": t * int * 'a -> unit;
            val setReal32 =
               _prim "Pointer_setReal32": t * int * Real32.real -> unit;
            val setReal64 =
               _prim "Pointer_setReal64": t * int * Real64.real -> unit;
            val setWord8 =
               _prim "Pointer_setWord8": t * int * Word8.word -> unit;
            val setWord16 =
               _prim "Pointer_setWord16": t * int * Word16.word -> unit;
            val setWord32 =
               _prim "Pointer_setWord32": t * int * Word32.word -> unit;
            val setWord64 =
               _prim "Pointer_setWord64": t * int * Word64.word -> unit;
         end

      structure Real64 =
         struct
            open Real64

            structure Class =
               struct
                  type t = int
                     
                  val inf = _const "FP_INFINITE": t;
                  val nan = _const "FP_NAN": t;
                  val normal = _const "FP_NORMAL": t;
                  val subnormal = _const "FP_SUBNORMAL": t;
                  val zero = _const "FP_ZERO": t;
               end
            
            structure Math =
               struct
                  type real = real

                  val acos = _prim "Real64_Math_acos": real -> real;
                  val asin = _prim "Real64_Math_asin": real -> real;
                  val atan = _prim "Real64_Math_atan": real -> real;
                  val atan2 = _prim "Real64_Math_atan2": real * real -> real;
                  val cos = _prim "Real64_Math_cos": real -> real;
                  val cosh = _import "cosh": real -> real;
                  val e = #1 _symbol "Real64_Math_e": real GetSet.t; ()
                  val exp = _prim "Real64_Math_exp": real -> real;
                  val ln = _prim "Real64_Math_ln": real -> real;
                  val log10 = _prim "Real64_Math_log10": real -> real;
                  val pi = #1 _symbol "Real64_Math_pi": real GetSet.t; ()
                  val pow = _import "pow": real * real -> real;
                  val sin = _prim "Real64_Math_sin": real -> real;
                  val sinh = _import "sinh": real -> real;
                  val sqrt = _prim "Real64_Math_sqrt": real -> real;
                  val tan = _prim "Real64_Math_tan": real -> real;
                  val tanh = _import "tanh": real -> real;
               end

            val * = _prim "Real64_mul": real * real -> real;
            val *+ = _prim "Real64_muladd": real * real * real -> real;
            val *- = _prim "Real64_mulsub": real * real * real -> real;
            val + = _prim "Real64_add": real * real -> real;
            val - = _prim "Real64_sub": real * real -> real;
            val / = _prim "Real64_div": real * real -> real;
            val op < = _prim "Real64_lt": real * real -> bool;
            val op <= = _prim "Real64_le": real * real -> bool;
            val == = _prim "Real64_equal": real * real -> bool;
            val ?= = _prim "Real64_qequal": real * real -> bool;
            val abs = _prim "Real64_abs": real -> real;
            val class = _import "Real64_class": real -> int;
            val frexp = _import "Real64_frexp": real * int ref -> real;
            val gdtoa =
               _import "Real64_gdtoa": real * int * int * int ref -> CString.t;
            val fromInt = _prim "WordS32_toReal64": int -> real;
            val ldexp = _prim "Real64_ldexp": real * int -> real;
            val maxFinite = #1 _symbol "Real64_maxFinite": real GetSet.t; ()
            val minNormalPos = #1 _symbol "Real64_minNormalPos": real GetSet.t; ()
            val minPos = #1 _symbol "Real64_minPos": real GetSet.t; ()
            val modf = _import "Real64_modf": real * real ref -> real;
            val nextAfter = _import "Real64_nextAfter": real * real -> real;
            val round = _prim "Real64_round": real -> real;
            val signBit = _import "Real64_signBit": real -> int;
            val strto = _import "Real64_strto": NullString.t -> real;
            val toInt = _prim "Real64_toWordS32": real -> int;
            val ~ = _prim "Real64_neg": real -> real;

            val fromLarge : real -> real = fn x => x
            val toLarge : real -> real = fn x => x
            val precision : int = 53
            val radix : int = 2
         end
      
      structure Real32 =
         struct
            open Real32

            val precision : int = 24
            val radix : int = 2

            val fromLarge = _prim "Real64_toReal32": real64 -> real;
            val toLarge = _prim "Real32_toReal64": real -> real64;

            fun unary (f: Real64.real -> Real64.real) (r: real): real =
               fromLarge (f (toLarge r))

            fun binary (f: Real64.real * Real64.real -> Real64.real)
               (r: real, r': real): real =
               fromLarge (f (toLarge r, toLarge r'))
               
            structure Math =
               struct
                  type real = real

                  val acos = _prim "Real32_Math_acos": real -> real;
                  val asin = _prim "Real32_Math_asin": real -> real;
                  val atan = _prim "Real32_Math_atan": real -> real;
                  val atan2 = _prim "Real32_Math_atan2": real * real -> real;
                  val cos = _prim "Real32_Math_cos": real -> real;
                  val cosh = unary Real64.Math.cosh
                  val e = #1 _symbol "Real32_Math_e": real GetSet.t; ()
                  val exp = _prim "Real32_Math_exp": real -> real;
                  val ln = _prim "Real32_Math_ln": real -> real;
                  val log10 = _prim "Real32_Math_log10": real -> real;
                  val pi = #1 _symbol "Real32_Math_pi": real GetSet.t; ()
                  val pow = binary Real64.Math.pow
                  val sin = _prim "Real32_Math_sin": real -> real;
                  val sinh = unary Real64.Math.sinh
                  val sqrt = _prim "Real32_Math_sqrt": real -> real;
                  val tan = _prim "Real32_Math_tan": real -> real;
                  val tanh = unary Real64.Math.tanh
               end

            val * = _prim "Real32_mul": real * real -> real;
            val *+ = _prim "Real32_muladd": real * real * real -> real;
            val *- = _prim "Real32_mulsub": real * real * real -> real;
            val + = _prim "Real32_add": real * real -> real;
            val - = _prim "Real32_sub": real * real -> real;
            val / = _prim "Real32_div": real * real -> real;
            val op < = _prim "Real32_lt": real * real -> bool;
            val op <= = _prim "Real32_le": real * real -> bool;
            val == = _prim "Real32_equal": real * real -> bool;
            val ?= = _prim "Real32_qequal": real * real -> bool;
            val abs = _prim "Real32_abs": real -> real;
            val class = _import "Real32_class": real -> int;
            fun frexp (r: real, ir: int ref): real =
               fromLarge (Real64.frexp (toLarge r, ir))
            val gdtoa =
               _import "Real32_gdtoa": real * int * int * int ref -> CString.t;
            val fromInt = _prim "WordS32_toReal32": int -> real;
            val ldexp = _prim "Real32_ldexp": real * int -> real;
            val maxFinite = #1 _symbol "Real32_maxFinite": real GetSet.t; ()
            val minNormalPos = #1 _symbol "Real32_minNormalPos": real GetSet.t; ()
            val minPos = #1 _symbol "Real32_minPos": real GetSet.t; ()
            val modf = _import "Real32_modf": real * real ref -> real;
            val signBit = _import "Real32_signBit": real -> int;
            val strto = _import "Real32_strto": NullString.t -> real;
            val toInt = _prim "Real32_toWordS32": real -> int;
            val ~ = _prim "Real32_neg": real -> real;
         end
    
      structure Real32 =
         struct
            open Real32
            local
               structure S = RealComparisons (Real32)
            in
               open S
            end
         end

      structure Real64 =
         struct
            open Real64
            local
               structure S = RealComparisons (Real64)
            in
               open S
            end
         end

      structure Ref =
         struct
            val deref = _prim "Ref_deref": 'a ref -> 'a;
            val assign = _prim "Ref_assign": 'a ref * 'a -> unit;
         end

      structure Signal:>
         sig
            eqtype t
            type how

            val fromInt: int -> t
            val toInt: t -> int
         end =
         struct
            type t = int
            type how = int

            val fromInt = fn s => s
            val toInt = fn s => s
         end

      structure Socket:>
         sig
            type sock

            val fromInt: int -> sock
            val toInt: sock -> int
         end =
         struct
            type sock = int

            fun fromInt i = i
            fun toInt i = i
         end
            
      structure Socket =
         struct
            open Socket

            type pre_sock_addr = word8 array
            type sock_addr = word8 vector
            val sockAddrLenMax = _const "Socket_sockAddrLenMax": int;
            structure AF =
               struct
                  type addr_family = int
                  val UNIX = _const "Socket_AF_UNIX": addr_family;
                  val INET = _const "Socket_AF_INET": addr_family;
                  val INET6 = _const "Socket_AF_INET6": addr_family;
                  val UNSPEC = _const "Socket_AF_UNSPEC": addr_family;
               end
            structure SOCK:>
               sig
                  eqtype sock_type

                  val fromInt: int -> sock_type
               end =
               struct
                  type sock_type = int

                  val fromInt = fn i => i
               end
            structure SOCK =
               struct
                  open SOCK
                  val STREAM = _const "Socket_SOCK_STREAM": sock_type;
                  val DGRAM = _const "Socket_SOCK_DGRAM": sock_type;
               end
            structure CtlExtra =
               struct
                  type level = int
                  type optname = int
                  type request = int
                  (* host byte order (LSB) *)
                  type read_data = word8 vector
                  type write_data = word8 array

                  val setSockOpt = 
                     _import "Socket_Ctl_setSockOpt": sock * level * optname * 
                                                   read_data * int -> 
                                                   int;
                  val getSockOpt = 
                     _import "Socket_Ctl_getSockOpt": sock * level * optname * 
                                                   write_data * int ref -> 
                                                   int;
                  val setIOCtl =
                     _import "Socket_Ctl_getsetIOCtl": sock * request *
                                                    read_data ->
                                                    int;
                  val getIOCtl =
                     _import "Socket_Ctl_getsetIOCtl": sock * request *
                                                    write_data ->
                                                    int;
               end
            structure Ctl =
               struct
                  open CtlExtra
                  val SOCKET = _const "Socket_Ctl_SOL_SOCKET": level;
                  val DEBUG = _const "Socket_Ctl_SO_DEBUG": optname;
                  val REUSEADDR = _const "Socket_Ctl_SO_REUSEADDR": optname;
                  val KEEPALIVE = _const "Socket_Ctl_SO_KEEPALIVE": optname;
                  val DONTROUTE = _const "Socket_Ctl_SO_DONTROUTE": optname;
                  val LINGER = _const "Socket_Ctl_SO_LINGER": optname;
                  val BROADCAST = _const "Socket_Ctl_SO_BROADCAST": optname;
                  val OOBINLINE = _const "Socket_Ctl_SO_OOBINLINE": optname;
                  val SNDBUF = _const "Socket_Ctl_SO_SNDBUF": optname;
                  val RCVBUF = _const "Socket_Ctl_SO_RCVBUF": optname;
                  val TYPE = _const "Socket_Ctl_SO_TYPE": optname;
                  val ERROR = _const "Socket_Ctl_SO_ERROR": optname;

                  val getPeerName =
                     _import "Socket_Ctl_getPeerName": sock * pre_sock_addr * int ref -> int;
                  val getSockName =
                     _import "Socket_Ctl_getSockName": sock * pre_sock_addr * int ref -> int;

                  (* val NBIO = _const "Socket_Ctl_FIONBIO": request; *)
                  val NREAD = _const "Socket_Ctl_FIONREAD": request;
                  val ATMARK = _const "Socket_Ctl_SIOCATMARK": request;
               end

            val familyOfAddr =
               _import "Socket_familyOfAddr": sock_addr -> AF.addr_family;
            val bind = _import "Socket_bind": sock * sock_addr * int -> int;
            val listen = _import "Socket_listen": sock * int -> int;
            val connect =
               _import "Socket_connect": sock * sock_addr * int -> int;
            val accept =
               _import "Socket_accept": sock * pre_sock_addr * int ref -> int;
            val close = _import "Socket_close": sock -> int;

            type how = int
            val SHUT_RD = _const "Socket_SHUT_RD": how;
            val SHUT_WR = _const "Socket_SHUT_WR": how;
            val SHUT_RDWR = _const "Socket_SHUT_RDWR": how;
            val shutdown = _import "Socket_shutdown": sock * how -> int;

            type flags = word
            val MSG_DONTROUTE = _const "Socket_MSG_DONTROUTE": flags;
            val MSG_DONTWAIT = _const "Socket_MSG_DONTWAIT": flags;
            val MSG_OOB = _const "Socket_MSG_OOB": flags;
            val MSG_PEEK = _const "Socket_MSG_PEEK": flags;

            val sendArr = _import "Socket_send":
               sock * word8 array * int * int * word -> int;
            val sendVec = _import "Socket_send":
               sock * word8 vector * int * int * word -> int;
            val sendToArr = _import "Socket_sendTo":
               sock * word8 array * int * int * word * sock_addr * int -> int;
            val sendToVec = _import "Socket_sendTo":
               sock * word8 vector * int * int * word * sock_addr * int -> int;
            val recv = _import "Socket_recv":
               sock * word8 array * int * int * word -> int;
            val recvFrom = _import "Socket_recvFrom":
               sock * word8 array * int * int * word * pre_sock_addr * int ref
               -> int;

            structure GenericSock =
               struct
                  val socket = 
                     _import "GenericSock_socket": AF.addr_family * 
                                                SOCK.sock_type * 
                                                int -> int;
                  val socketPair = 
                     _import "GenericSock_socketPair": AF.addr_family * 
                                                    SOCK.sock_type * 
                                                    int * 
                                                    int ref * int ref -> int;
               end

            structure INetSock =
               struct
                  val toAddr = _import "INetSock_toAddr": NetHostDB.in_addr * int * 
                                                       pre_sock_addr * int ref -> unit;
                  val fromAddr = _import "INetSock_fromAddr": sock_addr -> unit;
                  val getInAddr = _import "INetSock_getInAddr": NetHostDB.pre_in_addr -> 
                                                             unit;
                  val getPort = _import "INetSock_getPort": unit -> int;
                  structure TCP =
                     struct
                        open CtlExtra
                        val TCP = _const "Socket_INetSock_TCP_SOL_TCP": level;
                        val NODELAY = _const "Socket_INetSock_TCP_SO_NODELAY": optname;
                     end
               end
            structure UnixSock =
               struct
                  val toAddr =
                     _import "UnixSock_toAddr"
                     : NullString.t * int * pre_sock_addr * int ref -> unit;
                  val pathLen = _import "UnixSock_pathLen": sock_addr -> int;
                  val fromAddr =
                     _import "UnixSock_fromAddr"
                     : sock_addr * char array * int -> unit;
               end
         end

      structure Status:>
         sig
            eqtype t

            val failure: t
            val fromInt: int -> t
            val success: t
            val toInt: t -> int
         end =
         struct
            type t = int

            val failure = 1
            val fromInt = fn i => i
            val success = 0
            val toInt = fn i => i
         end

      val halt = _prim "MLton_halt": Status.t -> unit;

      structure String =
         struct
            val fromWord8Vector =
               _prim "Word8Vector_toString": word8 vector -> string;
            val toWord8Vector =
               _prim "String_toWord8Vector": string -> word8 vector;
         end

      structure TextIO =
         struct
            val bufSize = _command_line_const "TextIO.bufSize": int = 4096;
         end
      
      structure Thread =
         struct
            type preThread = PreThread.t
            type thread = Thread.t

            val atomicBegin = _prim "Thread_atomicBegin": unit -> unit;
            val canHandle = _prim "Thread_canHandle": unit -> int;
            fun atomicEnd () =
               if Int.<= (canHandle (), 0)
                  then raise Fail "Thread.atomicEnd with no atomicBegin"
               else _prim "Thread_atomicEnd": unit -> unit; ()
            val copy = _prim "Thread_copy": preThread -> thread;
            (* copyCurrent's result is accesible via savedPre ().
             * It is not possible to have the type of copyCurrent as
             * unit -> preThread, because there are two different ways to
             * return from the call to copyCurrent.  One way is the direct
             * obvious way, in the thread that called copyCurrent.  That one,
             * of course, wants to call savedPre ().  However, another way to
             * return is by making a copy of the preThread and then switching
             * to it.  In that case, there is no preThread to return.  Making
             * copyCurrent return a preThread creates nasty bugs where the
             * return code from the CCall expects to see a preThread result
             * according to the C return convention, but there isn't one when
             * switching to a copy.
             *)
            val copyCurrent = _prim "Thread_copyCurrent": unit -> unit;
            val current = _import "Thread_current": unit -> thread;
            val finishHandler = _import "Thread_finishHandler": unit -> unit;
            val returnToC = _prim "Thread_returnToC": unit -> unit;
            val saved = _import "Thread_saved": unit -> thread;
            val savedPre = _import "Thread_saved": unit -> preThread;
            val setCallFromCHandler =
               _import "Thread_setCallFromCHandler": thread -> unit;
            val setHandler = _import "Thread_setHandler": thread -> unit;
            val setSaved = _import "Thread_setSaved": thread -> unit;
            val startHandler = _import "Thread_startHandler": unit -> unit;
            val switchTo = _prim "Thread_switchTo": thread -> unit;
         end      

      structure Time =
         struct
            val gettimeofday = _import "Time_gettimeofday": unit -> int;
            val sec = _import "Time_sec": unit -> int;
            val usec = _import "Time_usec": unit -> int;
         end

      structure TopLevel =
         struct
            val setHandler =
               _prim "TopLevel_setHandler": (exn -> unit) -> unit;
            val setSuffix =
               _prim "TopLevel_setSuffix": (unit -> unit) -> unit;
         end

      structure Vector =
         struct
            val sub = _prim "Vector_sub": 'a vector * int -> 'a;
            val length = _prim "Vector_length": 'a vector -> int;

            (* Don't mutate the array after you apply fromArray, because vectors
             * are supposed to be immutable and the optimizer depends on this.
             *)
            val fromArray = _prim "Array_toVector": 'a array -> 'a vector;
         end

      structure Word1 =
         struct
            type big = Word8.word
            type word = word1
            val fromBigUnsafe = _prim "WordU8_toWord1": big -> word;
            val toBig = _prim "WordU1_toWord8": word -> big;
            val wordSize = 1
         end
      structure Word2 =
         struct
            type big = Word8.word
            type word = word2
            val fromBigUnsafe = _prim "WordU8_toWord2": big -> word;
            val toBig = _prim "WordU2_toWord8": word -> big;
            val wordSize = 2
         end
      structure Word3 =
         struct
            type big = Word8.word
            type word = word3
            val fromBigUnsafe = _prim "WordU8_toWord3": big -> word;
            val toBig = _prim "WordU3_toWord8": word -> big;
            val wordSize = 3
         end
      structure Word4 =
         struct
            type big = Word8.word
            type word = word4
            val fromBigUnsafe = _prim "WordU8_toWord4": big -> word;
            val toBig = _prim "WordU4_toWord8": word -> big;
            val wordSize = 4
         end
      structure Word5 =
         struct
            type big = Word8.word
            type word = word5
            val fromBigUnsafe = _prim "WordU8_toWord5": big -> word;
            val toBig = _prim "WordU5_toWord8": word -> big;
            val wordSize = 5
         end
      structure Word6 =
         struct
            type big = Word8.word
            type word = word6
            val fromBigUnsafe = _prim "WordU8_toWord6": big -> word;
            val toBig = _prim "WordU6_toWord8": word -> big;
            val wordSize = 6
         end
      structure Word7 =
         struct
            type big = Word8.word
            type word = word7
            val fromBigUnsafe = _prim "WordU8_toWord7": big -> word;
            val toBig = _prim "WordU7_toWord8": word -> big;
            val wordSize = 7
         end
      structure Word8 =
         struct
            open Word8
               
            val wordSize: int = 8

            val + = _prim "Word8_add": word * word -> word;
            val andb = _prim "Word8_andb": word * word -> word;
            val ~>> = _prim "WordS8_rshift": word * Word.word -> word;
            val div = _prim "WordU8_quot": word * word -> word;
            val fromInt = _prim "WordU32_toWord8": int -> word;
            val fromLarge = _prim "WordU64_toWord8": LargeWord.word -> word;
            val << = _prim "Word8_lshift": word * Word.word -> word;
            val op < = _prim "WordU8_lt": word * word -> bool;
            val mod = _prim "WordU8_rem": word * word -> word;
            val * = _prim "WordU8_mul": word * word -> word;
            val ~ = _prim "Word8_neg": word -> word;
            val notb = _prim "Word8_notb": word -> word;
            val orb = _prim "Word8_orb": word * word -> word;
            val rol = _prim "Word8_rol": word * Word.word -> word;
            val ror = _prim "Word8_ror": word * Word.word -> word;
            val >> = _prim "WordU8_rshift": word * Word.word -> word;
            val - = _prim "Word8_sub": word * word -> word;
            val toInt = _prim "WordU8_toWord32": word -> int;
            val toIntX = _prim "WordS8_toWord32": word -> int;
            val toLarge = _prim "WordU8_toWord64": word -> LargeWord.word;
            val toLargeX = _prim "WordS8_toWord64": word -> LargeWord.word;
            val xorb = _prim "Word8_xorb": word * word -> word;
         end
      structure Word8 =
         struct
            open Word8
            local
               structure S = Comparisons (Word8)
            in
               open S
            end
         end
      structure Word8Array =
         struct
            val subWord =
               _prim "Word8Array_subWord": word8 array * int -> word;
            val subWordRev =
               _import "Word8Array_subWord32Rev": word8 array * int -> word;
            val updateWord =
               _prim "Word8Array_updateWord": word8 array * int * word -> unit;
            val updateWordRev =
               _import "Word8Array_updateWord32Rev": word8 array * int * word -> unit;
         end
      structure Word8Vector =
         struct
            val subWord =
               _prim "Word8Vector_subWord": word8 vector * int -> word;
            val subWordRev =
               _import "Word8Vector_subWord32Rev": word8 vector * int -> word;
         end
      structure Word9 =
         struct
            type big = Word16.word
            type word = word9
            val fromBigUnsafe = _prim "WordU16_toWord9": big -> word;
            val toBig = _prim "WordU9_toWord16": word -> big;
            val wordSize = 9
         end
      structure Word10 =
         struct
            type big = Word16.word
            type word = word10
            val fromBigUnsafe = _prim "WordU16_toWord10": big -> word;
            val toBig = _prim "WordU10_toWord16": word -> big;
            val wordSize = 10
         end
      structure Word11 =
         struct
            type big = Word16.word
            type word = word11
            val fromBigUnsafe = _prim "WordU16_toWord11": big -> word;
            val toBig = _prim "WordU11_toWord16": word -> big;
            val wordSize = 11
         end
      structure Word12 =
         struct
            type big = Word16.word
            type word = word12
            val fromBigUnsafe = _prim "WordU16_toWord12": big -> word;
            val toBig = _prim "WordU12_toWord16": word -> big;
            val wordSize = 12
         end
      structure Word13 =
         struct
            type big = Word16.word
            type word = word13
            val fromBigUnsafe = _prim "WordU16_toWord13": big -> word;
            val toBig = _prim "WordU13_toWord16": word -> big;
            val wordSize = 13
         end
      structure Word14 =
         struct
            type big = Word16.word
            type word = word14
            val fromBigUnsafe = _prim "WordU16_toWord14": big -> word;
            val toBig = _prim "WordU14_toWord16": word -> big;
            val wordSize = 14
         end
      structure Word15 =
         struct
            type big = Word16.word
            type word = word15
            val fromBigUnsafe = _prim "WordU16_toWord15": big -> word;
            val toBig = _prim "WordU15_toWord16": word -> big;
            val wordSize = 15
         end
      structure Word16 =
         struct
            open Word16
               
            val wordSize: int = 16

            val + = _prim "Word16_add": word * word -> word;
            val andb = _prim "Word16_andb": word * word -> word;
            val ~>> = _prim "WordS16_rshift": word * Word.word -> word;
            val div = _prim "WordU16_quot": word * word -> word;
            val fromInt = _prim "WordU32_toWord16": int -> word;
            val fromLarge = _prim "WordU64_toWord16": LargeWord.word -> word;
            val << = _prim "Word16_lshift": word * Word.word -> word;
            val op < = _prim "WordU16_lt": word * word -> bool;
            val mod = _prim "WordU16_rem": word * word -> word;
            val * = _prim "WordU16_mul": word * word -> word;
            val ~ = _prim "Word16_neg": word -> word;
            val notb = _prim "Word16_notb": word -> word;
            val orb = _prim "Word16_orb": word * word -> word;
            val >> = _prim "WordU16_rshift": word * Word.word -> word;
            val - = _prim "Word16_sub": word * word -> word;
            val toInt = _prim "WordU16_toWord32": word -> int;
            val toIntX = _prim "WordS16_toWord32": word -> int;
            val toLarge = _prim "WordU16_toWord64": word -> LargeWord.word;
            val toLargeX = _prim "WordS16_toWord64": word -> LargeWord.word;
            val xorb = _prim "Word16_xorb": word * word -> word;
         end
      structure Word16 =
         struct
            open Word16
            local
               structure S = Comparisons (Word16)
            in
               open S
            end
         end
      structure Word17 =
         struct
            type big = Word32.word
            type word = word17
            val fromBigUnsafe = _prim "WordU32_toWord17": big -> word;
            val toBig = _prim "WordU17_toWord32": word -> big;
            val wordSize = 17
         end
      structure Word18 =
         struct
            type big = Word32.word
            type word = word18
            val fromBigUnsafe = _prim "WordU32_toWord18": big -> word;
            val toBig = _prim "WordU18_toWord32": word -> big;
            val wordSize = 18
         end
      structure Word19 =
         struct
            type big = Word32.word
            type word = word19
            val fromBigUnsafe = _prim "WordU32_toWord19": big -> word;
            val toBig = _prim "WordU19_toWord32": word -> big;
            val wordSize = 19
         end
      structure Word20 =
         struct
            type big = Word32.word
            type word = word20
            val fromBigUnsafe = _prim "WordU32_toWord20": big -> word;
            val toBig = _prim "WordU20_toWord32": word -> big;
            val wordSize = 20
         end
      structure Word21 =
         struct
            type big = Word32.word
            type word = word21
            val fromBigUnsafe = _prim "WordU32_toWord21": big -> word;
            val toBig = _prim "WordU21_toWord32": word -> big;
            val wordSize = 21
         end
      structure Word22 =
         struct
            type big = Word32.word
            type word = word22
            val fromBigUnsafe = _prim "WordU32_toWord22": big -> word;
            val toBig = _prim "WordU22_toWord32": word -> big;
            val wordSize = 22
         end
      structure Word23 =
         struct
            type big = Word32.word
            type word = word23
            val fromBigUnsafe = _prim "WordU32_toWord23": big -> word;
            val toBig = _prim "WordU23_toWord32": word -> big;
            val wordSize = 23
         end
      structure Word24 =
         struct
            type big = Word32.word
            type word = word24
            val fromBigUnsafe = _prim "WordU32_toWord24": big -> word;
            val toBig = _prim "WordU24_toWord32": word -> big;
            val wordSize = 24
         end
      structure Word25 =
         struct
            type big = Word32.word
            type word = word25
            val fromBigUnsafe = _prim "WordU32_toWord25": big -> word;
            val toBig = _prim "WordU25_toWord32": word -> big;
            val wordSize = 25
         end
      structure Word26 =
         struct
            type big = Word32.word
            type word = word26
            val fromBigUnsafe = _prim "WordU32_toWord26": big -> word;
            val toBig = _prim "WordU26_toWord32": word -> big;
            val wordSize = 26
         end
      structure Word27 =
         struct
            type big = Word32.word
            type word = word27
            val fromBigUnsafe = _prim "WordU32_toWord27": big -> word;
            val toBig = _prim "WordU27_toWord32": word -> big;
            val wordSize = 27
         end
      structure Word28 =
         struct
            type big = Word32.word
            type word = word28
            val fromBigUnsafe = _prim "WordU32_toWord28": big -> word;
            val toBig = _prim "WordU28_toWord32": word -> big;
            val wordSize = 28
         end
      structure Word29 =
         struct
            type big = Word32.word
            type word = word29
            val fromBigUnsafe = _prim "WordU32_toWord29": big -> word;
            val toBig = _prim "WordU29_toWord32": word -> big;
            val wordSize = 29
         end
      structure Word30 =
         struct
            type big = Word32.word
            type word = word30
            val fromBigUnsafe = _prim "WordU32_toWord30": big -> word;
            val toBig = _prim "WordU30_toWord32": word -> big;
            val wordSize = 30
         end
      structure Word31 =
         struct
            type big = Word32.word
            type word = word31
            val fromBigUnsafe = _prim "WordU32_toWord31": big -> word;
            val toBig = _prim "WordU31_toWord32": word -> big;
            val wordSize = 31
         end
      structure Word32 =
         struct
            open Word32
               
            val wordSize: int = 32

            val + = _prim "Word32_add": word * word -> word;
            val andb = _prim "Word32_andb": word * word -> word;
            val ~>> = _prim "WordS32_rshift": word * word -> word;
            val div = _prim "WordU32_quot": word * word -> word;
            val fromInt = _prim "WordU32_toWord32": int -> word;
            val fromLarge = _prim "WordU64_toWord32": LargeWord.word -> word;
            val << = _prim "Word32_lshift": word * word -> word;
            val op < = _prim "WordU32_lt": word * word -> bool;
            val mod = _prim "WordU32_rem": word * word -> word;
            val * = _prim "WordU32_mul": word * word -> word;
            val ~ = _prim "Word32_neg": word -> word;
            val notb = _prim "Word32_notb": word -> word;
            val orb = _prim "Word32_orb": word * word -> word;
            val rol = _prim "Word32_rol": word * word -> word;
            val ror = _prim "Word32_ror": word * word -> word;
            val >> = _prim "WordU32_rshift": word * word -> word;
            val - = _prim "Word32_sub": word * word -> word;
            val toInt = _prim "WordU32_toWord32": word -> int;
            val toIntX = _prim "WordS32_toWord32": word -> int;
            val toLarge = _prim "WordU32_toWord64": word -> LargeWord.word;
            val toLargeX = _prim "WordS32_toWord64": word -> LargeWord.word;
            val xorb = _prim "Word32_xorb": word * word -> word;
         end
      structure Word32 =
         struct
            open Word32
            local
               structure S = Comparisons (Word32)
            in
               open S
            end
         end
      structure Word = Word32
      structure Word64 =
         struct
            open Word64
               
            val wordSize: int = 64

            val + = _prim "Word64_add": word * word -> word;
            val andb = _prim "Word64_andb": word * word -> word;
            val ~>> = _prim "WordS64_rshift": word * Word.word -> word;
            val div = _prim "WordU64_quot": word * word -> word;
            val fromInt = _prim "WordS32_toWord64": int -> word;
            val fromLarge: LargeWord.word -> word = fn x => x
            val << = _prim "Word64_lshift": word * Word.word -> word;
            val op < = _prim "WordU64_lt": word * word -> bool;
            val mod = _prim "WordU64_rem": word * word -> word;
            val * = _prim "WordU64_mul": word * word -> word;
            val ~ = _prim "Word64_neg": word -> word;
            val notb = _prim "Word64_notb": word -> word;
            val orb = _prim "Word64_orb": word * word -> word;
            val >> = _prim "WordU64_rshift": word * Word.word -> word;
            val - = _prim "Word64_sub": word * word -> word;
            val toInt = _prim "WordU64_toWord32": word -> int;
            val toIntX = _prim "WordU64_toWord32": word -> int;
            val toLarge: word -> LargeWord.word = fn x => x
            val toLargeX: word -> LargeWord.word = fn x => x
            val xorb = _prim "Word64_xorb": word * word -> word;
         end
      structure Word64 =
         struct
            open Word64
            local
               structure S = Comparisons (Word64)
            in
               open S
            end
         end

      structure Cygwin =
         struct
            val toFullWindowsPath =
               _import "Cygwin_toFullWindowsPath": NullString.t -> CString.t;
         end

      structure FileDesc:>
         sig
            eqtype t

            val fromWord: word -> t
            val fromInt: int -> t
            val toInt: t -> int
            val toWord: t -> word
         end =
         struct
            type t = int

            val fromWord = Word32.toInt
            fun fromInt i = i
            fun toInt i = i
            val toWord = Word32.fromInt
         end

      structure Windows =
         struct
            structure Process =
               struct
                  val create = 
                     _import "Windows_Process_create"
                     : (NullString.t * NullString.t * NullString.t
                        * FileDesc.t * FileDesc.t * FileDesc.t) -> Pid.t;
                  val terminate =
                     _import "Windows_terminate": Pid.t * Signal.t -> int;
               end
         end

      structure World =
         struct
            val isOriginal = _import "World_isOriginal": unit -> bool;
            val makeOriginal = _import "World_makeOriginal": unit -> unit;
            val save = _prim "World_save": word (* filedes *) -> unit;
         end
   end

structure Primitive =
   struct
      open Primitive

      structure Int32 =
         struct
            open Int32
               
            local
               fun make f (i: int, i': int): bool =
                  f (Primitive.Word32.fromInt i, Primitive.Word32.fromInt i')
            in
               val geu = make Primitive.Word32.>=
               val gtu = make Primitive.Word32.> 
            end
         end
      structure Int = Int32
   end

structure NullString =
   struct
      open NullString

      fun fromString s =
         if #"\000" = let
                         open Primitive
                      in
                         Vector.sub (s, Int.- (Vector.length s, 1))
                      end
            then NullString.fromString s
         else raise Fail "NullString.fromString"

      val empty = fromString "\000"
   end

(* Quell unused warnings. *)
local
   val _ = #"a": Char2.t: Char2.char
   val _ = #"a": Char4.t: Char4.char
   val _ = "a": String2.t: String2.string
   val _ = "a": String4.t: String4.string
   open Primitive
   open Char2
   val _ = op <
   val _ = chr
   val _ = ord
   open Char4
   val _ = op <
   val _ = chr
   val _ = ord
   open Int64
   val _ = << 
   val _ = >>
   val _ = ~>>
   val _ = andb
in
end

(* Install an emergency exception handler. *)
local
   open Primitive
   val _ =
      TopLevel.setHandler 
      (fn exn => 
       (Stdio.print "unhandled exception: "
        ; case exn of
             Fail msg => (Stdio.print "Fail "
                          ; Stdio.print msg)
           | _ => Stdio.print (Exn.name exn)
        ; Stdio.print "\n"
        ; bug (NullString.fromString 
               "unhandled exception in Basis Library\000")))
in
end
