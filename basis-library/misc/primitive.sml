(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

(* Primitive names are special -- see atoms/prim.fun.
 * _prim declarations of ground type declare compile time constants, which are
 * evaluated by calling gcc just before running infer/infer.fun.
 * The polymorphic primitives are eta-expanded so that my create-basis
 * script produces type-correct SML code.
 *)

type 'a array = 'a array
structure Bool =
   struct
      datatype bool = datatype bool
   end
datatype bool = datatype Bool.bool
structure Char =
   struct
      type char = char
   end
type char = Char.char
type exn = exn
structure Int8 =
   struct
      type int = int8
   end
structure Int16 =
   struct
      type int = int16
   end
structure Int32 =
   struct
      type int = int32
   end
structure Int64 =
   struct
      type int = int64
   end
structure IntInf =
   struct
      type int = intInf
   end
datatype list = datatype list
type pointer = pointer (* C integer, not SML heap pointer *)
structure Real32 =
   struct
      type real = real32
   end
structure Real64 =
   struct
      type real = real64
   end
datatype ref = datatype ref
type preThread = preThread
type thread = thread
structure Word8 =
   struct
      type word = word8
   end
structure Word16 =
   struct
      type word = word16
   end
structure Word32 =
   struct
      type word = word32
   end
type 'a vector = 'a vector
type 'a weak = 'a weak
type string = char vector
type nullString = string

structure Int = Int32
type int = Int.int
structure Real = Real64
type real = Real.real
structure Word = Word32
type word = Word.word

exception Bind = Bind
exception Fail of string
exception Match = Match
exception Overflow = Overflow
exception Size

structure Primitive =
   struct
      structure Debug =
	 struct
	    val enter = _ffi "Debug_enter": string -> unit;
	    val leave = _ffi "Debug_leave": string -> unit;
	 end
   end
   
structure Primitive =
   struct
      val detectOverflow = _build_const "MLton_detectOverflow": bool;
      val enterLeave = _ffi "MLton_enterLeave": unit -> unit;
      val eq = fn z => _prim "MLton_eq": 'a * 'a -> bool; z
      val errno = _ffi "MLton_errno": unit -> int;
      val halt = _prim "MLton_halt": int -> unit;
      val handlesSignals = _prim "MLton_handlesSignals": bool;
      val installSignalHandler =
	 _prim "MLton_installSignalHandler": unit -> unit;
      val safe = _build_const "MLton_safe": bool;
      val touch = fn z => _prim "MLton_touch": 'a -> unit; z
      val usesCallcc: bool ref = ref false;

      structure Stdio =
	 struct
	    val print = _ffi "Stdio_print": string -> unit;
	    val sprintf =
	       _ffi "Stdio_sprintf": char array * nullString * real -> int;
	 end

      structure Array =
	 struct
	    val array0Const =
	       fn () => _prim "Array_array0Const": unit -> 'a array; ()
	    val length = fn x => _prim "Array_length": 'a array -> int; x
	    (* There is no maximum length on arrays, so maxLen = maxInt. *)
	    val maxLen: int = 0x7FFFFFFF
	    val sub = fn x => _prim "Array_sub": 'a array * int -> 'a; x
	    val update =
	       fn x => _prim "Array_update": 'a array * int * 'a -> unit; x
	 end

      structure Byte =
	 struct
	    val byteToChar = _prim "Byte_byteToChar": word8 -> char;
	    val charToByte = _prim "Byte_charToByte": char -> word8;
	 end

      structure C =
	 struct
	    (* char* *)
	    structure CS =
	       struct
		  type t = pointer

		  val sub = _ffi "C_CS_sub": t * int -> char;
		  val update =
		     _ffi "C_CS_update": t * int * char -> unit; (* primitive *)
		  val charArrayToWord8Array =
		     _prim "C_CS_charArrayToWord8Array":
		     char array -> word8 array;
	       end
	    
	    (* char** *)
	    structure CSS =
	       struct
		  type t = pointer
		     
		  val sub = _ffi "C_CSS_sub": t * int -> CS.t;
	       end
	 end

      type cstring = C.CS.t
      type cstringArray = C.CSS.t
      type nullString = string

      structure Char =
	 struct
	    val < = _prim "Char_lt": char * char -> bool;
	    val <= = _prim "Char_le": char * char -> bool;
	    val > = _prim "Char_gt": char * char -> bool;
	    val >= = _prim "Char_ge": char * char -> bool;
	    val chr = _prim "Char_chr": int -> char;
	    val ord = _prim "Char_ord": char -> int;
	 end

      structure CommandLine =
	 struct
	    val argc = fn () => _ffi "CommandLine_argc": int;
	    val argv = fn () => _ffi "CommandLine_argv": cstringArray;
	    val commandName = fn () => _ffi "CommandLine_commandName": cstring;
	 end

      structure Cpointer =
	 struct
	    val isNull = _prim "Cpointer_isNull": pointer -> bool;
	 end

      structure Date =
	 struct
	    type time = int
	    type size = int

	    structure Tm =
	       struct
		  val sec = _ffi "Date_Tm_sec": unit -> int;
		  val min = _ffi "Date_Tm_min": unit -> int;
		  val hour = _ffi "Date_Tm_hour": unit -> int;
		  val mday = _ffi "Date_Tm_mday": unit -> int;
		  val mon = _ffi "Date_Tm_mon": unit -> int;
		  val year = _ffi "Date_Tm_year": unit -> int;
		  val wday = _ffi "Date_Tm_wday": unit -> int;
		  val yday = _ffi "Date_Tm_yday": unit -> int;
		  val isdst = _ffi "Date_Tm_isdst": unit -> int;

		  val setSec = _ffi "Date_Tm_setSec": int -> unit;
		  val setMin = _ffi "Date_Tm_setMin": int -> unit;
		  val setHour = _ffi "Date_Tm_setHour": int -> unit;
		  val setMday = _ffi "Date_Tm_setMday": int -> unit;
		  val setMon = _ffi "Date_Tm_setMon": int -> unit;
		  val setYear = _ffi "Date_Tm_setYear": int -> unit;
		  val setWday = _ffi "Date_Tm_setWday": int -> unit;
		  val setYday = _ffi "Date_Tm_setYday": int -> unit;
		  val setIsdst = _ffi "Date_Tm_setIsdst": int -> unit;
	       end
	       
	    val ascTime = _ffi "Date_ascTime": unit -> cstring;
	    val gmTime = _ffi "Date_gmTime": time ref -> unit;
	    val localOffset = _ffi "Date_localOffset": unit -> int;
	    val localTime = _ffi "Date_localTime": time ref -> unit;
	    val mkTime = _ffi "Date_mkTime": unit -> time;
	    val strfTime =
	       _ffi "Date_strfTime": char array * size * nullString -> size;
	 end

      structure Debug = Primitive.Debug

      structure Exn =
	 struct
	    (* The polymorphism with extra and setInitExtra is because primitives
	     * are only supposed to deal with basic types.  The polymorphism
	     * allows the various passes like monomorphisation to translate
	     * the types appropriately.
	     *)
	    type extra = string list

	    val extra = fn x => _prim "Exn_extra": exn -> 'a; x
	    val extra: exn -> extra = extra
	    val name = _prim "Exn_name": exn -> string;
	    val keepHistory = _build_const "Exn_keepHistory": bool;
	    val setExtendExtra =
	       fn x => _prim "Exn_setExtendExtra": (string * 'a -> 'a) -> unit; x
	    val setExtendExtra: (string * extra -> extra) -> unit =
	       setExtendExtra
	    val setInitExtra = fn x => _prim "Exn_setInitExtra": 'a -> unit; x
	    val setInitExtra: extra -> unit = setInitExtra
	    val setTopLevelHandler =
	       _prim "Exn_setTopLevelHandler": (exn -> unit) -> unit;
	 end

      structure FFI =
	 struct
	    val getBool = _ffi "MLton_FFI_getBool": int -> bool;
	    val getChar = _ffi "MLton_FFI_getChar": int -> char;
	    val getInt8 = _ffi "MLton_FFI_getInt8": int -> Int8.int;
	    val getInt16 = _ffi "MLton_FFI_getInt16": int -> Int16.int;
	    val getInt32 = _ffi "MLton_FFI_getInt32": int -> Int32.int;
	    val getInt64 = _ffi "MLton_FFI_getInt64": int -> Int64.int;
	    val getOp = _ffi "MLton_FFI_getOp": unit -> int;
	    val getPointer = fn z => _prim "FFI_getPointer": int -> 'a; z
	    val getReal32 = _ffi "MLton_FFI_getReal32": int -> Real32.real;
	    val getReal64 = _ffi "MLton_FFI_getReal64": int -> Real64.real;
	    val getWord8 = _ffi "MLton_FFI_getWord8": int -> Word8.word;
	    val getWord16 = _ffi "MLton_FFI_getWord16": int -> Word16.word;
	    val getWord32 = _ffi "MLton_FFI_getWord32": int -> Word32.word;
	    val numExports = _build_const "MLton_FFI_numExports": int;
	    val setBool = _ffi "MLton_FFI_setBool": bool -> unit;
	    val setChar = _ffi "MLton_FFI_setChar": char -> unit;
	    val setInt8 = _ffi "MLton_FFI_setInt8": Int8.int -> unit;
	    val setInt16 = _ffi "MLton_FFI_setInt16": Int16.int -> unit;
	    val setInt32 = _ffi "MLton_FFI_setInt32": Int32.int -> unit;
	    val setInt64 = _ffi "MLton_FFI_setInt64": Int64.int -> unit;
	    val setPointer = fn z => _prim "FFI_setPointer": 'a -> unit; z
	    val setReal32 = _ffi "MLton_FFI_setReal32": Real32.real -> unit;
	    val setReal64 = _ffi "MLton_FFI_setReal64": Real64.real -> unit;
  	    val setWord8 = _ffi "MLton_FFI_setWord8": Word8.word -> unit;
	    val setWord16 = _ffi "MLton_FFI_setWord16": Word16.word -> unit;
	    val setWord32 = _ffi "MLton_FFI_setWord32": Word32.word -> unit;
	 end

      structure GC =
	 struct
	    val collect = _prim "GC_collect": unit -> unit;
	    val pack = _prim "GC_pack": unit -> unit;
	    val setMessages = _ffi "GC_setMessages": bool -> unit;
	    val setSummary = _ffi "GC_setSummary": bool -> unit;
	    val unpack = _prim "GC_unpack": unit -> unit;
	 end
      
      structure IEEEReal =
	 struct
	    val getRoundingMode = _ffi "IEEEReal_getRoundingMode": unit -> int;
	    val setRoundingMode = _ffi "IEEEReal_setRoundingMode": int -> unit;
	 end

      structure Int8 =
	 struct
	    type int = Int8.int
	       
	    val precision' : Int.int = 8
	    val maxInt' : int = 0x7f
	    val minInt' : int = ~0x80

	    val *? = _prim "Int8_mul": int * int -> int;
	    val * =
	       if detectOverflow
		  then _prim "Int8_mulCheck": int * int -> int;
	       else *?
	    val +? = _prim "Int8_add": int * int -> int;
	    val + =
	       if detectOverflow
		  then _prim "Int8_addCheck": int * int -> int;
	       else +?
	    val -? = _prim "Int8_sub": int * int -> int;
	    val - =
	       if detectOverflow
		  then _prim "Int8_subCheck": int * int -> int;
	       else -?
	    val < = _prim "Int8_lt": int * int -> bool;
	    val <= = _prim "Int8_le": int * int -> bool;
	    val > = _prim "Int8_gt": int * int -> bool;
	    val >= = _prim "Int8_ge": int * int -> bool;
	    val quot = _prim "Int8_quot": int * int -> int;
	    val rem = _prim "Int8_rem": int * int -> int;
	    val ~? = _prim "Int8_neg": int -> int; 
	    val ~ =
	       if detectOverflow
		  then _prim "Int8_negCheck": int -> int;
	       else ~?
	    val fromInt = _prim "Int32_toInt8": Int.int -> int;
	    val toInt = _prim "Int8_toInt32": int -> Int.int;
	 end
      structure Int16 =
	 struct
	    type int = Int16.int
	       
	    val precision' : Int.int = 16
	    val maxInt' : int = 0x7fff
	    val minInt' : int = ~0x8000

	    val *? = _prim "Int16_mul": int * int -> int;
	    val * =
	       if detectOverflow
		  then _prim "Int16_mulCheck": int * int -> int;
	       else *?
	    val +? = _prim "Int16_add": int * int -> int;
	    val + =
	       if detectOverflow
		  then _prim "Int16_addCheck": int * int -> int;
	       else +?
	    val -? = _prim "Int16_sub": int * int -> int;
	    val - =
	       if detectOverflow
		  then _prim "Int16_subCheck": int * int -> int;
	       else -?
	    val < = _prim "Int16_lt": int * int -> bool;
	    val <= = _prim "Int16_le": int * int -> bool;
	    val > = _prim "Int16_gt": int * int -> bool;
	    val >= = _prim "Int16_ge": int * int -> bool;
	    val quot = _prim "Int16_quot": int * int -> int;
	    val rem = _prim "Int16_rem": int * int -> int;
	    val ~? = _prim "Int16_neg": int -> int; 
	    val ~ =
	       if detectOverflow
		  then _prim "Int16_negCheck": int -> int;
	       else ~?
	    val fromInt = _prim "Int32_toInt16": Int.int -> int;
	    val toInt = _prim "Int16_toInt32": int -> Int.int;
	 end
      structure Int32 =
	 struct
	    type int = Int32.int
	    val precision' : Int.int = 32
	    val maxInt' : int = 0x7fffffff
	    val minInt' : int = ~0x80000000

	    val *? = _prim "Int32_mul": int * int -> int;
	    val * =
	       if detectOverflow
		  then _prim "Int32_mulCheck": int * int -> int;
	       else *?
	    val +? = _prim "Int32_add": int * int -> int;
	    val + =
	       if detectOverflow
		  then _prim "Int32_addCheck": int * int -> int;
	       else +?
	    val -? = _prim "Int32_sub": int * int -> int;
	    val - =
	       if detectOverflow
		  then _prim "Int32_subCheck": int * int -> int;
	       else -?
	    val < = _prim "Int32_lt": int * int -> bool;
	    val <= = _prim "Int32_le": int * int -> bool;
	    val > = _prim "Int32_gt": int * int -> bool;
	    val >= = _prim "Int32_ge": int * int -> bool;
	    val quot = _prim "Int32_quot": int * int -> int;
	    val rem = _prim "Int32_rem": int * int -> int;
	    val ~? = _prim "Int32_neg": int -> int; 
	    val ~ =
	       if detectOverflow
		  then _prim "Int32_negCheck": int -> int;
	       else ~?
	    val fromInt : int -> int = fn x => x
	    val toInt : int -> int = fn x => x
	 end
      structure Int = Int32
      structure Int64 =
	 struct
	    infix 7 *?
	    infix 6 +? -?
	    infix 4 = <> > >= < <=

	    type int = Int64.int

	    val precision' : Int.int = 64
	    val maxInt' : int = 0x7FFFFFFFFFFFFFFF
	    val minInt' : int = ~0x8000000000000000

	    val op +? = _ffi "Int64_add": int * int -> int;
	    val op *? = _ffi "Int64_mul": int * int -> int;
	    val op -? = _ffi "Int64_sub": int * int -> int;
	    val ~? = fn i => 0 -? i
	    val op < = _ffi "Int64_lt": int * int -> bool;
	    val op <= = _ffi "Int64_le": int * int -> bool;
	    val op > = _ffi "Int64_gt": int * int -> bool;
	    val op >= = _ffi "Int64_ge": int * int -> bool;
	    val quot = _ffi "Int64_quot": int * int -> int;
	    val rem = _ffi "Int64_rem": int * int -> int;
	    val geu = _ffi "Int64_geu": int * int -> bool;
	    val gtu = _ffi "Int64_gtu": int * int -> bool;
	    val fromInt = _ffi "Int32_toInt64": Int.int -> int;
	    val fromWord = _ffi "Word32_toInt64": word -> int;
	    val toInt = _ffi "Int64_toInt32": int -> Int.int;
	    val toWord = _ffi "Int64_toWord32": int -> word;

	    val ~ =
	       if detectOverflow
		  then (fn i: int => if i = minInt'
					then raise Overflow
				     else ~? i)
	       else ~?
		  
	    val + =
	       if detectOverflow
		  then
		     fn (i, j) =>
		     if (if i >= 0
			    then j > maxInt' -? i
			 else j < minInt' -? i)
			then raise Overflow
		     else i +? j
	       else op +?

	    val - =
	       if detectOverflow
		  then
		     fn (i, j) =>
		     if (if i >= 0
			    then j < i -? maxInt'
			 else j > i -? minInt')
			then raise Overflow
		     else i -? j
	       else op -?

	    val * = fn _ => raise Fail "Int64.* unimplemented"
	 end

      structure Array =
	 struct
	    open Array

	    val array = fn n => _prim "Array_array": int -> 'a array; n
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
	       _ffi "IntInf_smallMul": word * word * word ref -> word;
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
	    val set = _ffi "Itimer_set": which * int * int * int * int -> unit;
	    val virtual = _const "Itimer_virtual": which;
	 end

      structure MLton =
	 struct
	    val native = _build_const "MLton_native": bool;
(*       val deserialize = _prim "MLton_deserialize": Word8Vector.vector -> 'a ref; *)
(*       val serialize = _prim "MLton_serialize": 'a ref -> Word8Vector.vector; *)
	    val size = fn x => _prim "MLton_size": 'a ref -> int; x

	    structure Platform =
	       struct
		  datatype arch = Sparc | X86

		  val arch: arch =
		     case _const "MLton_Platform_arch": int; of
			0 => Sparc
		      | 1 => X86
		      | _ => raise Fail "strange MLton_Platform_arch"
			   
		  datatype os = Cygwin | FreeBSD | Linux | SunOS

		  val os: os =
		     case _const "MLton_Platform_os": int; of
			0 => Cygwin
		      | 1 => FreeBSD
		      | 2 => Linux
		      | 3 => SunOS
		      | _ => raise Fail "strange MLton_Platform_os"

		  val isBigEndian =
		     case arch of
			X86 => false
		      | Sparc => true
	       end

	    structure Profile =
	       struct
		  val isOn = _build_const "MLton_profile_isOn": bool;
		  structure Data =
		     struct
		        type t = word

			val dummy:t = 0w0
			val free = _ffi "MLton_Profile_Data_free": t -> unit;
			val malloc = _ffi "MLton_Profile_Data_malloc": unit -> t;
			val write =
			   _ffi "MLton_Profile_Data_write"
			   : t * word (* fd *) -> unit;
		     end
		  val current = _ffi "MLton_Profile_current": unit -> Data.t;
		  val done = _ffi "MLton_Profile_done": unit -> unit;
		  val setCurrent =
		     _ffi "MLton_Profile_setCurrent": Data.t -> unit;
	       end
	    
	    structure Rlimit =
	       struct
		  type rlim = word
		     
		  val infinity = _const "MLton_Rlimit_infinity": rlim;

		  type resource = int

		  val cpuTime =
		     _const "MLton_Rlimit_cpuTime": resource;
		  val coreFileSize =
		     _const "MLton_Rlimit_coreFileSize": resource;
		  val dataSize =
		     _const "MLton_Rlimit_dataSize": resource;
		  val fileSize =
		     _const "MLton_Rlimit_fileSize": resource;
		  val lockedInMemorySize =
		     _const "MLton_Rlimit_lockedInMemorySize": resource;
		  val numFiles =
		     _const "MLton_Rlimit_numFiles": resource;
		  val numProcesses =
		     _const "MLton_Rlimit_numProcesses": resource;
		  val residentSetSize =
		     _const "MLton_Rlimit_residentSetSize": resource;
		  val stackSize =
		     _const "MLton_Rlimit_stackSize": resource;
		  val virtualMemorySize =
		     _const "MLton_Rlimit_virtualMemorySize": resource;
		     
		  val get = _ffi "MLton_Rlimit_get": resource -> int;
		  val getHard = _ffi "MLton_Rlimit_getHard": unit -> rlim;
		  val getSoft = _ffi "MLton_Rlimit_getSoft": unit -> rlim;
		  val set =
		     _ffi "MLton_Rlimit_set": resource * rlim * rlim -> int;
	       end
	    
	    structure Rusage =
               struct
		 val ru = _ffi "MLton_Rusage_ru": unit -> unit;
		 val self_utime_sec = _ffi "MLton_Rusage_self_utime_sec": unit -> int;
		 val self_utime_usec = _ffi "MLton_Rusage_self_utime_usec": unit -> int;
		 val self_stime_sec = _ffi "MLton_Rusage_self_stime_sec": unit -> int;
		 val self_stime_usec = _ffi "MLton_Rusage_self_stime_usec": unit -> int;
		 val children_utime_sec = _ffi "MLton_Rusage_children_utime_sec": unit -> int;
		 val children_utime_usec = _ffi "MLton_Rusage_children_utime_usec": unit -> int;
		 val children_stime_sec = _ffi "MLton_Rusage_children_stime_sec": unit -> int;
		 val children_stime_usec = _ffi "MLton_Rusage_children_stime_usec": unit -> int;
		 val gc_utime_sec = _ffi "MLton_Rusage_gc_utime_sec": unit -> int;
		 val gc_utime_usec = _ffi "MLton_Rusage_gc_utime_usec": unit -> int;
		 val gc_stime_sec = _ffi "MLton_Rusage_gc_stime_sec": unit -> int;
		 val gc_stime_usec = _ffi "MLton_Rusage_gc_stime_usec": unit -> int;
	       end

	    structure Process =
	       struct
		  val spawne =
		     _ffi "MLton_Process_spawne"
		     : nullString * nullString array * nullString array -> int;
		  val spawnp =
		     _ffi "MLton_Process_spawnp"
		     : nullString * nullString array -> int;
	       end
	    
	    structure Weak =
	       struct
		  type 'a t = 'a weak
		     
		  val canGet = fn x => _prim "Weak_canGet": 'a t -> bool; x
		  val get = fn x => _prim "Weak_get": 'a t -> 'a; x
		  val new = fn x => _prim "Weak_new" : 'a -> 'a t; x
	       end
	 end

      structure Net =
	 struct
 	    val htonl = _ffi "Net_htonl": int -> int;
	    val ntohl = _ffi "Net_ntohl": int -> int;
 	    val htons = _ffi "Net_htons": int -> int;
	    val ntohs = _ffi "Net_ntohs": int -> int;
	 end

      structure NetHostDB =
	 struct
	    (* network byte order (MSB) *)
	    type pre_in_addr = word8 array
	    type in_addr = word8 vector
	    val inAddrLen = _const "NetHostDB_inAddrLen": int;
	    val INADDR_ANY = _const "NetHostDB_INADDR_ANY": int;
	    type addr_family = int
	    val entryName = _ffi "NetHostDB_Entry_name": unit -> cstring;
	    val entryNumAliases = _ffi "NetHostDB_Entry_numAliases": unit -> int;
	    val entryAliasesN = _ffi "NetHostDB_Entry_aliasesN": int -> cstring;
	    val entryAddrType = _ffi "NetHostDB_Entry_addrType": unit -> int;
	    val entryLength = _ffi "NetHostDB_Entry_length": unit -> int;
	    val entryNumAddrs = _ffi "NetHostDB_Entry_numAddrs": unit -> int;
	    val entryAddrsN =
	       _ffi "NetHostDB_Entry_addrsN": int * pre_in_addr -> unit;
	    val getByAddress =
	       _ffi "NetHostDB_getByAddress": in_addr * int -> bool;
	    val getByName = _ffi "NetHostDB_getByName": nullString -> bool;
	    val getHostName =
	       _ffi "NetHostDB_getHostName": char array * int -> int;
	 end

      structure NetProtDB =
	 struct
	    val entryName = _ffi "NetProtDB_Entry_name": unit -> cstring;
	    val entryNumAliases = _ffi "NetProtDB_Entry_numAliases": unit -> int;
	    val entryAliasesN = _ffi "NetProtDB_Entry_aliasesN": int -> cstring;
	    val entryProtocol = _ffi "NetProtDB_Entry_protocol": unit -> int;
	    val getByName = _ffi "NetProtDB_getByName": nullString -> bool;
	    val getByNumber = _ffi "NetProtDB_getByNumber": int -> bool;
	 end

      structure NetServDB =
	 struct
	    val entryName = _ffi "NetServDB_Entry_name": unit -> cstring;
	    val entryNumAliases = _ffi "NetServDB_Entry_numAliases": unit -> int;
	    val entryAliasesN = _ffi "NetServDB_Entry_aliasesN": int -> cstring;
	    val entryPort = _ffi "NetServDB_Entry_port": unit -> int;
	    val entryProtocol = _ffi "NetServDB_Entry_protocol": unit -> cstring;
	    val getByName = _ffi "NetServDB_getByName": nullString * nullString -> bool;
	    val getByNameNull = _ffi "NetServDB_getByNameNull": nullString -> bool;
	    val getByPort = _ffi "NetServDB_getByPort": int * nullString -> bool;
	    val getByPortNull = _ffi "NetServDB_getByPortNull": int -> bool;
	 end

      structure OS =
	 struct
	    structure FileSys =
	       struct
		  val tmpnam = _ffi "OS_FileSys_tmpnam": unit -> cstring;
	       end
	    structure IO =
	       struct
		  val POLLIN = _const "OS_IO_POLLIN": word;
		  val POLLPRI = _const "OS_IO_POLLPRI": word;
		  val POLLOUT = _const "OS_IO_POLLOUT": word;
		  val poll = _ffi "OS_IO_poll": int vector * word vector * 
                                                int * int * word array -> int;
	       end
	 end

      structure PackReal =
	 struct
	    val subVec = _ffi "PackReal_subVec": word8 vector * int -> real;
	    val update =
	       _ffi "PackReal_update": word8 array * int * real -> unit;
	 end

      structure Ptrace =
	 struct
	    type pid = int
	       
	    (*
	     * These constants are from
	     *   /usr/include/linux/ptrace.h
	     *   /usr/src/linux/include/asm-i386/ptrace.h
	     *)
	    val TRACEME = _const "Ptrace_TRACEME": int;
	    val PEEKTEXT = _const "Ptrace_PEEKTEXT": int;
	    val PEEKDATA = _const "Ptrace_PEEKDATA": int;
(*	    val PEEKUSR = _const "Ptrace_PEEKUSR": int; *)
	    val POKETEXT = _const "Ptrace_POKETEXT": int;
	    val POKEDATA = _const "Ptrace_POKEDATA": int;
(*	    val POKEUSR = _const "Ptrace_POKEUSR": int; *)
	    val CONT = _const "Ptrace_CONT": int;
	    val KILL = _const "Ptrace_KILL": int;
	    val SINGLESTEP = _const "Ptrace_SINGLESTEP": int;
	    val ATTACH = _const "Ptrace_ATTACH": int;
	    val DETACH = _const "Ptrace_DETACH": int;
	    val GETREGS = _const "Ptrace_GETREGS": int;
	    val SETREGS = _const "Ptrace_SETREGS": int;
	    val GETFPREGS = _const "Ptrace_GETFPREGS": int;
	    val SETFPREGS = _const "Ptrace_SETFPREGS": int;
	    val SYSCALL = _const "Ptrace_SYSCALL": int;

	    val ptrace2 = _ffi "Ptrace_ptrace2": int * pid -> int;
	    val ptrace4 =
	       _ffi "Ptrace_ptrace4": int * pid * word * word ref -> int;
	 end

      structure Real =
	 struct
	    type real = real64

	    structure Math =
	       struct
		  type real = real
		     
		  val acos = _prim "Real64_Math_acos": real -> real;
		  val asin = _prim "Real64_Math_asin": real -> real;
		  val atan = _prim "Real64_Math_atan": real -> real;
		  val atan2 = _prim "Real64_Math_atan2": real * real -> real;
		  val cos = _prim "Real64_Math_cos": real -> real;
		  val cosh = _ffi "cosh": real -> real;
		  val e = _ffi "Real64_Math_e": real;
		  val exp = _prim "Real64_Math_exp": real -> real;
		  val ln = _prim "Real64_Math_ln": real -> real;
		  val log10 = _prim "Real64_Math_log10": real -> real;
		  val pi = _ffi "Real64_Math_pi": real;
		  val pow = _ffi "pow": real * real -> real;
		  val sin = _prim "Real64_Math_sin": real -> real;
		  val sinh = _ffi "sinh": real -> real;
		  val sqrt = _prim "Real64_Math_sqrt": real -> real;
		  val tan = _prim "Real64_Math_tan": real -> real;
		  val tanh = _ffi "tanh": real -> real;
	       end

	    val * = _prim "Real64_mul": real * real -> real;
	    val *+ = _prim "Real64_muladd": real * real * real -> real;
	    val *- = _prim "Real64_mulsub": real * real * real -> real;
	    val + = _prim "Real64_add": real * real -> real;
	    val - = _prim "Real64_sub": real * real -> real;
	    val / = _prim "Real64_div": real * real -> real;
	    val < = _prim "Real64_lt": real * real -> bool;
	    val <= = _prim "Real64_le": real * real -> bool;
	    val == = _prim "Real64_equal": real * real -> bool;
	    val > = _prim "Real64_gt": real * real -> bool;
	    val >= = _prim "Real64_ge": real * real -> bool;
	    val ?= = _prim "Real64_qequal": real * real -> bool;
	    val abs = _prim "Real64_abs": real -> real;
	    val class = _ffi "Real64_class": real -> int;
	    val copySign = _ffi "copysign": real * real -> real;
	    val frexp = _ffi "frexp": real * int ref -> real;
	    val gdtoa =
	       _ffi "Real64_gdtoa": real * int * int * int ref -> cstring;
	    val fromInt = _prim "Int32_toReal64": int -> real;
	    val isFinite = _ffi "Real64_isFinite": real -> bool;
	    val isNan = _ffi "Real64_isNan": real -> bool;
	    val isNormal = _ffi "Real64_isNormal": real -> bool;
	    val ldexp = _prim "Real64_ldexp": real * int -> real;
	    val maxFinite = _ffi "Real64_maxFinite": real;
	    val minNormalPos = _ffi "Real64_minNormalPos": real;
	    val minPos = _ffi "Real64_minPos": real;
	    val modf = _ffi "modf": real * real ref -> real;
	    val nextAfter = _ffi "Real64_nextAfter": real * real -> real;
	    val round = _prim "Real64_round": real -> real;
	    val signBit = _ffi "Real64_signBit": real -> bool;
	    val strtod = _ffi "Real64_strtod": nullString -> real;
	    val toInt = _prim "Real64_toInt": real -> int;
	    val ~ = _prim "Real64_neg": real -> real;
	 end
      
      structure Ref =
	 struct
	    val deref = fn x => _prim "Ref_deref": 'a ref -> 'a; x
	    val assign = fn x => _prim "Ref_assign": 'a ref * 'a -> unit; x
	 end

      structure Socket =
	 struct
	    type sock = int
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
	    structure SOCK =
	       struct
		  type sock_type = int
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
		     _ffi "Socket_Ctl_setSockOpt": sock * level * optname * 
		                                   read_data * int -> 
                                                   int;
		  val getSockOpt = 
		     _ffi "Socket_Ctl_getSockOpt": sock * level * optname * 
		                                   write_data * int ref -> 
                                                   int;
		  val setIOCtl =
		     _ffi "Socket_Ctl_getsetIOCtl": sock * request *
		                                    read_data ->
						    int;
		  val getIOCtl =
		     _ffi "Socket_Ctl_getsetIOCtl": sock * request *
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
		     _ffi "Socket_Ctl_getPeerName": sock * pre_sock_addr * int ref -> int;
		  val getSockName =
		     _ffi "Socket_Ctl_getSockName": sock * pre_sock_addr * int ref -> int;

		  val NBIO = _const "Socket_Ctl_FIONBIO": request;
		  val NREAD = _const "Socket_Ctl_FIONREAD": request;
		  val ATMARK = _const "Socket_Ctl_SIOCATMARK": request;
	       end

	    val familyOfAddr = _ffi "Socket_familyOfAddr": sock_addr -> AF.addr_family;
	    val bind = _ffi "Socket_bind": sock * sock_addr * int -> int;
	    val listen = _ffi "Socket_listen": sock * int -> int;
	    val connect = _ffi "Socket_connect": sock * sock_addr * int -> int;
	    val accept = _ffi "Socket_accept": sock * pre_sock_addr * int ref -> int;
	    val close = _ffi "Socket_close": sock -> int;

	    type how = int
	    val SHUT_RD = _const "Socket_SHUT_RD": how;
	    val SHUT_WR = _const "Socket_SHUT_WR": how;
	    val SHUT_RDWR = _const "Socket_SHUT_RDWR": how;
	    val shutdown = _ffi "Socket_shutdown": sock * how -> int;

	    type flags = word
	    val MSG_DONTROUTE = _const "Socket_MSG_DONTROUTE": flags;
	    val MSG_OOB = _const "Socket_MSG_OOB": flags;
	    val MSG_PEEK = _const "Socket_MSG_PEEK": flags;

	    val send = _ffi "Socket_send": sock * word8 vector * 
                                           int * int * word -> int;
	    val sendTo = _ffi "Socket_sendTo": sock * word8 vector * 
                                               int * int * word *
                                               sock_addr * int -> int;
	    val recv = _ffi "Socket_recv": sock * word8 array * 
                                           int * int * word -> int;
	    val recvFrom = _ffi "Socket_recvFrom": sock * word8 array * 
	                                           int * int * word *
                                                   pre_sock_addr * int ref -> int;

	    structure GenericSock =
	       struct
		  val socket = 
		     _ffi "GenericSock_socket": AF.addr_family * 
		                                SOCK.sock_type * 
						int -> int;
		  val socketPair = 
		     _ffi "GenericSock_socketPair": AF.addr_family * 
		                                    SOCK.sock_type * 
						    int * 
						    int ref * int ref -> int;
	       end

	    structure INetSock =
	       struct
		  val toAddr = _ffi "INetSock_toAddr": NetHostDB.in_addr * int * 
                                                       pre_sock_addr * int ref -> unit;
		  val fromAddr = _ffi "INetSock_fromAddr": sock_addr -> unit;
		  val getInAddr = _ffi "INetSock_getInAddr": NetHostDB.pre_in_addr -> 
                                                             unit;
		  val getPort = _ffi "INetSock_getPort": unit -> int;
		  structure UDP =
		     struct
		     end
		  structure TCP =
		     struct
		        open CtlExtra
		        val TCP = _const "Socket_INetSock_TCP_SOL_TCP": level;
			val NODELAY = _const "Socket_INetSock_TCP_SO_NODELAY": optname;
		     end
	       end
	    structure UnixSock =
	       struct
		  val toAddr = _ffi "UnixSock_toAddr": nullString * int *
                                                       pre_sock_addr * int ref -> unit;
		  val pathLen = _ffi "UnixSock_pathLen": sock_addr -> int;
		  val fromAddr =
		     _ffi "UnixSock_fromAddr"
		     : sock_addr * char array * int -> unit;
		  structure Strm =
		     struct
		     end
		  structure DGrm =
		     struct
		     end
	       end
	 end

      structure String =
	 struct
	    val fromWord8Vector =
	       _prim "Word8Vector_toString": word8 vector -> string;
	    val toWord8Vector =
	       _prim "String_toWord8Vector": string -> word8 vector;
	 end

      structure TextIO =
	 struct
	    val bufSize = _build_const "TextIO_bufSize": int;
	 end
      
      structure Thread =
	 struct
	    type preThread = preThread
	    type thread = thread

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
	    val current = _ffi "Thread_current": unit -> thread;
	    val finishHandler = _ffi "Thread_finishHandler": unit -> unit;
	    val returnToC = _prim "Thread_returnToC": unit -> unit;
	    val saved = _ffi "Thread_saved": unit -> thread;
	    val savedPre = _ffi "Thread_saved": unit -> preThread;
	    val setCallFromCHandler =
	       _ffi "Thread_setCallFromCHandler": thread -> unit;
	    val setHandler = _ffi "Thread_setHandler": thread -> unit;
	    val setSaved = _ffi "Thread_setSaved": thread -> unit;
	    val startHandler = _ffi "Thread_startHandler": unit -> unit;
	    val switchTo = _prim "Thread_switchTo": thread -> unit;
	 end      

      structure Time =
	 struct
	    val gettimeofday = _ffi "Time_gettimeofday": unit -> int;
	    val sec = _ffi "Time_sec": unit -> int;
	    val usec = _ffi "Time_usec": unit -> int;
	 end

      structure Vector =
	 struct
	    val sub = fn x => _prim "Vector_sub": 'a vector * int -> 'a; x
	    val length = fn x => _prim "Vector_length": 'a vector -> int; x

	    (* Don't mutate the array after you apply fromArray, because vectors
	     * are supposed to be immutable and the optimizer depends on this.
	     *)
	    val fromArray =
	       fn x => _prim "Array_toVector": 'a array -> 'a vector; x
	 end

      structure Word8 =
	 struct
	    type word = word8
	    val wordSize: int = 8

	    val + = _prim "Word8_add": word * word -> word;
	    val addCheck = _prim "Word8_addCheck": word * word -> word;
	    val andb = _prim "Word8_andb": word * word -> word;
	    val ~>> = _prim "Word8_arshift": word * word32 -> word;
	    val div = _prim "Word8_div": word * word -> word;
	    val fromInt = _prim "Int32_toWord8": int -> word;
	    val fromLargeWord = _prim "Word32_toWord8": word32 -> word;
	    val >= = _prim "Word8_ge": word * word -> bool;
	    val > = _prim "Word8_gt" : word * word -> bool;
	    val <= = _prim "Word8_le": word * word -> bool;
	    val << = _prim "Word8_lshift": word * word32 -> word;
	    val < = _prim "Word8_lt" : word * word -> bool;
	    val mod = _prim "Word8_mod": word * word -> word;
	    val * = _prim "Word8_mul": word * word -> word;
	    val mulCheck = _prim "Word8_mulCheck": word * word -> word;
	    val ~ = _prim "Word8_neg": word -> word;
	    val notb = _prim "Word8_notb": word -> word;
	    val orb = _prim "Word8_orb": word * word -> word;
	    val rol = _prim "Word8_rol": word * word32 -> word;
	    val ror = _prim "Word8_ror": word * word32 -> word;
	    val >> = _prim "Word8_rshift": word * word32 -> word;
	    val - = _prim "Word8_sub": word * word -> word;
	    val toInt = _prim "Word8_toInt32": word -> int;
	    val toIntX = _prim "Word8_toInt32X": word -> int;
	    val toLargeWord = _prim "Word8_toWord32": word -> word32;
	    val toLargeWordX = _prim "Word8_toWord32X": word -> word32;
	    val xorb = _prim "Word8_xorb": word * word -> word;
	 end

      structure Word8Array =
	 struct
	    val subWord =
	       _prim "Word8Array_subWord": word8 array * int -> word;
	    val updateWord =
	       _prim "Word8Array_updateWord": word8 array * int * word -> unit;
	 end
      
      structure Word8Vector =
	 struct
	    val subWord =
	       _prim "Word8Vector_subWord": word8 vector * int -> word;
	 end

      structure Word16 =
	 struct
	    type word = word16
	    val wordSize: int = 16

	    val + = _prim "Word16_add": word * word -> word;
	    val addCheck = _prim "Word16_addCheck": word * word -> word;
	    val andb = _prim "Word16_andb": word * word -> word;
	    val ~>> = _prim "Word16_arshift": word * word32 -> word;
	    val div = _prim "Word16_div": word * word -> word;
	    val fromInt = _prim "Int32_toWord16": int -> word;
	    val fromLargeWord = _prim "Word32_toWord16": word32 -> word;
	    val >= = _prim "Word16_ge": word * word -> bool;
	    val > = _prim "Word16_gt" : word * word -> bool;
	    val <= = _prim "Word16_le": word * word -> bool;
	    val << = _prim "Word16_lshift": word * word32 -> word;
	    val < = _prim "Word16_lt" : word * word -> bool;
	    val mod = _prim "Word16_mod": word * word -> word;
	    val * = _prim "Word16_mul": word * word -> word;
	    val mulCheck = _prim "Word16_mulCheck": word * word -> word;
	    val ~ = _prim "Word16_neg": word -> word;
	    val notb = _prim "Word16_notb": word -> word;
	    val orb = _prim "Word16_orb": word * word -> word;
	    val rol = _prim "Word16_rol": word * word32 -> word;
	    val ror = _prim "Word16_ror": word * word32 -> word;
	    val >> = _prim "Word16_rshift": word * word32 -> word;
	    val - = _prim "Word16_sub": word * word -> word;
	    val toInt = _prim "Word16_toInt32": word -> int;
	    val toIntX = _prim "Word16_toInt32X": word -> int;
	    val toLargeWord = _prim "Word16_toWord32": word -> word32;
	    val toLargeWordX = _prim "Word16_toWord32X": word -> word32;
	    val xorb = _prim "Word16_xorb": word * word -> word;
	 end

      structure Word32 =
	 struct
	    type word = word32
	    val wordSize: int = 32

	    val + = _prim "Word32_add": word * word -> word;
	    val addCheck = _prim "Word32_addCheck": word * word -> word;
	    val andb = _prim "Word32_andb": word * word -> word;
	    val ~>> = _prim "Word32_arshift": word * word -> word;
	    val div = _prim "Word32_div": word * word -> word;
	    val fromInt = _prim "Int32_toWord32": int -> word;
	    val fromLargeWord : word -> word = fn x => x
	    val >= = _prim "Word32_ge": word * word -> bool;
	    val > = _prim "Word32_gt" : word * word -> bool;
	    val <= = _prim "Word32_le": word * word -> bool;
	    val << = _prim "Word32_lshift": word * word -> word;
	    val < = _prim "Word32_lt" : word * word -> bool;
	    val mod = _prim "Word32_mod": word * word -> word;
	    val * = _prim "Word32_mul": word * word -> word;
	    val mulCheck = _prim "Word32_mulCheck": word * word -> word;
	    val ~ = _prim "Word32_neg": word -> word;
	    val notb = _prim "Word32_notb": word -> word;
	    val orb = _prim "Word32_orb": word * word -> word;
	    val rol = _prim "Word32_rol": word * word -> word;
	    val ror = _prim "Word32_ror": word * word -> word;
	    val >> = _prim "Word32_rshift": word * word -> word;
	    val - = _prim "Word32_sub": word * word -> word;
	    val toInt = _prim "Word32_toInt32": word -> int;
	    val toIntX = _prim "Word32_toInt32X": word -> int;
	    val toLargeWord : word -> word = fn x => x
	    val toLargeWordX : word -> word = fn x => x
	    val xorb = _prim "Word32_xorb": word * word -> word;
	 end
      structure Word = Word32

      structure World =
	 struct
	    val isOriginal = _ffi "World_isOriginal": unit -> bool;
	    val makeOriginal = _ffi "World_makeOriginal": unit -> unit;
	    val save = _prim "World_save": word (* filedes *) -> unit;
	 end
   end

structure Primitive =
   struct
      open Primitive

      structure Int8 =
	 struct
	    open Int8
	       
	    local
	       fun make f (i: int, i': int): bool =
		  f (Primitive.Word8.fromInt i, Primitive.Word8.fromInt i')
	    in
	       val geu = make Primitive.Word8.>=
	       val gtu = make Primitive.Word8.> 
	    end
	 end
      structure Int16 =
	 struct
	    open Int16
	       
	    local
	       fun make f (i: int, i': int): bool =
		  f (Primitive.Word16.fromInt i, Primitive.Word16.fromInt i')
	    in
	       val geu = make Primitive.Word16.>=
	       val gtu = make Primitive.Word16.> 
	    end
	 end
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
