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
datatype bool = datatype bool
type char = char
type exn = exn
type int = int
type intInf = intInf
datatype list = datatype list
type nullString = string
type pointer = pointer (* C integer, not SML heap pointer *)
type real = real
datatype ref = datatype ref
type string = string
type preThread = preThread
type thread = thread
type word = word
type word8 = word8
type word32 = word
type 'a vector = 'a vector

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
      val debug = _build_const "MLton_debug": bool;
      val detectOverflow = _build_const "MLton_detectOverflow": bool;
      val eq = fn z => _prim "MLton_eq": 'a * 'a -> bool; z
      val errno = _ffi "MLton_errno": unit -> int;
      val halt = _prim "MLton_halt": int -> unit;
      val handlesSignals = _prim "MLton_handlesSignals": bool;
      val installSignalHandler = _prim "MLton_installSignalHandler": unit -> unit;
      val isLittleEndian = _const "MLton_isLittleEndian": bool;
      val safe = _build_const "MLton_safe": bool;
      val usesCallcc: bool ref = ref false;

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
		  type cs = pointer

		  val sub = _ffi "C_CS_sub": cs * int -> char;
		  val update =
		     _ffi "C_CS_update": cs * int * char -> unit; (* primitive *)
		  val charArrayToWord8Array =
		     _prim "C_CS_charArrayToWord8Array":
		     char array -> word8 array;
	       end
	    
	    (* char** *)
	    structure CSS =
	       struct
		  type css = pointer
		  val sub = _ffi "C_CSS_sub": css * int -> CS.cs;
	       end
	 end

      type cstring = C.CS.cs
      type cstringArray = C.CSS.css
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
	    type extra = string list ref

	    val extra = fn x => _prim "Exn_extra": exn -> 'a; x
	    val extra: exn -> extra = extra
	    val name = _prim "Exn_name": exn -> string;
	    val keepHistory = _build_const "Exn_keepHistory": bool;
	    val setInitExtra =
	       fn x => _prim "Exn_setInitExtra": (unit -> 'a) -> unit; x
	    val setInitExtra: (unit -> extra) -> unit = setInitExtra
	    val setRaise = _prim "Exn_setRaise": (string * exn -> unit) -> unit;
	    val setTopLevelHandler =
	       _prim "Exn_setTopLevelHandler": (exn -> unit) -> unit;
	 end

      structure GC =
	 struct
	    val collect = _prim "GC_collect": unit -> unit;
	    val setMessages = _ffi "GC_setMessages": bool -> unit;
	    val setSummary = _ffi "GC_setSummary": bool -> unit;
	 end
      
      structure IEEEReal =
	 struct
	    val setRoundingMode = _ffi "IEEEReal_setRoundingMode": int -> unit;
	    val getRoundingMode = _ffi "IEEEReal_getRoundingMode": unit -> int;
	 end

      structure Int =
	 struct
	    type int = int

	    val *? = _prim "Int_mul": int * int -> int;
	    val * =
	       if detectOverflow
		  then _prim "Int_mulCheck": int * int -> int;
	       else *?
	    val +? = _prim "Int_add": int * int -> int;
	    val + =
	       if detectOverflow
		  then _prim "Int_addCheck": int * int -> int;
	       else +?
	    val -? = _prim "Int_sub": int * int -> int;
	    val - =
	       if detectOverflow
		  then _prim "Int_subCheck": int * int -> int;
	       else -?
	    val < = _prim "Int_lt": int * int -> bool;
	    val <= = _prim "Int_le": int * int -> bool;
	    val > = _prim "Int_gt": int * int -> bool;
	    val >= = _prim "Int_ge": int * int -> bool;
	    val geu = _prim "Int_geu": int * int -> bool;
	    val gtu = _prim "Int_gtu": int * int -> bool;
	    val quot = _prim "Int_quot": int * int -> int;
	    val rem = _prim "Int_rem": int * int -> int;
	    val ~? = _prim "Int_neg": int -> int; 
	    val ~ =
	       if detectOverflow
		  then _prim "Int_negCheck": int -> int;
	       else ~?
	 end

      structure Array =
	 struct
	    open Array

      	    fun array n =
	       if safe andalso Int.< (n, 0)
		  then raise Size
	       else if eq (n, 0)
		       then _prim "Array_array0": unit -> 'a array; ()
		    else _prim "Array_array": int -> 'a array; n
	 end

      structure IntInf =
	 struct
	    type int = intInf

	    val + = _prim "IntInf_add": int * int * word -> int;
	    val compare = _prim "IntInf_compare": int * int -> Int.int;
	    val fromVector = _prim "IntInf_fromVector": word vector -> int;
	    val fromWord = _prim "IntInf_fromWord": word -> int;
	    val gcd = _prim "IntInf_gcd": int * int * word -> int;
	    val * = _prim "IntInf_mul": int * int * word -> int;
	    val ~ = _prim "IntInf_neg": int * word -> int;
	    val quot = _prim "IntInf_quot": int * int * word -> int;
	    val rem = _prim "IntInf_rem": int * int * word -> int;
	    val smallMul =
	       _ffi "IntInf_smallMul": word * word * word ref -> word;
	    val - = _prim "IntInf_sub": int * int * word -> int; 
 	    val toString
	       = _prim "IntInf_toString": int * Int.int * word -> string;
	    val toVector = _prim "IntInf_toVector": int -> word vector;
	    val toWord = _prim "IntInf_toWord": int -> word;
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
	    datatype hostType =
	       Cygwin | Linux
	    val hostType: hostType =
	       case _const "MLton_hostType": int; of
		  0 => Cygwin
		| 1 => Linux

	    val native = _build_const "MLton_native": bool;

	    structure Profile =
	       struct
		  val profile = _build_const "MLton_profile": bool;
		  structure Data =
		     struct
		        type t = word
			val dummy = 0wx0: t;
			val free = _ffi "MLton_Profile_Data_free": t -> unit;
			val malloc = _ffi "MLton_Profile_Data_malloc": unit -> t;
			val reset = _ffi "MLton_Profile_Data_reset": t -> unit;
			val write =
			   _ffi "MLton_Profile_Data_write"
			   : t * word (* fd *) -> unit;
		     end
		  val init = _ffi "MLton_Profile_init": unit -> unit;
		  val setCurrent =
		     _ffi "MLton_Profile_setCurrent": Data.t -> unit;
		  val installHandler =
		     _ffi "MLton_Profile_installHandler": unit -> unit;
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
	    
(*       val deserialize = _prim "MLton_deserialize": Word8Vector.vector -> 'a ref; *)
(*       val serialize = _prim "MLton_serialize": 'a ref -> Word8Vector.vector; *)

	    val size = fn x => _prim "MLton_size": 'a ref -> int; x
	 end

      structure OS =
	 struct
	    structure FileSys =
	       struct
		  val tmpnam = _ffi "OS_FileSys_tmpnam": unit -> cstring;
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
	    structure Math =
	       struct
		  type real = real
		     
		  val acos = _prim "Real_Math_acos": real -> real;
		  val asin = _prim "Real_Math_asin": real -> real;
		  val atan = _prim "Real_Math_atan": real -> real;
		  val atan2 = _prim "Real_Math_atan2": real * real -> real;
		  val cos = _prim "Real_Math_cos": real -> real;
		  val cosh = _prim "Real_Math_cosh": real -> real;
		  val e = _ffi "Real_Math_e": real;
		  val exp = _prim "Real_Math_exp": real -> real;
		  val ln = _prim "Real_Math_ln": real -> real;
		  val log10 = _prim "Real_Math_log10": real -> real;
		  val pi = _ffi "Real_Math_pi": real;
		  val pow = _prim "Real_Math_pow": real * real -> real;
		  val sin = _prim "Real_Math_sin": real -> real;
		  val sinh = _prim "Real_Math_sinh": real -> real;
		  val sqrt = _prim "Real_Math_sqrt": real -> real;
		  val tan = _prim "Real_Math_tan": real -> real;
		  val tanh = _prim "Real_Math_tanh": real -> real;
	       end

	    val * = _prim "Real_mul": real * real -> real;
	    val *+ = _prim "Real_muladd": real * real * real -> real;
	    val *- = _prim "Real_mulsub": real * real * real -> real;
	    val + = _prim "Real_add": real * real -> real;
	    val - = _prim "Real_sub": real * real -> real;
	    val / = _prim "Real_div": real * real -> real;
	    val < = _prim "Real_lt": real * real -> bool;
	    val <= = _prim "Real_le": real * real -> bool;
	    val == = _prim "Real_equal": real * real -> bool;
	    val > = _prim "Real_gt": real * real -> bool;
	    val >= = _prim "Real_ge": real * real -> bool;
	    val ?= = _prim "Real_qequal": real * real -> bool;
	    val abs = _prim "Real_abs": real -> real;
	    val class = _ffi "Real_class": real -> int;
	    val copySign = _prim "Real_copysign": real * real -> real;
	    val frexp = _prim "Real_frexp": real * int ref -> real;
	    val fromInt = _prim "Real_fromInt": int -> real;
	    val isFinite = _ffi "Real_isFinite": real -> bool;
	    val isNan = _ffi "Real_isNan": real -> bool;
	    val isNormal = _ffi "Real_isNormal": real -> bool;
	    val ldexp = _prim "Real_ldexp": real * int -> real;
	    val maxFinite = _ffi "Real_maxFinite": real;
	    val minNormalPos = _ffi "Real_minNormalPos": real;
	    val minPos = _ffi "Real_minPos": real;
	    val modf = _prim "Real_modf": real * real ref -> real;
	    val round = _prim "Real_round": real -> real;
	    val signBit = _ffi "Real_signBit": real -> bool;
	    val toInt = _prim "Real_toInt": real -> int;
	    val ~ = _prim "Real_neg": real -> real;
	 end
      
      structure Ref =
	 struct
	    val deref = fn x => _prim "Ref_deref": 'a ref -> 'a; x
	    val assign = fn x => _prim "Ref_assign": 'a ref * 'a -> unit; x
	 end

      structure Socket =
	 struct
	    type fd = int
	    type socket = int
	    type port = int
	    type address = word

	    structure Addr =
	       struct
		  val address = _ffi "Socket_Addr_address": unit -> address;
		  val port = _ffi "Socket_Addr_port": unit -> port;
	       end

	    structure Host =
	       struct
		  val name = _ffi "Socket_Host_name": unit -> cstring;
		  val getByAddress =
		     _ffi "Socket_Host_getByAddress": address -> bool;
		  val getByName =
		     _ffi "Socket_Host_getByName": nullString -> bool;
	       end

	    val accept = _ffi "Socket_accept": socket -> fd;
	    val connect = _ffi "Socket_connect": string * port -> socket;
	    val listen = _ffi "Socket_listen": port ref * socket ref -> int;
	    type how = int;
	    val shutdownRead = _const "Socket_shutdownRead": how;
	    val shutdownWrite = _const "Socket_shutdownWrite": how;
	    val shutdownReadWrite = _const "Socket_shutdownReadWrite": how;
	    val shutdown = _ffi "Socket_shutdown": fd * how -> int;
	 end

      structure Stdio =
	 struct
	    val print = _ffi "Stdio_print": string -> unit;
	    val sprintf =
	       _ffi "Stdio_sprintf": char array * nullString * real -> int;
	 end

      structure String =
	 struct
	    val fromCharVector =
	       _prim "String_fromCharVector": char vector -> string;
	    val fromWord8Vector =
	       _prim "String_fromWord8Vector": word8 vector -> string;
	    val size = _prim "String_size": string -> int;
	    val toCharVector =
	       _prim "String_toCharVector": string -> char vector;
	    val toWord8Vector =
	       _prim "String_toWord8Vector": string -> word8 vector;
	    val sub = _prim "String_sub": string * int -> char;
	 end

      structure TextIO =
	 struct
	    val bufSize = _build_const "TextIO_bufSize": int;
	 end
      
      structure Thread =
	 struct
	    type preThread = preThread
	    type thread = thread

	    fun atomicBegin () =
	       if handlesSignals
		  then _prim "Thread_atomicBegin": unit -> unit; ()
	       else ()
	    val canHandle = _prim "Thread_canHandle": unit -> int;
	    fun atomicEnd () =
	       if handlesSignals
		  then
		     if Int.<= (canHandle (), 0)
			then raise Fail "Thread.atomicEnd with no atomicBegin"
		     else _prim "Thread_atomicEnd": unit -> unit; ()
	       else ()
	    val copy = _prim "Thread_copy": preThread -> thread;
	    val copyCurrent = _prim "Thread_copyCurrent": unit -> preThread;
	    val current = _prim "Thread_current": unit -> thread;
	    val finishHandler = _ffi "Thread_finishHandler": unit -> unit;
	    val saved = _ffi "Thread_saved": unit -> thread;
	    val setHandler = _ffi "Thread_setHandler": thread -> unit;
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
	       fn x => _prim "Vector_fromArray": 'a array -> 'a vector; x
	 end

      structure Word8 =
	 struct
	    type word = word8

	    val + = _prim "Word8_add": word * word -> word;
	    val andb = _prim "Word8_andb": word * word -> word;
	    val ~>> = _prim "Word8_arshift": word * word32 -> word;
	    val div = _prim "Word8_div": word * word -> word;
	    val fromInt = _prim "Word8_fromInt": int -> word;
	    val fromLargeWord = _prim "Word8_fromLargeWord": word32 -> word;
	    val >= = _prim "Word8_ge": word * word -> bool;
	    val > = _prim "Word8_gt" : word * word -> bool;
	    val <= = _prim "Word8_le": word * word -> bool;
	    val << = _prim "Word8_lshift": word * word32 -> word;
	    val < = _prim "Word8_lt" : word * word -> bool;
	    val mod = _prim "Word8_mod": word * word -> word;
	    val * = _prim "Word8_mul": word * word -> word;
	    val ~ = _prim "Word8_neg": word -> word;
	    val notb = _prim "Word8_notb": word -> word;
	    val orb = _prim "Word8_orb": word * word -> word;
	    val rol = _prim "Word8_rol": word * word32 -> word;
	    val ror = _prim "Word8_ror": word * word32 -> word;
	    val >> = _prim "Word8_rshift": word * word32 -> word;
	    val - = _prim "Word8_sub": word * word -> word;
	    val toInt = _prim "Word8_toInt": word -> int;
	    val toIntX = _prim "Word8_toIntX": word -> int;
	    val toLargeWord = _prim "Word8_toLargeWord": word -> word32;
	    val toLargeWordX = _prim "Word8_toLargeWordX": word -> word32;
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

      structure Word32 =
	 struct
	    type word = word32

	    val + = _prim "Word32_add": word * word -> word;
	    val addCheck = _prim "Word32_addCheck": word * word -> word;
	    val andb = _prim "Word32_andb": word * word -> word;
	    val ~>> = _prim "Word32_arshift": word * word -> word;
	    val div = _prim "Word32_div": word * word -> word;
	    val fromInt = _prim "Word32_fromInt": int -> word;
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
	    val toIntX = _prim "Word32_toIntX": word -> int;
	    val xorb = _prim "Word32_xorb": word * word -> word;
	 end

      structure World =
	 struct
	    val isOriginal = _ffi "World_isOriginal": unit -> bool;
	    val makeOriginal = _ffi "World_makeOriginal": unit -> unit;
	    val save = _prim "World_save": word (* filedes *) -> unit;
	 end
   end
