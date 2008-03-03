(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(* Primitive names are special -- see atoms/prim.fun. *)

structure Primitive = struct

open Primitive

structure MLton = struct

val eq = _prim "MLton_eq": 'a * 'a -> bool;
(* val deserialize = _prim "MLton_deserialize": Word8Vector.vector -> 'a ref; *)
val halt = _prim "MLton_halt": C_Status.t -> unit;
(* val serialize = _prim "MLton_serialize": 'a ref -> Word8Vector.vector; *)
val share = _prim "MLton_share": 'a -> unit;
val size = _prim "MLton_size": 'a ref -> C_Size.t;

val installSignalHandler =
   _prim "MLton_installSignalHandler": unit -> unit;

structure GCState =
   struct
      type t = Pointer.t

      val gcState = #1 _symbol "gcStateAddress": t GetSet.t; ()
   end

structure Align =
   struct
      datatype t = Align4 | Align8

      val align =
         case _build_const "MLton_Align_align": Int32.int; of
            4 => Align4
          | 8 => Align8
          | _ => raise Primitive.Exn.Fail8 "MLton_Align_align"
   end

structure CallStack =
   struct
      (* The most recent caller is at index 0 in the array. *)
      datatype t = T of Word32.word array

      val callStack =
         _import "GC_callStack": GCState.t * Word32.word array -> unit;
      val frameIndexSourceSeq =
         _import "GC_frameIndexSourceSeq": GCState.t * Word32.word -> Pointer.t;
      val keep = _command_line_const "CallStack.keep": bool = false;
      val numStackFrames =
         _import "GC_numStackFrames": GCState.t -> Word32.word;
      val sourceName = _import "GC_sourceName": GCState.t * Word32.word -> C_String.t;
   end

structure Codegen =
   struct
      datatype t = Bytecode | C | x86 | amd64

      val codegen =
         case _build_const "MLton_Codegen_codegen": Int32.int; of
            0 => Bytecode
          | 1 => C
          | 2 => x86
          | 3 => amd64
          | _ => raise Primitive.Exn.Fail8 "MLton_Codegen_codegen"

      val isBytecode = codegen = Bytecode
      val isC = codegen = C
      val isX86 = codegen = x86
      val isAmd64 = codegen = amd64
      (* val isNative = isX86 orelse isAmd64 *)
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
      val keepHistory = _command_line_const "Exn.keepHistory": bool = false;
      val setExtendExtra = _prim "Exn_setExtendExtra": ('a -> 'a) -> unit;
      val setExtendExtra: (extra -> extra) -> unit = setExtendExtra
      val setInitExtra = _prim "Exn_setInitExtra": 'a -> unit;
      val setInitExtra: extra -> unit = setInitExtra

      (* Ensure that setInitExtra and setExtendExtra are initialized.
       * Important for -const 'Exn.keepHistory true', so that 
       * exceptions can be raised (and handled) during Basis Library
       * initialization.
       *)
      val setInitExtra : extra -> unit =
         if keepHistory
            then (setInitExtra (NONE: extra)
                  ; fn _ => ())
         else fn _ => ()
      val setExtendExtra : (extra -> extra) -> unit =
         if keepHistory
            then (setExtendExtra (fn _ => NONE)
                  ; setExtendExtra)
         else fn _ => ()
   end

structure FFI =
   struct
      val cpointerArray = #1 _symbol "MLton_FFI_CPointer": Pointer.t GetSet.t; ()
      val getOp = #1 _symbol "MLton_FFI_op": Int32.t GetSet.t;
      val int8Array = #1 _symbol "MLton_FFI_Int8": Pointer.t GetSet.t; ()
      val int16Array = #1 _symbol "MLton_FFI_Int16": Pointer.t GetSet.t; ()
      val int32Array = #1 _symbol "MLton_FFI_Int32": Pointer.t GetSet.t; ()
      val int64Array = #1 _symbol "MLton_FFI_Int64": Pointer.t GetSet.t; ()
      val numExports = _build_const "MLton_FFI_numExports": Int32.int;
      val objptrArray = #1 _symbol "MLton_FFI_Objptr": Pointer.t GetSet.t; ()
      val real32Array = #1 _symbol "MLton_FFI_Real32": Pointer.t GetSet.t; ()
      val real64Array = #1 _symbol "MLton_FFI_Real64": Pointer.t GetSet.t; ()
      val word8Array = #1 _symbol "MLton_FFI_Word8": Pointer.t GetSet.t; ()
      val word16Array = #1 _symbol "MLton_FFI_Word16": Pointer.t GetSet.t; ()
      val word32Array = #1 _symbol "MLton_FFI_Word32": Pointer.t GetSet.t; ()
      val word64Array = #1 _symbol "MLton_FFI_Word64": Pointer.t GetSet.t; ()
   end

structure Finalizable =
   struct
      val touch = _prim "MLton_touch": 'a -> unit;
   end

structure GC =
   struct
      val collect = _prim "GC_collect": unit -> unit;
      val pack = _import "GC_pack": GCState.t -> unit;
      val setHashConsDuringGC =
         _import "GC_setHashConsDuringGC": GCState.t * bool -> unit;
      val setMessages = _import "GC_setMessages": GCState.t * bool -> unit;
      val setRusageMeasureGC = 
         _import "GC_setRusageMeasureGC": GCState.t * bool -> unit;
      val setSummary = _import "GC_setSummary": GCState.t * bool -> unit;
      val unpack = _import "GC_unpack": GCState.t -> unit;
   end

structure Platform =
   struct
      structure Arch =
         struct
            datatype t = 
               Alpha 
             | AMD64 
             | ARM 
             | HPPA 
             | IA64 
             | m68k 
             | MIPS 
             | PowerPC 
             | S390 
             | Sparc 
             | X86

            val host: t =
               case _const "MLton_Platform_Arch_host": String8.string; of
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
                | _ => raise Primitive.Exn.Fail8 "strange MLton_Platform_Arch_host"

            val hostIsBigEndian = _const "MLton_Platform_Arch_bigendian": bool;
         end

      structure OS =
         struct
            datatype t =
               AIX
             | Cygwin
             | Darwin
             | FreeBSD
             | HPUX
             | Linux
             | MinGW
             | NetBSD
             | OpenBSD
             | Solaris

            val host: t =
               case _const "MLton_Platform_OS_host": String8.string; of
                  "aix" => AIX
                | "cygwin" => Cygwin
                | "darwin" => Darwin
                | "freebsd" => FreeBSD
                | "hpux" => HPUX
                | "linux" => Linux
                | "mingw" => MinGW
                | "netbsd" => NetBSD
                | "openbsd" => OpenBSD
                | "solaris" => Solaris
                | _ => raise Primitive.Exn.Fail8 "strange MLton_Platform_OS_host"

            val forkIsEnabled =
               case host of
                  Cygwin =>
                     #1 _symbol "MLton_Platform_CygwinUseMmap": bool GetSet.t; ()
                | MinGW => false
                | _ => true

            val useWindowsProcess = not forkIsEnabled
         end
   end

structure Pointer =
   struct
      open Pointer
      type pointer = t

      val add =
         _prim "CPointer_add": pointer * C_Size.word -> pointer;
      val sub =
         _prim "CPointer_sub": pointer * C_Size.word -> pointer;
      val diff =
         _prim "CPointer_diff": pointer * pointer -> C_Size.word;
      val < = _prim "CPointer_lt": pointer * pointer -> bool;
      local
         structure S = IntegralComparisons(type t = pointer
                                           val < = <)
      in
         open S
      end

      val fromWord =
         _prim "CPointer_fromWord": C_Size.word -> pointer;
      val toWord =
         _prim "CPointer_toWord": pointer -> C_Size.word;

      val null: t = fromWord 0w0

      fun isNull p = p = null

      val getCPointer = _prim "CPointer_getCPointer": t * C_Ptrdiff.t -> t;
      val getInt8 = _prim "CPointer_getWord8": t * C_Ptrdiff.t -> Int8.int;
      val getInt16 = _prim "CPointer_getWord16": t * C_Ptrdiff.t -> Int16.int;
      val getInt32 = _prim "CPointer_getWord32": t * C_Ptrdiff.t -> Int32.int;
      val getInt64 = _prim "CPointer_getWord64": t * C_Ptrdiff.t -> Int64.int;
      val getObjptr = _prim "CPointer_getObjptr": t * C_Ptrdiff.t -> 'a;
      val getReal32 = _prim "CPointer_getReal32": t * C_Ptrdiff.t -> Real32.real;
      val getReal64 = _prim "CPointer_getReal64": t * C_Ptrdiff.t -> Real64.real;
      val getWord8 = _prim "CPointer_getWord8": t * C_Ptrdiff.t -> Word8.word;
      val getWord16 = _prim "CPointer_getWord16": t * C_Ptrdiff.t -> Word16.word;
      val getWord32 = _prim "CPointer_getWord32": t * C_Ptrdiff.t -> Word32.word;
      val getWord64 = _prim "CPointer_getWord64": t * C_Ptrdiff.t -> Word64.word;
      val setCPointer = _prim "CPointer_setCPointer": t * C_Ptrdiff.t * t -> unit;
      val setInt8 = _prim "CPointer_setWord8": t * C_Ptrdiff.t * Int8.int -> unit;
      val setInt16 = _prim "CPointer_setWord16": t * C_Ptrdiff.t * Int16.int -> unit;
      val setInt32 = _prim "CPointer_setWord32": t * C_Ptrdiff.t * Int32.int -> unit;
      val setInt64 = _prim "CPointer_setWord64": t * C_Ptrdiff.t * Int64.int -> unit;
      val setObjptr = _prim "CPointer_setObjptr": t * C_Ptrdiff.t * 'a -> unit;
      val setReal32 = _prim "CPointer_setReal32": t * C_Ptrdiff.t * Real32.real -> unit;
      val setReal64 = _prim "CPointer_setReal64": t * C_Ptrdiff.t * Real64.real -> unit;
      val setWord8 = _prim "CPointer_setWord8": t * C_Ptrdiff.t * Word8.word -> unit;
      val setWord16 = _prim "CPointer_setWord16": t * C_Ptrdiff.t * Word16.word -> unit;
      val setWord32 = _prim "CPointer_setWord32": t * C_Ptrdiff.t * Word32.word -> unit;
      val setWord64 = _prim "CPointer_setWord64": t * C_Ptrdiff.t * Word64.word -> unit;
   end

structure Profile =
   struct
      val isOn = _build_const "MLton_Profile_isOn": bool;
      structure Data =
         struct
            type t = Pointer.t

            val dummy = Pointer.null
            val free = _import "GC_profileFree": GCState.t * t -> unit;
            val malloc = _import "GC_profileMalloc": GCState.t -> t;
            val write =
               _import "GC_profileWrite": GCState.t * t * NullString8.t -> unit;
         end
      val done = _import "GC_profileDone": GCState.t -> unit;
      val getCurrent = _import "GC_getProfileCurrent": GCState.t -> Data.t;
      val setCurrent = _import "GC_setProfileCurrent" : GCState.t * Data.t -> unit;
   end

structure Thread =
   struct
      type preThread = PreThread.t
      type thread = Thread.t

      val atomicState = _prim "Thread_atomicState": unit -> Word32.word;
      val atomicBegin = _prim "Thread_atomicBegin": unit -> unit;
      fun atomicEnd () = 
         if atomicState () = 0w0
            then raise Primitive.Exn.Fail8 "Thread.atomicEnd"
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
      val current = _import "GC_getCurrentThread": GCState.t -> thread;
      val finishSignalHandler = _import "GC_finishSignalHandler": GCState.t -> unit;
      val returnToC = _prim "Thread_returnToC": unit -> unit;
      val saved = _import "GC_getSavedThread": GCState.t -> thread;
      val savedPre = _import "GC_getSavedThread": GCState.t -> preThread;
      val setCallFromCHandler =
         _import "GC_setCallFromCHandlerThread": GCState.t * thread -> unit;
      val setSignalHandler = 
         _import "GC_setSignalHandlerThread": GCState.t * thread -> unit;
      val setSaved = _import "GC_setSavedThread": GCState.t * thread -> unit;
      val startSignalHandler = _import "GC_startSignalHandler": GCState.t -> unit;
      val switchTo = _prim "Thread_switchTo": thread -> unit;
   end

structure Weak =
   struct
      open Weak

      val canGet = _prim "Weak_canGet": 'a t -> bool;
      val get = _prim "Weak_get": 'a t -> 'a;
      val new = _prim "Weak_new": 'a -> 'a t;
   end

structure World =
   struct
      val getAmOriginal = _import "GC_getAmOriginal": GCState.t -> bool;
      val setAmOriginal = _import "GC_setAmOriginal": GCState.t * bool -> unit;
      val getSaveStatus = _import "GC_getSaveWorldStatus": GCState.t -> bool C_Errno.t;
      (* save's result status is accesible via getSaveStatus ().
       * It is not possible to have the type of save as
       * NullString8.t -> bool C_Errno.t, because there are two
       * different ways to return from the call to save.  One way is
       * the direct obvious way, in the program instance that called
       * save.  However, another way to return is in the program
       * instance that loads the world.  Making save return a bool
       * creates nasty bugs where the return code from the CCall
       * expects to see a bool result according to the C return
       * convention, but there isn't one when returning in the load
       * world.  
       *)
      val save = _prim "World_save": NullString8.t -> unit;
   end

end 

end
