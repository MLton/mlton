(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(* Primitive names are special -- see atoms/prim.fun. *)

structure Char = Char8
type char = Char.char
structure Int = Int32
type int = Int.int
structure Real = Real64
type real = Real.real

structure String = String8
type string = String.string

structure Word = Word32
type word = Word.word
structure LargeWord = Word64

structure Primitive =
   struct

      structure PackReal32 =
         struct
            type real = Real32.real
               
            val subVec = _import "PackReal32_subVec": Word8.word vector * int -> real;
            val subVecRev =
               _import "PackReal32_subVecRev": Word8.word vector * int -> real;
            val update =
               _import "PackReal32_update": Word8.word array * int * real -> unit;
            val updateRev =
               _import "PackReal32_updateRev": Word8.word array * int * real -> unit;
         end

      structure PackReal64 =
         struct
            type real = Real64.real
               
            val subVec = _import "PackReal64_subVec": Word8.word vector * int -> real;
            val subVecRev =
               _import "PackReal64_subVecRev": Word8.word vector * int -> real;
            val update =
               _import "PackReal64_update": Word8.word array * int * real -> unit;
            val updateRev =
               _import "PackReal64_updateRev": Word8.word array * int * real -> unit;
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

            val fromLarge = _prim "Real64_toReal32": Real64.real -> real;
            val toLarge = _prim "Real32_toReal64": real -> Real64.real;

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

      structure TextIO =
         struct
            val bufSize = _command_line_const "TextIO.bufSize": int = 4096;
         end

      structure Word8Array =
         struct
            val subWord =
               _prim "Word8Array_subWord": Word8.word array * int -> word;
            val subWordRev =
               _import "Word8Array_subWord32Rev": Word8.word array * int -> word;
            val updateWord =
               _prim "Word8Array_updateWord": Word8.word array * int * word -> unit;
            val updateWordRev =
               _import "Word8Array_updateWord32Rev": Word8.word array * int * word -> unit;
         end
      structure Word8Vector =
         struct
            val subWord =
               _prim "Word8Vector_subWord": Word8.word vector * int -> word;
            val subWordRev =
               _import "Word8Vector_subWord32Rev": Word8.word vector * int -> word;
         end

      structure Cygwin =
         struct
            val toFullWindowsPath =
               _import "Cygwin_toFullWindowsPath": NullString.t -> CString.t;
         end

   end
