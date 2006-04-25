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
