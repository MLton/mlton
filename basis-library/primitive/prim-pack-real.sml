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

structure PackReal32 =
   struct
      type real = Real32.real
      type word = Word32.word

      val castFromWord = _prim "Word32_castToReal32": word -> real;
      val castToWord = _prim "Real32_castToWord32": real -> word;
   end

structure PackReal64 =
   struct
      type real = Real64.real
      type word = Word64.word

      val castFromWord = _prim "Word64_castToReal64": word -> real;
      val castToWord = _prim "Real64_castToWord64": real -> word;
   end

end
