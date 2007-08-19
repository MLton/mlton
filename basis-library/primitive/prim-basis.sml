(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Primitive = struct

(* Primitive Basis (Definition) *)
structure Bool =
   struct
      datatype t = datatype bool
      datatype bool = datatype t
   end
structure Exn =
   struct
      type t = exn
      type exn = t
      exception Bind = Bind
      exception Match = Match
      exception PrimOverflow = Overflow
   end
structure List =
   struct
      datatype t = datatype list
      datatype list = datatype t
   end
structure Ref =
   struct
      datatype t = datatype ref
      datatype ref = datatype t
   end
structure Unit =
   struct
      type t = unit
      type unit = t
   end

(* Primitive Basis (Basis Library) *)
structure Array =
   struct
      type 'a t = 'a array
      type 'a array = 'a t
   end
structure Vector =
   struct
      type 'a t = 'a vector
      type 'a vector = 'a t
   end

(* Primitive Basis (Primitive Types) *)
structure Char8 =
   struct
      type t = char8
      type char = t
   end
structure Char16 =
   struct
      type t = char16
      type char = t
   end
structure Char32 =
   struct
      type t = char32
      type char = t
   end

structure Int1 =
   struct
      type t = int1
      type int = t
   end
structure Int2 =
   struct
      type t = int2
      type int = t
   end
structure Int3 =
   struct
      type t = int3
      type int = t
   end
structure Int4 =
   struct
      type t = int4
      type int = t
   end
structure Int5 =
   struct
      type t = int5
      type int = t
   end
structure Int6 =
   struct
      type t = int6
      type int = t
   end
structure Int7 =
   struct
      type t = int7
      type int = t
   end
structure Int8 =
   struct
      type t = int8
      type int = t
   end
structure Int9 =
   struct
      type t = int9
      type int = t
   end
structure Int10 =
   struct
      type t = int10
      type int = t
   end
structure Int11 =
   struct
      type t = int11
      type int = t
   end
structure Int12 =
   struct
      type t = int12
      type int = t
   end
structure Int13 =
   struct
      type t = int13
      type int = t
   end
structure Int14 =
   struct
      type t = int14
      type int = t
   end
structure Int15 =
   struct
      type t = int15
      type int = t
   end
structure Int16 =
   struct
      type t = int16
      type int = t
   end
structure Int17 =
   struct
      type t = int17
      type int = t
   end
structure Int18 =
   struct
      type t = int18
      type int = t
   end
structure Int19 =
   struct
      type t = int19
      type int = t
   end
structure Int20 =
   struct
      type t = int20
      type int = t
   end
structure Int21 =
   struct
      type t = int21
      type int = t
   end
structure Int22 =
   struct
      type t = int22
      type int = t
   end
structure Int23 =
   struct
      type t = int23
      type int = t
   end
structure Int24 =
   struct
      type t = int24
      type int = t
   end
structure Int25 =
   struct
      type t = int25
      type int = t
   end
structure Int26 =
   struct
      type t = int26
      type int = t
   end
structure Int27 =
   struct
      type t = int27
      type int = t
   end
structure Int28 =
   struct
      type t = int28
      type int = t
   end
structure Int29 =
   struct
      type t = int29
      type int = t
   end
structure Int30 =
   struct
      type t = int30
      type int = t
   end
structure Int31 =
   struct
      type t = int31
      type int = t
   end
structure Int32 =
   struct
      type t = int32
      type int = t
   end
structure Int64 =
   struct
      type t = int64
      type int = t
   end
structure IntInf =
   struct
      type t = intInf
      type int = t
   end

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

structure String8 =
   struct
      type t = Char8.t vector
      type string = t
   end
structure String16 =
   struct
      type t = Char16.t vector
      type string = t
   end
structure String32 =
   struct
      type t = Char32.t vector
      type string = t
   end

structure Word1 =
   struct
      type t = word1
      type word = t
   end
structure Word2 =
   struct
      type t = word2
      type word = t
   end
structure Word3 =
   struct
      type t = word3
      type word = t
   end
structure Word4 =
   struct
      type t = word4
      type word = t
   end
structure Word5 =
   struct
      type t = word5
      type word = t
   end
structure Word6 =
   struct
      type t = word6
      type word = t
   end
structure Word7 =
   struct
      type t = word7
      type word = t
   end
structure Word8 =
   struct
      type t = word8
      type word = t
   end
structure Word9 =
   struct
      type t = word9
      type word = t
   end
structure Word10 =
   struct
      type t = word10
      type word = t
   end
structure Word11 =
   struct
      type t = word11
      type word = t
   end
structure Word12 =
   struct
      type t = word12
      type word = t
   end
structure Word13 =
   struct
      type t = word13
      type word = t
   end
structure Word14 =
   struct
      type t = word14
      type word = t
   end
structure Word15 =
   struct
      type t = word15
      type word = t
   end
structure Word16 =
   struct
      type t = word16
      type word = t
   end
structure Word17 =
   struct
      type t = word17
      type word = t
   end
structure Word18 =
   struct
      type t = word18
      type word = t
   end
structure Word19 =
   struct
      type t = word19
      type word = t
   end
structure Word20 =
   struct
      type t = word20
      type word = t
   end
structure Word21 =
   struct
      type t = word21
      type word = t
   end
structure Word22 =
   struct
      type t = word22
      type word = t
   end
structure Word23 =
   struct
      type t = word23
      type word = t
   end
structure Word24 =
   struct
      type t = word24
      type word = t
   end
structure Word25 =
   struct
      type t = word25
      type word = t
   end
structure Word26 =
   struct
      type t = word26
      type word = t
   end
structure Word27 =
   struct
      type t = word27
      type word = t
   end
structure Word28 =
   struct
      type t = word28
      type word = t
   end
structure Word29 =
   struct
      type t = word29
      type word = t
   end
structure Word30 =
   struct
      type t = word30
      type word = t
   end
structure Word31 =
   struct
      type t = word31
      type word = t
   end
structure Word32 =
   struct
      type t = word32
      type word = t
   end
structure Word64 =
   struct
      type t = word64
      type word = t
   end

(* Primitive Basis (MLton Extensions) *)
structure Pointer =
   struct
      type t = cpointer
   end
structure Thread =
   struct
      type t = thread
   end
structure Weak =
   struct
      type 'a t = 'a weak
   end

end

(* Top-level bindings *)
datatype bool = datatype Primitive.Bool.bool
type exn = Primitive.Exn.exn
datatype list = datatype Primitive.List.list
datatype ref = datatype Primitive.Ref.ref
type unit = Primitive.Unit.unit
type 'a array = 'a Primitive.Array.array
type 'a vector = 'a Primitive.Vector.vector
