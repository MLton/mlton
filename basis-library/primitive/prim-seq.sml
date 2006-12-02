(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(* Primitive names are special -- see atoms/prim.fun. *)

structure Primitive = struct

open Primitive

structure Array =
   struct
      open Array
      val arrayUnsafe = _prim "Array_array": SeqIndex.int -> 'a array;
      val array0Const = _prim "Array_array0Const": unit -> 'a array;
      val length = _prim "Array_length": 'a array -> SeqIndex.int;
      (* There is no maximum length on arrays, so maxLen' = SeqIndex.maxInt'. *)
      (* val maxLen': SeqIndex.int = SeqIndex.maxInt' *)
      val subUnsafe = _prim "Array_sub": 'a array * SeqIndex.int -> 'a;
      val updateUnsafe = _prim "Array_update": 'a array * SeqIndex.int * 'a -> unit;
   end

structure Vector =
   struct
      open Vector 
      (* Don't mutate the array after you apply fromArray, because vectors 
       * are supposed to be immutable and the optimizer depends on this.  
       *)
      val fromArray = _prim "Array_toVector": 'a array -> 'a vector;
      val length = _prim "Vector_length": 'a vector -> SeqIndex.int;
      val subUnsafe = _prim "Vector_sub": 'a vector * SeqIndex.int -> 'a;
   end

end
