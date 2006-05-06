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

structure IntInf =
   struct
      open IntInf

      val + = _prim "IntInf_add": int * int * C_Size.t -> int;
      val andb = _prim "IntInf_andb": int * int * C_Size.t -> int;
      val ~>> = _prim "IntInf_arshift": int * Word32.word * C_Size.t -> int;
      val compare = _prim "IntInf_compare": int * int -> Int32.int;
      val fromVector = _prim "WordVector_toIntInf": C_MPLimb.t vector -> int;
      val fromWord = _prim "Word_toIntInf": ObjptrWord.word -> int;
      val gcd = _prim "IntInf_gcd": int * int * C_Size.t -> int;
      val << = _prim "IntInf_lshift": int * Word32.word * C_Size.t -> int;
      val * = _prim "IntInf_mul": int * int * C_Size.t -> int;
      val ~ = _prim "IntInf_neg": int * C_Size.t -> int;
      val notb = _prim "IntInf_notb": int * C_Size.t -> int;
      val orb = _prim "IntInf_orb": int * int * C_Size.t -> int;
      val quot = _prim "IntInf_quot": int * int * C_Size.t -> int;
      val rem = _prim "IntInf_rem": int * int * C_Size.t -> int;
      val - = _prim "IntInf_sub": int * int * C_Size.t -> int; 
      val toString =
         _prim "IntInf_toString": int * Int32.int * C_Size.t -> String8.string;
      val toVector = _prim "IntInf_toVector": int -> C_MPLimb.t vector;
      val toWord = _prim "IntInf_toWord": int -> ObjptrWord.word;
      val xorb = _prim "IntInf_xorb": int * int * C_Size.t -> int;
   end

end
