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

structure GetSet =
   struct
      type 'a t = (unit -> 'a) * ('a -> unit)
   end

structure PreThread :> sig type t end = struct type t = Thread.t end
structure Thread :> sig type t end = struct type t = Thread.t end

(**************************************************************************)

structure Bool =
   struct
      open Bool
      fun not b = if b then false else true
   end

structure Controls =
   struct
      val debug = _command_line_const "MLton.debug": bool = false;
      val detectOverflow = _command_line_const "MLton.detectOverflow": bool = true;
      val safe = _command_line_const "MLton.safe": bool = true;
      val bufSize = _command_line_const "TextIO.bufSize": Int32.int = 4096;
   end

structure Exn =
   struct
      open Exn

      val name = _prim "Exn_name": exn -> String8.string;

      exception Div
      exception Domain
      exception Fail8 of String8.string
      exception Fail16 of String16.string
      exception Fail32 of String32.string
      exception Overflow
      exception Size
      exception Subscript

      val wrapOverflow: ('a -> 'b) -> ('a -> 'b) =
         fn f => fn a => f a handle PrimOverflow => raise Overflow
   end

structure Order =
   struct
      datatype t = LESS | EQUAL | GREATER
      datatype order = datatype t
   end

structure Option =
   struct
      datatype 'a t = NONE | SOME of 'a
      datatype option = datatype t
   end

structure Ref =
   struct
      open Ref
      val deref = _prim "Ref_deref": 'a ref -> 'a;
      val assign = _prim "Ref_assign": 'a ref * 'a -> unit;
   end

structure TopLevel =
   struct
      val getHandler = _prim "TopLevel_getHandler": unit -> (exn -> unit);
      val getSuffix = _prim "TopLevel_getSuffix": unit -> (unit -> unit);
      val setHandler = _prim "TopLevel_setHandler": (exn -> unit) -> unit;
      val setSuffix = _prim "TopLevel_setSuffix": (unit -> unit) -> unit;
   end

end

val not = Primitive.Bool.not

exception Bind = Primitive.Exn.Bind
exception Div = Primitive.Exn.Div
exception Domain = Primitive.Exn.Domain
exception Match = Primitive.Exn.Match
exception Overflow = Primitive.Exn.Overflow
exception Size = Primitive.Exn.Size
exception Subscript = Primitive.Exn.Subscript

datatype option = datatype Primitive.Option.option
datatype order = datatype Primitive.Order.order

infix 4 = <>
val op = = _prim "MLton_equal": ''a * ''a -> bool;
val op <> = fn (x, y) => not (x = y)
