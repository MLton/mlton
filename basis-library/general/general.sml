(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
structure General: GENERAL =
   struct
      type unit = unit
      type exn = exn
	 
      exception Bind = Bind
      exception Chr
      exception Div
      exception Domain
      exception Fail of string
      exception Match = Match
      exception Overflow = Overflow
      exception Size = Size
      exception Span
      exception Subscript

      datatype order = LESS | EQUAL | GREATER

      val exnName = Primitive.Exn.name
      val exnMessage = exnName
 
      fun (f o g) x = f (g x)
      fun x before () = x
      fun ignore _ = ()
      val op := = Primitive.Ref.assign
      val ! = Primitive.Ref.deref
   end

structure GeneralGlobal: GENERAL_GLOBAL = General
open GeneralGlobal
