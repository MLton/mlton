(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure General =
   struct
      type unit = unit

      type exn = exn
      exception Bind = Bind
      exception Match = Match
      exception Chr
      exception Div
      exception Domain
      exception Fail = Fail
      exception Overflow = Overflow
      exception Size = Size
      exception Span
      exception Subscript
      val exnName = Primitive.Exn.name
 
      datatype order = LESS | EQUAL | GREATER

      val ! = Primitive.Ref.deref
      val op := = Primitive.Ref.assign
      fun (f o g) x = f (g x)
      fun x before () = x
      fun ignore _ = ()
   end

local
   open General
in
   datatype order = datatype order
   exception Chr = Chr
   exception Div = Div
   exception Domain = Domain
   exception Span = Span
   exception Subscript = Subscript
   val ! = !
   val op := = op :=
   val op before = op before
   val exnName = exnName
   val ignore = ignore
   val op o = op o
end
