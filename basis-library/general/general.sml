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
      (* exnMessage will be improved upon after MLton.Exn is defined. *)
      val exnMessage = exnName 
 
      datatype order = LESS | EQUAL | GREATER

      val ! = Primitive.Ref.deref
      val op := = Primitive.Ref.assign
      fun (f o g) x = f (g x)
      fun x before () = x
      fun ignore _ = ()
   end

(* GeneralGlobal will be defined once MLton.Exn is.  For now, we just open
 * everything we have.
 *)
local
   structure Z: GENERAL_GLOBAL = General
in
   open Z
end
