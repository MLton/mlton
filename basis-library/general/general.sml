(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure General: GENERAL_EXTRA =
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

      datatype order = LESS | EQUAL | GREATER

      val ! = Primitive.Ref.deref
      val op := = Primitive.Ref.assign
      fun (f o g) x = f (g x)
      fun x before () = x
      fun ignore _ = ()
      val exnName = Primitive.Exn.name

      local
         val messagers: (exn -> string option) list ref = ref []
      in
         val addExnMessager: (exn -> string option) -> unit =
            fn f => messagers := f :: !messagers
            
         val rec exnMessage: exn -> string =
            fn e =>
            let
               val rec find =
                  fn [] => exnName e
                   | m :: ms =>
                        case m e of
                           NONE => find ms
                         | SOME s => s
            in
               find (!messagers)
            end
      end
   end

structure GeneralGlobal: GENERAL_GLOBAL = General
open GeneralGlobal

