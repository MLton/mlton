(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Err =
   struct
      datatype t = T of {inner: t option,
                         name: string,
                         obj: Layout.t}

      fun layout (T {inner, name, obj}): Layout.t =
         let
            open Layout
         in
            align [case inner of
                      NONE => empty
                    | SOME e => layout e,
                   seq [str (concat ["invalid ", name, ": "]), obj]]
         end

      exception E of t

      fun check' (name: string,
                  ok: unit -> 'a option,
                  layout: unit -> Layout.t): 'a =
         case ok () handle E e => raise E (T {inner = SOME e,
                                              name = name,
                                              obj = layout ()}) of
            NONE => raise E (T {inner = NONE,
                                name = name,
                                obj = layout ()})
          | SOME a => a

      fun boolToUnitOpt b = if b then SOME () else NONE

      fun check (name, ok, layout) =
         check' (name, boolToUnitOpt o ok, layout)
   end
