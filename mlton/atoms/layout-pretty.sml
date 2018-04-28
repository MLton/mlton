(* Copyright (C) 2017 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure BindingStrength =
   struct
      datatype t =
         Arrow
       | Tuple
       | Unit
   end

structure LayoutPretty =
   struct
      type t = Layout.t * ({isChar: bool} * BindingStrength.t)

      fun simple (l: Layout.t): t =
         (l, ({isChar = false}, BindingStrength.Unit))

      val dontCare: t = simple (Layout.str "_")
      fun bracket ((l, ({isChar}, _)): t): t =
         (Layout.seq [Layout.str "[", l, Layout.str "]"],
          ({isChar = isChar}, BindingStrength.Unit))
   end
