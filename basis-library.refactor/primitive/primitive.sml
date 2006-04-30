(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(* Primitive names are special -- see atoms/prim.fun. *)

structure Primitive =
   struct

      structure Cygwin =
         struct
            val toFullWindowsPath =
               _import "Cygwin_toFullWindowsPath": NullString.t -> CString.t;
         end

   end
