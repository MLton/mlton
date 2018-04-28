(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature AST_PROGRAMS_STRUCTS =
   sig
      include AST_ATOMS_STRUCTS
   end

signature AST_PROGRAMS =
   sig
      include AST_MODULES

      structure Program:
         sig
            datatype t = T of Topdec.t list list

            val checkSyntax: t -> unit
            val coalesce: t -> t
            val empty: t
            val size: t -> int
            val layout: t -> Layout.t
         end
   end
