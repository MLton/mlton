(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor PrimTycons (S: PRIM_TYCONS_STRUCTS)
   :> PRIM_TYCONS where type tycon = S.t =
   struct
      open S

      type tycon = t

      val array = fromString "array"
      val arrow = fromString "->"
      val bool = fromString "bool"
      val char = fromString "char"
      val exn = fromString "exn"
      val int = fromString "int"
      val intInf = fromString "intInf"
      val list = fromString "list"
      val pointer = fromString "pointer"
      val preThread = fromString "preThread"
      val real = fromString "real"
      val reff = fromString "ref"
      val thread = fromString "thread"
      val string = fromString "string"
      val tuple = fromString "*"
      val vector = fromString "vector"
      val word = fromString "word"
      val word8 = fromString "word8"

      val prims =
	 [array, arrow, bool, char, exn, int, intInf, list, pointer,
	  preThread, real, reff, string, thread, tuple, vector, word, word8]

      val defaultInt = int
      val defaultWord = word

      fun equalTo t t' = equals (t, t')

      local
	 fun is l t = List.exists (l, equalTo t)
      in
	 val isWordX = is [word, word8]
	 val isIntX = is [int, intInf]
      end
   end
