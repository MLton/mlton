(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature SOURCE =
   sig
      type t

      (* The pos in the following specs is a file position (e.g. yypos of mllex).
       *)
      val getPos: t * int -> SourcePos.t
      val lineDirective:
         t * File.t option * {lineNum: int, lineStart: int} -> unit
      val lineStart: t -> SourcePos.t
      val new: File.t -> t
      val newline: t * int -> unit
      val name: t -> string
   end
