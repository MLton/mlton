(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

type int = Int.t
   
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
   end
