(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
signature MY_DIRS =
   sig
      val dirs: unit -> {home: Dir.t,
			  sml: Dir.t,
			  smlnj: Dir.t,
			  bin: Dir.t,
			  binFiles: Dir.t,
			  heap: Dir.t,
			  src: Dir.t,
			  compiler: Dir.t}
      val exportFn: string * (string * string list -> int) -> unit
      val exportML: string -> bool
   end
