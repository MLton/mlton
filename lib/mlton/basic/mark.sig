(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
type int = Int.t
   
signature MARK =
   sig
      include ORDER

      val bol: t -> t
      val forwardChar: t -> t
      val forwardChars: t * int -> t
      val fromFile: File.t -> t
      val fromString: string -> t
      val int: t -> t * int
      val nextLine: t -> t
      val nextLines: t * int -> t
      val previousLine: t -> t
      val previousLines: t * int -> t
      val real: t -> t * real
      val search: t * string -> t (* right after end of string *)
      val searchBackward: t * string -> t
      val skipSpaces: t -> t
   end
