(* Modified by sweeks@acm.org on 2000-8-24.
 * Ported to MLton
 *)
type int = Int.int
   
(* lib-base-sig.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *)

signature LIB_BASE =
  sig

    exception Unimplemented of string
	(* raised to report unimplemented features *)
    exception Impossible of string
	(* raised to report internal errors *)

    exception NotFound
	(* raised by searching operations *)

    val failure: {module: string, func: string, msg: string} -> 'a
	(* raise the exception Fail with a standard format message. *)

    val version: {date: string, system: string, version_id: int list}
    val banner: string

  end (* LIB_BASE *)

