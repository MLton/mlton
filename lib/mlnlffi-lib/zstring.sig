(* zstring.sig
 * 2005 Matthew Fluet (mfluet@acm.org)
 *  Adapted for MLton.
 *)

(*
 * Functions for translating between 0-terminated C strings and native
 * ML strings.
 *
 *  (C) 2001, Lucent Technologies, Bell Laboratories
 *
 * author: Matthias Blume (blume@research.bell-labs.com)
 *)
signature ZSTRING = sig

    type 'c zstring = (C.uchar, 'c) C.obj C.ptr
    type 'c zstring' = (C.uchar, 'c) C.obj C.ptr'

    (* the C strlen function *)
    val length : 'c zstring -> int
    val length' : 'c zstring' -> int

    (* make ML string from 0-terminated C string *)
    val toML : 'c zstring -> string
    val toML' : 'c zstring' -> string

    (* Copy contents of ML string into C string and add terminating 0. *)
    val cpML : { from: string, to: C.rw zstring } -> unit
    val cpML' : { from: string, to: C.rw zstring' } -> unit

    (* Make C-duplicate of ML string (allocate memory and then copy). *)
    val dupML : string -> 'c zstring
    val dupML' : string -> 'c zstring'
end
