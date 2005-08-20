(* c-debug.sml
 * 2005 Matthew Fluet (mfluet@acm.org)
 *  Adapted for MLton.
 *)

(*
 * Encoding C's type system in SML.  This module provides the "public"
 * view of the implementation.
 *
 * DEBUG VERSION with CHECKED POINTER DEREFERENCING.
 * 
 *   (C) 2002, Lucent Technologies, Bell Laboratories
 *
 * author: Matthias Blume (blume@research.bell-labs.com)
 *)
structure C_Debug : C_DEBUG = struct
    (* first of all, we look mostly like structure C... *)
    open C

    (* ... but then, we also check for NULL pointers... *)
    exception NullPointer

    (* ... which means that we have to re-implement some things: *)
    structure Ptr = struct
        open Ptr
        val |*! = fn p => if isNull' p then raise NullPointer else |*! p
        val |*| = fn p => if isNull p then raise NullPointer else |*| p
    end
end
