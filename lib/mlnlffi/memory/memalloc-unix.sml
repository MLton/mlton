(* memalloc-unix.sml
 * 2005 Matthew Fluet (mfluet@acm.org)
 *  Adapted for MLton.
 *)

(* memalloc-a4-unix.sml
 *
 *   Memory allocation (via malloc) for Unix.
 *   Size of address: 4 bytes.
 *
 * Copyright (c) 2004 by The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)
structure CMemAlloc : CMEMALLOC = struct

    exception OutOfMemory

    structure Ptr = MLton.Pointer

    type addr = Ptr.t
    type addr' = addr

(*
    structure DL = DynLinkage

    fun main's s = DL.lib_symbol (DL.main_lib, s)
    val malloc_h = main's "malloc"
    val free_h = main's "free"

    fun sys_malloc (n : C_Size.word) = 
        let val w_p = _import * : MLton.Pointer.t -> C_Size.word -> addr;
            val a = w_p (DL.addr malloc_h) n
        in if a = Ptr.null then raise OutOfMemory else a
        end

    fun sys_free (a : addr) = 
        let val p_u = _import * : MLton.Pointer.t -> addr -> unit;
        in p_u (DL.addr free_h) a
        end
*)

    fun sys_malloc (n : C_Size.word) = 
        let val w_p = _import "malloc" : C_Size.word -> addr;
            val a = w_p n
        in if a = Ptr.null then raise OutOfMemory else a
        end

    fun sys_free (a : addr) = 
        let val p_u = _import "free" : addr -> unit;
        in p_u a
        end

    fun alloc bytes = sys_malloc (C_Size.fromLarge (Word.toLarge bytes))
    fun free a = sys_free a
end
