(* linkage-libdl.sml
 * 2005 Matthew Fluet (mfluet@acm.org)
 *  Adapted for MLton.
 *)

(*  linkage-dlopen.sml
 *
 * This module implements a high-level interface for dlopen.
 *   While addresses (those obtained by applying function "addr" below
 *   or addresses derived from those) will not remain valid across
 *   export{ML,Fn}/restart, handles *will* stay valid.
 *
 * Copyright (c) 2004 by The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)
structure DynLinkage :> DYN_LINKAGE = struct

    exception DynLinkError of string
    val () = 
       MLton.Exn.addExnMessager 
       (fn DynLinkError s => SOME (concat ["DynLinkError: ", s]) 
         | _ => NONE)

    local
        type era = unit ref
        type addr = MLton.Pointer.t

        (* a handle remembers an address and the era of its creation as
         * well as a function to re-create the address when necessary *)
        type h = (addr * era) ref * (unit -> addr)
    in
        type lib_handle = h
        type addr_handle = h
    end

    type mode = C_UInt.word
    local
       val RTLD_LAZY   = 0wx00001
       val RTLD_NOW    = 0wx00002
       val RTLD_GLOBAL = 0wx00100
       val RTLD_LOCAL  = 0wx00000
    in
       fun mk_mode {lazy: bool, global: bool} : mode=
          C_UInt.orb
          (if lazy then RTLD_LAZY else RTLD_NOW,
           if global then RTLD_GLOBAL else RTLD_LOCAL)
    end

    local
        (* low-level linkage via dlopen/dlsym *)
        local
           val dlopen =
              _import "dlopen": string * mode -> MLton.Pointer.t;
           val dlopen_null =
              _import "dlopen": MLton.Pointer.t * mode -> MLton.Pointer.t;
           val dlsym =
              _import "dlsym": MLton.Pointer.t * string -> MLton.Pointer.t;
           val dlerror =
              _import "dlerror": unit -> MLton.Pointer.t;
           val dlclose =
              _import "dlclose": MLton.Pointer.t -> Int32.int;
        in
           (* mid-level linkage *)
           val dlopen = fn (filename, lazy, global) =>
              let
                 val mode = mk_mode {lazy = lazy, global = global}
              in
                 case filename of
                    NONE => dlopen_null (MLton.Pointer.null, mode)
                  | SOME filename => dlopen (filename ^ "\000", mode)
              end
           val dlsym = fn (hndl, symbol) =>
              dlsym (hndl, symbol ^ "\000")
           val dlerror = fn () =>
              let
                 val addr = dlerror ()
              in
                 if addr = MLton.Pointer.null
                    then NONE
                    else let
                            fun loop (index, cs) =
                               let
                                  val w = MLton.Pointer.getWord8 (addr, index)
                                  val c = Byte.byteToChar w
                               in
                                  if c = #"\000"
                                     then SOME (implode (rev cs))
                                     else loop (index + 1, c::cs)
                               end
                         in
                            loop (0, [])
                         end
              end
           val dlclose = fn hndl =>
              let val _ = dlclose hndl
              in ()
              end
        end

        (* label used for CleanUp *)
(*
        val label = "DynLinkNewEra"
*)
        (* generate a new "era" indicator *)
        fun newEra () = ref ()

        (* the current era *)
        val now = ref (newEra ())

        (* make a handle, remember era of creation of its current value *)
        fun mkHandle f = (ref (f (), !now), f)

        (* fetch from a handle; use the stored address if it was created
         * in the current era, otherwise regenerate the address *)
        fun get (r as ref (a, e), f) =
            if e = !now then a
            else let val a = f ()
                 in r := (a, !now); a
                 end

        (* call a dl-function and check for errors *)
        fun checked dlf x = let
            val r = dlf x
        in
            case dlerror () of
                NONE => r
              | SOME s => raise DynLinkError s
        end

        (* add a cleanup handler that causes a new era to start
         * every time the runtime system is started anew *)
(*
        open SMLofNJ.Internals.CleanUp
        val _ = addCleaner (label, [AtInit, AtInitFn],
                            fn _ => now := newEra ())

        val _ = Cleaner.addNew (Cleaner.atLoadWorld, fn () => now := newEra ())
*)
    in
        val main_lib = mkHandle (fn () => checked dlopen (NONE, true, true))

        fun open_lib' { name, lazy, global, dependencies } =
            mkHandle (fn () => (app (ignore o get) dependencies;
                                checked dlopen (SOME name, lazy, global)))
        fun open_lib { name, lazy, global } =
            open_lib' { name = name, lazy = lazy, global = global,
                        dependencies = [] }

        fun lib_symbol (lh, s) = mkHandle (fn () => checked dlsym (get lh, s))

        val addr = get

        fun close_lib lh = checked dlclose (get lh)
    end
end
