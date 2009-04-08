(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 2004-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature C_FUNCTION_STRUCTS = 
   sig
      structure CType: C_TYPE
   end

signature C_FUNCTION = 
   sig
      include C_FUNCTION_STRUCTS

      structure Convention:
         sig
            datatype t = Cdecl | Stdcall

            val layout: t -> Layout.t
            val toString: t -> string
         end

      structure SymbolScope:
         sig
            datatype t = External | Private | Public

            val layout: t -> Layout.t
            val toString: t -> string
         end

      structure Target:
         sig
            datatype t = Direct of string | Indirect

            val layout: t -> Layout.t
            val toString: t -> string
         end

      datatype 'a t = T of {args: 'a vector,
                            (* bytesNeeded = SOME i means that the i'th
                             * argument to the function is a word that
                             * specifies the number of bytes that must be
                             * free in order for the C function to succeed.
                             * Limit check insertion is responsible for
                             * making sure that the bytesNeeded is available.
                             *)
                            bytesNeeded: int option,
                            convention: Convention.t,
                            ensuresBytesFree: bool,
                            mayGC: bool,
                            maySwitchThreads: bool,
                            modifiesFrontier: bool,
                            prototype: CType.t vector * CType.t option,
                            readsStackTop: bool,
                            return: 'a,
                            symbolScope: SymbolScope.t,
                            (* target = Indirect means that the 0'th
                             * argument to the function is a word
                             * that specifies the target.
                             *)
                            target: Target.t,
                            writesStackTop: bool}

      val args: 'a t -> 'a vector
      val bytesNeeded: 'a t -> int option
      val convention: 'a t -> Convention.t
      val ensuresBytesFree: 'a t -> bool
      val equals: 'a t * 'a t -> bool
      val cPointerType: 'a t -> string
      val cPrototype: 'a t -> string
      val isOk: 'a t * {isUnit: 'a -> bool} -> bool
      val layout: 'a t * ('a -> Layout.t) -> Layout.t
      val map: 'a t * ('a -> 'b) -> 'b t
      val mayGC: 'a t -> bool
      val maySwitchThreads: 'a t -> bool
      val modifiesFrontier: 'a t -> bool
      val prototype: 'a t -> CType.t vector * CType.t option
      val readsStackTop: 'a t -> bool
      val return: 'a t -> 'a
      val symbolScope: 'a t -> SymbolScope.t
      val target: 'a t -> Target.t
      val writesStackTop: 'a t -> bool
      val vanilla: {args: 'a vector,
                    name: string,
                    prototype: CType.t vector * CType.t option,
                    return: 'a} -> 'a t
   end
