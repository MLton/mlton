(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 2004-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature FFI_STRUCTS = 
   sig
      structure CFunction: C_FUNCTION
      structure CType: C_TYPE
   end

signature FFI = 
   sig
      include FFI_STRUCTS

      val addExport: {args: CType.t vector,
                      convention: CFunction.Convention.t,
                      name: string,
                      res: CType.t option,
                      symbolScope: CFunction.SymbolScope.t} -> int
      val addSymbol: {ty: CType.t,
                      name: string,
                      symbolScope: CFunction.SymbolScope.t} -> unit
      val checkScope: {name: string,
                       symbolScope: CFunction.SymbolScope.t} ->
                      CFunction.SymbolScope.t
      val declareExports: {print: string -> unit} -> unit
      val declareHeaders: {print: string -> unit} -> unit
      val numExports: unit -> int
   end
