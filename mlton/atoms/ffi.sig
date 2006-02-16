(* Copyright (C) 2004-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

type int = Int.t
   
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
                      res: CType.t option} -> int
      val addSymbol: {ty: CType.t,
                      name: string} -> unit
      val declareExports: {print: string -> unit} -> unit
      val declareHeaders: {print: string -> unit} -> unit
      val numExports: unit -> int
   end
