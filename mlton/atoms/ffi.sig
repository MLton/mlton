(* Copyright (C) 2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
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
      val declareExports: {print: string -> unit} -> unit
      val declareHeaders: {print: string -> unit} -> unit
      val numExports: unit -> int
   end
