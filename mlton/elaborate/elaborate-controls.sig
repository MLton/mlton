(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
signature ELABORATE_CONTROLS_STRUCTS = 
   sig
      structure Ast: AST
      structure ConstType: CONST_TYPE
      structure CoreML: CORE_ML
   end

signature ELABORATE_CONTROLS = 
   sig
      include ELABORATE_CONTROLS_STRUCTS

      val allowConstant: bool ref
      val allowExport: bool ref
      val allowImport: bool ref
      val allowOverload: bool ref
      val allowPrim: bool ref
      val allowRebindEquals: bool ref
      val deadCode: bool ref
      val forceUsed: int ref
      val lookupConstant: (string * ConstType.t -> CoreML.Const.t) ref
      val sequenceUnit: bool ref
      val warnMatch: bool ref
      val warnUnused: bool ref

      val withDefault: (unit -> 'a) -> 'a
      val withAnns: (string list * Region.t) list * (unit -> 'a) -> 'a
   end
