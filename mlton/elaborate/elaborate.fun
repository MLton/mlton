(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor Elaborate (S: ELABORATE_STRUCTS): ELABORATE= 
struct

open S

structure ConstType =
   struct
      datatype t = Bool | Real | String | Word

      val toString =
	 fn Bool => "Bool"
	  | Real => "Real"
	  | String => "String"
	  | Word => "Word"
   end

structure Ctrls = ElaborateControls(structure Ast = Ast
				    structure ConstType = ConstType
				    structure CoreML = CoreML)

structure Env = ElaborateEnv (structure Ast = Ast
			      structure CoreML = CoreML
			      structure Ctrls = Ctrls
			      structure TypeEnv = TypeEnv)

local
   open Env
in
   structure Decs = Decs
end

structure ElaborateMLBs = ElaborateMLBs (structure Ast = Ast
					 structure ConstType = ConstType
					 structure CoreML = CoreML
					 structure Ctrls = Ctrls
					 structure Decs = Decs
					 structure Env = Env)

open ElaborateMLBs
end
