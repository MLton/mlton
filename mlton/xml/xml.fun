(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor Xml (S: XML_STRUCTS): XML =
   struct
      structure XmlTree = XmlTree (S)

      open XmlTree
      structure TypeCheck = TypeCheck (structure XmlTree = XmlTree)
      val typeCheck = TypeCheck.typeCheck

      structure SimplifyTypes = SimplifyTypes (open XmlTree)

      structure SccFuns = SccFuns (open XmlTree)

      structure Simplify = Simplify (structure XmlTree = XmlTree)
      val simplify =
	 Simplify.simplify
(* SimplifyTypes doesn't do anything yet.
 *       o SimplifyTypes.simplifyTypes
 *)
	 o SccFuns.sccFuns
   end
