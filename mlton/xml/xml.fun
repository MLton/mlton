(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
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
