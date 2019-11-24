(* Copyright (C) 2019 Matthew Fluet.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor BackendAtoms (S: BACKEND_ATOMS_STRUCTS): BACKEND_ATOMS =
struct

structure BackendAtoms =
   struct
      open S

      structure ObjptrTycon = ObjptrTycon ()
      structure Runtime = Runtime ()
      structure RepType = RepType (structure CFunction = CFunction
                                   structure CType = CType
                                   structure Label = Label
                                   structure ObjptrTycon = ObjptrTycon
                                   structure Prim = Prim
                                   structure RealSize = RealSize
                                   structure RealX = RealX
                                   structure Runtime = Runtime
                                   structure WordSize = WordSize
                                   structure WordX = WordX
                                   structure WordXVector = WordXVector)
      structure ObjectType = RepType.ObjectType

      structure Static = Static (structure Const = Const
                                 structure ObjptrTycon = ObjptrTycon
                                 structure Runtime = Runtime
                                 structure RealX = RealX
                                 structure WordSize = WordSize
                                 structure WordX = WordX
                                 structure WordXVector = WordXVector)
      structure Type = RepType
   end

open BackendAtoms

end
