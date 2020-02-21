(* Copyright (C) 2019-2020 Matthew Fluet.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor BackendAtoms (S: BACKEND_ATOMS_STRUCTS): BACKEND_ATOMS =
struct

structure BackendAtoms =
   struct
      open S

      structure Runtime = Runtime ()
      structure ObjptrTycon = ObjptrTycon (structure RealSize = RealSize
                                           structure Runtime = Runtime
                                           structure WordSize = WordSize
                                           structure WordX = WordX)
      structure RepType = RepType (structure CFunction = CFunction
                                   structure CType = CType
                                   structure Const = Const
                                   structure Label = Label
                                   structure ObjptrTycon = ObjptrTycon
                                   structure Prim = Prim
                                   structure Prod = Prod
                                   structure RealSize = RealSize
                                   structure RealX = RealX
                                   structure Runtime = Runtime
                                   structure WordSize = WordSize
                                   structure WordX = WordX
                                   structure WordXVector = WordXVector)
      structure ObjectType = RepType.ObjectType

      structure Type = RepType
   end

open BackendAtoms

end
