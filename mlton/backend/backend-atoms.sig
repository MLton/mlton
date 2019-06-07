(* Copyright (C) 2019 Matthew Fluet.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature BACKEND_ATOMS_STRUCTS =
   sig
      include ATOMS
   end

signature BACKEND_ATOMS' =
   sig
      include BACKEND_ATOMS_STRUCTS

      structure ObjectType: OBJECT_TYPE
      structure ObjptrTycon: OBJPTR_TYCON
      structure Runtime: RUNTIME
      structure Type: REP_TYPE

      sharing Atoms = Type
      sharing ObjectType = Type.ObjectType
      sharing ObjptrTycon = ObjectType.ObjptrTycon = Type.ObjptrTycon
      sharing Runtime = ObjectType.Runtime = Type.Runtime
   end

signature BACKEND_ATOMS =
   sig
      structure BackendAtoms: BACKEND_ATOMS'

      include BACKEND_ATOMS'

      sharing ObjectType = BackendAtoms.ObjectType
      sharing ObjptrTycon = BackendAtoms.ObjptrTycon
      sharing Runtime = BackendAtoms.Runtime
      sharing Type = BackendAtoms.Type
   end
