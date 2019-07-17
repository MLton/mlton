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
      structure Static: STATIC
      structure Type: REP_TYPE

      sharing ObjectType = Type.ObjectType
      sharing ObjptrTycon = ObjectType.ObjptrTycon = Type.ObjptrTycon
      sharing Runtime = ObjectType.Runtime = Type.Runtime

      (* SML/NJ bug:
       *
       * `sharing Atoms = Type`
       *
       * should suffice, instead of the following enumeration,
       * but leads to `implied type sharing violation` errors;
       * the sharing of `Atoms` and `Type` seems to "forget"
       * the components of `Atoms` not present in `Type`.
       *)
      (* sharing Atoms = Type *)
      sharing CFunction = Type.CFunction
      sharing CType = Type.CType
      sharing Label = Type.Label
      sharing Prim = Type.Prim
      sharing RealSize = Type.RealSize
      sharing WordSize = Type.WordSize
      sharing WordX = Type.WordX
      sharing WordXVector = Type.WordXVector
      sharing WordSize = Static.WordSize
      sharing WordX = Static.WordX
      sharing WordXVector = Static.WordXVector
   end

signature BACKEND_ATOMS =
   sig
      structure BackendAtoms: BACKEND_ATOMS'

      include BACKEND_ATOMS'

      sharing Atoms = BackendAtoms.Atoms
      sharing ObjectType = BackendAtoms.ObjectType
      sharing ObjptrTycon = BackendAtoms.ObjptrTycon
      sharing Runtime = BackendAtoms.Runtime
      sharing Type = BackendAtoms.Type
   end
