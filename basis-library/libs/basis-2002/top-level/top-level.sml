(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

(* Non-standard signatures *)
signature MLTON = MLTON
signature SML_OF_NJ = SML_OF_NJ
signature UNSAFE = UNSAFE
signature SML90 = SML90

(* Non-standard structures *)
structure Primitive = Primitive
structure Basis1997 = Basis1997
structure MLton = MLton
structure SMLofNJ = SMLofNJ
structure Unsafe = Unsafe
structure SML90 = SML90

open Basis2002
