(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
signature SCHEME = 
   sig
      include GENERIC_SCHEME

      structure Type: TYPE
      structure Tyvar: TYVAR
      structure Tycon: TYCON

      sharing type ty = Type.t
      sharing type tyvar = Type.Tyvar.t
      sharing Tycon = Type.Tycon
      sharing Tyvar = Type.Tyvar
   end
