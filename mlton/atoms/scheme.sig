(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
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
