(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

functor ThreePointLattice(S: THREE_POINT_LATTICE_STRUCTS): THREE_POINT_LATTICE = 
struct

open S

structure L = NPointLattice(val names = [bottom, mid, top])
open L

val isBottom = fn x => isN (x, 0)
val isMid = fn x => isN (x, 1)
val isTop = fn x => isN (x, 2)
val makeMid = fn x => makeN (x, 1)
val makeTop = fn x => makeN (x, 2)
val whenMid = fn (x, h) => whenN (x, 1, h)
val whenTop = fn (x, h) => whenN (x, 2, h)
end