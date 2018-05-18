(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature PROPERTY =
   sig
      structure Plist: PROPERTY_LIST

      type ('sym, 'val) init

      val initConst: 'val -> ('sym, 'val) init
      val initFun: ('sym -> 'val) -> ('sym, 'val) init
      val initRaise: string * ('sym -> Layout.t) -> ('sym, 'val) init
      val initRec: ('sym * ('sym -> 'val) -> 'val) -> ('sym, 'val) init

      val destGet:
         ('sym -> Plist.t) * ('sym, 'val) init
         -> {
             destroy: unit -> unit,
             get: 'sym -> 'val
            }

      val destGetSet:
         ('sym -> Plist.t) * ('sym, 'val) init
         -> {
             destroy: unit -> unit,
             get: 'sym -> 'val,
             set: 'sym * 'val -> unit
            }

      val destGetSetOnce:
         ('sym -> Plist.t) * ('sym, 'val) init
         -> {
             get: 'sym -> 'val,
             set: 'sym * 'val -> unit,
             destroy: unit -> unit
            }

      (* For all of the rem functions, it is OK if the property isn't there. *)

      val get:
         ('sym -> Plist.t) * ('sym, 'val) init
         -> {
             get: 'sym -> 'val,
             rem: 'sym -> unit
             }

      val getSet:
         ('sym -> Plist.t) * ('sym, 'val) init
         -> {
             get: 'sym -> 'val,
             rem: 'sym -> unit,
             set: 'sym * 'val -> unit
            }

      (* Property can only be set or initialized once. *)
      val getSetOnce:
         ('sym -> Plist.t) * ('sym, 'val) init
         -> {
             get: 'sym -> 'val,
             rem: 'sym -> unit,
             set: 'sym * 'val -> unit
            }

   end
