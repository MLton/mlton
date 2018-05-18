(* Copyright (C) 2017 Matthew Fluet.
 * Copyright (C) 2003-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature PRETTY =
   sig
      type t = Layout.t

      val casee: {default: t option,
                  rules: (t * t) vector,
                  test: t} -> t
      val conApp: {arg: t option,
                   con: Layout.t,
                   targs: Layout.t vector} -> t
      val handlee: {catch: t,
                    handler: t,
                    try: t} -> t
      val lett: t * t -> t
      val locall: t * t -> t
      val primApp: {args: t vector,
                    prim: t,
                    targs: t vector} -> t
      val raisee: t -> t
      val seq: t vector -> t
   end
