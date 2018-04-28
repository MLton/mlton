(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 2003-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature SOURCE_INFO_STRUCTS =
   sig
   end

signature SOURCE_INFO =
   sig
      include SOURCE_INFO_STRUCTS

      type t

      val all: unit -> t list
      val equals: t * t -> bool
      val file: t -> File.t option
      val gc: t
      val gcArrayAllocate: t
      val hash: t -> word
      val fromC: string -> t
      val function: {name: string list,
                     region: Region.t} -> t
      val isC: t -> bool
      val layout: t -> Layout.t
      val main: t
      val plist: t -> PropertyList.t
      val polyEqual: t
      val polyHash: t
      val toString: t -> string
      val toString': t * string -> string
      val unknown: t
   end
