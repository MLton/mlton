(* Copyright (C) 2009,2017 Matthew Fluet.
 * Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature ID_STRUCTS =
   sig
      val noname: string
   end

signature ID =
   sig
      include ID_STRUCTS

      type t

      val bogus: t
      val clear: t -> unit
      val clearPrintName: t -> unit
      val equals: t * t -> bool
      val layout: t -> Layout.t
      val fromString: string -> t (* doesn't add uniquefying suffix *)
      val hash: t -> word
      val new: t -> t (* new id with the same originalName *)
      val newNoname: unit -> t (* prefix is noname *)
      val newString: string -> t (* new id with printName not set *)
      val originalName: t -> string (* raw destructor *)
      val plist: t -> PropertyList.t
      val printNameAlphaNumeric: bool ref
      val toString: t -> string
   end
