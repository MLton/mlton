(* Copyright (C) 2009,2014,2019 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature PROFILE_INFO_STRUCTS =
   sig
      structure ProfileLabel: PROFILE_LABEL
   end

signature PROFILE_INFO =
   sig
      include PROFILE_INFO_STRUCTS

      datatype t =
         T of {(* For each frame, gives the index into sourceSeqs of the
                * source functions corresponding to the frame.
                *)
               frameSources: int vector,
               labels: {label: ProfileLabel.t,
                        sourceSeqsIndex: int} vector,
               names: string vector,
               (* Each sourceSeq describes a sequence of source functions,
                * each given as an index into the source vector.
                *)
               sourceSeqs: int vector vector,
               sources: {nameIndex: int,
                         successorsIndex: int} vector}

      val empty: t
      val clear: t -> unit
      val layouts: t * (Layout.t -> unit) -> unit
      val layout: t -> Layout.t
      val modify: t -> {newProfileLabel: ProfileLabel.t -> ProfileLabel.t,
                        delProfileLabel: ProfileLabel.t -> unit,
                        getProfileInfo: unit -> t}
   end
