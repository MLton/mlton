(* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature AMD64_LIVENESS_STRUCTS =
  sig
    structure amd64: AMD64
    structure amd64MLtonBasic: AMD64_MLTON_BASIC
    sharing amd64 = amd64MLtonBasic.amd64
  end

signature AMD64_LIVENESS =
  sig
    include AMD64_LIVENESS_STRUCTS

    structure LiveSet: sig 
                         include SET
                         val toMemLocSet: t -> amd64.MemLocSet.t
                       end
    sharing type LiveSet.Element.t = amd64.MemLoc.t

    val track : amd64.MemLoc.t -> bool

    structure LiveInfo:
      sig
        type t
        val newLiveInfo : unit -> t

        val setLiveOperands : t * amd64.Label.t * amd64.Operand.t list -> unit
        val setLive : t * amd64.Label.t * LiveSet.t -> unit
        val getLive : t * amd64.Label.t -> LiveSet.t
        val completeLiveInfo : {chunk: amd64.Chunk.t,
                                liveInfo: t,
                                pass: string} -> unit
        val completeLiveInfo_msg : unit -> unit
        val verifyLiveInfo : {chunk: amd64.Chunk.t,
                              liveInfo: t} -> bool
        val verifyLiveInfo_msg : unit -> unit
      end

    structure Liveness:
      sig
        datatype t = T of {liveIn: LiveSet.t,
                           liveOut: LiveSet.t,
                           dead: LiveSet.t}

        val dead: t -> LiveSet.t
        val liveIn: t -> LiveSet.t
        val livenessAssembly : {assembly : amd64.Assembly.t, live : LiveSet.t} -> t
        val livenessEntry : {entry : amd64.Entry.t, live : LiveSet.t} -> t
        val livenessTransfer : {transfer: amd64.Transfer.t, liveInfo: LiveInfo.t} -> t
      end

    structure LivenessBlock:
      sig
        datatype t = T of {entry: (amd64.Entry.t * Liveness.t),
                           profileLabel: amd64.ProfileLabel.t option,
                           statements: (amd64.Assembly.t * Liveness.t) list,
                           transfer: (amd64.Transfer.t * Liveness.t)}

        val printBlock : t -> unit
        val toLivenessEntry : {entry: amd64.Entry.t,
                               live: LiveSet.t} ->
                              {entry: (amd64.Entry.t * Liveness.t),
                               live: LiveSet.t}
        val reLivenessEntry : {entry: (amd64.Entry.t * Liveness.t),
                               live: LiveSet.t} ->
                              {entry: (amd64.Entry.t * Liveness.t),
                               live: LiveSet.t}
        val toLivenessStatements : {statements: amd64.Assembly.t list,
                                    live: LiveSet.t} ->
                                   {statements: (amd64.Assembly.t * Liveness.t) list,
                                    live: LiveSet.t}
        val reLivenessStatements : {statements: (amd64.Assembly.t * Liveness.t) list,
                                    live: LiveSet.t} ->
                                   {statements: (amd64.Assembly.t * Liveness.t) list,
                                    live: LiveSet.t}
        val toLivenessTransfer : {transfer: amd64.Transfer.t,
                                  liveInfo: LiveInfo.t} ->
                                 {transfer: (amd64.Transfer.t * Liveness.t),
                                  live: LiveSet.t}
        val reLivenessTransfer : {transfer: (amd64.Transfer.t * Liveness.t)} ->
                                 {transfer: (amd64.Transfer.t * Liveness.t),
                                  live: LiveSet.t}
        val toLivenessBlock : {block: amd64.Block.t, liveInfo: LiveInfo.t} -> t
        val toLivenessBlock_msg : unit -> unit
        val verifyLivenessBlock : {block: t,
                                   liveInfo: LiveInfo.t} -> bool
        val verifyLivenessBlock_msg : unit -> unit
        val toBlock : {block: t} -> amd64.Block.t   
        val toBlock_msg : unit -> unit
      end
  end
