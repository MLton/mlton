
type int = Int.t
type word = Word.t

signature X86_LIVE_TRANSFERS_STRUCTS =
  sig
    structure x86 : X86
    structure x86MLton : X86_MLTON
    sharing x86 = x86MLton.x86
    structure x86Liveness : X86_LIVENESS
    sharing x86 = x86Liveness.x86
    sharing x86.MemLocSet = x86Liveness.LiveSet
    structure x86JumpInfo : X86_JUMP_INFO
    sharing x86 = x86JumpInfo.x86
    structure x86LoopInfo : X86_LOOP_INFO
    sharing x86 = x86LoopInfo.x86
  end

signature X86_LIVE_TRANSFERS =
  sig
    include X86_LIVE_TRANSFERS_STRUCTS

    type t

    val computeLiveTransfers : {chunk : x86.Chunk.t,
				transferRegs : x86.Register.t list,
				transferFltRegs : Int.t, 
				liveInfo : x86Liveness.LiveInfo.t,
				jumpInfo : x86JumpInfo.t,
				loopInfo : x86LoopInfo.t} -> t
    val computeLiveTransfers_totals : unit -> unit

    val getLiveTransfers : t * x86.Label.t -> 
                           ((x86.MemLoc.t * x86.Register.t * bool) list *
			    (x86.MemLoc.t * bool) list)
    val setLiveTransfersEmpty : t * x86.Label.t -> unit 
  end
