
type int = Int.t
type word = Word.t

signature X86_JUMP_INFO_STRUCTS =
  sig
    structure x86 : X86
  end

signature X86_JUMP_INFO =
  sig
    include X86_JUMP_INFO_STRUCTS

    datatype status = Count of int | None
    type t

    val newJumpInfo : unit -> t

    val completeJumpInfo : {chunk: x86.Chunk.t,
			    jumpInfo: t} -> unit
    val completeJumpInfo_msg : unit -> unit
    val verifyJumpInfo : {chunk: x86.Chunk.t,
			  jumpInfo: t} -> bool
    val verifyJumpInfo_msg : unit -> unit

    val incNear : t * x86.Label.t -> unit
    val decNear : t * x86.Label.t -> unit
    val getNear : t * x86.Label.t -> status
  end
