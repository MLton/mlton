
type int = Int.t
type word = Word.t

signature X86_LOOP_INFO_STRUCTS =
  sig
    structure x86 : X86
  end

signature X86_LOOP_INFO =
  sig
    include X86_LOOP_INFO_STRUCTS
    
    type t

    val createLoopInfo : {chunk: x86.Chunk.t, farLoops: bool} -> t
    val createLoopInfo_msg : unit -> unit
(*
    val verifyLoopInfo : {chunk: x86.Chunk.t,
			  loopInfo: t} -> bool
    val verifyLoopInfo_msg : unit
*)

    val getLoopForest : t -> x86.Label.t list Tree.t list
    val getLoopTreeAt : t * x86.Label.t -> {up: x86.Label.t list Tree.t,
					    down: x86.Label.t list Tree.t} option
    val getLoopDepth : t * x86.Label.t -> int option
    val isLoopHeader : t * x86.Label.t -> bool
    val getLoopDistance : t * x86.Label.t * x86.Label.t -> int option
  end