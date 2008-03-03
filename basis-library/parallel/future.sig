signature MLTON_PARALLEL_FUTURE =
sig

  (* a future yielding a value of type 'a *)
  type 'a t

  (* depending on the scheduling policy BOTH future and force may suspend the
    current task. *)
  (* create a new parallel future.  futures may be executed speculatively. *)
  val future : (unit -> 'a) -> 'a t

  (* force the execution of a future if that has not yet occured. *)
  val force : 'a t -> 'a

end
