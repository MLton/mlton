signature MLTON_PARALLEL_BASIC =
sig

  (* the empty type *)
  type void
  (* the type of a parallel task -- work never returns normally *)
  type work = unit -> void
  (* a suspended computation waiting for a value of type 'a *)
  type 'a t

  (* reify the current point of execution and then add the given work to the
   queue; the list is assumed to be given in order of decreasing priority.  NB
   no processor should resume this suspension until *AFTER* it has returned a
   new set of parallel tasks. *)
  val suspend : ('a t -> work list) -> 'a

  (* end the current task and add the given computation to the queue *)
  val resume : 'a t * 'a -> void

  (* end the current task and return control to the scheduler *)
  val return : unit -> void

  (* add the given work to the queue; the list is assumed to be given in order
   of decreasing priority.  may suspend under some scheduling policies -- the
   implementation performs no worse than
       addWork ws = suspend (fn k => (fn () => resume (k, ()))::ws)
   *)
  val addWork : work list -> unit

  (* temporarily yield, but continue as scheduling policy permits.  
     the implementation performs no worse than
       continue f = suspend (fn k => [fn () => resume (k, f ())])
   *)
  val continue : (unit -> 'a) -> 'a

  (* general errors related to parallelism *)
  exception Parallel of string

  (* informational *)
  val policyName : string
  val numberOfProcessors : int
  val maxBytesLive : unit -> Word64.word
  val resetBytesLive : unit -> unit

end
