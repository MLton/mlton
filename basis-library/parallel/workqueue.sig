signature PARALLEL_WORKQUEUE =
sig

  (* processor identifier *)
  type proc = int
  (* abstract type of work *)
  type work

  (* these take the identifier of the current processor as their first
   argument *)
  (* add new work to the queue *)
  val addWork : proc -> work list -> unit
  (* remove the next, highest priority work *)
  val getWork : proc -> work option
  (* mark the most recent unit of work as done *)
  val finishWork : proc -> unit
  (* is there higher priority work for the given processor? *)
  val shouldYield : proc -> bool 

  (* name of the current policy *)
  val policyName : string

end
