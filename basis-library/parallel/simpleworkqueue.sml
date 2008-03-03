functor WorkQueue (W : sig type work val numberOfProcessors : unit -> int end) : PARALLEL_WORKQUEUE =
struct

  type proc = int
  type work = W.work

  local
    val lock_ = _import "Parallel_lock": int -> unit;
    val unlock_ = _import "Parallel_unlock": int -> unit;
  in
  fun lock () = lock_ 0
  fun unlock () = unlock_ 0
  end
    
  (* initialize state *)
  val queue = ref nil : work list ref

  fun addWork _ ws = 
    let in
      lock ();
      queue := ws @ (!queue);
      unlock ()
    end

  fun getWork _ = 
    let in
      lock ();
      case !queue
        of nil => (unlock ();
                   NONE)
         | w::ws => (queue := ws;
                     unlock ();
                     SOME w)
    end

  fun finishWork _ = ()

  fun shouldYield _ = true

  val policyName = "sim"

end
