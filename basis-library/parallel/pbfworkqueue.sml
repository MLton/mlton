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

  datatype 'a mlist = 
     Cons of 'a * 'a mlist ref 
   | Nil

  (* initialize state *)
  val (head, tail) = (ref Nil, ref Nil)

  fun addWork _ ws = 
    let
      fun add w = 
          tail := (case !tail of
                     Cons (_, r) => 
                     let 
                       val n = Cons (w, ref (!r))
                       val () = r := n
                     in
                       n
                     end
                   | Nil => 
                     let 
                       val n = Cons (w, ref Nil)
                       val () = head := n
                     in
                       n
                     end)
    in
      lock ();
      app add ws;
      unlock ()
    end

  fun getWork _ = 
    let in
      lock ();
      case !head
       of Nil => (unlock ();
                   NONE)
        | Cons (w, t) => (head := !t;
                           case !t of Nil => tail := !t
                                    | _ => ();
                           unlock ();
                           SOME w)
    end

  fun finishWork _ = ()

  fun shouldYield _ = true

  val policyName = "pbf"

end
