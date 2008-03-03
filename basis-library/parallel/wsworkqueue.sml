functor WorkQueue (W : sig type work val numberOfProcessors : unit -> int end) : PARALLEL_WORKQUEUE =
struct

  type proc = int
  type work = W.work

  val lock = _import "Parallel_lock": int -> unit;
  val unlock = _import "Parallel_unlock": int -> unit;

  local 
    exception Impossible
    open TextIO
  in
  fun die n = (output (stdErr, 
                       "WSWorkQueue: die at " ^ (Int.toString n) ^ "\n"); 
               flushOut stdErr;
               (* XX unlock (); *)
               raise Impossible)
  end

  datatype 'a mdlist = 
     Cons of 'a mdlist ref * int ref * 'a option ref * 'a mdlist ref 
   | Nil

  structure A = Array
  val numberOfProcessors = W.numberOfProcessors ()

  (* private state *)
  val state = A.tabulate (numberOfProcessors, fn _ => nil)

  fun addWork p ws = 
    let 
      val () = lock p
      val lq = A.unsafeSub (state, p)
    in
      A.unsafeUpdate (state, p, ws @ lq);
      unlock p
    end 

  local
    fun victim () = Word.toIntX (MLtonRandom.rand ()) mod numberOfProcessors
  in
  fun getWork p = 
    let
      fun steal () = 
          let 
            val p' = victim ()
            val () = lock p'
            val lq' = A.unsafeSub (state, p')
            val l = length lq'
          in
            (if l > 0 then
               let 
                 val (ws, ws') = (List.take (lq', l - 1), List.drop (lq', l - 1))
                 val w = case ws' of w::nil => w | _ => die 1
               in
                 A.unsafeUpdate (state, p', ws);
                 SOME w
               end
             else NONE)
            before unlock p'
          end

      val () = lock p
      val lq = A.unsafeSub (state, p)
      val res = case lq of 
                  w::ws => (A.unsafeUpdate (state, p, ws); 
                            SOME w 
                            before unlock p)
                | nil => (unlock p; 
                          steal ())
    in
      res
    end
  end

  fun finishWork p = ()

  fun shouldYield _ = false

  val policyName = "ws"

end
