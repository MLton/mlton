structure MLtonParallelBasic :> MLTON_PARALLEL_BASIC =
struct

  type void = unit
  type work = unit -> void

  val numberOfProcessors = MLtonParallelInternal.numberOfProcessors

  structure Q = WorkQueue (struct 
                             type work = work 
                             val numberOfProcessors = fn () => numberOfProcessors
                           end) 
    :> PARALLEL_WORKQUEUE where type work = work

  val processorNumber = MLtonParallelInternal.processorNumber
  val profileDisable = _import "GC_profileDisable": unit -> unit;
  val profileEnable = _import "GC_profileEnable": unit -> unit;

  exception Parallel of string

  structure T = MLtonThread
  type 'a t = 'a T.t

  val enabled = ref true

  fun schedule ws () = 
    let 
      fun loop p =
          let in
            case Q.getWork p
             of NONE => 
                let in
                  (* if !enabled then (enabled := false; profileDisable ()) else (); *)
                  ()
                end
              | SOME w => 
                let in
                  (* if not (!enabled) then (enabled := true; profileEnable ()) else (); *)
                  w ()
                  handle e => TextIO.output (TextIO.stdErr, 
                                             ("WARNING: Caught exception \""
                                              ^ (Primitive.Exn.name e) 
                                              ^ "\" in parallel scheduler!\n"))
                end;
            (* NB we call processorNumber again here in case that this job has
             been split across two processors *)
            loop (processorNumber ())
          end

      val p = processorNumber ()
    in
      Q.addWork p ws;
      Q.finishWork p;
      loop p
    end

  fun suspend f = 
      T.switch 
          (fn k => 
              let 
                val ws = f k
              in
                (* Note that we cannot call addWork on this thread!  One of
                 the ws may contain a reference to the current thread, and if
                 that work is scheduled it would be running on the same stack
                 as us! *)
                (* Also, we can't just call schedule here because we need to
                 preserve the current thread/stack.  Instead we create a new
                 thread that will continue by calling schedule. *)
                T.prepare (T.new (schedule ws), ())
              end)

  fun resume (k, v) = 
      if Q.shouldYield (processorNumber ()) then
        let in
          schedule [fn () => T.switch (fn _ => T.prepare (k, v))] ()
        end
      else
        T.switch (fn _ => T.prepare (k, v))

  val return = schedule []

  fun addWork ws = 
      let 
        val p = processorNumber ()
      in
        if Q.shouldYield p then
          suspend (fn k => (fn () => resume (k, ()))::ws)
        else
          Q.addWork p ws
      end

  fun continue f = 
      if Q.shouldYield (processorNumber ()) then
        suspend (fn k => [fn () => resume (k, f ())])
      else
        f ()

  val () = (_export "Parallel_run": (unit -> void) -> unit;) return
  (* init MUST come after schedulerLoop has been exported *)
  val () = (_import "Parallel_init": unit -> unit;) ()

  val policyName = Q.policyName
  val maxBytesLive = _import "Parallel_maxBytesLive": unit -> Word64.word;
  val resetBytesLive = _import "Parallel_resetBytesLive": unit -> unit;

end
