(* running.sml
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

(* running.sml
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *
 * A flag to tell us if CML is running.  This gets set and cleared in the
 * RunCMLFn functor, but other modules need to test it.
 *)

structure Running =
  struct
    val isRunning = ref false
  end
