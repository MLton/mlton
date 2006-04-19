(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure MLtonCont:> MLTON_CONT =
struct

structure Thread = Primitive.Thread
val gcState = Primitive.GCState.gcState

type 'a t = (unit -> 'a) -> unit

fun callcc (f: 'a t -> 'a): 'a =
   if MLtonThread.amInSignalHandler () then
      die "callcc can not be used in a signal handler\n"
   else 
      let
         datatype 'a state =
            Original of 'a t -> 'a
          | Copy of unit -> 'a
          | Clear
         val r: 'a state ref = ref (Original f)
         val _ = Thread.atomicBegin () (* Match 1 *)
         val _ = Thread.copyCurrent ()
      in
         case (!r before r := Clear) of
            Clear => raise Fail "callcc saw Clear"
          | Copy v => (Thread.atomicEnd () (* Match 2 *)
                       ; v ())
          | Original f =>
               let
                  val t = Thread.savedPre gcState
               in
                  Thread.atomicEnd () (* Match 1 *)
                  ; f (fn v =>
                       let
                          val _ = Thread.atomicBegin () (* Match 2 *)
                          val _ = r := Copy v
                          val new = Thread.copy t
                          (* The following Thread.atomicBegin () 
                           * is matched by Thread.switchTo.
                           *)
                          val _ = Thread.atomicBegin ()
                       in
                          Thread.switchTo new
                       end)
               end
      end

fun ('a, 'b) throw' (k: 'a t, v: unit -> 'a): 'b =
   (k v; raise Fail "throw bug")
   
fun ('a, 'b) throw (k: 'a t, v: 'a): 'b = throw' (k, fn () => v)

fun prepend (k, f) v = throw' (k, f o v)

end
