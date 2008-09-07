open Posix.Signal MLton.Signal
      
fun for (start, stop, f) =
   let
      fun loop i =
         if i >= stop
            then ()
         else (f i; loop (i + 1))
   in
      loop start
   end
   
structure Queue:
   sig
      type 'a t

      val new: unit -> 'a t
      val enque: 'a t * 'a -> unit
      val deque: 'a t -> 'a option
   end =
   struct
      datatype 'a t = T of {front: 'a list ref, back: 'a list ref}

      fun new () = T {front = ref [], back = ref []}

      fun enque (T {back, ...}, x) = back := x :: !back

      fun deque (T {front, back}) =
         case !front of
            [] => (case !back of
                      [] => NONE
                    | l => let val l = rev l
                           in case l of
                              [] => raise Fail "deque"
                            | x :: l => (back := []; front := l; SOME x)
                           end)
          | x :: l => (front := l; SOME x) 
   end

structure Thread:
   sig
      val exit: unit -> 'a
      val run: unit -> unit
      val spawn: (unit -> unit) -> unit
      val yield: unit -> unit
      structure Mutex:
         sig
            type t

            val new: unit -> t
            val lock: t -> unit
            val unlock: t -> unit
         end
   end =
   struct
      open MLton
      open Itimer Signal Thread

      val topLevel: Thread.Runnable.t option ref = ref NONE

      local
         val threads: Thread.Runnable.t Queue.t = Queue.new ()
      in
         fun ready t = Queue.enque (threads, t)
         fun next () : Thread.Runnable.t =
            case Queue.deque threads of
               NONE => valOf (!topLevel)
             | SOME t => t
      end
      
      fun 'a exit (): 'a = switch (fn _ =>
                                   (print "exiting\n"
                                    ; next ()))
   
      fun new (f: unit -> unit): Thread.Runnable.t =
         Thread.prepare
         (Thread.new (fn () => ((f () handle _ => exit ())
                                ; exit ())),
          ())
            
      fun schedule t = (ready t; next ())

      fun yield (): unit = switch (fn t => schedule (Thread.prepare (t, ())))

      val spawn = ready o new

      fun setItimer t =
         Itimer.set (Itimer.Real,
                     {value = t,
                      interval = t})

      fun run (): unit =
         (switch (fn t =>
                  (topLevel := SOME (Thread.prepare (t, ()))
                   ; new (fn () =>
                          (setHandler (alrm, Handler.handler schedule)
                           ; setItimer (Time.fromMilliseconds 10)))))
          ; setItimer Time.zeroTime
          ; setHandler (alrm, Handler.ignore)
          ; topLevel := NONE)
            
      structure Mutex =
         struct
            datatype t = T of {locked: bool ref,
                               waiting: unit Thread.t Queue.t}
                  
            fun new () =
               T {locked = ref false,
                  waiting = Queue.new ()}

            fun lock (T {locked, waiting, ...}) =
               let
                  fun loop () =
                     (Thread.atomicBegin ()
                      ; if !locked
                           then (Thread.atomicEnd ()
                                 ; switch (fn t =>
                                           (Queue.enque (waiting, t)
                                            ; next ()))
                                 ; loop ())
                        else (locked := true
                              ; Thread.atomicEnd ()))
               in loop ()
               end
               
            fun safeUnlock (T {locked, waiting, ...}) =
               (locked := false
                ; (case Queue.deque waiting of
                      NONE => ()
                    | SOME t => ready (Thread.prepare (t,()))))

            fun unlock (m: t) =
               (Thread.atomicBegin ()
                ; safeUnlock m
                ; Thread.atomicEnd ())
         end
   end

open Thread
      
fun main (name, args) =
   let
      val m = Mutex.new ()
      val gotIt = ref false
      val _ = 
         for (0, 10, fn _ =>
              Thread.spawn
              (fn () =>
               let
                  val _ = print "starting\n"
                  fun loop i =
                     if i = 0
                        then ()
                     else (Mutex.lock m
                           ; if !gotIt
                                then raise Fail "bug"
                             else (gotIt := true
                                   ; for (0, 100000, fn _ => ())
                                   ; gotIt := false
                                   ; Mutex.unlock m
                                   ; loop (i - 1)))
               in loop 10000
               end))
   in
      run ()
   end

val _ = main ( CommandLine.name (), CommandLine.arguments () )
