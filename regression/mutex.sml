functor Z (S: sig
		 structure MLton:
		    sig
		       structure Itimer:
			  sig
			     datatype t =
				Prof
			      | Real
			      | Virtual

			     val set: t * {value: Time.time,
					   interval: Time.time} -> unit
			  end
		       structure Thread:
			  sig
			     type 'a t

			     val atomicBegin: unit -> unit
			     val atomicEnd: unit -> unit
			     val new: ('a -> unit) -> 'a t
			     val switch: ('a t -> 'b t * 'b) -> 'a
			  end
		       structure Signal:
			  sig
			     type t

			     val alrm: t
			     val handleWith':
				t * (unit Thread.t -> unit Thread.t) -> unit
			     val ignore: t -> unit
			  end
		    end
	      end) =
struct
   open S
      
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

	 val topLevel: unit Thread.t option ref = ref NONE

	 local
	    val threads: unit Thread.t Queue.t = Queue.new ()
	 in
	    fun ready t = Queue.enque (threads, t)
	    fun next () =
	       case Queue.deque threads of
		  NONE => valOf (!topLevel)
		| SOME t => t
	 end
      
	 fun 'a exit (): 'a = switch (fn _ =>
				      (print "exiting\n"
				       ; (next (), ())))
   
	 fun new (f: unit -> unit): unit Thread.t =
	    Thread.new (fn () => ((f () handle _ => exit ())
				  ; exit ()))
	    
	 fun schedule t = (ready t; next ())

	 fun yield (): unit = switch (fn t => (schedule t, ()))

	 val spawn = ready o new

	 fun setItimer t =
	    Itimer.set (Itimer.Real,
			{value = t,
			 interval = t})

	 fun run (): unit =
	    (switch (fn t =>
		     (topLevel := SOME t
		      ; (new (fn () =>
			      (handleWith' (alrm, schedule)
			       ; setItimer (Time.fromMilliseconds 10))),
			 ())))
	     ; setItimer Time.zeroTime
	     ; ignore alrm
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
					       ; (next (), ())))
				    ; loop ())
			   else (locked := true
				 ; Thread.atomicEnd ()))
		  in loop ()
		  end
	       
	       fun safeUnlock (T {locked, waiting, ...}) =
		  (locked := false
		   ; (case Queue.deque waiting of
			 NONE => ()
		       | SOME t => ready t))

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
				      ; for (0, 1000, fn _ => ())
				      ; gotIt := false
				      ; Mutex.unlock m
				      ; loop (i - 1)))
		  in loop 10000
		  end))
      in
	 run ()
      end
end

structure Z = Z (structure MLton = MLton)

val _ = Z.main ( CommandLine.name (), CommandLine.arguments () )
