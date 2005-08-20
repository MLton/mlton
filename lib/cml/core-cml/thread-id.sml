(* thread.sml
 * 2004 Matthew Fluet (mfluet@acm.org)
 *  Ported to MLton threads.
 *)

(* thread.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *)

structure ThreadID : THREAD_ID_EXTRA =
   struct
      structure Assert = LocalAssert(val assert = false)

      structure R = RepTypes


      datatype thread_id = datatype R.thread_id
      datatype thread_id' = datatype thread_id

      fun sameTid (TID{id=a, ...}, TID{id=b, ...}) = a = b
      fun compareTid (TID{id=a, ...}, TID{id=b, ...}) = Int.compare (a, b)
      fun hashTid (TID{id, ...}) = Word.fromInt id

      fun tidToString (TID{id, ...}) =
         concat["[", StringCvt.padLeft #"0" 6 (Int.toString id), "]"]

      fun exnHandler (_ : exn) = ()
      val defaultExnHandler = ref exnHandler

      fun new' n =
         TID {id = n,
              alert = ref false,
              done_comm = ref false,
              exnHandler = ref (!defaultExnHandler),
              props = ref [],
              dead = CVar.new ()}

      local
         val tidCounter = ref 0
      in
         fun new () =
            let 
               val _ = Assert.assertAtomic' ("ThreadID.newTid", NONE)
               val n = !tidCounter
               val _ = tidCounter := n + 1
            in
               new' n
            end

         fun reset () = tidCounter := 0
      end

      fun bogus s =
         let val n = CharVector.foldr (fn (c, n) => 2 * n - Char.ord c) 0 s
         in new' n
         end

      fun mark (TID{done_comm, ...}) = 
         (Assert.assertAtomic' ("ThreadID.mark", NONE)
          ; done_comm := true)
      fun unmark (TID{done_comm, ...}) = 
         (Assert.assertAtomic' ("ThreadID.unmark", NONE)
          ; done_comm := false)
      fun isMarked (TID{done_comm, ...}) = !done_comm
   end
