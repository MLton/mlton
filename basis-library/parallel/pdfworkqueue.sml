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

  local 
    exception Impossible
    open TextIO
  in
  fun die n = (output (stdErr, 
                       "PDFWorkQueue: die at " ^ (Int.toString n) ^ "\n"); 
               flushOut stdErr;
               unlock ();
               raise Impossible)
  end

  datatype 'a mdlist = 
     Cons of 'a mdlist ref * int ref * 'a option ref * 'a mdlist ref 
   | Nil

  (* initialize global state *)
  val head = Cons (ref Nil, ref 0, ref NONE, ref Nil)

  (* private state *)
  structure A = Array
  val state = A.tabulate (W.numberOfProcessors (), fn _ => head)

(*
  fun ntos MNil = "_"
    | ntos (MCons (l, s, c, wr, r)) =
      let 
        val star = case !wr of SOME _ => "*" | _ => ""
        val ls = case !l of MNil => "_"
                          | MCons (_, ls, _, _, _) => ls
        val rs = case !r of MNil => "_"
                          | MCons (_, rs, _, _, _) => rs
      in
        (s ^ star ^ "[" ^ Int.toString (!c) ^ "](" ^ ls ^ "," ^ rs ^ ")")
      end

  fun pr nr =
      let
        fun loop MNil = ()
          | loop (n as MCons (_, _, _, _, r)) =
            let 
            in
              print (ntos n ^ ", ");
              loop (!r)
            end

        val s = case !nr of (MCons (_, s, _, _, _)) => s
                          | MNil => "nil"
      in
        print "hd: ";
        loop head;
        print ("\ncr: " ^ s ^ "\n")
      end

  local
    val count = ref 0
  in
    fun next () = (Int.toString (!count))
                  before count := !count + 1
  end
*)

  fun addWork p ws = 
    let 
      fun add w =
          case A.unsafeSub (state, p) of
            (l as Cons (_, c, wr, rl)) =>
            let in
              (* Can't read wr unless we have the lock! *)
              case !wr of 
                (* Easy if we can replace the current node *)
                  NONE => wr := SOME w
                (* Need to insert to the right of the current node *)
                | SOME _ => 
                  let
                    val r = !rl
                    val n = Cons (ref l, ref 1, ref (SOME w), ref r)
                    val () = c := !c - 1
                    val () = A.unsafeUpdate (state, p, n)
                    val () = rl := n
                  in
                    case r of
                      Cons (lr, _, _, _) => lr := n
                    | Nil => ()
                  end
          end
          | Nil => die 1
    in
      lock ();
      app add ws;
      unlock ()
    end 

  fun getWork p = 
    let
      (* get assumes that its argument doesn't need its ref count 
        decremented *)
      fun get Nil = NONE
        | get (n as Cons (l, c, wr, r)) = 
          let in
            case !wr of 
              SOME w => (wr := NONE;
                         A.unsafeUpdate (state, p, n);
                         c := !c + 1;
                         SOME w)
            (* Otherwise, continue down the list *)
            | _ => get (!r)
          end
    in
      lock ();
      (* leftmost => always start from the head *)
      get head
      before
      unlock ()
    end

  fun finishWork p =
      let in
        case A.unsafeSub (state, p)
             before A.unsafeUpdate (state, p, head) of
          (Cons (l, c, wr, r)) =>
          let 
            val () = lock ()
            (* whatever happens, our state will no longer point at this node *)
            val c' = !c - 1
          in
            (* decrement the ref count *)
            c := c';
            case (!wr, !l, !r, c') of
              (SOME _, _, _, _) => () (* still work to do! *)
            | (NONE, Nil, _, _) => () (* do nothing if first node in list *)
            | (NONE, l as Cons (_, _, _, rl), r, 0) => 
              (* if not first and new ref count is 0 then remove *)
              let in
                rl := r;
                case r of Cons (lr, _, _, _) =>
                          lr := l
                        | Nil => ()
              end
            | (NONE, _, _, _) => (); (* do nothing if ref count > 0 *)
            unlock ()
          end
        | Nil => die 2
      end

  (* PERF could check to see if there are any waiting jobs earlier in the
    queue *)
  fun shouldYield _ = true

  val policyName = "pdf"

end
