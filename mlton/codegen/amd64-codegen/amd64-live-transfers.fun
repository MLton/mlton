(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(*
 * Some of this doesn't make sense if we track the liveness of the GCHold class.
 * Need to update the enque'' of returns, handlers of NonTail and Runtime
 * so they reflect what happens at these transfers; (i.e., stackTop and frontier 
 * are defed on return from NonTail).
 *)

functor amd64LiveTransfers(S: AMD64_LIVE_TRANSFERS_STRUCTS) : AMD64_LIVE_TRANSFERS =
struct
  open S
  open amd64

  local
     open Runtime
  in
     structure CFunction = CFunction
  end

  structure LiveSet = amd64Liveness.LiveSet
  structure LiveInfo = amd64Liveness.LiveInfo
  open amd64JumpInfo
  open amd64LoopInfo

  val track = amd64Liveness.track

  val tracerTop = amd64.tracerTop

  fun temp_uses_defs {uses : Operand.t list,
                      defs : Operand.t list}
    = let
        val baseUses
          = List.fold
            (uses,
             MemLocSet.empty,
             fn (operand, baseUses)
              => case Operand.deMemloc operand
                   of SOME memloc => if amd64Liveness.track memloc
                                       then MemLocSet.add(baseUses, memloc)
                                       else baseUses
                    | NONE => baseUses)

        val tempUses
          = let
              fun doit (operands, tempUses)
                = List.fold
                  (operands,
                   tempUses,
                   fn (operand, tempUses)
                    => case Operand.deMemloc operand
                         of SOME memloc
                          => List.fold(MemLoc.utilized memloc,
                                       tempUses,
                                       fn (memloc, tempUses)
                                        => if amd64Liveness.track memloc
                                             then MemLocSet.add(tempUses, memloc)
                                             else tempUses)
                          | NONE => tempUses)
            in
              doit(defs, 
              doit(uses, 
                   baseUses))
            end

        val baseDefs
          = List.fold
            (defs,
             MemLocSet.empty,
             fn (operand, baseDefs)
              => case Operand.deMemloc operand
                   of SOME memloc => if amd64Liveness.track memloc
                                       then MemLocSet.add(baseDefs, memloc)
                                       else baseDefs
                    | NONE => baseDefs)
        val tempDefs = baseDefs
      in
        {uses = tempUses,
         defs = tempDefs}
      end

  datatype t = T of {get: Label.t -> 
                          ((MemLoc.t * Register.t * bool) list *
                           (MemLoc.t * XmmRegister.t * bool) list),
                     set: Label.t * 
                          ((MemLoc.t * Register.t * bool) list *
                           (MemLoc.t * XmmRegister.t * bool) list) -> unit}

  local

  in
    structure I' = struct
                     open Int
                     fun sign x = if x = 0
                                    then 0
                                  else if x > 0
                                    then 1
                                  else ~1
                   end
    structure I =
      struct
        datatype t = NegInfinity
                   | Finite of I'.t
                   | PosInfinity
        val toString
          = fn NegInfinity => "-inf"
             | Finite n => I'.toString n
             | PosInfinity => "+inf"
        val zero = Finite (I'.zero)

        fun NegInfinity < NegInfinity = false
          | NegInfinity < _ = true
          | (Finite _) < NegInfinity = false
          | (Finite x) < (Finite y) = I'.<(x,y)
          | (Finite _) < PosInfinity = true
          | PosInfinity < _ = false

        fun NegInfinity + PosInfinity = zero
          | NegInfinity + _ = NegInfinity
          | (Finite _) + NegInfinity = NegInfinity
          | (Finite x) + (Finite y) 
          = ((Finite (I'.+(x,y))) handle Overflow => if x > 0 
                                                       then PosInfinity
                                                       else NegInfinity)
          | (Finite _) + PosInfinity = PosInfinity
          | PosInfinity + NegInfinity = zero
          | PosInfinity + _ = PosInfinity

        fun NegInfinity * NegInfinity = PosInfinity
          | NegInfinity * (Finite x)
          = (case I'.sign x
               of ~1 => PosInfinity
                | 0 => zero
                | _ => NegInfinity)
          | NegInfinity * PosInfinity = NegInfinity
          | (Finite x) * NegInfinity
          = (case I'.sign x
               of ~1 => PosInfinity
                | 0 => zero
                | _ => NegInfinity)
          | (Finite x) * (Finite y)
          = ((Finite (I'.*(x, y))) handle Overflow => (case (I'.sign x, I'.sign y)
                                                         of (~1, ~1) => PosInfinity
                                                          | (1, ~1) => NegInfinity
                                                          | (~1, 1) => NegInfinity
                                                          | _ => PosInfinity))
          | (Finite x) * PosInfinity
          = (case I'.sign x
               of ~1 => NegInfinity
                | 0 => zero
                | _ => PosInfinity)
          | PosInfinity * NegInfinity = NegInfinity
          | PosInfinity * (Finite x)
          = (case I'.sign x
               of ~1 => NegInfinity
                | 0 => zero
                | _ => PosInfinity)
          | PosInfinity * PosInfinity = PosInfinity
      end
  end

  fun computeLiveTransfers {chunk = Chunk.T {blocks,...},
                            transferRegs : Entry.t -> Register.t list,
                            transferXmmRegs : Entry.t -> XmmRegister.t list,
                            liveInfo : amd64Liveness.LiveInfo.t,
                            jumpInfo : amd64JumpInfo.t,
                            loopInfo : amd64LoopInfo.t}
    = let
        val (useLF, useB, sync)
          = case !Control.Native.liveTransfer
              of 1 => (false, false, false)
               | 2 => (false, false, true)
               | 3 => (false, true, false)
               | 4 => (false, true, true)
               | 5 => (true, false, false)
               | 6 => (true, false, true)
               | 7 => (true, true, false)
               | _ => (true, true, true)

        val cutoff = !Control.Native.cutoff
        datatype u = Position of I.t | Length of I'.t

        val {get = getInfo :
                   Label.t -> 
                   {block: Block.t,
                    pred: Label.t list ref,
                    succ: Label.t list ref,
                    live: {memloc: MemLoc.t,
                           distanceF': u option ref,
                           distanceF: (I.t * Label.t option) option ref,
                           distanceB': u option ref,
                           distanceB: (I.t * Label.t option) option ref} vector,
                    liveTransfers: ((MemLoc.t * Register.t * bool ref) list *
                                    (MemLoc.t * XmmRegister.t * bool ref) list) option ref,
                    defed: MemLocSet.t option ref},
             set = setInfo,
             destroy = destInfo}
          = Property.destGetSetOnce
            (Label.plist,
             Property.initRaise ("amd64LiveTransfers:getInfo", Label.layout))

        val (labels, funcs)
          = List.fold
            (blocks,
             ([], []),
             fn (block as Block.T {entry, transfer, ...}, (labels, funcs))
              => let
                   val label = Entry.label entry
                   val succ = Transfer.nearTargets transfer
                   val live = LiveInfo.getLive(liveInfo, label)
                   val live = List.fold
                              (succ,
                               live,
                               fn (label, live)
                                => LiveSet.+(live, LiveInfo.getLive(liveInfo, label)))
                   val live = LiveSet.toList live
                   val _ 
                     = setInfo(label,
                               {block = block,
                                pred = ref [],
                                succ = ref succ,
                                live = Vector.fromListMap
                                       (live,
                                        fn memloc 
                                         => {memloc = memloc,
                                             distanceF' = ref NONE,
                                             distanceF = ref NONE,
                                             distanceB' = ref NONE,
                                             distanceB = ref NONE}),
                                liveTransfers = ref NONE,
                                defed = ref NONE})
                   val labels = label::labels
                   val funcs = case entry
                                 of Entry.Func _ => label::funcs
                                  | _ => funcs
                 in
           (labels, funcs)
                 end)

        val labels = Vector.fromList labels
        val funcs = Vector.fromList funcs

        val _
          = Vector.foreach
            (labels,
             fn label
              => let
                   val {block, ...} = getInfo label
                   fun doit target
                     = let
                         val {pred = pred', ...} = getInfo target
                       in
                         List.push (pred', label)
                       end
                   val Block.T {transfer, ...} = block
                   datatype z = datatype Transfer.t
                 in
                   case transfer
                     of Goto {target, ...} 
                      => doit target
                      | Iff {truee, falsee, ...} 
                      => (doit truee; 
                          doit falsee)
                      | Switch {cases, default, ...}
                      => (doit default;
                          Transfer.Cases.foreach(cases, doit o #2))
                      | Tail {...}
                      => ()
                      | NonTail {return, handler, ...}
                      => (doit return;
                          case handler 
                            of SOME handler => doit handler
                             | NONE => ())
                      | Return {...}
                      => ()
                      | Raise {...}
                      => ()
                      | CCall {return, ...}
                      => Option.app (return, doit)
                 end)

        val _
          = Vector.foreach
            (labels,
             fn label
              => let
                   val {block, live, ...} = getInfo label
                   val Block.T {entry, statements, transfer, ...} = block

                   val l
                     = List.fold
                       (statements,
                        I'.two,
                        fn (Assembly.Comment _, l) => l
                         | (_, l) => I'.+(l, I'.one))

                   fun pos ([], n, m)
                     = let
                         val {uses, defs, ...}
                           = Transfer.uses_defs_kills transfer
                         val {uses,defs} 
                           = temp_uses_defs {uses = uses,
                                             defs = defs}
                       in
                         Vector.foreach
                         (live,
                          fn {memloc, distanceF' as ref NONE, ...}
                           => if MemLocSet.contains(uses,memloc)
                                then distanceF' := SOME (Position (I.Finite n))
                                else distanceF' := SOME (Length l)
                           | _ => ());
                         Vector.foreach
                         (live,
                          fn {memloc, distanceB', ...}
                           => if MemLocSet.contains(uses,memloc)
                                 orelse
                                 MemLocSet.contains(defs,memloc)
                                then distanceB' := SOME (Position (I.Finite m))
                                else ())
                       end
                     | pos ((Assembly.Comment _)::assembly,n,m)
                     = pos (assembly,n,m)
                     | pos (asm::assembly,n,m)
                     = let
                         val {uses,defs,...} 
                              = Assembly.uses_defs_kills asm
                         val {uses,defs} 
                           = temp_uses_defs {uses = uses,
                                             defs = defs}
                       in
                         Vector.foreach
                         (live,
                          fn {memloc, distanceF' as ref NONE, ...}
                           => if MemLocSet.contains(uses,memloc)
                                then distanceF' := SOME (Position (I.Finite n))
                                else ()
                           | _ => ());
                         Vector.foreach
                         (live,
                          fn {memloc, distanceB', ...}
                           => if MemLocSet.contains(uses,memloc)
                                 orelse
                                 MemLocSet.contains(defs,memloc)
                                then distanceB' := SOME (Position (I.Finite m))
                                else ());
                         pos(assembly, I'.+(n, I'.one), I'.-(m, I'.one))
                       end
                 in
                   let
                     val n = I'.zero
                     val m = I'.-(l, I'.one)
                     val {uses,defs,...}
                       = Entry.uses_defs_kills entry
                     val {uses,defs} 
                       = temp_uses_defs {uses = uses,
                                         defs = defs}
                   in
                     Vector.foreach
                     (live,
                      fn {memloc, distanceF' as ref NONE, ...}
                       => if MemLocSet.contains(uses,memloc)
                            then distanceF' := SOME (Position (I.Finite n))
                            else ()
                       | _ => ());
                     Vector.foreach
                     (live,
                      fn {memloc, distanceB', ...}
                       => if MemLocSet.contains(uses,memloc)
                             orelse
                             MemLocSet.contains(defs,memloc)
                            then distanceB' := SOME (Position (I.Finite m))
                            else distanceB' := SOME (Length l));
                     pos(statements, I'.+(n, I'.one), I'.-(m, I'.one))
                   end 
                 end)

        fun get_distanceF {temp: MemLoc.t,
                           label: Label.t}
          = let
              val {block, succ, live, ...} = getInfo label
              val Block.T {transfer, ...} = block
            in
              case Vector.peek
                   (live, 
                    fn {memloc, ...} => MemLoc.eq(temp, memloc))
                of SOME {distanceF = ref (SOME (df, dfl)), ...}
                 => (df, dfl)
                 | SOME {distanceF', distanceF, ...}
                 => (case valOf (!distanceF')
                       of Position n => (distanceF := SOME (n, SOME label); 
                                         (n, SOME label))
                        | Length n
                        => let
                             val loopLabels = getLoopLabels (loopInfo, label)
                             val _ = distanceF := SOME (I.PosInfinity, NONE)
                             fun default ()
                               = let
                                   val n = I.Finite n
                                   val (min, minl)
                                     = List.fold
                                       (!succ,
                                        (I.PosInfinity, NONE),
                                        fn (label, (min, minl))
                                         => let
                                              val (n', l') 
                                                = get_distanceF {temp = temp,
                                                                 label = label}
                                              val n' = I.+(n, n')
                                              val n''
                                                = case (l', useLF)
                                                    of (NONE, _) => n'
                                                     | (_, false) => n'
                                                     | (SOME l', true)
                                                     => if List.contains
                                                           (loopLabels,
                                                            l', Label.equals)
                                                          then n'
                                                          else I.*(I.Finite 5, n')
                                            in
                                              if I.<(n'', min)
                                                then (n', l')
                                                else (min, minl)
                                            end)
                                 in
                                   (min, minl)
                                 end

                             datatype z = datatype Transfer.t
                             val (n, l)
                               = case transfer
                                   of Tail _ => (I.PosInfinity, NONE)
                                    | NonTail _ => (I.PosInfinity, NONE)
                                    | Return _ => (I.PosInfinity, NONE)
                                    | Raise _ => (I.PosInfinity, NONE)
                                    | CCall {func, ...}
                                    => if CFunction.maySwitchThreads func
                                          orelse Size.class (MemLoc.size temp) <> Size.INT
                                         then (I.PosInfinity, NONE)
                                         else default ()
                                  | _ => default ()
                           in
                             distanceF := SOME (n, l) ; (n, l)
                           end)
                 | _ => (I.PosInfinity, NONE)
            end

        fun get_distanceB {temp: MemLoc.t,
                           label: Label.t}
          = let
              val {block, pred, live, ...} = getInfo label
              val Block.T {entry, ...} = block
            in
              case Vector.peek
                   (live, 
                    fn {memloc, ...} => MemLoc.eq(temp, memloc))
                of SOME {distanceB = ref (SOME (db, dbl)), ...}
                 => (db, dbl)
                 | SOME {distanceB, ...}
                 => let
                      val loopLabels = getLoopLabels(loopInfo, label)
                      val _ = distanceB := SOME (I.PosInfinity, NONE)
                      fun default () 
                        = List.fold
                          (!pred, 
                           (I.PosInfinity, NONE),
                           fn (label, (min, minl))
                            => let
                                 val {live, ...} = getInfo label
                               in
                                 case Vector.peek
                                      (live, 
                                       fn {memloc, ...} => MemLoc.eq(temp, memloc))
                                   of SOME {distanceB', ...}
                                    => (case valOf(!distanceB')
                                          of Position n 
                                           => if I.<(n, min)
                                                then (n, SOME label)
                                                else (min, minl)
                                           | Length n
                                           => let
                                                val n = I.Finite n
                                                val (n', l') 
                                                  = get_distanceB {temp = temp,
                                                                   label = label}
                                                val n' = I.+(n, n')
                                                val n''
                                                  = case (l', useLF)
                                                      of (NONE, _) => n'
                                                       | (_, false) => n'
                                                       | (SOME l', true)
                                                       => if List.contains
                                                             (loopLabels,
                                                              l', Label.equals)
                                                            then n'
                                                            else I.*(I.Finite 5, n')
                                              in
                                                if I.<(n'', min)
                                                  then (n', l')
                                                  else (min, minl)
                                              end)
                                    | _ => (min, minl)
                               end)

                      datatype z = datatype Entry.t
                      val (n, l)
                        = case entry
                            of Func {...} => (I.PosInfinity, NONE)
                             | Cont {...} => (I.PosInfinity, NONE)
                             | Handler {...} => (I.PosInfinity, NONE)
                             | CReturn {func, ...}
                             => if (CFunction.maySwitchThreads func
                                    orelse Size.class (MemLoc.size temp) <> Size.INT)
                                  then (I.PosInfinity, NONE)
                                  else default ()
                             | _ => default ()
                    in
                      distanceB := SOME (n, l) ; (n, l)
                    end
                 | _ => (I.PosInfinity, NONE)
            end

        local
          val queue = ref (Queue.empty ())
        in
          fun enque x = queue := Queue.enque(!queue, x)
          fun deque () =
             case Queue.deque (!queue) of
                NONE => NONE
              | SOME (queue', x) => (queue := queue'; SOME x)
        end

        fun doit {label, hints}
          = let
              val {block as Block.T {entry, ...}, 
                   live = liveData, liveTransfers, ...} = getInfo label
            in
              case !liveTransfers
                of SOME _ => ()
                 | NONE 
                 => let
                      val loopLabels = getLoopLabels(loopInfo, label)
                      val Block.T {transfer, ...} = block

                      val (regHints, xmmregHints) = hints

                      val live = LiveSet.toList(LiveInfo.getLive(liveInfo, label))

                      val _ 
                        = if true then ()
                          else
                          (print (Label.toString label);
                           print "\nloopLabels: ";
                           print (List.toString Label.toString loopLabels);
                           print "\nliveData:\n";
                           Vector.foreach
                           (liveData,
                            fn {memloc, distanceF', distanceB', ...} => 
                            (print (MemLoc.toString memloc);
                             print ": ";
                             case !distanceF' of
                               NONE => print "?"
                             | SOME (Position i) => (print "Pos "; print (I.toString i))
                             | SOME (Length i) => (print "Len "; print (I'.toString i));
                             print " ";
                             case !distanceB' of
                               NONE => print "?"
                             | SOME (Position i) => (print "Pos "; print (I.toString i))
                             | SOME (Length i) => (print "Len "; print (I'.toString i));
                             print "\n"));
                           print "regHints:\n";
                           List.foreach
                           (regHints,
                            fn (memloc,register,sync) =>
                            (print (MemLoc.toString memloc);
                             print ": ";
                             print (Register.toString register);
                             print ": ";
                             print (Bool.toString (!sync));
                             print "\n"));
                           print "xmmregHints:\n";
                           List.foreach
                           (xmmregHints,
                            fn (memloc,register,sync) =>
                            (print (MemLoc.toString memloc);
                             print ": ";
                             print (XmmRegister.toString register);
                             print ": ";
                             print (Bool.toString (!sync));
                             print "\n"));
                           print "live:\n";
                           List.foreach
                           (live,
                            fn memloc
                             => (print (MemLoc.toString memloc);
                                 print "\n"));
                           print "distance_F:\n";
                           List.foreach
                           (live,
                            fn memloc
                             => (print (MemLoc.toString memloc);
                                 print ": ";
                                 let
                                   val (n, l) = get_distanceF {temp = memloc,
                                                               label = label}
                                 in
                                   print (I.toString n);
                                   print " ";
                                   print (Option.toString Label.toString l)
                                 end;
                                 print "\n"));
                           print "distance_B:\n";
                           List.foreach
                           (live,
                            fn memloc
                             => (print (MemLoc.toString memloc);
                                 print ": ";
                                 let
                                   val (n, l) = get_distanceB {temp = memloc,
                                                               label = label}
                                 in
                                   print (I.toString n);
                                   print " ";
                                   print (Option.toString Label.toString l)
                                 end;
                                 print "\n")))

                      val live
                        = if not useB
                            then List.keepAllMap
                                 (live,
                                  fn memloc
                                   => case get_distanceF {temp = memloc, 
                                                          label = label}
                                        of (I.Finite n, SOME l)
                                         => if n < cutoff
                                              then if useLF
                                                     then if List.contains
                                                             (loopLabels,
                                                              l, Label.equals)
                                                            then SOME (memloc, n)
                                                            else SOME (memloc, n * 5)
                                                     else SOME (memloc, n)
                                              else NONE
                                         | (I.PosInfinity, _)
                                         => NONE
                                         | _
                                         => Error.bug 
                                            "amd64LiveTransfers.computeLiveTransfers.live: get_distanceF")
                            else List.keepAllMap
                                 (live,
                                  fn memloc
                                   => case (get_distanceB {temp = memloc, 
                                                           label = label},
                                            get_distanceF {temp = memloc, 
                                                           label = label})
                                        of ((I.PosInfinity, _), _)
                                         => NONE
                                         | (_, (I.PosInfinity, _))
                                         => NONE
                                         | ((I.Finite n, SOME nl), 
                                            (I.Finite m, SOME ml))
                                         => if (n + m) < cutoff
                                              then if useLF
                                                     then case (List.contains
                                                                (loopLabels,
                                                                 nl, Label.equals),
                                                                List.contains
                                                                (loopLabels,
                                                                 ml, Label.equals))
                                                            of (true, true)
                                                             => SOME (memloc, n + m)
                                                             | (true, false)
                                                             => SOME (memloc, 
                                                                      n + 5 * m)
                                                             | (false, true)
                                                             => SOME (memloc,
                                                                      5 * n + m)
                                                             | (false, false)
                                                             => SOME (memloc,
                                                                      5 * n + 5 * m)
                                                     else SOME (memloc, n + m)
                                              else NONE
                                         | _
                                         => Error.bug 
                                            "amd64LiveTransfers.computeLiveTransfers.live: get_distanceB")

                      (* List.partition will reverse the lists.
                       * So sort in increasing order.
                       *)
                      val live
                        = List.insertionSort
                          (live, fn ((_,n1),(_,n2)) => I'.>(n1, n2))

                      val _ 
                        = if true then () else
                          (print "live:\n";
                           List.foreach
                           (live,
                            fn (memloc,n)
                             => (print (MemLoc.toString memloc);
                                 print ": ";
                                 print (I'.toString n);
                                 print "\n")))

                      val {yes = liveRegs, no = liveXmmRegs}
                        = List.partition
                          (live,
                           fn (memloc,_) 
                            => Size.class (MemLoc.size memloc) = Size.INT)

                      val liveRegs
                        = List.map
                          (liveRegs,
                           fn (memloc,weight)
                            => case List.peek
                                    (regHints,
                                     fn (memloc',_,_)
                                      => MemLoc.eq(memloc,memloc'))
                                 of SOME (_,register',_)
                                 => (memloc,weight,SOME register')
                                 | NONE 
                                 => (memloc,weight,NONE))                 

                      val rec doitRegs
                        = fn ([],_,liveTransfers) => liveTransfers
                           | (_,[],liveTransfers) => liveTransfers
                           | (transferRegs,
                              (memloc,_,register)::live,
                              liveTransfers)
                           => let
                                fun finish register
                                  = let
                                      val transferRegs
                                        = List.removeAll
                                          (transferRegs,
                                           fn register'
                                            => Register.coincide(register, 
                                                                 register'))
                                    in
                                      doitRegs 
                                      (transferRegs,
                                       live,
                                       (memloc,register,ref true)::liveTransfers)
                                    end

                                fun default ()
                                  = let
                                      val size = MemLoc.size memloc
                                      val transferRegs'
                                        = List.keepAllMap
                                          (transferRegs,
                                           fn register
                                            => if Size.eq
                                                  (size,
                                                   Register.size register)
                                                 then SOME
                                                      (register,
                                                       List.index
                                                       (live,
                                                        fn (_,_,SOME register')
                                                         => Register.eq
                                                            (register,
                                                             register')
                                                         | (_,_,NONE) => false))
                                                 else NONE)
                                      val transferRegs'
                                        = List.insertionSort
                                          (transferRegs',
                                           fn ((_,SOME index1),(_,SOME index2))
                                            => Int.>(index1, index2)
                                            | ((_, NONE),_)
                                            => true
                                            | (_, (_, NONE))
                                            => false)
                                    in
                                      case transferRegs'
                                        of nil
                                         => doitRegs (transferRegs,
                                                      live,
                                                      liveTransfers)
                                         | (register,_)::_
                                         => finish register
                                    end
                              in
                                case register
                                  of SOME register
                                   => if List.contains(transferRegs,
                                                       register,
                                                       Register.eq)
                                        then finish register
                                        else default ()
                                   | NONE => default ()
                              end

                      val liveRegsTransfers = doitRegs(transferRegs entry, liveRegs, [])

                      val liveXmmRegs
                        = List.map
                          (liveXmmRegs,
                           fn (memloc,weight)
                            => case List.peek
                                    (xmmregHints,
                                     fn (memloc',_,_)
                                      => MemLoc.eq(memloc,memloc'))
                                 of SOME (_,register',_)
                                 => (memloc,weight,SOME register')
                                 | NONE 
                                 => (memloc,weight,NONE))

                      val rec doitXmmRegs
                        = fn ([],_,liveTransfers) => liveTransfers
                           | (_,[],liveTransfers) => liveTransfers
                           | (transferXmmRegs,
                              (memloc,_,register)::live,
                              liveTransfers)
                           => let
                                fun finish register
                                  = let
                                      val transferXmmRegs
                                        = List.removeAll
                                          (transferXmmRegs,
                                           fn register'
                                            => XmmRegister.coincide(register, 
                                                                     register'))
                                    in
                                      doitXmmRegs 
                                      (transferXmmRegs,
                                       live,
                                       (memloc,register,ref true)::liveTransfers)
                                    end

                                fun default ()
                                  = let
                                      val size = MemLoc.size memloc
                                      val transferXmmRegs'
                                        = List.keepAllMap
                                          (transferXmmRegs,
                                           fn register
                                            => if Size.eq
                                                  (size,
                                                   XmmRegister.size register)
                                                 then SOME
                                                      (register,
                                                       List.index
                                                       (live,
                                                        fn (_,_,SOME register')
                                                         => XmmRegister.eq
                                                            (register,
                                                             register')
                                                         | (_,_,NONE) => false))
                                                 else NONE)
                                      val transferXmmRegs'
                                        = List.insertionSort
                                          (transferXmmRegs',
                                           fn ((_,SOME index1),(_,SOME index2))
                                            => Int.>(index1, index2)
                                            | ((_, NONE),_)
                                            => true
                                            | (_, (_, NONE))
                                            => false)
                                    in
                                      case transferXmmRegs'
                                        of nil
                                         => doitXmmRegs (transferXmmRegs,
                                                          live,
                                                          liveTransfers)
                                         | (register,_)::_
                                         => finish register
                                    end
                              in
                                case register
                                  of SOME register
                                   => if List.contains(transferXmmRegs,
                                                       register,
                                                       XmmRegister.eq)
                                        then finish register
                                        else default ()
                                   | NONE => default ()
                              end

                      val liveXmmRegsTransfers = doitXmmRegs(transferXmmRegs entry, liveXmmRegs, [])

                      val _ = liveTransfers := SOME (liveRegsTransfers, 
                                                     liveXmmRegsTransfers)

(*
                      val _
                        = (print "liveRegsTransfers:\n";
                           List.foreach
                           (liveRegsTransfers,
                            fn (memloc,register,sync) =>
                            (print (MemLoc.toString memloc);
                             print ": ";
                             print (Register.toString register);
                             print ": ";
                             print (Bool.toString (!sync));
                             print "\n"));
                           print "liveFltRegsTransfers:\n";
                           List.foreach
                           (liveFltRegsTransfers,
                            fn (memloc,sync) =>
                            (print (MemLoc.toString memloc);
                             print ": ";
                             print (Bool.toString (!sync));
                             print "\n"));
                           print "")
*)

                      fun doit' label = enque {label = label, 
                                               hints = (liveRegsTransfers,
                                                        liveXmmRegsTransfers)}
                      fun doit'' label = enque {label = label, 
                                                hints = ([],[])}
                      fun doit''' func label =
                         let
                            val hints =
                               List.fold
                               (Operand.cReturnTemps (CFunction.return func),
                                ([],[]),
                                fn ({src, dst}, (regHints, xmmregHints)) =>
                                case src of
                                   Operand.Register reg =>
                                      ((dst, reg, ref true) :: regHints,
                                       xmmregHints)
                                 | Operand.XmmRegister reg =>
                                      (regHints,
                                       (dst, reg, ref true) :: xmmregHints)
                                 | _ => (regHints, xmmregHints))
                         in
                            enque {hints = hints,
                                   label = label}
                         end
                      datatype z = datatype Transfer.t
                    in
                      case transfer
                        of Goto {target, ...} 
                         => (doit' target)
                         | Iff {truee, falsee, ...}
                         => (doit' truee;
                             doit' falsee)
                         | Switch {cases, default, ...}
                         => (doit' default;
                             Transfer.Cases.foreach(cases, doit' o #2))
                         | Tail {...}
                         => ()
                         | NonTail {return, handler, ...}
                         => (doit'' return;
                             case handler 
                               of SOME handler => doit'' handler
                                | NONE => ())
                         | Return {...}
                         => ()
                         | Raise {...}
                         => ()
                         | CCall {func, return, ...}
                         => if CFunction.maySwitchThreads func
                              then Option.app (return, doit'')
                            else Option.app (return, doit''' func)
                    end
            end

        val _ = Vector.foreach
                (funcs, 
                 fn label => enque {label = label, hints = ([],[])})

        fun loop ()
          = (case deque ()
               of NONE => ()
                | SOME {label, hints}
                => (doit {label = label, hints = hints};
                    loop ()))
        val _ = loop ()

        fun doit {label, defed = defed'}
          = let

              val {block, liveTransfers, defed, ...} = getInfo label
              val (liveRegs, liveXmmRegs) = valOf (!liveTransfers)

              val defed'
                = case getNear(jumpInfo, label)
                    of None => MemLocSet.empty
                     | Count 0 => MemLocSet.empty
                     | Count 1 => defed'
                     | Count _ 
                     => MemLocSet.subset
                        (defed',
                         fn memloc
                          => List.exists
                             (liveRegs, 
                              fn (memloc',_,_) => MemLoc.eq(memloc', memloc))
                             orelse
                             List.exists
                             (liveXmmRegs, 
                              fn (memloc',_,_) => MemLoc.eq(memloc', memloc)))

              fun default defed''
                = let
                    val Block.T {entry, statements, transfer, ...} = block

                    val _ = List.foreach
                            (liveRegs,
                             fn (memloc,_,sync) 
                              => if MemLocSet.contains(defed', memloc)
                                   then sync := false
                                   else ())
                    val _ = List.foreach
                            (liveXmmRegs,
                             fn (memloc,_,sync) 
                              => if MemLocSet.contains(defed', memloc)
                                   then sync := false
                                   else ())

                    val defed' = MemLocSet.+(defed'', defed')
                    val _ = defed := SOME defed'

                    fun doit' (defed', defs)
                      = List.fold
                        (defs,
                         defed',
                         fn (def,defed')
                          => case Operand.deMemloc def
                               of SOME def => if track def
                                                then MemLocSet.add(defed', def)
                                                else defed'
                                | NONE => defed')

                       val {defs, ...} = Entry.uses_defs_kills entry
                       val defed' = doit' (defed', defs)

                       val defed'
                         = List.fold
                           (statements,
                            defed',
                            fn (asm,defed')
                             => let
                                  val {defs, ...} = Assembly.uses_defs_kills asm
                                in
                                  doit' (defed', defs)
                                end)

                       val {defs, ...} = Transfer.uses_defs_kills transfer
                       val defed' = doit' (defed', defs)

                       fun doit' label = doit {label = label,
                                               defed = defed'}
                       fun doit'' label = doit {label = label,
                                                defed = MemLocSet.empty}

                       datatype z = datatype Transfer.t
                  in
                    case transfer
                      of Goto {target, ...}
                       => (doit' target)
                       | Iff {truee, falsee, ...}
                       => (doit' truee;
                           doit' falsee)
                       | Switch {cases, default, ...}
                       => (Transfer.Cases.foreach(cases, doit' o #2);
                           doit' default)
                       | Tail {...}
                       => ()
                       | NonTail {return, handler, ...}
                       => (doit'' return;
                           case handler 
                             of SOME handler => doit'' handler
                              | NONE => ())
                       | Return {...}
                       => ()
                       | Raise {...}
                       => ()
                       | CCall {func, return, ...}
                       => if CFunction.maySwitchThreads func
                            then Option.app (return, doit'')
                            else Option.app (return, doit')
                  end
            in
              case !defed
                of NONE => default MemLocSet.empty
                 | SOME defed => if MemLocSet.<=(defed',defed)
                                   then ()
                                   else default defed
            end

        val _ = Vector.foreach
                (funcs,
                 fn label => doit {label = label, 
                                   defed = MemLocSet.empty})

        val {get = getLiveTransfers : 
             Label.t -> ((MemLoc.t * Register.t * bool) list *
                         (MemLoc.t * XmmRegister.t * bool) list),
             set = setLiveTransfers, ...}
           = Property.getSet
             (Label.plist, 
              Property.initRaise ("amd64LiveTransfers:getLiveTransfers", Label.layout))

        val _ = Vector.foreach
                (labels,
                 fn label
                  => let
                       val {liveTransfers, ...} = getInfo label
                       val (liveRegs, liveXmmRegs) = valOf (!liveTransfers)
                       val (liveRegs, liveXmmRegs)
                         = if sync
                             then (List.map
                                   (liveRegs, 
                                    fn (memloc,reg, sync) => (memloc, reg, !sync)),
                                   List.map
                                   (liveXmmRegs, 
                                    fn (memloc,reg, sync) => (memloc, reg, !sync)))
                             else (List.map
                                   (liveRegs, 
                                    fn (memloc,reg, _) => (memloc, reg, false)),
                                   List.map
                                   (liveXmmRegs, 
                                    fn (memloc,reg, _) => (memloc, reg, false)))
                     in
                       setLiveTransfers(label, (liveRegs, liveXmmRegs))
                     end)

        val _ = destInfo ()
      in
        T {get = getLiveTransfers,
           set = setLiveTransfers}
      end


  val computeLiveTransfers 
    = fn {chunk, transferRegs, transferXmmRegs, liveInfo, jumpInfo, loopInfo}
       => if !Control.Native.liveTransfer > 0
            then computeLiveTransfers {chunk = chunk, 
                                       transferRegs = transferRegs,
                                       transferXmmRegs = transferXmmRegs,
                                       liveInfo = liveInfo, 
                                       jumpInfo = jumpInfo,
                                       loopInfo = loopInfo}
            else let
                   val {get = getLiveTransfers,
                        set = setLiveTransfers, ...}
                     = Property.getSetOnce(Label.plist, 
                                           Property.initConst ([], []))
                 in
                   T {get = getLiveTransfers,
                      set = setLiveTransfers}
                 end

  val (computeLiveTransfers : {chunk : Chunk.t,
                               transferRegs : Entry.t -> Register.t list,
                               transferXmmRegs : Entry.t -> XmmRegister.t list,
                               liveInfo : LiveInfo.t,
                               jumpInfo : amd64JumpInfo.t,
                               loopInfo : amd64LoopInfo.t} -> t,
       computeLiveTransfers_msg)
    = tracerTop
      "computeLiveTransfers"
      computeLiveTransfers

  fun computeLiveTransfers_totals ()
    = (computeLiveTransfers_msg ())

  fun getLiveTransfers (T {get, ...}, label) = get label

  fun setLiveTransfersEmpty (T {set, ...}, label) = set(label, ([], []))
end
