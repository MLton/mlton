(* Copyright (C) 1999-2010 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor amd64AllocateRegisters(S: AMD64_ALLOCATE_REGISTERS_STRUCTS) : AMD64_ALLOCATE_REGISTERS =
struct

  open S
  open amd64

  val tracer = amd64.tracer
  val tracerTop = amd64.tracerTop

  fun track memloc = let
                       val trackClasses 
                         = ClassSet.+(ClassSet.+
                                      (!amd64MLton.Classes.livenessClasses,
                                       !amd64MLton.Classes.holdClasses),
                                      ClassSet.fromList
                                      [amd64MLton.Classes.StaticNonTemp,
                                       amd64MLton.Classes.CArg])
                     in
                       ClassSet.contains(trackClasses, MemLoc.class memloc)
                     end
  fun volatile memloc = let
                          val volatileClasses 
                            = !amd64MLton.Classes.volatileClasses
                     in
                       ClassSet.contains(volatileClasses, MemLoc.class memloc)
                     end

  fun partition(l, p)
    = let
        val rec partition' 
          = fn ([],PS) => PS
             | (h::t,PS) => let
                              val rec partition''
                                = fn [] => [[h]]
                                   | P::PS => if List.exists(P,fn x => p(h, x))
                                                then (h::P)::PS
                                                else P::(partition'' PS)
                            in
                              partition'(t,partition'' PS)
                            end
      in
        partition'(l,[])
      end

  fun totalOrder (l, plt)
    = let
        val rec totalOrder'
          = fn ([],l) => l
             | (h::t,l) => let
                             val rec split
                               = fn (lt,t) 
                                  => case List.splitPrefix
                                          (t, fn x => plt(x,h))
                                       of (nil,t) => lt@[h]@t
                                        | (lt',t) => split(lt@lt',t)
                           in
                             totalOrder'(t,split([],l))
                           end
      in
        totalOrder'(l,[])
      end

  val bool_lt
    = fn (false, true) => true
       | _ => false

  val bool_gt
    = fn (true, false) => true
       | _ => false

  fun option_lt lt
    = fn (SOME x, SOME y) => lt (x,y)
       | (NONE, SOME _) => true
       | _ => false

  structure Liveness =
    struct

      datatype futureMemlocTag = FLIVE 
                               | FCOMMIT | FREMOVE | FDEAD 
                               | FUSE | FUSEDEF | FDEF

      val futureMemlocTag_toString
        = fn FLIVE => "FLIVE"
           | FCOMMIT => "FCOMMIT"
           | FREMOVE => "FREMOVE"
           | FDEAD => "FDEAD"
           | FUSE => "FUSE"
           | FUSEDEF => "FUSEDEF"
           | FDEF => "FDEF"

      type futureMemloc = futureMemlocTag * MemLoc.t

      datatype futureMemlocPredTag = FCOMMITP | FREMOVEP | FDEADP 
                                   | FMCOMMITP | FMREMOVEP

      val futureMemlocPredTag_toString
        = fn FCOMMITP => "FCOMMITP"
           | FREMOVEP => "FREMOVEP"
           | FDEADP => "FDEADP"
           | FMCOMMITP => "FMCOMMITP"
           | FMREMOVEP => "FMREMOVEP"

      type futureMemlocPred = futureMemlocPredTag * (MemLoc.t -> bool)

      datatype future = M of futureMemloc | MP of futureMemlocPred

      val future_toString
        = fn (M (tag, memloc))
           => concat [futureMemlocTag_toString tag, " ", MemLoc.toString memloc]
           | (MP (tag, _))
           => concat [futureMemlocPredTag_toString tag]


      type hint = Register.t * MemLoc.t list * MemLocSet.t
      type xmmhint = XmmRegister.t * MemLoc.t list * MemLocSet.t

      val hint_toString
        = fn (register, memlocs, _) 
           => concat ["{ ",
                      List.fold
                      (memlocs,
                       "",
                       fn (memloc, s) => s ^ (MemLoc.toString memloc) ^ " "),
                      "} -> ",
                      Register.toString register]

      val xmmhint_toString
        = fn (register, memlocs, _) 
           => concat ["{ ",
                      List.fold
                      (memlocs,
                       "",
                       fn (memloc, s) => s ^ (MemLoc.toString memloc) ^ " "),
                      "} -> ",
                      XmmRegister.toString register]

      type t = {dead: MemLocSet.t,
                commit: MemLocSet.t,
                remove: MemLocSet.t,
                futures: {pre: future list,
                          post: future list},
                hint: hint list,
                xmmhint: xmmhint list}

(*
      fun toString {dead, commit, remove, futures = {pre, post}, hint, xmmhint}
        = let
            fun doit (name, l, toString, s)
              = List.fold(l, s,
                          fn (x, s)
                           => concat [name, toString x, "\n", s])
            fun doit' (name, l, toString, s)
              = MemLocSet.fold(l, s,
                               fn (x, s)
                                => concat [name, toString x, "\n", s])
          in
            doit'("dead: ", dead, MemLoc.toString,
            doit'("commit: ", commit, MemLoc.toString,
            doit'("remove: ", remove, MemLoc.toString,
            doit("future (pre): ", List.rev pre, future_toString,
            doit("future (post): ", List.rev post, future_toString, 
            doit("hint: ", hint, hint_toString,
            doit("xmmhint: ", xmmhint, xmmhint_toString, "")))))))
          end
*)

      fun toComments {dead, commit, remove, futures = {pre, post}, hint, xmmhint} 
        = let
            fun doit (name, l, toString, ac)
              = List.fold(l, ac, 
                          fn (x, ac) 
                           => (Assembly.comment (concat [name, toString x]))::
                              ac)
            fun doit' (name, l, toString, ac)
              = MemLocSet.fold(l, ac, 
                               fn (x, ac) 
                                => (Assembly.comment (concat [name, toString x]))::
                                   ac)
          in
            doit'("dead: ", dead, MemLoc.toString,
            doit'("commit: ", commit, MemLoc.toString,
            doit'("remove: ", remove, MemLoc.toString,
            doit("future (pre): ", List.rev pre, future_toString,
            doit("future (post): ", List.rev post, future_toString,
            doit("hint: ", hint, hint_toString, 
            doit("xmmhint: ", xmmhint, xmmhint_toString, [])))))))
          end


      datatype commit = NO | COMMIT | REMOVE | DEAD

      fun predict(future, memloc)
        = let
            val rec sawNothing
              = fn [] => if track memloc then DEAD else REMOVE
                 | (M (tag',memloc'))::future
                 => if MemLoc.eq(memloc, memloc')
                      then case tag'
                             of FLIVE => NO
                              | FCOMMIT => sawCommit future
                              | FREMOVE => sawRemove future
                              | FDEAD => DEAD
                              | FUSE => sawUse future
                              | FUSEDEF => NO
                              | FDEF => DEAD
                    else if ((tag' = FUSEDEF) orelse (tag' = FDEF))
                            andalso
                            List.exists
                            (MemLoc.utilized memloc,
                             fn memloc'' => MemLoc.mayAlias(memloc'', memloc'))
                      then REMOVE
                    else if MemLoc.mayAlias(memloc, memloc')
                      then case tag'
                             of FUSE => sawCommit future
                              | FUSEDEF => REMOVE
                              | FDEF => REMOVE
                              | _ => sawNothing future
                    else sawNothing future
                 | (MP (tag',pred'))::future
                 => if pred' memloc
                      then case tag'
                             of FCOMMITP => sawCommit future
                              | FREMOVEP => sawRemove future
                              | FDEADP => DEAD
                              | FMCOMMITP => sawCommit future
                              | FMREMOVEP => sawRemove future
                      else sawNothing future
            and sawCommit
              = fn [] => REMOVE
                 | (M (tag',memloc'))::future
                 => if MemLoc.eq(memloc, memloc')
                      then case tag'
                             of FLIVE => COMMIT
                              | FCOMMIT => sawCommit future
                              | FREMOVE => REMOVE
                              | FDEAD => REMOVE
                              | FUSE => COMMIT
                              | FUSEDEF => COMMIT
                              | FDEF => REMOVE
                    else if MemLoc.mayAlias(memloc, memloc')
                      then case tag'
                             of FUSE => sawCommit future
                              | FUSEDEF => REMOVE
                              | FDEF => REMOVE
                              | _ => sawCommit future
                    else sawCommit future
                 | (MP (tag',pred'))::future
                 => if pred' memloc
                      then case tag'
                             of FCOMMITP => sawCommit future
                              | FREMOVEP => REMOVE
                              | FDEADP => REMOVE
                              | FMCOMMITP => sawCommit future
                              | FMREMOVEP => REMOVE
                      else sawCommit future
            and sawRemove
              = fn [] => REMOVE
                 | (M (tag',memloc'))::future
                 => if MemLoc.eq(memloc, memloc')
                      then case tag'
                             of FLIVE => REMOVE
                              | FCOMMIT => REMOVE
                              | FREMOVE => sawRemove future
                              | FDEAD => DEAD
                              | FUSE => REMOVE
                              | FUSEDEF => REMOVE
                              | FDEF => DEAD
                    else if MemLoc.mayAlias(memloc, memloc')
                      then case tag'
                             of FUSE => REMOVE
                              | FUSEDEF => REMOVE
                              | FDEF => REMOVE
                              | _ => sawRemove future
                    else sawRemove future
                 | (MP (tag',pred'))::future
                 => if pred' memloc
                      then case tag'
                             of FCOMMITP => REMOVE
                              | FREMOVEP => REMOVE
                              | FDEADP => DEAD
                              | FMCOMMITP => REMOVE
                              | FMREMOVEP => sawRemove future
                      else sawRemove future
            and sawUse
              = fn [] => if track memloc then NO else COMMIT
                 | (M (tag',memloc'))::future
                 => if MemLoc.eq(memloc, memloc')
                      then case tag'
                             of FLIVE => NO
                              | FCOMMIT => sawUseCommit future
                              | FREMOVE => NO
                              | FDEAD => NO
                              | FUSE => sawUse future
                              | FUSEDEF => NO
                              | FDEF => NO
                    else if MemLoc.mayAlias(memloc, memloc')
                      then case tag'
                             of FUSE => sawUseCommit future
                              | FUSEDEF => NO
                              | FDEF => NO
                              | _ => sawUse future
                    else sawUse future
                 | (MP (tag',pred'))::future
                 => if pred' memloc
                      then case tag'
                             of FCOMMITP => sawUseCommit future
                              | FREMOVEP => NO
                              | FDEADP => NO
                              | FMCOMMITP => sawUseCommit future
                              | FMREMOVEP => NO
                      else sawUse future
            and sawUseCommit
              = fn [] => if track memloc then NO else COMMIT
                 | (M (tag',memloc'))::future
                 => if MemLoc.eq(memloc, memloc')
                      then case tag'
                             of FLIVE => COMMIT
                              | FCOMMIT => sawUseCommit future
                              | FREMOVE => NO
                              | FDEAD => NO
                              | FUSE => COMMIT
                              | FUSEDEF => COMMIT
                              | FDEF => NO
                    else if MemLoc.mayAlias(memloc, memloc')
                      then case tag'
                             of FUSE => sawUseCommit future
                              | FUSEDEF => NO
                              | FDEF => NO
                              | _ => sawUseCommit future
                    else sawUseCommit future
                 | (MP (tag',pred'))::future
                 => if pred' memloc
                      then case tag'
                             of FCOMMITP => sawUseCommit future
                              | FREMOVEP => NO
                              | FDEADP => NO
                              | FMCOMMITP => sawUseCommit future
                              | FMREMOVEP => NO
                      else sawUseCommit future

            fun check commit
              = if List.exists
                   (MemLoc.utilized memloc,
                    fn memloc' => case predict (future, memloc')
                                    of REMOVE => true
                                     | DEAD => true
                                     | _ => false)
                  then REMOVE
                  else commit

            val default = case sawNothing future
                            of REMOVE => REMOVE
                             | DEAD => DEAD
                             | commit => check commit
          in
            default
          end

      val split
        = fn (set, p)
           => MemLocSet.fold
              (set,
               (MemLocSet.empty,MemLocSet.empty,MemLocSet.empty,MemLocSet.empty),
               fn (memloc, (no, commit, remove, dead))
                => let
                     val add = fn set => MemLocSet.add(set, memloc)
                   in
                     case p memloc
                       of NO => (add no, commit, remove, dead)
                        | COMMIT => (no, add commit, remove, dead)
                        | REMOVE => (no, commit, add remove, dead)
                        | DEAD => (no, commit, remove, add dead)
                   end)

      fun liveness {uses: MemLocSet.t,
                    defs: MemLocSet.t,
                    future: future list} :
                   {dead: MemLocSet.t,
                    commit: MemLocSet.t,
                    remove: MemLocSet.t,
                    future: future list}
        = let
            local
              fun doit' (memlocs, set)
                = MemLocSet.fold
                  (memlocs,
                   set,
                   fn (memloc, set)
                    => MemLocSet.union
                       (set, MemLocSet.fromList (MemLoc.utilized memloc)))
            in
              val allUses
                = doit'(defs,
                  doit'(uses,
                        uses))
              val allDefs 
                = defs
            end

            val current
              = MemLocSet.+(allUses, allDefs)
            val current_usedef
              = MemLocSet.intersect(allUses, allDefs)
            val current_use
              = MemLocSet.-(allUses, current_usedef)
            val current_def
              = MemLocSet.-(allDefs, current_usedef)

            val (_,commit,remove,dead)
              = split(current, fn memloc => predict(future, memloc))

            val future
              = let
                  fun doit(memlocs, tag, future)
                    = MemLocSet.fold
                      (memlocs,
                       future,
                       fn (memloc,future)
                        => (M (tag, memloc))::future)
                in
                  doit(current_use, FUSE,
                  doit(current_usedef, FUSEDEF,
                  doit(current_def, FDEF,
                       future)))
                end

            val info
              = {dead = dead,
                 commit = commit,
                 remove = remove,
                 future = future}
          in
            info
          end

      fun livenessInstruction {instruction: Instruction.t,
                               future: future list}
        = let
            val future_post = future

            val {uses, defs, ...} = Instruction.uses_defs_kills instruction
            local 
              fun doit operands
                = List.fold
                  (operands,
                   MemLocSet.empty,
                   fn (operand, memlocs)
                    => case Operand.deMemloc operand
                         of SOME memloc => MemLocSet.add(memlocs, memloc)
                          | NONE => memlocs)
            in
              val uses = doit uses
              val defs = doit defs
            end 

            val {dead,commit,remove,future}
              = liveness {uses = uses,
                          defs = defs,
                          future = future_post}
            val future_pre = future

            val info = {dead = dead,
                        commit = commit,
                        remove = remove,
                        futures = {pre = future_pre, post = future_post}}

          in
            info
          end

      fun livenessDirective {directive: Directive.t,
                             future: future list}
        = let
            val future_post = future

            fun addLive (memlocsX, f)
              = List.fold
                (memlocsX,
                 future,
                 fn (X, future) => (M (FLIVE, f X))::future)
            fun addLive' (memlocs)
              = MemLocSet.fold
                (memlocs,
                 future,
                 fn (memloc, future) => (M (FLIVE, memloc))::future)

            val future_pre
              = case directive
                  of Directive.Reset 
                   => []
                   | Directive.Cache {caches, ...}
                   => addLive(caches, fn {memloc, ...} => memloc)
                   | Directive.XmmCache {caches, ...}
                   => addLive(caches, fn {memloc, ...} => memloc)
                   | Directive.Force {commit_memlocs,
                                      commit_classes,
                                      remove_memlocs,
                                      remove_classes,
                                      dead_memlocs,
                                      dead_classes,
                                      ...}
                   => MemLocSet.fold
                      (commit_memlocs,
                       MemLocSet.fold
                       (remove_memlocs,
                        MemLocSet.fold
                        (dead_memlocs,
                         (MP (FCOMMITP, 
                              fn memloc 
                               => ClassSet.contains(commit_classes,
                                                    MemLoc.class memloc)))::
                         (MP (FREMOVEP, 
                              fn memloc 
                               => ClassSet.contains(remove_classes,
                                                    MemLoc.class memloc)))::
                         (MP (FDEADP, 
                              fn memloc 
                               => ClassSet.contains(dead_classes,
                                                    MemLoc.class memloc)))::
                         future,
                         fn (memloc,future) => (M (FDEAD, memloc))::future),
                        fn (memloc,future) => (M (FREMOVE, memloc))::future),
                       fn (memloc,future) => (M (FCOMMIT, memloc))::future)
                   | Directive.CCall
                   => (MP (FCOMMITP,
                           fn memloc
                            => MemLoc.Class.eq
                               (MemLoc.class memloc, 
                                MemLoc.Class.CStack)))::
                      (MP (FMREMOVEP,
                           fn memloc
                            => (not (MemLoc.Class.eq
                                     (MemLoc.class memloc,
                                      MemLoc.Class.CStack)))
                               andalso
                               (Size.class (MemLoc.size memloc) <> Size.INT)))::
                      future
                   | Directive.Return {returns}
                   => (List.map(returns, fn {dst, ...} => M (FDEF, dst))) @ future
                   | Directive.SaveRegAlloc {live, ...}
                   => addLive'(live)
                   | _ => future

            val info = {dead = MemLocSet.empty,
                        commit = MemLocSet.empty,
                        remove = MemLocSet.empty,
                        futures = {pre = future_pre, post = future_post}}
          in
            info
          end

      fun livenessAssembly {assembly: Assembly.t,
                            future: future list,
                            hint: hint list,
                            xmmhint: xmmhint list} : t
        = let
            fun default () = {dead = MemLocSet.empty,
                              commit = MemLocSet.empty,
                              remove = MemLocSet.empty,
                              futures = {pre = future, post = future}}
            val {dead, commit, remove, futures}
              = case assembly
                  of Assembly.Comment _ => default ()
                   | Assembly.Directive d 
                   => livenessDirective {directive = d,
                                         future = future}
                   | Assembly.Instruction i 
                   => livenessInstruction {instruction = i,
                                           future = future}
                   | Assembly.Label _ => default ()
                   | Assembly.PseudoOp _ => default ()

            val hint' = Assembly.hints assembly
            val hint
              = List.fold
                (case assembly
                   of Assembly.Directive Directive.Reset => []
                    | _ => hint,
                 List.revMap
                 (hint',
                  fn (memloc, register)
                   => (register, [memloc], MemLocSet.empty)),
                 fn ((hint_register,hint_memlocs,hint_ignore),hint)
                  => if List.exists
                        (hint,
                         fn (hint_register',_,_) => Register.coincide(hint_register,
                                                                      hint_register'))
                       then hint
                       else let
                              val hint_memloc = hd hint_memlocs
                            in
                              if List.fold
                                 (hint,
                                  false,
                                  fn ((_,hint_memlocs',_),b)
                                   => b orelse List.contains
                                               (hint_memlocs',
                                                hint_memloc,
                                                MemLoc.eq))
                                then hint
                                else (hint_register,
                                      [hint_memloc],
                                      MemLocSet.union(dead, hint_ignore))::hint
                            end)
            val hint
              = case assembly
                  of (Assembly.Instruction (Instruction.MOV 
                                            {src = Operand.MemLoc src', 
                                             dst = Operand.MemLoc dst',
                                             ...}))
                   => List.revMap
                      (hint,
                       fn (hint_register,hint_memlocs,hint_ignore)
                        => if List.contains(hint_memlocs, dst', MemLoc.eq)
                             then (hint_register,
                                   src'::hint_memlocs,
                                   hint_ignore)
                             else (hint_register,hint_memlocs,hint_ignore))
                   | _ => hint
            val xmmhint
              = case assembly
                  of (Assembly.Instruction (Instruction.SSE_MOVS
                                            {src = Operand.MemLoc src', 
                                             dst = Operand.MemLoc dst',
                                             ...}))
                   => List.revMap
                      (xmmhint,
                       fn (hint_register,hint_memlocs,hint_ignore)
                        => if List.contains(hint_memlocs, dst', MemLoc.eq)
                             then (hint_register,
                                   src'::hint_memlocs,
                                   hint_ignore)
                             else (hint_register,hint_memlocs,hint_ignore))
                   | _ => xmmhint

            val info = {dead = dead,
                        commit = commit,
                        remove = remove,
                        futures = futures,
                        hint = hint,
                        xmmhint = xmmhint}
          in
            info
          end

      fun toLiveness (assembly: Assembly.t list) : ((Assembly.t * t) list)
        = let
            val {assembly,...}
              = List.foldr
                (assembly,
                 {assembly = [], future = [], hint = [], xmmhint = []},
                 fn (asm, {assembly,future,hint,xmmhint})
                  => let
                       val info as {futures = {pre, ...}, hint, xmmhint, ...}
                         = livenessAssembly {assembly = asm,
                                             future = future,
                                             hint = hint,
                                             xmmhint = xmmhint}
                     in
                       {assembly = (asm,info)::assembly,
                        future = pre,
                        hint = hint,
                        xmmhint = xmmhint}
                     end)
          in
            assembly
          end

      val (toLiveness,toLiveness_msg)
        = tracer
          "toLiveness"
          toLiveness

      fun toNoLiveness (assembly: Assembly.t list) : ((Assembly.t * t) list)
        = List.map(assembly, fn asm => (asm,{dead = MemLocSet.empty,
                                             commit = MemLocSet.empty,
                                             remove = MemLocSet.empty,
                                             futures = {pre = [], post = []},
                                             hint = [],
                                             xmmhint = []}))

      val (toNoLiveness,toNoLiveness_msg)
        = tracer
          "toNoLiveness"
          toNoLiveness
   end

  structure RegisterAllocation =
    struct
      exception Spill
      val spill : Int.t ref = ref 0
      val spillLabel = Label.fromString "spill"
      val depth : Int.t ref = ref 0

      datatype commit 
        = NO 
        | COMMIT of int 
        | REMOVE of int
        | TRYCOMMIT of int 
        | TRYREMOVE of int

      val commit_toString
        = fn NO => "NO"
           | COMMIT i => "COMMIT " ^ (Int.toString i)
           | REMOVE i => "REMOVE " ^ (Int.toString i)
           | TRYCOMMIT i => "TRYCOMMIT " ^ (Int.toString i)
           | TRYREMOVE i => "TRYREMOVE " ^ (Int.toString i)

      type value = {register: Register.t,
                    memloc: MemLoc.t,
                    weight: int,
                    sync: bool,
                    commit: commit}

      fun value_toString {register, memloc, weight, sync, commit}
        = concat [Register.toString register, " ",
                  MemLoc.toString memloc, " ",
                  Int.toString weight, " ",
                  Bool.toString sync, " ",
                  commit_toString commit]

      type xmmvalue = {register: XmmRegister.t,
                        memloc: MemLoc.t,
                        weight: int,
                        sync: bool,
                        commit: commit}

      fun xmmvalue_toString {register, memloc, weight, sync, commit}
        = concat [XmmRegister.toString register, " ",
                  MemLoc.toString memloc, " ",
                  Int.toString weight, " ",
                  Bool.toString sync, " ",
                  commit_toString commit]

      type t = {entries: value list,
                reserved: Register.t list,
                xmmentries: xmmvalue list,
                xmmreserved: XmmRegister.t list}

      fun toString ({entries, reserved, xmmentries, xmmreserved}: t) 
        = let
            fun doit (name, l, toString, ac)
              = (name ^ "\n") ^
                (List.fold(l, ac,
                           fn (x, ac)
                            => (toString x) ^ "\n" ^ ac))
          in
            doit("entries:", entries, value_toString,
            doit("reserved:", reserved, Register.toString,
            doit("xmmentries:", xmmentries, xmmvalue_toString, 
            doit("xmmreserved:", xmmreserved, XmmRegister.toString, ""))))
          end

      fun toComments ({entries, reserved, xmmentries, xmmreserved}: t)
        = let
            fun doit (name, l, toString, ac)
              = (Assembly.comment name)::
                (List.fold(l, ac,
                           fn (x, ac)
                            => (Assembly.comment (toString x))::
                               ac))
          in
            AppendList.fromList
            (doit("entries:", entries, value_toString,
             doit("reserved:", reserved, Register.toString,
             doit("xmmentries:", xmmentries, xmmvalue_toString,
             doit("xmmreserved:", xmmreserved, XmmRegister.toString,
                  [])))))
          end

      val {get = getRA : Directive.Id.t -> {registerAllocation: t},
           set = setRA, ...}
        = Property.getSetOnce
          (Directive.Id.plist,
           Property.initRaise ("getRA", fn _ => Layout.empty))

      fun empty () : t
        = {entries = [],
           reserved = [],
           xmmentries = [],
           xmmreserved = []}

      fun reserve' {register: Register.t,
                    registerAllocation = {entries, reserved, 
                                          xmmentries, xmmreserved}: t}
        = {assembly = AppendList.empty,
           registerAllocation = {entries = entries,
                                 reserved = register::reserved,
                                 xmmentries = xmmentries,
                                 xmmreserved = xmmreserved}}

      fun xmmreserve' {register: XmmRegister.t,
                        registerAllocation = {entries, reserved, 
                                              xmmentries, xmmreserved}: t}
        = {assembly = AppendList.empty,
           registerAllocation = {entries = entries,
                                 reserved = reserved,
                                 xmmentries = xmmentries,
                                 xmmreserved = register::xmmreserved}}

      fun reserve {registers: Register.t list,
                   registerAllocation = {entries, reserved, 
                                         xmmentries, xmmreserved}: t}
        = {assembly = AppendList.empty,
           registerAllocation = {entries = entries,
                                 reserved = registers @ reserved,
                                 xmmentries = xmmentries,
                                 xmmreserved = xmmreserved}}

      fun xmmreserve {registers: XmmRegister.t list,
                       registerAllocation = {entries, reserved, 
                                             xmmentries, xmmreserved}: t}
        = {assembly = AppendList.empty,
           registerAllocation = {entries = entries,
                                 reserved = reserved,
                                 xmmentries = xmmentries,
                                 xmmreserved = registers @ xmmreserved}}

      fun unreserve' {register: Register.t,
                      registerAllocation = {entries, reserved, 
                                            xmmentries, xmmreserved}: t}
        = {assembly = AppendList.empty,
           registerAllocation = {entries = entries,
                                 reserved = List.revRemoveAll
                                            (reserved,
                                             fn register' 
                                              => Register.eq
                                                 (register',
                                                  register)),
                                 xmmentries = xmmentries,
                                 xmmreserved = xmmreserved}}

      fun xmmunreserve' {register: XmmRegister.t,
                          registerAllocation = {entries, reserved, 
                                                xmmentries, xmmreserved}: t}
        = {assembly = AppendList.empty,
           registerAllocation = {entries = entries,
                                 reserved = reserved,
                                 xmmentries = xmmentries,
                                 xmmreserved = List.revRemoveAll
                                                (xmmreserved,
                                                 fn register' 
                                                  => XmmRegister.eq
                                                     (register',
                                                      register))}}

      fun unreserve {registers: Register.t list,
                     registerAllocation = {entries, reserved,
                                           xmmentries, xmmreserved}: t}
        = {assembly = AppendList.empty,
           registerAllocation = {entries = entries,
                                 reserved = List.revRemoveAll
                                            (reserved,
                                             fn register' 
                                             => List.contains
                                                (registers,
                                                 register',
                                                 Register.eq)),
                                 xmmentries = xmmentries,
                                 xmmreserved = xmmreserved}}

      fun xmmunreserve {registers: XmmRegister.t list,
                         registerAllocation = {entries, reserved,
                                               xmmentries, xmmreserved}: t}
        = {assembly = AppendList.empty,
           registerAllocation = {entries = entries,
                                 reserved = reserved,
                                 xmmentries = xmmentries,
                                 xmmreserved = List.revRemoveAll
                                                (xmmreserved,
                                                 fn register' 
                                                  => List.contains
                                                     (registers,
                                                      register',
                                                      XmmRegister.eq))}}

      fun valueMap {map, 
                    registerAllocation = {entries,
                                          reserved, 
                                          xmmentries,
                                          xmmreserved}: t}
        = {entries = List.revMap(entries, map),
           reserved = reserved,
           xmmentries = xmmentries,
           xmmreserved = xmmreserved}

      fun xmmvalueMap {map, 
                        registerAllocation = {entries,
                                              reserved, 
                                              xmmentries,
                                              xmmreserved}: t}
        = {entries = entries, 
           reserved = reserved,
           xmmentries = List.revMap(xmmentries, map),
           xmmreserved = xmmreserved}

      fun valueFilter {filter,
                       registerAllocation = {entries, 
                                             ...}: t}
        = List.revKeepAll(entries, filter)

      fun xmmvalueFilter {filter,
                           registerAllocation = {xmmentries, 
                                                 ...}: t}
        = List.revKeepAll(xmmentries, filter)

      fun valueRegister {register,
                         registerAllocation}
        = case valueFilter {filter = fn {register = register', ...}
                                      => Register.eq(register, register'),
                            registerAllocation = registerAllocation}
            of [] => NONE
             | [value] => SOME value
             | _ => Error.bug "amd64AllocateRegisters.RegisterAllocation.valueRegister"

(*
      fun xmmvalueRegister {register,
                             registerAllocation}
        = case xmmvalueFilter {filter = fn {register = register', ...}
                                          => XmmRegister.eq(register, register'),
                                registerAllocation = registerAllocation}
            of [] => NONE
             | [value] => SOME value
             | _ => Error.bug "amd64AllocateRegisters.RegisterAllocation.xmmvalueRegister"
*)

      fun valuesRegister {register = Register.T {reg, ...}, 
                          registerAllocation = {entries, 
                                                ...}: t}
        = List.revKeepAll(entries, 
                          fn {register 
                              = Register.T {reg = reg',
                                            ...},
                              ...}
                           => reg = reg')

      fun xmmvaluesXmmRegister {register = XmmRegister.T {reg, ...}, 
                                  registerAllocation = {xmmentries, 
                                                        ...}: t}
        = List.revKeepAll(xmmentries, 
                          fn {register 
                              = XmmRegister.T {reg = reg',
                                                ...},
                              ...}
                           => reg = reg')

      fun update {value as {register,...},
                  registerAllocation = {entries, reserved, 
                                        xmmentries, xmmreserved}: t}
        = {entries = let
                       val entries 
                         = List.revRemoveAll(entries,
                                             fn {register = register',...} 
                                              => Register.eq(register,register'))
                     in
                       value::entries
                     end,
           reserved = reserved,
           xmmentries = xmmentries,
           xmmreserved = xmmreserved}

      fun xmmupdate {value as {register,...},
                      registerAllocation = {entries, reserved, 
                                            xmmentries, xmmreserved}: t}
        = {entries = entries,
           reserved = reserved,
           xmmentries = let
                           val xmmentries 
                             = List.revRemoveAll(xmmentries,
                                                 fn {register = register',...} 
                                                  => XmmRegister.eq(register,register'))
                         in
                           value::xmmentries
                         end,
           xmmreserved = xmmreserved}

      fun delete {register,
                  registerAllocation = {entries, reserved, 
                                        xmmentries, xmmreserved}: t}
        = {entries = List.revRemoveAll(entries,
                                       fn {register = register',...}
                                        => Register.eq(register, register')),
           reserved = reserved,
           xmmentries = xmmentries,
           xmmreserved = xmmreserved}

      fun xmmdelete {register,
                      registerAllocation = {entries, reserved, 
                                        xmmentries, xmmreserved}: t}
        = {entries = entries,
           reserved = reserved,
           xmmentries = List.revRemoveAll(xmmentries,
                                           fn {register = register',...}
                                            => XmmRegister.eq(register, register')),
           xmmreserved = xmmreserved}

      fun deletes {registers, registerAllocation: t}
        = List.fold(registers,
                    registerAllocation,
                    fn (register, registerAllocation)
                     => delete {register = register,
                                registerAllocation = registerAllocation})

      fun xmmdeletes {registers, registerAllocation: t}
        = List.fold(registers,
                    registerAllocation,
                    fn (register, registerAllocation)
                     => xmmdelete {register = register,
                                    registerAllocation = registerAllocation})

      fun allocated {memloc, 
                     registerAllocation: t}
        = case valueFilter {filter = fn {memloc = memloc',...}
                                      => MemLoc.eq(memloc,memloc'),
                            registerAllocation = registerAllocation}
            of [] => NONE
             | [value] => SOME value
             | _ => Error.bug "amd64AllocateRegisters.RegisterAllocation.allocated"

      fun xmmallocated {memloc, 
                         registerAllocation: t}
        = case xmmvalueFilter {filter = fn {memloc = memloc',...}
                                          => MemLoc.eq(memloc,memloc'),
                                registerAllocation = registerAllocation}
            of [] => NONE
             | [value] => SOME value
             | _ => Error.bug "amd64AllocateRegisters.RegisterAllocation.xmmallocated"

      fun remove {memloc,
                  registerAllocation: t}
        = case allocated {memloc = memloc,
                          registerAllocation = registerAllocation}
            of SOME {register, ...}
             => delete {register = register,
                        registerAllocation = registerAllocation}
             | NONE => registerAllocation

      fun xmmremove {memloc,
                      registerAllocation: t}
        = case xmmallocated {memloc = memloc,
                              registerAllocation = registerAllocation}
            of SOME {register, ...}
             => xmmdelete {register = register,
                            registerAllocation = registerAllocation}
             | NONE => registerAllocation

      fun removes {memlocs,
                   registerAllocation: t}
        = List.fold(memlocs,
                    registerAllocation,
                    fn (memloc,registerAllocation)
                     => remove {memloc = memloc,
                                registerAllocation = registerAllocation})

      fun xmmremoves {memlocs,
                       registerAllocation: t}
        = List.fold(memlocs,
                    registerAllocation,
                    fn (memloc,registerAllocation)
                     => xmmremove {memloc = memloc,
                                    registerAllocation = registerAllocation})

      local
        val commitPush' 
          = fn NO => NO
             | COMMIT i => COMMIT (i + 1)
             | REMOVE i => REMOVE (i + 1)
             | TRYCOMMIT i => TRYCOMMIT (i + 1)
             | TRYREMOVE i => TRYREMOVE (i + 1)

        val commitPop' 
          = fn NO => NO
             | COMMIT i => COMMIT (i - 1)
             | REMOVE i => REMOVE (i - 1)
             | TRYCOMMIT i => TRYCOMMIT (i - 1)
             | TRYREMOVE i => TRYREMOVE (i - 1)
      in
        fun commitPush {registerAllocation: t}
          = valueMap {map = fn {register,memloc,weight,sync,commit}
                             => {register = register,
                                 memloc = memloc,
                                 weight = weight,
                                 sync = sync,
                                 commit = commitPush' commit},
                      registerAllocation = registerAllocation}

        fun xmmcommitPush {registerAllocation: t}
          = xmmvalueMap {map = fn {register,memloc,weight,sync,commit}
                                 => {register = register,
                                     memloc = memloc,
                                     weight = weight,
                                     sync = sync,
                                     commit = commitPush' commit},
                          registerAllocation = registerAllocation}

        fun commitPop {registerAllocation: t}
          = valueMap {map = fn {register,memloc,weight,sync,commit}
                             => {register = register,
                                 memloc = memloc,
                                 weight = weight,
                                 sync = sync,
                                 commit = commitPop' commit},
                      registerAllocation = registerAllocation}

        fun xmmcommitPop {registerAllocation: t}
          = xmmvalueMap {map = fn {register,memloc,weight,sync,commit}
                                 => {register = register,
                                     memloc = memloc,
                                     weight = weight,
                                     sync = sync,
                                     commit = commitPop' commit},
                          registerAllocation = registerAllocation}
      end

      fun savedRegisters {saves: Operand.t list,
                          registerAllocation: t} :
                         Register.t list
        = List.concatMap
          (saves,
           fn Operand.MemLoc m
            => (case allocated {memloc = m,
                                registerAllocation = registerAllocation}
                  of SOME {register, ...} => [register]
                   | NONE => [])
            | Operand.Register r => [r]
            | Operand.Address (Address.T {base, index, ...})
            => (case (base, index)
                  of (NONE,    NONE   ) => []
                   | (SOME rb, NONE   ) => [rb] 
                   | (NONE,    SOME ro) => [ro]
                   | (SOME rb, SOME ro) => [rb,ro])
            | _ => [])

      fun savedXmmRegisters {saves: Operand.t list,
                              registerAllocation: t} :
                             XmmRegister.t list
        = List.concatMap
          (saves,
           fn Operand.MemLoc m
            => (case xmmallocated {memloc = m,
                                    registerAllocation = registerAllocation}
                  of SOME {register, ...} => [register]
                   | NONE => [])
            | Operand.XmmRegister r => [r]
            | _ => [])

      fun supportedRegisters {supports: Operand.t list,
                              registerAllocation: t} :
                             Register.t list
        = let
            fun supportedRegisters' memloc
              = case (allocated {memloc = memloc,
                                 registerAllocation = registerAllocation},
                      xmmallocated {memloc = memloc,
                                     registerAllocation = registerAllocation})
                  of (SOME {register, ...}, _) => [register]
                   | (_, SOME _) => []
                   | (NONE, NONE) => List.concatMap(MemLoc.utilized memloc,
                                                    supportedRegisters')
          in
            List.concatMap
            (supports,
             fn Operand.MemLoc m => supportedRegisters' m
              | _ => [])
          end

      fun supportedXmmRegisters {supports: Operand.t list,
                                  registerAllocation: t} :
                                 XmmRegister.t list
        = let
            fun supportedXmmRegisters' memloc
              = case (allocated {memloc = memloc,
                                 registerAllocation = registerAllocation},
                      xmmallocated {memloc = memloc,
                                     registerAllocation = registerAllocation})
                  of (SOME _, _) => []
                   | (_, SOME {register, ...}) => [register]
                   | (NONE, NONE) => List.concatMap(MemLoc.utilized memloc,
                                                    supportedXmmRegisters')
          in
            List.concatMap
            (supports,
             fn Operand.MemLoc m => supportedXmmRegisters' m
              | _ => [])
          end

      fun supportedMemLocs {supports: Operand.t list,
                            registerAllocation: t} :
                           MemLoc.t list
        = let
            fun supportedMemLocs' memloc
              = case (allocated {memloc = memloc,
                                 registerAllocation = registerAllocation},
                      xmmallocated {memloc = memloc,
                                     registerAllocation = registerAllocation})
                  of (SOME _, _) => [memloc]
                   | (_, SOME _) => [memloc]
                   | (NONE, NONE) => List.concatMap(MemLoc.utilized memloc,
                                                    supportedMemLocs')
          in
            List.concatMap
            (supports,
             fn Operand.MemLoc m => supportedMemLocs' m
              | _ => [])
          end


      fun 'a spillAndReissue {info: Liveness.t,
                              supports: Operand.t list,
                              saves: Operand.t list,
                              registerAllocation: t,
                              spiller : {info: Liveness.t,
                                         supports: Operand.t list,
                                         saves: Operand.t list,
                                         registerAllocation: t} ->
                                        {assembly: Assembly.t AppendList.t,
                                         registerAllocation: t},
                              msg : string,
                              reissue : {assembly: Assembly.t AppendList.t,
                                         registerAllocation: t} -> 'a} : 'a
        = (Int.dec depth;
           if !depth = 0
             then let
                    val _ = Int.inc depth
                    val {assembly, registerAllocation}
                      = spiller
                        {info = info,
                         supports = supports,
                         saves = saves,
                         registerAllocation = registerAllocation}
                    val return
                      = reissue {assembly = assembly,
                                 registerAllocation = registerAllocation}
                        handle Spill
                         => (Error.bug (concat [msg, ":reSpill"]))
                    val _ = Int.dec depth
                  in
                    return
                  end
             else raise Spill)

      fun potentialRegisters ({size, force, ...}:
                              {size: Size.t,
                               saves: Operand.t list,
                               force: Register.t list,
                               registerAllocation: t}):
                             Register.t list
        = case force
            of [] => Register.registers size
             | registers => List.revKeepAll(Register.registers size,
                                            fn register 
                                             => List.contains(registers, 
                                                              register, 
                                                              Register.eq))

      fun potentialXmmRegisters ({size, force, ...}:
                                  {size: Size.t,
                                   saves: Operand.t list,
                                   force: XmmRegister.t list,
                                   registerAllocation: t}):
                                 XmmRegister.t list
        = case force
            of [] => XmmRegister.registers size
             | registers => List.revKeepAll(XmmRegister.registers size,
                                            fn register 
                                             => List.contains(registers, 
                                                              register, 
                                                              XmmRegister.eq))

      fun chooseRegister {info = {futures = {pre = future, ...},
                                  hint,...}: Liveness.t,
                          memloc: MemLoc.t option,
                          size: Size.t,
                          supports: Operand.t list,
                          saves: Operand.t list,
                          force: Register.t list,
                          registerAllocation as {reserved,...}: t} :
                         {register: Register.t,
                          coincide_values: value list}
        = let
            val registers = potentialRegisters {size = size,
                                                saves = saves,
                                                force = force,
                                                registerAllocation
                                                = registerAllocation}

            val saved
              = savedRegisters {saves = saves,
                                registerAllocation = registerAllocation}

            val preserved
              = let
                  fun doit(registers, preserved)
                    = List.fold
                      (registers,
                       preserved,
                       fn (register,preserved) 
                        => if List.contains(preserved,
                                            register,
                                            Register.eq)
                             then preserved
                             else register::preserved)
                in
                  doit(saved,
                  doit(reserved,
                       []))
                end

            val registers 
              = List.revRemoveAll
                (registers,
                 fn register'
                  => List.exists
                     (preserved,
                      fn register''
                       => Register.coincide(register',register'')))

            val supported = supportedRegisters {supports = supports,
                                                registerAllocation
                                                = registerAllocation}

            val values = valueFilter {filter = fn _ => true,
                                      registerAllocation = registerAllocation}
            val memlocs = List.revMap(values, #memloc)

            val registers_costs
              = List.revMap
                (registers,
                 fn register'
                  => let
                       val hint_cost
                         = List.fold
                           (hint,
                            0,
                            fn ((hint_register,hint_memlocs,hint_ignore),
                                hint_cost)
                             => if Register.eq(register',
                                               hint_register)
                                  then case memloc
                                         of SOME memloc
                                          => (case (List.contains
                                                    (hint_memlocs,
                                                     memloc,
                                                     MemLoc.eq),
                                                    MemLocSet.contains
                                                    (hint_ignore,
                                                     memloc))
                                                of (true, _) => hint_cost + 5
                                                 | (false, true) => hint_cost
                                                 | (false, false) => hint_cost - 5)
                                          | NONE => hint_cost - 5
                                else if Register.coincide(register',
                                                          hint_register)
                                  then hint_cost - 5
                                else hint_cost)

                       val values = valuesRegister {register = register',
                                                    registerAllocation 
                                                    = registerAllocation}
                       val (support_cost,
                            commit_cost,
                            future_cost,
                            utilized_cost,
                            sync_cost,
                            weight_cost)
                         = List.fold
                           (values,
                            (false,false,NONE,0,true,0),
                            fn ({register,memloc,weight,sync,commit,...},
                                cost as (support_cost,
                                         commit_cost,
                                         future_cost,
                                         utilized_cost,
                                         sync_cost,
                                         weight_cost))
                             => if Register.coincide(register,register')
                                  then let
                                         val support_cost'
                                           = List.contains(supported,
                                                           register,
                                                           Register.eq)

                                         val commit_cost'
                                           = case commit
                                               of TRYREMOVE _ => false
                                                | REMOVE _ => false
                                                | _ => true

                                         val future_cost'
                                           = List.index
                                             (future,
                                              fn Liveness.M (tag, memloc')
                                               => let
                                                    val eq = MemLoc.eq(memloc, memloc')
                                                  in 
                                                    case tag
                                                      of Liveness.FLIVE => eq
                                                       | Liveness.FUSE => eq
                                                       | Liveness.FUSEDEF => eq
                                                       | _ => false
                                                  end
                                               | _ => false)

                                         val utilized_cost'
                                           = List.fold
                                             (memlocs,
                                              0,
                                              fn (memloc',uc')
                                               => List.fold
                                                  (MemLoc.utilized memloc',
                                                   0,
                                                   fn (memloc'',uc'')
                                                    => if MemLoc.eq
                                                          (memloc,
                                                           memloc'')
                                                         then uc'' + 1
                                                         else uc'') + uc')

                                         val sync_cost' = sync

                                         val weight_cost' = weight
                                       in
                                         (support_cost orelse support_cost',
                                          commit_cost orelse commit_cost',
                                          case (future_cost,future_cost')
                                            of (_, NONE) => future_cost
                                             | (NONE, _) => future_cost'
                                             | (SOME f,SOME f') 
                                             => SOME (Int.min(f,f')),
                                          utilized_cost + utilized_cost',
                                          sync_cost andalso sync_cost',
                                          weight_cost + weight_cost')
                                       end
                                  else cost)
                     in
                       (register',
                        (support_cost,
                         commit_cost,
                         future_cost,
                         hint_cost,
                         utilized_cost,
                         sync_cost,
                         weight_cost))
                     end)

            val registers_costs_sorted
              = List.insertionSort
                (registers_costs,
                 fn ((_,(support_c1,
                         commit_c1,
                         future_c1,
                         hint_c1,
                         utilized_c1,
                         sync_c1,
                         weight_c1)),
                     (_,(support_c2,
                         commit_c2,
                         future_c2,
                         hint_c2,
                         utilized_c2,
                         sync_c2,
                         weight_c2)))
                  => bool_lt(support_c1,support_c2) orelse
                     (support_c1 = support_c2 andalso
                      (bool_lt(commit_c1,commit_c2) orelse
                       (commit_c1 = commit_c2 andalso
                        (option_lt (op >) (future_c1, future_c2) orelse
                         (future_c1 = future_c2 andalso
                          (hint_c1 > hint_c2 orelse
                           (hint_c1 = hint_c2 andalso
                            (utilized_c1 < utilized_c2 orelse
                             (utilized_c1 = utilized_c2 andalso
                              (bool_gt(sync_c1,sync_c2) orelse
                               (sync_c1 = sync_c2 andalso
                                weight_c1 < weight_c2))))))))))))

            val registers
              = List.map(registers_costs_sorted, #1)

            val register
              = case registers
                  of [] 
(*
                   => raise Spill
*)
                   => let
                        fun listToString(ss: string list): string
                          = "[" ^ (concat(List.separate(ss, ", "))) ^ "]"

                        val size = Size.toString size
                        val supports
                          = listToString(List.map(supports,Operand.toString))
                        val saves 
                          = listToString(List.map(saves,Operand.toString)) 
                        val force
                          = listToString(List.map(force,Register.toString)) 
                        val reserved
                          = listToString(List.map(reserved,Register.toString)) 

                        val msg = concat["\n",
                                         "chooseRegister:\n",
                                         (toString registerAllocation),
                                         "size = ", size, "\n",
                                         "supports = ", supports, "\n",
                                         "saves = ", saves, "\n",
                                         "force = ", force, "\n",
                                         "reserved = ", reserved, "\n",
                                         "depth = ", Int.toString (!depth), "\n"]

                        val _ = print msg
                      in
                        print "Raising Spill in chooseRegister\n";
                        raise Spill
                      end
                   | register::_ => register

            val values = valuesRegister {register = register,
                                         registerAllocation 
                                         = registerAllocation}
            val coincide_values
              = List.revKeepAll(values,
                                fn {register = register',...}
                                 => Register.coincide(register',register))
          in
            {register = register,
             coincide_values = coincide_values}
          end

      fun chooseXmmRegister {info = {futures = {pre = future, ...},
                                      xmmhint,...}: Liveness.t,
                              memloc: MemLoc.t option,
                              size: Size.t,
                              supports: Operand.t list,
                              saves: Operand.t list,
                              force: XmmRegister.t list,
                              registerAllocation as {xmmreserved,...}: t} :
                             {register: XmmRegister.t,
                              coincide_values: xmmvalue list}
        = let
            val registers = potentialXmmRegisters {size = size,
                                                    saves = saves,
                                                    force = force,
                                                    registerAllocation
                                                    = registerAllocation}

            val saved
              = savedXmmRegisters {saves = saves,
                                    registerAllocation = registerAllocation}

            val preserved
              = let
                  fun doit(registers, preserved)
                    = List.fold
                      (registers,
                       preserved,
                       fn (register,preserved) 
                        => if List.contains(preserved,
                                            register,
                                            XmmRegister.eq)
                             then preserved
                             else register::preserved)
                in
                  doit(saved,
                  doit(xmmreserved,
                       []))
                end

            val registers 
              = List.revRemoveAll
                (registers,
                 fn register'
                  => List.exists
                     (preserved,
                      fn register''
                       => XmmRegister.coincide(register',register'')))

            val supported = supportedXmmRegisters {supports = supports,
                                                    registerAllocation
                                                    = registerAllocation}

            val values = xmmvalueFilter {filter = fn _ => true,
                                          registerAllocation = registerAllocation}
            val memlocs = List.revMap(values, #memloc)

            val registers_costs
              = List.revMap
                (registers,
                 fn register'
                  => let
                       val hint_cost
                         = List.fold
                           (xmmhint,
                            0,
                            fn ((hint_register,hint_memlocs,hint_ignore),
                                hint_cost)
                             => if XmmRegister.eq(register',
                                                   hint_register)
                                  then case memloc
                                         of SOME memloc
                                          => (case (List.contains
                                                    (hint_memlocs,
                                                     memloc,
                                                     MemLoc.eq),
                                                    MemLocSet.contains
                                                    (hint_ignore,
                                                     memloc))
                                                of (true, _) => hint_cost + 5
                                                 | (false, true) => hint_cost
                                                 | (false, false) => hint_cost - 5)
                                          | NONE => hint_cost - 5
                                else if XmmRegister.coincide(register',
                                                              hint_register)
                                  then hint_cost - 5
                                else hint_cost)

                       val values = xmmvaluesXmmRegister {register = register',
                                                            registerAllocation 
                                                            = registerAllocation}
                       val (support_cost,
                            commit_cost,
                            future_cost,
                            utilized_cost,
                            sync_cost,
                            weight_cost)
                         = List.fold
                           (values,
                            (false,false,NONE,0,true,0),
                            fn ({register,memloc,weight,sync,commit,...},
                                cost as (support_cost,
                                         commit_cost,
                                         future_cost,
                                         utilized_cost,
                                         sync_cost,
                                         weight_cost))
                             => if XmmRegister.coincide(register,register')
                                  then let
                                         val support_cost'
                                           = List.contains(supported,
                                                           register,
                                                           XmmRegister.eq)

                                         val commit_cost'
                                           = case commit
                                               of TRYREMOVE _ => false
                                                | REMOVE _ => false
                                                | _ => true

                                         val future_cost'
                                           = List.index
                                             (future,
                                              fn Liveness.M (tag, memloc')
                                               => let
                                                    val eq = MemLoc.eq(memloc, memloc')
                                                  in 
                                                    case tag
                                                      of Liveness.FLIVE => eq
                                                       | Liveness.FUSE => eq
                                                       | Liveness.FUSEDEF => eq
                                                       | _ => false
                                                  end
                                               | _ => false)

                                         val utilized_cost'
                                           = List.fold
                                             (memlocs,
                                              0,
                                              fn (memloc',uc')
                                               => List.fold
                                                  (MemLoc.utilized memloc',
                                                   0,
                                                   fn (memloc'',uc'')
                                                    => if MemLoc.eq
                                                          (memloc,
                                                           memloc'')
                                                         then uc'' + 1
                                                         else uc'') + uc')

                                         val sync_cost' = sync

                                         val weight_cost' = weight
                                       in
                                         (support_cost orelse support_cost',
                                          commit_cost orelse commit_cost',
                                          case (future_cost,future_cost')
                                            of (_, NONE) => future_cost
                                             | (NONE, _) => future_cost'
                                             | (SOME f,SOME f') 
                                             => SOME (Int.min(f,f')),
                                          utilized_cost + utilized_cost',
                                          sync_cost andalso sync_cost',
                                          weight_cost + weight_cost')
                                       end
                                  else cost)
                     in
                       (register',
                        (support_cost,
                         commit_cost,
                         future_cost,
                         hint_cost,
                         utilized_cost,
                         sync_cost,
                         weight_cost))
                     end)

            val registers_costs_sorted
              = List.insertionSort
                (registers_costs,
                 fn ((_,(support_c1,
                         commit_c1,
                         future_c1,
                         hint_c1,
                         utilized_c1,
                         sync_c1,
                         weight_c1)),
                     (_,(support_c2,
                         commit_c2,
                         future_c2,
                         hint_c2,
                         utilized_c2,
                         sync_c2,
                         weight_c2)))
                  => bool_lt(support_c1,support_c2) orelse
                     (support_c1 = support_c2 andalso
                      (bool_lt(commit_c1,commit_c2) orelse
                       (commit_c1 = commit_c2 andalso
                        (option_lt (op >) (future_c1, future_c2) orelse
                         (future_c1 = future_c2 andalso
                          (hint_c1 > hint_c2 orelse
                           (hint_c1 = hint_c2 andalso
                            (utilized_c1 < utilized_c2 orelse
                             (utilized_c1 = utilized_c2 andalso
                              (bool_gt(sync_c1,sync_c2) orelse
                               (sync_c1 = sync_c2 andalso
                                weight_c1 < weight_c2))))))))))))

            val registers
              = List.map(registers_costs_sorted, #1)

            val register
              = case registers
                  of [] 
(*
                   => raise Spill
*)
                   => let
                        fun listToString(ss: string list): string
                          = "[" ^ (concat(List.separate(ss, ", "))) ^ "]"

                        val size = Size.toString size
                        val supports
                          = listToString(List.map(supports,Operand.toString))
                        val saves 
                          = listToString(List.map(saves,Operand.toString)) 
                        val force
                          = listToString(List.map(force,XmmRegister.toString)) 
                        val xmmreserved
                          = listToString(List.map(xmmreserved,XmmRegister.toString)) 

                        val msg = concat["\n",
                                         "chooseXmmRegister:\n",
                                         (toString registerAllocation),
                                         "size = ", size, "\n",
                                         "supports = ", supports, "\n",
                                         "saves = ", saves, "\n",
                                         "force = ", force, "\n",
                                         "xmmreserved = ", xmmreserved, "\n",
                                         "depth = ", Int.toString (!depth), "\n"]

                        val _ = print msg
                      in
                        print "Raising Spill in chooseXmmRegister\n";
                        raise Spill
                      end
                   | register::_ => register

            val values = xmmvaluesXmmRegister {register = register,
                                                 registerAllocation 
                                                 = registerAllocation}
            val coincide_values
              = List.revKeepAll(values,
                                fn {register = register',...}
                                 => XmmRegister.coincide(register',register))
          in
            {register = register,
             coincide_values = coincide_values}
          end

      fun freeRegister ({info: Liveness.t,
                         memloc: MemLoc.t option,
                         size: Size.t,
                         supports: Operand.t list,
                         saves: Operand.t list,
                         force: Register.t list, 
                         registerAllocation: t}) :
                       {register: Register.t,
                        assembly: Assembly.t AppendList.t,
                        registerAllocation: t}
        = let
            val _ = Int.inc depth

            val {register = final_register,
                 coincide_values}
              = chooseRegister {info = info,
                                memloc = memloc,
                                size = size,
                                supports = supports,
                                saves = saves,
                                force = force,
                                registerAllocation = registerAllocation}

            val supported = supportedMemLocs {supports = supports,
                                              registerAllocation
                                              = registerAllocation}

            fun supportRemove memloc
              = let
                  fun supportRemove' memlocs
                    = List.concatMap
                      (memlocs,
                       fn memloc' 
                        => if MemLoc.eq(memloc,memloc')
                             then []
                             else supportRemove' (MemLoc.utilized memloc'))
                in
                  List.fold
                  (supports,
                   [],
                   fn (Operand.MemLoc memloc', supports)
                    => List.concat [(supportRemove' [memloc']), supports]
                    | (_, supports) => supports)
                end

            val {assembly = assembly_support,
                 registerAllocation}
              = List.fold
                (coincide_values,
                 {assembly = AppendList.empty,
                  registerAllocation = registerAllocation},
                 fn ({memloc,...},
                     {assembly,
                      registerAllocation})
                  => if List.contains(supported,
                                      memloc,
                                      MemLoc.eq)
                       then let
                              val supports = supportRemove memloc

                              val force
                                = List.revRemoveAll
                                  (Register.registers (MemLoc.size memloc),
                                   fn register'
                                    => Register.coincide(final_register,
                                                         register'))

                              val {assembly = assembly_register,
                                   registerAllocation,
                                   ...}
                                = toRegisterMemLoc
                                  {memloc = memloc,
                                   info = info,
                                   size = MemLoc.size memloc,
                                   move = true,
                                   supports = supports,
                                   saves = (Operand.register 
                                            final_register)::saves,
                                   force = force,
                                   registerAllocation = registerAllocation}
                            in
                              {assembly = AppendList.append (assembly, 
                                                             assembly_register),
                               registerAllocation = registerAllocation}
                            end
                       else {assembly = assembly,
                             registerAllocation = registerAllocation})

            val registerAllocation
              = valueMap
                {map = fn value as {register,
                                    memloc,
                                    weight,
                                    sync,
                                    ...}
                        => if Register.coincide(register,
                                                final_register)
                             then {register = register,
                                   memloc = memloc,
                                   weight = weight,
                                   sync = sync,                            
                                   commit = REMOVE 0}
                             else value,
                 registerAllocation = registerAllocation}

            val {assembly = assembly_commit,
                 registerAllocation}
              = commitRegisters {info = info,
                                 supports = supports,
                                 saves = saves,
                                 registerAllocation = registerAllocation}

            val _ = Int.dec depth
          in
            {register = final_register,
             assembly = AppendList.appends [assembly_support, 
                                            assembly_commit],
             registerAllocation = registerAllocation}
          end
          handle Spill 
           => spillAndReissue 
              {info = info,
               supports = supports,
               saves = saves,
               registerAllocation = registerAllocation,
               spiller = spillRegisters,
               msg = "freeRegister",
               reissue = fn {assembly = assembly_spill,
                             registerAllocation}
                          => let
                               val {register, assembly, registerAllocation}
                                 = freeRegister 
                                   {info = info,
                                    memloc = memloc,
                                    size = size,
                                    supports = supports,
                                    saves = saves,
                                    force = force,
                                    registerAllocation = registerAllocation}
                             in
                               {register = register,
                                assembly = AppendList.append (assembly_spill,
                                                              assembly),
                                registerAllocation = registerAllocation}
                             end}

      and freeXmmRegister ({info: Liveness.t,
                             memloc: MemLoc.t option,
                             size: Size.t,
                             supports: Operand.t list,
                             saves: Operand.t list,
                             force: XmmRegister.t list, 
                             registerAllocation: t}) :
                           {register: XmmRegister.t,
                            assembly: Assembly.t AppendList.t,
                            registerAllocation: t}
        = let
            val _ = Int.inc depth

            val {register = final_register,
                 coincide_values}
              = chooseXmmRegister {info = info,
                                    memloc = memloc,
                                    size = size,
                                    supports = supports,
                                    saves = saves,
                                    force = force,
                                    registerAllocation = registerAllocation}

            val supported = supportedMemLocs {supports = supports,
                                              registerAllocation
                                              = registerAllocation}

            fun supportRemove memloc
              = let
                  fun supportRemove' memlocs
                    = List.concatMap
                      (memlocs,
                       fn memloc' 
                        => if MemLoc.eq(memloc,memloc')
                             then []
                             else supportRemove' (MemLoc.utilized memloc'))
                in
                  List.fold
                  (supports,
                   [],
                   fn (Operand.MemLoc memloc', supports)
                    => List.concat [(supportRemove' [memloc']), supports]
                    | (_, supports) => supports)
                end

            val {assembly = assembly_support,
                 registerAllocation}
              = List.fold
                (coincide_values,
                 {assembly = AppendList.empty,
                  registerAllocation = registerAllocation},
                 fn ({memloc,...},
                     {assembly,
                      registerAllocation})
                  => if List.contains(supported,
                                      memloc,
                                      MemLoc.eq)
                       then let
                              val supports = supportRemove memloc

                              val force
                                = List.revRemoveAll
                                  (XmmRegister.registers (MemLoc.size memloc),
                                   fn register'
                                    => XmmRegister.coincide(final_register,
                                                             register'))

                              val {assembly = assembly_register,
                                   registerAllocation,
                                   ...}
                                = toXmmRegisterMemLoc
                                  {memloc = memloc,
                                   info = info,
                                   size = MemLoc.size memloc,
                                   move = true,
                                   supports = supports,
                                   saves = (Operand.xmmregister 
                                            final_register)::saves,
                                   force = force,
                                   registerAllocation = registerAllocation}
                            in
                              {assembly = AppendList.append (assembly, 
                                                             assembly_register),
                               registerAllocation = registerAllocation}
                            end
                       else {assembly = assembly,
                             registerAllocation = registerAllocation})

            val registerAllocation
              = xmmvalueMap
                {map = fn value as {register,
                                    memloc,
                                    weight,
                                    sync,
                                    ...}
                        => if XmmRegister.coincide(register,
                                                    final_register)
                             then {register = register,
                                   memloc = memloc,
                                   weight = weight,
                                   sync = sync,                            
                                   commit = REMOVE 0}
                             else value,
                 registerAllocation = registerAllocation}

            val {assembly = assembly_commit,
                 registerAllocation}
              = commitXmmRegisters {info = info,
                                     supports = supports,
                                     saves = saves,
                                     registerAllocation = registerAllocation}

            val _ = Int.dec depth
          in
            {register = final_register,
             assembly = AppendList.appends [assembly_support, 
                                            assembly_commit],
             registerAllocation = registerAllocation}
          end
          handle Spill 
           => spillAndReissue 
              {info = info,
               supports = supports,
               saves = saves,
               registerAllocation = registerAllocation,
               spiller = spillRegisters,
               msg = "freeXmmRegister",
               reissue = fn {assembly = assembly_spill,
                             registerAllocation}
                          => let
                               val {register, assembly, registerAllocation}
                                 = freeXmmRegister 
                                   {info = info,
                                    memloc = memloc,
                                    size = size,
                                    supports = supports,
                                    saves = saves,
                                    force = force,
                                    registerAllocation = registerAllocation}
                             in
                               {register = register,
                                assembly = AppendList.append (assembly_spill,
                                                              assembly),
                                registerAllocation = registerAllocation}
                             end}

      and commitRegisters {info: Liveness.t,
                           supports: Operand.t list,
                           saves: Operand.t list,
                           registerAllocation as {reserved,...}: t} :
                          {assembly: Assembly.t AppendList.t,
                           registerAllocation: t}
        = let
            val _ = Int.inc depth
            val commit_values
              = valueFilter {filter = fn {commit = COMMIT 0, ...} => true
                                       | {commit = REMOVE 0, ...} => true
                                       | {commit = TRYCOMMIT 0, ...} => true
                                       | {commit = TRYREMOVE 0, ...} => true
                                       | _ => false, 
                             registerAllocation = registerAllocation}

            val commit_memlocs = List.revMap(commit_values, #memloc)

            val commit_memlocs
              = totalOrder
                (commit_memlocs,
                 fn (memloc1,memloc2)
                  => List.contains(MemLoc.utilized memloc1,
                                   memloc2,
                                   MemLoc.eq))

            val {assembly = assembly_commit,
                 registerAllocation}
              = List.fold
                (commit_memlocs,
                 {assembly = AppendList.empty,
                  registerAllocation = registerAllocation},
                 fn (memloc,
                     {assembly,
                      registerAllocation})
                  => (case allocated {memloc = memloc,
                                      registerAllocation 
                                      = registerAllocation}
                        of NONE => {assembly = assembly,
                                    registerAllocation = registerAllocation}
                         | SOME ({register,
                                  memloc,
                                  weight,
                                  sync,
                                  commit})
                         => let
                              fun doCommitFalse ()
                                = let
                                    val registerAllocation
                                      = update {value = {register = register,
                                                         memloc = memloc,
                                                         weight = weight,
                                                         sync = true,
                                                         commit = NO},
                                                registerAllocation 
                                                = registerAllocation}

                                    val registerAllocation
                                      = commitPush {registerAllocation 
                                                    = registerAllocation}

                                    val commit_saves 
                                      = List.removeDuplicates
                                        ((Operand.register register)::saves,
                                         Operand.eq)

                                    val size = Register.size register
                                    val {address,
                                         assembly = assembly_address,
                                         registerAllocation}
                                      = toAddressMemLoc {memloc = memloc,
                                                         info = info,
                                                         size = size,
                                                         supports = supports,
                                                         saves = commit_saves,
                                                         registerAllocation 
                                                         = registerAllocation}

                                    val registerAllocation
                                      = commitPop {registerAllocation 
                                                   = registerAllocation}
                                  in
                                    {assembly 
                                     = AppendList.appends 
                                       [assembly,
                                        assembly_address,
                                        AppendList.single
                                        (Assembly.instruction_mov
                                         {dst = Operand.Address address,
                                          src = Operand.Register register,
                                          size = size})],
                                     registerAllocation = registerAllocation}
                                  end

                              fun doCommitTrue ()
                                = let
                                    val registerAllocation
                                      = update {value = {register = register,
                                                         memloc = memloc,
                                                         weight = weight,
                                                         sync = true,
                                                         commit = NO},
                                                registerAllocation 
                                                = registerAllocation}
                                  in
                                    {assembly = assembly,
                                     registerAllocation = registerAllocation}
                                  end

                              fun doRemoveFalse ()
                                = let
                                    val registerAllocation
                                      = update {value = {register = register,
                                                         memloc = memloc,
                                                         weight = weight,
                                                         sync = true,
                                                         commit = NO},
                                                registerAllocation 
                                                = registerAllocation}

                                    val registerAllocation
                                      = commitPush {registerAllocation 
                                                    = registerAllocation}

                                    val commit_saves 
                                      = List.removeDuplicates
                                        ((Operand.register register)::saves,
                                         Operand.eq)

                                    val size = Register.size register
                                    val {address,
                                         assembly = assembly_address,
                                         registerAllocation}
                                      = toAddressMemLoc {memloc = memloc,
                                                         info = info,
                                                         size = size,
                                                         supports = supports,
                                                         saves = commit_saves,
                                                         registerAllocation 
                                                         = registerAllocation}

                                    val registerAllocation
                                      = commitPop {registerAllocation 
                                                   = registerAllocation}

                                    val registerAllocation
                                      = if List.contains
                                           (reserved,
                                            register,
                                            Register.eq)
                                          then registerAllocation
                                          else remove {memloc = memloc,
                                                       registerAllocation 
                                                       = registerAllocation}
                                  in
                                    {assembly 
                                     = AppendList.appends
                                       [assembly,
                                        assembly_address,
                                        AppendList.single
                                        (Assembly.instruction_mov
                                         {dst = Operand.Address address,
                                          src = Operand.Register register,
                                          size = size})],
                                     registerAllocation = registerAllocation}
                                  end

                              fun doRemoveTrue ()
                                = let
                                    val registerAllocation
                                      = update {value = {register = register,
                                                         memloc = memloc,
                                                         weight = weight,
                                                         sync = true,
                                                         commit = NO},
                                                registerAllocation 
                                                = registerAllocation}

                                    val registerAllocation
                                      = if List.contains
                                           (reserved,
                                            register,
                                            Register.eq)
                                          then registerAllocation
                                          else remove {memloc = memloc,
                                                       registerAllocation 
                                                       = registerAllocation}
                                  in
                                    {assembly = assembly,
                                     registerAllocation = registerAllocation}
                                  end
                            in
                              case (commit,sync)
                                of (COMMIT 0, false) => doCommitFalse ()
                                 | (COMMIT 0, true) => doCommitTrue ()
                                 | (REMOVE 0, false) => doRemoveFalse ()
                                 | (REMOVE 0, true) => doRemoveTrue ()
                                 | (TRYCOMMIT 0, false) => doCommitFalse ()
                                 | (TRYCOMMIT 0, true) => doCommitTrue ()
                                 | (TRYREMOVE 0, false) => doRemoveFalse ()
                                 | (TRYREMOVE 0, true) => doRemoveTrue ()
                                 | _ 
                                 => Error.bug "amd64AllocateRegisters.RegisterAllocation.commitRegisters"
                            end))
            val _ = Int.dec depth
          in
            {assembly = assembly_commit,
             registerAllocation = registerAllocation}
          end
          handle Spill 
           => spillAndReissue 
              {info = info,
               supports = supports,
               saves = saves,
               registerAllocation = registerAllocation,
               spiller = spillRegisters,
               msg = "commitRegisters",
               reissue = fn {assembly = assembly_spill,
                             registerAllocation}
                          => let
                               val {assembly, registerAllocation}
                                 = commitRegisters 
                                   {info = info,
                                    supports = supports,
                                    saves = saves,
                                    registerAllocation = registerAllocation}
                             in
                               {assembly = AppendList.append (assembly_spill,
                                                              assembly),
                                registerAllocation = registerAllocation}
                             end}

      and commitXmmRegisters {info: Liveness.t,
                               supports: Operand.t list,
                               saves: Operand.t list,
                               registerAllocation as {xmmreserved,...}: t} :
                              {assembly: Assembly.t AppendList.t,
                               registerAllocation: t}
        = let
            val _ = Int.inc depth
            val commit_values
              = xmmvalueFilter {filter = fn {commit = COMMIT 0, ...} => true
                                           | {commit = REMOVE 0, ...} => true
                                           | {commit = TRYCOMMIT 0, ...} => true
                                           | {commit = TRYREMOVE 0, ...} => true
                                           | _ => false, 
                                 registerAllocation = registerAllocation}

            val commit_memlocs = List.revMap(commit_values, #memloc)

            val commit_memlocs
              = totalOrder
                (commit_memlocs,
                 fn (memloc1,memloc2)
                  => List.contains(MemLoc.utilized memloc1,
                                   memloc2,
                                   MemLoc.eq))

            val {assembly = assembly_commit,
                 registerAllocation}
              = List.fold
                (commit_memlocs,
                 {assembly = AppendList.empty,
                  registerAllocation = registerAllocation},
                 fn (memloc,
                     {assembly,
                      registerAllocation})
                  => (case xmmallocated {memloc = memloc,
                                          registerAllocation 
                                          = registerAllocation}
                        of NONE => {assembly = assembly,
                                    registerAllocation = registerAllocation}
                         | SOME ({register,
                                  memloc,
                                  weight,
                                  sync,
                                  commit})
                         => let
                              fun doCommitFalse ()
                                = let
                                    val registerAllocation
                                      = xmmupdate {value = {register = register,
                                                             memloc = memloc,
                                                             weight = weight,
                                                             sync = true,
                                                             commit = NO},
                                                    registerAllocation 
                                                    = registerAllocation}

                                    val registerAllocation
                                      = xmmcommitPush {registerAllocation 
                                                        = registerAllocation}

                                    val commit_saves 
                                      = List.removeDuplicates
                                        ((Operand.xmmregister register)::saves,
                                         Operand.eq)

                                    val size = XmmRegister.size register
                                    val {address,
                                         assembly = assembly_address,
                                         registerAllocation}
                                      = toAddressMemLoc {memloc = memloc,
                                                         info = info,
                                                         size = size,
                                                         supports = supports,
                                                         saves = commit_saves,
                                                         registerAllocation 
                                                         = registerAllocation}

                                    val registerAllocation
                                      = xmmcommitPop {registerAllocation 
                                                       = registerAllocation}
                                  in
                                    {assembly 
                                     = AppendList.appends 
                                       [assembly,
                                        assembly_address,
                                        AppendList.single
                                        (Assembly.instruction_sse_movs
                                         {dst = Operand.Address address,
                                          src = Operand.XmmRegister register,
                                          size = size})],
                                     registerAllocation = registerAllocation}
                                  end

                              fun doCommitTrue ()
                                = let
                                    val registerAllocation
                                      = xmmupdate {value = {register = register,
                                                             memloc = memloc,
                                                             weight = weight,
                                                             sync = true,
                                                             commit = NO},
                                                    registerAllocation 
                                                    = registerAllocation}
                                  in
                                    {assembly = assembly,
                                     registerAllocation = registerAllocation}
                                  end

                              fun doRemoveFalse ()
                                = let
                                    val registerAllocation
                                      = xmmupdate {value = {register = register,
                                                             memloc = memloc,
                                                             weight = weight,
                                                             sync = true,
                                                             commit = NO},
                                                    registerAllocation 
                                                    = registerAllocation}

                                    val registerAllocation
                                      = xmmcommitPush {registerAllocation 
                                                        = registerAllocation}

                                    val commit_saves 
                                      = List.removeDuplicates
                                        ((Operand.xmmregister register)::saves,
                                         Operand.eq)

                                    val size = XmmRegister.size register
                                    val {address,
                                         assembly = assembly_address,
                                         registerAllocation}
                                      = toAddressMemLoc {memloc = memloc,
                                                         info = info,
                                                         size = size,
                                                         supports = supports,
                                                         saves = commit_saves,
                                                         registerAllocation 
                                                         = registerAllocation}

                                    val registerAllocation
                                      = xmmcommitPop {registerAllocation 
                                                       = registerAllocation}

                                    val registerAllocation
                                      = if List.contains
                                           (xmmreserved,
                                            register,
                                            XmmRegister.eq)
                                          then registerAllocation
                                          else xmmremove {memloc = memloc,
                                                           registerAllocation 
                                                           = registerAllocation}
                                  in
                                    {assembly 
                                     = AppendList.appends
                                       [assembly,
                                        assembly_address,
                                        AppendList.single
                                        (Assembly.instruction_sse_movs
                                         {dst = Operand.Address address,
                                          src = Operand.XmmRegister register,
                                          size = size})],
                                     registerAllocation = registerAllocation}
                                  end

                              fun doRemoveTrue ()
                                = let
                                    val registerAllocation
                                      = xmmupdate {value = {register = register,
                                                             memloc = memloc,
                                                             weight = weight,
                                                             sync = true,
                                                             commit = NO},
                                                    registerAllocation 
                                                    = registerAllocation}

                                    val registerAllocation
                                      = if List.contains
                                           (xmmreserved,
                                            register,
                                            XmmRegister.eq)
                                          then registerAllocation
                                          else xmmremove {memloc = memloc,
                                                           registerAllocation 
                                                           = registerAllocation}
                                  in
                                    {assembly = assembly,
                                     registerAllocation = registerAllocation}
                                  end
                            in
                              case (commit,sync)
                                of (COMMIT 0, false) => doCommitFalse ()
                                 | (COMMIT 0, true) => doCommitTrue ()
                                 | (REMOVE 0, false) => doRemoveFalse ()
                                 | (REMOVE 0, true) => doRemoveTrue ()
                                 | (TRYCOMMIT 0, false) => doCommitFalse ()
                                 | (TRYCOMMIT 0, true) => doCommitTrue ()
                                 | (TRYREMOVE 0, false) => doRemoveFalse ()
                                 | (TRYREMOVE 0, true) => doRemoveTrue ()
                                 | _ 
                                 => Error.bug "amd64AllocateRegisters.RegisterAllocation.commitXmmRegisters"
                            end))
            val _ = Int.dec depth
          in
            {assembly = assembly_commit,
             registerAllocation = registerAllocation}
          end
          handle Spill 
           => spillAndReissue 
              {info = info,
               supports = supports,
               saves = saves,
               registerAllocation = registerAllocation,
               spiller = spillRegisters,
               msg = "commitXmmRegisters",
               reissue = fn {assembly = assembly_spill,
                             registerAllocation}
                          => let
                               val {assembly, registerAllocation}
                                 = commitXmmRegisters 
                                   {info = info,
                                    supports = supports,
                                    saves = saves,
                                    registerAllocation = registerAllocation}
                             in
                               {assembly = AppendList.append (assembly_spill,
                                                              assembly),
                                registerAllocation = registerAllocation}
                             end}


      and spillRegisters {info: Liveness.t,
                          supports: Operand.t list,
                          saves: Operand.t list,
                          registerAllocation} :
                         {assembly: Assembly.t AppendList.t,
                          registerAllocation: t}
        = let
            val _ = Int.inc depth
            val spillStart = !spill

            val {reserved, ...} = registerAllocation
            val {assembly = assembly_unreserve,
                 registerAllocation}
              = List.fold
                (reserved,
                 {assembly = AppendList.empty,
                  registerAllocation = registerAllocation},
                 fn (register, 
                     {assembly, registerAllocation})
                  => let
                       val {assembly = assembly_unreserve,
                            registerAllocation}
                         = unreserve'
                           {register = register,
                            registerAllocation = registerAllocation}
                     in
                       {assembly = AppendList.append (assembly,
                                                      assembly_unreserve),
                        registerAllocation = registerAllocation}
                     end)

            val saved = savedRegisters {saves = saves,
                                        registerAllocation = registerAllocation}

            val saved = List.fold
                        (reserved,
                         saved,
                         fn (register,saved)
                          => if List.contains(saved,register,Register.eq)
                               then saved
                               else register::saved)

            val saves = valueFilter
                        {filter = fn {register, ...}
                                   => List.contains(saved,
                                                    register,
                                                    Register.eq),
                         registerAllocation = registerAllocation}

            val all = valueFilter 
                      {filter = fn _ => true,
                       registerAllocation = registerAllocation}

            (* partition the values in the register file 
             * by their base register.
             *)
            val groups = partition (all,
                                    fn ({register = Register.T {reg = reg1, ...},...},
                                        {register = Register.T {reg = reg2, ...},...})
                                     => reg1 = reg2)

            (* order the groups by number of registers used
             *)
            val groups
              = List.insertionSort
                (groups, 
                 fn (g1,g2) => (List.length g1) < (List.length g2))

            (* choose four registers to spill
             *)
            val spills
              = case groups
                  of g1::g2::g3::g4::_ => List.concat [g1,g2,g3,g4]
                   | _ => Error.bug "amd64AllocateRegisters.RegisterAllocation.spillRegisters"

            (* totally order the spills by utilization
             *)
            val spills
              = totalOrder
                (spills,
                 fn ({memloc = memloc1, ...},
                     {memloc = memloc2, ...})
                  => List.contains(MemLoc.utilized memloc2,
                                   memloc1,
                                   MemLoc.eq))

            fun mkReplacer (spillMap : (value * MemLoc.t) list)
              = fn memloc'
                 => case List.peek(spillMap, fn ({memloc,...},_) 
                                              => MemLoc.eq(memloc,memloc'))
                      of SOME (_,spillMemloc) => spillMemloc
                       | NONE => memloc'

            (* associate each spilled value with a spill slot
             *)
            val (spillMap, spillEnd)
              = List.fold
                (spills,
                 ([], spillStart),
                 fn (value as {memloc, ...},
                     (spillMap, spillEnd))
                  => let        
                       val spillMemLoc
                         = MemLoc.imm {base = Immediate.label spillLabel,
                                       index = Immediate.int spillEnd,
                                       scale = amd64MLton.wordScale,
                                       size = MemLoc.size memloc,
                                       class = amd64MLton.Classes.Temp}
                     in
                       ((value,spillMemLoc)::spillMap, 
                        spillEnd + 1)
                     end)

            val replacer = mkReplacer spillMap

            (* commit everything in the register file;
             * also replace all memlocs that are spilled with their spill slot
             *)
            val registerAllocation
              = valueMap {map = fn {register, memloc, weight, sync, commit}
                                 => if List.exists
                                       (spillMap,
                                        fn ({memloc = memloc',...},_)
                                         => MemLoc.eq(memloc,memloc'))
                                      then {register = register,
                                            memloc = MemLoc.replace replacer memloc,
                                            weight = weight,
                                            sync = false,
                                            commit = NO}
                                      else {register = register,
                                            memloc = MemLoc.replace replacer memloc,
                                            weight = weight,
                                            sync = sync,
                                            commit = case commit
                                                       of NO => COMMIT 0
                                                        | COMMIT _ => COMMIT 0
                                                        | TRYCOMMIT _ => COMMIT 0
                                                        | REMOVE _ => REMOVE 0
                                                        | TRYREMOVE _ => REMOVE 0},
                          registerAllocation = registerAllocation}

            (* update next available spill slot for cascading spills *)
            val _ = spill := spillEnd
            (* commit everything;
             * since the spilt memlocs look like they are spill slots,
             * they can all be committed to memory without any additional
             * registers.
             *)
            val {assembly = assembly_commit1,
                 registerAllocation = registerAllocation}
              = commitRegisters
                {info = info,
                 supports = [],
                 saves = [],
                 registerAllocation = registerAllocation}

            (* unspill; as we pull values in, we update the memloc to what it 
             * looks under the pending unspills, and then replace any occurences 
             * of the spill slot with the updated memloc;
             * by the time we are done, everything should be mapped back to 
             * its original form.
             *)
            val {assembly = assembly_unspill,
                 registerAllocation = registerAllocation}
              = let
                  val rec doit
                    = fn ([],{assembly,registerAllocation}) 
                       => {assembly = assembly,
                           registerAllocation = registerAllocation}
                       | (({memloc, weight, sync, commit, ...},
                           spillMemLoc)::spillMap,
                          {assembly, registerAllocation})
                       => let
                            val replacer = mkReplacer spillMap
                            val memloc' = MemLoc.replace replacer memloc

                            val {register,
                                 assembly = assembly_unspill,
                                 registerAllocation}
                              = toRegisterMemLoc
                                {memloc = spillMemLoc,
                                 info = info,
                                 size = MemLoc.size memloc,
                                 move = true,
                                 supports = [],
                                 saves = [],
                                 force = [],
                                 registerAllocation = registerAllocation}
                            val registerAllocation
                              = update {value = {register = register,
                                                 memloc = memloc',
                                                 weight = weight,
                                                 sync = sync,
                                                 commit 
                                                 = case commit
                                                     of NO => COMMIT 0
                                                      | COMMIT _ => COMMIT 0
                                                      | TRYCOMMIT _ => COMMIT 0
                                                      | REMOVE _ => REMOVE 0
                                                      | TRYREMOVE _ => REMOVE 0},
                                        registerAllocation = registerAllocation}

                            val registerAllocation
                              = valueMap
                                {map = fn {register,
                                           memloc,
                                           weight,
                                           sync,
                                           commit}
                                        => {register = register,
                                            memloc = MemLoc.replace
                                                     (fn memloc'' => if MemLoc.eq
                                                                        (memloc'',
                                                                         spillMemLoc)
                                                                       then memloc'
                                                                       else memloc'')
                                                     memloc,
                                            weight = weight,
                                            sync = sync,
                                            commit = commit},
                                 registerAllocation = registerAllocation}

                          in
                            doit(spillMap,
                                 {assembly = AppendList.append (assembly,
                                                                assembly_unspill),
                                  registerAllocation = registerAllocation})
                          end
                in
                  doit(spillMap,
                       {assembly = AppendList.empty, 
                        registerAllocation = registerAllocation})
                end
            (* everything is unspilled *)
            val _ = spill := spillStart

            (* commit all the memlocs that got spilled.
             *)
            val {assembly = assembly_commit2,
                 registerAllocation = registerAllocation}
              = commitRegisters
                {info = info,
                 supports = [],
                 saves = [],
                 registerAllocation = registerAllocation}
            val _ = spill := spillStart

            (* restore the saved operands to their previous locations.
             *)
            val {assembly = assembly_restore,
                 registerAllocation}
              = List.fold
                (saves,
                 {assembly = AppendList.empty,
                  registerAllocation = registerAllocation},
                 fn ({register, memloc, weight, commit, ...},
                     {assembly, registerAllocation})
                  => let
                       val {assembly = assembly_register,
                            registerAllocation,
                            ...}
                         = toRegisterMemLoc
                           {memloc = memloc,
                            info = info,
                            size = Register.size register,
                            move = true,
                            supports = supports,
                            saves = [],
                            force = [register],
                            registerAllocation = registerAllocation}
                       val registerAllocation
                         = update {value = {register = register,
                                            memloc = memloc,
                                            weight = weight,
                                            sync = true,
                                            commit = commit},
                                   registerAllocation = registerAllocation}
                       val {assembly = assembly_reserve,
                            registerAllocation}
                         = reserve' {register = register,
                                     registerAllocation = registerAllocation}
                     in
                       {assembly = AppendList.appends [assembly,
                                                       assembly_register,
                                                       assembly_reserve],
                        registerAllocation = registerAllocation}
                     end)
            val {assembly = assembly_unreserve',
                 registerAllocation}
              = List.fold
                (saved,
                 {assembly = AppendList.empty,
                  registerAllocation = registerAllocation},
                 fn (register, 
                     {assembly, registerAllocation})
                  => let
                       val {assembly = assembly_unreserve',
                            registerAllocation}
                         = unreserve'
                           {register = register,
                            registerAllocation = registerAllocation}
                     in
                       {assembly = AppendList.append (assembly,
                                                      assembly_unreserve'),
                        registerAllocation = registerAllocation}
                     end)
            val {assembly = assembly_reserve,
                 registerAllocation}
              = List.fold
                (reserved,
                 {assembly = AppendList.empty,
                  registerAllocation = registerAllocation},
                 fn (register, 
                     {assembly, registerAllocation})
                  => let
                       val {assembly = assembly_reserve,
                            registerAllocation}
                         = reserve'
                           {register = register,
                            registerAllocation = registerAllocation}
                     in
                       {assembly = AppendList.append (assembly,
                                                      assembly_reserve),
                        registerAllocation = registerAllocation}
                     end)

            val _ = Int.dec depth
          in
            {assembly = AppendList.appends [assembly_unreserve,
                                            assembly_commit1,
                                            assembly_unspill,
                                            assembly_commit2,
                                            assembly_restore,
                                            assembly_unreserve',
                                            assembly_reserve],
             registerAllocation = registerAllocation}
          end

      and toRegisterMemLoc {memloc: MemLoc.t, 
                            info: Liveness.t,
                            size: Size.t, 
                            move: bool,
                            supports: Operand.t list,
                            saves: Operand.t list, 
                            force: Register.t list, 
                            registerAllocation: t} :
                           {register: Register.t,
                            assembly: Assembly.t AppendList.t,
                            registerAllocation: t}
        = (Int.inc depth;
           (case allocated {memloc = memloc,
                            registerAllocation = registerAllocation}
              of SOME {register,memloc,weight,sync,commit}
               => let
                    val registers
                      = potentialRegisters {size = size,
                                            saves = saves,
                                            force = force,
                                            registerAllocation
                                            = registerAllocation}
                  in
                    if List.contains(registers, register, Register.eq)
                      then {register = register,
                            assembly = AppendList.empty,
                            registerAllocation = registerAllocation}
                      else let
                             val {register = final_register,
                                  coincide_values}
                               = chooseRegister 
                                 {info = info,
                                  memloc = SOME memloc,
                                  size = size,
                                  supports = supports,
                                  saves = (Operand.register register)::saves,
                                  force = force,
                                  registerAllocation = registerAllocation}

                             val {memloc,
                                  sync,
                                  registerAllocation}
                               = if List.contains(saves, 
                                                  Operand.register final_register,
                                                  Operand.eq)
                                    orelse
                                    List.contains(saves,
                                                  Operand.memloc memloc,
                                                  Operand.eq)
                                   then {memloc 
                                         = MemLoc.imm
                                           {base = Immediate.label
                                                   (Label.fromString "BUG"),
                                            index = Immediate.zero,
                                            scale = Scale.One,
                                            size = MemLoc.size memloc,
                                            class = MemLoc.Class.Temp},
                                         sync = true,
                                         registerAllocation
                                         = registerAllocation}
                                   else {memloc = memloc,
                                         sync = sync,
                                         registerAllocation
                                         = delete {register = register,
                                                   registerAllocation
                                                   = registerAllocation}}
                           in
                             case coincide_values
                               of [] 
                                => if move
                                     then let
                                            val registerAllocation
                                              = update {value 
                                                        = {register 
                                                           = final_register,
                                                           memloc = memloc,
                                                           weight = weight,
                                                           sync = sync,
                                                           commit = commit},
                                                        registerAllocation
                                                        = registerAllocation}
                                          in
                                            {register = final_register,
                                             assembly
                                             = AppendList.single
                                               (Assembly.instruction_mov
                                                {src = Operand.register register,
                                                 dst = Operand.register 
                                                       final_register,
                                                 size = size}),
                                             registerAllocation 
                                             = registerAllocation}
                                          end
                                     else let
                                            val registerAllocation
                                              = update {value 
                                                        = {register 
                                                           = final_register,
                                                           memloc = memloc,
                                                           weight = weight,
                                                           sync = true,
                                                           commit = commit},
                                                        registerAllocation
                                                        = registerAllocation}
                                          in
                                            {register = final_register,
                                             assembly = AppendList.empty,
                                             registerAllocation
                                             = registerAllocation}
                                          end
                                | [{register = register',
                                    memloc = memloc',
                                    weight = weight',
                                    sync = sync',
                                    commit = commit'}] 
                                => if Register.eq(register',final_register)
                                     then let
                                            val registerAllocation
                                              = delete {register
                                                        = register',
                                                        registerAllocation
                                                        = registerAllocation}
                                            val registerAllocation
                                              = update {value
                                                        = {register 
                                                           = register,
                                                           memloc = memloc',
                                                           weight = weight',
                                                           sync = sync',
                                                           commit = commit'},
                                                        registerAllocation
                                                        = registerAllocation}
                                          in
                                            if move
                                              then let
                                                     val registerAllocation
                                                       = update
                                                         {value
                                                          = {register 
                                                             = final_register,
                                                             memloc = memloc,
                                                             weight = weight,
                                                             sync = sync,
                                                             commit = commit},
                                                          registerAllocation
                                                          = registerAllocation}
                                                   in
                                                     {register = final_register,
                                                      assembly 
                                                      = AppendList.single
                                                        (Assembly.instruction_xchg
                                                         {src = Operand.register 
                                                                register,
                                                          dst = Operand.register 
                                                                final_register,
                                                          size = size}),
                                                      registerAllocation
                                                      = registerAllocation}
                                                   end
                                              else let
                                                     val registerAllocation
                                                       = update
                                                         {value
                                                          = {register 
                                                             = final_register,
                                                             memloc = memloc,
                                                             weight = weight,
                                                             sync = true,
                                                             commit = commit},
                                                          registerAllocation
                                                          = registerAllocation}
                                                   in
                                                     {register = final_register,
                                                      assembly 
                                                      = AppendList.single
                                                        (Assembly.instruction_mov
                                                         {src = Operand.register 
                                                                final_register,
                                                          dst = Operand.register 
                                                                register,
                                                          size = size}),
                                                      registerAllocation
                                                      = registerAllocation}
                                                   end
                                          end
                                     else let
                                            val {register = final_register,
                                                 assembly = assembly_register,
                                                 registerAllocation}
                                              = freeRegister
                                                {info = info,
                                                 memloc = SOME memloc,
                                                 size = size,
                                                 supports = supports,
                                                 saves = (Operand.register
                                                          register)::saves,
                                                 force = force,
                                                 registerAllocation
                                                 = registerAllocation}
                                            val registerAllocation
                                              = remove 
                                                {memloc = memloc,
                                                 registerAllocation
                                                 = registerAllocation}
                                          in
                                            if move
                                              then let
                                                     val registerAllocation
                                                       = update 
                                                         {value
                                                          = {register
                                                             = final_register,
                                                             memloc = memloc,
                                                             weight = weight,
                                                             sync = sync,
                                                             commit = commit},
                                                          registerAllocation
                                                          = registerAllocation}
                                                   in
                                                     {register = final_register,
                                                      assembly 
                                                      = AppendList.appends
                                                        [assembly_register,
                                                         AppendList.single
                                                         (Assembly.instruction_mov
                                                          {src = Operand.register 
                                                                 register,
                                                           dst = Operand.register 
                                                                 final_register,
                                                           size = size})],
                                                      registerAllocation
                                                      = registerAllocation}
                                                   end
                                              else let
                                                     val registerAllocation
                                                       = update 
                                                         {value
                                                          = {register
                                                             = final_register,
                                                             memloc = memloc,
                                                             weight = weight,
                                                             sync = true,
                                                             commit = commit},
                                                          registerAllocation
                                                          = registerAllocation}
                                                   in
                                                     {register = final_register,
                                                      assembly 
                                                      = assembly_register,
                                                      registerAllocation
                                                      = registerAllocation}
                                                   end
                                          end
                                | _ 
                                => let
                                     val {register = final_register,
                                          assembly = assembly_register,
                                          registerAllocation}
                                       = freeRegister {info = info,
                                                       memloc = SOME memloc,
                                                       size = size,
                                                       supports = supports,
                                                       saves = (Operand.register
                                                                register)::saves,
                                                       force = force,
                                                       registerAllocation
                                                       = registerAllocation}
                                     val registerAllocation
                                       = remove {memloc = memloc,
                                                 registerAllocation
                                                 = registerAllocation}
                                   in
                                     if move
                                       then let
                                              val registerAllocation
                                                = update {value
                                                          = {register
                                                             = final_register,
                                                             memloc = memloc,
                                                             weight = weight,
                                                             sync = sync,
                                                             commit = commit},
                                                          registerAllocation
                                                          = registerAllocation}
                                            in
                                              {register = final_register,
                                               assembly 
                                               = AppendList.appends
                                                 [assembly_register,
                                                  AppendList.single
                                                  (Assembly.instruction_mov
                                                   {src = Operand.register 
                                                          register,
                                                    dst = Operand.register 
                                                          final_register,
                                                    size = size})],
                                               registerAllocation
                                               = registerAllocation}
                                            end
                                       else let
                                              val registerAllocation
                                                = update {value
                                                          = {register
                                                             = final_register,
                                                             memloc = memloc,
                                                             weight = weight,
                                                             sync = true,
                                                             commit = commit},
                                                          registerAllocation
                                                          = registerAllocation}
                                            in
                                              {register = final_register,
                                               assembly 
                                               = assembly_register,
                                               registerAllocation
                                               = registerAllocation}
                                            end
                                   end
                           end

                  end
               | NONE 
               => if move
                    then case MemLoc.size memloc
                           of Size.BYTE
                            => let
                                 val {register = register', 
                                      assembly = assembly_register,
                                      registerAllocation}
                                   = freeRegister 
                                     {info = info,
                                      memloc = SOME memloc,
                                      size = size,
                                      supports = (Operand.memloc memloc)::
                                                 supports,
                                      saves = saves,
                                      force = [],
                                      registerAllocation 
                                      = registerAllocation}

                                 val {address, 
                                      assembly = assembly_address,
                                      registerAllocation}
                                   = toAddressMemLoc 
                                     {memloc = memloc,
                                      info = info,
                                      size = size,
                                      supports = supports,
                                      saves = (Operand.register register')::
                                              saves,
                                      registerAllocation = registerAllocation}


                                 val registerAllocation
                                   = remove 
                                     {memloc = memloc,
                                      registerAllocation = registerAllocation}

                                 val registerAllocation
                                   = update 
                                     {value = {register = register',
                                               memloc = memloc,
                                               weight = 1024,
                                               sync = true,
                                               commit = NO},
                                      registerAllocation = registerAllocation}

                                 val {register,
                                      assembly = assembly_force,
                                      registerAllocation}
                                   = toRegisterMemLoc
                                     {memloc = memloc,
                                      info = info,
                                      size = size,
                                      move = move,
                                      supports = supports,
                                      saves = saves,
                                      force = force,
                                      registerAllocation = registerAllocation}

                               in
                                 {register = register,
                                  assembly 
                                  = AppendList.appends
                                    [assembly_register,
                                     assembly_address,
                                     AppendList.single
                                     (Assembly.instruction_mov
                                      {dst = Operand.register register',
                                       src = Operand.address address,
                                       size = size}),
                                     assembly_force],
                                  registerAllocation = registerAllocation}
                               end
                            | _ 
                            => let
                                 val {address, 
                                      assembly = assembly_address,
                                      registerAllocation}
                                   = toAddressMemLoc 
                                     {memloc = memloc,
                                      info = info,
                                      size = size,
                                      supports = supports,
                                      saves = saves,
                                      registerAllocation = registerAllocation}

                                 val saves'
                                   = case address
                                       of Address.T {base = SOME base',
                                                     index = SOME index',
                                                     ...}
                                        => (Operand.register base')::
                                           (Operand.register index')::saves
                                        | Address.T {base = SOME base',
                                                     ...}
                                        => (Operand.register base')::saves
                                        | Address.T {index = SOME index',
                                                     ...}
                                        => (Operand.register index')::saves
                                        | _ => saves

                                 val {register = register', 
                                      assembly = assembly_register,
                                      registerAllocation}
                                   = freeRegister 
                                     {info = info,
                                      memloc = SOME memloc,
                                      size = size,
                                      supports = supports,
                                      saves = saves',
                                      force = [],
                                      registerAllocation = registerAllocation}

                                 val registerAllocation
                                   = remove 
                                     {memloc = memloc,
                                      registerAllocation = registerAllocation}

                                 val registerAllocation
                                   = update 
                                     {value = {register = register',
                                               memloc = memloc,
                                               weight = 1024,
                                               sync = true,
                                               commit = NO},
                                      registerAllocation = registerAllocation}

                                 val {register,
                                      assembly = assembly_force,
                                      registerAllocation}
                                   = toRegisterMemLoc
                                     {memloc = memloc,
                                      info = info,
                                      size = size,
                                      move = move,
                                      supports = supports,
                                      saves = saves,
                                      force = force,
                                      registerAllocation = registerAllocation}

                               in
                                 {register = register,
                                  assembly 
                                  = AppendList.appends 
                                    [assembly_address,
                                     assembly_register,
                                     AppendList.single
                                     (Assembly.instruction_mov
                                      {dst = Operand.register register',
                                       src = Operand.address address,
                                       size = size}),
                                     assembly_force],
                                  registerAllocation = registerAllocation}
                               end
                    else let
                           val {register, 
                                assembly = assembly_register,
                                registerAllocation}
                             = freeRegister {info = info,
                                             memloc = SOME memloc,
                                             size = size,
                                             supports = supports,
                                             saves = saves,
                                             force = force,
                                             registerAllocation 
                                             = registerAllocation}
                           val registerAllocation
                             = remove {memloc = memloc,
                                       registerAllocation = registerAllocation}

                           val registerAllocation
                             = update {value = {register = register,
                                                memloc = memloc,
                                                weight = 1024,
                                                sync = true,
                                                commit = NO},
                                       registerAllocation = registerAllocation}
                         in
                           {register = register,
                            assembly = assembly_register,
                            registerAllocation = registerAllocation}
                         end) 
              before (Int.dec depth))
          handle Spill 
           => spillAndReissue 
              {info = info,
               supports = supports,
               saves = saves,
               registerAllocation = registerAllocation,
               spiller = spillRegisters,
               msg = "toRegisterMemLoc",
               reissue = fn {assembly = assembly_spill,
                             registerAllocation}
                          => let
                               val {register, assembly, registerAllocation}
                                 = toRegisterMemLoc
                                   {memloc = memloc,
                                    info = info,
                                    size = size,
                                    move = move,
                                    supports = supports,
                                    saves = saves,
                                    force = force,
                                    registerAllocation = registerAllocation}
                             in
                               {register = register,
                                assembly = AppendList.append (assembly_spill,
                                                              assembly),
                                registerAllocation = registerAllocation}
                             end}

      and toXmmRegisterMemLoc {memloc: MemLoc.t, 
                                info: Liveness.t,
                                size: Size.t, 
                                move: bool,
                                supports: Operand.t list,
                                saves: Operand.t list, 
                                force: XmmRegister.t list, 
                                registerAllocation: t} :
                               {register: XmmRegister.t,
                                assembly: Assembly.t AppendList.t,
                                registerAllocation: t}
        = (Int.inc depth;
           (case xmmallocated {memloc = memloc,
                                registerAllocation = registerAllocation}
              of SOME {register,memloc,weight,sync,commit}
               => let
                    val registers
                      = potentialXmmRegisters {size = size,
                                                saves = saves,
                                                force = force,
                                                registerAllocation
                                                = registerAllocation}
                  in
                    if List.contains(registers, register, XmmRegister.eq)
                      then {register = register,
                            assembly = AppendList.empty,
                            registerAllocation = registerAllocation}
                      else let
                             val {register = final_register,
                                  coincide_values}
                               = chooseXmmRegister 
                                 {info = info,
                                  memloc = SOME memloc,
                                  size = size,
                                  supports = supports,
                                  saves = (Operand.xmmregister register)::saves,
                                  force = force,
                                  registerAllocation = registerAllocation}

                             val {memloc,
                                  sync,
                                  registerAllocation}
                               = if List.contains(saves, 
                                                  Operand.xmmregister final_register,
                                                  Operand.eq)
                                    orelse
                                    List.contains(saves,
                                                  Operand.memloc memloc,
                                                  Operand.eq)
                                   then {memloc 
                                         = MemLoc.imm
                                           {base = Immediate.label
                                                   (Label.fromString "BUG"),
                                            index = Immediate.zero,
                                            scale = Scale.One,
                                            size = MemLoc.size memloc,
                                            class = MemLoc.Class.Temp},
                                         sync = true,
                                         registerAllocation
                                         = registerAllocation}
                                   else {memloc = memloc,
                                         sync = sync,
                                         registerAllocation
                                         = xmmdelete {register = register,
                                                       registerAllocation
                                                       = registerAllocation}}
                           in
                             case coincide_values
                               of [] 
                                => if move
                                     then let
                                            val registerAllocation
                                              = xmmupdate {value 
                                                            = {register 
                                                               = final_register,
                                                               memloc = memloc,
                                                               weight = weight,
                                                               sync = sync,
                                                               commit = commit},
                                                            registerAllocation
                                                            = registerAllocation}
                                          in
                                            {register = final_register,
                                             assembly
                                             = AppendList.single
                                               (Assembly.instruction_sse_movs
                                                {src = Operand.xmmregister register,
                                                 dst = Operand.xmmregister 
                                                       final_register,
                                                 size = size}),
                                             registerAllocation 
                                             = registerAllocation}
                                          end
                                     else let
                                            val registerAllocation
                                              = xmmupdate {value 
                                                            = {register 
                                                               = final_register,
                                                               memloc = memloc,
                                                               weight = weight,
                                                               sync = true,
                                                               commit = commit},
                                                            registerAllocation
                                                            = registerAllocation}
                                          in
                                            {register = final_register,
                                             assembly = AppendList.empty,
                                             registerAllocation
                                             = registerAllocation}
                                          end
                                | _ 
                                => let
                                     val {register = final_register,
                                          assembly = assembly_register,
                                          registerAllocation}
                                       = freeXmmRegister {info = info,
                                                           memloc = SOME memloc,
                                                           size = size,
                                                           supports = supports,
                                                           saves = (Operand.xmmregister
                                                                    register)::saves,
                                                           force = force,
                                                           registerAllocation
                                                           = registerAllocation}
                                     val registerAllocation
                                       = xmmremove {memloc = memloc,
                                                     registerAllocation
                                                     = registerAllocation}
                                   in
                                     if move
                                       then let
                                              val registerAllocation
                                                = xmmupdate {value
                                                              = {register
                                                                 = final_register,
                                                                 memloc = memloc,
                                                                 weight = weight,
                                                                 sync = sync,
                                                                 commit = commit},
                                                              registerAllocation
                                                              = registerAllocation}
                                            in
                                              {register = final_register,
                                               assembly 
                                               = AppendList.appends
                                                 [assembly_register,
                                                  AppendList.single
                                                  (Assembly.instruction_sse_movs
                                                   {src = Operand.xmmregister 
                                                          register,
                                                    dst = Operand.xmmregister 
                                                          final_register,
                                                    size = size})],
                                               registerAllocation
                                               = registerAllocation}
                                            end
                                       else let
                                              val registerAllocation
                                                = xmmupdate {value
                                                              = {register
                                                                 = final_register,
                                                                 memloc = memloc,
                                                                 weight = weight,
                                                                 sync = true,
                                                                 commit = commit},
                                                              registerAllocation
                                                              = registerAllocation}
                                            in
                                              {register = final_register,
                                               assembly 
                                               = assembly_register,
                                               registerAllocation
                                               = registerAllocation}
                                            end
                                   end
                           end

                  end
               | NONE 
               => if move
                    then       let
                                 val {address, 
                                      assembly = assembly_address,
                                      registerAllocation}
                                   = toAddressMemLoc 
                                     {memloc = memloc,
                                      info = info,
                                      size = size,
                                      supports = supports,
                                      saves = saves,
                                      registerAllocation = registerAllocation}

                                 val saves'
                                   = case address
                                       of Address.T {base = SOME base',
                                                     index = SOME index',
                                                     ...}
                                        => (Operand.register base')::
                                           (Operand.register index')::saves
                                        | Address.T {base = SOME base',
                                                     ...}
                                        => (Operand.register base')::saves
                                        | Address.T {index = SOME index',
                                                     ...}
                                        => (Operand.register index')::saves
                                        | _ => saves

                                 val {register = register', 
                                      assembly = assembly_register,
                                      registerAllocation}
                                   = freeXmmRegister 
                                     {info = info,
                                      memloc = SOME memloc,
                                      size = size,
                                      supports = supports,
                                      saves = saves',
                                      force = [],
                                      registerAllocation = registerAllocation}

                                 val registerAllocation
                                   = xmmremove 
                                     {memloc = memloc,
                                      registerAllocation = registerAllocation}

                                 val registerAllocation
                                   = xmmupdate 
                                     {value = {register = register',
                                               memloc = memloc,
                                               weight = 1024,
                                               sync = true,
                                               commit = NO},
                                      registerAllocation = registerAllocation}

                                 val {register,
                                      assembly = assembly_force,
                                      registerAllocation}
                                   = toXmmRegisterMemLoc
                                     {memloc = memloc,
                                      info = info,
                                      size = size,
                                      move = move,
                                      supports = supports,
                                      saves = saves,
                                      force = force,
                                      registerAllocation = registerAllocation}

                               in
                                 {register = register,
                                  assembly 
                                  = AppendList.appends 
                                    [assembly_address,
                                     assembly_register,
                                     AppendList.single
                                     (Assembly.instruction_sse_movs
                                      {dst = Operand.xmmregister register',
                                       src = Operand.address address,
                                       size = size}),
                                     assembly_force],
                                  registerAllocation = registerAllocation}
                               end
                    else let
                           val {register, 
                                assembly = assembly_register,
                                registerAllocation}
                             = freeXmmRegister {info = info,
                                                 memloc = SOME memloc,
                                                 size = size,
                                                 supports = supports,
                                                 saves = saves,
                                                 force = force,
                                                 registerAllocation 
                                                 = registerAllocation}
                           val registerAllocation
                             = xmmremove {memloc = memloc,
                                           registerAllocation = registerAllocation}

                           val registerAllocation
                             = xmmupdate {value = {register = register,
                                                    memloc = memloc,
                                                    weight = 1024,
                                                    sync = true,
                                                    commit = NO},
                                           registerAllocation = registerAllocation}
                         in
                           {register = register,
                            assembly = assembly_register,
                            registerAllocation = registerAllocation}
                         end) 
              before (Int.dec depth))
          handle Spill 
           => spillAndReissue 
              {info = info,
               supports = supports,
               saves = saves,
               registerAllocation = registerAllocation,
               spiller = spillRegisters,
               msg = "toXmmRegisterMemLoc",
               reissue = fn {assembly = assembly_spill,
                             registerAllocation}
                          => let
                               val {register, assembly, registerAllocation}
                                 = toXmmRegisterMemLoc
                                   {memloc = memloc,
                                    info = info,
                                    size = size,
                                    move = move,
                                    supports = supports,
                                    saves = saves,
                                    force = force,
                                    registerAllocation = registerAllocation}
                             in
                               {register = register,
                                assembly = AppendList.append (assembly_spill,
                                                              assembly),
                                registerAllocation = registerAllocation}
                             end}

      and toAddressMemLoc {memloc: MemLoc.t, 
                           info: Liveness.t,
                           size: Size.t, 
                           supports: Operand.t list,
                           saves: Operand.t list, 
                           registerAllocation: t} :
                          {address: Address.t,
                           assembly: Assembly.t AppendList.t,
                           registerAllocation: t}
        = (Int.inc depth;
           (let
              val MemLoc.U {immBase, memBase, immIndex, memIndex, scale, ...}
                = MemLoc.destruct memloc
                
              (* Whenever possible, find labels with RIP-relative addressing.
               * It's smaller code and faster even for position dependent code.
               * However, RIP-relative addressing cannot be used with an index
               * register. For PIC code we will thus break the access down
               * into a leal for the symbol and a toRegister for the memIndex.
               *)
              
              (* Combine all immediate offsets into one *)
              val disp 
                = case (immBase, immIndex) of
                     (NONE, NONE) => Immediate.zero
                   | (SOME immBase, NONE) => immBase
                   | (NONE, SOME immIndex) 
                   => (case Immediate.destruct immIndex of
                          Immediate.Word _ => immIndex
                        | _ => Error.bug "amd64AllocateRegisters.RegisterAllocation.toAddressMemLoc:indexLabel")
                   | (SOME immBase, SOME immIndex) 
                   => (case (Immediate.destruct immBase, Immediate.destruct immIndex) of
                          (Immediate.Label l1, Immediate.Word w2) => 
                             Immediate.labelPlusWord (l1, w2)
                        | (Immediate.LabelPlusWord (l1, w1), Immediate.Word w2) => 
                             Immediate.labelPlusWord (l1, WordX.add (w1, w2))
                        | _ => Error.bug "amd64AllocateRegisters.RegisterAllocation.toAddressMemLoc:disp")
              
              (* The base register gets supplied by three distinct cases:
               * 1 - memBase (which means that there is no label)
               * 2 - RIP     (which means there is no index)
               * 3 - lea     (which means this is PIC)
               * else nothing
               *)
              val {disp,
                   register = register_base,
                   assembly = assembly_base,
                   registerAllocation}
               = case (Immediate.destruct disp, memBase, memIndex) of
                    (Immediate.Word _, NONE, _)
                  =>  {disp = SOME disp,
                       register = NONE,
                       assembly = AppendList.empty,
                       registerAllocation = registerAllocation}
                  | (Immediate.Word _, SOME memBase, _) (* no label, no rip *)
                  => let
                        val {register, assembly, registerAllocation}
                          = toRegisterMemLoc 
                            {memloc = memBase,
                             info = info,
                             size = MemLoc.size memBase,
                             move = true,
                             supports 
                             = case memIndex
                                 of NONE => supports
                                  | SOME memIndex
                                 => (Operand.memloc memIndex)::
                                     supports,
                             saves = saves,
                             force = Register.baseRegisters,
                             registerAllocation = registerAllocation}
                     in
                       {disp = SOME disp,
                        register = SOME register,
                        assembly = assembly,
                        registerAllocation = registerAllocation}
                     end
                  | (_, SOME _, _) (* label & memBase? bad input *)
                  => Error.bug "amd64AllocateRegisters.RegisterAllocation.toAddressMemLoc:base*2"
                  | (_, NONE, NONE) (* no index => safe to use RIP-relative *)
                  => {disp = SOME disp,
                      register = SOME Register.rip,
                      assembly = AppendList.empty,
                      registerAllocation = registerAllocation}
                  | (_, NONE, SOME memIndex) (* label + index => use lea if PIC *)
                  => if !Control.positionIndependent = false
                        then {disp = SOME disp,
                              register = NONE,
                              assembly = AppendList.empty,
                              registerAllocation = registerAllocation}
                     else let
                             val {register, assembly, registerAllocation}
                               = toRegisterImmediate
                                 {immediate = disp,
                                  info = info,
                                  size = MemLoc.size memIndex,
                                  supports = Operand.memloc memIndex :: supports,
                                  saves = saves,
                                  force = Register.baseRegisters,
                                  registerAllocation = registerAllocation}
                          in
                             { disp = NONE,
                               register = SOME register,
                               assembly = assembly,
                               registerAllocation = registerAllocation}
                          end

              val {register = register_index,
                   assembly = assembly_index,
                   registerAllocation}
                = case memIndex
                    of NONE => {register = NONE,
                                assembly = AppendList.empty,
                                registerAllocation = registerAllocation}
                     | SOME memIndex
                     => let
                          val {register, assembly, registerAllocation}
                            = toRegisterMemLoc 
                              {memloc = memIndex,
                               info = info,
                               size = MemLoc.size memIndex,
                               move = true,
                               supports = supports,
                               saves 
                               = case (memBase, register_base)
                                   of (NONE, NONE) => saves
                                    | (NONE, SOME register_base)
                                    => if register_base = Register.rip
                                          then saves
                                       else Operand.register register_base ::
                                            saves
                                    | (SOME memBase, SOME register_base)
                                    => (Operand.memloc memBase)::
                                       (Operand.register register_base)::
                                       saves
                                    | _ => Error.bug "amd64AllocateRegisters.RegisterAllocation.toAddressMemLoc",
                               force = Register.indexRegisters,
                               registerAllocation = registerAllocation}
                        in
                          {register = SOME register,
                           assembly = assembly,
                           registerAllocation = registerAllocation}
                        end
            in
              {address = Address.T {disp = disp,
                                    base = register_base,
                                    index = register_index,
                                    scale = case memIndex
                                              of SOME _ => SOME scale
                                               | NONE => NONE},
               assembly = AppendList.append (assembly_base, 
                                             assembly_index),
               registerAllocation = registerAllocation}
            end)

(*
           (case MemLoc.destruct memloc
              of MemLoc.U {base = MemLoc.Imm base, index = MemLoc.Imm index,
                           scale, size, ...}
               => let
                    val disp' 
                      = if Immediate.eq(index, Immediate.const_int 0)
                          then NONE
                          else SOME (Immediate.binexp 
                                     {oper = Immediate.Multiplication, 
                                      exp1 = index, 
                                      exp2 = Scale.toImmediate scale})
                    val disp
                      = case disp'
                          of NONE => SOME base
                           | SOME disp' => SOME (Immediate.binexp 
                                                 {oper = Immediate.Addition,
                                                  exp1 = base, 
                                                  exp2 = disp'})
                  in
                    {address = Address.T {disp = disp,
                                          base = NONE,
                                          index = NONE,
                                          scale = NONE},
                     assembly = AppendList.empty,
                     registerAllocation = registerAllocation}
                  end
               | MemLoc.U {base = MemLoc.Imm base, index = MemLoc.Mem index,
                           scale, size, ...}
               => let
                    val disp = SOME base

                    val {register = register_index,
                         assembly = assembly_index,
                         registerAllocation}
                      = toRegisterMemLoc {memloc = index,
                                          info = info,
                                          size = MemLoc.size index,
                                          move = true,
                                          supports = supports,
                                          saves = saves,
                                          force = Register.indexRegisters,
                                          registerAllocation 
                                          = registerAllocation}
                  in
                    {address = Address.T {disp = disp,
                                          base = NONE,
                                          index = SOME register_index,
                                          scale = SOME scale},
                     assembly = assembly_index,
                     registerAllocation = registerAllocation}
                  end
               | MemLoc.U {base = MemLoc.Mem base, index = MemLoc.Imm index,
                           scale, size, ...}
               => let
                    val disp
                      = if Immediate.eq(index, Immediate.const_int 0)
                          then NONE
                          else SOME (Immediate.binexp 
                                     {oper = Immediate.Multiplication,
                                      exp1 = index, 
                                      exp2 = Scale.toImmediate scale})

                    val {register = register_base,
                         assembly = assembly_base,
                         registerAllocation}
                      = toRegisterMemLoc {memloc = base,
                                          info = info,
                                          size = MemLoc.size base,
                                          move = true,
                                          supports = supports,
                                          saves = saves,
                                          force = Register.baseRegisters,
                                          registerAllocation 
                                          = registerAllocation}
                  in
                    {address = Address.T {disp = disp,
                                          base = SOME register_base,
                                          index = NONE,
                                          scale = NONE},
                     assembly = assembly_base,
                     registerAllocation = registerAllocation}
                  end
               | MemLoc.U {base = MemLoc.Mem base, index = MemLoc.Mem index,
                           scale, size, ...}
               => let
                    val {register = register_base,
                         assembly = assembly_base,
                         registerAllocation}
                      = toRegisterMemLoc {memloc = base,
                                          info = info,
                                          size = MemLoc.size base,
                                          move = true,
                                          supports 
                                          = (Operand.memloc index)::supports,
                                          saves = saves,
                                          force = Register.baseRegisters,
                                          registerAllocation 
                                          = registerAllocation}

                    val {register = register_index,
                         assembly = assembly_index,
                         registerAllocation}
                      = toRegisterMemLoc {memloc = index,
                                          info = info,
                                          size = MemLoc.size index,
                                          move = true,
                                          supports = supports,
                                          saves = (Operand.memloc base)::
                                                  (Operand.register 
                                                   register_base)::
                                                  saves,
                                          force = Register.indexRegisters,
                                          registerAllocation 
                                          = registerAllocation}
                  in
                    {address = Address.T {disp = NONE,
                                          base = SOME register_base,
                                          index = SOME register_index,
                                          scale = SOME scale},
                     assembly = AppendList.append (assembly_base, 
                                                   assembly_index),
                     registerAllocation = registerAllocation}
                  end)
*)
              before (Int.dec depth))
          handle Spill 
           => spillAndReissue 
              {info = info,
               supports = supports,
               saves = saves,
               registerAllocation = registerAllocation,
               spiller = spillRegisters,
               msg = "toAddressMemLoc",
               reissue = fn {assembly = assembly_spill,
                             registerAllocation}
                          => let
                               val {address, assembly, registerAllocation}
                                 = toAddressMemLoc
                                   {memloc = memloc,
                                    info = info,
                                    size = size,
                                    supports = supports,
                                    saves = saves,
                                    registerAllocation = registerAllocation}
                             in
                               {address = address,
                                assembly = AppendList.append (assembly_spill,
                                                              assembly),
                                registerAllocation = registerAllocation}
                             end}

      and toRegisterImmediate {immediate: Immediate.t,
                               info: Liveness.t, 
                               size: Size.t,
                               supports: Operand.t list,
                               saves: Operand.t list,
                               force: Register.t list,
                               registerAllocation: t} :
                              {register: Register.t,
                               assembly: Assembly.t AppendList.t,
                               registerAllocation: t}
        = let
            val _ = Int.inc depth
            val {register = final_register, assembly, registerAllocation}
              = freeRegister {info = info,
                              memloc = NONE,
                              size = size,
                              supports = supports,
                              saves = saves,
                              force = force,
                              registerAllocation = registerAllocation}
            val _ = Int.dec depth
            val instruction
              = case Immediate.destruct immediate of
                   Immediate.Word x =>
                      if size = Size.QUAD andalso
                         WordX.equals (x, WordX.resize (WordX.resize (x, WordSize.word32), WordSize.word64))
                      then (* use the implicit zero-extend of 32 bit ops *)
                       Assembly.instruction_mov 
                       {dst = Operand.Register (Register.lowPartOf (final_register, Size.LONG)),
                        src = Operand.immediate_word (WordX.resize (x, WordSize.word32)),
                        size = Size.LONG}
                      else
                       Assembly.instruction_mov 
                       {dst = Operand.Register final_register,
                        src = Operand.Immediate immediate,
                        size = size}
                 | _ =>
                      Assembly.instruction_lea
                      {dst = Operand.Register final_register,
                       src = Operand.Address
                              (Address.T { disp = SOME immediate,
                                           base = SOME Register.rip,
                                           index = NONE, scale = NONE }),
                       size = size}
          in
            {register = final_register,
             assembly = AppendList.appends
                        [assembly,
                         AppendList.single instruction],
             registerAllocation = registerAllocation}
          end
          handle Spill 
           => spillAndReissue 
              {info = info,
               supports = supports,
               saves = saves,
               registerAllocation = registerAllocation,
               spiller = spillRegisters,
               msg = "toRegisterImmediate",
               reissue = fn {assembly = assembly_spill,
                             registerAllocation}
                          => let
                               val {register, assembly, registerAllocation}
                                 = toRegisterImmediate
                                   {immediate = immediate,
                                    info = info,
                                    size = size,
                                    supports = supports,
                                    saves = saves,
                                    force = force,
                                    registerAllocation = registerAllocation}
                             in
                               {register = register,
                                assembly = AppendList.append (assembly_spill,
                                                              assembly),
                                registerAllocation = registerAllocation}
                             end}

      fun pre {uses: Operand.t list,
               defs: Operand.t list,
               kills: Operand.t list,
               info as {dead,
                        remove,
                        ...}: Liveness.t,
               registerAllocation: t} :
              {assembly: Assembly.t AppendList.t,
               registerAllocation: t}
        = let
            val ra = registerAllocation 

            val dead_memlocs = dead
            val remove_memlocs = remove

            val (allUses, allDefs, allKills)
              = let
                  fun doit operands
                    = List.fold
                      (operands,
                       MemLocSet.empty,
                       fn (operand,set)
                        => case Operand.deMemloc operand
                             of SOME memloc
                              => MemLocSet.add(set, memloc)
                              | NONE => set)

                  val uses = doit uses
                  val defs = doit defs
                  val kills = doit kills

                  fun doit' (memlocs, set)
                    = MemLocSet.fold
                      (memlocs,
                       set,
                       fn (memloc, set)
                        => MemLocSet.union
                           (set,
                            MemLocSet.fromList (MemLoc.utilized memloc)))
                  val allUses
                    = doit'(uses,
                      doit'(defs,
                            uses))
                  val allDefs = defs
                  val allKills = kills
                in
                  (allUses, allDefs, allKills)
                end

            val allDest = MemLocSet.unions 
                          [allDefs, allKills, dead_memlocs, remove_memlocs]
            val allKeep = MemLocSet.unions 
                          [allUses, allDefs, allKills]

            val registerAllocation
              = xmmvalueMap
                {map = fn {register,
                           memloc,
                           weight, 
                           sync, 
                           commit}
                        => let
                             val must_commit0
                               = (MemLocSet.exists
                                  (allDefs,
                                   fn memloc'
                                    => not (MemLoc.eq(memloc', memloc))
                                       andalso (MemLoc.mayAlias(memloc', memloc))))
                             val must_commit1
                               = (MemLocSet.exists
                                  (allUses,
                                   fn memloc' 
                                    => not (MemLoc.eq(memloc', memloc))
                                       andalso (MemLoc.mayAlias(memloc', memloc))))
                             val must_commit2
                               = (List.exists
                                  (MemLoc.utilized memloc,
                                   fn memloc
                                    => MemLocSet.contains (allDest, memloc)))
                             val must_commit3
                               = (MemLocSet.contains
                                  (MemLocSet.-(allKills, dead_memlocs), memloc))
                             val sync
                               = if volatile memloc
                                   then true
                                   else sync
                             val commit
                               = if volatile memloc
                                   then REMOVE 0
                                 else if must_commit3
                                   then COMMIT 0
                                 else if must_commit2
                                   then if MemLocSet.contains
                                           (allKeep, memloc)
                                          then COMMIT 0
                                          else REMOVE 0
                                 else if must_commit1 orelse must_commit0
                                   then case commit
                                          of TRYREMOVE _ => REMOVE 0
                                           | REMOVE _ => REMOVE 0
                                           | _ => COMMIT 0
                                 else commit
                           in
                             {register = register,
                              memloc = memloc,
                              weight = weight,
                              sync = sync,
                              commit = commit}
                           end,
                 registerAllocation = registerAllocation}

            val {assembly = assembly_commit_xmmregisters,
                 registerAllocation,
                 ...}
              = commitXmmRegisters {info = info,
                                     supports = [],
                                     saves = [],
                                     registerAllocation = registerAllocation}

            val registerAllocation
              = valueMap
                {map = fn {register,
                           memloc,
                           weight, 
                           sync, 
                           commit}
                        => let
                             val must_commit0
                               = (MemLocSet.exists
                                  (allDefs,
                                   fn memloc'
                                    => not (MemLoc.eq(memloc', memloc))
                                       andalso (MemLoc.mayAlias(memloc', memloc))))
                             val must_commit1
                               = (MemLocSet.exists
                                  (allUses,
                                   fn memloc' 
                                    => not (MemLoc.eq(memloc', memloc))
                                       andalso (MemLoc.mayAlias(memloc', memloc))))
                             val must_commit2
                               = (List.exists
                                  (MemLoc.utilized memloc,
                                   fn memloc
                                    => MemLocSet.contains (allDest, memloc)))
                             val must_commit3
                               = (MemLocSet.contains
                                  (MemLocSet.-(allKills, dead_memlocs), memloc))
                             val sync
                               = if volatile memloc
                                   then true
                                   else sync
                             val commit
                               = if volatile memloc
                                   then REMOVE 0
                                 else if MemLocSet.contains(allDefs, memloc)
                                   then if must_commit1 orelse must_commit0
                                          then case commit
                                                 of TRYREMOVE _ => REMOVE 0
                                                  | REMOVE _ => REMOVE 0
                                                  | _ => COMMIT 0
                                          else commit
                                 else if must_commit3
                                   then COMMIT 0
                                 else if must_commit2
                                   then if MemLocSet.contains
                                           (allKeep, memloc)
                                          then COMMIT 0
                                          else REMOVE 0
                                 else if must_commit1 orelse must_commit0
                                   then case commit 
                                          of TRYREMOVE _ => REMOVE 0
                                           | REMOVE _ => REMOVE 0
                                           | _ => COMMIT 0
                                 else commit
                           in
                             {register = register,
                              memloc = memloc,
                              weight = weight,
                              sync = sync,
                              commit = commit}
                           end,
                 registerAllocation = registerAllocation}

            val {assembly = assembly_commit_registers,
                 registerAllocation}
              = commitRegisters {info = info,
                                 supports = [],
                                 saves = [],
                                 registerAllocation = registerAllocation}
          in
            {assembly = AppendList.appends
                        [if !Control.Native.commented > 3
                           then AppendList.cons
                                ((Assembly.comment "pre begin:"),
                                 (toComments ra))
                           else AppendList.empty,
                         assembly_commit_xmmregisters,
                         assembly_commit_registers,
                         if !Control.Native.commented > 3
                           then AppendList.cons
                                ((Assembly.comment "pre end:"),
                                 (toComments registerAllocation))
                           else AppendList.empty],
             registerAllocation = registerAllocation}
          end

      val (pre, pre_msg)
        = tracer
          "pre"
          pre

      fun post {uses: Operand.t list,
                final_uses: Operand.t list,
                defs: Operand.t list,
                final_defs: Operand.t list,
                kills: Operand.t list,
                info as {dead,
                         commit,
                         remove,
                         ...}: Liveness.t,
                registerAllocation: t} :
               {assembly: Assembly.t AppendList.t,
                registerAllocation: t}
        = let 
            val ra = registerAllocation

            val (final_uses_registers,
                 final_defs_registers,
                 final_uses_xmmregisters,
                 final_defs_xmmregisters)
              = let
                  fun doit(operands, (final_registers, final_xmmregisters))
                    = List.fold
                      (operands,
                       (final_registers, final_xmmregisters),
                       fn (operand, (final_registers, final_xmmregisters))
                        => case (Operand.deRegister operand,
                                 Operand.deXmmregister operand)
                             of (SOME register, _) 
                              => if List.contains(final_registers,
                                                  register,
                                                  Register.eq)
                                   then (final_registers,
                                         final_xmmregisters)
                                   else (register::final_registers,
                                         final_xmmregisters)
                              | (_, SOME register)
                              => if List.contains(final_xmmregisters,
                                                  register,
                                                  XmmRegister.eq)
                                   then (final_registers,
                                         final_xmmregisters)
                                   else (final_registers,
                                         register::final_xmmregisters)
                              | _ => (final_registers, final_xmmregisters))
                  val (final_uses_registers, final_uses_xmmregisters)
                    = doit(final_uses, ([], []))
                  val (final_defs_registers, final_defs_xmmregisters)
                    = doit(final_defs, ([], []))
                in
                  (final_uses_registers,
                   final_defs_registers,
                   final_uses_xmmregisters,
                   final_defs_xmmregisters)
                end

            val dead_memlocs = dead
            val commit_memlocs = commit
            val remove_memlocs = remove

            val (_, allDefs, allKills)
              = let
                  fun doit operands
                    = List.fold
                      (operands,
                       MemLocSet.empty,
                       fn (operand,set)
                        => case Operand.deMemloc operand
                             of SOME memloc
                              => MemLocSet.add(set, memloc)
                              | NONE => set)

                  val uses = doit uses
                  val defs = doit defs
                  val kills = doit kills

                  fun doit' (memlocs, set)
                    = MemLocSet.fold
                      (memlocs,
                       set,
                       fn (memloc, set)
                        => MemLocSet.union
                           (set,
                            MemLocSet.fromList (MemLoc.utilized memloc)))
                  val allUses
                    = doit'(uses,
                      doit'(defs,
                            uses))
                  val allDefs = defs
                  val allKills = kills
                in
                  (allUses, allDefs, allKills)
                end

            val allDest = MemLocSet.unions 
                          [allDefs, allKills, dead_memlocs, remove_memlocs]

            val registerAllocation
              = xmmvalueMap 
                {map = fn {register,
                           memloc,
                           weight,
                           sync,
                           commit}
                        => if volatile memloc
                             then let
                                    val isDst
                                      = List.contains
                                        (final_defs_xmmregisters,
                                         register,
                                       XmmRegister.eq)
                                    val isDef = isDst
                                  in
                                    {register = register,
                                     memloc = memloc,
                                     sync = sync andalso (not isDef),
                                     weight = weight - 500,
                                     commit = REMOVE 0}
                                  end
                           else if MemLocSet.contains
                                  (dead_memlocs, memloc)
                             then {register = register,
                                   memloc = memloc,
                                   sync = true,
                                   weight = weight - 500,
                                   commit = TRYREMOVE 0}
                           else let
                                  val isSrc
                                    = List.contains
                                      (final_uses_xmmregisters,
                                       register,
                                       XmmRegister.eq)

                                  val isDst
                                    = List.contains
                                      (final_defs_xmmregisters,
                                       register,
                                       XmmRegister.eq)

                                  val isDef = isDst
                                in
                                  {register = register,
                                   memloc = memloc,
                                   weight = weight - 5
                                            + (if isSrc
                                                 then 5
                                                 else 0)
                                            + (if isDst
                                                 then 10
                                                 else 0),
                                   sync = sync andalso (not isDef),
                                   commit =        if List.exists
                                                      (MemLoc.utilized memloc,
                                                       fn memloc'
                                                       => MemLocSet.contains
                                                          (allDest, memloc'))
                                                     then REMOVE 0
                                                   else if MemLocSet.contains
                                                           (remove_memlocs,
                                                            memloc)
                                                     then TRYREMOVE 0
                                                   else if MemLocSet.contains
                                                           (commit_memlocs,
                                                            memloc)
                                                     then TRYCOMMIT 0
                                                   else commit}
                                end,
                  registerAllocation = registerAllocation}

            val {assembly = assembly_commit_xmmregisters,
                 registerAllocation,
                 ...}
              = commitXmmRegisters {info = info,
                                     supports = [],
                                     saves = [],
                                     registerAllocation = registerAllocation}

            val registerAllocation
              = valueMap 
                {map = fn value as {register,
                                    memloc,
                                    weight,
                                    sync,
                                    commit}
                        => if volatile memloc
                             then let
                                    val isDst
                                      = List.contains
                                        (final_defs_registers,
                                         register,
                                         Register.eq)
                                    val isDef = isDst
                                  in
                                    {register = register,
                                     memloc = memloc,
                                     sync = sync andalso (not isDef),
                                     weight = weight - 500,
                                     commit = REMOVE 0}
                                  end
                           else if MemLocSet.contains
                                   (dead_memlocs, memloc)
                             then value
                           else let
                                  val isSrc
                                    = List.contains
                                      (final_uses_registers,
                                       register,
                                       Register.eq)

                                  val isDst
                                    = List.contains
                                      (final_defs_registers,
                                       register,
                                       Register.eq)

                                  val isDef = isDst
                                in
                                  {register = register,
                                   memloc = memloc,
                                   weight = weight - 5
                                            + (if isSrc
                                                 then 5
                                                 else 0)
                                            + (if isDst
                                                 then 10
                                                 else 0),
                                   sync = sync andalso (not isDef),
                                   commit = if List.exists
                                               (MemLoc.utilized memloc,
                                                fn memloc'
                                                 => MemLocSet.contains
                                                    (allDest, memloc'))
                                              then REMOVE 0
                                            else if MemLocSet.contains
                                                    (remove_memlocs,
                                                     memloc)
                                              then TRYREMOVE 0
                                            else if MemLocSet.contains
                                                    (commit_memlocs,
                                                     memloc)
                                              then TRYCOMMIT 0
                                            else commit}
                                end,
                  registerAllocation = registerAllocation}

            val {assembly = assembly_commit_registers,
                 registerAllocation}
              = commitRegisters {info = info,
                                 supports = [],
                                 saves = [],
                                 registerAllocation = registerAllocation}

            val registerAllocation
              = valueMap 
                {map = fn value as {register,
                                    memloc,
                                    weight,
                                    ...}
                        => if MemLocSet.contains
                              (dead_memlocs, memloc)
                             then {register = register,
                                   memloc = memloc,
                                   sync = true,
                                   weight = weight,
                                   commit = REMOVE 0}
                             else value,
                 registerAllocation = registerAllocation}

            val {assembly = assembly_dead_registers,
                 registerAllocation}
              = commitRegisters {info = info,
                                 supports = [],
                                 saves = [],
                                 registerAllocation = registerAllocation}
          in
            {assembly = AppendList.appends
                        [if !Control.Native.commented > 3
                           then AppendList.cons
                                ((Assembly.comment "post begin:"),
                                 (toComments ra))
                           else AppendList.empty,
                         assembly_commit_xmmregisters,
                         assembly_commit_registers,
                         assembly_dead_registers,
                         if !Control.Native.commented > 3
                           then AppendList.cons
                                ((Assembly.comment "post end:"),
                                 (toComments registerAllocation))
                           else AppendList.empty],
             registerAllocation = registerAllocation}
          end

      val (post, post_msg)
        = tracer
          "post"
          post

      fun allocateOperand {operand: Operand.t,
                           options = {register: bool,
                                      immediate: WordSize.t option,
                                      label: bool,
                                      address: bool},
                           info as {dead, 
                                    remove,
                                    ...}: Liveness.t,
                           size: Size.t,
                           move: bool,
                           supports: Operand.t list,
                           saves: Operand.t list,
                           force: Register.t list,
                           registerAllocation: t} :
                          {operand: Operand.t,
                           assembly: Assembly.t AppendList.t,
                           registerAllocation: t}
        = case operand
            of Operand.Immediate i
             => if Option.isSome immediate
                   andalso (case Immediate.destruct i of
                               Immediate.Word w => 
                                  let
                                     val dstSize = Option.valOf immediate
                                     val srcSize = WordX.size w
                                  in
                                     case WordSize.compare (srcSize, dstSize) of
                                        LESS => true
                                      | EQUAL => true
                                      | GREATER => 
                                           WordX.equals
                                           (w, 
                                            WordX.resizeX 
                                            (WordX.resizeX (w, dstSize), 
                                             srcSize))
                                  end
                             | _ => false)
                  then {operand = operand,
                        assembly = AppendList.empty,
                        registerAllocation = registerAllocation}
                else if register
                  then let
                         val {register, 
                              assembly, 
                              registerAllocation}
                           = toRegisterImmediate {immediate = i,
                                                  info = info,
                                                  size = size,
                                                  supports = supports,
                                                  saves = saves,
                                                  force = force,
                                                  registerAllocation
                                                  = registerAllocation}
                       in
                         {operand = Operand.register register,
                          assembly = assembly,
                          registerAllocation = registerAllocation}
                       end
                else if address
                  then let
                         val address
                           = Address.T 
                             {disp = SOME (Immediate.label (Label.fromString "raTemp1")),
                              base = NONE,
                              index = NONE,
                              scale = NONE}
                       in 
                         {operand = Operand.address address,
                          assembly = AppendList.single
                                     (Assembly.instruction_mov
                                      {src = Operand.immediate i,
                                       dst = Operand.address address,
                                       size = size}),
                          registerAllocation = registerAllocation}
                       end 
               else Error.bug "amd64AllocateRegisters.RegisterAllocation.allocateOperand: operand:Immediate"
             | Operand.Label l
             => if label
                  then {operand = operand,
                        assembly = AppendList.empty,
                        registerAllocation = registerAllocation}
                else if Option.isSome immediate
                        andalso (* FIXME: could use RIP relative with 32bit immediate. *)
                                (let
                                    val dstSize = Option.valOf immediate
                                 in
                                    case WordSize.compare (WordSize.word64, dstSize) of
                                       LESS => true
                                     | EQUAL => true
                                     | GREATER => false
                                 end)
                  then {operand = Operand.immediate_label l,
                        assembly = AppendList.empty,
                        registerAllocation = registerAllocation}
                else if register
                  then let
                         val {register, 
                              assembly, 
                              registerAllocation}
                           = toRegisterImmediate {immediate 
                                                  = Immediate.label l,
                                                  info = info,
                                                  size = size,
                                                  supports = supports,
                                                  saves = saves,
                                                  force = force,
                                                  registerAllocation
                                                  = registerAllocation}
                       in
                         {operand = Operand.register register,
                          assembly = assembly,
                          registerAllocation = registerAllocation}
                       end
                else Error.bug "amd64AllocateRegisters.RegisterAllocation.allocateOperand: operand:Label"
             | Operand.MemLoc m
             => let
                  fun toRegisterMemLoc' ()
                    = let
                        val {register, 
                             assembly, 
                             registerAllocation}
                          = toRegisterMemLoc 
                            {memloc = m,
                             info = info,
                             size = size,
                             move = move,
                             supports = supports,
                             saves = saves,
                             force = force,
                             registerAllocation = registerAllocation}
                      in
                        {operand = Operand.Register register,
                         assembly = assembly,
                         registerAllocation = registerAllocation}
                      end
                  fun toAddressMemLoc' ()
                    = let
                        val {address, 
                             assembly, 
                             registerAllocation}
                          = toAddressMemLoc 
                            {memloc = m,
                             info = info,
                             size = size,
                             supports = supports,
                             saves = saves,
                             registerAllocation
                             = registerAllocation}
                      in
                        {operand = Operand.Address address,
                         assembly = assembly,
                         registerAllocation = registerAllocation}
                      end
                  fun toAddressMemLocRemove' ()
                    = let
                        val registerAllocation
                          = valueMap {map 
                                      = fn value as {register,
                                                     memloc,
                                                     weight,
                                                     sync,
                                                     ...}
                                         => if MemLoc.eq(memloc, m)
                                              then {register = register, 
                                                    memloc = memloc, 
                                                    weight = weight, 
                                                    sync = sync, 
                                                    commit = REMOVE 0}
                                              else value,
                                      registerAllocation = registerAllocation}

                        val {assembly = assembly_commit,
                             registerAllocation}
                          = commitRegisters {info = info,
                                             supports = supports,
                                             saves = saves,
                                             registerAllocation 
                                             = registerAllocation}

                        val {address, assembly, registerAllocation}
                          = toAddressMemLoc {memloc = m,
                                             info = info,
                                             size = size,
                                             supports = supports,
                                             saves = saves,
                                             registerAllocation
                                             = registerAllocation}
                      in
                        {operand = Operand.Address address,
                         assembly = AppendList.append (assembly_commit,
                                                       assembly),
                         registerAllocation = registerAllocation}
                      end
                in 
                  if register andalso address
                    then case allocated {memloc = m,
                                         registerAllocation 
                                         = registerAllocation}
                           of NONE 
                            => if MemLocSet.contains(dead, m)
                                  orelse
                                  MemLocSet.contains(remove, m)
                                 then toAddressMemLoc' ()
                                 else toRegisterMemLoc' ()
                            | SOME _
                            => toRegisterMemLoc' ()
                  else if register
                     then toRegisterMemLoc' ()
                  else if address
                     then toAddressMemLocRemove' ()
                  else Error.bug "amd64AllocateRegisters.RegisterAllocation.allocateOperand: operand:MemLoc"
                end
             | _ => Error.bug "amd64AllocateRegisters.RegisterAllocation.allocateOperand: operand"

      val (allocateOperand, allocateOperand_msg)
        = tracer
          "allocateOperand"
          allocateOperand

      fun allocateXmmOperand {operand: Operand.t,
                               options = {xmmregister: bool,
                                          address: bool},
                               info as {dead, 
                                        remove,
                                        ...}: Liveness.t,
                               size: Size.t,
                               move: bool,
                               supports: Operand.t list,
                               saves: Operand.t list,
                               force: XmmRegister.t list,
                               registerAllocation: t} :
                              {operand: Operand.t,
                               assembly: Assembly.t AppendList.t,
                               registerAllocation: t}
        = case operand
            of Operand.MemLoc m
             => let
                  fun toXmmRegisterMemLoc' ()
                    = let
                        val {register, 
                             assembly, 
                             registerAllocation}
                          = toXmmRegisterMemLoc 
                            {memloc = m,
                             info = info,
                             size = size,
                             move = move,
                             supports = supports,
                             saves = saves,
                             force = force,
                             registerAllocation = registerAllocation}
                      in
                        {operand = Operand.XmmRegister register,
                         assembly = assembly,
                         registerAllocation = registerAllocation}
                      end
                  fun toAddressMemLoc' ()
                    = let
                        val {address, 
                             assembly, 
                             registerAllocation}
                          = toAddressMemLoc 
                            {memloc = m,
                             info = info,
                             size = size,
                             supports = supports,
                             saves = saves,
                             registerAllocation
                             = registerAllocation}
                      in
                        {operand = Operand.Address address,
                         assembly = assembly,
                         registerAllocation = registerAllocation}
                      end
                  fun toAddressMemLocRemove' ()
                    = let
                        val registerAllocation
                          = xmmvalueMap {map 
                                          = fn value as {register,
                                                         memloc,
                                                         weight,
                                                         sync,
                                                         ...}
                                             => if MemLoc.eq(memloc, m)
                                                  then {register = register, 
                                                        memloc = memloc, 
                                                        weight = weight, 
                                                        sync = sync, 
                                                        commit = REMOVE 0}
                                                  else value,
                                          registerAllocation = registerAllocation}

                        val {assembly = assembly_commit,
                             registerAllocation}
                          = commitXmmRegisters {info = info,
                                                 supports = supports,
                                                 saves = saves,
                                                 registerAllocation 
                                                 = registerAllocation}

                        val {address, assembly, registerAllocation}
                          = toAddressMemLoc {memloc = m,
                                             info = info,
                                             size = size,
                                             supports = supports,
                                             saves = saves,
                                             registerAllocation
                                             = registerAllocation}
                      in
                        {operand = Operand.Address address,
                         assembly = AppendList.append (assembly_commit,
                                                       assembly),
                         registerAllocation = registerAllocation}
                      end
                in 
                  if xmmregister andalso address
                    then case xmmallocated {memloc = m,
                                             registerAllocation 
                                             = registerAllocation}
                           of NONE 
                            => if MemLocSet.contains(dead, m)
                                  orelse
                                  MemLocSet.contains(remove, m)
                                 then toAddressMemLoc' ()
                                 else toXmmRegisterMemLoc' ()
                            | SOME _
                            => toXmmRegisterMemLoc' ()
                  else if xmmregister
                     then toXmmRegisterMemLoc' ()
                  else if address
                     then toAddressMemLocRemove' ()
                  else Error.bug "amd64AllocateRegisters.RegisterAllocation.allocateXmmOperand: operand:MemLoc"
                end
             | _ => Error.bug "amd64AllocateRegisters.RegisterAllocation.allocateXmmOperand: operand"

      val (allocateXmmOperand, allocateXmmOperand_msg)
        = tracer
          "allocateXmmOperand"
          allocateXmmOperand

      (* Implementation of directives. *)

      fun assume {assumes : {register: Register.t, 
                             memloc: MemLoc.t, 
                             weight: int,
                             sync: bool,
                             reserve: bool} list,
                  info = _,
                  registerAllocation}
        = let
            val {assembly,
                 registerAllocation}
              = List.foldr
                (assumes,
                 {assembly = AppendList.empty,
                  registerAllocation = registerAllocation},
                 fn ({register,
                      memloc,
                      weight,
                      sync,
                      reserve},
                     {assembly, registerAllocation})
                  => let
                       val registerAllocation
                         = update
                           {value = {register = register,
                                     memloc = memloc,
                                     weight = weight,
                                     sync = sync,
                                     commit = NO},
                            registerAllocation = registerAllocation}

                       val {assembly = assembly_reserve,
                            registerAllocation}
                         = if reserve
                             then reserve' {register = register,
                                            registerAllocation = registerAllocation}
                             else unreserve' {register = register,
                                              registerAllocation = registerAllocation}
                     in
                       {assembly = AppendList.append (assembly,
                                                      assembly_reserve),
                        registerAllocation = registerAllocation}
                     end)
          in
            {assembly = assembly,
             registerAllocation = registerAllocation}
          end 

      fun xmmassume {assumes : {register: XmmRegister.t, 
                                 memloc: MemLoc.t, 
                                 weight: int,
                                 sync: bool,
                                 reserve: bool} list,
                      info = _,
                      registerAllocation}
        = let
            val {assembly,
                 registerAllocation}
              = List.foldr
                (assumes,
                 {assembly = AppendList.empty,
                  registerAllocation = registerAllocation},
                 fn ({register,
                      memloc,
                      weight,
                      sync,
                      reserve},
                     {assembly, registerAllocation})
                  => let
                       val registerAllocation
                         = xmmupdate
                           {value = {register = register,
                                     memloc = memloc,
                                     weight = weight,
                                     sync = sync,
                                     commit = NO},
                            registerAllocation = registerAllocation}

                       val {assembly = assembly_reserve,
                            registerAllocation}
                         = if reserve
                             then xmmreserve' {register = register,
                                                registerAllocation = registerAllocation}
                             else xmmunreserve' {register = register,
                                                  registerAllocation = registerAllocation}
                     in
                       {assembly = AppendList.append (assembly,
                                                      assembly_reserve),
                        registerAllocation = registerAllocation}
                     end)
          in
            {assembly = assembly,
             registerAllocation = registerAllocation}
          end 

      fun cache {caches: {register: Register.t,
                          memloc: MemLoc.t,
                          reserve: bool} list,
                 info,
                 registerAllocation}
        = let
            val supports
              = List.revMap
                (caches,
                 fn {memloc, ...} => Operand.memloc memloc)

            datatype u = None | Reg of Register.t | Mem of MemLoc.t

            fun computeEdges' {reg,
                               registerAllocation}
              = List.revMap
                (Register.coincident' reg,
                 fn register'
                  => let
                       val (from, m)
                         = case List.peek
                                (caches,
                                 fn {register, ...} 
                                  => Register.eq(register, register'))
                             of NONE => (None, NONE)
                              | SOME {memloc, ...}
                              => (case allocated {memloc = memloc,
                                                  registerAllocation 
                                                  = registerAllocation}
                                    of NONE 
                                     => (Mem memloc, SOME memloc)
                                     | SOME {register, ...} 
                                     => (Reg register, SOME memloc))

                       val to
                         = case valueRegister
                                {register = register',
                                 registerAllocation = registerAllocation}
                             of NONE => None
                              | SOME {memloc = memloc', ...}
                              => (case List.peek
                                       (caches,
                                        fn {memloc, ...}
                                         => MemLoc.eq(memloc, memloc'))
                                    of NONE => None
                                     | SOME {register, ...} => Reg register)
                     in
                       (from, m, register', to)
                     end)

            fun computeEdges {registerAllocation}
              = List.revMap
                (Register.allReg,
                 fn reg
                  => (reg, computeEdges' {reg = reg,
                                          registerAllocation = registerAllocation}))

            fun doitSelf {edges,
                          saves,
                          assembly,
                          registerAllocation}
              = let
                  val {yes = self, no = edges}
                    = List.partition
                      (edges,
                       fn (_, edges')
                        => List.forall
                           (edges', 
                            fn (Reg rf, _, r, Reg rt)
                             => Register.eq(rf, r) andalso
                                Register.eq(r, rt)
                             | _ => false))
                in
                  if not (List.isEmpty self)
                    then let
                           val saves_self 
                             = List.fold
                               (self,
                                [],
                                fn ((_, edges'), saves)
                                 => List.fold
                                    (edges',
                                     saves,
                                     fn ((_,_,r,_), saves)
                                      => (Operand.register r)::saves))
                         in
                           doit {edges = edges,
                                 saves = saves_self @ saves,
                                 assembly = assembly,
                                 registerAllocation = registerAllocation}
                         end
                    else doitEasy {edges = edges,
                                   saves = saves,
                                   assembly = assembly,
                                   registerAllocation = registerAllocation}
                end

            and doitEasy {edges,
                          saves,
                          assembly,
                          registerAllocation}
              = let
                  val {easy}
                    = List.fold
                      (edges,
                       {easy = NONE},
                       fn ((_, edges'), {easy = NONE})
                        => let
                             val {easy}
                               = List.fold
                                 (edges',
                                  {easy = NONE},
                                  fn ((Reg _, SOME m, r, None), 
                                      {easy = NONE})
                                   => {easy = SOME (m, r)}
                                   | (_, {easy})
                                   => {easy = easy})
                           in
                             {easy = easy}
                           end
                        | ((_, _), {easy})
                        => {easy = easy})
                in
                  case easy
                    of SOME (m, r)
                     => let
                          val {assembly = assembly_register,
                               registerAllocation,
                               ...}
                            = toRegisterMemLoc 
                              {memloc = m,
                               info = info,
                               size = MemLoc.size m,
                               move = true,
                               supports = supports,
                               saves = saves, 
                               force = [r], 
                               registerAllocation = registerAllocation}

                          val edges = computeEdges 
                                      {registerAllocation = registerAllocation}
                        in
                          doit {edges = edges,
                                saves = [],
                                assembly = AppendList.append
                                           (assembly, assembly_register),
                                registerAllocation = registerAllocation}
                        end
                     | NONE => doitHard {edges = edges,
                                         saves = saves,
                                         assembly = assembly,
                                         registerAllocation = registerAllocation}
                end

            and doitHard {edges,
                          saves,
                          assembly,
                          registerAllocation}
              = let
                  val {hard}
                    = List.fold
                      (edges,
                       {hard = NONE},
                       fn ((_, edges'), {hard = NONE})
                        => let
                             val {hard}
                               = List.fold
                                 (edges',
                                  {hard = NONE},
                                  fn ((Mem _, SOME m, r, None), 
                                      {hard = NONE})
                                   => {hard = SOME (m, r)}
                                   | (_, {hard})
                                   => {hard = hard})
                           in
                             {hard = hard}
                           end
                        | ((_, _), {hard})
                        => {hard = hard})
                in
                  case hard
                    of SOME (m, r)
                     => let
                          val {assembly = assembly_register,
                               registerAllocation,
                               ...}
                            = toRegisterMemLoc 
                              {memloc = m,
                               info = info,
                               size = MemLoc.size m,
                               move = true,
                               supports = supports,
                               saves = saves, 
                               force = [r], 
                               registerAllocation = registerAllocation}

                          val edges = computeEdges 
                                      {registerAllocation = registerAllocation}
                        in
                          doit {edges = edges,
                                saves = [],
                                assembly = AppendList.append
                                           (assembly, assembly_register),
                                registerAllocation = registerAllocation}
                        end
                     | NONE => doitCycle {edges = edges,
                                          saves = saves,
                                          assembly = assembly,
                                          registerAllocation = registerAllocation}
                end

            and doitCycle {edges,
                           saves,
                           assembly,
                           registerAllocation = registerAllocation}
              = let
                  val {cycle}
                    = List.fold
                      (edges,
                       {cycle = NONE},
                       fn ((_, edges'), {cycle = NONE})
                        => let
                             val {cycle}
                               = List.fold
                                 (edges',
                                  {cycle = NONE},
                                  fn ((Reg _, SOME m, r, Reg _), 
                                      {cycle = NONE})
                                   => {cycle = SOME (m, r)}
                                   | (_, {cycle})
                                   => {cycle = cycle})
                           in
                             {cycle = cycle}
                           end
                        | ((_, _), {cycle})
                        => {cycle = cycle})
                in
                  case cycle
                    of SOME (m, r)
                     => let
                          val {assembly = assembly_register,
                               registerAllocation,
                               ...}
                            = toRegisterMemLoc 
                              {memloc = m,
                               info = info,
                               size = MemLoc.size m,
                               move = true,
                               supports = supports,
                               saves = saves, 
                               force = [r], 
                               registerAllocation = registerAllocation}

                          val edges = computeEdges 
                                      {registerAllocation = registerAllocation}
                        in
                          doit {edges = edges,
                                saves = [],
                                assembly = AppendList.append
                                           (assembly, assembly_register),
                                registerAllocation = registerAllocation}
                        end
                     | NONE => doitCycle {edges = edges,
                                          saves = saves,
                                          assembly = assembly,
                                          registerAllocation = registerAllocation}
                end

            and doit {edges,
                      saves,
                      assembly,
                      registerAllocation}
              = let
                  val edges
                    = List.fold
                      (edges,
                       [],
                       fn ((reg, edges'), edges)
                        => let
                             val edges' 
                               = List.revRemoveAll
                                 (edges',
                                  fn (None, _, _, None) => true
                                   | _ => false)
                           in
                             if List.isEmpty edges'
                               then edges
                               else (reg, edges')::edges
                           end)
                in
                  if List.isEmpty edges
                    then {assembly = assembly,
                          registerAllocation = registerAllocation}
                    else doitSelf {edges = edges,
                                   saves = saves,
                                   assembly = assembly,
                                   registerAllocation = registerAllocation}
                end

            val {assembly = assembly_force,
                 registerAllocation}
              = doit {edges = computeEdges {registerAllocation = registerAllocation},
                      saves = [],
                      assembly = AppendList.empty,
                      registerAllocation = registerAllocation}

            val {assembly = assembly_reserve,
                 registerAllocation}
              = reserve {registers = List.revKeepAllMap
                                     (caches, 
                                      fn {register, reserve, ...} 
                                       => if reserve 
                                            then SOME register 
                                            else NONE),
                         registerAllocation = registerAllocation}

          in
            {assembly = AppendList.append(assembly_force, assembly_reserve),
             registerAllocation = registerAllocation}
          end

(*
      fun xmmcache {caches: {register: XmmRegister.t,
                              memloc: MemLoc.t,
                              reserve: bool} list,
                     info,
                     registerAllocation}
        = let
            val supports
              = List.revMap
                (caches,
                 fn {memloc, ...} => Operand.memloc memloc)

            datatype u = None | XmmReg of XmmRegister.t | Mem of MemLoc.t

            fun computeEdges' {reg,
                               registerAllocation}
              = List.revMap
                (XmmRegister.coincident' reg,
                 fn register'
                  => let
                       val (from, m)
                         = case List.peek
                                (caches,
                                 fn {register, ...} 
                                  => XmmRegister.eq(register, register'))
                             of NONE => (None, NONE)
                              | SOME {memloc, ...}
                              => (case xmmallocated {memloc = memloc,
                                                      registerAllocation 
                                                      = registerAllocation}
                                    of NONE 
                                     => (Mem memloc, SOME memloc)
                                     | SOME {register, ...} 
                                     => (XmmReg register, SOME memloc))

                       val to
                         = case xmmvalueRegister
                                {register = register',
                                 registerAllocation = registerAllocation}
                             of NONE => None
                              | SOME {memloc = memloc', ...}
                              => (case List.peek
                                       (caches,
                                        fn {memloc, ...}
                                         => MemLoc.eq(memloc, memloc'))
                                    of NONE => None
                                     | SOME {register, ...} => XmmReg register)
                     in
                       (from, m, register', to)
                     end)

            fun computeEdges {registerAllocation}
              = List.revMap
                (XmmRegister.allReg,
                 fn reg
                  => (reg, computeEdges' {reg = reg,
                                          registerAllocation = registerAllocation}))

            fun doitSelf {edges,
                          saves,
                          assembly,
                          registerAllocation}
              = let
                  val {yes = self, no = edges}
                    = List.partition
                      (edges,
                       fn (_, edges')
                        => List.forall
                           (edges', 
                            fn (XmmReg rf, _, r, XmmReg rt)
                             => XmmRegister.eq(rf, r) andalso
                                XmmRegister.eq(r, rt)
                             | _ => false))
                in
                  if not (List.isEmpty self)
                    then let
                           val saves_self 
                             = List.fold
                               (self,
                                [],
                                fn ((_, edges'), saves)
                                 => List.fold
                                    (edges',
                                     saves,
                                     fn ((_,_,r,_), saves)
                                      => (Operand.xmmregister r)::saves))
                         in
                           doit {edges = edges,
                                 saves = saves_self @ saves,
                                 assembly = assembly,
                                 registerAllocation = registerAllocation}
                         end
                    else doitEasy {edges = edges,
                                   saves = saves,
                                   assembly = assembly,
                                   registerAllocation = registerAllocation}
                end

            and doitEasy {edges,
                          saves,
                          assembly,
                          registerAllocation}
              = let
                  val {easy}
                    = List.fold
                      (edges,
                       {easy = NONE},
                       fn ((_, edges'), {easy = NONE})
                        => let
                             val {easy}
                               = List.fold
                                 (edges',
                                  {easy = NONE},
                                  fn ((XmmReg _, SOME m, r, None), 
                                      {easy = NONE})
                                   => {easy = SOME (m, r)}
                                   | (_, {easy})
                                   => {easy = easy})
                           in
                             {easy = easy}
                           end
                        | ((_, _), {easy})
                        => {easy = easy})
                in
                  case easy
                    of SOME (m, r)
                     => let
                          val {assembly = assembly_register,
                               registerAllocation,
                               ...}
                            = toXmmRegisterMemLoc 
                              {memloc = m,
                               info = info,
                               size = MemLoc.size m,
                               move = true,
                               supports = supports,
                               saves = saves, 
                               force = [r], 
                               registerAllocation = registerAllocation}

                          val edges = computeEdges 
                                      {registerAllocation = registerAllocation}
                        in
                          doit {edges = edges,
                                saves = [],
                                assembly = AppendList.append
                                           (assembly, assembly_register),
                                registerAllocation = registerAllocation}
                        end
                     | NONE => doitHard {edges = edges,
                                         saves = saves,
                                         assembly = assembly,
                                         registerAllocation = registerAllocation}
                end

            and doitHard {edges,
                          saves,
                          assembly,
                          registerAllocation}
              = let
                  val {hard}
                    = List.fold
                      (edges,
                       {hard = NONE},
                       fn ((_, edges'), {hard = NONE})
                        => let
                             val {hard}
                               = List.fold
                                 (edges',
                                  {hard = NONE},
                                  fn ((Mem _, SOME m, r, None), 
                                      {hard = NONE})
                                   => {hard = SOME (m, r)}
                                   | (_, {hard})
                                   => {hard = hard})
                           in
                             {hard = hard}
                           end
                        | ((_, _), {hard})
                        => {hard = hard})
                in
                  case hard
                    of SOME (m, r)
                     => let
                          val {assembly = assembly_register,
                               registerAllocation,
                               ...}
                            = toXmmRegisterMemLoc 
                              {memloc = m,
                               info = info,
                               size = MemLoc.size m,
                               move = true,
                               supports = supports,
                               saves = saves, 
                               force = [r], 
                               registerAllocation = registerAllocation}

                          val edges = computeEdges 
                                      {registerAllocation = registerAllocation}
                        in
                          doit {edges = edges,
                                saves = [],
                                assembly = AppendList.append
                                           (assembly, assembly_register),
                                registerAllocation = registerAllocation}
                        end
                     | NONE => doitCycle {edges = edges,
                                          saves = saves,
                                          assembly = assembly,
                                          registerAllocation = registerAllocation}
                end

            and doitCycle {edges,
                           saves,
                           assembly,
                           registerAllocation = registerAllocation}
              = let
                  val {cycle}
                    = List.fold
                      (edges,
                       {cycle = NONE},
                       fn ((_, edges'), {cycle = NONE})
                        => let
                             val {cycle}
                               = List.fold
                                 (edges',
                                  {cycle = NONE},
                                  fn ((XmmReg _, SOME m, r, XmmReg _), 
                                      {cycle = NONE})
                                   => {cycle = SOME (m, r)}
                                   | (_, {cycle})
                                   => {cycle = cycle})
                           in
                             {cycle = cycle}
                           end
                        | ((_, _), {cycle})
                        => {cycle = cycle})
                in
                  case cycle
                    of SOME (m, r)
                     => let
                          val {assembly = assembly_register,
                               registerAllocation,
                               ...}
                            = toXmmRegisterMemLoc 
                              {memloc = m,
                               info = info,
                               size = MemLoc.size m,
                               move = true,
                               supports = supports,
                               saves = saves, 
                               force = [r], 
                               registerAllocation = registerAllocation}

                          val edges = computeEdges 
                                      {registerAllocation = registerAllocation}
                        in
                          doit {edges = edges,
                                saves = [],
                                assembly = AppendList.append
                                           (assembly, assembly_register),
                                registerAllocation = registerAllocation}
                        end
                     | NONE => doitCycle {edges = edges,
                                          saves = saves,
                                          assembly = assembly,
                                          registerAllocation = registerAllocation}
                end

            and doit {edges,
                      saves,
                      assembly,
                      registerAllocation}
              = let
                  val edges
                    = List.fold
                      (edges,
                       [],
                       fn ((reg, edges'), edges)
                        => let
                             val edges' 
                               = List.revRemoveAll
                                 (edges',
                                  fn (None, _, _, None) => true
                                   | _ => false)
                           in
                             if List.isEmpty edges'
                               then edges
                               else (reg, edges')::edges
                           end)
                in
                  if List.isEmpty edges
                    then {assembly = assembly,
                          registerAllocation = registerAllocation}
                    else doitSelf {edges = edges,
                                   saves = saves,
                                   assembly = assembly,
                                   registerAllocation = registerAllocation}
                end

            val {assembly = assembly_force,
                 registerAllocation}
              = doit {edges = computeEdges {registerAllocation = registerAllocation},
                      saves = [],
                      assembly = AppendList.empty,
                      registerAllocation = registerAllocation}

            val {assembly = assembly_reserve,
                 registerAllocation}
               = xmmreserve {registers = List.revKeepAllMap
                                     (caches, 
                                      fn {register, reserve, ...} 
                                       => if reserve 
                                            then SOME register 
                                            else NONE),
                         registerAllocation = registerAllocation}

          in
            {assembly = AppendList.append(assembly_force, assembly_reserve),
             registerAllocation = registerAllocation}
          end
*)

      fun xmmcache {caches : {register: XmmRegister.t,
                               memloc: MemLoc.t,
                               reserve: bool} list,
                     info,
                     registerAllocation}
        = let
            val supports
              = List.map
                (caches,
                 fn {memloc, ...} => Operand.memloc memloc)

            val {assembly,
                 registerAllocation,
                 ...}
              = List.foldr
                (caches,
                 {assembly = AppendList.empty,
                  registerAllocation = registerAllocation,
                  saves = []},
                 fn ({register,
                      memloc,
                      reserve},
                     {assembly,
                      registerAllocation,
                      saves})
                  => let
                       val {register,
                            assembly = assembly_register,
                            registerAllocation}
                         = toXmmRegisterMemLoc 
                           {memloc = memloc,
                            info = info,
                            size = MemLoc.size memloc,
                            move = true,
                            supports = supports,
                            saves = saves, 
                            force = [register], 
                            registerAllocation = registerAllocation}

                       val {assembly = assembly_reserve,
                            registerAllocation}
                         = if reserve
                             then xmmreserve' {register = register,
                                                registerAllocation = registerAllocation}
                             else {assembly = AppendList.empty,
                                   registerAllocation = registerAllocation}
                     in
                       {assembly = AppendList.appends [assembly,
                                                       assembly_register,
                                                       assembly_reserve],
                        registerAllocation = registerAllocation,
                        saves = (Operand.memloc memloc)::saves}
                     end)
          in
            {assembly = assembly,
             registerAllocation = registerAllocation}
          end

      fun reset ({...}: {registerAllocation: t})
        = {assembly = AppendList.empty,
           registerAllocation = empty ()}

      fun force {commit_memlocs: MemLocSet.t,
                 commit_classes: ClassSet.t,
                 remove_memlocs: MemLocSet.t,
                 remove_classes: ClassSet.t,
                 dead_memlocs: MemLocSet.t,
                 dead_classes: ClassSet.t,
                 info: Liveness.t,
                 registerAllocation: t}
        = let
            val toCommit 
              = fn TRYREMOVE _ => REMOVE 0
                 | REMOVE _ => REMOVE 0
                 | _ => COMMIT 0
            val toRemove
              = fn _ => REMOVE 0

            val shouldCommit 
              = fn memloc => (MemLocSet.contains(commit_memlocs,
                                                 memloc)
                              orelse
                              ClassSet.contains(commit_classes,
                                                MemLoc.class memloc))
            val shouldRemove
              = fn memloc => (MemLocSet.contains(remove_memlocs,
                                                 memloc)
                              orelse
                              ClassSet.contains(remove_classes,
                                                MemLoc.class memloc))
            val shouldDead
              = fn memloc => (MemLocSet.contains(dead_memlocs,
                                                 memloc)
                              orelse
                              ClassSet.contains(dead_classes,
                                                MemLoc.class memloc))

            val registerAllocation
              = xmmvalueMap {map 
                              = fn value as {register,
                                             memloc,
                                             weight,
                                             sync,
                                             commit}
                                  => case (shouldCommit memloc,
                                           shouldRemove memloc,
                                           shouldDead memloc)
                                       of (true,false,false)
                                        => {register = register, 
                                            memloc = memloc, 
                                            weight = weight, 
                                            sync = sync, 
                                            commit = toCommit commit}
                                        | (false,true,false)
                                        => {register = register, 
                                            memloc = memloc, 
                                            weight = weight, 
                                            sync = sync, 
                                            commit = toRemove commit}
                                        | (false,false,true)
                                        => {register = register, 
                                            memloc = memloc, 
                                            weight = weight, 
                                            sync = true, 
                                            commit = toRemove commit}
                                        | (false,false,false)
                                        => if List.exists
                                              (MemLoc.utilized memloc,
                                               fn memloc' => shouldDead memloc')
                                             then {register = register, 
                                                   memloc = memloc, 
                                                   weight = weight, 
                                                   sync = sync, 
                                                   commit = toRemove commit}
                                           else if List.exists
                                                   (MemLoc.utilized memloc,
                                                    fn memloc' => shouldRemove memloc')
                                             then {register = register, 
                                                   memloc = memloc, 
                                                   weight = weight, 
                                                   sync = sync, 
                                                   commit = toCommit commit}
                                             else value
                                        | _ => Error.bug "amd64AllocateRegisters.RegisterAllocation.force",
                          registerAllocation = registerAllocation}

            val {assembly = assembly_commit_xmmregisters,
                 registerAllocation, 
                 ...}
              = commitXmmRegisters {info = info,
                                     supports = [],
                                     saves = [],
                                     registerAllocation 
                                     = registerAllocation}

            val registerAllocation
              = valueMap {map 
                          = fn value as {register,
                                         memloc,
                                         weight,
                                         sync,
                                         commit}
                             => case (shouldCommit memloc,
                                      shouldRemove memloc,
                                      shouldDead memloc)
                                  of (true,false,false)
                                   => {register = register, 
                                       memloc = memloc, 
                                       weight = weight, 
                                       sync = sync, 
                                       commit = toCommit commit}
                                   | (false,true,false)
                                   => {register = register, 
                                       memloc = memloc, 
                                       weight = weight, 
                                       sync = sync, 
                                       commit = toRemove commit}
                                   | (false,false,true)
                                   => value
                                   | (false,false,false)
                                   => if List.exists
                                         (MemLoc.utilized memloc,
                                          fn memloc' => shouldDead memloc')
                                        then {register = register, 
                                              memloc = memloc, 
                                              weight = weight, 
                                              sync = sync, 
                                              commit = toRemove commit}
                                      else if List.exists
                                              (MemLoc.utilized memloc,
                                               fn memloc' => shouldRemove memloc')
                                        then {register = register, 
                                              memloc = memloc, 
                                              weight = weight, 
                                              sync = sync, 
                                              commit = toCommit commit}
                                        else value
                                   | _ => Error.bug "amd64AllocateRegisters.RegisterAllocation.force",
                          registerAllocation = registerAllocation}

            val {assembly = assembly_commit_registers,
                 registerAllocation}
              = commitRegisters {info = info,
                                 supports = [],
                                 saves = [],
                                 registerAllocation 
                                 = registerAllocation}

            val registerAllocation
              = valueMap {map 
                          = fn value as {register,
                                         memloc,
                                         weight,
                                         commit,
                                         ...}
                             => if shouldDead memloc
                                  then {register = register, 
                                        memloc = memloc, 
                                        weight = weight, 
                                        sync = true, 
                                        commit = toRemove commit}
                                  else value,
                          registerAllocation = registerAllocation}

            val {assembly = assembly_dead_registers,
                 registerAllocation}
              = commitRegisters {info = info,
                                 supports = [],
                                 saves = [],
                                 registerAllocation 
                                 = registerAllocation}
          in
            {assembly = AppendList.appends
                        [assembly_commit_xmmregisters,
                         assembly_commit_registers,
                         assembly_dead_registers],
             registerAllocation = registerAllocation}
          end

      fun ccall {info: Liveness.t,
                 registerAllocation: t}
        = let
            val cargClasses = !amd64MLton.Classes.cargClasses
            val cstaticClasses = !amd64MLton.Classes.cstaticClasses

            val {reserved = reservedStart, 
                 xmmreserved = xmmreservedStart, ...} = registerAllocation

            val {assembly = assembly_xmmreserve,
                 registerAllocation}
              = List.fold
                (XmmRegister.callerSaveRegisters,
                 {assembly = AppendList.empty, 
                  registerAllocation = registerAllocation},
                 fn (register, {assembly, registerAllocation})
                  => let
                       val {assembly = assembly_reserve,
                            registerAllocation}
                         = xmmreserve' {register = register,
                                         registerAllocation = registerAllocation}
                     in
                       {assembly = AppendList.append (assembly,
                                                      assembly_reserve),
                        registerAllocation = registerAllocation}
                     end)

            val {assembly = assembly_reserve,
                 registerAllocation}
              = List.fold
                (Register.callerSaveRegisters,
                 {assembly = AppendList.empty, 
                  registerAllocation = registerAllocation},
                 fn (register, {assembly, registerAllocation})
                  => let
                       val {assembly = assembly_reserve,
                            registerAllocation}
                         = reserve' {register = register,
                                     registerAllocation = registerAllocation}
                     in
                       {assembly = AppendList.append (assembly,
                                                      assembly_reserve),
                        registerAllocation = registerAllocation}
                     end)

            val availCalleeSaveXmmRegisters =
               List.keepAll
               (XmmRegister.calleeSaveRegisters,
                fn calleeSaveReg =>
                List.forall
                (#xmmreserved registerAllocation,
                 fn reservedReg =>
                 not (XmmRegister.coincide (reservedReg, calleeSaveReg))))

            val {assembly = assembly_xmmshuffle,
                 registerAllocation, ...}
              = if !Control.Native.shuffle then 
                List.fold
                (xmmvalueFilter {filter = fn {register, memloc, ...}
                                        => (List.contains
                                            (XmmRegister.callerSaveRegisters,
                                             register,
                                             XmmRegister.eq)
                                            andalso
                                            (not o ClassSet.contains)
                                            (cargClasses,
                                             MemLoc.class memloc))
                                           andalso
                                           List.exists
                                           (availCalleeSaveXmmRegisters,
                                            fn calleeSaveReg =>
                                            Size.eq (XmmRegister.size register,
                                                     XmmRegister.size calleeSaveReg)),
                              registerAllocation = registerAllocation},
                 {assembly = AppendList.empty, 
                  registerAllocation = registerAllocation},
                 fn ({memloc, ...}, {assembly, registerAllocation})
                  => let
                       val {assembly = assembly_shuffle,
                            registerAllocation, ...} 
                         = allocateXmmOperand {operand = Operand.memloc memloc,
                                            options = {xmmregister = true,
                                                       address = true},
                                            info = info,
                                            size = MemLoc.size memloc,
                                            move = true,
                                            supports = [],
                                            saves = [],
                                            force = XmmRegister.calleeSaveRegisters,
                                            registerAllocation
                                            = registerAllocation}
                     in
                       {assembly = AppendList.append (assembly,
                                                      assembly_shuffle),
                        registerAllocation = registerAllocation}
                     end)
                else {assembly = AppendList.empty,
                      registerAllocation = registerAllocation}

            val availCalleeSaveRegisters =
               List.keepAll
               (Register.calleeSaveRegisters,
                fn calleeSaveReg =>
                List.forall
                (#reserved registerAllocation,
                 fn reservedReg =>
                 not (Register.coincide (reservedReg, calleeSaveReg))))

            val {assembly = assembly_shuffle,
                 registerAllocation, ...}
              = if !Control.Native.shuffle then 
                List.fold
                (valueFilter {filter = fn {register, memloc, ...}
                                        => (List.contains
                                            (Register.callerSaveRegisters,
                                             register,
                                             Register.eq)
                                            andalso
                                            (not o ClassSet.contains)
                                            (cargClasses,
                                             MemLoc.class memloc))
                                           andalso
                                           List.exists
                                           (availCalleeSaveRegisters,
                                            fn calleeSaveReg =>
                                            Size.eq (Register.size register,
                                                     Register.size calleeSaveReg)),
                              registerAllocation = registerAllocation},
                 {assembly = AppendList.empty, 
                  registerAllocation = registerAllocation},
                 fn ({memloc, ...}, {assembly, registerAllocation})
                  => let
                       val {assembly = assembly_shuffle,
                            registerAllocation, ...} 
                         = allocateOperand {operand = Operand.memloc memloc,
                                            options = {register = true,
                                                       immediate = NONE,
                                                       label = false,
                                                       address = true},
                                            info = info,
                                            size = MemLoc.size memloc,
                                            move = true,
                                            supports = [],
                                            saves = [],
                                            force = Register.calleeSaveRegisters,
                                            registerAllocation
                                            = registerAllocation}
                     in
                       {assembly = AppendList.append (assembly,
                                                      assembly_shuffle),
                        registerAllocation = registerAllocation}
                     end)
                else {assembly = AppendList.empty,
                      registerAllocation = registerAllocation}

            val registerAllocation
              = xmmvalueMap {map = fn value as {register,
                                                 memloc,
                                                 weight,
                                                 sync,
                                                 ...}
                                     => if (List.contains
                                            (XmmRegister.callerSaveRegisters,
                                             register,
                                             XmmRegister.eq)
                                            andalso
                                            (not o ClassSet.contains)
                                            (cargClasses,
                                             MemLoc.class memloc))
                                           orelse
                                           ClassSet.contains
                                           (cstaticClasses,
                                            MemLoc.class memloc)
                                          then {register = register, 
                                                memloc = memloc, 
                                                weight = weight, 
                                                sync = sync, 
                                                commit = REMOVE 0}
                                          else value,
                               registerAllocation = registerAllocation}


            val registerAllocation
              = valueMap {map = fn value as {register,
                                             memloc,
                                             weight,
                                             sync,
                                             ...}
                                 => if (List.contains
                                        (Register.callerSaveRegisters,
                                         register,
                                         Register.eq)
                                        andalso
                                        (not o ClassSet.contains)
                                        (cargClasses,
                                         MemLoc.class memloc))
                                       orelse
                                       ClassSet.contains
                                       (cstaticClasses,
                                        MemLoc.class memloc)
                                      then {register = register, 
                                            memloc = memloc, 
                                            weight = weight, 
                                            sync = sync, 
                                            commit = REMOVE 0}
                                      else value,
                          registerAllocation = registerAllocation}

            val {assembly = assembly_commit_xmmregisters,
                 registerAllocation, ...}
              = commitXmmRegisters {info = info,
                                     supports = [],
                                     saves = [],
                                     registerAllocation = registerAllocation}

            val {assembly = assembly_commit_registers,
                 registerAllocation}
              = commitRegisters {info = info,
                                 supports = [],
                                 saves = [],
                                 registerAllocation = registerAllocation}

            val {assembly = assembly_xmmunreserve,
                 registerAllocation}
              = List.fold
                (List.removeAll
                 (XmmRegister.callerSaveRegisters,
                  fn register => List.contains(xmmreservedStart, register, XmmRegister.eq)),
                 {assembly = AppendList.empty, 
                  registerAllocation = registerAllocation},
                 fn (register, {assembly, registerAllocation})
                  => let
                       val {assembly = assembly_unreserve,
                            registerAllocation}
                         = xmmunreserve' {register = register,
                                           registerAllocation = registerAllocation}
                     in
                       {assembly = AppendList.append (assembly,
                                                      assembly_unreserve),
                        registerAllocation = registerAllocation}
                     end)

            val {assembly = assembly_unreserve,
                 registerAllocation}
              = List.fold
                (List.removeAll
                 (Register.callerSaveRegisters,
                  fn register => List.contains(reservedStart, register, Register.eq)),
                 {assembly = AppendList.empty, 
                  registerAllocation = registerAllocation},
                 fn (register, {assembly, registerAllocation})
                  => let
                       val {assembly = assembly_unreserve,
                            registerAllocation}
                         = unreserve' {register = register,
                                       registerAllocation = registerAllocation}
                     in
                       {assembly = AppendList.append (assembly,
                                                      assembly_unreserve),
                        registerAllocation = registerAllocation}
                     end)

            val registerAllocation
              = xmmdeletes {registers = XmmRegister.callerSaveRegisters,
                             registerAllocation = registerAllocation}

            val registerAllocation
              = deletes {registers = Register.callerSaveRegisters,
                         registerAllocation = registerAllocation}
          in
            {assembly = AppendList.appends
                        [assembly_xmmreserve,
                         assembly_reserve,
                         assembly_xmmshuffle,
                         assembly_shuffle,
                         assembly_commit_xmmregisters,
                         assembly_commit_registers,
                         assembly_xmmunreserve,
                         assembly_unreserve],
             registerAllocation = registerAllocation}
          end

      fun return {returns: {src: Operand.t, dst: MemLoc.t} list,
                  info: Liveness.t,
                  registerAllocation: t} =
         let
            val killed_values =
               valueFilter {filter = fn {memloc, ...} =>
                            List.exists
                            (returns, fn {dst = return_memloc, ...} =>
                             List.exists(MemLoc.utilized memloc,
                                         fn memloc' =>
                                         MemLoc.eq(memloc', return_memloc))
                             orelse
                             MemLoc.mayAlias(return_memloc, memloc)),
                            registerAllocation = registerAllocation}
            val killed_memlocs = List.revMap(killed_values, #memloc)

            val registerAllocation =
               removes {memlocs = killed_memlocs,
                        registerAllocation = registerAllocation}
            val registerAllocation =
               xmmremoves {memlocs = killed_memlocs,
                           registerAllocation = registerAllocation}

            val registerAllocation =
               List.fold
               (returns, registerAllocation, fn ({src = operand, 
                                                  dst = return_memloc}, registerAllocation) =>
                case operand of
                   Operand.Register return_register =>
                      update {value = {register = return_register,
                                       memloc = return_memloc,
                                       weight = 1024,
                                       sync = false,
                                       commit = NO},
                              registerAllocation = registerAllocation}
                 | Operand.XmmRegister return_register => 
                      xmmupdate {value = {register = return_register,
                                           memloc = return_memloc,
                                           weight = 1024,
                                           sync = false,
                                           commit = NO},
                                  registerAllocation = registerAllocation}
                 | _ => Error.bug "amd64AllocateRegisters.RegisterAllocation.return")

            val (final_defs, defs) =
               List.fold
               (returns, ([],[]), fn ({src,dst},(final_defs,defs)) =>
                (src::final_defs,(Operand.memloc dst)::defs))
            val {assembly = assembly_post,
                 registerAllocation}
              = post {uses = [],
                      final_uses = [],
                      defs = defs,
                      final_defs = final_defs,
                      kills = [],
                      info = info,
                      registerAllocation = registerAllocation}
          in
            {assembly = assembly_post,
             registerAllocation = registerAllocation}
          end

(*
      fun return {memloc = return_memloc,
                  info: Liveness.t,
                  registerAllocation: t}
        = let
            val killed_values
              = valueFilter {filter = fn value as {memloc,...}
                                       => List.exists
                                          (MemLoc.utilized memloc,
                                           fn memloc'
                                            => MemLoc.eq(memloc',
                                                         return_memloc))
                                          orelse
                                          MemLoc.mayAlias(return_memloc,
                                                          memloc),
                             registerAllocation = registerAllocation}
            val killed_memlocs = List.revMap(killed_values, #memloc)

            val registerAllocation
              = removes {memlocs = killed_memlocs,
                         registerAllocation = registerAllocation}

            val return_register = Register.return (MemLoc.size return_memloc)
            val registerAllocation
              = update
                {value = {register = return_register,
                          memloc = return_memloc,
                          weight = 1024,
                          sync = false,
                          commit = NO},
                 registerAllocation = registerAllocation}

            val {assembly = assembly_post,
                 registerAllocation}
              = post {uses = [],
                      final_uses = [],
                      defs = [Operand.memloc return_memloc],
                      final_defs = [Operand.register return_register],
                      kills = [],
                      info = info,
                      registerAllocation = registerAllocation}
          in
            {assembly = assembly_post,
             registerAllocation = registerAllocation}
          end

      fun fltreturn {memloc = return_memloc,
                     info: Liveness.t,
                     registerAllocation: t}
        = let
            val return_register = FltRegister.return

            val {fltrename = fltrename_push,
                 registerAllocation}
              = fltpush
                {value = {fltregister = return_register,
                          memloc = return_memloc,
                          weight = 1024,
                          sync = false,
                          commit = NO},
                 registerAllocation = registerAllocation}

            val {assembly = assembly_post,
                 registerAllocation}
              = post {uses = [],
                      final_uses = [],
                      defs = [Operand.memloc return_memloc],
                      final_defs = [Operand.fltregister return_register],
                      kills = [],
                      info = info,
                      registerAllocation = registerAllocation}

          in
            {assembly = assembly_post,
             registerAllocation = registerAllocation}
          end
*)

      fun saveregalloc ({id, registerAllocation, ...}: 
                        {live: MemLocSet.t,
                         id: Directive.Id.t,
                         info: Liveness.t,
                         registerAllocation: t})
        = let
            val _ = setRA(id, {registerAllocation = registerAllocation})
          in
            {assembly = if !Control.Native.commented > 2
                          then (toComments registerAllocation)
                          else AppendList.empty,
             registerAllocation = registerAllocation}
          end

      fun restoreregalloc ({live, id, info, ...}: 
                           {live: MemLocSet.t,
                            id: Directive.Id.t,
                            info: Liveness.t,
                            registerAllocation: t})
        = let
            val {registerAllocation} = getRA id

            fun dump memloc
              = (track memloc) andalso 
                not (MemLocSet.contains(live,memloc))

            val registerAllocation
              = xmmvalueMap
                {map = fn value as {register,
                                    memloc,
                                    weight,
                                    sync,
                                    ...}
                        => if dump memloc
                             then {register = register,
                                   memloc = memloc,
                                   weight = weight,
                                   sync = true,
                                   commit = TRYREMOVE 0}
                           else if List.exists(MemLoc.utilized memloc, dump)
                              then {register = register,
                                    memloc = memloc,
                                    weight = weight,
                                    sync = sync,
                                    commit = TRYREMOVE 0}
                           else value,
                 registerAllocation = registerAllocation}

            val {assembly = assembly_commit_xmmregisters,
                 registerAllocation,
                 ...}
              = commitXmmRegisters {info = info,
                                     supports = [],
                                     saves = [],
                                     registerAllocation = registerAllocation}

            val registerAllocation
              = valueMap
                {map = fn value as {register,
                                    memloc,
                                    weight,
                                    sync,
                                    ...}
                        => if dump memloc
                             then {register = register,
                                   memloc = memloc,
                                   weight = weight,
                                   sync = true,
                                   commit = TRYREMOVE 0}
                           else if List.exists(MemLoc.utilized memloc, dump)
                              then {register = register,
                                    memloc = memloc,
                                    weight = weight,
                                    sync = sync,
                                    commit = TRYREMOVE 0}
                           else value,
                 registerAllocation = registerAllocation}

            val {assembly = assembly_commit_registers,
                 registerAllocation,
                 ...}
              = commitRegisters {info = info,
                                 supports = [],
                                 saves = [],
                                 registerAllocation = registerAllocation}
          in
            {assembly = AppendList.append (assembly_commit_xmmregisters,
                                           assembly_commit_registers),
             registerAllocation = registerAllocation}
          end
    end

  structure Instruction =
    struct
      structure RA = RegisterAllocation
      open Instruction

      (*
       * Require src/dst operands as follows:
       *
       *              dst
       *          reg imm lab add 
       *      reg  X           X
       *  src imm  X           X
       *      lab
       *      add  X
       *)
      fun allocateSrcDst {src: Operand.t,
                          dst: Operand.t,
                          move_dst: bool,
                          size: Size.t,
                          info as {dead, remove, ...}: Liveness.t,
                          registerAllocation: RegisterAllocation.t}
        = if Operand.eq(src, dst)
            then let
                   val {operand = final_src_dst, 
                        assembly = assembly_src_dst,
                        registerAllocation}
                     = RA.allocateOperand 
                       {operand = src,
                        options = {register = true,
                                   immediate = NONE,
                                   label = false,
                                   address = false},
                        info = info,
                        size = size,
                        move = true,
                        supports = [],
                        saves = [],
                        force = [],
                        registerAllocation 
                        = registerAllocation}
                 in
                   {final_src = final_src_dst,
                    final_dst = final_src_dst,
                    assembly_src_dst = assembly_src_dst,
                    registerAllocation = registerAllocation}
                 end
            else case (src, dst)
                   of (Operand.MemLoc _,
                       Operand.MemLoc memloc_dst)
                    => if MemLocSet.contains(dead,
                                             memloc_dst)
                          orelse
                          MemLocSet.contains(remove,
                                             memloc_dst)
                         then let
                                val {operand = final_dst,
                                     assembly = assembly_dst,
                                     registerAllocation}
                                  = RA.allocateOperand 
                                    {operand = dst,
                                     options = {register = true,
                                                immediate = NONE,
                                                label = false,
                                                address = true},
                                     info = info,
                                     size = size,
                                     move = move_dst,
                                     supports = [src],
                                     saves = [],
                                     force = [],
                                     registerAllocation 
                                     = registerAllocation}

                                val options_src
                                  = case final_dst
                                      of Operand.Register _
                                       => {register = true,
                                           immediate = NONE,
                                           label = false,
                                           address = true}
                                       | _ 
                                       => {register = true,
                                           immediate = NONE,
                                           label = false,
                                           address = false}

                                val {operand = final_src, 
                                     assembly = assembly_src,
                                     registerAllocation}
                                  = RA.allocateOperand 
                                    {operand = src,
                                     options = options_src,
                                     info = info,
                                     size = size,
                                     move = true,
                                     supports = [],
                                     saves = [dst,final_dst],
                                     force = [],
                                     registerAllocation 
                                     = registerAllocation}
                              in
                                {final_src = final_src,
                                 final_dst = final_dst,
                                 assembly_src_dst 
                                 = AppendList.appends 
                                   [assembly_dst,
                                    assembly_src],
                                 registerAllocation = registerAllocation}
                              end
                         else let
                                val {operand = final_src, 
                                     assembly = assembly_src,
                                     registerAllocation}
                                  = RA.allocateOperand 
                                    {operand = src,
                                     options = {register = true,
                                                immediate = NONE,
                                                label = false,
                                                address = true},
                                     info = info,
                                     size = size,
                                     move = true,
                                     supports = [dst],
                                     saves = [],
                                     force = [],
                                     registerAllocation 
                                     = registerAllocation}

                                val {operand = final_dst,
                                     assembly = assembly_dst,
                                     registerAllocation}
                                  = RA.allocateOperand 
                                    {operand = dst,
                                     options = {register = true,
                                                immediate = NONE,
                                                label = false,
                                                address = false},
                                     info = info,
                                     size = size,
                                     move = move_dst,
                                     supports = [],
                                     saves = [src,final_src],
                                     force = [],
                                     registerAllocation 
                                     = registerAllocation}
                              in
                                {final_src = final_src,
                                 final_dst = final_dst,
                                 assembly_src_dst 
                                 = AppendList.appends 
                                   [assembly_src, 
                                    assembly_dst],
                                 registerAllocation = registerAllocation}
                              end
                    | (_,
                       Operand.MemLoc memloc_dst)
                    => let
                         val {operand = final_src, 
                              assembly = assembly_src,
                              registerAllocation}
                           = RA.allocateOperand 
                             {operand = src,
                              options = {register = true,
                                         immediate = SOME WordSize.word32,
                                         label = false,
                                         address = false},
                              info = info,
                              size = size,
                              move = true,
                              supports = [dst],
                              saves = [],
                              force = [],
                              registerAllocation 
                              = registerAllocation}

                         fun default ()
                           = RA.allocateOperand 
                             {operand = dst,
                              options = {register = true,
                                         immediate = NONE,
                                         label = false,
                                         address = true},
                              info = info,
                              size = size,
                              move = move_dst,
                              supports = [],
                              saves = [src,final_src],
                              force = [],
                              registerAllocation 
                              = registerAllocation}

                         val {operand = final_dst,
                              assembly = assembly_dst,
                              registerAllocation}
                           = if MemLocSet.contains(dead,
                                                   memloc_dst)
                                orelse
                                MemLocSet.contains(remove,
                                                   memloc_dst)
                               then case RA.allocated 
                                         {memloc = memloc_dst,
                                          registerAllocation = registerAllocation}
                                      of SOME {register, sync, ...}
                                       => if sync
                                            then let
                                                   val registerAllocation
                                                     = RA.delete
                                                       {register = register,
                                                        registerAllocation 
                                                        = registerAllocation}
                                                 in
                                                   RA.allocateOperand 
                                                   {operand = dst,
                                                    options = {register = false,
                                                               immediate = NONE,
                                                               label = false,
                                                               address = true},
                                                    info = info,
                                                    size = size,
                                                    move = move_dst,
                                                    supports = [],
                                                    saves = [src,final_src],
                                                    force = [],
                                                    registerAllocation 
                                                    = registerAllocation}
                                                 end
                                            else default ()
                                       | NONE => default ()
                               else default ()
                       in
                         {final_src = final_src,
                          final_dst = final_dst,
                          assembly_src_dst 
                          = AppendList.appends 
                            [assembly_src, 
                             assembly_dst],
                          registerAllocation = registerAllocation}
                       end
                    | _ => Error.bug "amd64AllocateRegisters.Instruction.allocateSrcDst"

      (* 
       * Require src1/src2 operands as follows:
       *
       *               src2
       *           reg imm lab add 
       *       reg  X   X       X
       *  src1 imm
       *       lab
       *       add  X   X
       *)
      fun allocateSrc1Src2 {src1: Operand.t,
                            src2: Operand.t,
                            size: Size.t,
                            info: Liveness.t,
                            registerAllocation: RegisterAllocation.t}
        = if Operand.eq(src1, src2)
            then let
                   val {operand = final_src1_src2, 
                        assembly = assembly_src1_src2,
                        registerAllocation}
                     = RA.allocateOperand 
                       {operand = src1,
                        options = {register = true,
                                   immediate = NONE,
                                   label = false,
                                   address = false},
                        info = info,
                        size = size,
                        move = true,
                        supports = [],
                        saves = [],
                        force = [],
                        registerAllocation 
                        = registerAllocation}
                 in
                   {final_src1 = final_src1_src2,
                    final_src2 = final_src1_src2,
                    assembly_src1_src2 = assembly_src1_src2,
                    registerAllocation = registerAllocation}
                 end
            else let
                   val {operand = final_src1,
                        assembly = assembly_src1,
                        registerAllocation}
                     = RA.allocateOperand 
                       {operand = src1,
                        options = {register = true,
                                   immediate = NONE,
                                   label = false,
                                   address = true},
                        info = info,
                        size = size,
                        move = true,
                        supports = [src2],
                        saves = [],
                        force = [],
                        registerAllocation 
                        = registerAllocation}

                   val options_src2
                     = case final_src1
                         of Operand.Register _
                          => {register = true,
                              immediate = SOME WordSize.word32,
                              label = false,
                              address = true}
                          | _ 
                          => {register = true,
                              immediate = SOME WordSize.word32,
                              label = false,
                              address = false}

                   val {operand = final_src2, 
                        assembly = assembly_src2,
                        registerAllocation}
                     = RA.allocateOperand 
                       {operand = src2,
                        options = options_src2,
                        info = info,
                        size = size,
                        move = true,
                        supports = [],
                        saves = [src1,final_src1],
                        force = [],
                        registerAllocation 
                        = registerAllocation}
                 in
                   {final_src1 = final_src1,
                    final_src2 = final_src2,
                    assembly_src1_src2 
                    = AppendList.appends 
                      [assembly_src1, 
                       assembly_src2],
                    registerAllocation = registerAllocation}
                 end

      (*
       * Require src/dst operands as follows:
       *
       *              dst
       *          reg xmm imm lab add 
       *      reg
       *      xmm      X
       *  src imm
       *      lab
       *      add      ?
       *)
      fun allocateXmmSrcDstAux {src: Operand.t,
                                address_src: bool,
                                dst: Operand.t,
                                move_dst: bool,
                                size: Size.t,
                                info as {dead, remove, ...}: Liveness.t,
                                registerAllocation: RegisterAllocation.t}
        = if Operand.eq(src, dst)
            then let
                   val {operand = final_src_dst, 
                        assembly = assembly_src_dst,
                        registerAllocation}
                     = RA.allocateXmmOperand 
                       {operand = src,
                        options = {xmmregister = true,
                                   address = false},
                        info = info,
                        size = size,
                        move = true,
                        supports = [],
                        saves = [],
                        force = [],
                        registerAllocation 
                        = registerAllocation}
                 in
                   {final_src = final_src_dst,
                    final_dst = final_src_dst,
                    assembly_src_dst = assembly_src_dst,
                    registerAllocation = registerAllocation}
                 end
            else case (src, dst)
                   of (Operand.MemLoc _,
                       Operand.MemLoc memloc_dst)
                    => if MemLocSet.contains(dead,
                                             memloc_dst)
                          orelse
                          MemLocSet.contains(remove,
                                             memloc_dst)
                         then let
                                val {operand = final_dst,
                                     assembly = assembly_dst,
                                     registerAllocation}
                                  = RA.allocateXmmOperand 
                                    {operand = dst,
                                     options = {xmmregister = true,
                                                address = false},
                                     info = info,
                                     size = size,
                                     move = move_dst,
                                     supports = [src],
                                     saves = [],
                                     force = [],
                                     registerAllocation 
                                     = registerAllocation}

                                val {operand = final_src, 
                                     assembly = assembly_src,
                                     registerAllocation}
                                  = RA.allocateXmmOperand 
                                    {operand = src,
                                     options = {xmmregister = true,
                                                address = address_src},
                                     info = info,
                                     size = size,
                                     move = true,
                                     supports = [],
                                     saves = [dst,final_dst],
                                     force = [],
                                     registerAllocation 
                                     = registerAllocation}
                              in
                                {final_src = final_src,
                                 final_dst = final_dst,
                                 assembly_src_dst 
                                 = AppendList.appends 
                                   [assembly_dst,
                                    assembly_src],
                                 registerAllocation = registerAllocation}
                              end
                         else let
                                val {operand = final_src, 
                                     assembly = assembly_src,
                                     registerAllocation}
                                  = RA.allocateXmmOperand 
                                    {operand = src,
                                     options = {xmmregister = true,
                                                address = address_src},
                                     info = info,
                                     size = size,
                                     move = true,
                                     supports = [dst],
                                     saves = [],
                                     force = [],
                                     registerAllocation 
                                     = registerAllocation}

                                val {operand = final_dst,
                                     assembly = assembly_dst,
                                     registerAllocation}
                                  = RA.allocateXmmOperand 
                                    {operand = dst,
                                     options = {xmmregister = true,
                                                address = false},
                                     info = info,
                                     size = size,
                                     move = move_dst,
                                     supports = [],
                                     saves = [src,final_src],
                                     force = [],
                                     registerAllocation 
                                     = registerAllocation}
                              in
                                {final_src = final_src,
                                 final_dst = final_dst,
                                 assembly_src_dst 
                                 = AppendList.appends 
                                   [assembly_src, 
                                    assembly_dst],
                                 registerAllocation = registerAllocation}
                              end
                    | (_,
                       Operand.MemLoc memloc_dst)
                    => let
                         val {operand = final_src, 
                              assembly = assembly_src,
                              registerAllocation}
                           = RA.allocateXmmOperand 
                             {operand = src,
                              options = {xmmregister = true,
                                         address = false},
                              info = info,
                              size = size,
                              move = true,
                              supports = [dst],
                              saves = [],
                              force = [],
                              registerAllocation 
                              = registerAllocation}

                         fun default ()
                           = RA.allocateXmmOperand 
                             {operand = dst,
                              options = {xmmregister = true,
                                         address = false},
                              info = info,
                              size = size,
                              move = move_dst,
                              supports = [],
                              saves = [src,final_src],
                              force = [],
                              registerAllocation 
                              = registerAllocation}

                         val {operand = final_dst,
                              assembly = assembly_dst,
                              registerAllocation}
                           = if MemLocSet.contains(dead,
                                                   memloc_dst)
                                orelse
                                MemLocSet.contains(remove,
                                                   memloc_dst)
                               then case RA.xmmallocated 
                                         {memloc = memloc_dst,
                                          registerAllocation = registerAllocation}
                                      of SOME {register, sync, ...}
                                       => if sync
                                            then let
                                                   val registerAllocation
                                                     = RA.xmmdelete
                                                       {register = register,
                                                        registerAllocation 
                                                        = registerAllocation}
                                                 in
                                                    RA.allocateXmmOperand 
                                                    {operand = dst,
                                                     options = {xmmregister = true,
                                                                address = false},
                                                     info = info,
                                                     size = size,
                                                     move = move_dst,
                                                     supports = [],
                                                     saves = [src,final_src],
                                                     force = [],
                                                     registerAllocation 
                                                     = registerAllocation}
                                                 end
                                            else default ()
                                       | NONE => default ()
                               else default ()
                       in
                         {final_src = final_src,
                          final_dst = final_dst,
                          assembly_src_dst 
                          = AppendList.appends 
                            [assembly_src, 
                             assembly_dst],
                          registerAllocation = registerAllocation}
                       end
                    | _ => Error.bug "amd64AllocateRegisters.Instruction.allocateXmmSrcDstAux"

      (*
       * Require src/dst operands as follows:
       *
       *              dst
       *          reg xmm imm lab add 
       *      reg
       *      xmm      X
       *  src imm
       *      lab
       *      add      X
       *)
      fun allocateXmmSrcDst {src: Operand.t,
                             dst: Operand.t,
                             move_dst: bool,
                             size: Size.t,
                             info: Liveness.t,
                             registerAllocation: RegisterAllocation.t}
         = allocateXmmSrcDstAux {src = src,
                                 address_src = true,
                                 dst = dst,
                                 move_dst = move_dst,
                                 size = size,
                                 info = info,
                                 registerAllocation = registerAllocation}

      (*
       * Require src/dst operands as follows:
       *
       *              dst
       *          reg xmm imm lab add 
       *      reg
       *      xmm      X
       *  src imm
       *      lab
       *      add
       *)
      fun allocateXmmSrcDstReg {src: Operand.t,
                                dst: Operand.t,
                                move_dst: bool,
                                size: Size.t,
                                info: Liveness.t,
                                registerAllocation: RegisterAllocation.t}
         = allocateXmmSrcDstAux {src = src,
                                 address_src = false,
                                 dst = dst,
                                 move_dst = move_dst,
                                 size = size,
                                 info = info,
                                 registerAllocation = registerAllocation}

      (* 
       * Require src1/src2 operands as follows:
       *
       *               src2
       *           reg xmm imm lab add 
       *       reg
       *       xmm      X
       *  src1 imm
       *       lab
       *       add      X
       *)
      fun allocateXmmSrc1Src2 {src1: Operand.t,
                               src2: Operand.t,
                               size: Size.t,
                               info: Liveness.t,
                               registerAllocation: RegisterAllocation.t}
        = if Operand.eq(src1, src2)
            then let
                   val {operand = final_src1_src2, 
                        assembly = assembly_src1_src2,
                        registerAllocation}
                     = RA.allocateXmmOperand 
                       {operand = src1,
                        options = {xmmregister = true,
                                   address = false},
                        info = info,
                        size = size,
                        move = true,
                        supports = [],
                        saves = [],
                        force = [],
                        registerAllocation 
                        = registerAllocation}
                 in
                   {final_src1 = final_src1_src2,
                    final_src2 = final_src1_src2,
                    assembly_src1_src2 = assembly_src1_src2,
                    registerAllocation = registerAllocation}
                 end
            else let
                   val {operand = final_src1,
                        assembly = assembly_src1,
                        registerAllocation}
                     = RA.allocateXmmOperand 
                       {operand = src1,
                        options = {xmmregister = true,
                                   address = true},
                        info = info,
                        size = size,
                        move = true,
                        supports = [src2],
                        saves = [],
                        force = [],
                        registerAllocation 
                        = registerAllocation}

                   val {operand = final_src2, 
                        assembly = assembly_src2,
                        registerAllocation}
                     = RA.allocateXmmOperand 
                       {operand = src2,
                        options = {xmmregister = true,
                                   address = false},
                        info = info,
                        size = size,
                        move = true,
                        supports = [],
                        saves = [src1,final_src1],
                        force = [],
                        registerAllocation 
                        = registerAllocation}
                 in
                   {final_src1 = final_src1,
                    final_src2 = final_src2,
                    assembly_src1_src2 
                    = AppendList.appends 
                      [assembly_src1, 
                       assembly_src2],
                    registerAllocation = registerAllocation}
                 end


      fun allocateRegisters {instruction: t,
                             info as {dead, remove, ...}: Liveness.t,
                             registerAllocation: RegisterAllocation.t}
        = case instruction
            of NOP
               (* No operation *)
             => let
                  val {uses,defs,kills} 
                    = Instruction.uses_defs_kills instruction
                  val {assembly = assembly_pre,
                       registerAllocation}
                    = RA.pre {uses = uses,
                              defs = defs,
                              kills = kills,
                              info = info,
                              registerAllocation = registerAllocation}

                  val instruction 
                    = Instruction.NOP

                  val {uses = final_uses,
                       defs = final_defs,
                       ...}
                    = Instruction.uses_defs_kills instruction

                  val {assembly = assembly_post,
                       registerAllocation}
                    = RA.post {uses = uses,
                               final_uses = final_uses,
                               defs = defs,
                               final_defs = final_defs,
                               kills = kills,
                               info = info,
                               registerAllocation = registerAllocation}
                in
                  {assembly
                   = AppendList.appends 
                     [assembly_pre,
                      AppendList.single (Assembly.instruction instruction),
                      assembly_post],
                   registerAllocation = registerAllocation}
                end
             | HLT
               (* Halt *)
             => let
                  val {uses,defs,kills} 
                    = Instruction.uses_defs_kills instruction
                  val {assembly = assembly_pre,
                       registerAllocation}
                    = RA.pre {uses = uses,
                              defs = defs,
                              kills = kills,
                              info = info,
                              registerAllocation = registerAllocation}

                  val instruction 
                    = Instruction.HLT

                  val {uses = final_uses,
                       defs = final_defs,
                       ...}
                    = Instruction.uses_defs_kills instruction

                  val {assembly = assembly_post,
                       registerAllocation}
                    = RA.post {uses = uses,
                               final_uses = final_uses,
                               defs = defs,
                               final_defs = final_defs,
                               kills = kills,
                               info = info,
                               registerAllocation = registerAllocation}
                in
                  {assembly
                   = AppendList.appends 
                     [assembly_pre,
                      AppendList.single (Assembly.instruction instruction),
                      assembly_post],
                   registerAllocation = registerAllocation}
                end
             | BinAL {oper, src, dst, size}
               (* Integer binary arithmetic(w/o mult & div)/logic instructions.
                * Require src/dst operands as follows:
                *
                *              dst
                *          reg imm lab add 
                *      reg  X           X
                *  src imm  X           X
                *      lab
                *      add  X
                *)
             => let
                  val {uses,defs,kills} 
                    = Instruction.uses_defs_kills instruction
                  val {assembly = assembly_pre,
                       registerAllocation}
                    = RA.pre {uses = uses,
                              defs = defs,
                              kills = kills,
                              info = info,
                              registerAllocation = registerAllocation}

                  fun default ()
                    = let
                        val {final_src,
                             final_dst,
                             assembly_src_dst,
                             registerAllocation}
                          = allocateSrcDst {src = src,
                                            dst = dst,
                                            move_dst = true,
                                            size = size,
                                            info = info,
                                            registerAllocation = registerAllocation}

                        val instruction 
                          = Instruction.BinAL
                            {oper = oper,
                             src = final_src,
                             dst = final_dst,
                             size = size}

                        val {uses = final_uses,
                             defs = final_defs,
                             ...}
                          = Instruction.uses_defs_kills instruction

                        val {assembly = assembly_post,
                             registerAllocation}
                          = RA.post {uses = uses,
                                     final_uses = final_uses,
                                     defs = defs,
                                     final_defs = final_defs,
                                     kills = kills,
                                     info = info,
                                     registerAllocation = registerAllocation}
                      in
                        {assembly
                         = AppendList.appends 
                           [assembly_pre,
                            assembly_src_dst,
                            AppendList.single
                            (Assembly.instruction instruction),
                            assembly_post],
                           registerAllocation = registerAllocation}
                      end
                in
                  default ()
                end
             | pMD {oper, dst, src, size}
               (* Integer multiplication and division.
                * Require src operand as follows:
                *
                *               src
                *           reg imm lab add
                *            X           X
                *)
             => let
                  val {uses,defs,kills} 
                    = Instruction.uses_defs_kills instruction
                  val {assembly = assembly_pre,
                       registerAllocation}
                    = RA.pre {uses = uses,
                              defs = defs,
                              kills = kills,
                              info = info,
                              registerAllocation = registerAllocation}

                  val (hi,lo) 
                    = case size
                        of Size.BYTE 
                        => (Register.T {reg = Register.RDX, part = Register.L},
                            Register.T {reg = Register.RAX, part = Register.L})
                        | Size.WORD 
                        => (Register.T {reg = Register.RDX, part = Register.X},
                            Register.T {reg = Register.RAX, part = Register.X})
                        | Size.LONG
                        => (Register.T {reg = Register.RDX, part = Register.E},
                            Register.T {reg = Register.RAX, part = Register.E})
                        | Size.QUAD
                        => (Register.T {reg = Register.RDX, part = Register.R},
                            Register.T {reg = Register.RAX, part = Register.R})
                        | _ => Error.bug "amd64AllocateRegisters.Instruction.allocateRegisters: pMD, size"

                  val {assembly = assembly_clear,
                       registerAllocation,
                       ...}
                    = RA.freeRegister 
                      {info = info,
                       memloc = NONE,
                       size = size,
                       supports = [src,dst],
                       saves = [], 
                       force = [hi],
                       registerAllocation = registerAllocation}

                  val registerAllocation
                    = RA.delete {register = hi,
                                 registerAllocation = registerAllocation}

                  val {final_src,
                       assembly_src_dst,
                       registerAllocation,
                       ...}
                    = if Operand.eq(src, dst)
                        then let
                               val {operand = final_src_dst,
                                    assembly = assembly_src_dst,
                                    registerAllocation = registerAllocation}
                                 = RA.allocateOperand 
                                   {operand = dst,
                                    options = {register = true,
                                               immediate = NONE,
                                               label = false,
                                               address = false},
                                    info = info,
                                    size = size,
                                    move = true,
                                    supports = [],
                                    saves = [Operand.register hi],
                                    force = [lo],
                                    registerAllocation 
                                    = registerAllocation}
                             in
                               {final_src = final_src_dst,
                                final_dst = final_src_dst,
                                assembly_src_dst = assembly_src_dst,
                                registerAllocation = registerAllocation}
                             end
                        else let
                               val {operand = final_dst,
                                    assembly = assembly_dst,
                                    registerAllocation = registerAllocation}
                                 = RA.allocateOperand 
                                   {operand = dst,
                                    options = {register = true,
                                               immediate = NONE,
                                               label = false,
                                               address = false},
                                    info = info,
                                    size = size,
                                    move = true,
                                    supports = [src],
                                    saves = [Operand.register hi],
                                    force = [lo],
                                    registerAllocation 
                                    = registerAllocation}

                               val force_src 
                                 = List.revKeepAll
                                   (Register.registers size,
                                    fn r => not (Register.eq(r, hi) orelse 
                                                 Register.eq(r, lo)))

                               val {operand = final_src, 
                                    assembly = assembly_src,
                                    registerAllocation}
                                 = RA.allocateOperand 
                                   {operand = src,
                                    options = {register = true,
                                               immediate = NONE,
                                               label = false,
                                               address = true}, 
                                    info = info,
                                    size = size,
                                    move = true,
                                    supports = [],
                                    saves = [Operand.register hi,
                                             dst,final_dst],
                                    force = force_src,
                                    registerAllocation 
                                    = registerAllocation}
                             in
                               {final_src = final_src,
                                final_dst = final_dst,
                                assembly_src_dst 
                                = AppendList.appends 
                                  [assembly_dst, 
                                   assembly_src],
                                registerAllocation = registerAllocation}
                             end

                  val oper' 
                    = case oper
                        of Instruction.IMUL => Instruction.IMUL
                         | Instruction.MUL => Instruction.MUL
                         | Instruction.IDIV => Instruction.IDIV
                         | Instruction.DIV => Instruction.DIV
                         | Instruction.IMOD => Instruction.IDIV
                         | Instruction.MOD => Instruction.DIV

                  val registerAllocation
                    = if oper = Instruction.IMOD orelse
                         oper = Instruction.MOD
                        then case RA.valuesRegister {register = lo,
                                                     registerAllocation
                                                     = registerAllocation}
                               of [{memloc,
                                    weight,
                                    sync,
                                    commit, 
                                    ...}]
                                => let
                                     val registerAllocation
                                       = RA.delete {register = lo,
                                                    registerAllocation
                                                    = registerAllocation}

                                     val registerAllocation
                                       = RA.update {value = {register = hi,
                                                             memloc = memloc,
                                                             weight = weight,
                                                             sync = sync,
                                                             commit = commit},
                                                    registerAllocation 
                                                    = registerAllocation}
                                   in
                                     registerAllocation
                                   end
                                | _ => Error.bug "amd64AllocateRegisters.Instruction.allocateRegisters: pMD, lo"
                        else registerAllocation

                  val instruction
                    = Instruction.MD
                      {oper = oper',
                       src = final_src,
                       size = size}

                  val {uses = final_uses,
                       defs = final_defs,  
                       ...}
                    = Instruction.uses_defs_kills instruction

                  val {assembly = assembly_post,
                       registerAllocation}
                    = RA.post {uses = uses,
                               final_uses = final_uses,
                               defs = defs,
                               final_defs = final_defs,
                               kills = kills,
                               info = info,
                               registerAllocation = registerAllocation}
                in
                  {assembly 
                   = AppendList.appends 
                     [assembly_pre,
                      assembly_clear,
                      assembly_src_dst,
                      (if oper = Instruction.IDIV orelse
                          oper = Instruction.IMOD
                         then AppendList.single
                              (Assembly.instruction_cx
                               {size = size})
                         else if oper = Instruction.DIV orelse
                                 oper = Instruction.MOD
                                then AppendList.single
                                     (Assembly.instruction_binal
                                      {oper = Instruction.XOR,
                                       dst = Operand.register hi,
                                       src = Operand.register hi,
                                       size = size})
                                else AppendList.empty),
                      AppendList.single
                      (Assembly.instruction instruction),
                      assembly_post],
                   registerAllocation = registerAllocation}
                end
             | IMUL2 {src, dst, size}
               (* Integer signed/unsigned multiplication (two operand form).
                * Require src/dst operands as follows:
                *
                *              dst
                *          reg imm lab add 
                *      reg  X
                *  src imm  X
                *      lab
                *      add  X
                *)
             => let
                  val {uses,defs,kills} 
                    = Instruction.uses_defs_kills instruction
                  val {assembly = assembly_pre,
                       registerAllocation}
                    = RA.pre {uses = uses,
                              defs = defs,
                              kills = kills,
                              info = info,
                              registerAllocation = registerAllocation}

                  val {final_src,
                       final_dst,
                       assembly_src_dst,
                       registerAllocation}
                    = if Operand.eq(src, dst)
                        then let
                               val {operand = final_src_dst, 
                                    assembly = assembly_src_dst,
                                    registerAllocation}
                                 = RA.allocateOperand 
                                   {operand = src,
                                    options = {register = true,
                                               immediate = NONE,
                                               label = false,
                                               address = false},
                                    info = info,
                                    size = size,
                                    move = true,
                                    supports = [],
                                    saves = [],
                                    force = [],
                                    registerAllocation 
                                    = registerAllocation}
                             in
                               {final_src = final_src_dst,
                                final_dst = final_src_dst,
                                assembly_src_dst = assembly_src_dst,
                                registerAllocation = registerAllocation}
                             end
                        else let
                               val {operand = final_dst,
                                    assembly = assembly_dst,
                                    registerAllocation}
                                 = RA.allocateOperand 
                                   {operand = dst,
                                    options = {register = true,
                                               immediate = NONE,
                                               label = false,
                                               address = false},
                                    info = info,
                                    size = size,
                                    move = true,
                                    supports = [src],
                                    saves = [],
                                    force = [],
                                    registerAllocation 
                                    = registerAllocation}

                               val {operand = final_src, 
                                    assembly = assembly_src,
                                    registerAllocation}
                                 = RA.allocateOperand 
                                   {operand = src,
                                    options = {register = true,
                                               immediate = SOME WordSize.word32,
                                               label = false,
                                               address = false},
                                    info = info,
                                    size = size,
                                    move = true,
                                    supports = [],
                                    saves = [dst,final_dst],
                                    force = [],
                                    registerAllocation 
                                    = registerAllocation}
                             in
                               {final_src = final_src,
                                final_dst = final_dst,
                                assembly_src_dst 
                                = AppendList.appends 
                                  [assembly_dst,
                                   assembly_src],
                                registerAllocation = registerAllocation}
                             end

                  val instruction 
                    = Instruction.IMUL2
                      {src = final_src,
                       dst = final_dst,
                       size = size}

                  val {uses = final_uses,
                       defs = final_defs,
                       ...}
                    = Instruction.uses_defs_kills instruction

                  val {assembly = assembly_post,
                       registerAllocation}
                    = RA.post {uses = uses,
                               final_uses = final_uses,
                               defs = defs,
                               final_defs = final_defs,
                               kills = kills,
                               info = info,
                               registerAllocation = registerAllocation}
                in
                  {assembly
                   = AppendList.appends 
                     [assembly_pre,
                      assembly_src_dst,
                      AppendList.single
                      (Assembly.instruction instruction),
                      assembly_post],
                     registerAllocation = registerAllocation}
                end
             | UnAL {oper, dst, size}
               (* Integer unary arithmetic/logic instructions.
                * Require dst operand as follows:
                *
                *               dst
                *           reg imm lab add
                *            X           X
                *)
             => let
                  val {uses,defs,kills} 
                    = Instruction.uses_defs_kills instruction
                  val {assembly = assembly_pre,
                       registerAllocation}
                    = RA.pre {uses = uses,
                              defs = defs,
                              kills = kills,
                              info = info,
                              registerAllocation = registerAllocation}

                  val {operand = final_dst,
                       assembly = assembly_dst,
                       registerAllocation = registerAllocation}
                    = RA.allocateOperand {operand = dst,
                                          options = {register = true,
                                                     immediate = NONE,
                                                     label = false,
                                                     address = true},
                                          info = info,
                                          size = size,
                                          move = true,
                                          supports = [],
                                          saves = [],
                                          force = [],
                                          registerAllocation 
                                          = registerAllocation}

                  val instruction
                    = Instruction.UnAL
                      {oper = oper,
                       dst = final_dst,
                       size = size}

                  val {uses = final_uses,
                       defs = final_defs,  
                       ...}
                    = Instruction.uses_defs_kills instruction

                  val {assembly = assembly_post,
                       registerAllocation}
                    = RA.post {uses = uses,
                               final_uses = final_uses,
                               defs = defs,
                               final_defs = final_defs,
                               kills = kills,
                               info = info,
                               registerAllocation = registerAllocation}
                in
                  {assembly 
                   = AppendList.appends 
                     [assembly_pre,
                      assembly_dst,
                      AppendList.single
                      (Assembly.instruction instruction),
                      assembly_post],
                   registerAllocation = registerAllocation}
                end
             | SRAL {oper, count, dst, size}
               (* Integer shift/rotate arithmetic/logic instructions.
                * Require count operand as follows:
                *
                *              count
                *           reg imm lab add
                *            *   X
                *  * only register %cl
                *
                * Require dst operand as follows:
                *
                *               dst
                *           reg imm lab add
                *            X           X
                *)
             => let
                  val {uses,defs,kills} 
                    = Instruction.uses_defs_kills instruction
                  val {assembly = assembly_pre,
                       registerAllocation}
                    = RA.pre {uses = uses,
                              defs = defs,
                              kills = kills,
                              info = info,
                              registerAllocation = registerAllocation}

                  val {final_count,
                       assembly_count,
                       final_dst,
                       assembly_dst,
                       registerAllocation}
                    = if Operand.eq(count,dst)
                        then let
                               val {operand = final_count, 
                                    assembly = assembly_count,
                                    registerAllocation}
                                 = RA.allocateOperand 
                                   {operand = count,
                                    options = {register = true,
                                               immediate = NONE,
                                               label = false,
                                               address = false}, 
                                    info = info,
                                    size = size,
                                    move = true,
                                    supports = [],
                                    saves = [],
                                    force 
                                    = [Register.T {reg = Register.RCX,
                                                   part = Register.L},
                                       Register.T {reg = Register.RCX,
                                                   part = Register.X},
                                       Register.T {reg = Register.RCX,
                                                   part = Register.E},
                                       Register.T {reg = Register.RCX,
                                                   part = Register.R}],
                                    registerAllocation 
                                    = registerAllocation}

                               val final_dst = final_count
                               val assembly_dst = AppendList.empty
                             in
                               {final_count = final_count,
                                assembly_count = assembly_count,
                                final_dst = final_dst,
                                assembly_dst = assembly_dst,
                                registerAllocation = registerAllocation}
                             end
                        else let
                               val count_size = case Operand.size count
                                                  of NONE => Size.BYTE
                                                   | SOME size => size

                               val {operand = final_count, 
                                    assembly = assembly_count,
                                    registerAllocation}
                                 = RA.allocateOperand 
                                   {operand = count,
                                    options = {register = true,
                                               immediate = SOME WordSize.word8,
                                               label = false,
                                               address = false}, 
                                    info = info,
                                    size = count_size,
                                    move = true,
                                    supports = [dst],
                                    saves = [],
                                    force 
                                    = [Register.T {reg = Register.RCX,
                                                   part = Register.L},
                                       Register.T {reg = Register.RCX,
                                                   part = Register.X},
                                       Register.T {reg = Register.RCX,
                                                   part = Register.E},
                                       Register.T {reg = Register.RCX,
                                                   part = Register.R}],
                                    registerAllocation 
                                    = registerAllocation}

                               val {operand = final_dst,
                                    assembly = assembly_dst,
                                    registerAllocation = registerAllocation}
                                 = RA.allocateOperand 
                                   {operand = dst,
                                    options = {register = true,
                                               immediate = NONE,
                                               label = false,
                                               address = true},
                                    info = info,
                                    size = size,
                                    move = true,
                                    supports = [],
                                    saves = [count,final_count],
                                    force = [],
                                    registerAllocation 
                                    = registerAllocation}
                             in
                               {final_count = final_count,
                                assembly_count = assembly_count,
                                final_dst = final_dst,
                                assembly_dst = assembly_dst,
                                registerAllocation = registerAllocation}
                             end

                  val final_count
                    = case final_count
                        of Operand.Register _ 
                         => Operand.register 
                            (Register.T {reg = Register.RCX,
                                         part = Register.L})
                         | _ => final_count

                  val instruction 
                    = Instruction.SRAL
                      {oper = oper,
                       count = final_count,
                       dst = final_dst,
                       size = size}

                  val {uses = final_uses,
                       defs = final_defs,  
                       ...}
                    = Instruction.uses_defs_kills instruction

                  val {assembly = assembly_post,
                       registerAllocation}
                    = RA.post {uses = uses,
                               final_uses = final_uses,
                               defs = defs,
                               final_defs = final_defs,
                               kills = kills,
                               info = info,
                               registerAllocation = registerAllocation}
                in
                  {assembly 
                   = AppendList.appends 
                     [assembly_pre,
                      assembly_count,
                      assembly_dst,
                      AppendList.single
                      (Assembly.instruction instruction),
                      assembly_post],
                   registerAllocation = registerAllocation}
                end
             | CMP {src2, src1, size}
               (* Arithmetic compare
                * Require src1/src2 operands as follows:
                *
                *               src2
                *           reg imm lab add 
                *       reg  X   X       X
                *  src1 imm
                *       lab
                *       add  X   X
                *)
             => let
                  val {uses,defs,kills} 
                    = Instruction.uses_defs_kills instruction
                  val {assembly = assembly_pre,
                       registerAllocation}
                    = RA.pre {uses = uses,
                              defs = defs,
                              kills = kills,
                              info = info,
                              registerAllocation = registerAllocation}

                  val {final_src1,
                       final_src2,
                       assembly_src1_src2,
                       registerAllocation}
                    = allocateSrc1Src2 
                      {src1 = src1,
                       src2 = src2,
                       size = size,
                       info = info,
                       registerAllocation = registerAllocation}

                  val instruction
                    = Instruction.CMP 
                      {src1 = final_src1,
                       src2 = final_src2,
                       size = size}

                  val {uses = final_uses,
                       defs = final_defs,  
                       ...}
                    = Instruction.uses_defs_kills instruction

                  val {assembly = assembly_post,
                       registerAllocation}
                    = RA.post {uses = uses,
                               final_uses = final_uses,
                               defs = defs,
                               final_defs = final_defs,
                               kills = kills,
                               info = info,
                               registerAllocation = registerAllocation}
                in
                  {assembly 
                   = AppendList.appends 
                     [assembly_pre,
                      assembly_src1_src2,
                      AppendList.single
                      (Assembly.instruction instruction),
                      assembly_post],
                   registerAllocation = registerAllocation}
                end
             | TEST {src2, src1, size}
               (* Logical compare
                * Require src1/src2 operands as follows:
                *
                *               src2
                *           reg imm lab add 
                *       reg  X   X       X
                *  src1 imm  
                *       lab
                *       add  X   X    
                *)
             => let
                  val {uses,defs,kills} 
                    = Instruction.uses_defs_kills instruction
                  val {assembly = assembly_pre,
                       registerAllocation}
                    = RA.pre {uses = uses,
                              defs = defs,
                              kills = kills,
                              info = info,
                              registerAllocation = registerAllocation}

                  val {final_src1,
                       final_src2,
                       assembly_src1_src2,
                       registerAllocation}
                    = allocateSrc1Src2 
                      {src1 = src1,
                       src2 = src2,
                       size = size,
                       info = info,
                       registerAllocation = registerAllocation}

                  val instruction
                    = Instruction.TEST
                      {src1 = final_src1,
                       src2 = final_src2,
                       size = size}

                  val {uses = final_uses,
                       defs = final_defs,  
                       ...}
                    = Instruction.uses_defs_kills instruction

                  val {assembly = assembly_post,
                       registerAllocation}
                    = RA.post {uses = uses,
                               final_uses = final_uses,
                               defs = defs,
                               final_defs = final_defs,
                               kills = kills,
                               info = info,
                               registerAllocation = registerAllocation}
                in
                  {assembly 
                   = AppendList.appends 
                     [assembly_pre,
                      assembly_src1_src2,
                      AppendList.single
                      (Assembly.instruction instruction),
                      assembly_post],
                   registerAllocation = registerAllocation}
                end
             | SETcc {condition, dst, size}
               (* Set byte on condition
                * Require dst operand as follows:
                *
                *                dst
                *            reg imm lab add
                *             *           X
                *  * only byte registers
                *)
             => let
                  val {uses,defs,kills} 
                    = Instruction.uses_defs_kills instruction
                  val {assembly = assembly_pre,
                       registerAllocation}
                    = RA.pre {uses = uses,
                              defs = defs,
                              kills = kills,
                              info = info,
                              registerAllocation = registerAllocation}

                  val {operand = final_dst,
                       assembly = assembly_dst,
                       registerAllocation = registerAllocation}
                    = RA.allocateOperand 
                      {operand = dst,
                       options = {register = true,
                                  immediate = NONE,
                                  label = false,
                                  address = false},
                       info = info,
                       size = size,
                       move = false,
                       supports = [],
                       saves = [],
                       force = if Size.lt (Size.BYTE, size)
                                  then Register.withLowPart (size, Size.BYTE)
                               else Register.registers Size.BYTE,
                       registerAllocation = registerAllocation}

                  val temp_dst
                    = case final_dst
                        of Operand.Register r
                         => let
                              val register 
                                = Register.lowPartOf (r, Size.BYTE)
                            in
                              Operand.register register
                            end
                         | _ => Error.bug "amd64AllocateRegisters.Instruction.allocateRegisters: SETcc, temp_reg"

                  val {uses = final_uses,
                       defs = final_defs,  
                       ...}
                    = Instruction.uses_defs_kills
                      (Instruction.SETcc {condition = condition,
                                          dst = final_dst,
                                          size = size})

                  val {assembly = assembly_post,
                       registerAllocation}
                    = RA.post {uses = uses,
                               final_uses = final_uses,
                               defs = defs,
                               final_defs = final_defs,
                               kills = kills,
                               info = info,
                               registerAllocation = registerAllocation}
                in
                  {assembly 
                   = AppendList.appends 
                     [assembly_pre,
                      assembly_dst,
                      AppendList.single
                      (Assembly.instruction_setcc
                       {condition = condition,
                        dst = temp_dst,
                        size = Size.BYTE}),
                      if size = Size.BYTE
                        then if Operand.eq (final_dst, temp_dst)
                               then AppendList.empty
                               else AppendList.single
                                    (Assembly.instruction_mov
                                     {dst = final_dst,
                                      src = temp_dst,
                                      size = Size.BYTE})
                        else AppendList.single
                             (Assembly.instruction_movx
                              {oper = Instruction.MOVZX,
                               dst = final_dst,
                               src = temp_dst,
                               dstsize = size,
                               srcsize = Size.BYTE}),
                      assembly_post],
                   registerAllocation = registerAllocation}
                end
             | JMP {target, absolute}
               (* Jump
                * Require target operand as follows:
                *
                *               target
                *            reg imm lab add
                *             X   X   X   X
                *)
             => let
                  val {uses,defs,kills} 
                    = Instruction.uses_defs_kills instruction
                  val {assembly = assembly_pre,
                       registerAllocation}
                    = RA.pre {uses = uses,
                              defs = defs,
                              kills = kills,
                              info = info,
                              registerAllocation = registerAllocation}

                  val {operand = final_target,
                       assembly = assembly_target,
                       registerAllocation = registerAllocation}
                    = RA.allocateOperand {operand = target,
                                          options = {register = false,
                                                     immediate = SOME WordSize.word64,
                                                     label = true,
                                                     address = true},
                                          info = info,
                                          size = Size.QUAD,
                                          move = true,
                                          supports = [],
                                          saves = [],
                                          force = [],
                                          registerAllocation 
                                          = registerAllocation}

                  val instruction
                    = Instruction.JMP
                      {target = final_target,
                       absolute = absolute}

                  val {uses = final_uses,
                       defs = final_defs,  
                       ...}
                    = Instruction.uses_defs_kills instruction

                  val {assembly = assembly_post,
                       registerAllocation}
                    = RA.post {uses = uses,
                               final_uses = final_uses,
                               defs = defs,
                               final_defs = final_defs,
                               kills = kills,
                               info = info,
                               registerAllocation = registerAllocation}
                in
                  {assembly 
                   = AppendList.appends 
                     [assembly_pre,
                      assembly_target,
                      AppendList.single
                      (Assembly.instruction instruction),
                      assembly_post],
                   registerAllocation = registerAllocation}
                end
             | Jcc {condition, target}
               (* Jump if condition is met
                * Require target operand as follows:
                *
                *               target
                *            reg imm lab add
                *                 X   X
                *)
             => let
                  val {uses,defs,kills} 
                    = Instruction.uses_defs_kills instruction
                  val {assembly = assembly_pre,
                       registerAllocation}
                    = RA.pre {uses = uses,
                              defs = defs,
                              kills = kills,
                              info = info,
                              registerAllocation = registerAllocation}

                  val {operand = final_target,
                       assembly = assembly_target,
                       registerAllocation = registerAllocation}
                    = RA.allocateOperand {operand = target,
                                          options = {register = false,
                                                     immediate = SOME WordSize.word64,
                                                     label = true,
                                                     address = false},
                                          info = info,
                                          size = Size.QUAD,
                                          move = true,
                                          supports = [],
                                          saves = [],
                                          force = [],
                                          registerAllocation 
                                          = registerAllocation}

                  val instruction
                    = Instruction.Jcc
                      {condition = condition,
                       target = final_target}

                  val {uses = final_uses,
                       defs = final_defs,  
                       ...}
                    = Instruction.uses_defs_kills instruction

                  val {assembly = assembly_post,
                       registerAllocation}
                    = RA.post {uses = uses,
                               final_uses = final_uses,
                               defs = defs,
                               final_defs = final_defs,
                               kills = kills,
                               info = info,
                               registerAllocation = registerAllocation}
                in
                  {assembly 
                   = AppendList.appends 
                     [assembly_pre,
                      assembly_target,
                      AppendList.single
                      (Assembly.instruction instruction),
                      assembly_post],
                   registerAllocation = registerAllocation}
                end
             | CALL {target, absolute}
               (* Call procedure
                * Require target operand as follows:
                *
                *               target
                *            reg imm lab add
                *             X   X   X   X
                *)
             => let
                  val {uses,defs,kills} 
                    = Instruction.uses_defs_kills instruction
                  val {assembly = assembly_pre,
                       registerAllocation}
                    = RA.pre {uses = uses,
                              defs = defs,
                              kills = kills,
                              info = info,
                              registerAllocation = registerAllocation}

                  val {operand = final_target,
                       assembly = assembly_target,
                       registerAllocation = registerAllocation}
                    = RA.allocateOperand {operand = target,
                                          options = {register = true,
                                                     immediate = SOME WordSize.word64,
                                                     label = true,
                                                     address = true},
                                          info = info,
                                          size = Size.QUAD,
                                          move = true,
                                          supports = [],
                                          saves = [],
                                          force = [],
                                          registerAllocation 
                                          = registerAllocation}

                  val instruction
                    = Instruction.CALL
                      {target = final_target,
                       absolute = absolute}

                  val {uses = final_uses,
                       defs = final_defs,  
                       ...}
                    = Instruction.uses_defs_kills instruction

                  val {assembly = assembly_post,
                       registerAllocation}
                    = RA.post {uses = uses,
                               final_uses = final_uses,
                               defs = defs,
                               final_defs = final_defs,
                               kills = kills,
                               info = info,
                               registerAllocation = registerAllocation}
                in
                  {assembly 
                   = AppendList.appends 
                     [assembly_pre,
                      assembly_target,
                      AppendList.single
                      (Assembly.instruction instruction),
                      assembly_post],
                   registerAllocation = registerAllocation}
                end
             | RET {src = SOME src}
               (* Return from procedure
                * Require optional src operand as follows:
                *
                *                src
                *            reg imm lab add
                *                 X   
                *)
             => let
                  val {uses,defs,kills} 
                    = Instruction.uses_defs_kills instruction
                  val {assembly = assembly_pre,
                       registerAllocation}
                    = RA.pre {uses = uses,
                              defs = defs,
                              kills = kills,
                              info = info,
                              registerAllocation = registerAllocation}

                  val {operand = final_src,
                       assembly = assembly_src,
                       registerAllocation = registerAllocation}
                    = RA.allocateOperand {operand = src,
                                          options = {register = false,
                                                     immediate = SOME WordSize.word32,
                                                     label = false,
                                                     address = false},
                                          info = info,
                                          size = Size.LONG,
                                          move = true,
                                          supports = [],
                                          saves = [],
                                          force = [],
                                          registerAllocation 
                                          = registerAllocation}

                  val instruction
                    = Instruction.RET
                      {src = SOME final_src}

                  val {uses = final_uses,
                       defs = final_defs,  
                       ...}
                    = Instruction.uses_defs_kills instruction

                  val {assembly = assembly_post,
                       registerAllocation}
                    = RA.post {uses = uses,
                               final_uses = final_uses,
                               defs = defs,
                               final_defs = final_defs,
                               kills = kills,
                               info = info,
                               registerAllocation = registerAllocation}
                in
                  {assembly 
                   = AppendList.appends 
                     [assembly_pre,
                      assembly_src,
                      AppendList.single
                      (Assembly.instruction instruction),
                      assembly_post],
                   registerAllocation = registerAllocation}
                end
             | RET {src = NONE}
             => let
                  val {uses,defs,kills} 
                    = Instruction.uses_defs_kills instruction
                  val {assembly = assembly_pre,
                       registerAllocation}
                    = RA.pre {uses = uses,
                              defs = defs,
                              kills = kills,
                              info = info,
                              registerAllocation = registerAllocation}

                  val instruction
                    = Instruction.RET
                      {src = NONE}

                  val {uses = final_uses,
                       defs = final_defs,  
                       ...}
                    = Instruction.uses_defs_kills instruction

                  val {assembly = assembly_post,
                       registerAllocation}
                    = RA.post {uses = uses,
                               final_uses = final_uses,
                               defs = defs,
                               final_defs = final_defs,
                               kills = kills,
                               info = info,
                               registerAllocation = registerAllocation}
                in
                  {assembly 
                   = AppendList.appends 
                     [assembly_pre,
                      AppendList.single
                      (Assembly.instruction instruction),
                      assembly_post],
                   registerAllocation = registerAllocation}
                end
             | MOV {src, dst, size}
               (* Move
                * Require src/dst operands as follows:
                *
                *              dst
                *          reg imm lab add 
                *      reg  X           X
                *  src imm  X           X
                *      lab
                *      add  X
                *)
             => let
                  val {uses,defs,kills} 
                    = Instruction.uses_defs_kills instruction
                  val {assembly = assembly_pre,
                       registerAllocation}
                    = RA.pre {uses = uses,
                              defs = defs,
                              kills = kills,
                              info = info,
                              registerAllocation = registerAllocation}

                  fun default ()
                    = let
                        val {final_src,
                             final_dst,
                             assembly_src_dst,
                             registerAllocation}
                          = allocateSrcDst
                            {src = src,
                             dst = dst,
                             move_dst = false,
                             size = size,
                             info = info,
                             registerAllocation = registerAllocation}

                        val isConst0 = Immediate.isZero

                        (* special case moving 0 to a register
                         *)
                        val instruction
                          = case (final_src, final_dst)
                              of (Operand.Immediate immediate,
                                  Operand.Register _)
                               => if isConst0 immediate
                                    then Instruction.BinAL
                                         {oper = XOR,
                                          src = final_dst,
                                          dst = final_dst,
                                          size = size}
                                    else Instruction.MOV
                                         {src = final_src,
                                          dst = final_dst,
                                          size = size}
                               | _ => Instruction.MOV
                                      {src = final_src,
                                       dst = final_dst,
                                       size = size}

                        val {uses = final_uses,
                             defs = final_defs,  
                             ...}
                          = Instruction.uses_defs_kills instruction

                        val {assembly = assembly_post,
                             registerAllocation}
                          = RA.post {uses = uses,
                                     final_uses = final_uses,
                                     defs = defs,
                                     final_defs = final_defs,
                                     kills = kills,
                                     info = info,
                                     registerAllocation = registerAllocation}
                      in
                        {assembly 
                         = AppendList.appends 
                           [assembly_pre,
                            assembly_src_dst,
                            AppendList.single
                            (Assembly.instruction instruction),
                            assembly_post],
                         registerAllocation = registerAllocation}
                      end

                  fun default' ({register = register_src,
                                 commit = commit_src,
                                 ...} : RegisterAllocation.value,
                                memloc_dst)
                    = let
                        val registerAllocation
                          = RA.remove
                            {memloc = memloc_dst,
                             registerAllocation = registerAllocation}

                        val registerAllocation
                          = RA.update
                            {value = {register = register_src,
                                      memloc = memloc_dst,
                                      weight = 1024,
                                      sync = false,
                                      commit = commit_src},
                             registerAllocation = registerAllocation}

                        val final_uses = []
                        val final_defs 
                          = [Operand.register register_src]

                        val {assembly = assembly_post,
                             registerAllocation}
                          = RA.post {uses = uses,
                                     final_uses = final_uses,
                                     defs = defs,
                                     final_defs = final_defs,
                                     kills = kills,
                                     info = info,
                                     registerAllocation = registerAllocation}
                      in
                        {assembly 
                         = AppendList.appends [assembly_pre,
                                               assembly_post],
                         registerAllocation = registerAllocation}
                      end

                  fun default'' (memloc_dst)
                    = let
                        val registerAllocation
                          = RA.remove
                            {memloc = memloc_dst,
                             registerAllocation = registerAllocation}

                        val {final_src,
                             final_dst,
                             assembly_src_dst,
                             registerAllocation}
                          = allocateSrcDst
                            {src = src,
                             dst = dst,
                             move_dst = false,
                             size = size,
                             info = info,
                             registerAllocation = registerAllocation}

                        val instruction
                          = Instruction.MOV
                            {src = final_src,
                             dst = final_dst,
                             size = size}

                        val {uses = final_uses,
                             defs = final_defs,
                             ...}
                          = Instruction.uses_defs_kills instruction

                        val {assembly = assembly_post,
                             registerAllocation}
                          = RA.post {uses = uses,
                                     final_uses = final_uses,
                                     defs = defs,
                                     final_defs = final_defs,
                                     kills = kills,
                                     info = info,
                                     registerAllocation = registerAllocation}
                      in
                        {assembly 
                         = AppendList.appends 
                           [assembly_pre,
                            assembly_src_dst,
                            AppendList.single
                            (Assembly.instruction instruction),
                            assembly_post],
                         registerAllocation = registerAllocation}
                      end

                  val memloc_src = Operand.deMemloc src
                  val value_src 
                    = case memloc_src
                        of NONE => NONE
                         | SOME memloc_src
                         => RA.allocated {memloc = memloc_src,
                                          registerAllocation 
                                          = registerAllocation}
                  val memloc_dst = Operand.deMemloc dst
                in
                  case memloc_dst
                    of SOME memloc_dst
                     => if MemLocSet.contains(remove,memloc_dst)
                          then (case memloc_src
                                  of SOME memloc_src
                                   => if List.contains
                                         (memloc_src::(MemLoc.utilized memloc_src),
                                          memloc_dst,
                                          MemLoc.eq)
                                        then default ()
                                        else default'' memloc_dst
                                   | NONE => default'' memloc_dst)
                          else (case value_src
                                  of SOME (value_src as {memloc = memloc_src,
                                                         sync = sync_src, ...})
                                    => if MemLocSet.contains(dead,memloc_src)
                                          orelse
                                          (MemLocSet.contains(remove,memloc_src)
                                           andalso
                                           sync_src)
                                         then default' (value_src, memloc_dst)
                                         else default ()
                                    | NONE => default ())
                     | NONE => default ()
                end
             | CMOVcc {condition, src, dst, size}
               (* Conditional move
                * Require src/dst operands as follows:
                *
                *              dst
                *          reg imm lab add 
                *      reg  X
                *  src imm           
                *      lab
                *      add  X
                *)
             => let
                  val {uses,defs,kills} 
                    = Instruction.uses_defs_kills instruction
                  val {assembly = assembly_pre,
                       registerAllocation}
                    = RA.pre {uses = uses,
                              defs = defs,
                              kills = kills,
                              info = info,
                              registerAllocation = registerAllocation}

                  val {operand = final_src, 
                       assembly = assembly_src,
                       registerAllocation}
                    = RA.allocateOperand {operand = src,
                                          options = {register = true,
                                                     immediate = NONE,
                                                     label = false,
                                                     address = true}, 
                                          info = info,
                                          size = size,
                                          move = true,
                                          supports = [dst],
                                          saves = [],
                                          force = [],
                                          registerAllocation 
                                          = registerAllocation}

                  val {operand = final_dst,
                       assembly = assembly_dst,
                       registerAllocation = registerAllocation}
                    = RA.allocateOperand {operand = dst,
                                          options = {register = true,
                                                     immediate = NONE,
                                                     label = false,
                                                     address = false},
                                          info = info,
                                          size = size,
                                          move = false,
                                          supports = [],
                                          saves = [src,final_src],
                                          force = [],
                                          registerAllocation 
                                          = registerAllocation}

                  val instruction
                    = Instruction.CMOVcc 
                      {condition = condition,
                       src = final_src,
                       dst = final_dst,
                       size = size}

                  val {uses = final_uses,
                       defs = final_defs,  
                       ...}
                    = Instruction.uses_defs_kills instruction

                  val {assembly = assembly_post,
                       registerAllocation}
                    = RA.post {uses = uses,
                               final_uses = final_uses,
                               defs = defs,
                               final_defs = final_defs,
                               kills = kills,
                               info = info,
                               registerAllocation = registerAllocation}
                in
                  {assembly 
                   = AppendList.appends 
                     [assembly_pre,
                      assembly_src,
                      assembly_dst,
                      AppendList.single
                      (Assembly.instruction instruction),
                      assembly_post],
                   registerAllocation = registerAllocation}
                end
             | XCHG {src, dst, size}
               (* Exchange register/memory with register
                * Require src/dst operands as follows:
                *
                *              dst
                *          reg imm lab add 
                *      reg  X           X
                *  src imm           
                *      lab
                *      add  X
                *)
             => let
                  val {uses,defs,kills} 
                    = Instruction.uses_defs_kills instruction
                  val {assembly = assembly_pre,
                       registerAllocation}
                    = RA.pre {uses = uses,
                              defs = defs,
                              kills = kills,
                              info = info,
                              registerAllocation = registerAllocation}

                  val {final_src,
                       final_dst,
                       assembly_src_dst,
                       registerAllocation}
                    = allocateSrcDst {src = src,
                                      dst = dst,
                                      move_dst = true,
                                      size = size,
                                      info = info,
                                      registerAllocation = registerAllocation}

                  val instruction
                    = Instruction.XCHG
                      {src = final_src,
                       dst = final_dst,
                       size = size}

                  val {uses = final_uses,
                       defs = final_defs,  
                       ...}
                    = Instruction.uses_defs_kills instruction

                  val {assembly = assembly_post,
                       registerAllocation}
                    = RA.post {uses = uses,
                               final_uses = final_uses,
                               defs = defs,
                               final_defs = final_defs,
                               kills = kills,
                               info = info,
                               registerAllocation = registerAllocation}
                in
                  {assembly 
                   = AppendList.appends 
                     [assembly_pre,
                      assembly_src_dst,
                      AppendList.single
                      (Assembly.instruction instruction),
                      assembly_post],
                   registerAllocation = registerAllocation}
                end
             | pPUSH {src, base, size}
               (* Pseudo push a value onto the stack
                * Require src operand as follows:
                *
                *               src
                *           reg imm lab add
                *            *   X       X
                *   * only word or long registers
                *
                *               base
                *           reg imm lab add
                *            *
                *   * only %esp
                *)
             => let
                  val {uses,defs,kills} 
                    = Instruction.uses_defs_kills instruction
                  val {assembly = assembly_pre,
                       registerAllocation}
                    = RA.pre {uses = uses,
                              defs = defs,
                              kills = kills,
                              info = info,
                              registerAllocation = registerAllocation}

                  val {assembly = assembly_base,
                       registerAllocation,
                       ...}
                    = RA.allocateOperand {operand = base,
                                          options = {register = true,
                                                     immediate = NONE,
                                                     label = false,
                                                     address = false},
                                          info = info,
                                          size = Size.QUAD,
                                          move = true,
                                          supports = [src],
                                          saves = [],
                                          force = [Register.rsp],
                                          registerAllocation 
                                          = registerAllocation}

                  val options
                    = case size
                        of Size.WORD
                         => {register = true,
                             immediate = SOME WordSize.word16,
                             label = false,
                             address = true}
                         | Size.LONG
                         => {register = true,
                             immediate = SOME WordSize.word32,
                             label = false,
                             address = true}
                         | Size.QUAD
                         => {register = true,
                             immediate = SOME WordSize.word32,
                             label = false,
                             address = true}
                         | _
                         => {register = false,
                             immediate = SOME WordSize.word32,
                             label = false,
                             address = true}

                  val {operand = final_src, 
                       assembly = assembly_src,
                       registerAllocation}
                    = RA.allocateOperand {operand = src,
                                          options = options,
                                          info = info,
                                          size = size,
                                          move = true,
                                          supports = [],
                                          saves = [],
                                          force = [],
                                          registerAllocation 
                                          = registerAllocation}

                  val instruction
                    = Instruction.PUSH
                      {src = final_src,
                       size = size}

                  val {uses = final_uses,
                       defs = final_defs,  
                       ...}
                    = Instruction.uses_defs_kills instruction

                  val {assembly = assembly_post,
                       registerAllocation}
                    = RA.post {uses = uses,
                               final_uses = final_uses,
                               defs = defs,
                               final_defs = final_defs,
                               kills = kills,
                               info = info,
                               registerAllocation = registerAllocation}
                in
                  {assembly 
                   = AppendList.appends 
                     [assembly_pre,
                      assembly_base,
                      assembly_src,
                      AppendList.single
                      (Assembly.instruction instruction),
                      assembly_post],
                   registerAllocation = registerAllocation}
                end
             | pPOP {dst, base, size}
               (* Pseudo pop a value from the stack
                * Require dst operand as follows:
                *
                *               dst
                *           reg imm lab add
                *            *           X
                *   * only word or long registers
                *               base
                *           reg imm lab add
                *            *
                *   * only %esp
                *)
             => let
                  val {uses,defs,kills} 
                    = Instruction.uses_defs_kills instruction
                  val {assembly = assembly_pre,
                       registerAllocation}
                    = RA.pre {uses = uses,
                              defs = defs,
                              kills = kills,
                              info = info,
                              registerAllocation = registerAllocation}

                  val {assembly = assembly_base,
                       registerAllocation,
                       ...}
                    = RA.allocateOperand {operand = base,
                                          options = {register = true,
                                                     immediate = NONE,
                                                     label = false,
                                                     address = false},
                                          info = info,
                                          size = Size.QUAD,
                                          move = true,
                                          supports = [dst],
                                          saves = [],
                                          force = [Register.rsp],
                                          registerAllocation 
                                          = registerAllocation}

                  val options
                    = case size
                        of Size.WORD
                         => {register = true,
                             immediate = NONE,
                             label = false,
                             address = true}
                         | Size.LONG
                         => {register = true,
                             immediate = NONE,
                             label = false,
                             address = true}
                         | Size.QUAD
                         => {register = true,
                             immediate = NONE,
                             label = false,
                             address = true}
                         | _
                         => {register = false,
                             immediate = NONE,
                             label = false,
                             address = true}

                  val {operand = final_dst, 
                       assembly = assembly_dst,
                       registerAllocation}
                    = RA.allocateOperand {operand = dst,
                                          options = options,
                                          info = info,
                                          size = size,
                                          move = false,
                                          supports = [],
                                          saves = [],
                                          force = [],
                                          registerAllocation 
                                          = registerAllocation}

                  val instruction
                    = Instruction.POP
                      {dst = final_dst,
                       size = size}

                  val {uses = final_uses,
                       defs = final_defs,  
                       ...}
                    = Instruction.uses_defs_kills instruction

                  val {assembly = assembly_post,
                       registerAllocation}
                    = RA.post {uses = uses,
                               final_uses = final_uses,
                               defs = defs,
                               final_defs = final_defs,
                               kills = kills,
                               info = info,
                               registerAllocation = registerAllocation}
                in
                  {assembly 
                   = AppendList.appends 
                     [assembly_pre,
                      assembly_base,
                      assembly_dst,
                      AppendList.single
                      (Assembly.instruction instruction),
                      assembly_post],
                   registerAllocation = registerAllocation}
                end
             | MOVX {oper, src, dst, srcsize, dstsize}
               (* Move with extention.
                * Require src/dst operands as follows:
                *
                *              dst
                *          reg imm lab add 
                *      reg  X
                *  src imm
                *      lab
                *      add  X
                *)
             => let
                  val {uses,defs,kills} 
                    = Instruction.uses_defs_kills instruction
                  val {assembly = assembly_pre,
                       registerAllocation}
                    = RA.pre {uses = uses,
                              defs = defs,
                              kills = kills,
                              info = info,
                              registerAllocation = registerAllocation}

                  val {operand = final_src, 
                       assembly = assembly_src,
                       registerAllocation}
                    = RA.allocateOperand {operand = src,
                                          options = {register = true,
                                                     immediate = NONE,
                                                     label = false,
                                                     address = true}, 
                                          info = info,
                                          size = srcsize,
                                          move = true,
                                          supports = [dst],
                                          saves = [],
                                          force = [],
                                          registerAllocation 
                                          = registerAllocation}

                  val {operand = final_dst,
                       assembly = assembly_dst,
                       registerAllocation = registerAllocation}
                    = RA.allocateOperand {operand = dst,
                                          options = {register = true,
                                                     immediate = NONE,
                                                     label = false,
                                                     address = false},
                                          info = info,
                                          size = dstsize,
                                          move = false,
                                          supports = [],
                                          saves = [src,final_src],
                                          force = [],
                                          registerAllocation 
                                          = registerAllocation}

                  val instruction
                    = Instruction.MOVX
                      {oper = oper,
                       src = final_src,
                       dst = final_dst,
                       srcsize = srcsize,
                       dstsize = dstsize}

                  val {uses = final_uses,
                       defs = final_defs,  
                       ...}
                    = Instruction.uses_defs_kills instruction

                  val {assembly = assembly_post,
                       registerAllocation}
                    = RA.post {uses = uses,
                               final_uses = final_uses,
                               defs = defs,
                               final_defs = final_defs,
                               kills = kills,
                               info = info,
                               registerAllocation = registerAllocation}
                in
                  {assembly 
                   = AppendList.appends 
                     [assembly_pre,
                      assembly_src,
                      assembly_dst,
                      AppendList.single
                      (Assembly.instruction instruction),
                      assembly_post],
                   registerAllocation = registerAllocation}
                end
             | XVOM {src, dst, srcsize, dstsize}
               (* Move with contraction.
                * Require src/dst operands as follows:
                *
                *              dst
                *          reg imm lab add 
                *      reg  X           X
                *  src imm
                *      lab
                *      add
                *)
             => let
                  val {uses,defs,kills} 
                    = Instruction.uses_defs_kills instruction
                  val {assembly = assembly_pre,
                       registerAllocation}
                    = RA.pre {uses = uses,
                              defs = defs,
                              kills = kills,
                              info = info,
                              registerAllocation = registerAllocation}

                  val {operand = final_src, 
                       assembly = assembly_src,
                       registerAllocation}
                    = RA.allocateOperand {operand = src,
                                          options = {register = true,
                                                     immediate = NONE,
                                                     label = false,
                                                     address = false}, 
                                          info = info,
                                          size = srcsize,
                                          move = true,
                                          supports = [dst],
                                          saves = [],
                                          force 
                                          = Register.withLowPart (srcsize,
                                                                  dstsize),
                                          registerAllocation 
                                          = registerAllocation}

                  val {operand = final_dst,
                       assembly = assembly_dst,
                       registerAllocation = registerAllocation}
                    = RA.allocateOperand {operand = dst,
                                          options = {register = true,
                                                     immediate = NONE,
                                                     label = false,
                                                     address = true},
                                          info = info,
                                          size = dstsize,
                                          move = false,
                                          supports = [],
                                          saves = [src,final_src],
                                          force = [],
                                          registerAllocation 
                                          = registerAllocation}

                  val {uses = final_uses,
                       defs = final_defs,  
                       ...}
                    = Instruction.uses_defs_kills
                      (Instruction.XVOM
                       {src = final_src,
                        dst = final_dst,
                        srcsize = srcsize,
                        dstsize = dstsize})

                  val temp_reg
                    = case final_src
                        of Operand.Register r 
                         => Register.lowPartOf (r, dstsize)
                         | _ 
                         => Error.bug "amd64AllocateRegisters.Instruction.allocateRegisters: XVOM, temp_reg"

                  val instruction
                     = Instruction.MOV
                       {src = Operand.register temp_reg,
                        dst = final_dst,
                        size = dstsize}

                  val {assembly = assembly_post,
                       registerAllocation}
                    = RA.post {uses = uses,
                               final_uses = final_uses,
                               defs = defs,
                               final_defs = final_defs,
                               kills = kills,
                               info = info,
                               registerAllocation = registerAllocation}
                in
                  {assembly 
                   = AppendList.appends 
                     [assembly_pre,
                      assembly_src,
                      assembly_dst,     
                      AppendList.single
                      (Assembly.instruction instruction),
                      assembly_post],
                   registerAllocation = registerAllocation}
                end
             | LEA {src, dst, size}
               (* Load effective address
                * Require src/dst operands as follows:
                *
                *              dst
                *          reg imm lab add 
                *      reg
                *  src imm
                *      lab
                *      add  X
                *)
             => let
                  val {uses,defs,kills} 
                    = Instruction.uses_defs_kills instruction
                  val {assembly = assembly_pre,
                       registerAllocation}
                    = RA.pre {uses = uses,
                              defs = defs,
                              kills = kills,
                              info = info,
                              registerAllocation = registerAllocation}

                  val {operand = final_src, 
                       assembly = assembly_src,
                       registerAllocation}
                    = RA.allocateOperand {operand = src,
                                          options = {register = false,
                                                     immediate = NONE,
                                                     label = false,
                                                     address = true}, 
                                          info = info,
                                          size = size,
                                          move = true,
                                          supports = [dst],
                                          saves = [],
                                          force = [],
                                          registerAllocation 
                                          = registerAllocation}

                  val {operand = final_dst,
                       assembly = assembly_dst,
                       registerAllocation = registerAllocation}
                    = RA.allocateOperand {operand = dst,
                                          options = {register = true,
                                                     immediate = NONE,
                                                     label = false,
                                                     address = false},
                                          info = info,
                                          size = size,
                                          move = false, 
                                          supports = [],
                                          saves = [src,final_src],
                                          force = [],
                                          registerAllocation 
                                          = registerAllocation}

                  val instruction
                    = Instruction.LEA
                      {src = final_src,
                       dst = final_dst,
                       size = size}

                  val {uses = final_uses,
                       defs = final_defs,  
                       ...}
                    = Instruction.uses_defs_kills instruction

                  val {assembly = assembly_post,
                       registerAllocation}
                    = RA.post {uses = uses,
                               final_uses = final_uses,
                               defs = defs,
                               final_defs = final_defs,
                               kills = kills,
                               info = info,
                               registerAllocation = registerAllocation}
                in
                  {assembly 
                   = AppendList.appends 
                     [assembly_pre,
                      assembly_src,
                      assembly_dst,
                      AppendList.single
                      (Assembly.instruction instruction),
                      assembly_post],
                   registerAllocation = registerAllocation}
                end
             | SSE_BinAS {oper, src, dst, size}
               (* SSE scalar binary arithmetic instructions.
                * Require src/dst operands as follows:
                *
                *              dst
                *          reg xmm imm lab add 
                *      reg
                *      xmm      X
                *  src imm
                *      lab
                *      add      X
                *)
             => let
                  val {uses,defs,kills} 
                    = Instruction.uses_defs_kills instruction
                  val {assembly = assembly_pre,
                       registerAllocation}
                    = RA.pre {uses = uses,
                              defs = defs,
                              kills = kills,
                              info = info,
                              registerAllocation = registerAllocation}

                  fun default ()
                    = let
                        val {final_src,
                             final_dst,
                             assembly_src_dst,
                             registerAllocation}
                          = allocateXmmSrcDst {src = src,
                                               dst = dst,
                                               move_dst = true,
                                               size = size,
                                               info = info,
                                               registerAllocation = registerAllocation}

                        val instruction 
                          = Instruction.SSE_BinAS
                            {oper = oper,
                             src = final_src,
                             dst = final_dst,
                             size = size}

                        val {uses = final_uses,
                             defs = final_defs,
                             ...}
                          = Instruction.uses_defs_kills instruction

                        val {assembly = assembly_post,
                             registerAllocation}
                          = RA.post {uses = uses,
                                     final_uses = final_uses,
                                     defs = defs,
                                     final_defs = final_defs,
                                     kills = kills,
                                     info = info,
                                     registerAllocation = registerAllocation}
                      in
                        {assembly
                         = AppendList.appends 
                           [assembly_pre,
                            assembly_src_dst,
                            AppendList.single
                            (Assembly.instruction instruction),
                            assembly_post],
                           registerAllocation = registerAllocation}
                      end
                in
                  default ()
                end
             | SSE_UnAS {oper, src, dst, size}
               (* SSE scalar unary arithmetic instructions.
                * Require src/dst operands as follows:
                *
                *              dst
                *          reg xmm imm lab add 
                *      reg
                *      xmm      X
                *  src imm
                *      lab
                *      add      X
                *)
             => let
                  val {uses,defs,kills} 
                    = Instruction.uses_defs_kills instruction
                  val {assembly = assembly_pre,
                       registerAllocation}
                    = RA.pre {uses = uses,
                              defs = defs,
                              kills = kills,
                              info = info,
                              registerAllocation = registerAllocation}

                  fun default ()
                    = let
                        val {final_src,
                             final_dst,
                             assembly_src_dst,
                             registerAllocation}
                          = allocateXmmSrcDst {src = src,
                                               dst = dst,
                                               move_dst = false,
                                               size = size,
                                               info = info,
                                               registerAllocation = registerAllocation}

                        val instruction 
                          = Instruction.SSE_UnAS
                            {oper = oper,
                             src = final_src,
                             dst = final_dst,
                             size = size}

                        val {uses = final_uses,
                             defs = final_defs,
                             ...}
                          = Instruction.uses_defs_kills instruction

                        val {assembly = assembly_post,
                             registerAllocation}
                          = RA.post {uses = uses,
                                     final_uses = final_uses,
                                     defs = defs,
                                     final_defs = final_defs,
                                     kills = kills,
                                     info = info,
                                     registerAllocation = registerAllocation}
                      in
                        {assembly
                         = AppendList.appends 
                           [assembly_pre,
                            assembly_src_dst,
                            AppendList.single
                            (Assembly.instruction instruction),
                            assembly_post],
                           registerAllocation = registerAllocation}
                      end
                in
                  default ()
                end
             | SSE_BinLP {oper, src, dst, size}
               (* Packed SSE binary logical instructions (used as scalar).
                * Require src/dst operands as follows:
                *
                *              dst
                *          reg xmm imm lab add 
                *      reg
                *      xmm      X
                *  src imm
                *      lab
                *      add     (x)
                *
                * Disallow address for src, since it would be a 128-bit load.
                *)
             => let
                  val {uses,defs,kills} 
                    = Instruction.uses_defs_kills instruction
                  val {assembly = assembly_pre,
                       registerAllocation}
                    = RA.pre {uses = uses,
                              defs = defs,
                              kills = kills,
                              info = info,
                              registerAllocation = registerAllocation}

                  fun default ()
                    = let
                        val {final_src,
                             final_dst,
                             assembly_src_dst,
                             registerAllocation}
                          = allocateXmmSrcDstReg {src = src,
                                                  dst = dst,
                                                  move_dst = true,
                                                  size = size,
                                                  info = info,
                                                  registerAllocation = registerAllocation}

                        val instruction 
                          = Instruction.SSE_BinLP
                            {oper = oper,
                             src = final_src,
                             dst = final_dst,
                             size = size}

                        val {uses = final_uses,
                             defs = final_defs,
                             ...}
                          = Instruction.uses_defs_kills instruction

                        val {assembly = assembly_post,
                             registerAllocation}
                          = RA.post {uses = uses,
                                     final_uses = final_uses,
                                     defs = defs,
                                     final_defs = final_defs,
                                     kills = kills,
                                     info = info,
                                     registerAllocation = registerAllocation}
                      in
                        {assembly
                         = AppendList.appends 
                           [assembly_pre,
                            assembly_src_dst,
                            AppendList.single
                            (Assembly.instruction instruction),
                            assembly_post],
                           registerAllocation = registerAllocation}
                      end
                in
                  default ()
                end
             | SSE_MOVS {src, dst, size}
               (* Scalar SSE move instruction.
                * Require src/dst operands as follows:
                *
                *              dst
                *          reg xmm imm lab add 
                *      reg
                *      xmm      X           X
                *  src imm
                *      lab
                *      add      X
                *)
             => let
                  val {uses,defs,kills} 
                    = Instruction.uses_defs_kills instruction
                  val {assembly = assembly_pre,
                       registerAllocation}
                    = RA.pre {uses = uses,
                              defs = defs,
                              kills = kills,
                              info = info,
                              registerAllocation = registerAllocation}

                  fun default ()
                    = let
                        val {final_src,
                             final_dst,
                             assembly_src_dst,
                             registerAllocation}
                          = allocateXmmSrcDst
                            {src = src,
                             dst = dst,
                             move_dst = false,
                             size = size,
                             info = info,
                             registerAllocation = registerAllocation}

                        val instruction
                          = Instruction.SSE_MOVS
                            {src = final_src,
                             dst = final_dst,
                             size = size}

                        val {uses = final_uses,
                             defs = final_defs,  
                             ...}
                          = Instruction.uses_defs_kills instruction

                        val {assembly = assembly_post,
                             registerAllocation}
                          = RA.post {uses = uses,
                                     final_uses = final_uses,
                                     defs = defs,
                                     final_defs = final_defs,
                                     kills = kills,
                                     info = info,
                                     registerAllocation = registerAllocation}
                      in
                        {assembly 
                         = AppendList.appends 
                           [assembly_pre,
                            assembly_src_dst,
                            AppendList.single
                            (Assembly.instruction instruction),
                            assembly_post],
                         registerAllocation = registerAllocation}
                      end

                  fun default' ({register = register_src,
                                 commit = commit_src,
                                 ...} : RegisterAllocation.xmmvalue,
                                memloc_dst)
                    = let
                        val registerAllocation
                          = RA.xmmremove
                            {memloc = memloc_dst,
                             registerAllocation = registerAllocation}

                        val registerAllocation
                          = RA.xmmupdate
                            {value = {register = register_src,
                                      memloc = memloc_dst,
                                      weight = 1024,
                                      sync = false,
                                      commit = commit_src},
                             registerAllocation = registerAllocation}

                        val final_uses = []
                        val final_defs 
                          = [Operand.xmmregister register_src]

                        val {assembly = assembly_post,
                             registerAllocation}
                          = RA.post {uses = uses,
                                     final_uses = final_uses,
                                     defs = defs,
                                     final_defs = final_defs,
                                     kills = kills,
                                     info = info,
                                     registerAllocation = registerAllocation}
                      in
                        {assembly 
                         = AppendList.appends [assembly_pre,
                                               assembly_post],
                         registerAllocation = registerAllocation}
                      end

                  fun default'' (memloc_dst)
                    = let
                        val registerAllocation
                          = RA.xmmremove
                            {memloc = memloc_dst,
                             registerAllocation = registerAllocation}

                        val {final_src,
                             final_dst,
                             assembly_src_dst,
                             registerAllocation}
                          = allocateXmmSrcDst
                            {src = src,
                             dst = dst,
                             move_dst = false,
                             size = size,
                             info = info,
                             registerAllocation = registerAllocation}

                        val instruction
                          = Instruction.SSE_MOVS
                            {src = final_src,
                             dst = final_dst,
                             size = size}

                        val {uses = final_uses,
                             defs = final_defs,
                             ...}
                          = Instruction.uses_defs_kills instruction

                        val {assembly = assembly_post,
                             registerAllocation}
                          = RA.post {uses = uses,
                                     final_uses = final_uses,
                                     defs = defs,
                                     final_defs = final_defs,
                                     kills = kills,
                                     info = info,
                                     registerAllocation = registerAllocation}
                      in
                        {assembly 
                         = AppendList.appends 
                           [assembly_pre,
                            assembly_src_dst,
                            AppendList.single
                            (Assembly.instruction instruction),
                            assembly_post],
                         registerAllocation = registerAllocation}
                      end

                  val memloc_src = Operand.deMemloc src
                  val value_src 
                    = case memloc_src
                        of NONE => NONE
                         | SOME memloc_src
                         => RA.xmmallocated {memloc = memloc_src,
                                              registerAllocation 
                                              = registerAllocation}
                  val memloc_dst = Operand.deMemloc dst
                in
                  case memloc_dst
                    of SOME memloc_dst
                     => if MemLocSet.contains(remove,memloc_dst)
                          then (case memloc_src
                                  of SOME memloc_src
                                   => if List.contains
                                         (memloc_src::(MemLoc.utilized memloc_src),
                                          memloc_dst,
                                          MemLoc.eq)
                                        then default ()
                                        else default'' memloc_dst
                                   | NONE => default'' memloc_dst)
                          else (case value_src
                                  of SOME (value_src as {memloc = memloc_src,
                                                         sync = sync_src, ...})
                                    => if MemLocSet.contains(dead,memloc_src)
                                          orelse
                                          (MemLocSet.contains(remove,memloc_src)
                                           andalso
                                           sync_src)
                                         then default' (value_src, memloc_dst)
                                         else default ()
                                    | NONE => default ())
                     | NONE => default ()
                end
             | SSE_COMIS {src1, src2, size}
               (* Scalar SSE compare instruction.
                * Require src1/src2 operands as follows:
                *
                *               src2
                *           reg xmm imm lab add 
                *       reg
                *       xmm      X
                *  src1 imm
                *       lab
                *       add      X
                *
                * Require size modifier class as follows: FLT
                *)
             => let
                  val {uses,defs,kills} 
                    = Instruction.uses_defs_kills instruction
                  val {assembly = assembly_pre,
                       registerAllocation}
                    = RA.pre {uses = uses,
                              defs = defs,
                              kills = kills,
                              info = info,
                              registerAllocation = registerAllocation}

                  val {final_src1,
                       final_src2,
                       assembly_src1_src2,
                       registerAllocation}
                    = allocateXmmSrc1Src2 
                      {src1 = src1,
                       src2 = src2,
                       size = size,
                       info = info,
                       registerAllocation = registerAllocation}

                  val instruction
                    = Instruction.SSE_COMIS
                      {src1 = final_src1,
                       src2 = final_src2,
                       size = size}

                  val {uses = final_uses,
                       defs = final_defs,  
                       ...}
                    = Instruction.uses_defs_kills instruction

                  val {assembly = assembly_post,
                       registerAllocation}
                    = RA.post {uses = uses,
                               final_uses = final_uses,
                               defs = defs,
                               final_defs = final_defs,
                               kills = kills,
                               info = info,
                               registerAllocation = registerAllocation}
                in
                  {assembly 
                   = AppendList.appends 
                     [assembly_pre,
                      assembly_src1_src2,
                      AppendList.single
                      (Assembly.instruction instruction),
                      assembly_post],
                   registerAllocation = registerAllocation}
                end
             | SSE_UCOMIS {src1, src2, size}
               (* Scalar SSE unordered compare instruction.
                * Require src1/src2 operands as follows:
                *
                *               src2
                *           reg xmm imm lab add 
                *       reg
                *       xmm      X
                *  src1 imm
                *       lab
                *       add      X
                *
                * Require size modifier class as follows: FLT
                *)
             => let
                  val {uses,defs,kills} 
                    = Instruction.uses_defs_kills instruction
                  val {assembly = assembly_pre,
                       registerAllocation}
                    = RA.pre {uses = uses,
                              defs = defs,
                              kills = kills,
                              info = info,
                              registerAllocation = registerAllocation}

                  val {final_src1,
                       final_src2,
                       assembly_src1_src2,
                       registerAllocation}
                    = allocateXmmSrc1Src2 
                      {src1 = src1,
                       src2 = src2,
                       size = size,
                       info = info,
                       registerAllocation = registerAllocation}

                  val instruction
                    = Instruction.SSE_UCOMIS
                      {src1 = final_src1,
                       src2 = final_src2,
                       size = size}

                  val {uses = final_uses,
                       defs = final_defs,  
                       ...}
                    = Instruction.uses_defs_kills instruction

                  val {assembly = assembly_post,
                       registerAllocation}
                    = RA.post {uses = uses,
                               final_uses = final_uses,
                               defs = defs,
                               final_defs = final_defs,
                               kills = kills,
                               info = info,
                               registerAllocation = registerAllocation}
                in
                  {assembly 
                   = AppendList.appends 
                     [assembly_pre,
                      assembly_src1_src2,
                      AppendList.single
                      (Assembly.instruction instruction),
                      assembly_post],
                   registerAllocation = registerAllocation}
                end
             | SSE_CVTSFP2SFP {src, srcsize, dst, dstsize, ...}
               (* Scalar SSE floating-point/floating-point convert instruction.
                * Require src/dst operands as follows:
                *
                *              dst
                *          reg xmm imm lab add 
                *      reg
                *      xmm      X
                *  src imm
                *      lab
                *      add      X
                *
                * Require srcsize/dstsize modifier class as follows: FLT != FLT
                *)
             => let
                  val {uses,defs,kills} 
                    = Instruction.uses_defs_kills instruction
                  val {assembly = assembly_pre,
                       registerAllocation}
                    = RA.pre {uses = uses,
                              defs = defs,
                              kills = kills,
                              info = info,
                              registerAllocation = registerAllocation}

                   val {operand = final_src,
                        assembly = assembly_src,
                        registerAllocation}
                     = RA.allocateXmmOperand 
                       {operand = src,
                        options = {xmmregister = true,
                                   address = true},
                        info = info,
                        size = srcsize,
                        move = true,
                        supports = [dst],
                        saves = [],
                        force = [],
                        registerAllocation 
                        = registerAllocation}

                   val {operand = final_dst,
                        assembly = assembly_dst,
                        registerAllocation}
                     = RA.allocateXmmOperand 
                       {operand = dst,
                        options = {xmmregister = true,
                                   address = false},
                        info = info,
                        size = dstsize,
                        move = false,
                        supports = [src,final_src],
                        saves = [],
                        force = [],
                        registerAllocation 
                        = registerAllocation}

                  val instruction
                    = Instruction.SSE_CVTSFP2SFP
                      {src = final_src,
                       srcsize = srcsize,
                       dst = final_dst,
                       dstsize = dstsize}

                  val {uses = final_uses,
                       defs = final_defs,  
                       ...}
                    = Instruction.uses_defs_kills instruction

                  val {assembly = assembly_post,
                       registerAllocation}
                    = RA.post {uses = uses,
                               final_uses = final_uses,
                               defs = defs,
                               final_defs = final_defs,
                               kills = kills,
                               info = info,
                               registerAllocation = registerAllocation}
                in
                  {assembly 
                   = AppendList.appends 
                     [assembly_pre,
                      assembly_src,
                      assembly_dst,
                      AppendList.single
                      (Assembly.instruction instruction),
                      assembly_post],
                   registerAllocation = registerAllocation}
                end
             | SSE_CVTSFP2SI {src, srcsize, dst, dstsize, ...}
               (* Scalar SSE floating-point/signed-integer convert instruction.
                * Require src/dst operands as follows:
                *
                *              dst
                *          reg xmm imm lab add 
                *      reg
                *      xmm  X
                *  src imm
                *      lab
                *      add  X
                *
                * Require srcsize/dstsize modifier class as follows: FLT/INT
                *)
             => let
                  val {uses,defs,kills} 
                    = Instruction.uses_defs_kills instruction
                  val {assembly = assembly_pre,
                       registerAllocation}
                    = RA.pre {uses = uses,
                              defs = defs,
                              kills = kills,
                              info = info,
                              registerAllocation = registerAllocation}

                   val {operand = final_src,
                        assembly = assembly_src,
                        registerAllocation}
                     = RA.allocateXmmOperand 
                       {operand = src,
                        options = {xmmregister = true,
                                   address = true},
                        info = info,
                        size = srcsize,
                        move = true,
                        supports = [dst],
                        saves = [],
                        force = [],
                        registerAllocation 
                        = registerAllocation}

                   val {operand = final_dst,
                        assembly = assembly_dst,
                        registerAllocation}
                     = RA.allocateOperand 
                       {operand = dst,
                        options = {register = true,
                                   immediate = NONE,
                                   label = false,
                                   address = false},
                        info = info,
                        size = dstsize,
                        move = false,
                        supports = [src,final_src],
                        saves = [],
                        force = [],
                        registerAllocation 
                        = registerAllocation}

                  val instruction
                    = Instruction.SSE_CVTSFP2SI
                      {src = final_src,
                       srcsize = srcsize,
                       dst = final_dst,
                       dstsize = dstsize}

                  val {uses = final_uses,
                       defs = final_defs,  
                       ...}
                    = Instruction.uses_defs_kills instruction

                  val {assembly = assembly_post,
                       registerAllocation}
                    = RA.post {uses = uses,
                               final_uses = final_uses,
                               defs = defs,
                               final_defs = final_defs,
                               kills = kills,
                               info = info,
                               registerAllocation = registerAllocation}
                in
                  {assembly 
                   = AppendList.appends 
                     [assembly_pre,
                      assembly_src,
                      assembly_dst,
                      AppendList.single
                      (Assembly.instruction instruction),
                      assembly_post],
                   registerAllocation = registerAllocation}
                end
             | SSE_CVTSI2SFP {src, srcsize, dst, dstsize, ...}
               (* Scalar SSE floating-point/signed-integer convert instruction.
                * Require src/dst operands as follows:
                *
                *              dst
                *          reg xmm imm lab add 
                *      reg      X
                *      xmm
                *  src imm
                *      lab
                *      add      X
                *
                * Require srcsize/dstsize modifier class as follows: INT/FLT
                *)
             => let
                  val {uses,defs,kills} 
                    = Instruction.uses_defs_kills instruction
                  val {assembly = assembly_pre,
                       registerAllocation}
                    = RA.pre {uses = uses,
                              defs = defs,
                              kills = kills,
                              info = info,
                              registerAllocation = registerAllocation}

                   val {operand = final_src,
                        assembly = assembly_src,
                        registerAllocation}
                     = RA.allocateOperand 
                       {operand = src,
                        options = {register = true,
                                   immediate = NONE,
                                   label = false,
                                   address = true},
                        info = info,
                        size = srcsize,
                        move = true,
                        supports = [dst],
                        saves = [],
                        force = [],
                        registerAllocation 
                        = registerAllocation}

                   val {operand = final_dst,
                        assembly = assembly_dst,
                        registerAllocation}
                     = RA.allocateXmmOperand 
                       {operand = dst,
                        options = {xmmregister = true,
                                   address = false},
                        info = info,
                        size = dstsize,
                        move = false,
                        supports = [src,final_src],
                        saves = [],
                        force = [],
                        registerAllocation 
                        = registerAllocation}

                  val instruction
                    = Instruction.SSE_CVTSI2SFP
                      {src = final_src,
                       srcsize = srcsize,
                       dst = final_dst,
                       dstsize = dstsize}

                  val {uses = final_uses,
                       defs = final_defs,  
                       ...}
                    = Instruction.uses_defs_kills instruction

                  val {assembly = assembly_post,
                       registerAllocation}
                    = RA.post {uses = uses,
                               final_uses = final_uses,
                               defs = defs,
                               final_defs = final_defs,
                               kills = kills,
                               info = info,
                               registerAllocation = registerAllocation}
                in
                  {assembly 
                   = AppendList.appends 
                     [assembly_pre,
                      assembly_src,
                      assembly_dst,
                      AppendList.single
                      (Assembly.instruction instruction),
                      assembly_post],
                   registerAllocation = registerAllocation}
                end
             | SSE_MOVD {src, srcsize, dst, dstsize, ...}
               (* Scalar SSE move data instruction.
                * Require src/dst operands as follows:
                *
                *              dst
                *          reg xmm imm lab add 
                *      reg      X
                *      xmm  X               X
                *  src imm
                *      lab
                *      add      X
                *
                * Require size modifier class as follows: FLT/INT
                *)
             => let
                  val {uses,defs,kills} 
                    = Instruction.uses_defs_kills instruction
                  val {assembly = assembly_pre,
                       registerAllocation}
                    = RA.pre {uses = uses,
                              defs = defs,
                              kills = kills,
                              info = info,
                              registerAllocation = registerAllocation}

                   val {operand = final_src,
                        assembly = assembly_src,
                        registerAllocation}
                     = let
                          fun doitINT () =
                             RA.allocateOperand 
                             {operand = src,
                              options = {register = true,
                                         immediate = NONE,
                                         label = false,
                                         address = false},
                              info = info,
                              size = srcsize,
                              move = true,
                              supports = [dst],
                              saves = [],
                              force = [],
                              registerAllocation 
                              = registerAllocation}
                          fun doitFLT () =
                             RA.allocateXmmOperand 
                             {operand = src,
                              options = {xmmregister = true,
                                         address = false},
                              info = info,
                              size = srcsize,
                              move = true,
                              supports = [dst],
                              saves = [],
                              force = [],
                              registerAllocation 
                              = registerAllocation}
                       in
                          case src of
                             Operand.MemLoc memloc =>
                                (case Size.class (MemLoc.size memloc) of
                                    Size.INT => doitINT ()
                                  | Size.FLT => doitFLT ())
                           | Operand.Immediate _ => doitINT ()
                           | _ => Error.bug "amd64AllocateRegisters.Instruction.allocateRegisters: SSE_MOVD, src"
                       end

                   val {operand = final_dst,
                        assembly = assembly_dst,
                        registerAllocation}
                     = let
                          fun doitINT () =
                             RA.allocateOperand 
                             {operand = dst,
                              options = {register = true,
                                         immediate = NONE,
                                         label = false,
                                         address = false},
                              info = info,
                              size = dstsize,
                              move = false,
                              supports = [src,final_src],
                              saves = [],
                              force = [],
                              registerAllocation 
                              = registerAllocation}
                          fun doitFLT () =
                             RA.allocateXmmOperand 
                             {operand = dst,
                              options = {xmmregister = true,
                                         address = false},
                              info = info,
                              size = dstsize,
                              move = false,
                              supports = [src,final_src],
                              saves = [],
                              force = [],
                              registerAllocation 
                              = registerAllocation}
                       in
                          case dst of
                             Operand.MemLoc memloc =>
                                (case Size.class (MemLoc.size memloc) of
                                    Size.INT => doitINT ()
                                  | Size.FLT => doitFLT ())
                           | _ => Error.bug "amd64AllocateRegisters.Instruction.allocateRegisters: SSE_MOVD, dst"
                       end

                  val instruction
                    = Instruction.SSE_MOVD
                      {src = final_src,
                       srcsize = srcsize,
                       dst = final_dst,
                       dstsize = dstsize}

                  val {uses = final_uses,
                       defs = final_defs,  
                       ...}
                    = Instruction.uses_defs_kills instruction

                  val {assembly = assembly_post,
                       registerAllocation}
                    = RA.post {uses = uses,
                               final_uses = final_uses,
                               defs = defs,
                               final_defs = final_defs,
                               kills = kills,
                               info = info,
                               registerAllocation = registerAllocation}
                in
                  {assembly 
                   = AppendList.appends 
                     [assembly_pre,
                      assembly_src,
                      assembly_dst,
                      AppendList.single
                      (Assembly.instruction instruction),
                      assembly_post],
                   registerAllocation = registerAllocation}
                end
             | _ => Error.bug (concat ["amd64AllocateRegisters.Instruction.allocateRegisters: unimplemented: ", Instruction.toString instruction])

      val (allocateRegisters, allocateRegisters_msg)
        = tracer
          "Instruction.allocateRegisters"
          allocateRegisters
     end

  structure Directive =
    struct
      open Directive

      fun allocateRegisters {directive, info, registerAllocation}
        = let 
            val {assembly, registerAllocation}
              = case directive
                  of Assume {assumes}
                   => RegisterAllocation.assume 
                      {assumes = assumes,
                       info = info,
                       registerAllocation = registerAllocation}
                   | XmmAssume {assumes}
                   => RegisterAllocation.xmmassume
                      {assumes = assumes,
                       info = info,
                       registerAllocation = registerAllocation}
                   | Cache {caches}
                   => RegisterAllocation.cache 
                      {caches = caches,
                       info = info,
                       registerAllocation = registerAllocation}
                   | XmmCache {caches}
                   => RegisterAllocation.xmmcache
                      {caches = caches,
                       info = info,
                       registerAllocation = registerAllocation}
                   | Reset
                   => RegisterAllocation.reset 
                      {registerAllocation = registerAllocation}
                   | Force {commit_memlocs, commit_classes, 
                            remove_memlocs, remove_classes, 
                            dead_memlocs, dead_classes}
                   => RegisterAllocation.force
                      {commit_memlocs = commit_memlocs,
                       commit_classes = commit_classes,
                       remove_memlocs = remove_memlocs,
                       remove_classes = remove_classes,
                       dead_memlocs = dead_memlocs,
                       dead_classes = dead_classes,
                       info = info,
                       registerAllocation = registerAllocation}
                   | CCall
                   => RegisterAllocation.ccall
                      {info = info,
                       registerAllocation = registerAllocation}
                   | Return {returns}
                   => RegisterAllocation.return
                      {returns = returns,
                       info = info,
                       registerAllocation = registerAllocation}
                   | Reserve {registers}
                   => RegisterAllocation.reserve 
                      {registers = registers,
                       registerAllocation = registerAllocation}
                   | XmmReserve {registers}
                   => RegisterAllocation.xmmreserve 
                      {registers = registers,
                       registerAllocation = registerAllocation}
                   | Unreserve {registers}
                   => RegisterAllocation.unreserve 
                      {registers = registers,
                       registerAllocation = registerAllocation}
                   | XmmUnreserve {registers}
                   => RegisterAllocation.xmmunreserve 
                      {registers = registers,
                       registerAllocation = registerAllocation}
                   | SaveRegAlloc {live, id}
                   => RegisterAllocation.saveregalloc 
                      {live = live,
                       id = id,
                       info = info,
                       registerAllocation = registerAllocation}
                   | RestoreRegAlloc {live, id}
                   => RegisterAllocation.restoreregalloc
                      {live = live,
                       id = id,
                       info = info,
                       registerAllocation = registerAllocation}
          in
            {assembly = assembly,
             registerAllocation = registerAllocation}
          end

      val (allocateRegisters, allocateRegisters_msg)
        = tracer
          "Directive.allocateRegisters"
          allocateRegisters
    end

  structure Assembly =
    struct
      open Assembly

      fun allocateRegisters {assembly: (t * Liveness.t) list,
                             registerAllocation: RegisterAllocation.t}
        = let
            val {assembly, registerAllocation}
              = List.fold
                (assembly,
                 {assembly = AppendList.empty,
                  registerAllocation = registerAllocation},
                 fn ((Comment s,_), {assembly, registerAllocation})
                  => {assembly = AppendList.snoc
                                 (assembly,
                                  Comment s),
                      registerAllocation = registerAllocation}
                   | ((Directive d,info), {assembly, registerAllocation})
                   => let
                        val {assembly = assembly',
                             registerAllocation}
                          = Directive.allocateRegisters 
                            {directive = d,
                             info = info,
                             registerAllocation = registerAllocation}

                        val assembly''
                          = AppendList.appends
                            [if !Control.Native.commented > 1
                               then AppendList.fromList
                                    [Assembly.comment
                                     (String.make (60, #"*")),
                                     (Assembly.comment
                                      (Directive.toString d))]
                               else AppendList.empty,
                             if !Control.Native.commented > 4
                               then AppendList.fromList
                                    (Liveness.toComments info)
                               else AppendList.empty,
                             assembly',
                             if !Control.Native.commented > 5
                               then (RegisterAllocation.toComments 
                                     registerAllocation)
                               else AppendList.empty]
                      in
                        {assembly = AppendList.append 
                                    (assembly, 
                                     assembly''),
                         registerAllocation = registerAllocation}
                      end
                   | ((PseudoOp p,_), {assembly, registerAllocation})
                   => {assembly = AppendList.snoc
                                  (assembly,
                                   PseudoOp p),
                       registerAllocation = registerAllocation}
                   | ((Label l,_), {assembly, registerAllocation})
                   => {assembly = AppendList.snoc
                                  (assembly,
                                   Label l),
                       registerAllocation = registerAllocation}
                   | ((Instruction i,info), {assembly, registerAllocation})
                   => let
                        val {assembly = assembly',
                             registerAllocation}
                          = Instruction.allocateRegisters 
                            {instruction = i,
                             info = info,
                             registerAllocation = registerAllocation}

                        val assembly''
                          = AppendList.appends
                            [if !Control.Native.commented > 1
                               then AppendList.fromList
                                    [Assembly.comment
                                     (String.make (60, #"*")),
                                     (Assembly.comment
                                      (Instruction.toString i))]
                               else AppendList.empty,
                             if !Control.Native.commented > 4
                               then AppendList.fromList
                                    (Liveness.toComments info)
                               else AppendList.empty,
                             assembly',
                             if !Control.Native.commented > 5
                               then (RegisterAllocation.toComments 
                                     registerAllocation)
                               else AppendList.empty]
                      in
                        {assembly = AppendList.append
                                    (assembly,
                                     assembly''),
                         registerAllocation = registerAllocation}
                      end)

            val assembly = AppendList.toList assembly
            val assembly = if !Control.Native.commented > 1
                             then (Assembly.comment
                                   (String.make (60, #"&"))::
                                   Assembly.comment
                                   (String.make (60, #"&"))::
                                   assembly)
                             else assembly
          in
            {assembly = assembly,
             registerAllocation = registerAllocation}
          end

      val (allocateRegisters, allocateRegisters_msg)
        = tracer
          "Assembly.allocateRegisters"
          allocateRegisters
    end

  fun allocateRegisters {assembly : Assembly.t list list,
                         liveness : bool} :
                        Assembly.t list list
    = let
        val {get = getInfo : Label.t -> Label.t option,
             set = setInfo, ...}
          = Property.getSetOnce
            (Label.plist,
             Property.initConst NONE)

        fun unroll label
          = case getInfo label
              of NONE => label
               | SOME label' => unroll label'

        val assembly
          = List.fold
            (assembly,
             [],
             fn (assembly,assembly')
              => let
                   val assembly
                     = if liveness
                         then Liveness.toLiveness assembly
                         else Liveness.toNoLiveness assembly

                   val {assembly, ...}
                     = Assembly.allocateRegisters
                       {assembly = assembly,
                        registerAllocation
                        = RegisterAllocation.empty ()}

                   val rec doit 
                     = fn (Assembly.Comment _)::assembly 
                        => doit assembly
                        | (Assembly.PseudoOp (PseudoOp.P2align _))::assembly
                        => doit' (assembly, [])
                        | _ => false
                   and doit'
                     = fn ((Assembly.Comment _)::assembly, labels) 
                        => doit' (assembly, labels)
                        | ((Assembly.PseudoOp (PseudoOp.Local _))::assembly, labels)
                        => doit' (assembly, labels)
                        | ((Assembly.Label l)::assembly, labels)
                        => doit' (assembly, l::labels)
                        | (assembly, labels) => doit'' (assembly, labels)
                   and doit''
                     = fn ((Assembly.Comment _)::assembly, labels)
                        => doit'' (assembly, labels)
                        | ((Assembly.Instruction 
                            (Instruction.JMP 
                             {target = Operand.Label label, 
                              absolute = false}))::assembly, labels)
                        => doit''' (assembly, labels, label)
                        | _ => false
                   and doit'''
                     = fn ([], labels, label)
                        => let
                             val label' = unroll label
                           in
                             if List.contains(labels, label', Label.equals)
                               then false
                               else (List.foreach
                                     (labels,
                                      fn label'' => setInfo(label'', SOME label'));
                                     true)
                           end
                        | ((Assembly.Comment _)::assembly, labels, label)
                        => doit''' (assembly, labels, label)
                        | _ => false
                 in
                   if doit assembly
                     then assembly'
                     else assembly::assembly'
                 end)

        fun replacer _ oper
          = (case (Operand.deImmediate oper, Operand.deLabel oper)
               of (SOME immediate, _) 
                => (case Immediate.deLabel immediate
                      of SOME label => Operand.immediate_label (unroll label)
                       | NONE => oper)
                | (_, SOME label) => Operand.label (unroll label)
                | _ => oper)

        val assembly
          = List.fold
            (assembly,
             [],
             fn (assembly,assembly')
              => (List.map(assembly, Assembly.replace replacer))::assembly')
      in
        assembly
      end

  val (allocateRegisters, allocateRegisters_msg)
    = tracerTop
      "allocateRegisters"
      allocateRegisters

  fun allocateRegisters_totals ()
    = (allocateRegisters_msg ();
       Control.indent ();
       Liveness.toLiveness_msg ();
       Liveness.toNoLiveness_msg ();
       Assembly.allocateRegisters_msg ();
       Control.indent ();
       Instruction.allocateRegisters_msg ();
       Control.indent ();
       RegisterAllocation.pre_msg ();
       RegisterAllocation.post_msg ();
       RegisterAllocation.allocateOperand_msg ();
       RegisterAllocation.allocateXmmOperand_msg ();
       Control.unindent ();
       Directive.allocateRegisters_msg ();
       Control.unindent ();
       Control.unindent())
end
