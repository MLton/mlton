(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor DirectExp2 (S: DIRECT_EXP2_STRUCTS): DIRECT_EXP2 =
struct

open S

structure DirectExp =
struct

datatype t =
   Arith of {prim: Type.t Prim.t,
             args: t vector,
             overflow: t,
             ty: Type.t}
 | Call of {func: Func.t,
            args: t vector,
            ty: Type.t}
 | Case of {cases: cases,
            default: t option,
            test: t,
            ty: Type.t}
 | ConApp of {con: Con.t,
              args: t vector,
              ty: Type.t}
 | Const of Const.t
 | Detuple of {body: Var.t vector -> t,
               length: int,
               tuple: t}
 | DetupleBind of {body: t,
                   components: Var.t vector,
                   tuple: Var.t,
                   tupleTy: Type.t}
 | Handle of {try: t,
              catch: Var.t * Type.t,
              handler: t,
              ty: Type.t}
 | Let of {decs: {var: Var.t, exp: t} list,
           body: t}
 | Name of t * (Var.t -> t)
 | PrimApp of {prim: Type.t Prim.t,
               targs: Type.t vector,
               args: t vector,
               ty: Type.t}
 | Profile of ProfileExp.t
 | Raise of t
 | Runtime of {args: t vector,
               prim: Type.t Prim.t,
               ty: Type.t}
 | Select of {tuple: t,
              offset: int,
              ty: Type.t}
 | Seq of t * t
 | Tuple of {exps: t vector,
             ty: Type.t}
 | Var of Var.t * Type.t
and cases =
   Con of {con: Con.t,
           args: (Var.t * Type.t) vector,
           body: t} vector
 | Word of WordSize.t * (WordX.t * t) vector

val arith = Arith
val call = Call
val casee = Case
val conApp = ConApp
val const = Const
val detuple = Detuple
val detupleBind = DetupleBind
val handlee = Handle
val lett = Let
val name = Name
val profile = Profile
val raisee = Raise
val select = Select
val seq = Seq
val word = Const o Const.word

fun tuple (r as {exps, ...}) =
   if 1 = Vector.length exps
      then Vector.first exps
   else Tuple r

val var = Var

fun primApp {args, prim, targs, ty} =
   let
      fun runtime () =
         Runtime {args = args,
                  prim = prim,
                  ty = ty}
   in
      case Prim.name prim of
         Prim.Name.MLton_halt => runtime ()
       | Prim.Name.Thread_copyCurrent => runtime ()
       | _ => PrimApp {args = args,
                       prim = prim,
                       targs = targs,
                       ty = ty}
   end

local
   fun make c = conApp {con = c, args = Vector.new0 (), ty = Type.bool}
in
   val truee = make Con.truee
   val falsee = make Con.falsee
end

fun eq (e1, e2, ty) =
   primApp {prim = Prim.eq,
            targs = Vector.new1 ty,
            args = Vector.new2 (e1, e2),
            ty = Type.bool}

local
   open Layout
   fun lett (decs, body) =
      align [seq [str "let ", decs],
             seq [str "in ", body],
             str "end"]
in
   fun layout e : Layout.t =
      case e of
         Arith {prim, args, overflow, ...} =>
            align [Prim.layoutApp (prim, args, layout),
                   seq [str "Overflow => ", layout overflow]]
       | Call {func, args, ty} =>
            seq [Func.layout func, str " ", layouts args,
                 str ": ", Type.layout ty]
       | Case {cases, default, test, ...} =>
            align
            [seq [str "case ", layout test, str " of"],
             indent
             (align [let
                        fun doit (v, f) =
                           Vector.layout
                           (fn z =>
                            let
                               val (x, e) = f z
                            in
                               seq [str "| ", x, str " => ", layout e]
                            end)
                           v
                        fun simple (v, f) =
                           doit (v, (fn (x, e) => (f x, e)))
                     in
                        case cases of
                           Con v =>
                              doit (v, fn {con, args, body} =>
                                    (seq [Con.layout con,
                                          Vector.layout (Var.layout o #1) args],
                                     body))
                         | Word (_, v) => simple (v, WordX.layout)
                     end,
                        case default of
                           NONE => empty
                         | SOME e => seq [str "  _ => ", layout e]],
              2)]
       | ConApp {con, args, ty} =>
            seq [Con.layout con, layouts args, str ": ", Type.layout ty]
       | Const c => Const.layout c
       | Detuple {tuple, ...} => seq [str "detuple ", layout tuple]
       | DetupleBind {body, components, tuple, ...} =>
            lett (seq [Vector.layout Var.layout components,
                       str " = ", Var.layout tuple],
                  layout body)
       | Handle {try, catch, handler, ...} =>
            align [layout try,
                   seq [str "handle ", Var.layout (#1 catch),
                        str " => ", layout handler]]
       | Let {decs, body} =>
            lett (align
                  (List.map (decs, fn {var, exp} =>
                             seq [Var.layout var, str " = ", layout exp])),
                     layout body)
       | Name _ => str "Name"
       | PrimApp {args, prim, ...} =>
            Prim.layoutApp (prim, args, layout)
       | Profile e => ProfileExp.layout e
       | Raise e => seq [str "raise ", layout e]
       | Runtime {args, prim, ...} =>
            Prim.layoutApp (prim, args, layout)
       | Select {tuple, offset, ...} =>
            seq [str "#", str (Int.toString (1 + offset)), str " ",
                 layout tuple]
       | Seq (e1, e2) => seq [layout e1, str "; ", layout e2]
       | Tuple {exps, ...} => layouts exps
       | Var (x, t) =>
            seq [Var.layout x, str ": ", Type.layout t]
   and layouts es = Vector.layout layout es
end

structure Res =
   struct
      type t = {statements: Statement.t list,
                transfer: Transfer.t}

      fun layout {statements, transfer} =
         let
            open Layout
         in
            align [align (List.map (statements, Statement.layout)),
                   Transfer.layout transfer]
         end

      fun prefix ({statements, transfer}: t, s: Statement.t): t =
         {statements = s :: statements,
          transfer = transfer}
   end

structure Cont:
   sig
      type t

      val bind: Var.t * Res.t -> t
      val goto: Label.t -> t
      val layout: t -> Layout.t
      val receiveExp: (Exp.t * Type.t -> Res.t) -> t
      val receiveVar: (Var.t * Type.t -> Res.t) -> t
      val return: t
      val sendExp: t * Type.t * Exp.t -> Res.t
      val sendVar: t * Type.t * Var.t -> Res.t
      val toBlock: t * Type.t -> Block.t
   end =
   struct
      type bind = {arg: Var.t,
                   statements: Statement.t list,
                   transfer: Transfer.t}

      datatype t =
         Bind of bind
       | Goto of Label.t
       | Prefix of t * Statement.t
       | ReceiveExp of Exp.t * Type.t -> Res.t
       | ReceiveVar of Var.t * Type.t -> Res.t
       | Return

      fun layout (k: t): Layout.t =
         let
            open Layout
         in
            case k of
               Bind {arg, statements, transfer} =>
                  seq [str "Bind ",
                       record [("arg", Var.layout arg),
                               ("statements",
                                List.layout Statement.layout statements),
                               ("transfer", Transfer.layout transfer)]]
             | Goto l => seq [str "Goto ", Label.layout l]
             | Prefix (k, s) => seq [str "Prefix ",
                                     tuple [layout k, Statement.layout s]]
             | ReceiveExp _ => str "ReceiveExp"
             | ReceiveVar _ => str "ReceiveVar"
             | Return => str "Return"
         end

      fun bind (arg, {statements, transfer}) =
         Bind {arg = arg,
               statements = statements,
               transfer = transfer}

      val goto = Goto
      val receiveExp = ReceiveExp
      val receiveVar = ReceiveVar
      val return = Return

      fun toBind (k: t, ty: Type.t): bind =
         case k of
            Bind b => b
          | _ =>
               let
                  val arg = Var.newNoname ()
                  val {statements, transfer} = sendVar (k, ty, arg)
               in
                  {arg = arg,
                   statements = statements,
                   transfer = transfer}
               end
      and sendVar (k: t, ty: Type.t, x: Var.t): Res.t =
         case k of
            Bind b => sendBindExp (b, ty, Exp.Var x)
          | Goto dst => {statements = [],
                         transfer = Transfer.Goto {dst = dst,
                                                   args = Vector.new1 x}}
          | ReceiveExp f => f (Exp.Var x, ty)
          | ReceiveVar f => f (x, ty)
          | Prefix (k, s) => Res.prefix (sendVar (k, ty, x), s)
          | Return => {statements = [],
                       transfer = Transfer.Return (Vector.new1 x)}
      and sendBindExp ({arg, statements, transfer}, ty, e: Exp.t) = 
         {statements = Statement.T {var = SOME arg,
                                    ty = ty,
                                    exp = e} :: statements,
          transfer = transfer}

      val sendVar =
         Trace.trace3 ("DirectExp2.Cont.sendVar", layout, Type.layout, Var.layout,
                       Res.layout)
         sendVar

      val sendExp: t * Type.t * Exp.t -> Res.t =
         fn (k, ty, e) =>
         case k of
            ReceiveExp f => f (e, ty)
          | _ => sendBindExp (toBind (k, ty), ty, e)

      val sendExp =
         Trace.trace3 ("DirectExp2.Cont.sendExp", layout, Type.layout, Exp.layout,
                       Res.layout)
         sendExp

      fun toBlock (k: t, ty: Type.t): Block.t =
         let
            val {arg, statements, transfer} = toBind (k, ty)
            val label = Label.newNoname ()
         in
            Block.T {label = label,
                     args = Vector.new1 (arg, ty),
                     statements = Vector.fromList statements,
                     transfer = transfer}
         end

      val toBlock =
         Trace.trace2 ("DirectExp2.Cont.toBlock", layout, Type.layout, Block.layout)
         toBlock
   end

fun selects (tuple: Var.t, ty: Type.t, components: Var.t vector)
   : Statement.t list =
   let
      val ts = Type.deTuple ty
   in
      Vector.foldi
      (ts, [], fn (i, t, ss) =>
       Statement.T {var = SOME (Vector.sub (components, i)),
                    ty = t,
                    exp = Exp.Select {tuple = tuple,
                                      offset = i}}
       :: ss)
   end

fun linearize' (e: t, h: Handler.t, k: Cont.t): Label.t * Block.t list =
   let
      val traceLinearizeLoop =
         Trace.trace3 ("DirectExp.linearize'.loop", layout, Handler.layout, Cont.layout,
                       Res.layout)
      val blocks: Block.t list ref = ref []
      fun newBlock (args: (Var.t * Type.t) vector,
                    {statements: Statement.t list,
                     transfer: Transfer.t}): Label.t =
         let 
            val label = Label.newNoname ()
            val _ = List.push (blocks,
                               Block.T {label = label,
                                        args = args,
                                        statements = Vector.fromList statements,
                                        transfer = transfer})
         in
            label
         end
      fun reify (k: Cont.t, ty: Type.t): Label.t =
         let
            val b = Cont.toBlock (k, ty)
            val _ = List.push (blocks, b)
         in
            Block.label b
         end
      fun newLabel (args: (Var.t * Type.t) vector,
                    e: t,
                    h: Handler.t,
                    k: Cont.t): Label.t =
         newBlock (args, loop (e, h, k))
      and newLabel0 (e, h, k) = newLabel (Vector.new0 (), e, h, k)
      and loopf (e: t, h: Handler.t, f: Var.t * Type.t -> Res.t) =
         loop (e, h, Cont.receiveVar f)
      and loop arg : Res.t =
         traceLinearizeLoop
         (fn (e: t, h: Handler.t, k: Cont.t) =>
         case e of
            Arith {prim, args, overflow, ty} =>
               loops
               (args, h, fn xs =>
                let
                   val l = reify (k, ty)
                   val k = Cont.goto l
                in
                   {statements = [],
                    transfer =
                    Transfer.Arith {prim = prim,
                                    args = xs,
                                    overflow = newLabel0 (overflow, h, k),
                                    success = l,
                                    ty = ty}}
                end)
          | Call {func, args, ty} =>
               loops
               (args, h, fn xs =>
                {statements = [],
                 transfer = (Transfer.Call
                             {func = func,
                              args = xs,
                              return = Return.NonTail {cont = reify (k, ty),
                                                       handler = h}})})
          | Case {cases, default, test, ty} =>
               let
                  val k = Cont.goto (reify (k, ty))
               in
                  loopf (test, h, fn (x, _) =>
                         {statements = [],
                          transfer =
                          Transfer.Case
                          {test = x,
                           default = Option.map (default, fn e =>
                                                 newLabel0 (e, h, k)),
                           cases =
                           let
                              fun doit v = 
                                 Vector.map (v, fn (c, e) =>
                                             (c, newLabel0 (e, h, k)))
                           in
                              case cases of
                                 Con v =>
                                    Cases.Con
                                    (Vector.map
                                     (v, fn {con, args, body} =>
                                      (con,
                                       newLabel (args, body, h, k))))
                               | Word (s, v) => Cases.Word (s, doit v)
                           end}})
               end
          | ConApp {con, args, ty} =>
               loops (args, h, fn xs =>
                      Cont.sendExp (k, ty, Exp.ConApp {con = con, args = xs}))
          | Const c => Cont.sendExp (k, Type.ofConst c, Exp.Const c)
          | Detuple {tuple, length, body} =>
               loop (tuple, h,
                     Cont.receiveExp
                     (fn (e, ty) =>
                      let
                         fun doit (tuple: Var.t): Res.t =
                            let
                               val (ss, xs) = 
                                  case length of
                                     0 => ([], Vector.new0 ())
                                   | 1 => ([], Vector.new1 tuple)
                                   | _ =>
                                        let
                                           val xs = 
                                              Vector.tabulate
                                              (length, fn _ => Var.newNoname ())
                                        in (selects (tuple, ty, xs), xs)
                                        end
                               val {statements, transfer} = loop (body xs, h, k)
                            in
                               {statements = List.appendRev (ss, statements),
                                transfer = transfer}
                            end
                      in
                         case e of
                            Exp.Tuple xs => loop (body xs, h, k)
                          | Exp.Var x => doit x
                          | _ => 
                               let
                                  val tuple = Var.newNoname ()
                               in
                                  Res.prefix (doit tuple,
                                              Statement.T {var = SOME tuple,
                                                           ty = ty,
                                                           exp = e})
                               end
                      end))
          | DetupleBind {body, components, tuple, tupleTy} =>
               let
                  val {statements, transfer} = loop (body, h, k)
                  val ss =
                     case Vector.length components of
                        0 => []
                      | 1 => [Statement.T
                              {var = SOME (Vector.first components),
                               ty = tupleTy,
                               exp = Exp.Var tuple}]
                      | _ => selects (tuple, tupleTy, components)
               in
                  {statements = List.appendRev (ss, statements),
                   transfer = transfer}
               end
          | Handle {try, catch, handler, ty} =>
               let
                  val k = Cont.goto (reify (k, ty))
                  val hl = Label.newNoname ()
                  val {statements, transfer} = loop (handler, h, k)
                  val _ =
                     List.push (blocks,
                                Block.T {label = hl,
                                         args = Vector.new1 catch,
                                         statements = Vector.fromList statements,
                                         transfer = transfer})
               in
                  loop (try, Handler.Handle hl, k)
               end
          | Let {decs, body} =>
               let
                  fun each decs =
                     case decs of
                        [] => loop (body, h, k)
                      | {var, exp} :: decs =>
                           loop (exp, h, Cont.bind (var, each decs))
               in
                  each decs
               end
          | Name (e, f) => loopf (e, h, fn (x, _) => loop (f x, h, k))
          | PrimApp {prim, targs, args, ty} =>
               loops
               (args, h, fn xs =>
                Cont.sendExp (k, ty, Exp.PrimApp {prim = prim,
                                                  targs = targs,
                                                  args = xs}))
          | Profile e => Cont.sendExp (k, Type.unit, Exp.Profile e)
          | Raise e =>
               loopf (e, h, fn (x, _) =>
                      {statements = [],
                       transfer =
                       (case h of
                           Handler.Caller => Transfer.Raise (Vector.new1 x)
                         | Handler.Dead => Error.bug "DirectExp2.linearize'.loop:  Raise:to dead handler"
                         | Handler.Handle l =>
                              Transfer.Goto {args = Vector.new1 x,
                                             dst = l})})
          | Runtime {args, prim, ty} =>
               loops
               (args, h, fn xs =>
                let
                   val l = reify (k, ty)
                   val k = Cont.goto l
                   val (args, exps) =
                      case Type.deTupleOpt ty of
                         NONE =>
                            let
                               val res = Var.newNoname ()
                            in
                               (Vector.new1 (res, ty),
                                Vector.new1 (Var (res, ty)))
                            end
                       | SOME ts =>
                            if Vector.isEmpty ts
                               then (Vector.new0 (), Vector.new0 ())
                            else
                               Error.bug
                               (concat ["DirectExp2.linearlize'.loop: Runtime:with multiple return values: ",
                                        Prim.toString prim])
                in
                   {statements = [],
                    transfer =
                    Transfer.Runtime
                    {prim = prim,
                     args = xs,
                     return = newLabel (args,
                                        tuple {exps = exps,
                                               ty = ty},
                                        h, k)}}
                end)
          | Select {tuple, offset, ty} =>
               loopf (tuple, h, fn (tuple, _) =>
                      Cont.sendExp (k, ty, Exp.Select {tuple = tuple,
                                                       offset = offset}))
          | Seq (e1, e2) => loopf (e1, h, fn _ => loop (e2, h, k))
          | Tuple {exps, ty} =>
               loops (exps, h, fn xs => Cont.sendExp (k, ty, Exp.Tuple xs))
          | Var (x, ty) => Cont.sendVar (k, ty, x)) arg
      and loops (es: t vector, h: Handler.t, k: Var.t vector -> Res.t): Res.t =
         let
            val n = Vector.length es
            fun each (i, ac) =
               if i = n
                  then k (Vector.fromListRev ac)
               else loopf (Vector.sub (es, i), h, fn (x, _) =>
                           each (i + 1, x :: ac))
         in
            each (0, [])
         end
      val l = newLabel0 (e, h, k)
   in
      (l, !blocks)
   end

fun linearize (e: t, h) = linearize' (e, h, Cont.return)

val linearize =
   Trace.trace2 ("DirectExp2.linearize", layout, Handler.layout,
                Layout.tuple2 (Label.layout,
                               List.layout (Label.layout o Block.label)))
   linearize

fun linearizeGoto (e: t, h, l) = linearize' (e, h, Cont.goto l)

end
end
