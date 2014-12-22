(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor MatchCompile (S: MATCH_COMPILE_STRUCTS): MATCH_COMPILE =
struct

open S

local
   open Layout
in
   val wild = str "_"

   fun conApp (c, p) =
      let
         val c = Con.layout c
      in
         case p of
            NONE => c
          | SOME p => paren (seq [c, str " ", p])
      end
end

structure Env = MonoEnv (structure Domain = Var
                         structure Range = Var)

structure Fact =
   struct
      datatype t =
         Con of {arg: Var.t option,
                 con: Con.t}
       | Tuple of Var.t vector

      fun layout (f: t): Layout.t =
         let
            open Layout
         in
            case f of
               Con {arg, con} =>
                  seq [Con.layout con,
                       case arg of
                          NONE => empty
                        | SOME x => seq [str " ", Var.layout x]]
             | Tuple xs => tuple (Vector.toListMap (xs, Var.layout))
         end
   end

structure Examples =
   struct
      datatype t = T of {es: (Var.t * Layout.t) list,
                         isOnlyExns: bool}

      fun layout (T {es, ...}) =
         List.layout (Layout.tuple2 (Var.layout, fn l => l)) es

      val empty = T {es = [], isOnlyExns = true}

      fun add (T {es, isOnlyExns = is}, x, l, {isOnlyExns: bool}) =
         T {es = (x, l) :: es,
            isOnlyExns = is andalso isOnlyExns}
   end

structure Facts =
   struct
      datatype t = T of {fact: Fact.t,
                         var: Var.t} list

      fun layout (T fs) =
         let
            open Layout
         in
            List.layout (fn {fact, var} =>
                         seq [Var.layout var, str " = ", Fact.layout fact])
            fs
         end

      val empty: t = T []

      fun add (T fs, x, f) = T ({fact = f, var = x} :: fs)

      fun bind (T facts, x: Var.t, p: NestedPat.t): Env.t =
         let
            val {destroy, get = fact: Var.t -> Fact.t, set = setFact, ...} =
               Property.destGetSetOnce
               (Var.plist, Property.initRaise ("fact", Var.layout))
            val () = List.foreach (facts, fn {fact, var} => setFact (var, fact))
            fun loop (p: NestedPat.t, x: Var.t, env: Env.t): Env.t =
               let
                  datatype z = datatype NestedPat.node
               in
                  case NestedPat.node p of
                     Con {arg, ...} =>
                        (case arg of
                            NONE => env
                          | SOME p => 
                               (case fact x of
                                   Fact.Con {arg = SOME x, ...} =>
                                      loop (p, x, env)
                                 | _ => Error.bug "MatchCompile.Facts.bind: Con:wrong fact"))
                   | Const _ => env
                   | Layered (y, p) => loop (p, x, Env.extend (env, y, x))
                   | Tuple ps =>
                        if 0 = Vector.length ps
                           then env
                        else (case fact x of
                                 Fact.Tuple xs =>
                                    Vector.fold2 (ps, xs, env, loop)
                               | _ => Error.bug "MatchCompile.Facts.bind: Tuple:wrong fact")
                   | Var y => Env.extend (env, y, x)
                   | Wild => env
               end
            val env = loop (p, x, Env.empty)
            val () = destroy ()
         in
            env
         end

      val bind =
         Trace.trace3 ("MatchCompile.Facts.bind",
                       layout, Var.layout, NestedPat.layout, Env.layout)
         bind

      fun example (T facts, Examples.T {es, ...}, x: Var.t): Layout.t =
         let
            val {destroy,
                 get = fact: Var.t -> Fact.t option,
                 set = setFact, ...} =
               Property.destGetSetOnce (Var.plist, Property.initConst NONE)
            val () = List.foreach (facts, fn {fact, var} =>
                                   setFact (var, SOME fact))
            fun loop (x: Var.t): Layout.t =
               case fact x of
                  NONE =>
                     (case List.peek (es, fn (x', _) => Var.equals (x, x')) of
                         NONE => wild
                       | SOME (_, l) => l)
                | SOME f =>
                     case f of
                        Fact.Con {arg, con} =>
                           conApp (con, Option.map (arg, loop))
                      | Fact.Tuple xs =>
                           Layout.tuple (Vector.toListMap (xs, loop))
            val res = loop x
            val () = destroy ()
         in
            res
         end

      val example =
         Trace.trace3 
         ("MatchCompile.Facts.example", 
          layout, Examples.layout, Var.layout, fn l => l)
         example
   end

structure Pat =
   struct
      datatype t =
         Const of {const: Const.t,
                   isChar: bool,
                   isInt: bool}
       | Con of {arg: (t * Type.t) option,
                 con: Con.t,
                 targs: Type.t vector}
       | Tuple of t vector
       | Wild

      fun layout (p: t): Layout.t =
         let
            open Layout
         in
            case p of
               Const {const, ...} => Const.layout const
             | Con {arg, con, ...} =>
                  seq [Con.layout con,
                       case arg of
                          NONE => empty
                        | SOME (p, _) => seq [str " ", layout p]]
             | Tuple ps => tuple (Vector.toListMap (ps, layout))
             | Wild => str "_"
         end

      val isWild: t -> bool =
         fn Wild => true
          | _ => false

      val fromNestedPat: NestedPat.t -> t =
         let
            fun loop (p: NestedPat.t): t =
               case NestedPat.node p of
                  NestedPat.Con {arg, con, targs} =>
                     let
                        val arg =
                           Option.map (arg, fn p => (loop p, NestedPat.ty p))
                     in
                        Con {arg = arg, con = con, targs = targs}
                     end
                | NestedPat.Const r => Const r
                | NestedPat.Layered (_, p) => loop p
                | NestedPat.Tuple ps => Tuple (Vector.map (ps, loop))
                | NestedPat.Var _ => Wild
                | NestedPat.Wild => Wild
         in
            loop
         end
   end

structure Vector =
   struct
      open Vector

      fun dropNth (v: 'a t, n: int): 'a t =
         keepAllMapi (v, fn (i, a) => if i = n then NONE else SOME a)
   end

structure Rule =
   struct
      datatype t =
         T of {pats: Pat.t vector,
               rest: {examples: (Layout.t * {isOnlyExns: bool}) list ref,
                      finish: (Var.t -> Var.t) -> Exp.t,
                      nestedPat: NestedPat.t}}


      fun layout (T {pats, ...}) =
         Layout.tuple (Vector.toListMap (pats, Pat.layout))

      fun allWild (T {pats, ...}) = Vector.forall (pats, Pat.isWild)

      fun dropNth (T {pats, rest}, n) =
         T {pats = Vector.dropNth (pats, n),
            rest = rest}
   end

structure Rules =
   struct
      type t = Rule.t vector

      fun layout (rs: t) = Layout.align (Vector.toListMap (rs, Rule.layout))

      fun dropNth (rs: t, n: int): t =
         Vector.map (rs, fn r => Rule.dropNth (r, n))
   end

structure Vars =
   struct
      type t = (Var.t * Type.t) vector

      val layout = Vector.layout (Layout.tuple2 (Var.layout, Type.layout))
   end

val directCases =
   List.keepAllMap (WordSize.all, fn s =>
                    if WordSize.equals (s, WordSize.fromBits (Bits.fromInt 64))
                       then NONE
                    else SOME {size = s, ty = Type.word s})

(* unhandledConst cs returns a constant (of the appropriate type) not in cs. *)
fun unhandledConst (cs: Const.t vector): Const.t =
   let
      fun search {<= : 'a * 'a -> bool,
                  equals: 'a * 'a -> bool,
                  extract: Const.t -> 'a,
                  isMin: 'a -> bool,
                  make: 'a -> Const.t,
                  next: 'a -> 'a,
                  prev: 'a -> 'a} =
         let
            val cs = QuickSort.sortVector (Vector.map (cs, extract), op <=)
            val c = Vector.sub (cs, 0)
         in
            if not (isMin c)
               then make (prev c)
            else
               let
                  val n = Vector.length cs
                  fun loop (i, c) =
                     if i = n orelse not (equals (c, Vector.sub (cs, i)))
                        then make c
                     else loop (i + 1, next c)
               in
                  loop (0, c)
               end
         end
      val c = Vector.sub (cs, 0)
      datatype z = datatype Const.t
   in
      case c of
         IntInf _ =>
            let
               fun extract c =
                  case c of
                     IntInf i => i
                   | _ => Error.bug "MatchCompile.unhandledConst: expected IntInf"
            in
               search {<= = op <=,
                       equals = op =,
                       extract = extract,
                       isMin = fn _ => false,
                       make = Const.IntInf,
                       next = fn i => i + 1,
                       prev = fn i => i - 1}
            end
       | Null => Error.bug "MatchCompile.unhandledConst: match on null is not allowed"
       | Real _ => Error.bug "MatchCompile.unhandledConst: match on real is not allowed"
       | Word w =>
            let
               val s = WordX.size w
               fun extract c =
                  case c of
                     Word w => WordX.toIntInf w
                   | _ => Error.bug "MatchCompile.unhandledConst: expected Word"
            in
               search {<= = op <=,
                       equals = op =,
                       extract = extract,
                       isMin = fn w => w = 0,
                       make = fn w => Const.word (WordX.fromIntInf (w, s)),
                       next = fn w => w + 1,
                       prev = fn w => w - 1}
            end
       | WordVector v =>
            let
               val max =
                  Vector.fold
                  (cs, ~1, fn (c, max) =>
                   case c of
                      WordVector v => Int.max (max, WordXVector.length v)
                    | _ => Error.bug "MatchCompile.unhandledConst: expected Word8Vector")
               val elementSize = WordXVector.elementSize v
               val w = WordX.fromIntInf (IntInf.fromInt (Char.ord #"a"),
                                         elementSize)
            in
               Const.WordVector (WordXVector.tabulate
                                 ({elementSize = elementSize}, max + 1,
                                  fn _ => w))
            end
   end

structure Exp =
   struct
      open Exp

      fun layout (_: t) = Layout.str "<exp>"
   end

val traceMatch =
   Trace.trace4 ("MatchCompile.match",
                 Vars.layout, Rules.layout, Facts.layout, Examples.layout,
                 Exp.layout)
val traceConst =
   Trace.trace ("MatchCompile.const",
                fn (vars, rules, facts, es, _: Int.t, _: Exp.t) =>
                Layout.tuple [Vars.layout vars,
                              Rules.layout rules,
                              Facts.layout facts,
                              Examples.layout es],
                Exp.layout)
val traceSum =
   Trace.trace ("MatchCompile.sum",
                fn (vars, rules, facts, es, _: Int.t, _: Exp.t, _: Tycon.t) =>
                Layout.tuple [Vars.layout vars,
                              Rules.layout rules,
                              Facts.layout facts,
                              Examples.layout es],
                Exp.layout)
val traceTuple =
   Trace.trace ("MatchCompile.tuple",
                fn (vars, rules, facts, es, _: Int.t, _: Exp.t) =>
                Layout.tuple [Vars.layout vars,
                              Rules.layout rules,
                              Facts.layout facts,
                              Examples.layout es],
                Exp.layout)

(*---------------------------------------------------*)
(*                   matchCompile                    *)
(*---------------------------------------------------*)

fun matchCompile {caseType: Type.t,
                  cases: (NestedPat.t * ((Var.t -> Var.t) -> Exp.t)) vector,
                  conTycon: Con.t -> Tycon.t,
                  region: Region.t,
                  test: Var.t,
                  testType: Type.t,
                  tyconCons: Tycon.t -> {con: Con.t,
                                         hasArg: bool} vector} =
   let
      fun chooseColumn _ = 0
      fun match arg : Exp.t =
         traceMatch
         (fn (vars: Vars.t, rules: Rules.t, facts: Facts.t, es) =>
         if 0 = Vector.length rules
            then Error.bug "MatchCompile.match: no rules"
         else if Rule.allWild (Vector.sub (rules, 0))
            then (* The first rule matches. *)
               let
                  val Rule.T {rest = {examples, finish, nestedPat, ...}, ...} =
                     Vector.sub (rules, 0)
                  val env = Facts.bind (facts, test, nestedPat)
                  val Examples.T {isOnlyExns, ...} = es
                  val () =
                     List.push (examples,
                                (Facts.example (facts, es, test),
                                 {isOnlyExns = isOnlyExns}))
               in
                  finish (fn x => Env.lookup (env, x))
               end
         else
            let
               val i = chooseColumn rules
            in
               case Vector.peek (rules, fn Rule.T {pats, ...} =>
                                 not (Pat.isWild (Vector.sub (pats, i)))) of
                  NONE => match (Vector.dropNth (vars, i),
                                 Rules.dropNth (rules, i),
                                 facts, es)
                | SOME (Rule.T {pats, ...}) =>
                     let
                        datatype z = datatype Pat.t
                        val test = Exp.var (Vector.sub (vars, i))
                     in
                        case Vector.sub (pats, i) of
                           Const _ => const (vars, rules, facts, es, i, test)
                         | Con {con, ...} =>
                              sum (vars, rules, facts, es, i, test,
                                   conTycon con)
                         | Tuple _ => tuple (vars, rules, facts, es, i, test)
                         | Wild => Error.bug "MatchCompile.match: Wild"
                     end
            end) arg
      and const arg =
         traceConst
         (fn (vars, rules, facts, es, i, test) =>
         let
            val (var, ty) = Vector.sub (vars, i)
            val {isChar, isInt} =
               case Vector.peekMap (rules, fn Rule.T {pats, ...} =>
                                    case Vector.sub (pats, i) of
                                       Pat.Const {isChar, isInt, ...} =>
                                          SOME {isChar = isChar, isInt = isInt}
                                     | _ => NONE) of
                  NONE => {isChar = false, isInt = false}
                | SOME z => z
            fun layoutConst c =
               if isChar
                  then
                     case c of
                        Const.Word w =>
                           let
                              open Layout
                           in
                              seq [str "#\"",
                                   Char.layout (WordX.toChar w),
                                   str String.dquote]
                           end
                      | _ => Error.bug (concat
                                        ["MatchCompile.const.layoutConst: ",
                                         "strange char: ", 
                                         Layout.toString (Const.layout c)])
               else if isInt
                  then
                     case c of
                        Const.IntInf i => IntInf.layout i
                      | Const.Word w =>
                           IntInf.layout (WordX.toIntInfX w)
                      | _ => Error.bug (concat
                                        ["MatchCompile.const.layoutConst: ",
                                         "strange int: ", 
                                         Layout.toString (Const.layout c)])
               else Const.layout c
            val (cases, defaults) =
               Vector.foldr
               (rules, ([], []),
                fn (rule as Rule.T {pats, ...}, (cases, defaults)) =>
                let
                   val rule = Rule.dropNth (rule, i)
                in
                   case Vector.sub (pats, i) of
                      Pat.Const {const = c, ...} =>
                         let
                            fun insert (cases, ac) =
                               case cases of
                                  [] =>
                                     {const = c, rules = rule :: defaults} :: ac
                                | (casee as {const, rules}) :: cases =>
                                     if Const.equals (c, const)
                                        then
                                           {const = c, rules = rule :: rules}
                                           :: List.appendRev (ac, cases)
                                     else insert (cases, casee :: ac)
                         in
                            (insert (cases, []), defaults)
                         end
                    | Pat.Wild =>
                         (List.map (cases, fn {const, rules} =>
                                    {const = const, rules = rule :: rules}),
                          rule :: defaults)
                    | _ => Error.bug "MatchCompile.const: expected Const pat"
                end)
            val cases = Vector.fromListMap (cases, fn {const, rules} =>
                                            {const = const,
                                             rules = Vector.fromList rules})
            val defaults = Vector.fromList defaults
            val vars = Vector.dropNth (vars, i)
            fun finish (rules: Rule.t vector, e, isOnlyExns): Exp.t =
               match (vars, rules, facts,
                      Examples.add (es, var, e, {isOnlyExns = isOnlyExns}))
            fun default (): Exp.t =
               let
                  val (e, ioe) =
                     if 0 = Vector.length cases
                        then (wild, true)
                     else (layoutConst (unhandledConst
                                        (Vector.map (cases, #const))),
                           false)
               in
                  finish (defaults, e, ioe)
               end
         in
            case List.peek (directCases, fn {ty = ty', ...} =>
                            Type.equals (ty, ty')) of
               NONE => 
                  Vector.fold
                  (cases, default (), fn ({const, rules}, rest) =>
                   Exp.iff {test = Exp.equal (test, Exp.const const),
                            thenn = finish (rules, Const.layout const, true),
                            elsee = rest,
                            ty = caseType})
             | SOME {size, ...} =>
                  let
                     val default =
                        if WordSize.cardinality size
                           = IntInf.fromInt (Vector.length cases)
                           then NONE
                        else SOME (default (), region)
                     val cases =
                        Vector.map
                        (cases, fn {const, rules} =>
                         let
                            val w = 
                               case const of
                                  Const.Word w => w
                                | _ => Error.bug "MatchCompile.const: caseWord type error"
                         in
                            (w, finish (rules, layoutConst const, true))
                         end)
                  in
                     Exp.casee {cases = Cases.word (size, cases),
                                default = default,
                                test = test,
                                ty = caseType}
                  end
         end) arg
      and sum arg =
         traceSum
         (fn (vars: Vars.t, rules: Rules.t, facts: Facts.t, es,
              i, test, tycon) =>
         let
            val (var, _) = Vector.sub (vars, i)
            val (cases, defaults) =
               Vector.foldr
               (rules, ([], []),
                fn (rule as Rule.T {pats, ...}, (cases, defaults)) =>
                case Vector.sub (pats, i) of
                   Pat.Con {arg, con, targs} =>
                      let
                         fun oneCase () =
                            let
                               val (arg, vars) =
                                  case arg of
                                     NONE =>
                                        (NONE,
                                         Vector.keepAllMapi
                                         (vars, fn (i', x) =>
                                          if i = i' then NONE else SOME x))
                                   | SOME (_, ty) => 
                                        let
                                           val arg = Var.newNoname ()
                                        in
                                           (SOME (arg, ty),
                                            Vector.mapi
                                            (vars, fn (i', x) =>
                                             if i = i' then (arg, ty) else x))
                                        end
                            in
                               {rest = {arg = arg,
                                        con = con,
                                        targs = targs,
                                        vars = vars},
                                rules = rule :: defaults}
                            end
                         fun insert (cases, ac) =
                            case cases of
                               [] => oneCase () :: ac
                           | ((casee as {rest as {con = con', ...}, rules})
                              :: cases) =>
                             if Con.equals (con, con')
                                then
                                   {rest = rest, rules = rule :: rules}
                                   :: List.appendRev (ac, cases)
                             else insert (cases, casee :: ac)
                      in
                         (insert (cases, []), defaults)
                      end
                 | Pat.Wild =>
                      (List.map (cases, fn {rest, rules} =>
                                 {rest = rest, rules = rule :: rules}),
                       rule :: defaults)
                 | _ => Error.bug "MatchCompile.sum: expected Con pat")
            val cases =
               Vector.fromListMap
               (cases, fn {rest = {arg, con, targs, vars}, rules} =>
                let
                   val rules =
                      Vector.fromListMap
                      (rules, fn Rule.T {pats, rest} =>
                       let
                          val pats =
                             Vector.keepAllMapi
                             (pats, fn (i', p') =>
                              if i <> i' then SOME p'
                              else
                                 case p' of
                                    Pat.Con {arg, ...} => Option.map (arg, #1)
                                  | Pat.Wild =>
                                       Option.map (arg, fn _ => Pat.Wild)
                                  | _ => Error.bug "MatchCompile.sum: decon got strange pattern")
                       in
                          Rule.T {pats = pats, rest = rest}
                       end)
                   val facts =
                      Facts.add
                      (facts, var,
                       Fact.Con {arg = Option.map (arg, #1), con = con})
                in
                   {arg = arg,
                    con = con,
                    rhs = match (vars, rules, facts, es),
                    targs = targs}
                end)
            fun done (e, isOnlyExns) =
               SOME (match (Vector.dropNth (vars, i),
                            Rules.dropNth (Vector.fromList defaults, i),
                            facts,
                            Examples.add (es, var, e,
                                          {isOnlyExns = isOnlyExns})))
            val default =
               if Vector.isEmpty cases
                  then done (wild, true)
               else if Tycon.equals (tycon, Tycon.exn)
                  then done (Layout.str "e", true)
               else
                  let
                     val cons = tyconCons tycon
                  in
                     if Vector.length cases = Vector.length cons
                        then NONE
                     else
                        let
                           val unhandled =
                              Vector.keepAllMap
                              (cons, fn {con, hasArg, ...} =>
                               if Vector.exists (cases, fn {con = con', ...} =>
                                                 Con.equals (con, con'))
                                  then NONE
                               else SOME (conApp
                                          (con,
                                           if hasArg then SOME wild else NONE)))
                           open Layout
                        in
                           done
                           (seq (separate (Vector.toList unhandled, " | ")),
                            false)
                        end
                  end
            fun normal () =
               Exp.casee {cases = Cases.con cases,
                          default = Option.map (default, fn e => (e, region)),
                          test = test,
                          ty = caseType}
         in
            if 1 <> Vector.length cases
               then normal ()
            else
               let
                  val {arg, con, rhs, ...} = Vector.sub (cases, 0)
               in
                  if not (Con.equals (con, Con.reff))
                     then normal ()
                  else 
                     case arg of
                        NONE => Error.bug "MatchCompile.sum: ref missing arg"
                      | SOME (var, _) => 
                           Exp.lett {body = rhs,
                                     exp = Exp.deref test,
                                     var = var}
               end
         end) arg
      and tuple arg =
         traceTuple
         (fn (vars: Vars.t, rules: Rules.t, facts: Facts.t, es, i, test) =>
         let
            val (var, _) = Vector.sub (vars, i)
            fun body vars' =
               let
                  val n = Vector.length vars'
                  val vars =
                     Vector.concatV
                     (Vector.mapi
                      (vars, fn (i', x) =>
                       if i = i'
                          then vars'
                       else Vector.new1 x))
                  val rules =
                     Vector.map
                     (rules, fn Rule.T {pats, rest} =>
                      let
                         val pats =
                            Vector.concatV
                            (Vector.mapi
                             (pats, fn (i', p) =>
                              if i <> i'
                                 then Vector.new1 p
                              else (case p of
                                       Pat.Tuple ps => ps
                                     | Pat.Wild =>
                                          Vector.tabulate (n, fn _ => Pat.Wild)
                                     | _ => Error.bug "MatchCompile.tuple: detuple")))
                      in
                         Rule.T {pats = pats, rest = rest}
                      end)
               in
                  match (vars, rules,
                         Facts.add (facts, var,
                                    Fact.Tuple (Vector.map (vars', #1))),
                         es)
               end
         in
            Exp.detuple {body = body, tuple = test}
         end) arg
      val examples = Vector.tabulate (Vector.length cases, fn _ => ref [])
      val res =
         match (Vector.new1 (test, testType),
                Vector.map2 (cases, examples, fn ((p, f), r) =>
                             Rule.T {pats = Vector.new1 (Pat.fromNestedPat p),
                                     rest = {examples = r,
                                             finish = f,
                                             nestedPat = p}}),
                Facts.empty,
                Examples.empty)
   in
      (res, fn () => Vector.map (examples, fn r => Vector.fromList (!r)))
   end

val matchCompile =
   Trace.trace
   ("MatchCompile.matchCompile",
    fn {caseType, cases, test, testType, ...} =>
    Layout.record [("caseType", Type.layout caseType),
                   ("cases", Vector.layout (NestedPat.layout o #1) cases),
                   ("test", Var.layout test),
                   ("testType", Type.layout testType)],
    Exp.layout o #1)
   matchCompile

end
