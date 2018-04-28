(* Copyright (C) 2015,2017 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor MatchCompile (S: MATCH_COMPILE_STRUCTS): MATCH_COMPILE =
struct

open S

structure Example =
  struct
     datatype t =
        ConApp of {arg: t option, con: Con.t}
      | ConstRange of {lo: Const.t option, hi: Const.t option, isChar: bool, isInt: bool}
      | Exn
      | Or of t vector
      | Record of t SortedRecord.t
      | Vector of t vector * {dots: bool}
      | Wild

     fun layout (ex, isDelimited) =
        let
           open Layout
           fun delimit t = if isDelimited then t else paren t
           fun layoutChar c =
              let
                 fun loop (n: int, c: IntInf.t, ac: char list) =
                    if n = 0
                       then implode ac
                       else
                          let
                             val (q, r) = IntInf.quotRem (c, 0x10)
                          in
                             loop (n - 1, q, Char.fromHexDigit (Int.fromIntInf r) :: ac)
                          end
                 fun doit (n, esc) = str (concat ["\\", esc, loop (n, c, [])])
              in
                 if c <= 0xFF
                    then str (Char.escapeSML (Char.fromInt (Int.fromIntInf c)))
                 else if c <= 0xFFFF
                    then doit (4, "u")
                 else doit (8, "U")
              end
           fun layoutConst (c, isChar, isInt) =
              if isChar
                 then
                    case c of
                       Const.Word w =>
                          seq [str "#\"",
                               layoutChar (WordX.toIntInf w),
                               str "\""]
                     | _ => Error.bug (concat
                                       ["MatchCompile.Example.layout.layoutConst: ",
                                        "strange char: ",
                                        Layout.toString (Const.layout c)])
              else if isInt
                 then
                    case c of
                       Const.IntInf i => IntInf.layout i
                     | Const.Word w => IntInf.layout (WordX.toIntInfX w)
                     | _ => Error.bug (concat
                                       ["MatchCompile.Example.layout.layoutConst: ",
                                        "strange int: ",
                                        Layout.toString (Const.layout c)])
              else
                 case c of
                    Const.Word w =>
                       seq [str "0wx", str (IntInf.format (WordX.toIntInf w, StringCvt.HEX))]
                  | Const.WordVector ws =>
                       seq [str "\"",
                            seq (WordXVector.toListMap (ws, layoutChar o WordX.toIntInf)),
                            str "\""]
                  | _ => Error.bug (concat
                                    ["MatchCompile.Example.layout.layoutConst: ",
                                     "strange const: ",
                                     Layout.toString (Const.layout c)])
        in
           case ex of
              ConApp {arg, con} =>
                 (case arg of
                     NONE => str (Con.originalName con)
                   | SOME arg =>
                        (delimit o seq)
                        [str (Con.originalName con),
                         str " ",
                         layoutF arg])
            | ConstRange {lo, hi, isChar, isInt} =>
                 (case (lo, hi) of
                     (NONE, NONE) => str "..."
                   | (NONE, SOME hi) =>
                        delimit (seq [str "... ", layoutConst (hi, isChar, isInt)])
                   | (SOME lo, NONE) =>
                        delimit (seq [layoutConst (lo, isChar, isInt), str " ..."])
                   | (SOME lo, SOME hi) =>
                        if Const.equals (lo, hi)
                           then layoutConst (lo, isChar, isInt)
                           else delimit (seq [layoutConst (lo, isChar, isInt),
                                              str " .. ",
                                              layoutConst (hi, isChar, isInt)]))
            | Exn => delimit (str "_ : exn")
            | Or exs =>
                 (delimit o mayAlign o separateLeft)
                 (Vector.toListMap (exs, layoutT), "| ")
            | Record rexs =>
                 SortedRecord.layout
                 {extra = "",
                  layoutElt = layoutT,
                  layoutTuple = fn exs => tuple (Vector.toListMap (exs, layoutT)),
                  record = rexs,
                  separator = " = "}
            | Vector (exs, {dots}) =>
                 let
                    val exs = Vector.map (exs, layoutT)
                 in
                    vector (if dots
                               then Vector.concat [exs, Vector.new1 (str "...")]
                               else exs)
                 end
            | Wild => str "_"
        end
     and layoutF ex = layout (ex, false)
     and layoutT ex = layout (ex, true)

     val layout = layoutT

     fun isWild ex =
        case ex of
           Wild => true
         | _ => false

     fun const {const, isChar, isInt} =
        ConstRange {lo = SOME const, hi = SOME const,
                    isChar = isChar, isInt = isInt}
     fun constRange {lo, hi, isChar, isInt} =
        ConstRange {lo = lo, hi = hi,
                    isChar = isChar, isInt = isInt}

     fun record rexs =
        if SortedRecord.forall (rexs, isWild)
           then Wild
           else Record rexs

     fun vector exs = Vector (exs, {dots = false})
     fun vectorDots exs = Vector (exs, {dots = true})

     fun compare (ex1, ex2) =
        case (ex1, ex2) of
         (* Wild sorts last *)
           (Wild, Wild) => EQUAL
         | (_, Wild) => LESS
         | (Wild, _) => GREATER
         (* Exn sorts last *)
         | (Exn, Exn) => EQUAL
         | (_, Exn) => LESS
         | (Exn, _) => GREATER
         | (ConstRange {lo = lo1, hi = hi1, isInt, ...},
            ConstRange {lo = lo2, hi = hi2, ...}) =>
              let
                 fun cmp (x, y, b, k) =
                    case (x, y) of
                       (NONE, NONE) => k EQUAL
                     | (NONE, SOME _) => if b then LESS else GREATER
                     | (SOME _, NONE) => if b then GREATER else LESS
                     | (SOME (Const.Word w1), SOME (Const.Word w2)) =>
                          k (WordX.compare (w1, w2, {signed = isInt}))
                     | (SOME (Const.IntInf ii1), SOME (Const.IntInf ii2)) =>
                          k (IntInf.compare (ii1, ii2))
                     | (SOME (Const.WordVector ws1), SOME (Const.WordVector ws2)) =>
                          k (WordXVector.compare (ws1, ws2))
                     | _ => Error.bug "MatchCompile.Example.compare: ConstRange/ConstRange"
              in
                 cmp (lo1, lo2, true, fn order =>
                      case order of
                         LESS => LESS
                       | EQUAL => cmp (hi1, hi2, false, fn order => order)
                       | GREATER => GREATER)
              end
         | (ConApp {con = con1, arg = arg1}, ConApp {con = con2, arg = arg2}) =>
              (case String.compare (Con.toString con1, Con.toString con2) of
                  LESS => LESS
                | EQUAL => (case (arg1, arg2) of
                               (SOME arg1, SOME arg2) => compare' (arg1, arg2)
                             | (NONE, NONE) => EQUAL
                             | _ => Error.bug "MatchCompile.Example.compare: ConApp/ConApp")
                | GREATER => GREATER)
         | (Vector (exs1, {dots = dots1}), Vector (exs2, {dots = dots2})) =>
              (case (dots1, dots2) of
                  (false, true) => LESS
                | (true, false) => GREATER
                | _ => Vector.compare (exs1, exs2, compare'))
         | (Record rexs1, Record rexs2) =>
              Vector.compare (SortedRecord.range rexs1, SortedRecord.range rexs2, compare')
         | _ => Error.bug "MatchCompile.Example.compare"
     and compare' (ex1, ex2) =
        case (ex1, ex2) of
           (Or ex1s, Or ex2s) => compares (Vector.toList ex1s, Vector.toList ex2s)
         | (Or ex1s, _) => compares (Vector.toList ex1s, [ex2])
         | (_, Or ex2s) => compares ([ex1], Vector.toList ex2s)
         | _ => compare (ex1, ex2)
     and compares (exs1, exs2) =
        List.compare (exs1, exs2, compare)

     fun or exs =
        let
           fun join (exs1, exs2) =
              case (exs1, exs2) of
                 ([], _) => exs2
               | (_, []) => exs1
               | ((ex1 as ConApp {con = con1, arg = arg1})::exs1',
                  (ex2 as ConApp {con = con2, arg = arg2})::exs2') =>
                    (case String.compare (Con.toString con1, Con.toString con2) of
                        LESS => ex1::(join (exs1', exs2))
                      | EQUAL =>
                           let
                              val arg =
                                 case (arg1, arg2) of
                                    (SOME arg1, SOME arg2) => or [arg1, arg2]
                                  | (NONE, NONE) => NONE
                                  | _ => Error.bug "MatchCompile.Example.or.join"
                           in
                              (ConApp {con = con1, arg = arg})::
                              (join (exs1', exs2'))
                           end
                      | GREATER => ex2::(join (exs1, exs2')))
                | (ex1::exs1', ex2::exs2') =>
                    (case compare (ex1, ex2) of
                        LESS => ex1::(join (exs1', exs2))
                      | EQUAL => ex1::(join (exs1', exs2'))
                      | GREATER => ex2::(join (exs1, exs2')))
           val exss =
              List.map (exs, fn Or exs => Vector.toList exs | ex => [ex])
           val exs =
              List.fold (exss, [], join)
        in
           case exs of
              [] => NONE
            | [ex] => SOME ex
            | _ => SOME (Or (Vector.fromList exs))
        end

  end

structure Env = MonoEnv (structure Domain = Var
                         structure Range = Var)

structure Fact =
   struct
      datatype t =
         Con of {arg: Var.t option,
                 con: Con.t}
       | Record of Var.t SortedRecord.t
       | Vector of Var.t vector

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
             | Record r =>
                  SortedRecord.layout
                  {extra = "",
                   layoutElt = Var.layout,
                   layoutTuple = fn xs => tuple (Vector.toListMap (xs, Var.layout)),
                   record = r,
                   separator = " = "}
             | Vector xs => vector (Vector.map (xs, Var.layout))
         end
   end

structure Examples =
   struct
      datatype t = T of {exs: (Var.t * Example.t) list,
                         isOnlyExns: bool}

      fun layout (T {exs, ...}) =
         List.layout (Layout.tuple2 (Var.layout, Example.layout)) exs

      val empty = T {exs = [], isOnlyExns = true}

      fun add (T {exs, isOnlyExns = is}, x, ex, {isOnlyExns: bool}) =
         T {exs = (x, ex) :: exs,
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
                   | Or _ => Error.bug "MatchCompile.factbind: or pattern shouldn't be here"
                   | Record rp =>
                        (case fact x of
                            Fact.Record rx =>
                               Vector.fold2 (SortedRecord.range rp, SortedRecord.range rx, env, loop)
                          | _ => Error.bug "MatchCompile.Facts.bind: Record:wrong fact")
                   | Var y => Env.extend (env, y, x)
                   | Vector ps =>
                        (case fact x of
                            Fact.Vector xs =>
                               Vector.fold2 (ps, xs, env, loop)
                          | _ => Error.bug "MatchCompile.Facts.bind: Vector:wrong fact")
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

      fun example (T facts, Examples.T {exs, ...}, x: Var.t): Example.t =
         let
            val {destroy,
                 get = fact: Var.t -> Fact.t option,
                 set = setFact, ...} =
               Property.destGetSetOnce (Var.plist, Property.initConst NONE)
            val () = List.foreach (facts, fn {fact, var} =>
                                   setFact (var, SOME fact))
            fun loop (x: Var.t): Example.t =
               case fact x of
                  NONE =>
                     (case List.peek (exs, fn (x', _) => Var.equals (x, x')) of
                         NONE => Example.Wild
                       | SOME (_, ex) => ex)
                | SOME f =>
                     (case f of
                         Fact.Con {arg, con} =>
                            Example.ConApp {con = con, arg = Option.map (arg, loop)}
                       | Fact.Record rxs =>
                            Example.record (SortedRecord.map (rxs, loop))
                       | Fact.Vector xs =>
                            Example.vector (Vector.map (xs, loop)))
            val res = loop x
            val () = destroy ()
         in
            res
         end

      val example =
         Trace.trace3 
         ("MatchCompile.Facts.example", 
          layout, Examples.layout, Var.layout, Example.layout)
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
       | Record of t SortedRecord.t
       | Vector of t vector
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
             | Record rps =>
                  SortedRecord.layout
                  {extra = "",
                   layoutElt = layout,
                   layoutTuple = fn ps => tuple (Vector.toListMap (ps, layout)),
                   record = rps,
                   separator = " = "}
             | Vector ps => vector (Vector.map (ps, layout))
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
                | NestedPat.Or _ => Error.bug "MatchCompile.fromNestedPat: or pattern shouldn't be here"
                | NestedPat.Record rps => Record (SortedRecord.map (rps, loop))
                | NestedPat.Var _ => Wild
                | NestedPat.Vector ps => Vector (Vector.map (ps, loop))
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
               rest: {examples: (Example.t * {isOnlyExns: bool}) list ref option,
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

fun unhandledConsts {consts = cs: Const.t vector, isChar, isInt}: Example.t option =
   let
      fun search {<= : 'a * 'a -> bool,
                  equals: 'a * 'a -> bool,
                  extract: Const.t -> 'a,
                  make: 'a -> Const.t,
                  max: 'a option,
                  min: 'a option,
                  next: 'a -> 'a,
                  prev: 'a -> 'a} =
         let
            fun exampleConstRange (lo, hi) =
               Example.constRange
               {lo = Option.map (lo, make),
                hi = Option.map (hi, make),
                isChar = isChar, isInt = isInt}
            fun mkExampleConstRange (lo, hi) =
               if lo <= hi
                  then if equals (lo, hi)
                          then [exampleConstRange (SOME lo, SOME hi)]
                          else let
                                  val lo' = next lo
                                  val hi' = prev hi
                               in
                                  if equals (lo', hi)
                                     then [exampleConstRange (SOME lo, SOME lo),
                                           exampleConstRange (SOME hi, SOME hi)]
                                  else if equals (lo', hi')
                                     then [exampleConstRange (SOME lo, SOME lo),
                                           exampleConstRange (SOME lo', SOME hi'),
                                           exampleConstRange (SOME hi, SOME hi)]
                                  else [exampleConstRange (SOME lo, SOME hi)]
                               end
                  else []
            val cs = QuickSort.sortVector (Vector.map (cs, extract), op <=)
            val cs = Vector.toList cs
            fun loop cs =
               case cs of
                  [] => []
                | [cMax] =>
                     (case max of
                         NONE => [exampleConstRange (SOME (next cMax), NONE)]
                       | SOME max' =>
                            if equals (cMax, max')
                               then []
                               else mkExampleConstRange (next cMax, max'))
                | c1::c2::cs =>
                     (mkExampleConstRange (next c1, prev c2)) @ (loop (c2::cs))
            val cMin = hd cs
            val examples =
               case min of
                  NONE => [exampleConstRange (NONE, SOME (prev cMin))] @ (loop cs)
                | SOME min' =>
                     if equals (cMin, min')
                        then loop cs
                        else (mkExampleConstRange (min', prev cMin)) @ (loop cs)
         in
            Example.or examples
         end
      datatype z = datatype Const.t
   in
      case Vector.first cs of
         IntInf _ =>
            let
               fun extract c =
                  case c of
                     IntInf i => i
                   | _ => Error.bug "MatchCompile.unhandledConsts: expected IntInf"
            in
               search {<= = op <=,
                       equals = op =,
                       extract = extract,
                       make = Const.IntInf,
                       max = NONE,
                       min = NONE,
                       next = fn i => i + 1,
                       prev = fn i => i - 1}
            end
       | Null => Error.bug "MatchCompile.unhandledConsts: Null"
       | Real _ => Error.bug "MatchCompile.unhandledConsts: Real"
       | Word w =>
            let
               val s = WordX.size w
               val signed = {signed = isInt}
               fun extract c =
                  case c of
                     Word w => w
                   | _ => Error.bug "MatchCompile.unhandledConsts: expected Word"
            in
               search {<= = fn (w1, w2) => WordX.le (w1, w2, signed),
                       equals = WordX.equals,
                       extract = extract,
                       make = Const.word,
                       max = SOME (WordX.max (s, signed)),
                       min = SOME (WordX.min (s, signed)),
                       next = fn w => WordX.add (w, WordX.one s),
                       prev = fn w => WordX.sub (w, WordX.one s)}
            end
       | WordVector ws =>
            let
               val s = WordXVector.elementSize ws
               val signed = {signed = false}
               fun extract c =
                  case c of
                     WordVector ws => ws
                   | _ => Error.bug "MatchCompile.unhandledConsts: expected Word"
               fun next ws =
                  let
                     val wsOrig = List.rev (WordXVector.toListMap (ws, fn w => w))
                     val wsNext =
                        let
                           fun loop ws =
                              case ws of
                                 [] => [WordX.min (s, signed)]
                               | w::ws =>
                                    if WordX.isMax (w, signed)
                                       then (WordX.min (s, signed))::(loop ws)
                                       else (WordX.add (w, WordX.one s))::ws
                        in
                           loop wsOrig
                        end
                  in
                     WordXVector.fromListRev ({elementSize = s}, wsNext)
                  end
               fun prev ws =
                  let
                     val wsOrig = List.rev (WordXVector.toListMap (ws, fn w => w))
                     val wsPrev =
                        let
                           fun loop ws =
                              case ws of
                                 [] => Error.bug "MatchCompile.unhandledConst: WordXVector.prev"
                               | [w] =>
                                    if WordX.isMin (w, signed)
                                       then []
                                       else [WordX.sub (w, WordX.one s)]
                               | w::ws =>
                                    if WordX.isMin (w, signed)
                                       then (WordX.max (s, signed))::(loop ws)
                                       else (WordX.sub (w, WordX.one s))::ws
                        in
                           loop wsOrig
                        end
                  in
                     WordXVector.fromListRev ({elementSize = s}, wsPrev)
                  end
            in
               search {<= = WordXVector.le,
                       equals = WordXVector.equals,
                       extract = extract,
                       make = Const.wordVector,
                       max = NONE,
                       min = SOME (WordXVector.fromVector ({elementSize = s}, Vector.new0 ())),
                       next = next,
                       prev = prev}
            end
   end

structure Exp =
   struct
      open Exp

      fun layout (_: t) = Layout.str "<exp>"
   end

val traceMatch =
   Trace.trace ("MatchCompile.match",
                fn (vars, rules, facts, es) =>
                Layout.record [("vars", Vars.layout vars),
                               ("rules", Rules.layout rules),
                               ("facts", Facts.layout facts),
                               ("examples", Examples.layout es)],
                 Exp.layout)
val traceConst =
   Trace.trace ("MatchCompile.const",
                fn (vars, rules, facts, es, i: Int.t, test: Exp.t) =>
                Layout.record [("vars", Vars.layout vars),
                               ("rules", Rules.layout rules),
                               ("facts", Facts.layout facts),
                               ("examples", Examples.layout es),
                               ("index", Int.layout i),
                               ("test", Exp.layout test)],
                Exp.layout)
val traceSum =
   Trace.trace ("MatchCompile.sum",
                fn (vars, rules, facts, es, i: Int.t, test: Exp.t, _: Tycon.t) =>
                Layout.record [("vars", Vars.layout vars),
                               ("rules", Rules.layout rules),
                               ("facts", Facts.layout facts),
                               ("examples", Examples.layout es),
                               ("index", Int.layout i),
                               ("test", Exp.layout test)],
                Exp.layout)
val traceRecord =
   Trace.trace ("MatchCompile.record",
                fn (vars, rules, facts, es, i: Int.t, test: Exp.t, _: Field.t vector) =>
                Layout.record [("vars", Vars.layout vars),
                               ("rules", Rules.layout rules),
                               ("facts", Facts.layout facts),
                               ("examples", Examples.layout es),
                               ("index", Int.layout i),
                               ("test", Exp.layout test)],
                Exp.layout)
val traceVector =
   Trace.trace ("MatchCompile.vector",
                fn (vars, rules, facts, es, i: Int.t, test: Exp.t) =>
                Layout.record [("vars", Vars.layout vars),
                               ("rules", Rules.layout rules),
                               ("facts", Facts.layout facts),
                               ("examples", Examples.layout es),
                               ("index", Int.layout i),
                               ("test", Exp.layout test)],
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
         if Vector.isEmpty rules
            then Error.bug "MatchCompile.match: no rules"
         else if Rule.allWild (Vector.first rules)
            then (* The first rule matches. *)
               let
                  val Rule.T {rest = {examples, finish, nestedPat, ...}, ...} =
                     Vector.first rules
                  val env = Facts.bind (facts, test, nestedPat)
                  val Examples.T {isOnlyExns, ...} = es
                  val () =
                     Option.app
                     (examples, fn examples =>
                      List.push (examples,
                                 (Facts.example (facts, es, test),
                                  {isOnlyExns = isOnlyExns})))
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
                              sum (vars, rules, facts, es, i, test, conTycon con)
                         | Record rps =>
                              record (vars, rules, facts, es, i, test, SortedRecord.domain rps)
                         | Vector _ => vector (vars, rules, facts, es, i, test)
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
                | SOME {isChar, isInt} => {isChar = isChar, isInt = isInt}
            fun exampleConst c =
               Example.const {const = c, isChar = isChar, isInt = isInt}
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
            fun finish (rules: Rule.t vector, e): Exp.t =
               match (vars, rules, facts,
                      Examples.add (es, var, e, {isOnlyExns = false}))
            val default: Exp.t option =
               Option.map
               (unhandledConsts {consts = Vector.map (cases, #const),
                                 isChar = isChar, isInt = isInt},
                fn e => finish (defaults, e))
         in
            case List.peek (directCases, fn {ty = ty', ...} =>
                            Type.equals (ty, ty')) of
               NONE =>
                  let
                     val (cases, default) =
                        case default of
                           SOME default => (cases, default)
                         | NONE =>
                              (Vector.dropSuffix (cases, 1),
                               let val {const, rules} = Vector.last cases
                               in finish (rules, exampleConst const)
                               end)
                  in
                     Vector.fold
                     (cases, default, fn ({const, rules}, rest) =>
                      Exp.iff {test = Exp.equal (test, Exp.const const),
                               thenn = finish (rules, exampleConst const),
                               elsee = rest,
                               ty = caseType})
                  end
             | SOME {size, ...} =>
                  let
                     val default =
                        Option.map
                        (default, fn default =>
                         (default, region))
                     val cases =
                        Vector.map
                        (cases, fn {const, rules} =>
                         let
                            val w = 
                               case const of
                                  Const.Word w => w
                                | _ => Error.bug "MatchCompile.const: caseWord type error"
                         in
                            (w, finish (rules, exampleConst const))
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
                  then done (Example.Wild, true)
               else if Tycon.equals (tycon, Tycon.exn)
                  then done (Example.Exn, true)
               else
                  let
                     val cons = tyconCons tycon
                     val unhandled =
                        List.keepAllMap
                        (Vector.toList cons, fn {con, hasArg, ...} =>
                         if Vector.exists (cases, fn {con = con', ...} =>
                                           Con.equals (con, con'))
                            then NONE
                            else SOME (Example.ConApp
                                       {con = con,
                                        arg = if hasArg
                                                 then SOME Example.Wild
                                                 else NONE}))
                  in
                     Option.fold
                     (Example.or unhandled, NONE, fn (e, _) => done (e, false))
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
                  val {arg, con, rhs, ...} = Vector.first cases
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
      and record arg =
         traceRecord
         (fn (vars: Vars.t, rules: Rules.t, facts: Facts.t, es, i, test, fs) =>
         let
            val (var, varTy) = Vector.sub (vars, i)
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
                                       Pat.Record rps => SortedRecord.range rps
                                     | Pat.Wild =>
                                          Vector.tabulate (n, fn _ => Pat.Wild)
                                     | _ => Error.bug "MatchCompile.record: derecord")))
                      in
                         Rule.T {pats = pats, rest = rest}
                      end)
                  val facts =
                     Facts.add
                     (facts, var,
                      Fact.Record (SortedRecord.zip (fs, Vector.map (vars', #1))))
               in
                  match (vars, rules, facts, es)
               end
         in
            if Vector.length fs = 1
               then let val var' = Var.newNoname ()
                    in
                       (* Although 'test' is likely a variable,
                        * must bind to a fresh variable to maintain
                        * a unique Fact.t per variable in Facts.t.
                        *)
                       Exp.lett {var = var', exp = test,
                                 body = body (Vector.new1 (var', varTy))}
                    end
               else Exp.detuple {body = body, tuple = test}
         end) arg
      and vector arg =
         traceVector
         (fn (vars: Vars.t, rules: Rules.t, facts: Facts.t, es, i, test) =>
         let
            val (var, _) = Vector.sub (vars, i)
            val (cases, defaults) =
               Vector.foldr
               (rules, ([], []),
                fn (rule as Rule.T {pats, ...}, (cases, defaults)) =>
                case Vector.sub (pats, i) of
                   Pat.Vector args =>
                      let
                         fun oneCase () =
                            {len = Vector.length args,
                             rules = rule :: defaults}
                         fun insert (cases, ac) =
                            case cases of
                               [] => oneCase () :: ac
                             | ((casee as {len, rules})::cases) =>
                                  if Vector.length args = len
                                     then
                                        {len = len, rules = rule :: rules}
                                        :: List.appendRev (ac, cases)
                                     else insert (cases, casee :: ac)
                      in
                         (insert (cases, []), defaults)
                      end
                 | Pat.Wild =>
                      (List.map (cases, fn {len, rules} =>
                                 {len = len, rules = rule :: rules}),
                       rule :: defaults)
                 | _ => Error.bug "MatchCompile.vector: expected Vector pat")
            val default =
               let
                  val maxLen =
                     List.fold
                     (cases, ~1, fn ({len, ...}, max) =>
                      Int.max (max, len))
                  val unhandled =
                     Example.vectorDots (Vector.new (maxLen + 1, Example.Wild))
                  val unhandled =
                     Int.foldDown
                     (0, maxLen, [unhandled], fn (i, unhandled) =>
                      if List.exists (cases, fn {len, ...} => i = len)
                         then unhandled
                         else (Example.vector (Vector.new (i, Example.Wild))) :: unhandled)
                  val unhandled =
                     Example.or unhandled
               in
                  match (Vector.dropNth (vars, i),
                         Rules.dropNth (Vector.fromList defaults, i),
                         facts,
                         Option.fold
                         (unhandled, es, fn (unhandled, es) =>
                          Examples.add (es, var, unhandled, {isOnlyExns = false})))
               end
            val cases =
               Vector.fromListMap
               (cases, fn {len, rules} =>
                let
                   fun body vars' =
                      let
                         val vars =
                            Vector.concatV
                            (Vector.mapi
                             (vars, fn (i', x) =>
                              if i = i'
                                 then vars'
                                 else Vector.new1 x))
                         val rules =
                            Vector.fromListMap
                            (rules, fn Rule.T {pats, rest} =>
                             let
                                val pats =
                                   Vector.concatV
                                   (Vector.mapi
                                    (pats, fn (i', p) =>
                                     if i <> i'
                                        then Vector.new1 p
                                        else (case p of
                                                 Pat.Vector ps => ps
                                               | Pat.Wild => Vector.new (len, Pat.Wild)
                                               | _ => Error.bug "MatchCompile.vector: devector")))
                             in
                                Rule.T {pats = pats, rest = rest}
                             end)
                      in
                         match (vars, rules,
                                Facts.add (facts, var,
                                           Fact.Vector (Vector.map (vars', #1))),
                                es)
                      end
                in
                   (WordX.fromIntInf (IntInf.fromInt len, WordSize.seqIndex ()),
                    Exp.devector {vector = test, length = len, body = body})
                end)
         in
            Exp.casee
            {cases = Cases.word (WordSize.seqIndex (), cases),
             default = SOME (default, region),
             test = Exp.vectorLength test,
             ty = caseType}
         end) arg
      val examples = ref []
      val res =
         match (Vector.new1 (test, testType),
                Vector.mapi (cases, fn (i, (p, f)) =>
                             Rule.T {pats = Vector.new1 (Pat.fromNestedPat p),
                                     rest = {examples = if i = Vector.length cases - 1
                                                           then SOME examples
                                                           else NONE,
                                             finish = f,
                                             nestedPat = p}}),
                Facts.empty,
                Examples.empty)
      val examples =
         fn {dropOnlyExns} =>
         let
            val example =
               (Example.or o List.keepAllMap)
               (!examples, fn (ex, {isOnlyExns}) =>
                if dropOnlyExns andalso isOnlyExns
                   then NONE
                   else SOME ex)
         in
            Option.map (example, Example.layout)
         end
   in
      (res, examples)
   end

val matchCompile =
   fn {caseType: Type.t,
       cases: (NestedPat.t * (int -> (Var.t -> Var.t) -> Exp.t)) vector,
       conTycon: Con.t -> Tycon.t,
       region: Region.t,
       test: Var.t,
       testType: Type.t,
       tyconCons: Tycon.t -> {con: Con.t,
                              hasArg: bool} vector} =>
   let
      val cases =
         Vector.map
         (cases, fn (pat, mk) =>
          let
             val pats = NestedPat.flatten pat
             val mk = mk (Vector.length pats)
          in
             Vector.map (pats, fn pat => (pat, mk))
          end)
      val cases = Vector.concatV cases
   in
      matchCompile {caseType = caseType,
                    cases = cases,
                    conTycon = conTycon,
                    region = region,
                    test = test,
                    testType = testType,
                    tyconCons = tyconCons}
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
