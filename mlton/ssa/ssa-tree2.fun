(* Copyright (C) 2009,2014,2017-2020,2023-2024 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor SsaTree2 (S: SSA_TREE2_STRUCTS): SSA_TREE2 = 
struct

open S

(* infix declarations for Parse.Ops *)
infix  1 <|> >>=
infix  3 <*> <* *>
infixr 4 <$> <$$> <$$$> <$$$$> <$ <$?>

structure ObjectCon =
   struct
      datatype t =
         Con of Con.t
       | Sequence
       | Tuple

      val equals: t * t -> bool =
         fn (Con c, Con c') => Con.equals (c, c')
          | (Sequence, Sequence) => true
          | (Tuple, Tuple) => true
          | _ => false

      val isSequence: t -> bool =
         fn Sequence => true
          | _ => false

      val layout: t -> Layout.t =
         fn oc =>
         let
            open Layout
         in
            case oc of
               Con c => Con.layout c
             | Sequence => str "sequence"
             | Tuple => str "tuple"
         end

      local
         val conAlts = Vector.fromList [("sequence", Sequence), ("tuple", Tuple)]
      in
         val parse = Con.parseAs (conAlts, Con)
      end
   end

datatype z = datatype ObjectCon.t

structure Type =
   struct
      datatype t =
         T of {hash: Word.t,
               plist: PropertyList.t,
               tree: tree}
      and tree =
          CPointer
        | Datatype of Tycon.t
        | IntInf
        | Object of {args: t Prod.t,
                     con: ObjectCon.t}
        | Real of RealSize.t
        | Thread
        | Weak of t
        | Word of WordSize.t

      local
         fun make f (T r) = f r
      in
         val hash = make #hash
         val plist = make #plist
         val tree = make #tree
      end

      datatype dest = datatype tree

      val dest = tree

      fun equals (t, t') = PropertyList.equals (plist t, plist t')

      val deSequenceOpt: t -> t Prod.t option =
         fn t =>
         case dest t of
            Object {args, con = Sequence} => SOME args
          | _ => NONE

      val deSequence1: t -> t =
         fn t =>
         case deSequenceOpt t of
            SOME args =>
               if Prod.length args = 1
                  then Prod.elt (args, 0)
                  else Error.bug "SsaTree2.Type.deSequence1"
          | _ => Error.bug "SsaTree2.Type.deSequence1"

      val isSequence: t -> bool = isSome o deSequenceOpt

      val deWeakOpt: t -> t option =
         fn t =>
         case dest t of
            Weak t => SOME t
          | _ => NONE

      val deWeak: t -> t = valOf o deWeakOpt

      local
         val same: tree * tree -> bool =
            fn (CPointer, CPointer) => true
             | (Datatype t1, Datatype t2) => Tycon.equals (t1, t2)
             | (IntInf, IntInf) => true
             | (Object {args = a1, con = c1}, Object {args = a2, con = c2}) =>
                  ObjectCon.equals (c1, c2)
                  andalso Prod.equals (a1, a2, equals)
             | (Real s1, Real s2) => RealSize.equals (s1, s2)
             | (Thread, Thread) => true
             | (Weak t1, Weak t2) => equals (t1, t2)
             | (Word s1, Word s2) => WordSize.equals (s1, s2)
             | _ => false
         val table: t HashSet.t = HashSet.new {hash = hash}
      in
         fun lookup (hash, tr) =
            HashSet.lookupOrInsert (table, hash,
                                    fn t => same (tr, tree t),
                                    fn () => T {hash = hash,
                                                plist = PropertyList.new (),
                                                tree = tr})

         fun stats () =
            let open Layout
            in align [seq [str "num types in hash table = ",
                           Int.layout (HashSet.size table)],
                      Control.sizeMessage ("types hash table", lookup)]
            end
      end

      val newHash = Random.word

      local
         fun make f : t -> t =
            let
               val w = newHash ()
            in
               fn t => lookup (Hash.combine (w, hash t), f t)
            end
      in
         val weak = make Weak
      end

      val datatypee: Tycon.t -> t =
         fn t => lookup (Tycon.hash t, Datatype t)

      val bool = datatypee Tycon.bool

      val isBool: t -> bool =
         fn t =>
         case dest t of
            Datatype t => Tycon.equals (t, Tycon.bool)
          | _ => false

      local
         fun make (tycon, tree) = lookup (Tycon.hash tycon, tree)
      in
         val cpointer = make (Tycon.cpointer, CPointer)
         val intInf = make (Tycon.intInf, IntInf)
         val thread = make (Tycon.thread, Thread)
      end

      val real: RealSize.t -> t =
         fn s => lookup (Tycon.hash (Tycon.real s), Real s)

      val word: WordSize.t -> t =
         fn s => lookup (Tycon.hash (Tycon.word s), Word s)

      local
         val tuple = newHash ()
         val sequence = newHash ()
         fun hashProd (p, base) =
            Hash.combine (base, Hash.vectorMap (Prod.dest p, fn {elt, ...} => hash elt))
      in
         fun object {args, con}: t =
            let
               val base =
                  case con of
                     Con c => Con.hash c
                   | Sequence => sequence
                   | Tuple => tuple
               val hash = hashProd (args, base)
            in
               lookup (hash, Object {args = args, con = con})
            end
      end

      fun sequence p = object {args = p, con = Sequence}

      fun array1 ty = sequence (Prod.new1Mutable ty)
      fun vector1 ty = sequence (Prod.new1Immutable ty)

      fun ofConst c =
         let
            datatype z = datatype Const.t
         in
            case c of
               CSymbol _ => cpointer
             | IntInf _ => intInf
             | Null => cpointer
             | Real r => real (RealX.size r)
             | Word w => word (WordX.size w)
             | WordVector v => vector1 (word (WordXVector.elementSize v))
         end

      fun conApp (con, args) = object {args = args, con = Con con}

      fun tuple ts = object {args = ts, con = Tuple}

      fun reff1 t =
         object {args = Prod.make (Vector.new1 {elt = t, isMutable = true}),
                 con = Tuple}

      val unit: t = tuple (Prod.empty ())

      val isUnit: t -> bool =
         fn t =>
         case dest t of
            Object {args, con = Tuple} => Prod.isEmpty args
          | _ => false

      local
         open Layout
      in
         val {get = layout, ...} =
            Property.get
            (plist,
             Property.initRec
             (fn (t, layout) =>
              case dest t of
                 CPointer => str "cpointer"
               | Datatype t => Tycon.layout t
               | IntInf => str "intInf"
               | Object {args, con} =>
                    if isUnit t
                       then str "unit"
                    else seq [Prod.layout (args, layout),
                              str " ",
                              ObjectCon.layout con]
               | Real s => str (concat ["real", RealSize.toString s])
               | Thread => str "thread"
               | Weak t => seq [paren (layout t), str " ", str "weak"]
               | Word s => str (concat ["word", WordSize.toString s])))
      end

      local
         structure P = Parse
         open Parse.Ops

         val tyconAlts =
            Vector.fromList
            ([("cpointer", cpointer),
              ("intInf", intInf),
              ("unit", unit),
              ("thread", thread)] @
             List.map (WordSize.all, fn ws => ("word" ^ WordSize.toString ws, word ws)) @
             List.map (RealSize.all, fn rs => ("real" ^ RealSize.toString rs, real rs)))
         val unary =
            Con.parseAs (Vector.new3 (("sequence", sequence o Prod.new1Immutable),
                                      ("tuple", tuple o Prod.new1Immutable),
                                      ("weak", weak)),
                         fn con => fn ty =>
                         conApp (con, Prod.new1Immutable ty))
      in
         fun parse () =
            let
               val parse = P.delay parse
            in
               Tycon.parseAs (tyconAlts, datatypee)
               <|>
               (P.paren parse >>= (fn ty =>
                unary >>= (fn unary =>
                P.pure (unary ty))))
               <|>
               (Prod.parse parse >>= (fn args =>
                ObjectCon.parse >>= (fn con =>
                P.pure (object {args = args, con = con}))))
            end
         val parse = parse ()
      end

      fun checkPrimApp {args, prim, result}: bool =
         let
            exception BadPrimApp
            fun default () =
               let
                  val targs =
                     Prim.extractTargs
                     (prim,
                      {args = args,
                       result = result,
                       typeOps = {deArray = fn _ => raise BadPrimApp,
                                  deArrow = fn _ => raise BadPrimApp,
                                  deRef = fn _ => raise BadPrimApp,
                                  deVector = fn _ => raise BadPrimApp,
                                  deWeak = deWeak}})
               in
                  Prim.checkApp
                  (prim,
                   {args = args,
                    result = result,
                    targs = targs,
                    typeOps = {array = array1,
                               arrow = fn _ => raise BadPrimApp,
                               bool = bool,
                               cpointer = cpointer,
                               equals = equals,
                               exn = unit,
                               intInf = intInf,
                               real = real,
                               reff = fn _ => raise BadPrimApp,
                               thread = thread,
                               unit = unit,
                               vector = vector1,
                               weak = weak,
                               word = word}})
               end
            val default = fn () =>
               (default ()) handle BadPrimApp => false

            fun arg i = Vector.sub (args, i)
            fun oneArg f = 1 = Vector.length args andalso f (arg 0)
            fun twoArgs f = 2 = Vector.length args andalso f (arg 0, arg 1)
            fun fiveArgs f = 5 = Vector.length args andalso f (arg 0, arg 1, arg 2, arg 3, arg 4)
            val seqIndex = word (WordSize.seqIndex ())
         in
            case prim of
               Prim.Array_alloc _ =>
                  oneArg
                  (fn n =>
                   case deSequenceOpt result of
                      SOME resp =>
                         Prod.allAreMutable resp
                         andalso equals (n, seqIndex)
                    | _ => false)
             | Prim.Array_copyArray =>
                  fiveArgs
                  (fn (dst, di, src, si, len) =>
                   case (deSequenceOpt dst, deSequenceOpt src) of
                      (SOME dstp, SOME srcp) =>
                         Vector.equals (Prod.dest dstp, Prod.dest srcp,
                                        fn ({elt = dstElt, isMutable = dstIsMutable},
                                            {elt = srcElt, isMutable = srcIsMutable}) =>
                                        dstIsMutable andalso srcIsMutable
                                        andalso equals (dstElt, srcElt))
                         andalso equals (di, seqIndex)
                         andalso equals (si, seqIndex)
                         andalso equals (len, seqIndex)
                         andalso isUnit result
                    | _ => false)
             | Prim.Array_copyVector =>
                  fiveArgs
                  (fn (dst, di, src, si, len) =>
                   case (deSequenceOpt dst, deSequenceOpt src) of
                      (SOME dstp, SOME srcp) =>
                         Vector.equals (Prod.dest dstp, Prod.dest srcp,
                                        fn ({elt = dstElt, isMutable = dstIsMutable},
                                            {elt = srcElt, ...}) =>
                                        dstIsMutable
                                        andalso equals (dstElt, srcElt))
                         andalso equals (di, seqIndex)
                         andalso equals (si, seqIndex)
                         andalso equals (len, seqIndex)
                         andalso isUnit result
                    | _ => false)
             | Prim.Array_length =>
                  oneArg
                  (fn a =>
                   isSequence a andalso equals (result, seqIndex))
             | Prim.Array_toArray =>
                  oneArg
                  (fn arr =>
                   case (deSequenceOpt arr, deSequenceOpt result) of
                      (SOME arrp, SOME resp) =>
                         Vector.equals (Prod.dest arrp, Prod.dest resp,
                                        fn ({elt = arrElt, isMutable = arrIsMutable},
                                            {elt = resElt, isMutable = resIsMutable}) =>
                                        arrIsMutable andalso resIsMutable
                                        andalso equals (arrElt, resElt))
                    | _ => false)
             | Prim.Array_toVector =>
                  oneArg
                  (fn arr =>
                   case (deSequenceOpt arr, deSequenceOpt result) of
                      (SOME arrp, SOME resp) =>
                         Vector.equals (Prod.dest arrp, Prod.dest resp,
                                        fn ({elt = arrElt, isMutable = arrIsMutable},
                                            {elt = resElt, ...}) =>
                                        arrIsMutable
                                        andalso equals (arrElt, resElt))
                    | _ => false)
             | Prim.Array_uninit =>
                  twoArgs
                  (fn (arr, i) =>
                   case deSequenceOpt arr of
                      SOME arrp =>
                         Prod.allAreMutable arrp
                         andalso equals (i, seqIndex)
                         andalso isUnit result
                    | _ => false)
             | Prim.Array_uninitIsNop =>
                  oneArg
                  (fn arr =>
                   case deSequenceOpt arr of
                      SOME arrp =>
                         Prod.allAreMutable arrp
                         andalso isBool result
                    | _ => false)
             | _ => default ()
         end
   end

structure Base =
   struct
      datatype 'a t =
         Object of 'a
       | SequenceSub of {index: 'a,
                         sequence: 'a}

      fun layout (base: 'a t, layoutX: 'a -> Layout.t): Layout.t =
         let
            open Layout
         in
            case base of
               Object x => layoutX x
             | SequenceSub {index, sequence} =>
                  seq [str "$", tuple [layoutX sequence, layoutX index]]
         end

      fun parse (parseX: 'a Parse.t): 'a t Parse.t =
         let
            open Parse
         in
            SequenceSub <$>
            (mlSpaces *> str "$(" *>
             parseX >>= (fn sequence =>
             mlSpaces *> str "," *>
             parseX >>= (fn index =>
             mlSpaces *> str ")" *>
             pure {index = index, sequence = sequence})))
            <|>
            (Object <$> parseX)
         end

      fun layoutWithOffset (base: 'a t, offset, layoutX: 'a -> Layout.t): Layout.t =
         let
            open Layout
         in
            seq [str "#", Int.layout offset, str " ",
                 layout (base, layoutX)]
         end

      fun parseWithOffset (parseX: 'a Parse.t): ('a t * int) Parse.t =
         let
            open Parse
         in
            mlSpaces *> char #"#" *>
            (peek (nextSat Char.isDigit) *>
             fromScan (Function.curry Int.scan StringCvt.DEC)) >>= (fn offset =>
            parse parseX >>= (fn base =>
            pure (base, offset)))
         end

      val equals: 'a t * 'a t * ('a * 'a -> bool) -> bool =
         fn (b1, b2, equalsX) =>
         case (b1, b2) of
            (Object x1, Object x2) => equalsX (x1, x2)
          | (SequenceSub {index = i1, sequence = v1},
             SequenceSub {index = i2, sequence = v2}) =>
               equalsX (i1, i2) andalso equalsX (v1, v2)
          | _ => false

      fun object (b: 'a t): 'a =
         case b of
            Object x => x
          | SequenceSub {sequence = x, ...} => x

      local
         val newHash = Random.word
         val object = newHash ()
         val sequenceSub = newHash ()
      in
         val hash: 'a t * ('a -> word) -> word =
            fn (b, hashX) =>
            case b of
               Object x => Hash.combine (object, hashX x)
             | SequenceSub {index, sequence} =>
                  Hash.combine3 (hashX index, hashX sequence, sequenceSub)
      end

      fun foreach (b: 'a t, f: 'a -> unit): unit =
         case b of
            Object x => f x
          | SequenceSub {index, sequence} => (f index; f sequence)

      fun map (b: 'a t, f: 'a -> 'b): 'b t =
         case b of
            Object x => Object (f x)
          | SequenceSub {index, sequence} => SequenceSub {index = f index,
                                                          sequence = f sequence}
   end

structure Exp =
   struct
      datatype t =
         Const of Const.t
       | Inject of {sum: Tycon.t,
                    variant: Var.t}
       | Object of {con: Con.t option,
                    args: Var.t vector}
       | PrimApp of {prim: Type.t Prim.t,
                     args: Var.t vector}
       | Select of {base: Var.t Base.t,
                    offset: int}
       | Sequence of {args: Var.t vector vector}
       | Var of Var.t

      val unit = Object {con = NONE, args = Vector.new0 ()}

      fun foreachVar (e, v) =
         let
            fun vs xs = Vector.foreach (xs, v)
         in
            case e of
               Const _ => ()
             | Inject {variant, ...} => v variant
             | Object {args, ...} => vs args
             | PrimApp {args, ...} => vs args
             | Select {base, ...} => Base.foreach (base, v)
             | Sequence {args, ...} => Vector.foreach (args, vs)
             | Var x => v x
         end

      fun replaceVar (e, fx) =
         let
            fun fxs xs = Vector.map (xs, fx)
         in
            case e of
               Const _ => e
             | Inject {sum, variant} => Inject {sum = sum, variant = fx variant}
             | Object {con, args} => Object {con = con, args = fxs args}
             | PrimApp {prim, args} => PrimApp {args = fxs args, prim = prim}
             | Select {base, offset} =>
                  Select {base = Base.map (base, fx), offset = offset}
             | Sequence {args} =>
                  Sequence {args = Vector.map (args, fxs)}
             | Var x => Var (fx x)
         end

      fun layout' (e, layoutVar) =
         let
            open Layout
            fun layoutArgs xs = Vector.layout layoutVar xs
         in
            case e of
               Const c => Const.layout c
             | Inject {sum, variant} =>
                  seq [str "inj ", paren (layoutVar variant), str ": ", Tycon.layout sum]
             | Object {con, args} =>
                  seq [str "obj ",
                       (case con of
                           NONE => empty
                         | SOME c => seq [Con.layout c, str " "]),
                       layoutArgs args]
             | PrimApp {args, prim} =>
                  seq [str "prim ", Prim.layoutFull (prim, Type.layout), str " ", layoutArgs args]
             | Select {base, offset} =>
                  Base.layoutWithOffset (base, offset, layoutVar)
             | Sequence {args} =>
                  seq [str "seq ",
                       Vector.layout layoutArgs args]
             | Var x => layoutVar x
         end

      fun layout e = layout' (e, Var.layout)

      val parse =
         let
            open Parse
            val parseArgs = vector Var.parse
         in
            mlSpaces *> any
            [Const <$> Const.parse,
             Inject <$>
             (kw "inj" *>
              paren Var.parse >>= (fn variant =>
              sym ":" *>
              Tycon.parse >>= (fn sum =>
              pure {variant = variant, sum = sum}))),
             Object <$>
             (kw "obj" *>
              optional Con.parse >>= (fn con =>
              parseArgs >>= (fn args =>
              pure {con = con, args = args}))),
             PrimApp <$>
             (kw "prim" *>
              Prim.parseFull Type.parse >>= (fn prim =>
              parseArgs >>= (fn args =>
              pure {prim = prim, args = args}))),
             Select <$>
             (Base.parseWithOffset Var.parse >>= (fn (base, offset) =>
              pure {base = base, offset = offset})),
             Sequence <$>
             (kw "seq" *>
              vector parseArgs >>= (fn args =>
              pure {args = args})),
             Var <$> Var.parse]
         end

      fun maySideEffect (e: t): bool =
         case e of
            Const _ => false
          | Inject _ => false
          | Object _ => false
          | PrimApp {prim,...} => Prim.maySideEffect prim
          | Select _ => false
          | Sequence _ => false
          | Var _ => false

      fun varsEquals (xs, xs') = Vector.equals (xs, xs', Var.equals)

      fun equals (e: t, e': t): bool =
         case (e, e') of
            (Const c, Const c') => Const.equals (c, c')
          | (Object {con, args}, Object {con = con', args = args'}) =>
               Option.equals (con, con', Con.equals)
               andalso varsEquals (args, args')
          | (PrimApp {prim, args, ...},
             PrimApp {prim = prim', args = args', ...}) =>
               Prim.equals (prim, prim') andalso varsEquals (args, args')
          | (Select {base = b1, offset = i1}, Select {base = b2, offset = i2}) =>
               Base.equals (b1, b2, Var.equals) andalso i1 = i2
          | (Sequence {args}, Sequence {args = args'}) =>
               Vector.equals (args, args', varsEquals)
          | (Var x, Var x') => Var.equals (x, x')
          | _ => false
      (* quell unused warning *)
      val _ = equals

      local
         val newHash = Random.word
         val inject = newHash ()
         val primApp = newHash ()
         val select = newHash ()
         val sequence = newHash ()
         val tuple = newHash ()
         fun hashVars (xs: Var.t vector, w: Word.t): Word.t =
            Hash.combine (w, Hash.vectorMap (xs, Var.hash))
      in
         val hash: t -> Word.t =
            fn Const c => Const.hash c
             | Inject {sum, variant} =>
                  Hash.combine3 (inject, Tycon.hash sum, Var.hash variant)
             | Object {con, args, ...} =>
                  hashVars (args,
                            case con of
                               NONE => tuple
                             | SOME c => Con.hash c)
             | PrimApp {args, ...} => hashVars (args, primApp)
             | Select {base, offset} =>
                  Hash.combine3 (select, Base.hash (base, Var.hash), Word.fromInt offset)
             | Sequence {args} =>
                  Hash.combine (sequence,
                                Hash.vectorMap (args, fn args =>
                                                Hash.vectorMap (args, Var.hash)))
             | Var x => Var.hash x
      end
      (* quell unused warning *)
      val _ = hash
   end
datatype z = datatype Exp.t

structure Statement =
   struct
      datatype t =
         Bind of {var: Var.t option,
                  ty: Type.t,
                  exp: Exp.t}
       | Profile of ProfileExp.t
       | Update of {base: Var.t Base.t,
                    offset: int,
                    value: Var.t}

      fun layout' (s: t, layoutVar): Layout.t =
         let
            open Layout
         in
            case s of
               Bind {var, ty, exp} =>
                  let
                     val (sep, ty) =
                        if !Control.showTypes
                           then (str ":", indent (seq [Type.layout ty, str " ="], 2))
                           else (str " =", empty)
                  in
                     mayAlign [mayAlign [seq [str "val ",
                                              case var of
                                                 NONE => str "_"
                                               | SOME var => Var.layout var,
                                              sep],
                                         ty],
                               indent (Exp.layout' (exp, layoutVar), 2)]
                  end
             | Profile p => seq [str "prof ", ProfileExp.layout p]
             | Update {base, offset, value} =>
                  mayAlign [seq [str "upd ",
                                 Base.layoutWithOffset (base, offset, layoutVar),
                                 str " :="],
                            layoutVar value]
         end
      fun layout s = layout' (s, Var.layout)

      val parse =
         let
            open Parse
         in
            mlSpaces *> any
            [Bind <$>
             (kw "val" *>
              ((SOME <$> Var.parse) <|> (NONE <$ kw "_")) >>= (fn var =>
              sym ":" *> Type.parse >>= (fn ty =>
              sym "=" *> Exp.parse >>= (fn exp =>
              pure {var = var, ty = ty, exp = exp})))),
             Update <$>
             (kw "upd" *>
              Base.parseWithOffset Var.parse >>= (fn (base, offset) =>
              sym ":=" *> Var.parse >>= (fn value =>
              pure {base = base, offset = offset, value = value})))]
         end

      val profile = Profile

      fun foreachDef (s: t, f: Var.t * Type.t -> unit): unit =
         case s of
            Bind {ty, var, ...} => Option.app (var, fn x => f (x, ty))
          | _ => ()

      fun clear s = foreachDef (s, Var.clear o #1)

      fun prettifyGlobals (v: t vector): Var.t -> Layout.t =
         let
            val {get = global: Var.t -> Layout.t, set = setGlobal, ...} =
               Property.getSet (Var.plist, Property.initFun Var.layout)
            val _ = 
               Vector.foreach
               (v, fn s =>
                case s of
                   Bind {var, exp, ...} =>
                      Option.app
                      (var, fn var =>
                       let
                          fun set () =
                             let
                                val s = Layout.toString (Exp.layout' (exp, Var.layout))
                                val maxSize = 20
                                val dots = " ... "
                                val dotsSize = String.size dots
                                val frontSize = 2 * (maxSize - dotsSize) div 3
                                val backSize = maxSize - dotsSize - frontSize
                                val s = 
                                   if String.size s > maxSize
                                      then concat [String.prefix (s, frontSize),
                                                   dots,
                                                   String.suffix (s, backSize)]
                                      else s
                             in
                                if String.hasSubstring (s, {substring = "(*"})
                                   orelse String.hasSubstring (s, {substring = "*)"})
                                   then ()
                                   else setGlobal (var, Layout.seq [Var.layout var,
                                                                    Layout.str (" (*" ^ s ^ "*)")])
                             end
                       in
                          case exp of
                             Const _ => set ()
                           | Object {con, args, ...} =>
                                (case con of
                                    NONE => if Vector.isEmpty args then set () else ()
                                  | SOME _ => set ())
                           | _ => ()
                       end)
                 | _ => ())
         in
            global
         end

      fun foreachUse (s: t, f: Var.t -> unit): unit =
         case s of
            Bind {exp, ...} => Exp.foreachVar (exp, f)
          | Profile _ => ()
          | Update {base, value, ...} => (Base.foreach (base, f); f value)

      fun replaceDefsUses (s: t, {def: Var.t -> Var.t, use: Var.t -> Var.t}): t =
         case s of
            Bind {exp, ty, var} =>
               Bind {exp = Exp.replaceVar (exp, use),
                     ty = ty,
                     var = Option.map (var, def)}
          | Profile _ => s
          | Update {base, offset, value} =>
               Update {base = Base.map (base, use),
                       offset = offset,
                       value = use value}

      fun replaceUses (s, f) = replaceDefsUses (s, {def = fn x => x, use = f})
   end

datatype z = datatype Statement.t

structure Transfer =
   struct
      datatype t =
         Bug (* MLton thought control couldn't reach here. *)
       | Call of {args: Var.t vector,
                  func: Func.t,
                  return: Return.t}
       | Case of {test: Var.t,
                  cases: (Con.t, Label.t) Cases.t,
                  default: Label.t option} (* Must be nullary. *)
       | Goto of {dst: Label.t,
                  args: Var.t vector}
       | Raise of Var.t vector
       | Return of Var.t vector
       | Runtime of {prim: Type.t Prim.t,
                     args: Var.t vector,
                     return: Label.t}

      fun foreachFuncLabelVar (t, func: Func.t -> unit, label: Label.t -> unit, var) =
         let
            fun vars xs = Vector.foreach (xs, var)
         in
            case t of
               Bug => ()
             | Call {func = f, args, return, ...} =>
                  (func f
                   ; Return.foreachLabel (return, label)
                   ; vars args)
             | Case {test, cases, default, ...} =>
                  (var test
                   ; Cases.foreach (cases, label)
                   ; Option.app (default, label))
             | Goto {dst, args, ...} => (vars args; label dst)
             | Raise xs => vars xs
             | Return xs => vars xs
             | Runtime {args, return, ...} =>
                  (vars args
                   ; label return)
         end

      fun foreachFunc (t, func) =
         foreachFuncLabelVar (t, func, fn _ => (), fn _ => ())
      (* quell unused warning *)
      val _ = foreachFunc

      fun foreachLabelVar (t, label, var) =
         foreachFuncLabelVar (t, fn _ => (), label, var)

      fun foreachLabel (t, j) = foreachLabelVar (t, j, fn _ => ())
      fun foreachVar (t, v) = foreachLabelVar (t, fn _ => (), v)

      fun replaceLabelVar (t, fl, fx) =
         let
            fun fxs xs = Vector.map (xs, fx)
         in
            case t of
               Bug => Bug
             | Call {func, args, return} =>
                  Call {func = func, 
                        args = fxs args,
                        return = Return.map (return, fl)}
             | Case {test, cases, default} =>
                  Case {test = fx test, 
                        cases = Cases.map(cases, fl),
                        default = Option.map(default, fl)}
             | Goto {dst, args} => 
                  Goto {dst = fl dst, 
                        args = fxs args}
             | Raise xs => Raise (fxs xs)
             | Return xs => Return (fxs xs)
             | Runtime {prim, args, return} =>
                  Runtime {prim = prim,
                           args = fxs args,
                           return = fl return}
         end

      fun replaceLabel (t, f) = replaceLabelVar (t, f, fn x => x)
      (* quell unused warning *)
      val _ = replaceLabel
      fun replaceVar (t, f) = replaceLabelVar (t, fn l => l, f)

      fun layout' (t, layoutVar) =
         let
            open Layout
            fun layoutArgs xs = Vector.layout layoutVar xs
            fun layoutCase {test, cases, default} =
               let
                  fun doit (l, layout) =
                     Vector.toListMap
                     (l, fn (i, l) =>
                      seq [layout i, str " => ", Label.layout l])
                  datatype z = datatype Cases.t
                  val (suffix, cases) =
                     case cases of
                        Con l => (empty, doit (l, Con.layout))
                      | Word (size, l) => (str (WordSize.toString size),
                                           doit (l, fn w => (WordX.layout (w, {suffix = true}))))
                  val cases =
                     case default of
                        NONE => cases
                      | SOME j =>
                           cases @ [seq [str "_ => ", Label.layout j]]
               in
                  align [seq [str "case", suffix, str " ", layoutVar test, str " of"],
                         indent (alignPrefix (cases, "| "), 2)]
               end
            fun layoutPrim {prim, args} =
               seq [Prim.layoutFull (prim, Type.layout), str " ", layoutArgs args]
         in
            case t of
               Bug => str "bug"
             | Call {func, args, return} =>
                  let
                     val call = seq [Func.layout func, str " ", layoutArgs args]
                  in
                     case return of
                        Return.Dead => seq [str "call dead ", call]
                      | Return.NonTail {cont, handler} =>
                           seq [str "call ", Label.layout cont, str " ",
                                paren call,
                                str " handle _ => ",
                                case handler of
                                   Handler.Caller => str "raise"
                                 | Handler.Dead => str "dead"
                                 | Handler.Handle l => Label.layout l]
                      | Return.Tail => seq [str "call tail ", call]
                  end
             | Case arg => layoutCase arg
             | Goto {dst, args} =>
                  seq [str "goto ", Label.layout dst, str " ", layoutArgs args]
             | Raise xs => seq [str "raise ", layoutArgs xs]
             | Return xs => seq [str "return ", layoutArgs xs]
             | Runtime {prim, args, return} =>
                  seq [str "runtime ", Label.layout return, str " ",
                       paren (layoutPrim {prim = prim, args = args})]
         end
      fun layout t = layout' (t, Var.layout)

      val parse =
         let
            open Parse
            val parseArgs = vector Var.parse
            fun parseCase (parse', mk) =
               Var.parse >>= (fn test =>
               kw "of" *>
               (Vector.fromList <$>
                sepBy (parse' >>= (fn p =>
                       sym "=>" *>
                       Label.parse >>= (fn l =>
                       pure (p, l))),
                       sym "|")) >>= (fn cases =>
               optional ((if Vector.isEmpty cases then pure () else sym "|") *>
                         kw "_" *> sym "=>" *> Label.parse) >>= (fn default =>
               pure {test = test,
                     cases = mk cases,
                     default = default})))
            val parseCall =
               Func.parse >>= (fn func =>
               parseArgs >>= (fn args =>
               pure (fn return => pure {func = func, args = args, return = return})))
         in
            mlSpaces *> any
            [Bug <$ kw "bug",
             Call <$>
             (kw "call" *>
              mlSpaces *>
              any [kw "dead" *> parseCall >>= (fn mkCall => mkCall Return.Dead),
                   kw "tail" *> parseCall >>= (fn mkCall => mkCall Return.Tail),
                   Label.parse >>= (fn cont =>
                   paren parseCall >>= (fn mkCall =>
                   kw "handle" *> kw "_" *> sym "=>" *>
                   any [kw "raise" *> mkCall (Return.NonTail {cont = cont, handler = Handler.Caller}),
                        kw "dead" *> mkCall (Return.NonTail {cont = cont, handler = Handler.Dead}),
                        Label.parse >>= (fn h => mkCall (Return.NonTail {cont = cont, handler = Handler.Handle h}))]))]),
             Case <$>
             any ((kw "case" *> parseCase (Con.parse, Cases.Con)) ::
                  (List.map (WordSize.all, fn ws =>
                             kw ("case" ^ WordSize.toString ws) *>
                             parseCase (WordX.parse, fn cases => Cases.Word (ws, cases))))),
             Goto <$>
             (kw "goto" *>
              Label.parse >>= (fn dst =>
              parseArgs >>= (fn args =>
              pure {dst = dst, args = args}))),
             Raise <$> (kw "raise" *> parseArgs),
             Return <$> (kw "return" *> parseArgs),
             Runtime <$>
             (kw "runtime" *>
              Label.parse >>= (fn return =>
              paren (Prim.parseFull Type.parse >>= (fn prim =>
                     parseArgs >>= (fn args =>
                     pure (prim, args)))) >>= (fn (prim, args) =>
              pure {prim = prim, args = args, return = return})))]
         end

      fun varsEquals (xs, xs') = Vector.equals (xs, xs', Var.equals)

      fun equals (e: t, e': t): bool =
         case (e, e') of
            (Bug, Bug) => true
          | (Call {func, args, return}, 
             Call {func = func', args = args', return = return'}) =>
               Func.equals (func, func') andalso
               varsEquals (args, args') andalso
               Return.equals (return, return')
          | (Case {test, cases, default},
             Case {test = test', cases = cases', default = default'}) =>
               Var.equals (test, test')
               andalso Cases.equals (cases, cases', Con.equals, Label.equals)
               andalso Option.equals (default, default', Label.equals)
          | (Goto {dst, args}, Goto {dst = dst', args = args'}) =>
               Label.equals (dst, dst') andalso
               varsEquals (args, args')
          | (Raise xs, Raise xs') => varsEquals (xs, xs')
          | (Return xs, Return xs') => varsEquals (xs, xs')
          | (Runtime {prim, args, return},
             Runtime {prim = prim', args = args', return = return'}) =>
               Prim.equals (prim, prim') andalso
               varsEquals (args, args') andalso
               Label.equals (return, return')
          | _ => false
      (* quell unused warning *)
      val _ = equals

      local
         val newHash = Random.word
         val bug = newHash ()
         val raisee = newHash ()
         val return = newHash ()
         fun hashVars (xs: Var.t vector, w: Word.t): Word.t =
            Hash.combine (w, Hash.vectorMap (xs, Var.hash))
         fun hash2 (w1: Word.t, w2: Word.t) = Hash.combine (w1, w2)
      in
         val hash: t -> Word.t =
            fn Bug => bug
             | Call {func, args, return} =>
                  hashVars (args, hash2 (Func.hash func, Return.hash return))
             | Case {test, cases, default} =>
                  hash2 (Var.hash test, 
                         Cases.fold
                         (cases, 
                          Option.fold
                          (default, 0wx55555555, 
                           fn (l, w) => 
                           hash2 (Label.hash l, w)),
                          fn (l, w) => 
                          hash2 (Label.hash l, w)))
             | Goto {dst, args} =>
                  hashVars (args, Label.hash dst)
             | Raise xs => hashVars (xs, raisee)
             | Return xs => hashVars (xs, return)
             | Runtime {args, return, ...} => hashVars (args, Label.hash return)
      end
      (* quell unused warning *)
      val _ = hash
   end
datatype z = datatype Transfer.t

local
   open Layout
in
   fun layoutFormals (xts: (Var.t * Type.t) vector) =
      Vector.layout (fn (x, t) =>
                     if !Control.showTypes
                        then mayAlign [seq [Var.layout x, str ":"],
                                       indent (Type.layout t, 2)]
                        else Var.layout x)
      xts
end
local
   open Parse
in
   val parseFormals =
      vector (Var.parse >>= (fn x =>
              sym ":" *>
              Type.parse >>= (fn ty =>
              pure (x, ty))))
end

structure Block =
   struct
      datatype t =
         T of {args: (Var.t * Type.t) vector,
               label: Label.t,
               statements: Statement.t vector,
               transfer: Transfer.t}

      local
         fun make f (T r) = f r
      in
         val args = make #args
         val label = make #label
         val transfer = make #transfer
      end

      fun layout' (T {label, args, statements, transfer}, layoutVar) =
         let
            open Layout
            fun layoutStatement s = Statement.layout' (s, layoutVar)
            fun layoutTransfer t = Transfer.layout' (t, layoutVar)
         in
            align [seq [str "block ", Label.layout label, str " ",
                        layoutFormals args],
                   indent (align
                           [align
                            (Vector.toListMap (statements, layoutStatement)),
                            layoutTransfer transfer],
                           2)]
         end
      fun layout b = layout' (b, Var.layout)

      val parse =
         let
            open Parse
         in
            T <$>
            (kw "block" *>
             Label.parse >>= (fn label =>
             parseFormals >>= (fn args =>
             many Statement.parse >>= (fn statements =>
             Transfer.parse >>= (fn transfer =>
             pure {label = label,
                   args = args,
                   statements = Vector.fromList statements,
                   transfer = transfer})))))
         end

      fun clear (T {label, args, statements, ...}) =
         (Label.clear label
          ; Vector.foreach (args, Var.clear o #1)
          ; Vector.foreach (statements, Statement.clear))
   end

structure Datatype =
   struct
      datatype t =
         T of {cons: {args: Type.t Prod.t,
                      con: Con.t} vector,
               tycon: Tycon.t}

      fun layout (T {cons, tycon}) =
         let
            open Layout
         in
            seq [str "datatype ", Tycon.layout tycon,
                 str " = ",
                 alignPrefix
                 (Vector.toListMap
                  (cons, fn {con, args} =>
                   seq [Prod.layout (args, Type.layout), str " ",
                        Con.layout con]),
                  "| ")]
         end

      val parse =
         let
            open Parse
         in
            T <$>
            (kw "datatype" *> Tycon.parse >>= (fn tycon =>
             sym "=" *>
             sepBy (Prod.parse Type.parse >>= (fn args =>
                    Con.parse >>= (fn con =>
                    pure {con = con, args = args})),
                    sym "|") >>= (fn cons =>
             pure {tycon = tycon, cons = Vector.fromList cons})))
         end

      fun clear (T {cons, tycon}) =
         (Tycon.clear tycon
          ; Vector.foreach (cons, Con.clear o #con))
   end

structure Function =
   struct
      structure CPromise = ClearablePromise

      type dest = {args: (Var.t * Type.t) vector,
                   blocks: Block.t vector,
                   mayInline: bool,
                   name: Func.t,
                   raises: Type.t vector option,
                   returns: Type.t vector option,
                   start: Label.t}

      (* There is a messy interaction between the laziness used in controlFlow
       * and the property lists on labels because the former stores
       * stuff on the property lists.  So, if you force the laziness, then
       * clear the property lists, then try to use the lazy stuff, you will
       * get screwed with undefined properties.  The right thing to do is reset
       * the laziness when the properties are cleared.
       *)
      datatype t =
         T of {controlFlow:
               {dfsTree: unit -> Block.t Tree.t,
                dominatorTree: unit -> Block.t Tree.t,
                graph: unit DirectedGraph.t,
                labelNode: Label.t -> unit DirectedGraph.Node.t,
                nodeBlock: unit DirectedGraph.Node.t -> Block.t} CPromise.t,
               dest: dest}

      local
         fun make f (T {dest, ...}) = f dest
      in
         val blocks = make #blocks
         val dest = make (fn d => d)
         val name = make #name
      end

      fun foreachVar (f: t, fx: Var.t * Type.t -> unit): unit =
         let
            val {args, blocks, ...} = dest f
            val _ = Vector.foreach (args, fx)
            val _ =
               Vector.foreach
               (blocks, fn Block.T {args, statements, ...} =>
                (Vector.foreach (args, fx)
                 ; Vector.foreach (statements, fn s =>
                                   Statement.foreachDef (s, fx))))
         in
            ()
         end

      fun controlFlow (T {controlFlow, ...}) =
         let
            val {graph, labelNode, nodeBlock, ...} = CPromise.force controlFlow
         in
            {graph = graph, labelNode = labelNode, nodeBlock = nodeBlock}
         end

      local
         fun make sel =
            fn T {controlFlow, ...} => sel (CPromise.force controlFlow) ()
      in
         val dominatorTree = make #dominatorTree
      end

      fun dfs (f, v) =
         let
            val {blocks, start, ...} = dest f
            val numBlocks = Vector.length blocks
            val {get = labelIndex, set = setLabelIndex, rem, ...} =
               Property.getSetOnce (Label.plist,
                                    Property.initRaise ("index", Label.layout))
            val _ = Vector.foreachi (blocks, fn (i, Block.T {label, ...}) =>
                                     setLabelIndex (label, i))
            val visited = Array.array (numBlocks, false)
            fun visit (l: Label.t): unit =
               let
                  val i = labelIndex l
               in
                  if Array.sub (visited, i)
                     then ()
                  else
                     let
                        val _ = Array.update (visited, i, true)
                        val b as Block.T {transfer, ...} =
                           Vector.sub (blocks, i)
                        val v' = v b
                        val _ = Transfer.foreachLabel (transfer, visit)
                        val _ = v' ()
                     in
                        ()
                     end
               end
            val _ = visit start
            val _ = Vector.foreach (blocks, rem o Block.label)
         in
            ()
         end

      local
         structure Graph = DirectedGraph
         structure Node = Graph.Node
         structure Edge = Graph.Edge
      in
         fun determineControlFlow ({blocks, start, ...}: dest) =
            let
               open Dot
               val g = Graph.new ()
               fun newNode () = Graph.newNode g
               val {get = labelNode, ...} =
                  Property.get
                  (Label.plist, Property.initFun (fn _ => newNode ()))
               val {get = nodeInfo: unit Node.t -> {block: Block.t},
                    set = setNodeInfo, ...} =
                  Property.getSetOnce
                  (Node.plist, Property.initRaise ("info", Node.layout))
               val _ =
                  Vector.foreach
                  (blocks, fn b as Block.T {label, transfer, ...} =>
                   let
                      val from = labelNode label
                      val _ = setNodeInfo (from, {block = b})
                      val _ =
                         Transfer.foreachLabel
                         (transfer, fn to =>
                          (ignore o Graph.addEdge) 
                          (g, {from = from, to = labelNode to}))
                   in
                      ()
                   end)
               val root = labelNode start
               val dfsTree =
                  Promise.lazy
                  (fn () =>
                   Graph.dfsTree (g, {root = root,
                                      nodeValue = #block o nodeInfo}))
               val dominatorTree =
                  Promise.lazy
                  (fn () =>
                   Graph.dominatorTree (g, {root = root,
                                            nodeValue = #block o nodeInfo}))
            in
               {dfsTree = dfsTree,
                dominatorTree = dominatorTree,
                graph = g,
                labelNode = labelNode,
                nodeBlock = #block o nodeInfo}
            end

         fun layoutDot (f, layoutVar) =
            let
               fun toStringFormals args = Layout.toString (layoutFormals args)
               fun toStringHeader (name, args) = concat [name, " ", toStringFormals args]
               fun toStringBlockHeader (label, args) =
                  toStringHeader (Label.toString label, args)
               fun toStringFunctionHeader (func, args) =
                  toStringHeader ("fun " ^ Func.toString func, args)
               fun toStringStatement s = Layout.toString (Statement.layout' (s, layoutVar))
               fun toStringTransfer t =
                  Layout.toString
                  (case t of
                      Case {test, ...} =>
                         Layout.seq [Layout.str "case ", layoutVar test]
                    | _ => Transfer.layout' (t, layoutVar))
               val {name, args, start, blocks, returns, raises, ...} = dest f
               open Dot
               val graph = Graph.new ()
               val {get = nodeOptions, ...} =
                  Property.get (Node.plist, Property.initFun (fn _ => ref []))
               fun setNodeText (n: unit Node.t, l): unit =
                  List.push (nodeOptions n, NodeOption.Label l)
               fun newNode () = Graph.newNode graph
               val {destroy, get = labelNode} =
                  Property.destGet (Label.plist,
                                    Property.initFun (fn _ => newNode ()))
               val {get = edgeOptions, set = setEdgeOptions, ...} =
                  Property.getSetOnce (Edge.plist, Property.initConst [])
               fun edge (from, to, label: string, style: style): unit =
                  let
                     val e = Graph.addEdge (graph, {from = from,
                                                    to = to})
                     val _ = setEdgeOptions (e, [EdgeOption.label label,
                                                 EdgeOption.Style style])
                  in
                     ()
                  end
               val _ =
                  Vector.foreach
                  (blocks, fn Block.T {label, args, statements, transfer} =>
                   let
                      val from = labelNode label
                      val edge = fn (to, label, style) =>
                         edge (from, labelNode to, label, style)
                      val () =
                         case transfer of
                            Bug => ()
                          | Call {return, ...} =>
                               let
                                  val _ =
                                     case return of
                                        Return.Dead => ()
                                      | Return.NonTail {cont, handler} =>
                                           (edge (cont, "", Dotted)
                                            ; (Handler.foreachLabel
                                               (handler, fn l =>
                                                edge (l, "Handle", Dashed))))
                                      | Return.Tail => ()
                               in
                                  ()
                               end
                          | Case {cases, default, ...} =>
                               let
                                  fun doit (v, toString) =
                                     Vector.foreach
                                     (v, fn (x, j) =>
                                      edge (j, toString x, Solid))
                                  val _ =
                                     case cases of
                                        Cases.Con v =>
                                           doit (v, Con.toString)
                                      | Cases.Word (_, v) =>
                                           doit (v, fn w => WordX.toString (w, {suffix = true}))
                                  val _ = 
                                     case default of
                                        NONE => ()
                                      | SOME j =>
                                           edge (j, "Default", Solid)
                               in
                                  ()
                               end
                          | Goto {dst, ...} => edge (dst, "", Solid)
                          | Raise _ => ()
                          | Return _ => ()
                          | Runtime {return, ...} => edge (return, "", Dotted)
                      val lab =
                         [(toStringTransfer transfer, Left)]
                      val lab =
                         Vector.foldr
                         (statements, lab, fn (s, ac) =>
                          (toStringStatement s, Left) :: ac)
                      val lab =
                         (toStringBlockHeader (label, args), Left)::lab
                      val _ = setNodeText (from, lab)
                   in
                      ()
                   end)
               val startNode = labelNode start
               val funNode =
                  let
                     val funNode = newNode ()
                     val _ = edge (funNode, startNode, "Start", Solid)
                     val lab =
                        [(toStringTransfer (Transfer.Goto {dst = start, args = Vector.new0 ()}), Left)]
                     val lab =
                        if !Control.showTypes
                           then ((Layout.toString o Layout.seq)
                                 [Layout.str ": ",
                                  Layout.record [("returns",
                                                  Option.layout
                                                  (Vector.layout Type.layout)
                                                  returns),
                                                 ("raises",
                                                  Option.layout
                                                  (Vector.layout Type.layout)
                                                  raises)]],
                                 Left)::lab
                           else lab
                     val lab =
                        (toStringFunctionHeader (name, args), Left)::
                        lab
                     val _ = setNodeText (funNode, lab)
                  in
                     funNode
                  end
               val controlFlowGraphLayout =
                  Graph.layoutDot
                  (graph, fn {nodeName} => 
                   {title = concat [Func.toString name, " control-flow graph"],
                    options = [GraphOption.Rank (Min, [{nodeName = nodeName funNode}])],
                    edgeOptions = edgeOptions,
                    nodeOptions =
                    fn n => let
                               val l = ! (nodeOptions n)
                               open NodeOption
                            in FontColor Black :: Shape Box :: l
                            end})
               val () = Graph.removeNode (graph, funNode)
               fun dominatorTreeLayout () =
                  let
                     val {get = nodeOptions, set = setNodeOptions, ...} =
                        Property.getSetOnce (Node.plist, Property.initConst [])
                     val _ =
                        Vector.foreach
                        (blocks, fn Block.T {label, ...} =>
                         setNodeOptions (labelNode label,
                                         [NodeOption.label (Label.toString label)]))
                     val dominatorTreeLayout =
                        Tree.layoutDot
                        (Graph.dominatorTree (graph,
                                              {root = startNode,
                                               nodeValue = fn n => n}),
                         {title = concat [Func.toString name, " dominator tree"],
                          options = [],
                          nodeOptions = nodeOptions})
                  in
                     dominatorTreeLayout
                  end
               fun loopForestLayout () =
                  let
                     val {get = nodeName, set = setNodeName, ...} =
                        Property.getSetOnce (Node.plist, Property.initConst "")
                     val _ =
                        Vector.foreach
                        (blocks, fn Block.T {label, ...} =>
                         setNodeName (labelNode label, Label.toString label))
                     val loopForestLayout =
                        Graph.LoopForest.layoutDot
                        (Graph.loopForestSteensgaard (graph,
                                                      {root = startNode,
                                                       nodeValue = fn x => x}),
                         {title = concat [Func.toString name, " loop forest"],
                          options = [],
                          name = nodeName})
                  in
                     loopForestLayout
                  end
            in
               {destroy = destroy,
                controlFlowGraph = controlFlowGraphLayout,
                dominatorTree = dominatorTreeLayout,
                loopForest = loopForestLayout}
            end
      end

      fun new (dest: dest) =
         let
            val controlFlow = CPromise.delay (fn () => determineControlFlow dest)
         in
            T {controlFlow = controlFlow,
               dest = dest}
         end

      fun clear (T {controlFlow, dest, ...}) =
         let
            val {args, blocks, ...} = dest
            val _ = (Vector.foreach (args, Var.clear o #1)
                     ; Vector.foreach (blocks, Block.clear))
            val _ = CPromise.clear controlFlow
         in
            ()
         end

      fun layoutHeader (f: t): Layout.t =
         let
            val {args, name, mayInline, raises, returns, start, ...} = dest f
            open Layout
            val (sep, rty) =
               if !Control.showTypes
                  then (str ":",
                        indent (seq [record [("returns",
                                              Option.layout
                                              (Vector.layout Type.layout)
                                              returns),
                                             ("raises",
                                              Option.layout
                                              (Vector.layout Type.layout)
                                              raises)],
                                     str " ="],
                                2))
                  else (str " =", empty)
         in
            mayAlign [mayAlign [seq [str "fun ",
                                     if mayInline then empty else str "noinline ",
                                     Func.layout name,
                                     str " ",
                                     layoutFormals args,
                                     sep],
                                rty],
                      seq [Label.layout start, str " ()"]]
         end

      val parseHeader =
         let
            open Parse
         in
            kw "fun" *>
            optional (kw "noinline") >>= (fn noInline =>
            Func.parse >>= (fn name =>
            parseFormals >>= (fn args =>
            sym ":" *>
            cbrack (ffield ("returns", option (vector Type.parse)) >>= (fn returns =>
                    nfield ("raises", option (vector Type.parse)) >>= (fn raises =>
                    pure (returns, raises)))) >>= (fn (returns, raises) =>
            sym "=" *>
            Label.parse >>= (fn start =>
            paren (pure ()) *>
            pure (Option.isNone noInline, name, args, returns, raises, start))))))
         end

      fun layout' (f: t, layoutVar) =
         let
            val {blocks, ...} = dest f
            open Layout
            fun layoutBlock b = Block.layout' (b, layoutVar)
         in
            align [layoutHeader f,
                   indent (align (Vector.toListMap (blocks, layoutBlock)), 2)]
         end
      fun layout f = layout' (f, Var.layout)

      val parse =
         let
            open Parse
         in
            new <$>
            (parseHeader >>= (fn (mayInline, name, args, returns, raises, start) =>
             many Block.parse >>= (fn blocks =>
             pure {mayInline = mayInline,
                   name = name,
                   args = args,
                   returns = returns,
                   raises = raises,
                   start = start,
                   blocks = Vector.fromList blocks})))
         end

      fun layouts (f: t, layoutVar, output: Layout.t -> unit): unit =
         let
            val {blocks, name, ...} = dest f
            val _ = output (layoutHeader f)
            val _ =
               Vector.foreach
               (blocks, fn b =>
                output (Layout.indent (Block.layout' (b, layoutVar), 2)))
            val _ =
               if not (!Control.keepDot)
                  then ()
               else
                  let
                     val {destroy, controlFlowGraph, dominatorTree, loopForest} =
                        layoutDot (f, layoutVar)
                     val name = Func.toString name
                     fun doit (s, g) =
                        Control.saveToFile
                        {arg = (),
                         name = SOME (concat [name, ".", s]),
                         toFile = {display = Control.Layout (fn () => g),
                                   style = Control.Dot,
                                   suffix = "dot"},
                         verb = Control.Detail}
                     val _ = doit ("cfg", controlFlowGraph)
                        handle _ => Error.warning "SsaTree2.layouts: couldn't layout cfg"
                     val _ = doit ("dom", dominatorTree ())
                        handle _ => Error.warning "SsaTree2.layouts: couldn't layout dom"
                     val _ = doit ("lf", loopForest ())
                        handle _ => Error.warning "SsaTree2.layouts: couldn't layout lf"
                     val () = destroy ()
                  in
                     ()
                  end
         in
            ()
         end

      fun alphaRename f =
         let
            local
               fun make (new, plist) =
                  let
                     val {get, set, destroy, ...} = 
                        Property.destGetSetOnce (plist, Property.initConst NONE)
                     fun bind x =
                        let
                           val x' = new x
                           val _ = set (x, SOME x')
                        in
                           x'
                        end
                     fun lookup x =
                        case get x of
                           NONE => x
                         | SOME y => y
                  in (bind, lookup, destroy)
                  end
            in
               val (bindVar, lookupVar, destroyVar) =
                  make (Var.new, Var.plist)
               val (bindLabel, lookupLabel, destroyLabel) =
                  make (Label.new, Label.plist)
            end
            val {args, blocks, mayInline, name, raises, returns, start, ...} =
               dest f
            val args = Vector.map (args, fn (x, ty) => (bindVar x, ty))
            val bindLabel = ignore o bindLabel
            val bindVar = ignore o bindVar
            val _ = 
               Vector.foreach
               (blocks, fn Block.T {label, args, statements, ...} => 
                (bindLabel label
                 ; Vector.foreach (args, fn (x, _) => bindVar x)
                 ; Vector.foreach (statements, fn s =>
                                   Statement.foreachDef (s, bindVar o #1))))
            val blocks = 
               Vector.map
               (blocks, fn Block.T {label, args, statements, transfer} =>
                Block.T {label = lookupLabel label,
                         args = Vector.map (args, fn (x, ty) =>
                                            (lookupVar x, ty)),
                         statements = (Vector.map
                                       (statements, fn s =>
                                        Statement.replaceDefsUses
                                        (s, {def = lookupVar,
                                             use = lookupVar}))),
                         transfer = Transfer.replaceLabelVar
                                    (transfer, lookupLabel, lookupVar)})
            val start = lookupLabel start
            val _ = destroyVar ()
            val _ = destroyLabel ()
         in
            new {args = args,
                 blocks = blocks,
                 mayInline = mayInline,
                 name = name,
                 raises = raises,
                 returns = returns,
                 start = start}
         end
      (* quell unused warning *)
      val _ = alphaRename

      fun profile (f: t, sourceInfo): t =
         if !Control.profile = Control.ProfileNone
            orelse !Control.profileIL <> Control.ProfileSource
            then f
         else 
         let
            val _ = Control.diagnostic (fn () => layout f)
            val {args, blocks, mayInline, name, raises, returns, start} = dest f
            val extraBlocks = ref []
            val {get = labelBlock, set = setLabelBlock, rem} =
               Property.getSetOnce
               (Label.plist, Property.initRaise ("block", Label.layout))
            val _ =
               Vector.foreach
               (blocks, fn block as Block.T {label, ...} =>
                setLabelBlock (label, block))
            val blocks =
               Vector.map
               (blocks, fn Block.T {args, label, statements, transfer} =>
                let
                   val statements =
                      if Label.equals (label, start)
                         then (Vector.concat
                               [Vector.new1
                                (Profile (ProfileExp.Enter sourceInfo)),
                                statements])
                      else statements
                   fun leave () = Profile (ProfileExp.Leave sourceInfo)
                   fun prefix (l: Label.t,
                               statements: Statement.t vector): Label.t =
                      let
                         val Block.T {args, ...} = labelBlock l
                         val c = Label.newNoname ()
                         val xs = Vector.map (args, fn (x, _) => Var.new x)
                         val _ =
                            List.push
                            (extraBlocks,
                             Block.T
                             {args = Vector.map2 (xs, args, fn (x, (_, t)) =>
                                                  (x, t)),
                              label = c,
                              statements = statements,
                              transfer = Goto {args = xs,
                                               dst = l}})
                      in
                         c
                      end
                   fun genHandler (cont: Label.t)
                      : Statement.t vector * Label.t * Handler.t =
                      case raises of
                         NONE => (statements, cont, Handler.Caller)
                       | SOME ts => 
                            let
                               val xs = Vector.map (ts, fn _ => Var.newNoname ())
                               val l = Label.newNoname ()
                               val _ =
                                  List.push
                                  (extraBlocks,
                                   Block.T
                                   {args = Vector.zip (xs, ts),
                                    label = l,
                                    statements = Vector.new1 (leave ()),
                                    transfer = Transfer.Raise xs})
                            in
                               (statements,
                                prefix (cont, Vector.new0 ()),
                                Handler.Handle l)
                            end
                   fun addLeave () =
                      (Vector.concat [statements,
                                      Vector.new1 (leave ())],
                       transfer)
                   val (statements, transfer) =
                      case transfer of
                         Call {args, func, return} =>
                            let
                               datatype z = datatype Return.t
                            in
                               case return of
                                  Dead => (statements, transfer)
                                | NonTail {cont, handler} =>
                                     (case handler of
                                         Handler.Dead => (statements, transfer)
                                       | Handler.Caller =>
                                            let
                                               val (statements, cont, handler) =
                                                  genHandler cont
                                               val return =
                                                  Return.NonTail
                                                  {cont = cont,
                                                   handler = handler}
                                            in
                                               (statements,
                                                Call {args = args,
                                                      func = func,
                                                      return = return})
                                            end
                                       | Handler.Handle _ =>
                                            (statements, transfer))
                                | Tail => addLeave ()
                            end
                       | Raise _ => addLeave ()
                       | Return _ => addLeave ()
                       | _ => (statements, transfer)
                in
                   Block.T {args = args,
                            label = label,
                            statements = statements,
                            transfer = transfer}
                end)
            val _ = Vector.foreach (blocks, rem o Block.label)
            val blocks = Vector.concat [Vector.fromList (!extraBlocks), blocks]
            val f = 
               new {args = args,
                    blocks = blocks,
                    mayInline = mayInline,
                    name = name,
                    raises = raises,
                    returns = returns,
                    start = start}
            val _ = Control.diagnostic (fn () => layout f)
         in
            f
         end

      val profile =
         Trace.trace2 ("SsaTree2.Function.profile", layout, SourceInfo.layout, layout)
         profile
   end

structure Program =
   struct
      datatype t =
         T of {
               datatypes: Datatype.t vector,
               globals: Statement.t vector,
               functions: Function.t list,
               main: Func.t
               }
   end

structure Program =
   struct
      open Program

      local
         structure Graph = DirectedGraph
         structure Node = Graph.Node
         structure Edge = Graph.Edge
      in
         fun layoutCallGraph (T {functions, main, ...},
                              title: string): Layout.t =
            let
               open Dot
               val graph = Graph.new ()
               val {get = nodeOptions, set = setNodeOptions, ...} =
                  Property.getSetOnce
                  (Node.plist, Property.initRaise ("options", Node.layout))
               val {get = funcNode, destroy} =
                  Property.destGet
                  (Func.plist, Property.initFun
                   (fn f =>
                    let
                       val n = Graph.newNode graph
                       val _ =
                          setNodeOptions
                          (n,
                           let open NodeOption
                           in [FontColor Black, label (Func.toString f)]
                           end)
                    in
                       n
                    end))
               val {get = edgeOptions, set = setEdgeOptions, ...} =
                  Property.getSetOnce (Edge.plist, Property.initConst [])
               val _ =
                  List.foreach
                  (functions, fn f =>
                   let
                      val {name, blocks, ...} = Function.dest f
                      val from = funcNode name
                      val {get, destroy} =
                         Property.destGet
                         (Node.plist,
                          Property.initFun (fn _ => {nontail = ref false,
                                                     tail = ref false}))
                      val _ = 
                         Vector.foreach
                         (blocks, fn Block.T {transfer, ...} =>
                          case transfer of
                             Call {func, return, ...} =>
                                let
                                   val to = funcNode func
                                   val {tail, nontail} = get to
                                   datatype z = datatype Return.t
                                   val is =
                                      case return of
                                         Dead => false
                                       | NonTail _ => true
                                       | Tail => false
                                   val r = if is then nontail else tail
                                in
                                   if !r
                                      then ()
                                   else (r := true
                                         ; (setEdgeOptions
                                            (Graph.addEdge
                                             (graph, {from = from, to = to}),
                                             if is
                                                then []
                                             else [EdgeOption.Style Dotted])))
                                end
                           | _ => ())
                      val _ = destroy ()
                   in
                      ()
                   end)
               val root = funcNode main
               val l =
                  Graph.layoutDot
                  (graph, fn {nodeName} =>
                   {title = title,
                    options = [GraphOption.Rank (Min, [{nodeName = nodeName root}])],
                    edgeOptions = edgeOptions,
                    nodeOptions = nodeOptions})
               val _ = destroy ()
            in
               l
            end
      end

      fun layouts (p as T {datatypes, globals, functions, main},
                   output': Layout.t -> unit) =
         let
            val layoutVar = Statement.prettifyGlobals globals
            open Layout
            (* Layout includes an output function, so we need to rebind output
             * to the one above.
             *)
            val output = output' 
         in
            output (str "\n\n(* Datatypes: *)")
            ; Vector.foreach (datatypes, output o Datatype.layout)
            ; output (str "\n\n(* Globals: *)")
            ; Vector.foreach (globals, output o (fn s => Statement.layout' (s, layoutVar)))
            ; output (str "\n\n(* Functions: *)")
            ; List.foreach (functions, fn f =>
                            Function.layouts (f, layoutVar, output))
            ; output (seq [str "\n\n(* Main: *) ", Func.layout main])
            ; if not (!Control.keepDot)
                 then ()
              else
                 Control.saveToFile
                 {arg = (),
                  name = SOME "call-graph",
                  toFile = {display = Control.Layout (fn () => layoutCallGraph (p, !Control.inputFile)),
                            style = Control.Dot,
                            suffix = "dot"},
                  verb = Control.Detail}
         end

      val toFile = {display = Control.Layouts layouts, style = Control.ML, suffix = "ssa2"}

      fun parse () =
         let
            open Parse

            val () = Tycon.parseReset {prims = Vector.new1 Tycon.bool}
            val () = Con.parseReset {prims = Vector.new2 (Con.truee, Con.falsee)}
            val () = Var.parseReset {prims = Vector.new0 ()}
            val () = Label.parseReset {prims = Vector.new0 ()}
            val () = Func.parseReset {prims = Vector.new0 ()}

            val parseProgram =
               T <$>
               (many Datatype.parse >>= (fn datatypes =>
                many Statement.parse >>= (fn globals =>
                many Function.parse >>= (fn functions =>
                Func.parse >>= (fn main =>
                pure {datatypes = Vector.fromList datatypes,
                      globals = Vector.fromList globals,
                      functions = functions,
                      main = main})))))
         in
            parseProgram <* (mlSpaces *> (failing next <|> fail "end of file"))
         end

      fun layoutStats (program as T {datatypes, globals, functions, main, ...}) =
         let
            val (mainNumVars, mainNumBlocks) =
               case List.peek (functions, fn f =>
                               Func.equals (main, Function.name f)) of
                  NONE => Error.bug "SsaTree2.Program.layoutStats: no main"
                | SOME f =>
                     let
                        val numVars = ref 0
                        val _ = Function.foreachVar (f, fn _ => Int.inc numVars)
                        val {blocks, ...} = Function.dest f
                        val numBlocks = Vector.length blocks
                     in
                        (!numVars, numBlocks)
                     end
            val numTypes = ref 0
            val {get = countType, destroy} =
               Property.destGet
               (Type.plist,
                Property.initRec
                (fn (t, countType) =>
                 let
                    datatype z = datatype Type.dest
                    val _ =
                       case Type.dest t of
                          CPointer => ()
                        | Datatype _ => ()
                        | IntInf => ()
                        | Object {args, ...} => Prod.foreach (args, countType)
                        | Real _ => ()
                        | Thread => ()
                        | Weak t => countType t
                        | Word _ => ()
                    val _ = Int.inc numTypes
                 in
                    ()
                 end))
            val _ =
               Vector.foreach
               (datatypes, fn Datatype.T {cons, ...} =>
                Vector.foreach (cons, fn {args, ...} =>
                                Prod.foreach (args, countType)))
            val numGlobals = Vector.length globals
            val numStatements = ref numGlobals
            val numBlocks = ref 0
            val _ =
               List.foreach
               (functions, fn f =>
                let
                   val {args, blocks, ...} = Function.dest f
                   val _ = Vector.foreach (args, countType o #2)
                   val _ =
                      Vector.foreach
                      (blocks, fn Block.T {args, statements, ...} =>
                       let
                          val _ = Int.inc numBlocks
                          val _ = Vector.foreach (args, countType o #2)
                          val _ =
                             Vector.foreach
                             (statements, fn stmt =>
                              let
                                 val _ = Int.inc numStatements
                                 datatype z = datatype Statement.t
                                 val _ =
                                    case stmt of
                                       Bind {ty, ...} => countType ty
                                     | _ => ()
                              in () end)
                       in () end)
                in () end)
            val numFunctions = List.length functions
            val _ = destroy ()
            open Layout
         in
            align
            [Control.sizeMessage ("ssa2 program", program),
             seq [str "num globals = ", Int.layout (Vector.length globals)],
             seq [str "num vars in main = ", Int.layout mainNumVars],
             seq [str "num blocks in main = ", Int.layout mainNumBlocks],
             seq [str "num functions in program = ", Int.layout numFunctions],
             seq [str "num blocks in program = ", Int.layout (!numBlocks)],
             seq [str "num statements in program = ", Int.layout (!numStatements)],
             seq [str "num types in program = ", Int.layout (!numTypes)],
             Type.stats ()]
         end

      (* clear all property lists reachable from program *)
      fun clear (T {datatypes, globals, functions, ...}) =
         ((* Can't do Type.clear because it clears out the info needed for
           * Type.dest.
           *)
          Vector.foreach (datatypes, Datatype.clear)
          ; Vector.foreach (globals, Statement.clear)
          ; List.foreach (functions, fn f =>
                          (Function.clear f
                           ; Func.clear (Function.name f))))

      fun clearGlobals (T {globals, ...}) =
         Vector.foreach (globals, Statement.clear)

      fun clearTop (p as T {datatypes, functions, ...}) =
         (Vector.foreach (datatypes, Datatype.clear)
          ; List.foreach (functions, Func.clear o Function.name)
          ; clearGlobals p)

      fun foreachVar (T {globals, functions, ...}, f) =
         (Vector.foreach (globals, fn s => Statement.foreachDef (s, f))
          ; List.foreach (functions, fn g => Function.foreachVar (g, f)))

      fun foreachPrimApp (T {globals, functions, ...}, f) =
         let
            fun loopStatement (s: Statement.t) =
               case s of
                  Bind {exp = PrimApp {args, prim}, ...} =>
                     f {args = args, prim = prim}
                 | _ => ()
             fun loopTransfer t =
                case t of
                   Runtime {args, prim, ...} => f {args = args, prim = prim}
                 | _ => ()
             val _ = Vector.foreach (globals, loopStatement)
             val _ =
                List.foreach
                (functions, fn f =>
                 Vector.foreach
                 (Function.blocks f, fn Block.T {statements, transfer, ...} =>
                  (Vector.foreach (statements, loopStatement);
                   loopTransfer transfer)))
         in
            ()
         end

      fun hasPrim (p, f) =
         Exn.withEscape
         (fn escape =>
          (foreachPrimApp (p, fn {prim, ...} =>
                           if f prim then escape true else ())
           ; false))

      fun dfs (p, v) =
         let
            val T {functions, main, ...} = p
            val functions = Vector.fromList functions
            val numFunctions = Vector.length functions
            val {get = funcIndex, set = setFuncIndex, rem, ...} =
               Property.getSetOnce (Func.plist,
                                    Property.initRaise ("index", Func.layout))
            val _ = Vector.foreachi (functions, fn (i, f) =>
                                     setFuncIndex (#name (Function.dest f), i))
            val visited = Array.array (numFunctions, false)
            fun visit (f: Func.t): unit =
               let
                  val i = funcIndex f
               in
                  if Array.sub (visited, i)
                     then ()
                  else
                     let
                        val _ = Array.update (visited, i, true)
                        val f = Vector.sub (functions, i)
                        val v' = v f
                        val _ = Function.dfs 
                                (f, fn Block.T {transfer, ...} =>
                                 (Transfer.foreachFunc (transfer, visit)
                                  ; fn () => ()))
                        val _ = v' ()
                     in
                        ()
                     end
               end
            val _ = visit main
            val _ = Vector.foreach (functions, rem o Function.name)
         in
            ()
         end

   end

end
