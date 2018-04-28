(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(* Many of the algorithms in this module are based on
 * Compilers: Principles, Techniques, and Tools by Aho, Sethi, and Ullman,
 * which I will refer to in comments as the Dragon Book.
 *)
local
   fun ++ (r: int ref): int =
      let
         val n = 1 + !r
         val _ = r := n
      in n
      end

   val numChars: int = Char.maxOrd + 1

   local
      val validCharsString =
         "\n\t@abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 ()[]<>!?-&#;'/=\"$.\\"
   in    
      val validChars =
         Vector.tabulate (numChars, fn i =>
                          String.contains (validCharsString, Char.fromInt i))

      fun edgeLabel (cs: char list): string =
         let
            val chars = implode cs
            val n = String.size chars
            val numValidChars = String.size validCharsString
         in
            if n = numChars
               then "."
            else
               (if n >= Int.quot (numValidChars, 2)
                   then (* Character complement. *)
                      concat ["[^",
                              String.alphabetize
                              (String.keepAll
                               (validCharsString, fn c =>
                                not (String.contains
                                     (chars, c)))),
                              "]"]
                else if (1 = String.size chars
                         andalso not (String.contains
                                      (". ", String.sub (chars, 0))))
                        then chars
                     else concat ["[", chars, "]"])
         end
      val edgeLabel =
         Trace.trace ("Regexp.edgeLabel", List.layout Char.layout, String.layout)
         edgeLabel
   end

   structure Save =
      struct
         datatype t = T of {index: int ref}

         fun layout (T {index, ...}) =
            let
               open Layout
            in
               seq [str "Save ", Int.layout (!index)]
            end

         fun new () = T {index = ref ~1}

         fun equals (T {index = i, ...}, T {index = i', ...}) = i = i'

         fun assign (T {index, ...}, i) = index := i

         fun index (T {index, ...}) = !index

         val index = Trace.trace ("Regexp.Save.index", layout, Int.layout) index
      end

   structure Regexp =
      struct
         datatype t =
            AnchorFinish
          | AnchorStart
          | CharSet of char -> bool
          | Or of t list
          | Seq of t list
          | Save of t * Save.t
          | Star of t

         fun layout (r: t): Layout.t =
            let
               open Layout
            in
               case r of
                  AnchorFinish => str "AnchorFinish"
                | AnchorStart => str "AnchorStart"
                | CharSet f =>
                     seq [str "[",
                          str (edgeLabel (Int.foldDown
                                          (0, numChars, [], fn (i, ac) =>
                                           let
                                              val c = Char.fromInt i
                                           in
                                              if f c
                                                 then c :: ac
                                              else ac
                                           end))),
                          str "]"]
                | Or rs => seq [str "Or ", List.layout layout rs]
                | Seq rs => seq [str "Seq ", List.layout layout rs]
                | Save (r, s) => seq [str "Save ",
                                      Layout.tuple [layout r, Save.layout s]]
                | Star r => seq [str "Star ", layout r]
            end

         val toString = Layout.toString o layout
      end

   structure Stack:
      sig
         type 'a t

         val clear: 'a t -> unit
         val foreach: 'a t * ('a -> unit) -> unit
         val new: int * 'a -> 'a t
         val peekMap: 'a t * ('a ->'b option) -> 'b option
         val push: 'a t * 'a -> unit
      end =
      struct
         datatype 'a t = T of {elts: 'a array,
                               size: int ref}

         fun new (size: int, dummy: 'a): 'a t =
            T {elts = Array.new (size, dummy),
               size = ref 0}

         fun push (T {elts, size}, x) =
            let
               val n = !size
               val _ = Array.update (elts, n, x)
               val _ = size := n + 1
            in ()
            end

         fun clear (T {size, ...}) = size := 0

         fun foreach (T {elts, size, ...}, f) =
            Int.for (0, !size, fn i => f (Array.sub (elts, i)))

         fun peekMap (T {elts, size, ...}, f) =
            let
               val n = !size
               fun loop i =
                  if i = n
                     then NONE
                  else (case f (Array.sub (elts, i)) of
                           NONE => loop (i + 1)
                         | SOME z => SOME z)
            in
               loop 0
            end
      end

   (* NFA state. *)
   (* The states in an NFA are indexed from 0 to n-1, where n is the number
    * of states.
    *)
   structure State =
      struct
         type t = int

         val layout = Int.layout
      end

   structure MatchAction =
      struct
         datatype t =
            Finish of Save.t
          | Start of Save.t

         val equals =
            fn (Finish s, Finish s') => Save.equals (s, s')
             | (Start s, Start s') => Save.equals (s, s')
             | _ => false

         fun layout a =
            let
               open Layout
            in
               case a of
                  Finish s => seq [str "Finish ", Save.layout s]
                | Start s => seq [str "Start ", Save.layout s]
            end
      end

   structure Match =
      struct
         datatype t = T of {all: Substring.t,
                            matches: (Save.t * Substring.t) array}

         fun all (T {all, ...}) = all

         val startLength = #2 o Substring.base o all

         val endOf = Substring.endOf o all

         val length = Substring.length o all

         fun layout (T {all, matches}) =
            let open Layout
            in tuple [Substring.layout all,
                      Array.layout (Layout.tuple2
                                    (Save.layout, Substring.layout)) matches]
            end

         fun funs (T {matches, ...}) =
            let  
               fun peek (s: Save.t): Substring.t option =
                  Option.map (Array.peek (matches, fn (s', _) =>
                                          Save.equals (s, s')),
                              #2)
            in {exists = Option.isSome o peek,
                lookup = valOf o peek,
                peek = peek}
            end

         fun stringFuns m =
            let
               val {peek, lookup, exists} = funs m
            in
               {exists = exists,
                lookup = Substring.toString o lookup,
                peek = fn s => Option.map (peek s, Substring.toString)}
            end

         local
            fun make sel (m, s) = sel (funs m) s
         in
            val peek = make #peek
            val lookup = make #lookup
            val exists = make #exists
         end

         fun peekString (m, s) = Option.map (peek (m, s), Substring.toString)
         val lookupString = Substring.toString o lookup
      end

   structure Actions =
      struct
         datatype t = T of (int * MatchAction.t vector) list

         fun layout (T l) =
            List.layout (Layout.tuple2 (Int.layout,
                                        Vector.layout MatchAction.layout))
            l

         val empty = T []

         fun add (a as T l, i, v: MatchAction.t vector) =
                  if Vector.isEmpty v
                     then a
                  else T ((i, v) :: l)
      end

   structure NFA =
      struct
         structure State = State
         (* State i is final iff isSome (Array.sub (final, i)).
          * Characters are grouped into equivalence classes, represented by
          * integers in [0, numCharClasses).
          * The equivalence class of c is Array.sub (charClass, Char.toInt c).
          * The dimensions of next is numStates x numCharClasses.
          * The outgoing states from state i on input char c are given by
          * Array2.sub (next, i, Array.sub (charClass, Char.to Int c)).
          * seen, stack1, and stack2 are used in the two stack simulation of
          * the NFA (see fun match).  We preallocate them as part of the NFA
          * so they don't have to be allocated on each call to match.
          *)
         datatype t =
            T of {anchorStarts: (State.t * MatchAction.t vector) vector,
                  charClass: int array, (* of length numChars *)
                  final: {actions: MatchAction.t vector,
                          requireFinish: bool} option array,
                  next: (State.t * MatchAction.t vector) array Array2.t,
                  saves: Save.t vector,
                  seen: bool array,
                  stack1: (State.t * Actions.t) Stack.t,
                  stack2: (State.t * Actions.t) Stack.t,
                  start: State.t}
      end

   (* Non-deterministic Finite Automaton. *)
   structure NFA:
      sig
         structure State:
            sig
               type t = int

               val layout: t -> Layout.t
            end

         datatype t = datatype NFA.t

         val fromRegexp: Regexp.t -> t
         val layoutDot: t * string (* title *) -> Layout.t
         val match: {nfa: t,
                     short: bool,
                     string: string,
                     startPos: int} -> (int * Actions.t) option
         val numCharClasses: t -> int
         val numStates: t -> int
         val saves: t -> Save.t vector
      end =
      struct
         open NFA

         fun numStates (T {next, ...}) = Array2.nRows next
         fun numCharClasses (T {next, ...}) = Array2.nCols next
         fun saves (T {saves, ...}) = saves

         (* Simulating an NFA with two stacks and a bit vector, as in Algorithm
          * 3.4 (page 126) of the Dragon Book.
          *)
         fun match {nfa as T {anchorStarts, charClass, final,
                              next, stack1, stack2, start, ...},
                    short,
                    string = s,
                    startPos}: (int * Actions.t) option =
            let
               val numStates = numStates nfa
               val n = String.size s
               val seen = Array.array (numStates, false)
               fun loop (current, nextStates, i: int,
                         last: (int * Actions.t) option)
                  : (int * Actions.t) option =
                  let
                     val last = 
                        case (Stack.peekMap
                              (current, fn (s, a) =>
                               case Array.sub (final, s) of
                                  NONE => NONE
                                | SOME {actions, requireFinish} =>
                                     if requireFinish andalso i < n
                                        then NONE
                                     else SOME (i, Actions.add (a, i, actions)))) of
                           NONE => last
                         | s => s
                  in
                     if numStates = 0
                        orelse i = n
                        orelse (short andalso isSome last)
                        then (Stack.clear current
                              ; last)
                     else
                        let
                           val _ = Array.modify (seen, fn _ => false)
                           val c = Array.sub (charClass,
                                              Char.toInt (String.sub (s, i)))
                           val _ =
                              Stack.foreach
                              (current, fn (s, a) =>
                               Array.foreach
                               (Array2.sub (next, s, c), 
                                fn (s', v) =>
                                if Array.sub (seen, s')
                                   then ()
                                else (Array.update (seen, s', true)
                                      ; (Stack.push
                                         (nextStates,
                                          (s', Actions.add (a, i, v)))))))
                           val _ = Stack.clear current
                        in loop (nextStates, current, i + 1, last)
                        end
                  end
               val _ = Stack.push (stack1, (start, Actions.empty))
               val _ =
                  if startPos = 0
                     then (Vector.foreach
                           (anchorStarts, fn (s, v) =>
                            Stack.push
                            (stack1,
                             (s, Actions.add (Actions.empty, startPos, v)))))
                  else ()
            in
               loop (stack1, stack2, startPos, NONE)
            end

         (* This conversion from a regular expression to an NFA is based on
          * Section 3.9 (pages 134 -- 140) of the Dragon Book.
          *
          * It creates one NFA state for each CharSet (called a "position") that
          * is in the regexp.  There is also one extra state for the start state.
          * It adds edges as in rules 1 and 2 (page 138) for the followpos
          * function.
          *)
         fun fromRegexp (r: Regexp.t): t =
            let
               fun loop (r, ac as (saves, numPos)) =
                  let
                     open Regexp
                  in
                     case r of
                        AnchorFinish => (saves, numPos + 1)
                      | AnchorStart => (saves, numPos + 1)
                      | CharSet _ => (saves, numPos + 1)
                      | Or rs => List.fold (rs, ac, loop)
                      | Save (r, s) => loop (r, (s :: saves, numPos))
                      | Seq rs => List.fold (rs, ac, loop)
                      | Star r => loop (r, ac)
                  end
               val (saves, numPos) = loop (r, ([], 0))
               val saves = Vector.fromList saves
               val numStates = numPos + 1
               val start = numPos
               val posCounter = ref ~1
               val follow: MatchAction.t vector option Array2.t =
                  Array2.new (numStates, numStates, NONE)
               val posChars = Array2.tabulate (numPos, numChars, fn _ => false)
               local
                  (* Sets of positions represented as vectors of length numPos.
                   *)
                  datatype t = T of MatchAction.t vector option vector 
               in
                  type set = t
                  fun lookup (T v, s) = Vector.sub (v, s)
                  val empty: t = T (Vector.new (numPos, NONE))
                  fun addActions (T v, a) =
                     T (Vector.map
                        (v, fn opt =>
                         Option.map (opt, fn a' => Vector.concat [a, a'])))
                  fun addAction (s, a) = addActions (s, Vector.new1 a)
                  fun union (T v, T v'): t =
                     T (Vector.tabulate
                        (numPos, fn i =>
                         case (Vector.sub (v, i), Vector.sub (v', i)) of
                            (NONE, a) => a
                          | (a, NONE) => a
                          | _ => Error.bug "Regexp.NFA.fromRegexp.union"))
                  fun singleton (i: int): t =
                     T (Vector.tabulate (numPos, fn j =>
                                         if i = j
                                            then SOME (Vector.new0 ())
                                         else NONE))
                  fun foreach (T v, f) =
                     Vector.foreachi (v, fn (i, opt) =>
                                      case opt of
                                         NONE => ()
                                       | SOME a => f (i, a))
               end
               fun connect (v, v') =
                  foreach
                  (v, fn (s, a) =>
                   foreach
                   (v', fn (s', a') =>
                    Array2.update (follow, s, s',
                                   SOME (Vector.concat [a, a']))))
               val anchorFinishes = ref []
               val anchorStarts = ref []
               fun anchor r =
                  let
                     val i = ++ posCounter
                     val _ = List.push (r, i)
                     val first = singleton i
                  in
                     {first = first,
                      last = first,
                      nullable = NONE}
                  end
               (* The following loop fills in follow and posChars.
                * first set of positions that
                * nullable is SOME v iff the regexp is nullable, where v is the
                * sequence of actions to perform if the expression is null.
                *)
               fun loop (r: Regexp.t): {first: set,
                                        last: set,
                                        nullable: MatchAction.t vector option} =
                  case r of
                     Regexp.AnchorFinish => anchor anchorFinishes
                   | Regexp.AnchorStart => anchor anchorStarts
                   | Regexp.CharSet f =>
                        let
                           val i = ++ posCounter
                           val _ =
                              Int.for
                              (0, numChars, fn c =>
                               if f (Char.chr c)
                                  then Array2.update (posChars, i, c, true)
                               else ())
                           val first = singleton i
                        in {first = first,
                            last = first,
                            nullable = NONE}
                        end
                   | Regexp.Or rs =>
                        List.fold
                        (rs, {first = empty,
                              last = empty,
                              nullable = NONE},
                         fn (r, {first = f, last = l, nullable = n}) =>
                         let
                            val {first = f', last = l', nullable = n'} =
                               loop r
                         in
                            {first = union (f, f'),
                             last = union (l, l'),
                             nullable = if isSome n then n else n'}
                         end)
                   | Regexp.Save (r, s) =>
                        let
                           val {first = f, last = l, nullable = n} = loop r
                           val start = MatchAction.Start s
                           val finish = MatchAction.Finish s
                        in
                           {first = addAction (f, start),
                            last = addAction (l, finish),
                            nullable = Option.map (n, fn v =>
                                                   Vector.concat
                                                   [Vector.new1 start,
                                                    v,
                                                    Vector.new1 finish])}
                        end
                   | Regexp.Seq rs =>
                        List.fold
                        (rs, {first = empty,
                              last = empty,
                              nullable = SOME (Vector.new0 ())},
                         fn (r, {first = f, last = l, nullable = n}) =>
                         let
                            val {first = f', last = l', nullable = n'} =
                               loop r
                            val _ = connect (l, f')
                            val first =
                               case n of
                                  NONE => f
                                | SOME v => union (f, addActions (f', v))
                            val last =
                               case n' of
                                  NONE => l'
                                | SOME v => union (l', addActions (l, v))
                         in
                            {first = first, 
                             last = last,
                             nullable = (case (n, n') of
                                            (SOME v, SOME v') =>
                                               SOME (Vector.concat [v, v'])
                                          | _ => NONE)}
                         end)
                   | Regexp.Star r =>
                        let
                           val {first = f, last = l, ...} = loop r
                           val _ = connect (l, f)
                        in
                           {first = f, last = l,
                            nullable = SOME (Vector.new0 ())}
                        end
               val {first, last, nullable} = loop r
               local
                  fun extract (anchors, positions) =
                     Vector.keepAllMap
                     (Vector.fromListMap
                      (!anchors, fn s =>
                       Option.map (lookup (positions, s), fn v => (s, v))),
                      fn x => x)
               in
                  (* Any anchor starts in first should be anchor starts. *)
                  val anchorStarts = extract (anchorStarts, first)
                  (* Any anchor finishes in last should be anchor finishes *)
                  val anchorFinishes = extract (anchorFinishes, last)
               end
               (* The positions in first are reachable from the start state. *)
               val _ = foreach (first, fn (i, a) =>
                                Array2.update (follow, start, i, SOME a))
               val final = Array.array (numStates, NONE)
               (* The positions that are followed by an anchorFinish are final,
                * with requireFinish = true.
                *)
               val _ =
                  Vector.foreach
                  (anchorFinishes, fn (j, _) =>
                   Int.for
                   (0, numStates, fn i =>
                    case Array2.sub (follow, i, j) of
                       NONE => ()
                     | SOME a => 
                          Array.update (final, i, SOME {actions = a,
                                                        requireFinish = true})))
               (* The positions in last are all final. *)
               val _ =
                  foreach (last, fn (i, a) =>
                           Array.update (final, i, SOME {actions = a,
                                                         requireFinish = false}))
               (* The start state is final iff the whole regexp is nullable. *)
               val _ =
                  case nullable of
                     NONE => ()
                   | SOME v =>
                        Array.update (final, start,
                                      SOME {actions = v,
                                            requireFinish = false})
               (* Compute the transition table, "next". *)
               val tmp: MatchAction.t vector option Array.t =
                  Array.new (numStates, NONE)
               val next =
                  Array2.tabulate
                  (numStates, numChars, fn (i, c) =>
                   let
                      val _ =
                         Int.for
                         (0, numPos, fn j =>
                          case Array2.sub (follow, i, j) of
                             NONE => ()
                           | SOME a => 
                                if Array2.sub (posChars, j, c)
                                   then Array.update (tmp, j, SOME a)
                                else ())
                      val res =
                         Array.keepAllMapi (tmp, fn (i, opt) =>
                                            Option.map (opt, fn v => (i, v)))
                      val _ = Int.for (0, numStates, fn j =>
                                       Array.update (tmp, j, NONE))
                   in
                      res
                   end)
               (* Two characters are equivalent if all states treat them the
                * same.
                *)
               fun charEquiv (c: int, c': int) =
                  Int.forall
                  (0, numStates, fn i =>
                   Array.equals
                   (Array2.sub (next, i, c),
                    Array2.sub (next, i, c'),
                    fn ((j, v), (j', v')) =>
                    j = j' andalso Vector.equals (v, v', MatchAction.equals)))
               (* Compute charClass. *)
               val repCounter = ref ~1
               val reps = ref [] (* representative of each char class *)
               val charClass = Array.new (numChars, ~1)
               val _ = 
                  Int.for (0, numChars, fn c =>
                           let
                              val rep = 
                                 case List.peek (!reps, fn {char, ...} =>
                                                 charEquiv (c, char)) of
                                    NONE =>
                                       let
                                          val rep = ++ repCounter
                                       in List.push (reps, {char = c, rep = rep})
                                          ; rep
                                       end
                                  | SOME {rep, ...} => rep
                           in Array.update (charClass, c, rep)
                           end)
               val numClasses = 1 + !repCounter
               (* Compute "next" for the charClasses. *)
               val next' =
                  Array2.new (numStates, numClasses, Array.fromList [])
               val _ =
                  List.foreach
                  (!reps, fn {char, rep} =>
                   Int.for (0, numStates, fn state =>
                            Array2.update (next', state, rep,
                                           Array2.sub (next, state, char))))
            in
               T {anchorStarts = anchorStarts,
                  charClass = charClass,
                  final = final,
                  next = next',
                  saves = saves,
                  seen = Array.new (numStates, false),
                  stack1 = Stack.new (numStates, (~1, Actions.empty)),
                  stack2 = Stack.new (numStates, (~1, Actions.empty)),
                  start = start}
            end

         structure Graph = DirectedGraph
         fun layoutDot (T {anchorStarts, charClass, final, next, start, ...},
                        title: string): Layout.t =
            let
               val numStates = Array2.nRows next
               open Dot
               val g = Graph.new ()
               val nodes = Vector.tabulate (numStates, fn _ => Graph.newNode g)
               fun node i = Vector.sub (nodes, i)
               val {get = nodeOptions, ...} =
                  Property.get (Graph.Node.plist,
                                Property.initFun
                                (fn _ => let open NodeOption
                                         in ref []
                                         end))
               val {get = edgeOptions, ...} =
                  Property.get (Graph.Edge.plist,
                                Property.initFun
                                (fn _ => let open EdgeOption
                                         in ref []
                                         end))
               fun addNodeOption (i, opts) =
                  let val r = nodeOptions (node i)
                  in r := opts @ !r
                  end
               val _ = addNodeOption (start, [NodeOption.label "start"])
               val _ =
                  Int.for
                  (0, numStates, fn src =>
                   let
                      val shape = 
                         case (isSome (Array.sub (final, src)),
                               Vector.exists (anchorStarts, fn (s, _) =>
                                              s = src)) of
                            (false, false) => Ellipse
                          | (true, false) => Box
                          | (false, true) => Diamond
                          | (true, true) => Polygon {sides = 5, options = []}
                      val _ =
                         addNodeOption (src, let open NodeOption
                                             in [Shape shape]
                                             end)
                      val dsts = Array.new (numStates, [])
                      val _ = 
                         Int.forDown
                         (0, numChars, fn c =>
                          if Vector.sub (validChars, c)
                             then
                                let
                                   val char = Char.fromInt c
                                   val class = Array.sub (charClass, c)
                                in Array.foreach
                                   (Array2.sub (next, src, class), fn (dst, _) =>
                                    (Array.update (dsts, dst,
                                                   char :: Array.sub (dsts, dst))))
                                end
                          else ())
                   in
                      Array.foreachi
                      (dsts, fn (dst, cs) =>
                       case cs of
                          [] => ()
                        | _ =>
                             let
                                val edge = Graph.addEdge (g, {from = node src,
                                                              to = node dst})
                             in List.push (edgeOptions edge,
                                           EdgeOption.label (edgeLabel cs))
                             end)
                   end)
            in
               Graph.layoutDot (g, fn {nodeName} => 
                                {title = title,
                                 options = 
                                 let open GraphOption
                                 in [
                                     RankDir LeftToRight,
                                     Rank (Min, [{nodeName = nodeName (node start)}])
                                     ]
                                 end,
                                 edgeOptions = ! o edgeOptions,
                                 nodeOptions = ! o nodeOptions})
            end
      end

   structure DFA:
      sig
         type t

         val fromNFA: NFA.t -> t
         val layoutDot: {dfa: t,
                         showDead: bool,
                         title: string} -> Layout.t
         val match: {dfa: t,
                     short: bool,
                     string: string,
                     startPos: int,
                     anchorStart: bool} -> (int * Actions.t) option
         val minimize: t -> t
         val saves: t -> Save.t vector
      end =
      struct
         (* The states in a DFA are indexed from 0 to n-1, where n is the number
          * of states.
          *)
         structure State =
            struct
               type t = int

               val layout = Int.layout
            end

         type slot = int

         structure EdgeAction =
            struct
               datatype t =
                  Add of {from: slot,
                          to: slot,
                          actions: MatchAction.t vector}
                | Init of {to: slot,
                           actions: MatchAction.t vector}

               val equals =
                  fn (Add {from = f, to = t, actions = a},
                      Add {from = f', to = t', actions = a'}) =>
                     f = f' andalso t = t'
                     andalso Vector.equals (a, a', MatchAction.equals)
                   | (Init {to = t, actions = a},
                      Init {to = t', actions = a'}) =>
                     t = t' andalso Vector.equals (a, a', MatchAction.equals)
                   | _ => false

               val toString =
                  fn Add {from, to, actions} =>
                        concat ["(",
                                Int.toString from, ", ",
                                Int.toString to, ", ",
                                Layout.toString
                                (Vector.layout MatchAction.layout actions),
                                ")"]
                   | Init {to, actions} =>
                        concat ["(",
                                Int.toString to, ", ",
                                Layout.toString
                                (Vector.layout MatchAction.layout actions),
                                ")"]

               val layout =
                  let open Layout
                  in
                     fn Add {from, to, actions} =>
                           Layout.record
                           [("from", Int.layout from),
                            ("to", Int.layout to),
                            ("actions",
                             Vector.layout MatchAction.layout actions)]
                      | Init {actions, to} =>
                           Layout.record
                           [("to", Int.layout to),
                            ("actions",
                             Vector.layout MatchAction.layout actions)]
                  end
            end

         (* State i is final iff Array.sub (final, i).
          * Characters are grouped into equivalence classes, represented by
          * integers in [0, numCharClasses).
          * The equivalence class of c is Array.sub (charClass, Char.toInt c).
          * The dimensions of next are numStates x numCharClasses
          * The outgoing state from state i on input char c is
          *   Array2.sub (next, i, Array.sub (charClass, Char.toInt c)).
          * actions1 and actions2 are used only during matching.  They
          * represent the actions associated with each NFA state.  They are of
          * the same length as the number of states in the NFA.
          *)
         datatype t =
            T of {anchorStart: State.t,
                  anchorStartStack: MatchAction.t vector vector,
                  charClass: int array, (* of length numChars *)
                  dead: bool array,
                  final: {actions: MatchAction.t vector,
                          requireFinish: bool,
                          slot: int} option array,
                  next: (State.t * EdgeAction.t vector) Array2.t,
                  saves: Save.t vector,
                  stack1: Actions.t array, (* of size maxNumNFAStates *)
                  stack2: Actions.t array, (* of size maxNumNFAStates *)
                  start: State.t,
                  startStack: MatchAction.t vector vector}

         fun numStates (T {next, ...}): int = Array2.nRows next
         fun saves (T {saves, ...}) = saves

         fun dead (numStates, numCharClasses, final, next) =
            Array.tabulate
            (numStates, fn i =>
             not (isSome (Array.sub (final, i)))
             andalso Int.forall (0, numCharClasses, fn c =>
                                 let val (j, v) = Array2.sub (next, i, c)
                                 in i = j andalso Vector.isEmpty v
                                 end))

         (* To build a DFA from an NFA, I use the usual "subset construction",
          * as in algorithm 3.2 (page 118) of the Dragon Book.
          *
          * It associates each (reachable) set of states in the NFA with a single
          * state in the DFA.
          *)
         fun fromNFA (nfa as NFA.T {anchorStarts, charClass,
                                    final, next, saves, start, ...}) =
            let
               val numNFAStates = NFA.numStates nfa
               val numCharClasses = NFA.numCharClasses nfa
               (* Determine the NFA states that have save info.
                *)
               val nfaStateSave = Array.array (numNFAStates, false)
               fun visit (s: NFA.State.t): unit =
                  if Array.sub (nfaStateSave, s)
                     then ()
                  else (Array.update (nfaStateSave, s, true)
                        ; Int.for (0, numCharClasses, fn c =>
                                   Array.foreach
                                   (Array2.sub (next, s, c), fn (s', _) =>
                                    visit s')))
               val _ =
                  Vector.foreach
                  (anchorStarts, fn (s, v) =>
                   if Vector.isEmpty v
                      then ()
                   else visit s)
               val _ =
                  Int.for (0, numNFAStates, fn s =>
                           if Array.sub (nfaStateSave, s)
                              then ()
                           else
                              Int.for (0, numCharClasses, fn c =>
                                       Array.foreach
                                       (Array2.sub (next, s, c), fn (s', v) =>
                                        if Vector.isEmpty v
                                           then ()
                                        else visit s')))
               (* Sets of states are represented as arrays, sorted in increasing
                * order of state index.
                *)
               type states = NFA.State.t array
               val counter = ref ~1
               type work =
                  {states: states,
                   state: int,
                   out: (State.t * EdgeAction.t vector) vector option ref}
               val cache: work list ref = ref []
               val todo: work list ref = ref []
               val maxNumStates: int ref = ref 0
               fun statesToState (ss: states): State.t =
                  let
                     val n = Array.length ss
                     val _ = if n > !maxNumStates
                                then maxNumStates := n
                             else ()
                  in
                     case List.peek (!cache, fn {states, ...} =>
                                     Array.equals (ss, states, op =)) of
                        NONE =>
                           let
                              val state = ++ counter
                              val work = {out = ref NONE,
                                          state = state,
                                          states = ss}
                              val _ = List.push (cache, work)
                              val _ = List.push (todo, work)
                           in
                              state
                           end
                      | SOME {state, ...} => state
                  end
               val statesToState =
                  Trace.trace ("Regexp.DFA.fromNFA.statesToState", 
                               Array.layout NFA.State.layout,
                               State.layout)
                  statesToState
               local
                  val seen = Array.array (NFA.numStates nfa, NONE)
               in
                  fun computeOut states =
                     Vector.tabulate
                     (numCharClasses, fn c =>
                      let
                         val _ = Array.modify (seen, fn _ => NONE)
                         val _ = 
                            Array.foreachi
                            (states, fn (fromSlot: slot,
                                         fromState: NFA.State.t) =>
                             Array.foreach
                             (Array2.sub (next, fromState, c),
                              fn (toState: NFA.State.t, v) =>
                              case Array.sub (seen, toState) of
                                 NONE =>
                                    Array.update
                                    (seen, toState,
                                     SOME {fromSlot = fromSlot,
                                           fromState = fromState,
                                           toState = toState,
                                           actions = v})
                               | SOME _ => ()))
                         val toStates = Array.keepAllMap (seen, fn opt => opt)
                         val edgeActions = ref []
                         val toStates = 
                            Array.mapi
                            (toStates, fn (toSlot: slot,
                                           {fromSlot, fromState, toState,
                                            actions}) =>
                             (if Array.sub (nfaStateSave, toState)
                                 then
                                    List.push
                                    (edgeActions,
                                     if Array.sub (nfaStateSave, fromState)
                                        then
                                           EdgeAction.Add
                                           {from = fromSlot,
                                            to = toSlot,
                                            actions = actions}
                                     else (EdgeAction.Init
                                           {to = toSlot,
                                            actions = actions}))
                              else ()
                              ; toState))
                      in (statesToState toStates,
                          Vector.fromList (!edgeActions))
                      end)
               end
               fun loop () =
                  case !todo of
                     [] => ()
                   | {states, out, ...} :: rest =>
                        (todo := rest
                         ; out := SOME (computeOut states)
                         ; loop ())
               (* These calls to statesToState initialize the worklist. *)
               val start' = statesToState (Array.fromList [start])
               val startStack = Vector.new1 (Vector.new0 ())
               val anchorStartStates =
                  Array.fromList
                  (List.insert
                   (Vector.toListMap (anchorStarts, #1), start, op <=))
               val anchorStart' = statesToState anchorStartStates
               val anchorStartStack =
                  Vector.tabulate
                  (Array.length anchorStartStates,
                   fn i =>
                   let
                      val s = Array.sub (anchorStartStates, i)
                   in
                      case Vector.peek (anchorStarts, fn (s', _) => s = s') of
                         NONE => Vector.new0 ()
                       | SOME (_, v) => v
                   end)
               val _ = loop ()
               (* The worklist is empty.  Compute the transition table. *)
               val numStates = 1 + !counter
               val next' = Array2.new (numStates, numCharClasses,
                                       (~1, Vector.new0 ()))
               val final' = Array.new (numStates, NONE)
               val _ =
                  List.foreach
                  (!cache, fn {states, state = i, out, ...}: work =>
                   let
                      val _ =
                         Vector.foreachi
                         (valOf (! out), fn (c, j) =>
                          Array2.update (next', i, c, j))
                      val _ =
                         case Array.sub (final', i) of
                            SOME {requireFinish = false, ...} => ()
                          | _ =>
                               case Array.peekMapi (states, fn s =>
                                                    Array.sub (final, s)) of
                                  NONE => ()
                                | SOME (slot, {actions, requireFinish}) =>
                                     Array.update
                                     (final', i,
                                      SOME {actions = actions,
                                            requireFinish = requireFinish,
                                            slot = slot})
                   in
                      ()
                   end)
               fun newStack () = Array.new (!maxNumStates, Actions.empty)
            in T {anchorStart = anchorStart',
                  anchorStartStack = anchorStartStack,
                  charClass = charClass,
                  dead = dead (numStates, numCharClasses, final', next'),
                  final = final',
                  next = next',
                  saves = saves,
                  stack1 = newStack (),
                  stack2 = newStack (),
                  start = start',
                  startStack = startStack}
            end

         (*
          * match could be sped up some by doing the match in two passes.
          * The first pass just determines if the match will succeed.
          * The second pass computes all the edge actions.
          *)
         fun match {dfa = T {anchorStart = ancSt, anchorStartStack,
                              charClass, dead, final, next, stack1, stack2,
                              start, startStack, ...},
                    short: bool,
                    string = s,
                    startPos: int,
                    anchorStart: bool}: (int * Actions.t) option =
            let
               val n = String.size s
               fun loop (i: int,
                         state: int,
                         stack1, stack2,
                         last: (int * Actions.t) option)
                  : (int * Actions.t) option =
                  let
                     val last =
                        case Array.sub (final, state) of
                           NONE => last
                         | SOME {actions, requireFinish, slot} =>
                              if requireFinish andalso i < n
                                 then NONE
                              else
                                 SOME (i, Actions.add (Array.sub (stack1, slot),
                                                       i, actions))
                  in
                     if Array.sub (dead, state)
                        orelse i = n
                        orelse (short andalso isSome last)
                        then last
                     else
                        let
                           val (state, edgeActions) =
                              Array2.sub (next, state,
                                          Array.sub
                                          (charClass,
                                           Char.toInt (String.sub (s, i))))
                           val _ =
                              Vector.foreach
                              (edgeActions,
                               fn EdgeAction.Add {from, to, actions} =>
                                     Array.update
                                     (stack2, to,
                                      Actions.add (Array.sub (stack1, from),
                                                   i, actions))
                                | EdgeAction.Init {to, actions} =>
                                     Array.update
                                     (stack2, to,
                                      Actions.add (Actions.empty, i, actions)))
                        in
                           loop (i + 1, state, stack2, stack1, last)
                        end
                  end
               val (state, initStack) =
                  if anchorStart
                     then (ancSt, anchorStartStack)
                  else (start, startStack)
               val _ = 
                  Vector.foreachi
                  (initStack, fn (slot, v) =>
                   Array.update (stack1, slot,
                                 Actions.add (Actions.empty, startPos, v)))
               val res = loop (startPos, state, stack1, stack2, NONE)
            in
               res
            end

         val match =
            Trace.trace ("Regexp.DFA.match",
                         fn {string, startPos, ...} =>
                         Layout.tuple [String.layout string,
                                       Int.layout startPos],
                         Option.layout (Layout.tuple2
                                        (Int.layout, Actions.layout)))
            match

         structure Graph = DirectedGraph
         structure Env = Env (structure Domain = MonoVector (EdgeAction))
         fun layoutDot {dfa as T {anchorStart, charClass, dead, final,
                                  next, start, ...},
                        title: string,
                        showDead: bool}: Layout.t =
            let
               val numStates = numStates dfa
               open Dot
               val g = Graph.new ()
               val nodes = Vector.tabulate (numStates, fn _ => Graph.newNode g)
               fun node i = Vector.sub (nodes, i)
               val {get = nodeOptions, ...} =
                  Property.get (Graph.Node.plist,
                                Property.initFun
                                (fn _ => let open NodeOption
                                         in ref []
                                         end))
               val {get = edgeOptions, ...} =
                  Property.get (Graph.Edge.plist,
                                Property.initFun
                                (fn _ => let open EdgeOption
                                         in ref []
                                         end))
               fun addNodeOption (i, opts) =
                  let val r = nodeOptions (node i)
                  in r := opts @ !r
                  end
               val _ = addNodeOption (start, [NodeOption.label "start"])
               val _ =
                  Int.for
                  (0, numStates, fn src =>
                   let
                      val shape = 
                         case (isSome (Array.sub (final, src)),
                               src = anchorStart) of
                            (false, false) => Ellipse
                          | (true, false) => Box
                          | (false, true) => Diamond
                          | (true, true) => Polygon {sides = 5, options = []}
                      val _ =
                         addNodeOption (src, let open NodeOption
                                             in [Shape shape]
                                             end)
                      val dsts = Array.new (numStates, Env.empty ())
                      val _ = 
                         Int.forDown
                         (0, numChars, fn c =>
                          if Vector.sub (validChars, c)
                             then
                                let
                                   val (dst, v) =
                                      Array2.sub (next, src,
                                                  Array.sub (charClass, c))
                                   val e = Array.sub (dsts, dst)
                                   val c = Char.fromInt c
                                   val cs =
                                      case Env.peek (e, v) of
                                         NONE => [c]
                                       | SOME cs => c :: cs
                                in Array.update
                                   (dsts, dst, Env.extend (e, v, cs))
                                end
                          else ())
                      val src = node src
                   in
                      Array.foreachi
                      (dsts, fn (dst, e) =>
                       if not showDead andalso Array.sub (dead, dst)
                          then ()
                       else
                          Env.foreachi
                          (e, fn (v, cs) =>
                           let
                              val edge = Graph.addEdge (g, {from = src,
                                                            to = node dst})
                              val label =
                                 concat [edgeLabel cs,
                                         " -- ",
                                         Layout.toString 
                                         (Vector.layout (Layout.str o
                                                         EdgeAction.toString)
                                          v)]
                           in List.push (edgeOptions edge,
                                         EdgeOption.label label)
                           end))
                   end)
            in
               Graph.layoutDot (g, fn {nodeName} => 
                                {title = title,
                                 options = 
                                 let open GraphOption
                                 in [
                                     RankDir LeftToRight,
                                     Rank (Min, [{nodeName = nodeName (node start)}])
                                     ]
                                 end,
                                 edgeOptions = ! o edgeOptions,
                                 nodeOptions = ! o nodeOptions})
            end

         fun minimize d = d
         (* This DFA minimization algorithm is based on algorithm 3.6 (page 142)
          * of the Dragon Book.
          *
          * It maintains an array, r, that stores for each state s the
          * representative of the class to which s belongs.
          * It repeatedly refines an equivalence relation, represented by a list
          * of classes, where each class is a list of states.
          *)
(*       fun minimize (dfa as T {anchorStart, charClass, final,
 *                               start, next, ...}): t =
 *          let
 *             val numStates = numStates dfa
 *             val numCharClasses = numCharClasses dfa
 *             type class = int list
 *             type classes = class list
 *             val repCounter = ref ~1
 *             val change = ref false
 *             fun newRep () = (change := true; ++ repCounter)
 *             val finRep = newRep ()
 *             val nonfinRep = newRep ()
 *             val r = Array.tabulate (numStates, fn i =>
 *                                     if Array.sub (final, i)
 *                                        then finRep
 *                                     else nonfinRep)
 *             fun rep s = Array.sub (r, s)
 *             fun trans (s, c) = rep (Array2.sub (next, s, c))
 *             fun refine (class: class, ac: classes): classes =
 *                let
 *                   val r =
 *                      List.fold
 *                      (class, [], fn (state, classes) =>
 *                       let
 *                          fun loop (classes, ac) =
 *                             case classes of
 *                                [] =>
 *                                   (case ac of
 *                                       [] => [{class = [state],
 *                                               old = state}]
 *                                     | _ => 
 *                                          let
 *                                             val s = newRep ()
 *                                             val _ = Array.update (r, state, s)
 *                                          in {class = [state],
 *                                              old = state} :: ac
 *                                          end)
 *                              | (z as {class, old}) :: classes =>
 *                                   if Int.forall
 *                                      (0, numCharClasses, fn c =>
 *                                       trans (old, c) = trans (state, c))
 *                                      then
 *                                         (Array.update (r, state, rep old)
 *                                          ; {class = state :: class,
 *                                             old = old} :: (List.appendRev
 *                                                            (classes, ac)))
 *                                   else loop (classes, z :: ac)
 *                       in loop (classes, [])
 *                       end)
 *                in List.fold (r, ac, fn ({class, ...}, ac) =>
 *                              case class of
 *                                 [_] => ac
 *                               | _ => class :: ac)
 *                end
 *             val refine =
 *                Trace.trace ("refine",
 *                             (List.layout Int.layout o #1),
 *                             Layout.ignore)
 *                refine
 *             fun refineAll (classes: classes): unit =
 *                case classes of
 *                   [] => ()
 *                 | _ =>
 *                      let
 *                         val _ = change := false
 *                         val classes =
 *                            List.fold (classes, [], fn (class, ac) =>
 *                                       case class of
 *                                          [_] => ac
 *                                        | _ => refine (class, ac))
 *                      in if !change
 *                            then refineAll classes
 *                         else ()
 *                      end
 *             val (fin, nonfin) =
 *                Int.fold (0, numStates, ([], []), fn (i, (f, n)) =>
 *                          if Array.sub (final, i)
 *                             then (i :: f, n)
 *                          else (f, i :: n))
 *             val _ = refineAll [fin, nonfin]
 *             val numStates' = 1 + !repCounter
 *             (* Compute reachable states. *)
 *             val reached = Array.new (numStates', false)
 *             fun visit (s: int (* an old state *)): unit =
 *                let
 *                   val s' = rep s
 *                in
 *                   if Array.sub (reached, s')
 *                      then ()
 *                   else (Array.update (reached, s', true)
 *                         ; Int.for (0, numCharClasses, fn c =>
 *                                    visit (Array2.sub (next, s, c))))
 *                end
 *             val _ = visit start
 *             val _ = visit anchorStart
 *             (* Compute new representatives. *)
 *             val c = ref ~1
 *             val newR = Array.tabulate (numStates', fn s =>
 *                                        if Array.sub (reached, s)
 *                                           then ++ c
 *                                        else ~1)
 *             val numStates' = 1 + !c
 *             val _ = Array.modify (r, fn s => Array.sub (newR, s))
 *             val next' = Array2.new (numStates', numCharClasses, ~1)
 *             val _ =
 *                Array2.foreachi
 *                (next, fn (s, c, s') =>
 *                 Array2.update (next', rep s, c, rep s'))
 *             val final' = Array.array (numStates', false)
 *             val _ =
 *                Array.foreachi
 *                (final, fn (i, b) =>
 *                 if b then Array.update (final', rep i, true) else ())
 *          in T {anchorStart = rep anchorStart,
 *                charClass = charClass,
 *                dead = dead (numStates', numCharClasses, final', next'),
 *                final = final',
 *                start = rep start,
 *                next = next'}
 *          end
 *)
      end
in
   structure Regexp: REGEXP =
      struct
         structure Save = Save
         structure Match = Match

         open Regexp

         val anchorFinish = AnchorFinish
         val anchorStart = AnchorStart
         val isChar = CharSet
         fun isNotChar f = isChar (not o f)
         fun char c = isChar (fn c' => c = c')
         fun notChar c = isChar (fn c' => c <> c')
         val or = Or
         val save = Save
         val seq = Seq
         val star = Star
         val zeroOrMore = star

         val dquote = char #"\""

         val any = isChar (fn _ => true)
         val anys = star any
         val ascii = isChar (fn c => ord c <= 127)
         val asciis = star ascii

         val none = isChar (fn _ => false)
         fun oneOf s = isChar (fn c => String.contains (s, c))
         fun notOneOf s = isNotChar (fn c => String.contains (s, c))
         val digit = isChar Char.isDigit
         val digits = star digit
         val nonDigit = isNotChar Char.isDigit
         val space = isChar Char.isSpace
         val spaces = star space

         fun string (s: string): t =
            seq (Int.foldDown (0, String.size s, [], fn (i, ac) =>
                               char (String.sub (s, i)) :: ac))

         fun stringIgnoreCase (s: string): t =
            seq (Int.foldDown
                 (0, String.size s, [], fn (i, ac) =>
                  let
                     val c = Char.toLower (String.sub (s, i))
                  in
                     isChar (fn c' => c = Char.toLower c')
                  end :: ac))

         val null = seq [] (* Language containing the empty string only. *)
         fun oneOrMore r = seq [r, star r]
         fun optional r = or [null, r]
         fun repeat (r, n: int) = seq (List.tabulate (n, fn _ => r))
         fun lower (r, n: int) = seq [repeat (r, n), star r]
         fun upper (r, n: int) =
            if n <= 0
               then null
            else or [null, seq [r, upper (r, n - 1)]]
         fun range (r, n: int, m: int) =
            seq [repeat (r, n), upper (r, m - n)]

         structure Compiled =
            struct
               datatype machine =
                  DFA of DFA.t
                | NFA of NFA.t

               datatype t = T of {regexp: Regexp.t,
                                  machine: machine}

               fun layoutDot (T {machine, ...}) =
                  case machine of
                     DFA m => DFA.layoutDot {dfa = m, showDead = false,
                                             title = "dfa"}
                   | NFA m => NFA.layoutDot (m, "nfa")

               fun layoutDotToFile (c: t, f: File.t) =
                  File.withOut (f, fn out => Layout.output (layoutDot c, out))

               fun layout (T {regexp, ...}) =
                  let
                     open Layout
                  in
                     Regexp.layout regexp
(*
                     align [case machine of
                               DFA dfa => DFA.layout dfa
                             | NFA nfa => NFA.layout nfa
                            (* str "implementing", Regexp.layout regexp *)
                                  ]
*)
                  end

               fun match {compiled = T {machine, ...},
                          short, startPos, string} =
                  let
                     val anchorStart = startPos = 0
                     val (saves, opt) =
                        case machine of
                           DFA dfa =>
                              (DFA.saves dfa,
                               DFA.match {anchorStart = anchorStart,
                                          dfa = dfa,
                                          short = short,
                                          string = string,
                                          startPos = startPos})

                         | NFA nfa =>
                              (NFA.saves nfa,
                               NFA.match {nfa = nfa,
                                          short = short,
                                          string = string,
                                          startPos = startPos})
                     exception No
                  in
                     Option.map
                     (opt, fn (stop, Actions.T actions) =>
                      let
                         val _ = Vector.foreachi (saves, fn (i, s) =>
                                                  Save.assign (s, i))
                         val n = Vector.length saves
                         val starts = Array.array (n, ~1)
                         val matches = Array.array (n, NONE)
                         val _ =
                            List.foreach
                            (rev actions, fn (i, v) =>
                             Vector.foreach
                             (v, fn ma =>
                              case ma of
                                 MatchAction.Finish s =>
                                    let
                                       val index = Save.index s
                                       val start = Array.sub (starts, index)
                                    in
                                       Array.update
                                       (matches, index,
                                        SOME (Substring.substring
                                              (string, {start = start,
                                                        length = i - start})))
                                    end
                               | MatchAction.Start s =>
                                    Array.update (starts, Save.index s, i)))
                         val matches =
                            Array.keepAllMapi
                            (matches, fn (i, sso) =>
                             case sso of
                                NONE => NONE
                              | SOME ss => SOME (Vector.sub (saves, i), ss))
                         val all =
                            Substring.substring
                            (string, {start = startPos,
                                      length = stop - startPos})
                      in
                         Match.T {all = all,
                                  matches = matches}
                      end) handle No => NONE
                  end

               val match =
                  Trace.trace
                  ("Regexp.Compiled.match",
                   fn {compiled, short, startPos, string} =>
                   Layout.record
                   [("short", Bool.layout short),
                    ("startPos", Int.layout startPos),
                    ("string", String.layout string),
                    ("compiled", layout compiled)],
                   Option.layout Match.layout)
                  match

               fun matchLong (c, s, i) =
                  match {compiled = c,
                         short = false,
                         startPos = i,
                         string = s}

               fun matchShort (c, s, i) =
                  match {compiled = c,
                         short = true,
                         startPos = i,
                         string = s}

               fun matchAll (r, s) =
                  case matchLong (r, s, 0) of
                     NONE => NONE
                   | SOME m => if String.size s = Substring.length (Match.all m)
                                  then SOME m
                               else NONE

               val matchesAll = isSome o matchAll

               fun matchPrefix (r, s) = matchShort (r, s, 0)

               val matchesPrefix = isSome o matchPrefix

               fun find (c: t, s: string, startPos, short: bool) =
                  let
                     val n = String.size s
                     fun loop (i: int) =
                        if i >= n
                           then NONE
                        else
                           case match {compiled = c,
                                       short = short,
                                       startPos = i,
                                       string = s} of
                              NONE => loop (i + 1)
                            | SOME m => SOME m
                  in loop startPos
                  end

               fun findLong (c, s, i) = find (c, s, i, false)
               fun findShort (c, s, i) = find (c, s, i, true)

               fun foreachMatchShort (c, s, f: Match.t -> unit) =
                  let
                     fun loop i =
                        case findShort (c, s, i) of
                           NONE => ()
                         | SOME m => (f m; loop (Match.endOf m))
                  in
                     loop 0
                  end
            end

         fun compileDFA r =
            let
               val nfa = NFA.fromRegexp r
            in
               Compiled.T
               {regexp = r,
                machine = Compiled.DFA (DFA.minimize (DFA.fromNFA nfa))}
            end

         val compileDFA =
            Trace.trace ("Regexp.compileDFA", layout, Compiled.layout) compileDFA

         fun compileNFA r =
            Compiled.T
            {regexp = r,
             machine = Compiled.NFA (NFA.fromRegexp r)}

         val compileNFA =
            Trace.trace ("Regexp.compileNFA", layout, Compiled.layout) compileNFA

         (* POSIX 1003.2 regular expressions 
          *  caveats: does not support back references '\N'
          *           does not support unmatched ')'
          *           does not support '[=' style coallating elements
          *           does not support coallating elements as range endpoints
          * 
          * grammar:
          *    S ::= Re
          *   Re ::= Br Re0
          *  Re0 ::= e | '|' Br Re0
          *   Br ::= P Br0
          *  Br0 ::= e | P Br0
          *    P ::= A P0
          *   P0 ::= e | '*' | '+' | '?' | Bnd
          *  Bnd ::= '{' N Bnd0 '}'
          * Bnd0 ::= e | ',' Bnd1
          * Bnd1 ::= e | N
          *    A ::= '(' Re ')'
          *        | '()'
          *        | '[' Be ']'
          *        | '.'
          *        | '^'
          *        | '$'
          *        | '\' C
          *        | C
          *   Be ::= Be0
          *  Be0 ::= '^' Be1 | Be1
          *  Be1 ::= ']' Be2 | Be2
          *  Be2 ::= '-' Be3 | Be3
          *  Be3 ::= e
          *        | '-'
          *        | C '-' C Be3
          *        | '[' '.' Ce '.' ']' Be3
          *        | '[' ':' Cl ':' ']' Be3
          *        | C Be3
          *   Ce ::= e | C Ce
          *   Cl ::= 'alnum' | ... | 'xdigit'
          *)
         local
            exception X of string
            type res = t * Save.t vector

            fun S (s: char list) : res =
               Re (s, fn (s, re, saves) =>
                   case s of
                      [] => (re, saves)
                    | _ => raise (X "S"))
            and Re (s: char list,
                    k: char list * t * Save.t vector -> res) =
               Br (s, fn (s, re, saves) => 
                   Re0 (s, [re], [saves], k))
            and Re0 (s: char list, res: t list, savess: Save.t vector list,
                     k: char list * t * Save.t vector -> res) =
               let
                  fun finish s =
                     k (s, or (List.rev res), Vector.concat (List.rev savess))
               in
                  case s of
                     [] => finish s
                   | #")"::_ => finish s
                   | #"|"::s => Br (s, fn (s, re, saves) =>
                                    Re0 (s, re::res, saves::savess, k))
                   | _ => raise (X "Re0")
               end
            and Br (s: char list,
                    k: char list * t * Save.t vector -> res) =
               P (s, fn (s, re, saves) => 
                  Br0 (s, [re], [saves], k))
            and Br0 (s: char list, res: t list, savess: Save.t vector list,
                     k: char list * t * Save.t vector -> res) = 
               let
                  fun finish s =
                     k (s, seq (List.rev res), Vector.concat (List.rev savess))
               in
                  case s of
                     [] => finish s
                   | #")"::_ => finish s
                   | #"|"::_ => finish s
                   | _ => P (s, fn (s, re, saves) => 
                             Br0 (s, re::res, saves::savess, k))
               end
            and P (s: char list,
                   k: char list * t * Save.t vector -> res) =
               A (s, fn (s, re, saves) => P0 (s, re, saves, [], [], k))
            and P0 (s: char list, 
                    re: t, saves: Save.t vector,
                    res: t list, savess: Save.t vector list,
                    k: char list * t * Save.t vector -> res) =
               let
                  fun finish (s, re) =
                     k (s, seq (List.rev (re::res)), 
                        Vector.concat (List.rev (saves::savess)))
                  fun default () =
                     let
                        val res = re::res
                        val savess = saves::savess
                     in
                        A (s, fn (s, re, saves) =>
                           P0 (s, re, saves, res, savess, k))
                     end
               in
                  case s of 
                     [] => finish (s, re)
                   | #")"::_ => finish (s, re)
                   | #"|"::_ => finish (s, re)
                   | #"*"::s => finish (s, star re)
                   | #"+"::s => finish (s, oneOrMore re)
                   | #"?"::s => finish (s, optional re)
                   | #"{"::(c::s) => if Char.isDigit c
                                        then Bnd (c::s, fn (s, f) =>
                                                  finish (s, f re))
                                     else default ()
                   | _ => default ()
               end
            and Bnd (s: char list,
                     k: char list * (t -> t) -> res) =
               N (s, fn (s, n) =>
                  Bnd0 (s, n, fn (s, f) =>
                        case s of 
                           #"}"::s => k (s, f)
                         | _ => raise (X "Bnd")))
            and Bnd0 (s: char list, n: int,
                      k: char list * (t -> t) -> res) =
               let
                  fun finish (s, f) = k (s, f)
               in
                  case s of
                     #"}"::_ => finish (s, fn re => repeat (re, n))
                   | #","::s => Bnd1 (s, n, k)
                   | _ => raise (X "Bnd0")
               end
            and Bnd1 (s: char list, n: int, 
                      k: char list * (t -> t) -> res) =
               let
                  fun finish (s, f) = k (s, f)
               in
                  case s of 
                     #"}"::_ => finish (s, fn re => lower (re, n))
                   | _ => N (s, fn (s, m) =>
                             if m < n
                                then raise (X "Bnd1")
                             else finish (s, fn re => range (re, n, m)))
               end
            and N (s: char list,
                   k: char list * int -> res) =
               let
               in
                  case s of
                     d::s' => (case Char.digitToInt d of
                                  SOME d => N1 (s', d, k)
                                | NONE => raise (X "N"))
                     | _ => raise (X "N")
               end
            and N1 (s: char list, n: int,
                    k: char list * int -> res) =
               let
                  fun finish s =
                     k (s, n)
               in
                  case s of
                     [] => finish s
                   | d::s' => (case Char.digitToInt d of
                                  SOME d => N1 (s', n * 10 + d, k)
                                | NONE => finish s)
               end
            and A (s: char list,
                   k: char list * t * Save.t vector -> res) =
               let
                  fun finish (s, re, saves) =
                     k (s, re, saves)
                  fun finishR (s, re) =
                     finish (s, re, Vector.new0 ())
                  fun finishN s =
                     finishR (s, null)
                  fun finishC (s, c) =
                     finishR (s, char c)
               in
                  case s of 
                     #"("::(#")"::s) => finishN s
                   | #"("::s => let
                                   val save' = Save.new ()
                                in
                                   Re (s, fn (s, re, saves) =>
                                       case s of
                                          #")"::s => k (s, save (re, save'),
                                                        Vector.concat
                                                        [Vector.new1 save', saves])
                                        | _ => raise (X "A"))
                                end
                   | #"["::s => let
                                in
                                   Be (s, fn (s, re) => 
                                       case s of
                                          #"]"::s => finishR (s, re)
                                        | _ => raise (X "A"))
                                end
                   | #"."::s => finishR (s, any)
                   | #"^"::s => finishR (s, anchorStart)
                   | #"$"::s => finishR (s, anchorFinish)
                   | #"\\"::(c::s) => finishC (s, c)
                   | c::s => if String.contains (")|*+?{", c)
                                then raise (X "A")
                             else finishC (s, c)
                   | _ => raise (X "A")
               end
            and Be (s: char list,
                    k: char list * t -> res) =
               Be0 (s, k)
            and Be0 (s: char list,
                     k: char list * t -> res) =
               let
               in
                  case s of
                     #"^"::s => Be1 (s, true, k)
                   | _ => Be1 (s, false, k)
               end
            and Be1 (s: char list, inv: bool,
                     k: char list * t -> res) =
               let
               in
                  case s of 
                     #"]"::s => Be2 (s, inv, [#"]"], k)
                   | _ => Be2 (s, inv, [], k)
               end
            and Be2 (s: char list, inv: bool, cs: char list,
                     k: char list * t -> res) =
               let
               in
                  case s of 
                     #"-"::s => Be3 (s, inv, #"-"::cs, [], [], k)
                   | _ => Be3 (s, inv, cs, [], [], k)
               end
            and Be3 (s: char list, inv: bool,
                     cs: char list, cps: (char -> bool) list, ces: string list,
                     k: char list * t -> res) =
               let
                 fun finish (s: char list,
                             cs: char list,
                             cps: (char -> bool) list,
                             ces: string list) =
                    let
                       fun finish' re = k (s, re)
                       val s = implode cs
                       val cp = fn c => List.exists (cps, fn cp => cp c)
                    in
                       if inv
                          then
                             (case ces of
                                 [] =>
                                    finish'
                                    (isNotChar
                                     (fn c =>
                                      cp c orelse String.contains (s, c)))
                               | _ => Error.bug "Regexp.fromString: can't handle collating elements in negated bracket expressions")
                       else finish' (List.fold
                                     (ces, or [isChar cp,
                                               oneOf s],
                                      fn (ce, re) =>
                                      or [string ce, re]))
                    end
               in
                  case s of
                     #"]"::_ => finish (s, cs, cps, ces)
                   | #"-"::s => (case s of
                                    #"]"::_ => finish (s, #"-"::cs, cps, ces)
                                  | _ => raise (X "Be3"))
                   | c1::(#"-"::(c2::s)) => 
                        let
                           val r1 = Char.ord c1
                           val r2 = Char.ord c2
                           val cp = fn c =>
                                    let val r = Char.ord c
                                    in r1 <= r andalso r <= r2
                                    end
                        in 
                           Be3 (s, inv, cs, cp::cps, ces, k)
                        end
                   | #"["::(#"."::s) =>
                        Ce (s, [], fn (s, ce) =>
                            case s of
                               #"."::(#"]"::s) => Be3 (s, inv, cs, cps, ce::ces, k)
                             | _ => raise (X "Be3"))
                   | #"["::(#":"::s) =>
                        Cl (s, fn (s, cp) =>
                            case s of
                               #":"::(#"]"::s) => Be3 (s, inv, cs, cp::cps, ces, k)
                             | _ => raise (X "Be3"))
                   | c::s => Be3 (s, inv, c::cs, cps, ces, k)
                   | _ => raise (X "Be3")
               end
            and Ce (s: char list, ce: char list,
                    k: char list * string -> res) =
               let
                 fun finish s =
                    k (s, implode (List.rev ce))
               in
                 case s of
                    #"."::_ => finish s
                  | c::s => Ce (s, c::ce, k)
                  | _ => raise (X "Ce")
               end
            and Cl (s: char list,
                    k: char list * (char -> bool) -> res) =
               let
               in
                 case s of
                    #"a"::(#"l"::(#"n"::(#"u"::(#"m"::s)))) => 
                       k (s, Char.isAlphaNum)
                  | #"a"::(#"l"::(#"p"::(#"h"::(#"a"::s)))) => 
                       k (s, Char.isAlpha)
                  | #"b"::(#"l"::(#"a"::(#"n"::(#"k"::_)))) => 
                       raise (X "Cl:blank")
                  | #"c"::(#"n"::(#"t"::(#"r"::(#"l"::s)))) => 
                       k (s, Char.isCntrl)
                  | #"d"::(#"i"::(#"g"::(#"i"::(#"t"::s)))) => 
                       k (s, Char.isDigit)
                  | #"g"::(#"r"::(#"a"::(#"p"::(#"h"::s)))) => 
                       k (s, Char.isGraph)
                  | #"l"::(#"o"::(#"w"::(#"e"::(#"r"::s)))) => 
                       k (s, Char.isLower)
                  | #"p"::(#"r"::(#"i"::(#"n"::(#"t"::s)))) => 
                       k (s, Char.isPrint)
                  | #"p"::(#"u"::(#"n"::(#"c"::(#"t"::_)))) => 
                       raise (X "Cl:punct")
                  | #"s"::(#"p"::(#"a"::(#"c"::(#"e"::s)))) => 
                       k (s, Char.isSpace)
                  | #"u"::(#"p"::(#"p"::(#"e"::(#"r"::s)))) => 
                       k (s, Char.isUpper)
                  | #"x"::(#"d"::(#"i"::(#"g"::(#"i"::(#"t"::s))))) => 
                       k (s, Char.isHexDigit)
                  | _ => raise (X "Cl")
               end
         in
            val fromString: string -> (t * Save.t vector) option =
               fn s => (SOME (S (explode s))) handle X _ => NONE
            val fromString =
               Trace.trace ("Regexp.fromString",
                            String.layout,
                            Option.layout (layout o #1))
               fromString
         end
      end

(*    local
 *       val _ =
 *       let open Trace.Immediate
 *       in
 *          flagged()
 *          ; debug := Out Out.error
 *          ; on []
 *       end
 *       open Regexp
 *       val a = char #"a"
 *       val b = char #"b"
 *       val c = char #"c"
 *       val d = char #"d"
 *       val r = a
 *       val r = star a
 *       val r = or []
 *       val r = star any
 *       val r = seq []
 *       val r = or [a, b]
 *       val r = seq [a, b, c, d]
 *       val r = or [seq [a, b, c],
 *                seq [a, b, d]]
 *       val r =
 *       seq [star (or [a, b]),
 *            a, b, b]
 *       val d = digit
 *       val eol = char #"#"
 *       val space = oneOf " \t"
 *       val r =
 *       seq [or [anchorStart, notOneOf "0123456789("],
 *            or [seq [char #"(", d, d, d, char #")"],
 *                seq [d, d, d]],
 *            char #" ",
 *            d, d, d,
 *            oneOf " -",
 *            d, d, d, d,
 *            or [eol, nonDigit]]
 * 
 *       fun doit (name, lay) =
 *       let
 *          val dot = concat ["/tmp/", name, ".dot"]
 *          val ps = concat ["/tmp/", name, ".ps"]
 *          val _ = File.withOut (dot, fn out => Layout.output (lay, out))
 *          val _ = OS.Process.system (concat ["dot ", dot, " >", ps])
 *       in ()
 *       end
 *       val nfa = NFA.fromRegexp r
 *       val _ = doit ("nfa", NFA.layoutDot (nfa, "nfa"))
 *       val _ = Out.output (Out.error,
 *                        concat ["numCharClasses = ",
 *                                Int.toString (NFA.numCharClasses nfa),
 *                                "\n"])
 *       val dfa = DFA.fromNFA nfa
 *       val _ = doit ("dfa",
 *                  DFA.layoutDot {dfa = dfa, title = "dfa", showDead = false})
 *       val min = DFA.minimize dfa
 *       val _ = doit ("min",
 *                  DFA.layoutDot {dfa = min, title = "min", showDead = false})
 *    in
 *    end
 *)
end

(* local
 *    val _ =
 *       let
 *       open Trace.Immediate
 *       in
 *       debug := Out Out.error
 *       ; flagged()
 *       ; on ["Regexp.match"]
 *       end
 *    structure Z = TestRegexp (Regexp)
 * in
 * end
 *)
