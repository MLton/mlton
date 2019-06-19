(* Copyright (C) 2017,2019 Jason Carr, Matthew Fluet.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Parse :> PARSE =
struct

infix 1 <|> >>=
infix  3 <*> <* *>
infixr 4 <$> <$$> <$$$> <$$$$> <$ <$?>

structure Location = struct
   type t = {line: int, column: int}

   fun {line=line1, column=col1} <
       {line=line2, column=col2} =
       Int.< (line1, line2) orelse
       (line1 = line2) andalso
       Int.< (col1, col2)

   val new = {line=1, column=1}

   fun toString {line, column} =
      concat [Int.toString line, ".", Int.toString column]

end

structure Error = struct
   type t = {expected: string list,
             location: Location.t,
             stack: string list}

   fun max
      (f1 as {location=loc1, expected=_, stack=_},
       f2 as {location=loc2, expected=_, stack=_}) =
         if Location.< (loc1, loc2)
            then f2
         else f1

   val optionMax =
      fn (SOME f1, SOME f2) => SOME (max (f1, f2))
       | (SOME f1, NONE) => SOME f1
       | (NONE, SOME f2) => SOME f2
       | (NONE, NONE) => NONE
   val orElseMax =
      fn (SOME f1, f2) => max (f1, f2)
       | (NONE, f2) => f2
end

structure State = struct
   (* The state primarily consists of the stream of future
    * characters. We keep a buffer of the last string read
    * annotated with the appropriate locations. Hopefully, 
    * it should be created only when needed, if the functions
    * are sufficiently inlined *)
   datatype t = T of {
      stream: char Stream.t,
      (* Whenever we take an alternative
       * we'll record the furthest error seen, to better
       * deal with cases like `many x *> y`
       * giving very poor error messages. *)
      lastError: Error.t option,
      location: Location.t}

   fun fromStream s =
      T {lastError=NONE, location=Location.new, stream=s}
end

datatype 'a result = Success of 'a
                   | Failure of Error.t


datatype 'a t = T of
   {(* True if this can succeed and consume no input *)
    mayBeEmpty: bool,
    (* Potentially, a limited list of possible chars *)
    firstChars: char list option,
    (* These are used to report errors,
     * 1. If this parser is pruned by its first set
     * 2. To record the parser stack *)
    names: string list,
    run: (State.t * string list -> ('a * State.t) result)}


local
   fun failureString (filename, {expected, location, stack}) =
      let open String in
      concat
         ["Parse error\n",
          "at ", filename, " ", Location.toString location, "\n",
          case expected of
               [] => ""
             | _ => concat ["Expected one of: ", concatWith (expected, ", "), "."],
          if List.isEmpty stack
             then ""
          else concat ["\nin the context ", concatWith (List.rev stack, "/"), "\n\n"]]
      end

   fun toResult (filename, r) =
      case r of
           Success (a, _) => Result.Yes a
         | Failure err => Result.No (failureString (filename, err))

   fun inToStream i =
      case In.inputChar i of
           NONE => Stream.empty ()
         | SOME c =>
            Stream.cons (c,
               Stream.delay (fn () =>
                  inToStream i))
   fun listToStream l =
      case l of
           [] => Stream.empty ()
         | x :: xs => Stream.cons (x, listToStream xs)
in
   fun parseStream (T {run, ...}, stream) =
      toResult ("<string>",
      run (State.fromStream stream, []))
   fun parseString (T {run, ...}, str) =
      toResult ("<string>",
      run ((State.fromStream o listToStream o String.explode) str, []))

   fun parseFile (T {run, ...}, file) =
      toResult (File.toString file,
      File.withIn (file, fn i =>
         run ((State.fromStream o inToStream) i, [])))
end

fun incLocation ({line, column}, chr) =
   if chr = #"\n"
   then {line=line+1, column=0}
   else {line=line, column=column+1}

fun indexLocations (loc, s) =
   Vector.unfoldi (String.length s, loc,
      fn (i, {line, column}) =>
         let
            val loc = incLocation (loc, String.sub (s, i))
         in
            (loc, loc)
         end)

fun getNext (State.T {lastError, location, stream}):
      (char * Location.t * State.t) option =
   let
   in
      case Stream.force stream of
           SOME (c, rest) =>
            let
               val loc = incLocation (location, c)
            in
               SOME (c, loc, State.T {lastError=lastError, location=loc, stream=rest})
            end
         | NONE => NONE
   end

fun unionFirstChars (c1, c2) =
   case (c1, c2) of
        (SOME cs1, SOME cs2) => SOME (List.union (cs1, cs2, op =))
      | _ => NONE
fun checkFirstChars (cs, c) =
   case cs of
        SOME cs' => List.contains (cs', c, op =)
      | NONE => true

fun pure a =
   T {mayBeEmpty=true,
      firstChars=NONE,
      names=[],
      run=fn (s, _) => Success (a, s)}

fun fails ms =
   T {mayBeEmpty=false,
      firstChars=SOME [],
      names=ms,
      run=fn (State.T {location, ...}, stack) =>
         Failure {expected=ms, location=location, stack=stack}}
fun fail m = fails [m]

fun expected (names, location, stack) =
   Failure {expected=names, location=location, stack=stack}

fun named (name, T {firstChars, mayBeEmpty, run, ...}) =
    T {firstChars=firstChars,
       mayBeEmpty=mayBeEmpty,
       names=[name],
       run=fn (s, stack) =>
         run (s, name :: stack)}

fun (T {firstChars=chars1, mayBeEmpty=empty1, names=names1, run=run1})
     <*>
    (T {firstChars=chars2, mayBeEmpty=empty2, names=names2, run=run2}) =
   T {firstChars=
         (if empty1
         then unionFirstChars (chars1, chars2)
         else chars1),
      mayBeEmpty=empty1 andalso empty2,
      names=List.union (names1, names2, String.equals),
      run=fn (s, stack) =>
          case run1 (s, stack) of
              Success (f, s' as State.T {lastError, ...}) =>
                  (case run2 (s', stack) of
                       Success (b, s'') => Success (f b, s'')
                     | Failure err =>
                          (* report the latest error *)
                          Failure (Error.orElseMax (lastError, err)))
            | Failure err => Failure err}

fun (T {firstChars, mayBeEmpty, names, run}) >>= f =
   T {firstChars=
         if mayBeEmpty
         then NONE
         else firstChars,
      mayBeEmpty=mayBeEmpty,
      names=names,
      run=fn (s as State.T {lastError, ...}, stack) =>
         case run (s, stack) of
              Success (a, s') =>
                  (case f a of
                       T {run, ...} => run (s', stack))
            | Failure err =>
                 Failure (Error.orElseMax (lastError, err))}

fun fst a _ = a
fun snd _ b = b

fun curry f a b = f (a, b)
fun curry3 f a b c = f (a, b, c)
fun curry4 f a b c d = f (a, b, c, d)

fun f <$> p = (pure f) <*> p
fun f <$$> (p1, p2) = curry <$> (pure f) <*> p1 <*> p2
fun f <$$$> (p1, p2, p3) = curry3 <$> (pure f) <*> p1 <*> p2 <*> p3
fun f <$$$$> (p1, p2, p3, p4) = curry4 <$> (pure f) <*> p1 <*> p2 <*> p3 <*> p4
fun f <$?> p = p >>= (fn a => case f a of SOME b => pure b
                                        | NONE => fail "<$?>")
fun a <* b = fst <$> a <*> b
fun a *> b = snd <$> a <*> b
fun v <$ p = (fn _ => v) <$> p


local
   (* Attempt to select the most specific error,
    * first prioritize by farthest location,
    * else concatenate based on the stack *)
   fun selectError
      (location,
       f1 as {expected=exp1, stack=stack1, location=loc1},
       f2 as {expected=exp2, stack=stack2, location=loc2}) =
         if Location.< (loc1, loc2)
            then Failure f2
         else Failure f1
   fun selectRun (p1, p2, run1, run2, fail, location, s) =
      case (p1, p2) of
           (true, true) =>
              (case run1 s of
                     Success a => Success a
                   | Failure f1 =>
                        (case run2 s of
                              Success (a, State.T {lastError=f2, location, stream}) =>
                                 Success (a, State.T
                                    {lastError=Error.optionMax (f2, SOME f1),
                                     location=location, stream=stream})
                            | Failure f2 => selectError (location, f1, f2)))
          | (true, false) => run1 s
          | (false, true) => run2 s
          | _ => fail
in
   fun (T {firstChars=chars1, mayBeEmpty=empty1, names=names1, run=run1})
        <|>
       (T {firstChars=chars2, mayBeEmpty=empty2, names=names2, run=run2}) =
       let
          val names = List.union (names1, names2, String.equals)
       in
          T {firstChars=unionFirstChars (chars1, chars2),
             mayBeEmpty=empty1 orelse empty2,
             names=names,
             run=fn (s as State.T {location, ...}, stack) =>
                case getNext s of
                     SOME (c, location, _) =>
                         selectRun (empty1 orelse checkFirstChars (chars1, c),
                                    empty2 orelse checkFirstChars (chars2, c),
                                    run1, run2,
                                    expected (names, location, stack), location,
                                    (s, stack))
                   | NONE =>
                         selectRun (empty1, empty2,
                                    run1, run2,
                                    expected (names, location, stack), location,
                                    (s, stack))}
       end
end

structure Ops = struct
   val (op >>=) = (op >>=)
   val (op <*>) = (op <*>)
   val (op <$>) = (op <$>)
   val (op <$?>) = (op <$?>)
   val (op <$$>) = (op <$$>)
   val (op <$$$>) = (op <$$$>)
   val (op <$$$$>) = (op <$$$$>)
   val (op <*) = (op <*)
   val (op *>) = (op *>)
   val (op <$) = (op <$)
   val (op <|>) = (op <|>)
end

fun delay p = pure () >>= p

val next =
   T {mayBeEmpty=false,
      firstChars=NONE,
      names=["char"],
      run=fn (s as State.T {location, ...}, stack) =>
        case getNext s of
             SOME (c, _, s') => Success (c, s')
           | NONE => expected (["char"], location, stack)}

fun sat (T {firstChars, mayBeEmpty, names, run}, p) =
   T {firstChars=firstChars,
      mayBeEmpty=mayBeEmpty,
      names=names,
      run=fn (s as State.T {location, ...}, stack) =>
         case run (s, stack) of
              x as Success (a, _) =>
                  if p a
                     then x
                  else Failure {expected=["satisfying"],
                                location=location,
                                stack=stack}
            | x => x}

fun nextSat p = sat (next, p)

fun peek (p as T {mayBeEmpty, firstChars, names, run}) =
   T {mayBeEmpty=true,
      firstChars=firstChars,
      names=names,
      run=fn (s, stack) =>
        case run (s, stack) of
             Success (a, _) => Success (a, s)
           | Failure err => Failure err}

fun failing (p as T {names, run, ...}) =
   let
      val notNames = List.map (names, fn n => "not " ^ n)
   in
      T {mayBeEmpty=true,
         firstChars=NONE,
         names=notNames,
         run=fn (s as State.T {location, ...}, stack) =>
            case run (s, stack) of
                 Success _ => Failure
                    {expected=notNames,
                     location=location,
                     stack=stack}
               | Failure _ => Success ((), s)}
   end

fun notFollowedBy(p, c) =
   p <* failing c

fun many (T {firstChars, mayBeEmpty, names, run}) =
   let
      fun run' (s as State.T {lastError, location, stream}, stack, k) =
         case run (s, stack) of
              Success (a, s') => run' (s', stack, a :: k)
            | Failure err => Success (List.rev k,
                 State.T {lastError=Error.optionMax (lastError, SOME err),
                          location=location, stream=stream})
   in
      T {firstChars=firstChars,
         mayBeEmpty=true,
         names=names,
         run=fn (s, stack) => run' (s, stack, [])}
   end

fun many1 p = (op ::) <$$> (p, many p)

fun manyFailing (p, f) = many (failing f *> p)
fun manyCharsFailing f = many (failing f *> next)

fun sepBy1 (t, sep) = (op ::) <$$> (t, many (sep *> t))
fun sepBy (t, sep) = sepBy1 (t, sep) <|> pure []

fun any ps =
   let
      val {unions, ...} = List.set {equals=String.equals, layout=Layout.str}
      val names = unions (List.map (ps, fn T {names, ...} => names))
      val {unions, ...} = List.set {equals=Char.equals, layout=fn _ => Layout.empty}
      val firstChars =
         Option.map (List.fold (ps, SOME [],
            fn (T {firstChars, ...}, k) =>
               case (k, firstChars) of
                    (SOME xs, SOME x) => SOME (x :: xs)
                  | _ => NONE),
            unions)

      fun tryAll (s as State.T {lastError, location, stream}, stack, runs) =
         case runs of
              (* TODO Fix the error messages to make the best pick *)
              [] => expected (names, location, stack)
            | r :: rs =>
                 (case r (s, stack) of
                      Success a => Success a
                    | Failure err => tryAll
                     (State.T {lastError=Error.optionMax (lastError, SOME err),
                               location=location, stream=stream}, stack, rs))
   in
      T {firstChars=firstChars,
         mayBeEmpty=List.forall (ps, fn T {mayBeEmpty, ...} => mayBeEmpty),
         names=names,
         run=fn (s as State.T {location, ...}, stack) =>
            case getNext s of
                 SOME (c, location, _) =>
                     tryAll (s, stack, List.keepAllMap(ps,
                        fn T {mayBeEmpty, firstChars, run, ...} =>
                           if mayBeEmpty orelse checkFirstChars (firstChars, c)
                           then SOME run
                           else NONE))
               | NONE =>
                     tryAll (s, stack, List.keepAllMap(ps,
                        fn T {mayBeEmpty, firstChars, run, ...} =>
                           if mayBeEmpty
                           then SOME run
                           else NONE))}
   end

fun optional t = SOME <$> t <|> pure NONE

fun char c =
   let
      val name = "#\"" ^ (String.fromChar c) ^ "\""
   in
      T {mayBeEmpty=false,
         firstChars=SOME [c],
         names=[name],
         run=fn (s as State.T {location, ...}, stack) =>
            case getNext s of
                 SOME (c', _, s') =>
                  if c = c'
                  then Success (c, s')
                  else expected ([name], location, stack)
               | NONE =>
                    expected ([name], location, stack)}
   end

fun each ps = List.fold
   (ps, pure [],
    fn (p, x) => (op ::) <$$> (p, x))

fun str str =
   let
      val name = "\"" ^ str ^ "\""
      datatype 'a Status = Yes | More of 'a | No

      fun run (s as State.T {location, ...}, stack) =
         String.fold (str, Success (str, s),
            fn (c, progress) =>
               case progress of
                    Success (_, s) =>
                     (case getNext s of
                       SOME (c', l, s') =>
                           if c = c'
                           then Success (str, s')
                           else expected ([String.fromChar c], l, stack)
                     | NONE => expected ([String.fromChar c], location, stack))
                  | _ => progress)
   in
      if String.isEmpty str
      then named (name, pure String.empty)
      else
         T {firstChars=SOME [String.sub(str, 0)],
            mayBeEmpty=false,
            names=[name],
            run=run}
   end

val location =
   T {firstChars=NONE,
      mayBeEmpty=true,
      names=[],
      run=fn (s as State.T {location, ...}, _) =>
         Success (location, s)}

fun toReader (T {run, ...}) (s : State.t) : ('a * State.t) option =
   case run (s, []) of
        Success (b, s') => SOME (b, s')
      | Failure _ => NONE

fun fromReader (r : State.t -> ('a * State.t) option) =
   T {firstChars=NONE,
      mayBeEmpty=true,
      names=["fromReader"],
      run=fn (s as State.T {location, ...}, stack) =>
         case r s of
            SOME (b, s') => Success (b, s')
          | NONE => expected (["fromReader"], location, stack)}

fun fromScan scan = fromReader (scan (toReader next))

val int = fromScan (Function.curry Int.scan StringCvt.DEC)

val space = named ("space", nextSat Char.isSpace)
val spaces = many space


local
   fun finishComment n () =
      any
      [str "(*" *> delay (finishComment (n + 1)),
       str "*)" *> (if n = 1 then pure (Char.space) else delay (finishComment (n - 1))),
       next *> delay (finishComment n)]
in
   val mlComment = named ("comment", str "(*" *> finishComment 1 ())
end
val mlSpaces = many (any [mlComment, space])

(* The following parsers always (and only) consume spaces before
 * performing a `char` or `str`.
 *)

(* parse SML-style keyword (not followed by alphanum id character) *)
fun kw s =
   mlSpaces *> str s *>
   failing (nextSat (fn c => Char.isAlphaNum c orelse c = #"_" orelse c = #"'"))
(* parse SML-style symbol (not followed by symbolic id character) *)
fun sym s =
   mlSpaces *> str s *>
   failing (nextSat (fn c => String.contains ("!%&$#+-/:<=>?@\\~`^|*", c)))

(* parse `Bool.layout` *)
val bool: bool t =
   (true <$ kw "true") <|> (false <$ kw "false")

(* parse `Option.layout` *)
fun option (p: 'a t): 'a option t =
   (SOME <$> (kw "Some" *> p)) <|> (NONE <$ kw "None")

local
   fun between (l, p: 'a t, r): 'a t =
      mlSpaces *> char l *> p <* mlSpaces <* char r
in
   fun paren p = between (#"(", p, #")")
   fun cbrack p = between (#"{", p, #"}")
   fun sbrack p = between (#"[", p, #"]")
end
(* parse `List.layout` (not, `Layout.list`) *)
fun list (p: 'a t): 'a list t =
   sbrack (sepBy (p, mlSpaces *> char #","))
fun listOpt (p: 'a t): 'a list t =
   list p <|> pure []
(* parse `Vector.layout` (not, `Layout.vector`) *)
fun vector (p: 'a t): 'a vector t =
   Vector.fromList <$> paren (sepBy (p, mlSpaces *> char #","))
fun vectorOpt (p: 'a t): 'a vector t =
   vector p <|> pure (Vector.new0 ())

local
   fun field (s, p) = kw s *> sym "=" *> p
in
   (* parse first field of a record (not preceded by `,`) *)
   val ffield = field
   (* parse next field of a record (preceded by `,`) *)
   fun nfield (s, p) = mlSpaces *> char #"," *> field (s, p)
end

end
