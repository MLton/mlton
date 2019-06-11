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

structure Location =
   struct
      type t = {line: int, column: int}

      fun {line=line1, column=col1} <
          {line=line2, column=col2} =
          Int.< (line1, line2) orelse
          Int.< (col1, col2)

      val new = {line=0, column=0}

      fun toString {line, column} =
         concat [Int.toString line, ".", Int.toString column]

   end
structure State =
   struct
      (* The state primarily consists of the stream of future
       * characters. We keep a buffer of the last string read
       * annotated with the appropriate locations. Hopefully, 
       * it should be created only when needed, if the functions
       * are sufficiently inlined *)
      datatype t = T of {
         buffer: string,
         (* First location, even if buffer/locations is empty *)
         location: Location.t,
         (* Invariant: must be same length as buffer *)
         locations: Location.t vector,
         position: int,
         stream: char vector Stream.t}

      fun fromStream s =
         T {buffer=String.empty,
            location=Location.new,
            locations=Vector.new0 (),
            position=0,
            stream=s}
   end

datatype 'a result = Success of 'a
                   | Failure of
                     {expected: string list,
                      location: Location.t,
                      stack: string list}

datatype 'a t = T of
   {(* True if this can succeed and consume no input *)
    mayBeEmpty: bool,
    (* Potentially, a limited list of possible chars *)
    firstChars: char list option,
    (* These are used to report errors,
     * 1. If this parser is pruned by its first set
     * 2. To record the parser stack *)
    names: string list,
    run: (State.t -> ('a * State.t) result)}

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
          else concat ["\nin the context ", concatWith (stack, "/"), "\n\n"]]
      end

   fun toResult (filename, r) =
      case r of
           Success (a, _) => Result.Yes a
         | Failure err => Result.No (failureString (filename, err))

   fun inToStream i =
      Stream.cons (In.input i,
         Stream.delay (fn () =>
            inToStream i))
in
   fun parseStream (T {run, ...}, stream) =
      toResult ("<string>",
      run (State.fromStream
         (Stream.map (stream, Vector.new1))))
   fun parseString (T {run, ...}, str) =
      toResult ("<string>",
      run (State.fromStream (Stream.single str)))

   fun parseFile (T {run, ...}, file) =
      toResult (File.toString file,
      File.withIn (file, fn i =>
         (run o State.fromStream o inToStream) i))
end

fun indexLocations ({line, column}, s) =
   Vector.tabulate (String.length s,
      fn i =>
         if String.sub (s, i) = #"\n"
         then {line=line+1, column=0}
         else {line=line, column=column+1})

fun getNext (State.T {buffer, location, locations, position=i, stream}):
      (char * Location.t * State.t) option =
   let
      fun fillNext () =
         let
            val lastLocation = Vector.last locations
            val (buffer, stream) =
               case Stream.force stream of
                    SOME res => res
                  | NONE => (Vector.new0 (), Stream.empty ())
            val locations = indexLocations (lastLocation, buffer)
         in
            State.T {buffer=buffer,
                     location=lastLocation,
                     locations=locations,
                     position=0, stream=stream}
         end
   in
      case Int.compare (i, String.length buffer - 1) of
           LESS =>
               SOME (String.sub (buffer, i),
                     Vector.sub (locations, i),
                State.T {buffer=buffer, location=location,
                         locations=locations,
                         position=i+1, stream=stream})
         | EQ =>
               SOME (String.sub (buffer, i), Vector.sub (locations, i), fillNext ())
         | GREATER =>
              let
                 val state as State.T {buffer, location, locations, position, stream} = fillNext ()
              in
                 if String.length buffer = 0
                 then NONE
                 else SOME
                  (Vector.first buffer,
                   Vector.first locations,
                   State.T {buffer=buffer, location=location,
                            locations=locations,
                            position=position+1, stream=stream})
              end
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
      run=fn s => Success (a, s)}

fun fails ms =
   T {mayBeEmpty=false,
      firstChars=SOME [],
      names=ms,
      run=fn (State.T {location, ...}) =>
         Failure {expected=ms, location=location, stack=[]}}
fun fail m = fails [m]

fun expected (names, location) =
   Failure {expected=names, location=location, stack=[]}

fun named (name, T {firstChars, mayBeEmpty, run, ...}) =
    T {firstChars=firstChars,
       mayBeEmpty=mayBeEmpty,
       names=[name],
       run=fn s =>
         case run s of
              Success a => Success a
            | Failure {expected, location, stack} =>
                 Failure {expected=expected,
                          location=location,
                          stack=name :: stack}}

fun (T {firstChars=chars1, mayBeEmpty=empty1, names=names1, run=run1})
     <*>
    (T {firstChars=chars2, mayBeEmpty=empty2, names=names2, run=run2}) =
   T {firstChars=
         (if empty1
         then unionFirstChars (chars1, chars2)
         else chars1),
      mayBeEmpty=empty1 andalso empty2,
      names=List.concat [names1, names2],
      run=fn s =>
          case run1 s of
              Success (f, s') =>
                  (case run2 s' of
                       Success (b, s'') => Success (f b, s'')
                     | Failure err => Failure err)
            | Failure err => Failure err}

fun (T {firstChars, mayBeEmpty, names, run}) >>= f =
   T {firstChars=
         if mayBeEmpty
         then NONE
         else firstChars,
      mayBeEmpty=mayBeEmpty,
      names=names,
      run=fn s =>
         case run s of
              Success (a, s') =>
                  (case f a of
                       T {run, ...} => run s')
            | Failure err => Failure err}

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
         else if Location.< (loc2, loc1)
            then Failure f1
         else expected (List.concat [
                     (* if there's a stack, we'll use
                      * that as the best estimate *)
                     (case stack1 of
                          [] => exp1
                        | s :: _ => [s]),
                     (case stack2 of
                          [] => exp2
                        | s :: _ => [s])], location)
   fun selectRun (p1, p2, run1, run2, fail, location, s) =
      case (p1, p2) of
           (true, true) =>
              (case run1 s of
                     Success a => Success a
                   | Failure f1 =>
                        (case run2 s of
                              Success a => Success a
                            | Failure f2 => selectError (location, f1, f2)))
          | (true, false) => run1 s
          | (false, true) => run2 s
          | _ => fail


in
   fun (T {firstChars=chars1, mayBeEmpty=empty1, names=names1, run=run1})
        <|>
       (T {firstChars=chars2, mayBeEmpty=empty2, names=names2, run=run2}) =
       let
          val names = List.concat [names1, names2]
          fun failure location =
             Failure {expected=names,
                      location=location,
                      stack=[]}
       in
          T {firstChars=unionFirstChars (chars1, chars2),
             mayBeEmpty=empty1 orelse empty2,
             names=names,
             run=fn (s as State.T {location, ...}) =>
                case getNext s of
                     SOME (c, location, _) =>
                         selectRun (empty1 orelse checkFirstChars (chars1, c),
                                    empty2 orelse checkFirstChars (chars2, c),
                                    run1, run2,
                                    expected (names, location), location, s)
                   | NONE =>
                         selectRun (empty1, empty2,
                                    run1, run2,
                                    expected (names, location), location, s)}
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
      run=fn s as State.T {location, ...} =>
        case getNext s of
             SOME (c, _, s') => Success (c, s')
           | NONE =>
              Failure {expected=["char"],
                       location=location,
                       stack=[]}}

fun sat (t, _) = (* FIXME *) t
fun nextSat p = sat (next, p)

fun peek (p as T {mayBeEmpty, firstChars, names, run}) =
   T {mayBeEmpty=true,
      firstChars=firstChars,
      names=names,
      run=fn s =>
        case run s of
             Success (a, _) => Success (a, s)
           | Failure err => Failure err}

fun failing (p as T {names, run, ...}) =
   let
      val notNames = List.map (names, fn n => "not " ^ n)
   in
      T {mayBeEmpty=true,
         firstChars=NONE,
         names=notNames,
         run=fn s as State.T {location, ...} =>
            case run s of
                 Success _ => Failure
                    {expected=notNames,
                     location=location,
                     stack=[]}}
   end

fun notFollowedBy(p, c) =
   p <* failing c


fun many p = (pure []) <|> ((op ::) <$$> (p, delay (fn () => many p)))
fun many1 p = (op ::) <$$> (p, delay (fn () => many1 p))

fun manyFailing (p, f) = many (failing f *> p)
fun manyCharsFailing f = many (failing f *> next)

fun sepBy1 (t, sep) = (op ::) <$$> (t, many (sep *> t))
fun sepBy (t, sep) = (op ::) <$$> (t, many (sep *> t)) <|> pure []

fun any ps =
   let
      val names = List.concatMap (ps, fn T {names, ...} => names)
   in
      List.fold (ps, fails names, op <|>)
   end

fun optional t = SOME <$> t <|> pure NONE

fun char c =
   let
      val name = "#\"" ^ (String.fromChar c) ^ "\""
   in
      T {mayBeEmpty=false,
         firstChars=SOME [c],
         names=[name],
         run=fn s as State.T {location, ...} =>
            case getNext s of
                 SOME (c', _, s') =>
                  if c = c'
                  then Success (c, s')
                  else expected ([name], location)
               | NONE =>
                    expected ([name], location)}
   end

fun each ps = List.fold
   (ps, pure [],
    fn (p, x) => (op ::) <$$> (p, x))

fun str str =
   let
      val name = "\"" ^ str ^ "\""
   in
      if String.isEmpty str
      then named (name, pure String.empty)
      else fail "TODO str"
   end

val location = fail "TODO location"

fun toReader (T {run, ...}) (s : State.t) : ('a * State.t) option =
   case run s of
        Success (b, s') => SOME (b, s')
      | Failure _ => NONE

fun fromReader (r : State.t -> ('a * State.t) option) =
   T {firstChars=NONE,
      mayBeEmpty=true,
      names=["fromReader"],
      run=fn s as State.T {location, ...} =>
         case r s of
            SOME (b, s') => Success (b, s')
          | NONE => expected (["fromReader"], location)}

fun fromScan scan = fromReader (scan (toReader next))

val int = fromScan (Function.curry Int.scan StringCvt.DEC)

val space = nextSat Char.isSpace
val spaces = many space


local
   fun finishComment n () =
      any
      [str "(*" *> delay (finishComment (n + 1)),
       str "*)" *> (if n = 1 then pure (Char.space) else delay (finishComment (n - 1))),
       next *> delay (finishComment n)]
in
   val mlComment = str "(*" *> finishComment 1 ()
end
val mlSpaces = many (any [space, mlComment])

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
