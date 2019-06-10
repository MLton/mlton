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
    firstChars: char vector option,
    (* These are used to report errors,
     * 1. If this parser is pruned by its first set
     * 2. To record the parser stack *)
    names: string list,
    run: (State.t -> (State.t * 'a) result)}

fun indexLocations ({line, column}, s) =
   Vector.tabulate (String.length s,
      fn i =>
         if String.sub (s, i) = #"\n"
         then {line=line+1, column=0}
         else {line=line, column=column+1})

fun getNext (State.T {buffer, location, locations, position=i, stream}):
      (char * Location.t * State.T) option =
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
                State.T {buffer=buffer, location=location
                         locations=locations,
                         position=position+1, stream=stream})
         | EQ =>
               SOME (String.sub (buffer, i), Vector.sub (locations, i), fillNext ())
         | GREATER =>
              let
                 val state as State.T {buffer, locations, position, stream} = fillNext ()
              in
                 if String.length buffer = 0
                 then NONE
                 else SOME
                  (Vector.first buffer,
                   Vector.first locations,
                   State.T {buffer=buffer, locations=locations,
                            position=position+1, stream=stream})
              end
   end

fun unionFirstChars (c1, c2) =
   case (c1, c2) of
        (SOME cs1, SOME cs2) => SOME (List.union (cs1, cs2, op =))
      | _ => NONE
fun checkFirstChars (cs, c) =
   case cs of
        SOME cs' => Vector.contains (cs', op =)
      | NONE => true

fun pure a =
   T {mayBeEmpty=true,
      firstChars=NONE,
      names=[],
      run=fn s => (a, s)}

fun fail m =
   T {mayBeEmpty=false,
      firstChars=SOME [],
      names=[m],
      run=fn (State.T {location, ...}) =>
         Failure {expected=[m], location=location, stack=[]}}

fun expected (names, location) =
   Failure {expected=expected, location=location, stack=[]}

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
              Success (a, s') => #run (f a) s'
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


fun (T {firstChars=chars1, mayBeEmpty=empty1, names=names1, run=run1})
     <|>
    (T {firstChars=chars2, mayBeEmpty=empty2, names=names2, run=run2}) =
   T {firstChars=unionFirstChars (chars1, chars2),
      mayBeEmpty=empty1 orelse empty2,
      names=List.concat [names1, names2],
      run=fn s =>
        let
           val (c, _, _) = getNext ()
        in
           if checkFirstChars (chars1, c)
           then case run1 s of
                     Success a => Success a
                   | Failure err1 =>
                        (case run2 s of
                              Success a => Success a
                            | Failure err2 => Failure (err1 ++ err2))
           else expected names1
        end}

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

fun next (s : State.t) =
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
             Success (a, _) => (a, s)
           | Failure err => Failure err}

fun failing (p as T {names, run, ...}) =
   let
      val notNames = Vector.map (names, fn n => "not " ^ n)
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



fun many p = ((op ::) <$$> (p, fn s => many p s))
fun many1 p = ((op ::) <$$> (p, many' p))

fun manyFailing(p, f) = many (failing f *> p)
fun manyCharsFailing f = many (failing f *> next)

fun sepBy1(t, sep) = uncut ((op ::) <$$> (t, many' (sep *> t)))
fun sepBy(t, sep) = uncut ((op ::) <$$> (t, many' (sep *> t)) <|> pure [])

fun optional t = SOME <$> t <|> pure NONE

fun char c s =
   let
      val name = "#\"" ^ String.fromChar ^ "\""
   in
      T {mayBeEmpty=false,
         firstChars=SOME [c],
         names=[name],
         run=fn s =>
            case getNext s of
                 (c', _, s') =>
                  if c = c'
                  then Success (c, s')
                  else expected [name]}
   end

fun each([]) = pure []
  | each(p::ps) = (curry (op ::)) <$> p <*> (each ps)

fun matchList s1 l2 = case (Stream.force s1, l2)
   of (_, []) => Success ((), s1)
    | (NONE, (_::_)) => Failure []
    | (SOME ((h, _), r), (x :: xs)) => if h = x then matchList r xs else Failure []


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
      run=fn s =>
         case r s of
            SOME (b, s') => Success (b, s')
          | NONE => expected ["fromReader"]}

fun fromScan scan = fromReader (scan (toReader next))

val int = fromScan (Function.curry Int.scan StringCvt.DEC)

fun compose (p1 : char list t, p2 : 'a t) (s : State.t) =
   let
      exception ComposeFail of string list
      fun makeStr s' () = case Stream.force s' of
         NONE => Stream.empty ()
       | SOME ((_, pos), r) =>
            (case p1 s' of
                Success (b, r) => (case b of
                    (* equivalent, but avoids the jumping from append of fromList *)
                    c::[] => Stream.cons((c, pos), Stream.delay (makeStr r))
                  | _  => Stream.append
                        (indexStream(pos, Stream.fromList b),
                         Stream.delay (makeStr r)))
              | Failure m => raise ComposeFail m
              | FailCut m => raise ComposeFail m)
   in
      p2 (makeStr (s) () ) handle ComposeFail m => Failure m
   end

val space = nextSat Char.isSpace
val spaces = many space

(* The following parsers always (and only) consume spaces before
 * performing a `char` or `str`.
 *)

(* parse SML-style keyword (not followed by alphanum id character) *)
fun kw s =
   spaces *> str s *>
   failing (nextSat (fn c => Char.isAlphaNum c orelse c = #"_" orelse c = #"'"))
(* parse SML-style symbol (not followed by symbolic id character) *)
fun sym s =
   spaces *> str s *>
   failing (nextSat (fn c => String.contains ("!%&$#+-/:<=>?@\\~`^|*", c)))

(* parse `Bool.layout` *)
val bool: bool t =
   (true <$ kw "true") <|> (false <$ kw "false")

(* parse `Option.layout` *)
fun option (p: 'a t): 'a option t =
   (SOME <$> (kw "Some" *> p)) <|> (NONE <$ kw "None")

local
   fun between (l, p: 'a t, r): 'a t =
      spaces *> char l *> p <* spaces <* char r
in
   fun paren p = between (#"(", p, #")")
   fun cbrack p = between (#"{", p, #"}")
   fun sbrack p = between (#"[", p, #"]")
end
(* parse `List.layout` (not, `Layout.list`) *)
fun list (p: 'a t): 'a list t =
   sbrack (sepBy (p, spaces *> char #","))
fun listOpt (p: 'a t): 'a list t =
   list p <|> pure []
(* parse `Vector.layout` (not, `Layout.vector`) *)
fun vector (p: 'a t): 'a vector t =
   Vector.fromList <$> paren (sepBy (p, spaces *> char #","))
fun vectorOpt (p: 'a t): 'a vector t =
   vector p <|> pure (Vector.new0 ())

local
   fun field (s, p) = kw s *> sym "=" *> p
in
   (* parse first field of a record (not preceded by `,`) *)
   val ffield = field
   (* parse next field of a record (preceded by `,`) *)
   fun nfield (s, p) = spaces *> char #"," *> field (s, p)
end

local
   fun finiComment n () =
      any
      [str "(*" *> delay (finiComment (n + 1)),
       str "*)" *> (if n = 1 then pure [Char.space] else delay (finiComment (n - 1))),
       next *> delay (finiComment n)]

   val skipComments =
      any
      [str "(*" *> finiComment 1 (),
       (fn cs =>
        Char.dquote ::
        (List.foldr
         (cs, [Char.dquote], fn (c, cs) =>
          String.explode (Char.escapeSML c) @ cs))) <$>
       (char Char.dquote *>
        many (fromScan Char.scan) <*
        char Char.dquote),
       each [next]]
in
   val skipCommentsML = skipComments
end

end
