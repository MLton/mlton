(* Copyright (C) 2017,2019 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor XmlTree (S: XML_TREE_STRUCTS): XML_TREE =
struct

open S

infix  1 <|> >>=
infix  3 <*> <* *>
infixr 4 <$> <$$> <$$$> <$$$$> <$ <$?>
structure Parse =
   struct
      open Parse
      fun kw s =
         spaces *> str s *>
         failing (nextSat (fn c => Char.isAlphaNum c orelse c = #"_" orelse c = #"'"))
      fun sym s =
         spaces *> str s *>
         failing (nextSat (fn c => String.contains ("!%&$#+-/:<=>?@\\!`^|*", c)))
      fun option (p: 'a t): 'a option t =
         (kw "Some" *> (SOME <$> p)) <|>
         (kw "None" *> pure NONE)
      local
         fun between (l, p: 'a t, r): 'a t =
            spaces *> char l *> p <* spaces <* char r
      in
         fun paren p = between (#"(", p, #")")
         fun sbrack p = between (#"[", p, #"]")
      end
      fun vector (p: 'a t): 'a vector t =
         Vector.fromList <$> paren (sepBy (p, spaces *> char #","))
      fun vectorOpt (p: 'a t): 'a vector t =
         vector p <|> pure (Vector.new0 ())
      fun list (p: 'a t): 'a list t =
         sbrack (sepBy (p, spaces *> char #","))
      fun listOpt (p: 'a t): 'a list t =
         list p <|> pure []
   end

local
   open Layout
in
   fun layoutTyvars ts =
      case Vector.length ts of
         0 => empty
       | _ => seq [tuple (Vector.toListMap (ts, Tyvar.layout)), str " "]
end
local
   open Parse
in
   val parseTyvars =
      vectorOpt Tyvar.parse
end

structure Type =
   struct
      structure T = HashType (S)
      open T

      datatype dest =
         Var of Tyvar.t
       | Con of Tycon.t * t vector

      fun dest t =
         case Dest.dest t of
            Dest.Var a => Var a
          | Dest.Con x => Con x

      fun parse () =
         let
            open Parse
            val parse = delay parse
         in
            (kw "unit" *> pure unit)
            <|>
            (Tyvar.parse >>= (pure o T.var))
            <|>
            (vectorOpt parse >>= (fn args =>
             Tycon.parse >>= (fn con =>
             pure (T.con (con, args)))))
         end
      val parse = parse ()
   end

fun maybeConstrain (x, t) =
   let
      open Layout
   in
      if !Control.showTypes
         then seq [x, str " : ", Type.layout t]
      else x
   end

local
   open Layout
in
   fun layoutTargs (ts: Type.t vector) =
      if !Control.showTypes
         andalso 0 < Vector.length ts
         then list (Vector.toListMap (ts, Type.layout))
      else empty
end
local
   open Parse
in
   val parseTargs =
      Vector.fromList <$> listOpt Type.parse
end

structure Pat =
   struct
      datatype t = T of {arg: (Var.t * Type.t) option,
                         con: Con.t,
                         targs: Type.t vector}

      local
         open Layout
      in
         fun layout (T {arg, con, targs}) =
            seq [Con.layout con,
                 layoutTargs targs,
                 case arg of
                    NONE => empty
                  | SOME (x, t) =>
                       maybeConstrain (seq [str " ", Var.layout x], t)]
      end

      local
         open Parse
      in
         val parse =
            Con.parse >>= (fn con =>
            parseTargs >>= (fn targs =>
            optional (Var.parse >>= (fn x =>
                      sym ":" *>
                      Type.parse >>= (fn t =>
                      pure (x, t)))) >>= (fn arg =>
            pure (T {con = con, targs = targs, arg = arg}))))
      end

      fun con (T {con, ...}) = con

      local
         fun make c = T {con = c, targs = Vector.new0 (), arg = NONE}
      in 
         val falsee = make Con.falsee
         val truee = make Con.truee
      end
   end

structure Cases =
   struct
      datatype 'a t = 
         Con of (Pat.t * 'a) vector
       | Word of WordSize.t * (WordX.t * 'a) vector

      fun fold (c: 'a t, b: 'b, f: 'a * 'b -> 'b): 'b =
         let
            fun doit l = Vector.fold (l, b, fn ((_, a), b) => f (a, b))
         in
            case c of
               Con l => doit l
             | Word (_, l) => doit l
         end

      fun map (c: 'a t, f: 'a -> 'b): 'b t =
         let
            fun doit l = Vector.map (l, fn (i, x) => (i, f x))
         in
            case c of
               Con l => Con (doit l)
             | Word (s, l) => Word (s, doit l)
         end

      fun foreach (c, f) = fold (c, (), fn (x, ()) => f x)

      fun foreach' (c: 'a t, f: 'a -> unit, fc: Pat.t -> unit): unit =
         let
            fun doit l = Vector.foreach (l, fn (_, a) => f a)
         in
            case c of
               Con l => Vector.foreach (l, fn (c, a) => (fc c; f a))
             | Word (_, l) => doit l
         end
   end

structure VarExp =
   struct
      datatype t = T of {targs: Type.t vector,
                         var: Var.t}

      fun equals (T {targs = targs1, var = var1},
                  T {targs = targs2, var = var2}) =
         Var.equals (var1, var2)
         andalso Vector.equals (targs1, targs2, Type.equals)

      fun mono var = T {var = var, targs = Vector.new0 ()}

      local
         fun make f (T r) = f r
      in
         val var = make #var
      end

      fun layout (T {var, targs, ...}) =
         if !Control.showTypes
            then let open Layout
                 in
                    if Vector.isEmpty targs
                       then Var.layout var
                    else seq [Var.layout var, str " ",
                              Vector.layout Type.layout targs]
                 end
         else Var.layout var

      val parse =
         let
            open Parse
            val varExcepts = Vector.new3 ("exception", "val", "in")
         in
            Var.parseExcept varExcepts >>= (fn var =>
            vectorOpt Type.parse >>= (fn targs =>
            pure (T {var = var, targs = targs})))
         end
   end

(*---------------------------------------------------*)
(*           Expressions and Declarations            *)
(*---------------------------------------------------*)

datatype exp =
   Exp of {decs: dec list,
           result: VarExp.t}
and primExp =
    App of {func: VarExp.t,
            arg: VarExp.t}
  | Case of {test: VarExp.t,
             cases: exp Cases.t,
             default: exp option}
  | ConApp of {con: Con.t,
               targs: Type.t vector,
               arg: VarExp.t option}
  | Const of Const.t
  | Handle of {try: exp,
               catch: Var.t * Type.t,
               handler: exp}
  | Lambda of lambda
  | PrimApp of {args: VarExp.t vector,
                prim: Type.t Prim.t,
                targs: Type.t vector}
  | Profile of ProfileExp.t
  | Raise of {exn: VarExp.t, extend: bool}
  | Select of {tuple: VarExp.t,
               offset: int}
  | Tuple of VarExp.t vector
  | Var of VarExp.t
and dec =
   Exception of {arg: Type.t option,
                 con: Con.t}
  | Fun of {decs: {lambda: lambda,
                   ty: Type.t,
                   var: Var.t} vector,
            tyvars: Tyvar.t vector}
  | MonoVal of {exp: primExp,
                ty: Type.t,
                var: Var.t}
  | PolyVal of {exp: exp,
                ty: Type.t,
                tyvars: Tyvar.t vector,
                var: Var.t}
and lambda = Lam of {arg: Var.t,
                     argType: Type.t,
                     body: exp,
                     mayInline: bool,
                     plist: PropertyList.t}

local
   open Layout
in
   fun layoutConArg {arg, con} =
      seq [Con.layout con,
           case arg of
              NONE => empty
            | SOME t => seq [str " of ", Type.layout t]]
   fun layoutDec d =
      case d of
         Exception ca =>
            seq [str "exception ", layoutConArg ca]
       | Fun {decs, tyvars} =>
            align [seq [str "val rec ", layoutTyvars tyvars],
                   indent (align (Vector.toListMap
                                  (decs, fn {lambda, ty, var} =>
                                   align [seq [maybeConstrain (Var.layout var, ty),
                                               str " = "],
                                          indent (layoutLambda lambda, 3)])),
                           3)]
       | MonoVal {exp, ty, var} =>
            align [seq [str "val ",
                        maybeConstrain (Var.layout var, ty), str " = "],
                   indent (layoutPrimExp exp, 3)]
       | PolyVal {exp, ty, tyvars, var} =>
            align [seq [str "val ",
                        if !Control.showTypes
                           then layoutTyvars tyvars
                        else empty,
                        maybeConstrain (Var.layout var, ty),
                        str " = "],
                   indent (layoutExp exp, 3)]
   and layoutExp (Exp {decs, result}) =
      align [str "let",
             indent (align (List.map (decs, layoutDec)), 3),
             str "in",
             indent (VarExp.layout result, 3),
             str "end"]
   and layoutPrimExp e =
      case e of
         App {arg, func} => seq [VarExp.layout func, str " ", VarExp.layout arg]
       | Case {test, cases, default} =>
            let
               fun doit (v, layoutP) =
                  Vector.toListMap
                  (v, fn (p, e) =>
                   mayAlign [seq [layoutP p, str " =>"],
                             indent (layoutExp e, 3)])
               datatype z = datatype Cases.t
               val (suffix, cases) =
                  case cases of
                     Con v => (empty, doit (v, Pat.layout))
                   | Word (ws, v) => (str (WordSize.toString ws),
                                      doit (v, fn w => WordX.layout (w, {suffix = true})))
               val cases =
                  case default of
                     NONE => cases
                   | SOME e => cases @ [mayAlign [str "_ =>", indent (layoutExp e, 3)]]
            in
               align [seq [str "case", suffix, str " ", VarExp.layout test, str " of"],
                      indent (alignPrefix (cases, "| "), 2)]
            end
       | ConApp {arg, con, targs, ...} =>
            seq [str "new ",
                 Con.layout con,
                 layoutTargs targs,
                 case arg of
                    NONE => empty
                  | SOME x => seq [str " ", VarExp.layout x]]
       | Const c => Const.layout c
       | Handle {catch, handler, try} =>
            align [layoutExp try,
                   seq [str "handle ",
                        maybeConstrain (Var.layout (#1 catch), #2 catch),
                        str " => ", layoutExp handler]]
       | Lambda l => layoutLambda l
       | PrimApp {args, prim, targs} =>
            seq [str "prim ",
                 Prim.layoutFull(prim, Type.layout),
                 layoutTargs targs,
                 str " ", tuple (Vector.toListMap (args, VarExp.layout))]
       | Profile e => ProfileExp.layout e
       | Raise {exn, extend} =>
            seq [str "raise ",
                 str (if extend then "extend " else ""),
                 VarExp.layout exn]
       | Select {offset, tuple} =>
            seq [str "#", Int.layout offset, str " ", VarExp.layout tuple]
       | Tuple xs => tuple (Vector.toList
            (Vector.mapi(xs, fn (i, x) => seq
            (* very specific case to prevent open comments *)
                [str (if i = 0 andalso
                        (case x of (VarExp.T {var, ...}) =>
                           String.sub(Var.toString var, 0) = #"*")
                      then " "
                      else ""),
                 VarExp.layout x])))
       | Var x => VarExp.layout x
   and layoutLambda (Lam {arg, argType, body, mayInline, ...}) =
      align [seq [str "fn ",
                  str (if not mayInline then "noinline " else ""),
                  maybeConstrain (Var.layout arg, argType),
                  str " => "],
             layoutExp body]

end
local
   open Parse
in
   val parseConArg =
      Con.parse >>= (fn con =>
      optional (kw "of" *> Type.parse) >>= (fn arg =>
      pure {con = con, arg = arg}))
   val parseArgs = vector VarExp.parse
   fun parseDec () =
      any
      [kw "exception" *> (Exception <$> parseConArg),
       kw "val" *> kw "rec" *>
       parseTyvars >>= (fn tyvars =>
       many (Var.parse >>= (fn var =>
             sym ":" *>
             Type.parse >>= (fn ty =>
             sym "=" *>
             delay parseLambda >>= (fn lambda =>
             pure {var = var, ty = ty, lambda = lambda})))) >>= (fn decs =>
       pure (Fun {tyvars = tyvars, decs = Vector.fromList decs}))),
       kw "val" *>
       Var.parse >>= (fn var =>
       sym ":" *>
       Type.parse >>= (fn ty =>
       sym "=" *>
       delay parsePrimExp >>= (fn exp =>
       pure (MonoVal {var = var, ty = ty, exp = exp})))),
       kw "val" *>
       parseTyvars >>= (fn tyvars =>
       Var.parse >>= (fn var =>
       sym ":" *>
       Type.parse >>= (fn ty =>
       sym "=" *>
       delay parseExp >>= (fn exp =>
       pure (PolyVal {tyvars = tyvars, var = var, ty = ty, exp = exp})))))]
   and parseExp () =
      kw "let" *>
      many (delay parseDec) >>= (fn decs =>
      kw "in" *>
      VarExp.parse >>= (fn result =>
      kw "end" *>
      pure (Exp {decs = decs, result = result})))
   and parsePrimExp () =
      any
      [let
          fun parseCase (parseP, mk) =
             VarExp.parse >>= (fn test =>
             kw "of" *>
             (Vector.fromList <$>
              sepBy (parseP >>= (fn p =>
                     sym "=>" *>
                     delay parseExp >>= (fn e =>
                     pure (p, e))),
                     sym "|")) >>= (fn cases =>
              optional ((if Vector.isEmpty cases then pure () else sym "|") *>
                        kw "_" *> sym "=>" *> delay parseExp) >>= (fn default =>
              pure (Case {test = test,
                          cases = mk cases,
                          default = default}))))
       in
          any ((kw "case" *> parseCase (Pat.parse, Cases.Con)) ::
               (List.map (WordSize.all, fn ws =>
                          (kw ("case" ^ WordSize.toString ws) *>
                           parseCase (WordX.parse, fn cases => (Cases.Word (ws, cases)))))))
       end,
       kw "new" *>
       Con.parse >>= (fn con =>
       parseTargs >>= (fn targs =>
       optional VarExp.parse >>= (fn arg =>
       pure (ConApp {con = con, targs = targs, arg = arg})))),
       Const <$> Const.parse,
       delay parseExp >>= (fn try =>
       kw "handle" *>
       Var.parse >>= (fn var =>
       sym ":" *>
       Type.parse >>= (fn ty =>
       sym "=>" *>
       delay parseExp >>= (fn handler =>
       pure (Handle {try = try, catch = (var, ty), handler = handler}))))),
       Lambda <$> delay parseLambda,
       kw "prim" *>
       Prim.parseFull Type.parse >>= (fn prim =>
       parseTargs >>= (fn targs =>
       parseArgs >>= (fn args =>
       pure (PrimApp {prim = prim, targs = targs, args = args})))),
       kw "raise" *>
       optional (kw "extend") >>= (fn extend =>
       VarExp.parse >>= (fn exn =>
       pure (Raise {extend = Option.isSome extend, exn = exn}))),
       spaces *> char #"#" *>
       (peek (nextSat Char.isDigit) *>
        fromScan (fn getc => Int.scan (StringCvt.DEC, getc))) >>= (fn offset =>
       VarExp.parse >>= (fn tuple =>
       pure (Select {offset = offset, tuple = tuple}))),
       Tuple <$> parseArgs,
       VarExp.parse >>= (fn func =>
       VarExp.parse >>= (fn arg =>
       pure (App {func = func, arg = arg}))),
       Var <$> VarExp.parse]
   and parseLambda () =
      kw "fn" *>
      optional (kw "noinline") >>= (fn noInline =>
      Var.parse >>= (fn arg =>
      sym ":" *>
      Type.parse >>= (fn argType =>
      sym "=>" *>
      delay parseExp >>= (fn body =>
      pure (Lam {mayInline = Option.isNone noInline,
                 arg = arg, argType = argType,
                 body = body,
                 plist = PropertyList.new ()})))))
end

structure Dec =
   struct
      type exp = exp
      datatype t = datatype dec

      val layout = layoutDec
   end

structure PrimExp =
   struct
      type exp = exp
      datatype t = datatype primExp

      val layout = layoutPrimExp
   end

structure Exp =
   struct
      datatype t = datatype exp

      val layout = layoutExp
      val parse = parseExp ()

      val make = Exp
      fun dest (Exp r) = r
      val decs = #decs o dest
      val result = #result o dest

      fun fromPrimExp (exp: PrimExp.t, ty: Type.t): t =
         let val var = Var.newNoname ()
         in Exp {decs = [Dec.MonoVal {var = var, ty = ty, exp = exp}],
                 result = VarExp.mono var}
         end

      local
         fun make f (Exp {decs, result}, d) =
            Exp {decs = f (d, decs),
                 result = result}
      in val prefix = make (op ::)
         val prefixs = make (op @)
      end

      fun enterLeave (e: t, ty: Type.t, si: SourceInfo.t): t =
         let
            datatype z = datatype Dec.t
            datatype z = datatype PrimExp.t
            fun prof f =
               MonoVal {exp = Profile (f si),
                        ty = Type.unit,
                        var = Var.newNoname ()}
            val exn = Var.newNoname ()
            val res = Var.newNoname ()
            val handler =
               make {decs = [prof ProfileExp.Leave,
                             MonoVal {exp = Raise {exn = VarExp.mono exn,
                                                   extend = false},
                                      ty = ty,
                                      var = res}],
                     result = VarExp.mono res}
            val touch =
               if !Control.profile = Control.ProfileCount
                  then
                     let
                        val unit = Var.newNoname ()
                     in
                        [MonoVal {exp = Tuple (Vector.new0 ()),
                                  ty = Type.unit,
                                  var = unit},
                         MonoVal
                         {exp = PrimApp {args = Vector.new1 (VarExp.mono unit),
                                         prim = Prim.touch,
                                         targs = Vector.new1 Type.unit},
                          ty = Type.unit,
                          var = Var.newNoname ()}]
                     end
               else []
            val {decs, result} = dest e
            val decs =
               List.concat [[prof ProfileExp.Enter],
                            touch,
                            decs,
                            [prof ProfileExp.Leave]]
            val try = make {decs = decs, result = result}
         in
            fromPrimExp (Handle {catch = (exn, Type.exn),
                                 handler = handler,
                                 try = try},
                         ty)
         end

      (*------------------------------------*)
      (*              foreach               *)
      (*------------------------------------*)
      fun foreach {exp: t,
                   handleExp: t -> unit,
                   handlePrimExp: Var.t * Type.t * PrimExp.t -> unit,
                   handleBoundVar: Var.t * Tyvar.t vector * Type.t -> unit,
                   handleVarExp: VarExp.t -> unit}: unit =
         let
            fun monoVar (x, t) = handleBoundVar (x, Vector.new0 (), t)
            fun handleVarExps xs = Vector.foreach (xs, handleVarExp)
            fun loopExp e =
               let val {decs, result} = dest e
               in List.foreach (decs, loopDec)
                  ; handleVarExp result
                  ; handleExp e
               end
            and loopPrimExp (x: Var.t, ty: Type.t, e: PrimExp.t): unit =
               (handlePrimExp (x, ty, e)
                ; (case e of
                      Const _ => ()
                    | Var x => handleVarExp x
                    | Tuple xs => handleVarExps xs
                    | Select {tuple, ...} => handleVarExp tuple
                    | Lambda lambda => loopLambda lambda
                    | PrimApp {args, ...} => handleVarExps args
                    | Profile _ => ()
                    | ConApp {arg, ...} => (case arg of
                                               NONE => ()
                                             | SOME x => handleVarExp x)
                    | App {func, arg} => (handleVarExp func
                                          ; handleVarExp arg)
                    | Raise {exn, ...} => handleVarExp exn
                    | Handle {try, catch, handler, ...} =>
                         (loopExp try
                          ; monoVar catch
                          ; loopExp handler)
                    | Case {test, cases, default} =>
                         (handleVarExp test
                          ; Cases.foreach' (cases, loopExp,
                                            fn Pat.T {arg, ...} =>
                                            case arg of
                                               NONE => ()
                                             | SOME x => monoVar x)
                          ; Option.app (default, loopExp))))
            and loopDec d =
               case d of
                  MonoVal {var, ty, exp} =>
                     (monoVar (var, ty); loopPrimExp (var, ty, exp))
                | PolyVal {var, tyvars, ty, exp} =>
                     (handleBoundVar (var, tyvars, ty)
                      ; loopExp exp)
                | Exception _ => ()
                | Fun {tyvars, decs, ...} =>
                     (Vector.foreach (decs, fn {ty, var, ...} =>
                                      handleBoundVar (var, tyvars, ty))
                      ; Vector.foreach (decs, fn {lambda, ...} =>
                                        loopLambda lambda))
            and loopLambda (Lam {arg, argType, body, ...}): unit =
               (monoVar (arg, argType); loopExp body)
         in loopExp exp
         end

      fun ignore _ = ()

      fun foreachPrimExp (e, f) =
         foreach {exp = e,
                  handlePrimExp = f,
                  handleExp = ignore,
                  handleBoundVar = ignore,
                  handleVarExp = ignore}

      fun foreachVarExp (e, f) =
         foreach {exp = e,
                  handlePrimExp = ignore,
                  handleExp = ignore,
                  handleBoundVar = ignore,
                  handleVarExp = f}

      fun foreachBoundVar (e, f) =
         foreach {exp = e,
                  handlePrimExp = ignore,
                  handleExp = ignore,
                  handleBoundVar = f,
                  handleVarExp = ignore}

      fun foreachExp (e, f) =
         foreach {exp = e,
                  handlePrimExp = ignore,
                  handleExp = f,
                  handleBoundVar = ignore,
                  handleVarExp = ignore}
      (* quell unused warning *)
      val _ = foreachExp

      fun hasPrim (e, f) =
         Exn.withEscape
         (fn escape =>
          (foreachPrimExp (e, fn (_, _, e) =>
                           case e of
                              PrimApp {prim, ...} => if f prim then escape true
                                                     else ()
                            | _ => ())
           ; false))

      fun size e =
         let val n: int ref = ref 0
            fun inc () = n := 1 + !n
         in foreachPrimExp (e, fn _ => inc ());
            !n
         end
      val size = Trace.trace ("XmlTree.Exp.size", Layout.ignore, Int.layout) size
      (* quell unused warning *)
      val _ = size

      fun clear (e: t): unit =
         let open PrimExp
            fun clearTyvars ts = Vector.foreach (ts, Tyvar.clear)
            fun clearPat (Pat.T {arg, ...}) =
               case arg of
                  NONE => ()
                | SOME (x, _) => Var.clear x
            fun clearExp e = clearDecs (decs e)
            and clearDecs ds = List.foreach (ds, clearDec)
            and clearDec d =
               case d of
                  MonoVal {var, exp, ...} => (Var.clear var; clearPrimExp exp)
                | PolyVal {var, tyvars, exp, ...} =>
                     (Var.clear var
                      ; clearTyvars tyvars
                      ; clearExp exp)
                | Fun {tyvars, decs} =>
                     (clearTyvars tyvars
                      ; Vector.foreach (decs, fn {var, lambda, ...} =>
                                        (Var.clear var
                                         ; clearLambda lambda)))
                | Exception {con, ...} => Con.clear con
            and clearPrimExp e =
               case e of
                  Lambda l => clearLambda l
                | Case {cases, default, ...} =>
                     (Cases.foreach' (cases, clearExp, clearPat)
                      ; Option.app (default, clearExp))
                | Handle {try, catch, handler, ...} => 
                     (clearExp try
                      ; Var.clear (#1 catch)
                      ; clearExp handler)
                | _ => ()
            and clearLambda (Lam {arg, body, ...}) =
               (Var.clear arg; clearExp body)
         in clearExp e
         end
   end

(*---------------------------------------------------*)
(*                      Lambda                       *)
(*---------------------------------------------------*)

structure Lambda =
   struct
      type exp = exp
      datatype t = datatype lambda

      local
         fun make f (Lam r) = f r
      in
         val arg = make #arg
         val body = make #body
         val mayInline = make #mayInline
      end

      fun make {arg, argType, body, mayInline} =
         Lam {arg = arg,
              argType = argType,
              body = body,
              mayInline = mayInline,
              plist = PropertyList.new ()}

      fun dest (Lam {arg, argType, body, mayInline, ...}) =
         {arg = arg, argType = argType, body = body, mayInline = mayInline}

      fun plist (Lam {plist, ...}) = plist

      val layout = layoutLambda
      fun equals (f:t, f':t) = PropertyList.equals (plist f, plist f')
   end

(* ------------------------------------------------- *)
(*                     DirectExp                     *)
(* ------------------------------------------------- *)
structure DirectExp =
   struct
      open Dec PrimExp

      structure Cont =
         struct
            type t = PrimExp.t * Type.t -> Exp.t

            fun nameGen (k: VarExp.t * Type.t -> Exp.t): t =
               fn (e, t) =>
               case e of
                  Var x => k (x, t)
                | _ => let val x = Var.newNoname ()
                       in Exp.prefix (k (VarExp.mono x, t),
                                      MonoVal {var = x, ty = t, exp = e})
                       end

            fun name (k: VarExp.t * Type.t -> Exp.t): t = nameGen k

            val id: t = name (fn (x, _) => Exp {decs = [], result = x})

            fun return (k: t, xt) = k xt
         end

      type t = Cont.t -> Exp.t

      fun send (e: t, k: Cont.t): Exp.t = e k

      fun toExp e = send (e, Cont.id)

      fun fromExp (Exp {decs, result}, ty): t =
         fn k => Exp.prefixs (k (Var result, ty), decs)

      fun sendName (e, k) = send (e, Cont.name k)

      fun simple (e: PrimExp.t * Type.t) k = Cont.return (k, e)

      fun const c = simple (Const c, Type.ofConst c)

      val string = const o Const.string

      fun varExp (x, t) = simple (Var x, t)

      fun var {var, targs, ty} =
         varExp (VarExp.T {var = var, targs = targs}, ty)

      fun monoVar (x, t) = var {var = x, targs = Vector.new0 (), ty = t}

      fun convertsGen (es: t vector,
                       k: (VarExp.t * Type.t) vector -> Exp.t): Exp.t =
         let
            val n = Vector.length es
            fun loop (i, xs) =
               if i = n
                  then k (Vector.fromListRev xs)
               else sendName (Vector.sub (es, i),
                              fn x => loop (i + 1, x :: xs))
         in loop (0, [])
         end

      fun converts (es: t vector,
                    make: (VarExp.t * Type.t) vector -> PrimExp.t * Type.t): t =
         fn k => convertsGen (es, k o make)

      fun convert (e: t, make: VarExp.t * Type.t -> PrimExp.t * Type.t): t =
         fn k => send (e, Cont.name (k o make))

      fun convertOpt (e, make) =
         case e of
            NONE => simple (make NONE)
          | SOME e => convert (e, make o SOME o #1)

      fun tuple {exps: t vector, ty: Type.t}: t =
         if 1 = Vector.length exps
            then Vector.first exps
         else converts (exps, fn xs =>
                        (PrimExp.Tuple (Vector.map (xs, #1)), ty))

      fun select {tuple, offset, ty} =
         convert (tuple, fn (tuple, _) =>
                  (Select {tuple = tuple, offset = offset}, ty))

      fun conApp {con, targs, arg, ty} =
         convertOpt (arg, fn arg =>
                     (ConApp {con = con, targs = targs, arg = arg}, ty))

      local
         fun make c () = 
            conApp {con = c,
                    targs = Vector.new0 (),
                    arg = NONE,
                    ty = Type.bool}
      in
         val truee = make Con.truee
         val falsee = make Con.falsee
      end

      fun primApp {prim, targs, args, ty} =
         converts (args, fn args =>
                   (PrimApp {prim = prim,
                             targs = targs,
                             args = Vector.map (args, #1)},
                    ty))

      fun convert2 (e1, e2, make) =
         converts (Vector.new2 (e1, e2),
                   fn xs => make (Vector.first xs, Vector.sub (xs, 1)))

      fun app {func, arg, ty} =
         convert2 (func, arg, fn ((func, _), (arg, _)) =>
                   (App {func = func, arg = arg}, ty))

      fun casee {test, cases, default, ty} =
         convert (test, fn (test, _) =>
                  (Case
                   {test = test,
                    cases = Cases.map (cases, toExp),
                    default = Option.map (default, toExp)},
                   ty))

      fun raisee {exn: t, extend: bool, ty: Type.t}: t =
         convert (exn, fn (x, _) => (Raise {exn = x, extend = extend}, ty))

      fun handlee {try, catch, handler, ty} =
         simple (Handle {try = toExp try,
                         catch = catch,
                         handler = toExp handler},
                 ty)

      fun unit () = tuple {exps = Vector.new0 (), ty = Type.unit}

      fun reff (e: t): t =
         convert (e, fn (x, t) =>
                  (PrimApp {prim = Prim.reff,
                            targs = Vector.new1 t,
                            args = Vector.new1 x},
                   Type.reff t))

      fun deref (e: t): t =
         convert (e, fn (x, t) =>
                  let
                     val t = Type.deRef t
                  in
                     (PrimApp {prim = Prim.deref,
                               targs = Vector.new1 t,
                               args = Vector.new1 x},
                      t)
                  end)

      fun vectorLength (e: t): t =
         convert (e, fn (x, t) =>
                  let
                     val t = Type.deVector t
                  in
                     (PrimApp {prim = Prim.vectorLength,
                               targs = Vector.new1 t,
                               args = Vector.new1 x},
                      Type.word (WordSize.seqIndex ()))
                  end)

      fun vectorSub (e1: t, e2: t): t =
         convert2 (e1, e2, fn ((x1, t1), (x2, _)) =>
                   let
                      val t = Type.deVector t1
                   in
                      (PrimApp {prim = Prim.vectorSub,
                                targs = Vector.new1 t,
                                args = Vector.new2 (x1, x2)},
                       t)
                   end)

      fun equal (e1, e2) =
         convert2 (e1, e2, fn ((x1, t), (x2, _)) =>
                   (PrimApp {prim = Prim.equal,
                             targs = Vector.new1 t,
                             args = Vector.new2 (x1, x2)},
                    Type.bool))

      fun iff {test, thenn, elsee, ty} =
         casee {test = test,
                cases = Cases.Con (Vector.new2 ((Pat.truee, thenn),
                                                (Pat.falsee, elsee))),
                default = NONE,
                ty = ty}

      fun vall {var, exp}: Dec.t list =
         let val t = ref Type.unit
            val Exp {decs, result} =
               sendName (exp, fn (x, t') => (t := t';                    
                                             Exp {decs = [], result = x}))
         in decs @ [MonoVal {var = var, ty = !t, exp = Var result}]
         end

      fun sequence es =
         converts (es, fn xs => let val (x, t) = Vector.last xs
                                in (Var x, t)
                                end)

      val bug: string -> t =
         fn s =>
         primApp {prim = Prim.bug,
                  targs = Vector.new0 (),
                  args = Vector.new1 (string s),
                  ty = Type.unit}

      fun seq (es, make) =
         fn k => convertsGen (es, fn xts =>
                              send (make (Vector.map (xts, varExp)), k))

      fun lett {decs, body} = fn k => Exp.prefixs (send (body, k), decs)

      fun let1 {var, exp, body} =
         fn k => 
         send (exp, fn (exp, ty) =>
               Exp.prefix (send (body, k),
                           Dec.MonoVal {var = var, ty = ty, exp = exp}))

      fun lambda {arg, argType, body, bodyType, mayInline} =
         simple (Lambda (Lambda.make {arg = arg,
                                      argType = argType,
                                      body = toExp body,
                                      mayInline = mayInline}),
                 Type.arrow (argType, bodyType))

      fun fromLambda (l, ty) =
         simple (Lambda l, ty)

      fun detupleGen (e: PrimExp.t,
                      t: Type.t,
                      components: Var.t vector,
                      body: Exp.t): Exp.t =
         Exp.prefixs
         (body,
          case Vector.length components of
             0 => []
           | 1 => [MonoVal {var = Vector.first components, ty = t, exp = e}]
           | _ =>
                let
                   val ts = Type.deTuple t
                   val tupleVar = Var.newNoname ()
                in MonoVal {var = tupleVar, ty = t, exp = e}
                   ::
                   #2 (Vector.fold2
                       (components, ts, (0, []),
                        fn (x, t, (i, ac)) =>
                        (i + 1,
                         MonoVal {var = x, ty = t,
                                  exp = Select {tuple = VarExp.mono tupleVar,
                                                offset = i}}
                         :: ac)))
                end)

      fun detupleBind {tuple, components, body} =
         fn k => send (tuple, fn (e, t) => detupleGen (e, t, components, body k))

      fun detuple {tuple: t, body}: t =
         fn k =>
         tuple
         (fn (e, t) =>
          let
             val ts = Type.deTuple t
          in
             case e of
                Tuple xs => send (body (Vector.zip (xs, ts)), k)
              | _ => let
                        val components =
                           Vector.map (ts, fn _ => Var.newNoname ())
                     in
                        detupleGen (e, t, components,
                                    send (body (Vector.map2
                                                (components, ts, fn (x, t) =>
                                                 (VarExp.mono x, t))),
                                          k))
                     end
          end)

      fun devector {vector: t, length: int, body}: t =
         fn k =>
         let
            val es =
               Vector.tabulate
               (length, fn i =>
                vectorSub (vector, const (Const.word (WordX.fromIntInf (IntInf.fromInt i, WordSize.seqIndex ())))))
         in
            convertsGen (es, fn args => (body args) k)
         end
   end

(*---------------------------------------------------*)
(*                     Datatype                      *)
(*---------------------------------------------------*)

structure Datatype =
   struct
      type t = {cons: {arg: Type.t option,
                       con: Con.t} vector,
                tycon: Tycon.t,
                tyvars: Tyvar.t vector}

      fun layout ({cons, tycon, tyvars}: t): Layout.t =
         let
            open Layout
         in
            seq [str "datatype ",
                 layoutTyvars tyvars,
                 Tycon.layout tycon,
                 str " = ",
                 alignPrefix
                 (Vector.toListMap (cons, layoutConArg),
                  "| ")]
         end

      val parse =
         let
            open Parse
         in
            kw "datatype" *>
            parseTyvars >>= (fn tyvars =>
            Tycon.parse >>= (fn tycon =>
            sym "=" *>
            sepBy (parseConArg, sym "|") >>= (fn cons =>
            pure {tyvars = tyvars, tycon = tycon, cons = Vector.fromList cons})))
         end
   end

(*---------------------------------------------------*)
(*                      Program                      *)
(*---------------------------------------------------*)

structure Program =
   struct
      datatype t = T of {body: Exp.t,
                         datatypes: Datatype.t vector,
                         overflow: Var.t option}

      fun layout (T {body, datatypes, overflow, ...}) =
         let
            open Layout
         in
            align [str "\n\nDatatypes:",
                   align (Vector.toListMap (datatypes, Datatype.layout)),
                   seq [str "\n\nOverflow: ", Option.layout Var.layout overflow],
                   str "\n\nBody:",
                   Exp.layout body]
         end

      fun layouts (T {body, datatypes, overflow, ...}, output') =
         let
            open Layout
            (* Layout includes an output function, so we need to rebind output
             * to the one above.
             *)
            val output = output'
         in
            output (str "\n\n(* Datatypes: *)")
            ; Vector.foreach (datatypes, output o Datatype.layout)
            ; output (seq [str "\n\n(* Overflow: *) ", Option.layout Var.layout overflow])
            ; output (str "\n\n(* Body: *)")
            ; output (Exp.layout body)
         end

      fun parse () =
         let
            open Parse

            val () = Tyvar.parseReset {prims = Vector.new0 ()}
            val () = Tycon.parseReset {prims = Vector.fromListMap (Tycon.prims, #tycon)}
            val () = Con.parseReset {prims = Vector.new4 (Con.truee, Con.falsee, Con.overflow, Con.reff)}
            val () = Var.parseReset {prims = Vector.new0 ()}

            val parseProgram =
               many Datatype.parse >>= (fn datatypes =>
               option Var.parse >>= (fn overflow =>
               Exp.parse >>= (fn body =>
               pure (T {datatypes = Vector.fromList datatypes,
                        overflow = overflow,
                        body = body}))))

            fun finiComment n () =
               any
               [str "(*" *> delay (finiComment (n + 1)),
                str "*)" *> (if n = 1 then pure [Char.space] else delay (finiComment (n - 1))),
                next *> delay (finiComment n)]

            val skipComments =
               any
               [str "(*" *> finiComment 1 (),
                each [next]]
         in
            compose (skipComments, parseProgram <* (spaces *> (failing next <|> failCut "end of file")))
         end

      fun clear (T {datatypes, body, ...}) =
         (Vector.foreach (datatypes, fn {tycon, tyvars, cons} =>
                          (Tycon.clear tycon
                           ; Vector.foreach (tyvars, Tyvar.clear)
                           ; Vector.foreach (cons, Con.clear o #con)))
          ; Exp.clear body)

      fun layoutStats (T {datatypes, body, ...}) =
         let
            val numTypes = ref 0
            fun inc _ = numTypes := 1 + !numTypes
            val {hom, destroy} = Type.makeHom {var = inc, con = inc}
            val numPrimExps = ref 0
            open Layout
         in
            Vector.foreach (datatypes, fn {cons, ...} =>
                            Vector.foreach (cons, fn {arg, ...} =>
                                            case arg of
                                               NONE => ()
                                             | SOME t => hom t))
            ; (Exp.foreach
               {exp = body,
                handlePrimExp = fn _ => numPrimExps := 1 + !numPrimExps,
                handleVarExp = fn _ => (),
                handleBoundVar = hom o #3,
                handleExp = fn _ => ()})
            ; destroy ()
            ; align [seq [str "num primexps in program = ", Int.layout (!numPrimExps)],
                     seq [str "num types in program = ", Int.layout (!numTypes)],
                     Type.stats ()]
         end
   end

end
