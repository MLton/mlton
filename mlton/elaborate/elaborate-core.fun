(* Copyright (C) 2009-2012,2015,2017 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor ElaborateCore (S: ELABORATE_CORE_STRUCTS): ELABORATE_CORE =
struct

open S

local
   open Control.Elaborate
in
   val nonexhaustiveBind = fn () => current nonexhaustiveBind
   val nonexhaustiveExnBind = fn () => current nonexhaustiveExnBind
   val nonexhaustiveExnMatch = fn () => current nonexhaustiveExnMatch
   val nonexhaustiveExnRaise = fn () => current nonexhaustiveExnRaise
   val nonexhaustiveMatch = fn () => current nonexhaustiveMatch
   val nonexhaustiveRaise = fn () => current nonexhaustiveRaise
   val redundantBind = fn () => current redundantBind
   val redundantMatch = fn () => current redundantMatch
   val redundantRaise = fn () => current redundantRaise
   val resolveScope = fn () => current resolveScope
   val sequenceNonUnit = fn () => current sequenceNonUnit
   val valrecConstr = fn () => current valrecConstr
   fun check (c: (bool,bool) t, keyword: string, region) =
      if current c
         then ()
      else
         let
            open Layout
         in
            Control.error
            (region,
             str (concat (if expert c
                             then [keyword, " disallowed"]
                             else [keyword, " disallowed, compile with -default-ann '",
                                   name c, " true'"])),
             empty)
         end
end
structure ElabControl = Control.Elaborate


local
   open Layout
in
   val align = align
   val empty = empty
   val seq = seq
   val str = str
end

fun approximateN (l: Layout.t, prefixMax, suffixMax): Layout.t =
   let
      val s = Layout.toString l
      val n = String.size s
   in
      str
      (case suffixMax of
          NONE =>
             if n <= prefixMax
                then s
             else concat [String.prefix (s, prefixMax - 5), "  ..."]
        | SOME suffixMax =>
             if n <= prefixMax + suffixMax
                then s
             else concat [String.prefix (s, prefixMax - 2),
                          "  ...  ",
                          String.suffix (s, suffixMax - 5)])
   end
fun approximate (l: Layout.t): Layout.t =
   approximateN (l, 35, SOME 25)
fun approximatePrefix (l: Layout.t): Layout.t =
   approximateN (l, 15, NONE)

local
   open Ast
in
   structure Aconst = Const
   structure Adec = Dec
   structure Aexp = Exp
   structure Amatch = Match
   structure Apat = Pat
   structure Atype = Type
   structure Avar = Var
   structure Avid = Vid
   structure DatatypeRhs = DatatypeRhs
   structure DatBind = DatBind
   structure EbRhs = EbRhs
   structure Fixop = Fixop
   structure Longtycon = Longtycon
   structure Longvid = Longvid
   structure PrimKind = PrimKind
   structure ImportExportAttribute = PrimKind.ImportExportAttribute
   structure SymbolAttribute = PrimKind.SymbolAttribute
   structure Priority = Priority
   structure Record = Record
   structure SortedRecord = SortedRecord
   structure Symbol = Symbol
   structure TypBind = TypBind
end

local
   open Env
in
   structure Kind = Kind
   structure TypeEnv = TypeEnv
   structure TypeStr = TypeStr
   structure TyvarEnv = TyvarEnv
   structure Vid = Vid
end

local
   open TypeEnv
in
   structure Scheme = Scheme
   structure Time = Time
   structure Type = Type
end

local
   open CoreML
in
   structure CFunction = CFunction
   structure CType = CType
   structure CharSize = CharSize
   structure Convention  = CFunction.Convention
   structure SymbolScope  = CFunction.SymbolScope
   structure CKind = CFunction.Kind
   structure Con = Con
   structure Const = Const
   structure ConstType = Const.ConstType
   structure Cdec = Dec
   structure Cexp = Exp
   structure Ffi = Ffi
   structure IntSize = IntSize
   structure Lambda = Lambda
   structure Cpat = Pat
   structure Prim = Prim
   structure RealSize = RealSize
   structure RealX = RealX
   structure SourceInfo = SourceInfo
   structure Tycon = Tycon
   structure Tyvar = Tyvar
   structure Var = Var
   structure WordSize = WordSize
   structure WordX = WordX
   structure WordXVector = WordXVector
end
structure Tycon =
   struct
      open Tycon
      open TypeEnv.TyconExt
   end
structure Tyvar =
   struct
      open Tyvar
      open TypeEnv.TyvarExt
   end

fun matchDiagsFromNoMatch noMatch =
   case noMatch of
      Cexp.Impossible =>
         {nonexhaustiveExn = Control.Elaborate.DiagDI.Default,
          nonexhaustive = Control.Elaborate.DiagEIW.Ignore,
          redundant = Control.Elaborate.DiagEIW.Ignore}
    | Cexp.RaiseAgain =>
         {nonexhaustiveExn = nonexhaustiveExnRaise (),
          nonexhaustive = nonexhaustiveRaise (),
          redundant = redundantRaise ()}
    | Cexp.RaiseBind =>
         {nonexhaustiveExn = nonexhaustiveExnBind (),
          nonexhaustive = nonexhaustiveBind (),
          redundant = redundantBind ()}
    | Cexp.RaiseMatch =>
         {nonexhaustiveExn = nonexhaustiveExnMatch (),
          nonexhaustive = nonexhaustiveMatch (),
          redundant = redundantMatch ()}

structure AdmitsEquality = Tycon.AdmitsEquality

local
   open Record
in
   structure Field = Field
end

structure Parse = PrecedenceParse (structure Ast = Ast
                                   structure Env = Env)

structure Scope = Scope (structure Ast = Ast)

structure Apat =
   struct
      open Apat

      fun getName (p: t): string option =
         case node p of
            Var {name, ...} => SOME (Longvid.toString name)
          | Constraint (p, _) => getName p
          | FlatApp v =>
               if 1 = Vector.length v
                  then getName (Vector.first v)
               else NONE
          | Layered {var, ...} => SOME (Avar.toString var)
          | _ => NONE

      val getName =
         Trace.trace ("ElaborateCore.Apat.getName", layout, Option.layout String.layout)
         getName
   end

fun elaborateType (ty: Atype.t, E: Env.t,
                   {bogusAsUnknown: bool}): Type.t =
   let
      fun makeBogus (mc, ts) =
         if bogusAsUnknown
            then Type.new ()
            else let
                    val arity = Vector.length ts
                    val (name, region) =
                       Option.fold
                       (mc, ("t", NONE), fn (c, _) =>
                        (Longtycon.toString c,
                         SOME (Longtycon.region c)))
                    val c =
                       Tycon.makeBogus
                       {name = name,
                        kind = Kind.Arity arity,
                        region = region}
                 in
                    Type.con (c, ts)
                 end
      fun loop (ty: Atype.t): Type.t =
         case Atype.node ty of
            Atype.Var a => (* rule 44 *)
               (case TyvarEnv.lookupTyvar a of
                   NONE => makeBogus (NONE, Vector.new0 ())
                 | SOME a => Type.var a)
          | Atype.Con (c, ts) => (* rules 46, 47 *)
               let
                  val ts = Vector.map (ts, loop)
                  fun normal () =
                     case Env.lookupLongtycon (E, c) of
                        NONE => makeBogus (SOME c, ts)
                      | SOME s =>
                           let
                              val kind = TypeStr.kind s
                              val numArgs = Vector.length ts
                              val ts =
                                 case kind of
                                    Kind.Arity n =>
                                       let
                                          fun error () =
                                             let
                                                open Layout
                                                fun doit n =
                                                   seq [str "[",
                                                        case n of
                                                           0 => empty
                                                         | 1 => str "_"
                                                         | _ => seq [str "(",
                                                                     (seq o separate)
                                                                     (List.tabulate (n, fn _ => str "_"),
                                                                      ", "),
                                                                     str ")"],
                                                        str "] ",
                                                        Ast.Longtycon.layout c]
                                             in
                                                Control.error
                                                (Atype.region ty,
                                                 seq [str "type constructor applied to incorrect number of type arguments: ",
                                                      Ast.Longtycon.layout c],
                                                 align [seq [str "expects: ", doit n],
                                                        seq [str "but got: ", doit numArgs],
                                                        seq [str "in: ", Atype.layout ty]])
                                             end
                                       in
                                          case Int.compare (n, numArgs) of
                                             LESS =>
                                                (error (); Vector.prefix (ts, n))
                                           | EQUAL => ts
                                           | GREATER =>
                                                (error ()
                                                 ; Vector.concat
                                                   [ts,
                                                    Vector.tabulate
                                                    (n - numArgs, fn _ =>
                                                     makeBogus
                                                     (NONE, Vector.new0 ()))])
                                       end
                                  | Kind.Nary => ts
                           in
                              TypeStr.apply (s, ts)
                           end
               in
                  case (Ast.Longtycon.split c, Vector.length ts) of
                     (([], c), 2) =>
                        if Ast.Tycon.equals (c, Ast.Tycon.arrow)
                           then Type.arrow (Vector.sub (ts, 0),
                                            Vector.sub (ts, 1))
                        else normal ()
                   | _ => normal ()
               end
          | Atype.Paren t => loop t
          | Atype.Record r => (* rules 45, 49 *)
               Type.record
               (SortedRecord.fromVector
                (Vector.map
                 (Record.toVector r,
                  fn (f, (_, t)) => (f, loop t))))
   in
      loop ty
   end

val overloadChecks: (Ast.Priority.t * (unit -> unit)) list ref = ref []

fun resolveOverloads () =
   (* List.insertionSort is anti-stable; hence, it sorts and reverses the overloads. *)
   (List.foreach (List.insertionSort
                  (!overloadChecks, fn ((p1,_),(p2,_)) =>
                   Priority.<= (p2, p1)),
                  fn (_,th) => th ())
    ; overloadChecks := [])

val unresolvedFlexRecordChecks: (unit -> unit) list ref = ref []

fun reportUnresolvedFlexRecords () =
   (List.foreach (rev (!unresolvedFlexRecordChecks), fn th => th ())
    ; unresolvedFlexRecordChecks := [])

val undeterminedTypeChecks: (unit -> unit) list ref = ref []

fun reportUndeterminedTypes () =
   (List.foreach (rev (!undeterminedTypeChecks), fn th => th ())
    ; undeterminedTypeChecks := [])

val sequenceNonUnitChecks: (unit -> unit) list ref = ref []

fun reportSequenceNonUnit () =
   (List.foreach (rev (!sequenceNonUnitChecks), fn th => th ())
    ; sequenceNonUnitChecks := [])

val {hom = typeTycon: Type.t -> Tycon.t option, ...} =
   Type.makeHom {con = fn (c, _) => SOME c,
                 expandOpaque = false,
                 var = fn _ => NONE}

val typeTycon =
   Trace.trace
   ("ElaborateCore.typeTycon", Type.layout, Option.layout Tycon.layout)
   typeTycon

fun 'a elabConst (c: Aconst.t,
                  {layoutPrettyType: Type.t -> Layout.t},
                  make: (unit -> Const.t) * Type.t -> 'a,
                  {false = f: 'a, true = t: 'a}): 'a =
   let
      fun error (kind: string, ty: Type.t): unit =
         Control.error
         (Aconst.region c,
          seq [str kind, str " too large for type: ", Aconst.layout c],
          seq [str "type: ", layoutPrettyType ty])
      fun choose (tycon, all, sizeTycon, make) =
         case List.peek (all, fn s => Tycon.equals (tycon, sizeTycon s)) of
            NONE => Const.string "<bogus>"
          | SOME s => make s
      fun delay (ty: unit -> Type.t, resolve: Type.t -> Const.t): 'a =
         let
            val ty = ty ()
            val resolve = Promise.lazy (fn () => resolve ty)
            val _ = List.push (overloadChecks, (Priority.default, ignore o resolve))
         in
            make (resolve, ty)
         end
      val typeTycon =
         fn ty =>
         case typeTycon ty of
            NONE => Tycon.bogus
          | SOME c => c
   in
      case Aconst.node c of
         Aconst.Bool b => if b then t else f
       | Aconst.Char ch =>
            delay
            (Type.unresolvedChar, fn ty =>
             choose (typeTycon ty,
                     CharSize.all,
                     Tycon.word o WordSize.fromBits o CharSize.bits,
                     fn cs =>
                     let
                        val ws = WordSize.fromBits (CharSize.bits cs)
                     in
                        Const.Word
                        (if CharSize.isInRange (cs, ch)
                            then WordX.fromIntInf (ch, ws)
                            else (error ("char constant", ty); WordX.zero ws))
                     end))
       | Aconst.Int i =>
            delay
            (Type.unresolvedInt, fn ty =>
             let
                val tycon = typeTycon ty
             in
                if Tycon.equals (tycon, Tycon.intInf)
                   then Const.IntInf i
                else
                   choose (tycon, WordSize.all, Tycon.word, fn s =>
                           Const.Word
                           (if WordSize.isInRange (s, i, {signed = true})
                               then WordX.fromIntInf (i, s)
                            else (error ("int constant", ty); WordX.zero s)))
             end)
       | Aconst.Real r =>
            delay
            (Type.unresolvedReal, fn ty =>
             choose (typeTycon ty, RealSize.all, Tycon.real, fn s =>
                     Const.Real (case RealX.make (r, s) of
                                    NONE => (error ("real constant", ty); RealX.zero s)
                                  | SOME r => r)))
       | Aconst.String v =>
            delay
            (Type.unresolvedString, fn ty =>
             choose (typeTycon (Type.deVector ty),
                     CharSize.all,
                     Tycon.word o WordSize.fromBits o CharSize.bits,
                     fn cs =>
                     let
                        val ws = WordSize.fromBits (CharSize.bits cs)
                        val bigs = ref []
                        val wv =
                           Const.WordVector
                           (WordXVector.tabulate
                            ({elementSize = ws}, Vector.length v, fn i =>
                             let
                                val ch = Vector.sub (v, i)
                             in
                                if CharSize.isInRange (cs, ch)
                                   then WordX.fromIntInf (ch, ws)
                                   else (List.push (bigs, ch)
                                         ; WordX.zero ws)
                             end))
                        val () =
                           if List.isEmpty (!bigs)
                              then ()
                              else Control.error
                                   (Aconst.region c,
                                    seq [str "string constant with ",
                                         str (case !bigs of
                                                 [_] => "character "
                                               | _ => "characters "),
                                         str "too large for type: ",
                                         seq (Layout.separate
                                              (List.revMap
                                               (!bigs, fn ch =>
                                                Aconst.layout (Aconst.makeRegion (Aconst.Char ch, Region.bogus))),
                                               ", "))],
                                    seq [str "type: ", layoutPrettyType ty])
                     in
                        wv
                     end))
       | Aconst.Word w =>
            delay
            (Type.unresolvedWord, fn ty =>
             choose (typeTycon ty, WordSize.all, Tycon.word, fn s =>
                     Const.Word
                     (if WordSize.isInRange (s, w, {signed = false})
                         then WordX.fromIntInf (w, s)
                      else (error ("word constant", ty); WordX.zero s))))
   end

local
fun unifySeq (seqTy, seqStr,
              trs: (Type.t * Region.t) vector,
              unify): Type.t =
   if Vector.isEmpty trs
      then seqTy (Type.new ())
   else
      let
         val (t, _) = Vector.first trs
         val _ =
            Vector.foreach
            (trs, fn (t', r) =>
             unify (t, t', fn (l, l') =>
                    (r,
                     str (seqStr ^ " with element of different type"),
                     align [seq [str "element:  ", l'],
                            seq [str "previous: ", l]])))
      in
         seqTy t
      end
in
fun unifyList (trs: (Type.t * Region.t) vector, unify): Type.t =
   unifySeq (Type.list, "list", trs, unify)
fun unifyVector (trs: (Type.t * Region.t) vector, unify): Type.t =
   unifySeq (Type.vector, "vector", trs, unify)
end

val elabPatInfo = Trace.info "ElaborateCore.elabPat"

structure Var =
   struct
      open Var

      val fromAst = newString o Avar.toString
   end

structure DiagUtils =
   struct
      type t = {layoutPrettyType: Type.t -> LayoutPretty.t,
                layoutPrettyTycon: Tycon.t -> Layout.t,
                layoutPrettyTyvar: Tyvar.t -> Layout.t,
                unify: Type.t * Type.t * (Layout.t * Layout.t -> Region.t * Layout.t * Layout.t) -> unit}
      fun make E : t =
         let
            val {layoutPrettyTycon, ...} =
               Env.makeLayoutPrettyTycon (E, {prefixUnset = true})
            val {layoutPretty = layoutPrettyTyvar, ...} =
               TyvarEnv.makeLayoutPretty ()
            val layoutPrettyType = fn t =>
               Type.layoutPretty
               (t, {expandOpaque = false,
                    layoutPrettyTycon = layoutPrettyTycon,
                    layoutPrettyTyvar = layoutPrettyTyvar})
            fun unify (t, t', error) =
               let
                  val error = fn (l, l', {notes}) =>
                     let
                        val (r, m, d) = error (l, l')
                     in
                        Control.error
                        (r, m, align [d, notes ()])
                     end
               in
                  Type.unify
                  (t, t', {error = error,
                           layoutPretty = layoutPrettyType,
                           layoutPrettyTycon = layoutPrettyTycon,
                           layoutPrettyTyvar = layoutPrettyTyvar})
               end
         in
            {layoutPrettyType = layoutPrettyType,
             layoutPrettyTycon = layoutPrettyTycon,
             layoutPrettyTyvar = layoutPrettyTyvar,
             unify = unify}
         end
   end

val elaboratePat:
   unit
   -> Apat.t * Env.t * {bind: bool, isRvb: bool}
   -> Cpat.t * (Avar.t * Var.t * Type.t) vector =
   fn () =>
   let
      val others: (Apat.t * (Avar.t * Var.t * Type.t) vector) list ref = ref []
   in
      fn (p: Apat.t, E: Env.t, {bind = bindInEnv, isRvb}) =>
      let
         val {layoutPrettyType, unify, ...} = DiagUtils.make E
         fun ctxtTop () =
            seq [str "in: ", approximate (Apat.layout p)]
         val rename =
            let
               val renames: (Avar.t * Var.t) list ref = ref []
            in
               fn x =>
               case List.peek (!renames, fn (y, _) => Avar.equals (x, y)) of
                  NONE => let val x' = Var.fromAst x
                          in (List.push (renames, (x, x')); x')
                          end
                | SOME (_, x') => x'
            end
         val xts: (Avar.t * Var.t * Type.t) list ref = ref []
         fun bindToType (x: Avar.t, t: Type.t): Var.t =
            let
               val _ =
                  Avid.checkRedefineSpecial
                  (Avid.fromVar x,
                   {allowIt = true,
                    ctxt = ctxtTop,
                    keyword = if isRvb then "val rec" else "pattern"})
               val x' = rename x
               val () =
                  case List.peek (!xts, fn (y, _, _) => Avar.equals (x, y)) of
                     NONE => ()
                   | SOME _ =>
                        Control.error
                        (Avar.region x,
                         seq [str "duplicate variable in pattern: ", Avar.layout x],
                         ctxtTop ())
               val _ = List.push (xts, (x, x', t))
            in
               x'
            end
         fun bind (x: Avar.t): Var.t * Type.t =
            let
               val t = Type.new ()
            in
               (bindToType (x, t), t)
            end
         fun elabType (t: Atype.t): Type.t =
            elaborateType (t, E, {bogusAsUnknown = true})
         fun loop (arg: Apat.t) =
            Trace.traceInfo' (elabPatInfo, Apat.layout, Cpat.layout)
            (fn (p: Apat.t) =>
             let
                val region = Apat.region p
                fun ctxt () =
                   seq [str "in: ", approximate (Apat.layout p)]
                val unify = fn (a, b, f) =>
                   unify (a, b, fn z =>
                          let
                             val (r, m, d) = f z
                          in
                             (r, m, align [d, ctxt ()])
                          end)
                fun unifyPatternConstraint (p, c) =
                   unify
                   (p, c, fn (l1, l2) =>
                    (region,
                     str "pattern and constraint disagree",
                     align [seq [str "pattern:    ", l1],
                            seq [str "constraint: ", l2]]))
                fun dontCare () =
                   Cpat.wild (Type.new ())
             in
                case Apat.node p of
                   Apat.App (c, p) =>
                      (case Env.lookupLongcon (E, c) of
                          NONE => dontCare ()
                        | SOME (con, s) =>
                             let
                                val {args, instance} = Scheme.instantiate s
                                val args = args ()
                                val p = loop p
                                val (argType, resultType) =
                                   case Type.deArrowOpt instance of
                                      SOME types => types
                                    | NONE =>
                                         let
                                            val types =
                                               (Type.new (), Type.new ())
                                            val _ =
                                               unify
                                               (instance, Type.arrow types,
                                                fn _ =>
                                                (region,
                                                 str "constant constructor applied to argument in pattern",
                                                 Layout.empty))
                                         in
                                            types
                                         end
                                val _ =
                                   unify
                                   (Cpat.ty p, argType, fn (l, l') =>
                                    (region,
                                     str "constructor applied to incorrect argument in pattern",
                                     align [seq [str "expects: ", l'],
                                            seq [str "but got: ", l]]))
                             in
                                Cpat.make (Cpat.Con {arg = SOME p,
                                                     con = con,
                                                     targs = args},
                                           resultType)
                             end)
                 | Apat.Const c =>
                      elabConst
                      (c,
                       {layoutPrettyType = #1 o layoutPrettyType},
                       fn (resolve, ty) => Cpat.make (Cpat.Const resolve, ty),
                       {false = Cpat.falsee,
                        true = Cpat.truee})
                 | Apat.Constraint (p, t) =>
                      let
                         val p' = loop p
                         val _ =
                            unifyPatternConstraint
                            (Cpat.ty p', elabType t)
                      in
                         p'
                      end
                 | Apat.FlatApp items =>
                      loop (Parse.parsePat
                            (items, E, fn () => ctxt ()))
                 | Apat.Layered {var = x, constraint, pat, ...} =>
                      let
                         val t =
                            case constraint of
                               NONE => Type.new ()
                             | SOME t => elabType t
                         val xc = Avid.toCon (Avid.fromVar x)
                         val x =
                            case Env.peekLongcon (E, Ast.Longcon.short xc) of
                               NONE => bindToType (x, t)
                             | SOME _ =>
                                  let
                                     val _ =
                                        Control.error
                                        (region,
                                         seq [str "constructor cannot be redefined by as: ",
                                              Avar.layout x],
                                         ctxt ())
                                  in
                                     Var.fromAst x
                                  end
                         val pat' = loop pat
                         val _ =
                            unifyPatternConstraint (Cpat.ty pat', t)
                      in
                         Cpat.make (Cpat.Layered (x, pat'), t)
                      end
                 | Apat.List ps =>
                      let
                         val ps' = Vector.map (ps, loop)
                      in
                         Cpat.make (Cpat.List ps',
                                    unifyList
                                    (Vector.map2 (ps, ps', fn (p, p') =>
                                                  (Cpat.ty p', Apat.region p)),
                                     unify))
                      end
                 | Apat.Or ps =>
                      let
                         val _ = check (Control.Elaborate.allowOrPats, "Or patterns", region)
                         val xtsOrig = !xts
                         val n = Vector.length ps
                         val ps =
                            Vector.map
                            (ps, fn p =>
                             let
                                val _ = xts := []
                                val p' = loop p
                             in
                                (p, p', !xts)
                             end)
                         val ps' = Vector.map (ps, fn (_, p', _) => p')

                         val xtsPats =
                            Vector.fold
                            (ps, [], fn ((p, _, xtsPat), xtsPats) =>
                             List.fold
                             (xtsPat, xtsPats, fn ((x, x', t), xtsPats) =>
                              case List.peek (xtsPats, fn (y, _, _, _) => Avar.equals (x, y)) of
                                 NONE => (x, x', t, ref [x])::xtsPats
                               | SOME (_, _, t', l) =>
                                    let
                                       val _ = List.push (l, x)
                                       val _ =
                                          unify
                                          (t', t, fn (l', l) =>
                                           (Avar.region x,
                                            seq [str "or-pattern with variable of different type: ",
                                                 Avar.layout x],
                                            align [seq [str "variable: ", l],
                                                   seq [str "previous: ", l'],
                                                   seq [str "in: ", approximate (Apat.layout p)]]))
                                    in
                                       xtsPats
                                    end))
                         val _ =
                            List.foreach
                            (xtsPats, fn (x, _, _, l) =>
                             if List.length (!l) <> n
                                then let
                                        val _ =
                                           Control.error
                                           (Apat.region p,
                                            seq [str "variable does not occur in all patterns of or-pattern: ",
                                                 Avar.layout x],
                                            ctxt ())
                                     in
                                        ()
                                     end
                             else ())
                         val t = Type.new ()
                         val _ =
                            Vector.foreach
                            (ps, fn (p, p', _) =>
                             unify
                             (t, Cpat.ty p', fn (l, l') =>
                              (Apat.region p,
                               str "or-pattern with pattern of different type",
                               align [seq [str "pattern:  ", l'],
                                      seq [str "previous: ", l],
                                      seq [str "in: ", approximate (Apat.layout p)]])))
                         val xtsMerge =
                            List.fold
                            (xtsPats, xtsOrig, fn ((x, x', t, l), xtsMerge) =>
                             case List.peek (xtsMerge, fn (y, _, _) => Avar.equals (x, y)) of
                                NONE => (x, x', t)::xtsMerge
                              | SOME _ =>
                                   let
                                      val _ =
                                         List.foreach
                                         (List.rev (!l), fn x =>
                                          Control.error
                                          (Avar.region x,
                                           seq [str "duplicate variable in pattern: ", Avar.layout x],
                                           ctxtTop ()))
                                   in
                                      (x, x', t)::xtsMerge
                                   end)
                         val _ = xts := xtsMerge
                      in
                         Cpat.make (Cpat.Or ps', t)
                      end
                 | Apat.Paren p => loop p
                 | Apat.Record {flexible, items} =>
                      (* rules 36, 38, 39 and Appendix A, p.57 *)
                      let
                         val (fs, ps) =
                            Vector.unzip
                            (Vector.map
                             (items,
                              fn (f, _, i) =>
                              (f,
                               case i of
                                  Apat.Item.Field p => p
                                | Apat.Item.Vid (vid, tyo, po) =>
                                     let
                                        val p =
                                           case po of
                                              NONE =>
                                                 Apat.longvid (Longvid.short vid)
                                            | SOME p =>
                                                 Apat.layered
                                                 {fixop = Fixop.None,
                                                  var = Ast.Vid.toVar vid,
                                                  constraint = NONE,
                                                  pat = p}
                                     in
                                        case tyo of
                                           NONE => p
                                         | SOME ty => Apat.constraint (p, ty)
                                     end)))
                         val ps = Vector.map (ps, loop)
                         val r = SortedRecord.zip (fs, Vector.map (ps, Cpat.ty))
                         val ty =
                            if flexible
                               then
                                  let
                                     val (t, isResolved) = Type.flexRecord r
                                     fun resolve () =
                                        if isResolved ()
                                           then ()
                                        else
                                           Control.error
                                           (region,
                                            str "unresolved ... in record pattern",
                                            ctxt ())
                                     val _ = List.push (unresolvedFlexRecordChecks, resolve)
                                  in
                                     t
                                  end
                            else
                               Type.record r
                      in
                         Cpat.make
                         (Cpat.Record (Record.fromVector (Vector.zip (fs, ps))),
                          ty)
                      end
                 | Apat.Tuple ps =>
                      Cpat.tuple (Vector.map (ps, loop))
                 | Apat.Var {name, ...} =>
                      let
                         val (strids, x) = Ast.Longvid.split name
                         fun var () =
                            let
                               val (x, t) = bind (Ast.Vid.toVar x)
                            in
                               Cpat.make (Cpat.Var x, t)
                            end
                      in
                         case Env.peekLongcon (E, Ast.Longvid.toLongcon name) of
                            NONE =>
                               if List.isEmpty strids
                                  then var ()
                               else
                                  let
                                     val _ =
                                        Control.error
                                        (region,
                                         seq [str "undefined constructor: ",
                                              Ast.Longvid.layout name],
                                         empty)
                                  in
                                     Cpat.make (Cpat.Wild, Type.new ())
                                  end
                          | SOME (c, s) =>
                               if List.isEmpty strids andalso isRvb
                                  then var ()
                               else let
                                       val {args, instance} =
                                          Scheme.instantiate s
                                    in
                                       if Type.isArrow instance
                                          then
                                             (Control.error
                                              (region,
                                               seq [str "constructor used without argument in pattern: ",
                                                    Ast.Longvid.layout name],
                                               empty)
                                              ; dontCare ())
                                       else
                                          Cpat.make
                                          (Cpat.Con {arg = NONE,
                                                     con = c,
                                                     targs = args ()},
                                           instance)
                                    end
                      end
                 | Apat.Vector ps =>
                      let
                         val _ = check (ElabControl.allowVectorPats, "Vector patterns", Apat.region p)
                         val ps' = Vector.map (ps, loop)
                      in
                         Cpat.make (Cpat.Vector ps',
                                    unifyVector
                                    (Vector.map2 (ps, ps', fn (p, p') =>
                                                  (Cpat.ty p', Apat.region p)),
                                     unify))
                      end
                 | Apat.Wild =>
                      Cpat.make (Cpat.Wild, Type.new ())
             end) arg
         val p' = loop p
         val xts = Vector.fromList (!xts)
         val _ =
            Vector.foreach
            (xts, fn (x, _, _) =>
             case (List.peekMap
                   (!others, fn (p, v) =>
                    if Vector.exists (v, fn (y, _, _) =>
                                      Avar.equals (x, y))
                       then SOME p
                    else NONE)) of
                NONE => ()
              | SOME p' =>
                   Control.error
                   (Avar.region x,
                    seq [str "variable bound in multiple patterns: ",
                         Avar.layout x],
                    align [seq [str "pattern:  ",
                                approximate (Apat.layout p)],
                           seq [str "previous: ",
                                approximate (Apat.layout p')]]))
         val _ = List.push (others, (p, xts))
         val _ =
            if bindInEnv
               then Vector.foreach
                    (xts, fn (x, x', t) =>
                     Env.extendVar (E, x, x', Scheme.fromType t,
                                    {isRebind = false}))
            else ()
      in
         (p', xts)
      end
   end

(*---------------------------------------------------*)
(*                   Declarations                    *)
(*---------------------------------------------------*)

structure Nest =
   struct
      type t = string list

      val layout = List.layout String.layout
   end

val elabDecInfo = Trace.info "ElaborateCore.elabDec"
val elabExpInfo = Trace.info "ElaborateCore.elabExp"

structure Type =
   struct
      open Type

      val nullary: (string * CType.t * Tycon.t) list =
         let
            fun sized (tycon: Bits.t -> Tycon.t, ctypes) =
               List.map
               (ctypes, fn cty =>
                let
                   val c = tycon (Bytes.toBits (CType.size cty))
                   val s = Tycon.toString c
                   val s =
                      CharVector.tabulate
                      (String.size s, fn i =>
                       let
                          val c = String.sub (s, i)
                       in
                          if i = 0 then Char.toUpper c else c
                       end)
                in
                   (s, cty, c)
                end)
         in
            [("Bool", CType.bool, Tycon.bool),
             ("CPointer", CType.cpointer, Tycon.cpointer),
             ("Real32", CType.real RealSize.R32, Tycon.real RealSize.R32),
             ("Real64", CType.real RealSize.R64, Tycon.real RealSize.R64),
             ("Thread", CType.thread, Tycon.thread)]
            @ sized (Tycon.char o CharSize.fromBits,
                     let
                        open CType
                     in
                        [Word8, Word16, Word32]
                     end)
            @ sized (Tycon.int o IntSize.fromBits,
                     let
                        open CType
                     in
                        [Int8, Int16, Int32, Int64]
                     end)
            @ sized (Tycon.word o WordSize.fromBits,
                     let
                        open CType
                     in
                        [Word8, Word16, Word32, Word64]
                     end)
         end

      val nullary =
         List.map (nullary, fn (name, ctype, tycon) =>
                   {ctype = ctype, name = name, tycon = tycon})

      val unary: Tycon.t list =
         [Tycon.array, Tycon.reff, Tycon.vector]

      fun toNullaryCType (t: t): {ctype: CType.t, name: string} option =
         case deConOpt t of
            NONE => NONE
          | SOME (c, _) =>
               Option.map
               (List.peek (nullary, fn {tycon = c', ...} =>
                           Tycon.equals (c, c')),
                fn {ctype, name, ...} => {ctype = ctype, name = name})

      and toUnaryCType (t: t): {ctype: CType.t, name: string} option =
         case deConOpt t of
            NONE => NONE
          | SOME (c, ts) =>
               if List.exists (unary, fn c' => Tycon.equals (c, c'))
                  andalso 1 = Vector.length ts
                  andalso isSome (toCType (Vector.first ts))
                  then SOME {ctype = CType.objptr, name = "Objptr"}
                  else NONE

      and toCType (ty: t): {ctype: CType.t, name: string} option =
         case toNullaryCType ty of
            NONE => toUnaryCType ty
          | SOME {ctype, name} => SOME {ctype = ctype, name = name}

      val toCType =
         Trace.trace
         ("ElaborateCore.Type.toCType",
          layout,
          Option.layout (fn {ctype, name} =>
                         Layout.record
                         [("ctype", CType.layout ctype),
                          ("name", String.layout name)]))
         toCType

      type z = {ctype: CType.t, name: string, ty: t}

      fun toCBaseType (ty: t): z option =
         case toCType ty of
            NONE => NONE
          | SOME {ctype, name} =>
               SOME {ctype = ctype, name = name, ty = ty}
      fun toCArgType (ty: t): z vector option =
         case deTupleOpt ty of
            NONE =>
               (case toCBaseType ty of
                   NONE => NONE
                 | SOME z => SOME (Vector.new1 z))
          | SOME tys =>
               Exn.withEscape
               (fn esc =>
                (SOME o Vector.map)
                (tys, fn ty =>
                 case toCBaseType ty of
                    NONE => esc NONE
                  | SOME z => z))
      fun toCRetType (ty: t): z option option =
         case toCBaseType ty of
            NONE => if Type.isUnit ty
                       then SOME NONE
                       else NONE
          | SOME z => SOME (SOME z)
      fun toCFunType (ty: t): (z vector * z option) option =
         case deArrowOpt ty of
            NONE => NONE
          | SOME (arg, ret) =>
               (case toCArgType arg of
                   NONE => NONE
                 | SOME arg =>
                      (case toCRetType ret of
                          NONE => NONE
                        | SOME ret => SOME (arg, ret)))
      fun toCPtrType (ty: t): z option =
         if Type.isCPointer ty
            then let val {ctype, name} = valOf (toCType ty)
                 in SOME {ctype = ctype, name = name, ty = ty}
                 end
            else NONE
   end

val isIEAttributeConvention =
   fn ImportExportAttribute.Cdecl => true
    | ImportExportAttribute.Stdcall => true
    | _ => false

fun parseIEAttributesConvention (attributes: ImportExportAttribute.t list)
    : Convention.t option =
   case attributes of
      [] => SOME Convention.Cdecl
    | [a] =>
         (case a of
             ImportExportAttribute.Cdecl => SOME Convention.Cdecl
           | ImportExportAttribute.Stdcall =>
                if let
                      open Control
                   in
                      case !Target.os of
                         Target.Cygwin => true
                       | Target.MinGW => true
                       | _ => false
                   end
                   then SOME Convention.Stdcall
                else SOME Convention.Cdecl
           | _ => NONE)
    | _ => NONE

val isIEAttributeKind =
   fn ImportExportAttribute.Impure => true
    | ImportExportAttribute.Pure => true
    | ImportExportAttribute.Runtime => true
    | ImportExportAttribute.Reentrant => true
    | _ => false

fun parseIEAttributesKind (attributes: ImportExportAttribute.t list)
    : CKind.t option =
   case attributes of
      [] => SOME CKind.Impure
    | [a] =>
         (case a of
             ImportExportAttribute.Impure => SOME CKind.impure
           | ImportExportAttribute.Pure => SOME CKind.pure
           | ImportExportAttribute.Runtime => SOME CKind.runtimeDefault
           | ImportExportAttribute.Reentrant => SOME CKind.reentrant
           | _ => NONE)
    | _ => NONE

val isIEAttributeSymbolScope =
   fn ImportExportAttribute.External => true
    | ImportExportAttribute.Private => true
    | ImportExportAttribute.Public => true
    | _ => false

fun parseIEAttributesSymbolScope (attributes: ImportExportAttribute.t list,
                                  defScope : SymbolScope.t)
    : SymbolScope.t option =
   case attributes of
      [] => SOME defScope
    | [a] => (case a of
                 ImportExportAttribute.External => SOME SymbolScope.External
               | ImportExportAttribute.Private => SOME SymbolScope.Private
               | ImportExportAttribute.Public => SOME SymbolScope.Public
               | _ => NONE)
    | _ => NONE

fun scopeCheck {name, symbolScope, region} =
   let
      fun warn l =
         Control.warning (region, seq (List.map (l, str)), empty)
      val oldScope =
         Ffi.checkScope {name = name, symbolScope = symbolScope}
   in
      if symbolScope = oldScope then () else
      warn [ "symbol '", name, "' redeclared as ",
             SymbolScope.toString symbolScope,
             " (previously ",
             SymbolScope.toString oldScope,
             "). This may cause linker errors"]
   end

fun import {attributes: ImportExportAttribute.t list,
            elabedTy: Type.t,
            expandedTy: Type.t,
            name: string option,
            region: Region.t,
            layoutPrettyType: Type.t -> Layout.t}: Type.t Prim.t =
   let
      fun error l = Control.error (region, l, empty)
      fun invalidAttributes () =
         error (seq [str "invalid attributes for _import: ",
                     List.layout ImportExportAttribute.layout attributes])
      fun invalidType () =
         Control.error
         (region,
          str "invalid type for _import",
          layoutPrettyType elabedTy)
   in
      case Type.toCFunType expandedTy of
         NONE =>
            let
               val () = invalidType ()
            in
               Prim.bogus
            end
       | SOME (args, result) =>
            let
               datatype z = datatype CFunction.Target.t
               val convention =
                  List.keepAll (attributes, isIEAttributeConvention)
               val convention =
                  case parseIEAttributesConvention convention of
                     NONE => (invalidAttributes ()
                              ; Convention.Cdecl)
                   | SOME c => c
               val kind =
                  List.keepAll (attributes, isIEAttributeKind)
               val kind =
                  case parseIEAttributesKind kind of
                     NONE => (invalidAttributes ()
                              ; CKind.Impure)
                   | SOME k => k
               val symbolScope =
                  List.keepAll (attributes, isIEAttributeSymbolScope)
               val symbolScope =
                  case name of
                     NONE =>
                        (if List.isEmpty symbolScope
                            then ()
                            else invalidAttributes ()
                         ; SymbolScope.External)
                   | SOME name =>
                        let
                           val symbolScope =
                              case parseIEAttributesSymbolScope
                                   (symbolScope, SymbolScope.External) of
                                 NONE => (invalidAttributes ()
                                          ; SymbolScope.External)
                               | SOME s => s
                           val () = scopeCheck {name = name,
                                                symbolScope = symbolScope,
                                                region = region}
                        in
                           symbolScope
                        end
               val addrTy = Type.cpointer
               val func =
                  CFunction.T {args = let
                                         val args = Vector.map (args, #ty)
                                      in
                                         if isSome name
                                            then args
                                            else Vector.concat
                                                 [Vector.new1 addrTy, args]
                                      end,
                               convention = convention,
                               kind = kind,
                               prototype = (Vector.map (args, #ctype),
                                            Option.map (result, #ctype)),
                               return = (case result of
                                            NONE => Type.unit
                                          | SOME {ty, ...} => ty),
                               symbolScope = symbolScope,
                               target = (case name of
                                            NONE => Indirect
                                          | SOME name => Direct name)}
            in
               Prim.ffi func
            end
   end

fun primApp {args, prim, result: Type.t} =
   let
      val targs = Prim.extractTargs (prim,
                                     {args = Vector.map (args, Cexp.ty),
                                      result = result,
                                      typeOps = {deArray = Type.deArray,
                                                 deArrow = Type.deArrow,
                                                 deRef = Type.deRef,
                                                 deVector = Type.deVector,
                                                 deWeak = Type.deWeak}})
   in
      Cexp.make (Cexp.PrimApp {args = args,
                               prim = prim,
                               targs = targs},
                 result)
   end

local
   val zeroExpBool =
      Cexp.make (Cexp.Const
                 (fn () => Const.word (WordX.zero WordSize.bool)),
                 Type.word WordSize.bool)
   val oneExpBool =
      Cexp.make (Cexp.Const
                 (fn () => Const.word (WordX.one WordSize.bool)),
                 Type.word WordSize.bool)
   fun zeroExpPtrdiff () =
      Cexp.make (Cexp.Const
                 (fn () => Const.word (WordX.zero (WordSize.cptrdiff ()))),
                 Type.word (WordSize.cptrdiff ()))

   fun mkAddress {expandedPtrTy: Type.t,
                  name: string,
                  cty: CType.t option,
                  symbolScope: SymbolScope.t }: Cexp.t =
      primApp {args = Vector.new0 (),
               prim = Prim.ffiSymbol {name = name,
                                      cty = cty,
                                      symbolScope = symbolScope},
               result = expandedPtrTy}

   fun mkFetch {ctypeCbTy, isBool,
                expandedCbTy,
                ptrExp: Cexp.t}: Cexp.t =
      let
         val fetchExp =
            primApp {args = Vector.new2 (ptrExp, zeroExpPtrdiff ()),
                     prim = Prim.cpointerGet ctypeCbTy,
                     result = if isBool
                                 then Type.word WordSize.bool
                                 else expandedCbTy}
      in
         if not isBool then fetchExp else
         Cexp.iff (primApp
                   {args = Vector.new2 (fetchExp, zeroExpBool),
                    prim = Prim.wordEqual WordSize.bool,
                    result = expandedCbTy},
                   Cexp.falsee,
                   Cexp.truee)
      end

   fun mkStore {ctypeCbTy, isBool,
                ptrExp: Cexp.t, valueExp: Cexp.t}: Cexp.t =
      let
         val valueExp =
            if not isBool then valueExp else
            Cexp.iff (valueExp, oneExpBool, zeroExpBool)
      in
         primApp {args = Vector.new3 (ptrExp, zeroExpPtrdiff (), valueExp),
                  prim = Prim.cpointerSet ctypeCbTy,
                  result = Type.unit}
      end

   fun mkSymbol {ctypeCbTy: CType.t,
                 expandedCbTy: Type.t,
                 ptrExp: Cexp.t}: Cexp.t =
      let
         val isBool = Type.isBool expandedCbTy
         val getArg = Var.newNoname ()
         val setArg = Var.newNoname ()
      in
         (Cexp.tuple o Vector.new2)
         ((Cexp.lambda o Lambda.make)
          {arg = getArg,
           argType = Type.unit,
           body = mkFetch {ctypeCbTy = ctypeCbTy,
                           isBool = isBool,
                           expandedCbTy = expandedCbTy,
                           ptrExp = ptrExp},
           mayInline = true},
          (Cexp.lambda o Lambda.make)
          {arg = setArg,
           argType = expandedCbTy,
           body = mkStore {ctypeCbTy = ctypeCbTy,
                           isBool = isBool,
                           ptrExp = ptrExp,
                           valueExp = Cexp.var (setArg, expandedCbTy)},
           mayInline = true})
      end

   val isSymbolAttributeAlloc =
      fn SymbolAttribute.Alloc => true
       | _ => false

   fun parseSymbolAttributesAlloc (attributes: SymbolAttribute.t list)
       : bool option =
      case attributes of
         [] => SOME false
       | [a] => (case a of
                    SymbolAttribute.Alloc => SOME true
                  | _=> NONE)
       | _ => NONE

   val isSymbolAttributeSymbolScope =
      fn SymbolAttribute.Private => true
       | SymbolAttribute.Public => true
       | SymbolAttribute.External => true
       | _ => false

   fun parseSymbolAttributesSymbolScope (attributes: SymbolAttribute.t list,
                                         defScope: SymbolScope.t)
       : SymbolScope.t option =
      case attributes of
         [] => SOME defScope
       | [a] => (case a of
                    SymbolAttribute.Private => SOME SymbolScope.Private
                  | SymbolAttribute.Public => SOME SymbolScope.Public
                  | SymbolAttribute.External => SOME SymbolScope.External
                  | _=> NONE)
       | _ => NONE
in
   fun address {attributes: SymbolAttribute.t list,
                elabedTy: Type.t,
                expandedTy: Type.t,
                name: string,
                region: Region.t,
                layoutPrettyType: Type.t -> Layout.t}: Cexp.t =
      let
         fun error l = Control.error (region, l, empty)
         fun invalidAttributes () =
            error (seq [str "invalid attributes for _address: ",
                        List.layout SymbolAttribute.layout attributes])
         fun invalidType () =
            Control.error
            (region, str "invalid type for _address",
             layoutPrettyType elabedTy)
         val () =
            case Type.toCPtrType expandedTy of
               NONE => (invalidType (); ())
             | SOME _ => ()
         val expandedPtrTy = expandedTy
         val () =
            case List.keepAll (attributes, isSymbolAttributeAlloc) of
               [] => ()
             | _ => invalidAttributes ()
         val symbolScope =
            List.keepAll (attributes, isSymbolAttributeSymbolScope)
         val symbolScope =
            case parseSymbolAttributesSymbolScope
                 (symbolScope, SymbolScope.External) of
               NONE => (invalidAttributes ()
                        ; SymbolScope.External)
             | SOME s => s
         val () = scopeCheck {name = name,
                              symbolScope = symbolScope,
                              region = region}
         val addrExp =
            mkAddress {expandedPtrTy = expandedPtrTy,
                       name = name,
                       symbolScope = symbolScope,
                       cty = NONE}
         fun wrap (e, t) = Cexp.make (Cexp.node e, t)
      in
         wrap (addrExp, elabedTy)
      end

   fun symbolDirect {attributes: SymbolAttribute.t list,
                     elabedTy: Type.t,
                     expandedTy: Type.t,
                     name: string,
                     region: Region.t,
                     layoutPrettyType: Type.t -> Layout.t}: Cexp.t =
      let
         fun error l = Control.error (region, l, empty)
         fun invalidAttributes () =
            error (seq [str "invalid attributes for _symbol: ",
                        List.layout SymbolAttribute.layout attributes])
         fun invalidType () =
            Control.error
            (region, str "invalid type for _symbol",
             layoutPrettyType elabedTy)
         val expandedCbTy =
            Exn.withEscape
            (fn escape =>
             let
                val invalidType = fn () =>
                   (invalidType ()
                    ; ignore (escape Type.word8)
                    ; Error.bug "ElaborateCore.symbolDirect.escape")
             in
                case Type.deTupleOpt expandedTy of
                   NONE => invalidType ()
                 | SOME tys =>
                      if Vector.length tys <> 2
                         then invalidType ()
                         else let
                                 fun doit ty =
                                    case Type.deArrowOpt ty of
                                       NONE => invalidType ()
                                     | SOME tys => tys
                                 val (getArgTy, getResTy) =
                                    doit (Vector.first tys)
                                 val (setArgTy, setResTy) =
                                    doit (Vector.sub (tys, 1))
                                 val () =
                                    if Type.isUnit getArgTy
                                       then ()
                                       else invalidType ()
                                 val () =
                                    if Type.isUnit setResTy
                                       then ()
                                       else invalidType ()
                                 val () =
                                    if Type.canUnify (getResTy, setArgTy)
                                       then ()
                                       else invalidType ()
                              in
                                 getResTy
                              end
             end)
         val ctypeCbTy =
            case Type.toCBaseType expandedCbTy of
               NONE => (invalidType ()
                        ; CType.word (WordSize.word8, {signed = false}))
             | SOME {ctype, ...} => ctype
         val alloc =
            List.keepAll (attributes, isSymbolAttributeAlloc)
         val alloc =
            case parseSymbolAttributesAlloc alloc of
               NONE => (invalidAttributes ()
                        ; false)
             | SOME a => a
         val defScope =
            if alloc then SymbolScope.Public else SymbolScope.External
         val symbolScope =
            List.keepAll (attributes, isSymbolAttributeSymbolScope)
         val symbolScope =
            case parseSymbolAttributesSymbolScope
                 (symbolScope, defScope) of
               NONE => (invalidAttributes ()
                        ; defScope)
             | SOME s => s
         val () =
            if alloc andalso symbolScope = SymbolScope.External
            then invalidAttributes () else ()
         val () = scopeCheck {name = name,
                              symbolScope = symbolScope,
                              region = region}
         val () =
            if not alloc then () else
            Ffi.addSymbol {name = name,
                           ty = ctypeCbTy,
                           symbolScope = symbolScope}
         val addrExp =
            mkAddress {expandedPtrTy = Type.cpointer,
                       name = name,
                       cty = SOME ctypeCbTy,
                       symbolScope = symbolScope}
         val symExp =
            mkSymbol {ctypeCbTy = ctypeCbTy,
                      expandedCbTy = expandedCbTy,
                      ptrExp = addrExp}
         fun wrap (e, t) = Cexp.make (Cexp.node e, t)
      in
         wrap (symExp, elabedTy)
      end

   fun symbolIndirect {elabedTy: Type.t,
                       expandedTy: Type.t,
                       region: Region.t,
                       layoutPrettyType: Type.t -> Layout.t}: Cexp.t =
      let
         fun invalidType () =
            Control.error
            (region, str "invalid type for _symbol",
             layoutPrettyType elabedTy)
         val (expandedPtrTy, expandedCbTy) =
            Exn.withEscape
            (fn escape =>
             let
                val invalidType = fn () =>
                   (invalidType ()
                    ; ignore (escape (Type.cpointer, Type.word8))
                    ; Error.bug "ElaborateCore.symbolIndirect.escape")
             in
                case Type.deArrowOpt expandedTy of
                   NONE => invalidType ()
                 | SOME (ptrTy, symTy) =>
                      (case Type.deTupleOpt symTy of
                          NONE => invalidType ()
                        | SOME tys =>
                             if Vector.length tys <> 2
                                then invalidType ()
                                else let
                                        fun doit ty =
                                           case Type.deArrowOpt ty of
                                              NONE => invalidType ()
                                            | SOME tys => tys
                                        val (getArgTy, getResTy) =
                                           doit (Vector.sub (tys, 0))
                                        val (setArgTy, setResTy) =
                                           doit (Vector.sub (tys, 1))
                                        val () =
                                           if Type.isUnit getArgTy
                                              then ()
                                              else invalidType ()
                                        val () =
                                           if Type.isUnit setResTy
                                              then ()
                                              else invalidType ()
                                        val () =
                                           if Type.canUnify (getResTy, setArgTy)
                                              then ()
                                              else invalidType ()
                                     in
                                        (ptrTy, getResTy)
                                     end)
             end)
         val ctypeCbTy =
            case Type.toCBaseType expandedCbTy of
               NONE => (invalidType (); CType.word (WordSize.word8, {signed = false}))
             | SOME {ctype, ...} => ctype
         val () =
            case Type.toCPtrType expandedPtrTy of
               NONE => (invalidType (); ())
             | SOME _ => ()
         val ptrArg = Var.newNoname ()
         val ptrExp = Cexp.var (ptrArg, expandedPtrTy)
         val symExp =
            mkSymbol {ctypeCbTy = ctypeCbTy,
                      expandedCbTy = expandedCbTy,
                      ptrExp = ptrExp}
         fun wrap (e, t) = Cexp.make (Cexp.node e, t)
      in
         wrap ((Cexp.lambda o Lambda.make)
               {arg = ptrArg,
                argType = expandedPtrTy,
                body = symExp,
                mayInline = true},
               elabedTy)
      end
end

fun export {attributes: ImportExportAttribute.t list,
            elabedTy: Type.t,
            expandedTy: Type.t,
            name: string,
            region: Region.t,
            layoutPrettyType: Type.t -> Layout.t}: Aexp.t =
   let
      fun error l = Control.error (region, l, empty)
      fun invalidAttributes () =
         error (seq [str "invalid attributes for _export: ",
                     List.layout ImportExportAttribute.layout attributes])
      fun invalidType () =
         Control.error
         (region,
          str "invalid type for _export",
          layoutPrettyType elabedTy)
      val convention =
         List.keepAll (attributes, isIEAttributeConvention)
      val convention =
         case parseIEAttributesConvention convention of
            NONE => (invalidAttributes ()
                     ; Convention.Cdecl)
          | SOME c => c
      val symbolScope =
         List.keepAll (attributes, isIEAttributeSymbolScope)
      val symbolScope =
         case parseIEAttributesSymbolScope
              (symbolScope, SymbolScope.Public) of
            NONE => (invalidAttributes ()
                     ; SymbolScope.Public)
          | SOME SymbolScope.External =>
               (invalidAttributes ()
                ; SymbolScope.Public)
          | SOME s => s
      val () = scopeCheck {name = name,
                           symbolScope = symbolScope,
                           region = region}
      val (exportId, args, res) =
         case Type.toCFunType expandedTy of
            NONE =>
               (invalidType ()
                ; (0, Vector.new0 (), NONE))
          | SOME (args, result) =>
               let
                  val id =
                     Ffi.addExport {args = Vector.map (args, #ctype),
                                    convention = convention,
                                    name = name,
                                    res = Option.map (result, #ctype),
                                    symbolScope = symbolScope}
               in
                  (id, args, result)
               end
      open Ast
      fun id (name: string) =
         Aexp.longvid (Longvid.short
                       (Vid.fromSymbol (Symbol.fromString name, region)))
      fun int (i: int): Aexp.t =
         Aexp.const (Aconst.makeRegion (Aconst.Int (IntInf.fromInt i), region))
      val f = Var.fromSymbol (Symbol.fromString "f", region)
      val p = Var.fromSymbol (Symbol.fromString "p", region)
   in
      Exp.fnn
      (Vector.new1
       (Pat.var f,
        Exp.app
        (id "register",
         Exp.tuple
         (Vector.new2
          (int exportId,
           Exp.fnn
           (Vector.new1
            (Pat.var p,
             let
                val (args, decs) =
                   Vector.unzip
                   (Vector.mapi
                    (args, fn (i, {name, ...}) =>
                     let
                        val x =
                           Var.fromSymbol
                           (Symbol.fromString (concat ["x", Int.toString i]),
                            region)
                        val dec =
                           Dec.vall
                           (Vector.new0 (),
                            x,
                            Exp.app
                            (id (concat ["get", name]),
                             (Exp.tuple o Vector.new2)
                             (Exp.var p, int (i + 1))))
                     in
                        (x, dec)
                     end))
                val resVar = Var.fromSymbol (Symbol.fromString "res", region)
                fun newVar () = Var.fromSymbol (Symbol.fromString "none", region)
             in
                Exp.lett
                (Vector.concat
                 [decs,
                  Vector.map
                  (Vector.new2
                   ((resVar, Exp.app (Exp.var f,
                                      Exp.tuple (Vector.map (args, Exp.var)))),
                    (newVar (),
                     (case res of
                         NONE => Exp.constraint (Exp.var resVar, Type.unit)
                       | SOME {name, ...} =>
                            Exp.app
                            (id (concat ["set", name]),
                             (Exp.tuple o Vector.new3)
                             (Exp.var p,
                              int (Vector.length args + 1),
                              Exp.var resVar))))),
                   fn (x, e) => Dec.vall (Vector.new0 (), x, e))],
                 Exp.tuple (Vector.new0 ()),
                 region)
             end)))))))
   end

val export =
   Trace.trace
   ("ElaborateCore.export",
    fn {name, ...} => String.layout name,
    Aexp.layout)
   export

structure Aexp =
   struct
      open Aexp

      local
         val x = Avar.fromSymbol (Symbol.fromString "#", Region.bogus)
         val xField = Apat.Item.Field (Apat.var x)
         val xVar = var x
      in
         fun selector (f: Field.t, r: Region.t): t =
            fnn (Vector.new1
                 (Apat.makeRegion
                  (Apat.Record {flexible = true,
                                items = Vector.new1 (f, Region.bogus, xField)},
                   r),
                  xVar))
      end
   end

structure Con =
   struct
      open Con

      val fromAst = newString o Ast.Con.toString
   end

structure Cexp =
   struct
      open Cexp

      fun enterLeave (e: t, doit: bool, si): t =
         if not doit
            (* Don't create the sourceInfo if we're in the middle of elaborating
             * a functor body.  Count profiling keeps track of all sourceInfos
             * created and would show it with a count of zero, which would be
             * bad.
             *)
            orelse Env.amInsideFunctor ()
            (* Don't create the source info if we're profiling some IL. *)
            orelse !Control.profileIL <> Control.ProfileSource
            then e
         else make (EnterLeave (e, si ()), ty e)
   end

(* This property must be outside of elaborateDec, since we don't want it to
 * be created for each call to elaborateDec.  If it were, then property lists
 * on variables would be littered with lots of these.
 *)
val {get = recursiveTargs: Var.t -> (unit -> Type.t vector) option ref,
     ...} =
   Property.get (Var.plist, Property.initFun (fn _ => ref NONE))

fun elaborateDec (d, {env = E, nest}) =
   let
      val profileBody =
         let
            open Control
         in
            !profile <> ProfileNone
         end
      fun recursiveFun () =
         let
            val boundRef: (unit -> Tyvar.t vector) option ref = ref NONE
            val targs =
               Promise.lazy
               (fn () =>
                case !boundRef of
                   NONE => Error.bug "ElaborateCore.elaborateDec: boundRef not set"
                 | SOME f => Vector.map (f (), Type.var))
            fun markFunc func = recursiveTargs func := SOME targs
            fun unmarkFunc func = recursiveTargs func := NONE
            fun setBound b = boundRef := SOME b
         in
            {markFunc = markFunc,
             setBound = setBound,
             unmarkFunc = unmarkFunc}
         end
      fun elabType (t: Atype.t, {bogusAsUnknown}): Type.t =
         elaborateType (t, E, {bogusAsUnknown = bogusAsUnknown})
      fun elabTypBind (typBind: TypBind.t) =
         let
            val TypBind.T types = TypBind.node typBind
            val types =
               Vector.map
               (types, fn {def, tycon, tyvars} =>
                TyvarEnv.scope
                (tyvars, fn tyvars =>
                 {scheme = Scheme.make {canGeneralize = true,
                                        ty = elabType (def, {bogusAsUnknown = false}),
                                        tyvars = tyvars},
                  tycon = tycon}))
            val () =
               Vector.foreach
               (types, fn {scheme, tycon} =>
                Env.extendTycon
                (E, tycon, TypeStr.def scheme,
                 {forceUsed = false,
                  isRebind = false}))
            (* Rebuild type to propagate tycon equality
             * when 'withtype' components of 'datatype' decl. *)
            fun rebind () =
               Vector.foreach
               (types, fn {scheme, tycon} =>
                let
                   val (tyvars, ty) = Scheme.dest scheme
                   val ty = Type.copy ty
                   val scheme =
                      Scheme.make {canGeneralize = true,
                                   tyvars = tyvars,
                                   ty = ty}
                in
                   Env.extendTycon
                   (E, tycon, TypeStr.def scheme,
                    {forceUsed = false,
                     isRebind = true})
                end)
         in
            rebind
         end
      fun elabDatBind (datBind: DatBind.t, nest: string list)
         : Decs.t * {tycon: Ast.Tycon.t, typeStr: TypeStr.t} vector =
         (* rules 28, 29, 81, 82 *)
         let
            val DatBind.T {datatypes, withtypes} = DatBind.node datBind
            (* Build enough of an env so that that the withtypes and the
             * constructor argument types can be elaborated.
             *)
            val datatypes =
               Vector.map
               (datatypes, fn {cons, tycon = name, tyvars} =>
                let
                   val arity = Vector.length tyvars
                   val k = Kind.Arity arity
                   val n = Ast.Tycon.toString name
                   val pd = concat (List.separate (rev (n :: nest), "."))
                   val r = Ast.Tycon.region name
                   val tycon =
                      Tycon.make {admitsEquality = AdmitsEquality.Sometimes,
                                  kind = k,
                                  name = n,
                                  prettyDefault = pd,
                                  region = r}
                   val _ = Env.extendTycon (E, name, TypeStr.tycon tycon,
                                            {forceUsed = true,
                                             isRebind = false})
                in
                   {arity = arity,
                    cons = cons,
                    name = name,
                    tycon = tycon,
                    tyvars = tyvars}
                end)
            val rebindWithtypes = elabTypBind withtypes
            val datatypes =
               Vector.map
               (datatypes, fn {arity, cons, name, tycon, tyvars} =>
                let
                   val cons =
                      Vector.map
                      (cons, fn (name, arg) =>
                       TyvarEnv.scope
                       (tyvars, fn tyvars =>
                        {arg = Option.map (arg, fn t => elabType (t, {bogusAsUnknown = false})),
                         con = Con.fromAst name,
                         name = name,
                         tyvars = tyvars}))
                in
                   {arity = arity,
                    cons = cons,
                    name = name,
                    tycon = tycon}
                end)
            (* Maximize equality *)
            val change = ref false
            fun loop datatypes =
               let
                  val datatypes =
                     Vector.map
                     (datatypes, fn {arity, cons, name, tycon} =>
                      let
                         val isEquality = ref true
                         val cons =
                            Vector.map
                            (cons, fn {arg, con, name, tyvars} =>
                             let
                                val arg =
                                   Option.map
                                   (arg, fn arg =>
                                    let
                                       (* Rebuild type to propagate tycon equality. *)
                                       val arg = Type.copy arg
                                       val argScheme =
                                          Scheme.make {canGeneralize = true,
                                                       ty = arg,
                                                       tyvars = tyvars}
                                       val () =
                                          if Scheme.admitsEquality argScheme
                                             then ()
                                             else isEquality := false
                                    in
                                       arg
                                    end)
                             in
                                {arg = arg,
                                 con = con,
                                 name = name,
                                 tyvars = tyvars}
                             end)
                         datatype z = datatype AdmitsEquality.t
                         val () =
                            case Tycon.admitsEquality tycon of
                               Always =>
                                  Error.bug "ElaborateCore.elaborateDec.elabDatBind: Always"
                             | Never => ()
                             | Sometimes =>
                                  if !isEquality
                                     then ()
                                     else (Tycon.setAdmitsEquality (tycon, Never)
                                           ; change := true)
                      in
                         {arity = arity,
                          cons = cons,
                          name = name,
                          tycon = tycon}
                      end)
               in
                  if !change
                     then (change := false; loop datatypes)
                     else datatypes
               end
            val datatypes = loop datatypes
            val (datatypes, strs) =
               (Vector.unzip o Vector.map)
               (datatypes, fn {arity, cons, name, tycon} =>
                let
                   val tyvars' =
                      Vector.tabulate (arity, fn _ => Tyvar.makeNoname {equality = false})
                   val tyargs' =
                      Vector.map (tyvars', Type.var)
                   val (cons, cons') =
                      (Vector.unzip o Vector.map)
                      (cons, fn {arg, con, name, tyvars} =>
                       let
                          val res =
                             Type.con (tycon, Vector.map (tyvars, Type.var))
                          val (arg', ty) =
                             case arg of
                                NONE => (NONE, res)
                              | SOME arg =>
                                   let
                                      val argScheme =
                                         Scheme.make {canGeneralize = true,
                                                      ty = arg,
                                                      tyvars = tyvars}
                                      val arg' = Scheme.apply (argScheme, tyargs')
                                   in
                                      (SOME arg',
                                       Type.arrow (arg, res))
                                   end
                          val scheme =
                             Scheme.make {canGeneralize = true,
                                          ty = ty,
                                          tyvars = tyvars}
                       in
                          ({con = con,
                            name = name,
                            scheme = scheme},
                           {con = con,
                            arg = arg'})
                       end)
                   val cons = Env.newCons (E, cons)
                   val typeStr = TypeStr.data (tycon, cons)
                   val () =
                      Env.extendTycon
                      (E, name, typeStr,
                       {forceUsed = false,
                        isRebind = true})
                in
                   ({cons = cons',
                     tycon = tycon,
                     tyvars = tyvars'},
                    {tycon = name,
                     typeStr = typeStr})
                end)
            val () = rebindWithtypes ()
         in
            (Decs.single (Cdec.Datatype datatypes), strs)
         end
      fun elabDec arg : Decs.t =
         Trace.traceInfo
         (elabDecInfo,
          Layout.tuple3 (Ast.Dec.layout, Nest.layout, Bool.layout),
          Decs.layout, Trace.assertTrue)
         (fn (d, nest, isTop) =>
          let
             fun ctxt () = seq [str "in: ", approximate (Adec.layout d)]
             val region = Adec.region d
             fun generalizeError (var, lay, _) =
                Control.error
                (Avar.region var,
                 seq [str "type of variable cannot be generalized in expansive declaration: ",
                      Avar.layout var],
                 align [seq [str "type: ", lay],
                        ctxt ()])
             val () = Time.tick {region = region}
             fun checkSchemes (v: (Avar.t * Scheme.t) vector): unit =
                if isTop
                   then Vector.foreach
                        (v, fn (x, s) =>
                         if not (Scheme.haveUnknowns s)
                            then ()
                            else List.push
                                 (undeterminedTypeChecks, fn () =>
                                  if not (Scheme.haveUnknowns s)
                                     then ()
                                     else let
                                             (* Technically, wrong scope for region;
                                              * but saving environment would probably
                                              * be expensive.
                                              *)
                                             val (bs, t) = Scheme.dest s
                                             val {layoutPrettyTycon, ...} =
                                                Env.makeLayoutPrettyTycon (E, {prefixUnset = true})
                                             val {layoutPretty = layoutPrettyTyvar,
                                                  localInit = localInitLayoutPrettyTyvar, ...} =
                                                Tyvar.makeLayoutPretty ()
                                             val () = localInitLayoutPrettyTyvar bs
                                             val (lay, _) =
                                                Type.layoutPretty
                                                (t, {expandOpaque = false,
                                                     layoutPrettyTycon = layoutPrettyTycon,
                                                     layoutPrettyTyvar = layoutPrettyTyvar})
                                          in
                                             Control.warning
                                             (Avar.region x,
                                              seq [str "type of variable was not inferred and could not be generalized: ",
                                                   Avar.layout x],
                                              align [seq [str "type: ", lay],
                                                     ctxt ()])
                                          end))
                else ()
             fun checkConRedefine (vid, keyword, ctxt) =
                case Env.peekLongcon (E, Ast.Longcon.short (Avid.toCon vid)) of
                   NONE => ()
                 | SOME _ =>
                      (case valrecConstr () of
                          Control.Elaborate.DiagEIW.Error => Control.error
                        | Control.Elaborate.DiagEIW.Ignore => (fn _ => ())
                        | Control.Elaborate.DiagEIW.Warn => Control.warning)
                      (Avid.region vid,
                       seq [str "constructor redefined by ",
                            str keyword,
                            str ": ",
                            Avid.layout vid],
                       ctxt ())
             val elabDec = fn (d, isTop) => elabDec (d, nest, isTop)
             val decs =
                case Adec.node d of
                   Adec.Abstype {datBind, body} => (* rule 19 and p.57 *)
                      let
                         val ((decs, strs), decs') =
                            Env.localCore
                            (E,
                             fn () => elabDatBind (datBind, nest),
                             fn z => (z, elabDec (body, isTop)))
                         val () =
                            Vector.foreach
                            (strs, fn {tycon, typeStr} =>
                             Env.extendTycon (E, tycon, TypeStr.abs typeStr,
                                              {forceUsed = true,
                                               isRebind = false}))
                      in
                         Decs.append (decs, decs')
                      end
                 | Adec.Datatype rhs =>
                      (case DatatypeRhs.node rhs of
                          DatatypeRhs.DatBind datBind => (* rule 17 *)
                             #1 (elabDatBind (datBind, nest))
                        | DatatypeRhs.Repl {lhs, rhs} => (* rule 18 *)
                             let
                                val () =
                                   Option.app
                                   (Env.lookupLongtycon (E, rhs), fn s =>
                                    let
                                       val forceUsed =
                                          case TypeStr.node s of
                                             TypeStr.Datatype _ => true
                                           | _ => false
                                       val () =
                                          Env.extendTycon (E, lhs, s,
                                                           {forceUsed = forceUsed,
                                                            isRebind = false})
                                    in
                                       ()
                                    end)
                             in
                                Decs.empty
                             end)
                 | Adec.DoDec exp =>
                      let
                         val _ = check (ElabControl.allowDoDecls, "do declarations", Adec.region d)
                         val {unify, ...} = DiagUtils.make E
                         val exp' = elabExp (exp, nest, NONE)
                         val _ =
                            unify
                            (Cexp.ty exp', Type.unit, fn (l1, _) =>
                             (Aexp.region exp,
                              str "do declaration expression not of type unit",
                              align [seq [str "expression: ", l1],
                                     ctxt ()]))
                         val vb = {ctxt = fn _ => empty,
                                   exp = exp',
                                   layPat = fn _ => empty,
                                   nest = nest,
                                   pat = Cpat.wild Type.unit,
                                   regionPat = Region.bogus}
                      in
                         Decs.single
                         (Cdec.Val {matchDiags = matchDiagsFromNoMatch Cexp.Impossible,
                                    rvbs = Vector.new0 (),
                                    tyvars = Vector.new0,
                                    vbs = Vector.new1 vb})
                      end
                 | Adec.Exception ebs =>
                      let
                         val decs =
                            Vector.fold
                            (ebs, Decs.empty, fn ((exn, rhs), decs) =>
                             let
                                val decs =
                                   case EbRhs.node rhs of
                                      EbRhs.Def c =>
                                         (case Env.lookupLongexn (E, c) of
                                             NONE => decs
                                           | SOME (exn', scheme) =>
                                                let
                                                   val _ = Env.extendExn (E, exn, exn', scheme)
                                                in
                                                   decs
                                                end)
                                    | EbRhs.Gen arg =>
                                         let
                                            val exn' = Con.fromAst exn
                                            val (arg, ty) =
                                               case arg of
                                                  NONE => (NONE, Type.exn)
                                                | SOME t =>
                                                     let
                                                        val t = elabType (t, {bogusAsUnknown = false})
                                                     in
                                                        (SOME t,
                                                         Type.arrow (t, Type.exn))
                                                     end
                                            val scheme = Scheme.fromType ty
                                            val _ = Env.extendExn (E, exn, exn', scheme)
                                         in
                                            Decs.add (decs,
                                                      Cdec.Exception {arg = arg,
                                                                      con = exn'})
                                         end
                             in
                                decs
                             end)
                      in
                         decs
                      end
                 | Adec.Fix {ops, fixity} =>
                      (Vector.foreach (ops, fn op' =>
                                       Env.extendFix (E, op', fixity))
                       ; Decs.empty)
                 | Adec.Fun {tyvars = tyvars, fbs} =>
                      let
                         val close = TypeEnv.close {region = region}
                      in
                         TyvarEnv.scope
                         (tyvars, fn tyvars' =>
                          let
                             val {layoutPrettyTycon, layoutPrettyTyvar, unify, ...} =
                                DiagUtils.make E
                             val {markFunc, setBound, unmarkFunc} = recursiveFun ()
                             val fbs =
                                Vector.map2
                                (fbs, Adec.layoutFun {tyvars = tyvars, fbs = fbs}, fn (clauses, layFb) =>
                                 let
                                    val ctxtFb = fn () =>
                                       seq [str "in: ", approximate (layFb ())]
                                    val clauses =
                                       Vector.map
                                       (clauses, fn {body, pats, resultType} =>
                                        let
                                           fun layPats () =
                                              approximate (Apat.layoutFlatApp pats)
                                           fun layPatsPrefix () =
                                              approximatePrefix (Apat.layoutFlatApp pats)
                                           val regionPats =
                                              Region.append
                                              (Apat.region (Vector.first pats),
                                               Apat.region (Vector.last pats))
                                           val regionBody =
                                              Aexp.region body
                                           fun layClause () =
                                              approximate
                                              (seq [Apat.layoutFlatApp pats,
                                                    case resultType of
                                                       NONE => empty
                                                     | SOME rt => seq [str ": ",
                                                                       Atype.layout rt],
                                                    str " = ",
                                                    Aexp.layout body])
                                           val regionClause =
                                              Region.append (regionPats, regionBody)
                                           val {args = pats, func} =
                                              Parse.parseClause (pats, E, ctxt)
                                        in
                                           {body = body,
                                            func = func,
                                            layClause = layClause,
                                            layPats = layPats,
                                            layPatsPrefix = layPatsPrefix,
                                            pats = pats,
                                            regionClause = regionClause,
                                            regionPats = regionPats,
                                            resultType = resultType}
                                        end)
                                    val regionFb =
                                       Region.append
                                       (#regionClause (Vector.first clauses),
                                        #regionClause (Vector.last clauses))
                                    val {pats = pats0, func as func0, layClause = layClause0, ...} =
                                       Vector.first clauses
                                    val layFunc0 = fn () => str (Avar.toString func0)
                                    fun err (reg, msg, desc, layN, lay0) =
                                       Control.error
                                       (reg,
                                        seq [str msg],
                                        align [seq [str desc, approximate (layN ())],
                                               seq [str "previous: ", approximate (lay0 ())],
                                               ctxtFb ()])
                                    val _ =
                                       Vector.foreach
                                       (clauses, fn {func = funcN, pats = patsN, layClause = layClauseN, regionPats = regionPatsN, ...} =>
                                        let
                                           val layFuncN = fn () => str (Avar.toString funcN)
                                           val _ =
                                              if Avar.equals (func, funcN)
                                                 then ()
                                                 else err (Avar.region funcN,
                                                           "function clause with different name",
                                                           "name:     ", layFuncN, layFunc0)
                                           val _ =
                                              if Vector.length pats0 = Vector.length patsN
                                                 then ()
                                                 else err (regionPatsN,
                                                           "function clause with different number of arguments",
                                                           "clause:   ", layClauseN, layClause0)
                                        in
                                           ()
                                        end)
                                    val numArgs =
                                       Vector.fold
                                       (clauses, ~1, fn (r, numArgs) =>
                                        Int.max (Vector.length (#pats r), numArgs))
                                 in
                                    {clauses = clauses,
                                     ctxtFb = ctxtFb,
                                     func = func,
                                     numArgs = numArgs,
                                     regionFb = regionFb}
                                 end)
                             val _ =
                                Vector.fold
                                (fbs, [], fn ({func = f, ...}, ac) =>
                                 if List.exists (ac, fn f' => Avar.equals (f, f'))
                                    then
                                       (Control.error
                                        (Avar.region f,
                                         seq [str "duplicate function definition: ",
                                              Avar.layout f],
                                         ctxt ())
                                        ; ac)
                                    else f :: ac)
                             val fbs =
                                Vector.map
                                (fbs, fn {clauses, ctxtFb, func, numArgs, regionFb} =>
                                 let
                                    val argTys = Vector.tabulate (numArgs, fn _ => Type.new ())
                                    val resTy = Type.new ()
                                    val clauses =
                                       Vector.map
                                       (clauses, fn {body, layPats, layPatsPrefix, pats, regionPats, resultType, ...} =>
                                        let
                                           val elaboratePat = elaboratePat ()
                                           val (pats, bindss) =
                                              (Vector.unzip o Vector.mapi)
                                              (pats, fn (i, pat) =>
                                               let
                                                  val regionPat = Apat.region pat
                                                  val (pat, binds) =
                                                     elaboratePat
                                                     (pat, E,
                                                      {bind = false,
                                                       isRvb = false})
                                                  val _ =
                                                     unify
                                                     (Vector.sub (argTys, i), Cpat.ty pat, fn (l1, l2) =>
                                                      (regionPat,
                                                       str "function clause with argument of different type",
                                                       align [seq [str "argument: ", l2],
                                                              seq [str "previous: ", l1],
                                                              ctxtFb ()]))
                                               in
                                                  (pat, binds)
                                               end)
                                           val binds = Vector.concatV (Vector.rev bindss)
                                           val resultType =
                                              Option.map
                                              (resultType, fn resultType =>
                                               let
                                                  val regionResultType = Atype.region resultType
                                                  val resultType = elabType (resultType, {bogusAsUnknown = true})
                                                  val _ =
                                                     unify
                                                     (resTy, resultType,
                                                      fn (l1, l2) =>
                                                      (regionResultType,
                                                       str "function clause with result constraint of different type",
                                                       align [seq [str "constraint: ", l2],
                                                              seq [str "previous:   ", l1],
                                                              ctxtFb ()]))
                                               in
                                                  (resultType, regionResultType)
                                               end)
                                        in
                                           {binds = binds,
                                            body = body,
                                            layPats = layPats,
                                            layPatsPrefix = layPatsPrefix,
                                            pats = pats,
                                            regionPats = regionPats,
                                            resultType = resultType}
                                        end)
                                    val funTy =
                                       let
                                          fun chk ty =
                                             if Type.isUnknown ty
                                                then Type.new ()
                                                else ty
                                       in
                                          if Vector.forall (argTys, Type.isUnknown)
                                             andalso Type.isUnknown resTy
                                             then Type.new ()
                                             else Vector.foldr (Vector.map (argTys, chk), chk resTy, Type.arrow)
                                       end
                                    val funcVid = Avid.fromVar func
                                    val _ =
                                       Avid.checkRedefineSpecial
                                       (funcVid,
                                        {allowIt = true,
                                         ctxt = ctxtFb,
                                         keyword = "fun"})
                                    val _ =
                                       checkConRedefine
                                       (funcVid, "fun", ctxtFb)
                                    val var = Var.fromAst func
                                    val _ =
                                       Env.extendVar
                                       (E, func, var,
                                        Scheme.fromType funTy,
                                        {isRebind = false})
                                    val _ =
                                       markFunc var
                                 in
                                    {argTys = argTys,
                                     clauses = clauses,
                                     ctxtFb = ctxtFb,
                                     func = func,
                                     funTy = funTy,
                                     regionFb = regionFb,
                                     resTy = resTy,
                                     var = var}
                                 end)
                             val fbs =
                                Vector.map
                                (fbs, fn {argTys, clauses, ctxtFb, func, funTy, regionFb, resTy, var, ...} =>
                                 let
                                    val nest = Avar.toString func :: nest
                                    val resultTypeConstraint = Vector.exists (clauses, Option.isSome o #resultType)
                                    val rules =
                                       Vector.map
                                       (clauses, fn {binds, body, layPats, layPatsPrefix, pats, regionPats, resultType} =>
                                        let
                                           val regionBody = Aexp.region body
                                           val body =
                                              Env.scope
                                              (E, fn () =>
                                               (Vector.foreach
                                                (binds, fn (x, x', ty) =>
                                                 Env.extendVar
                                                 (E, x, x', Scheme.fromType ty,
                                                  {isRebind = false}))
                                                ; elabExp (body, nest, NONE)))
                                           val body =
                                              Cexp.enterLeave
                                              (body,
                                               profileBody andalso !Control.profileBranch,
                                               fn () =>
                                               SourceInfo.function
                                               {name = ("<case " ^ Layout.toString (layPatsPrefix ()) ^ ">") :: nest,
                                                region = regionBody})
                                           val _ =
                                              case resultType of
                                                 SOME (resultType, regionResultType) =>
                                                    unify
                                                    (resultType, Cexp.ty body,
                                                     fn (l1, l2) =>
                                                     (Region.append (regionResultType, regionBody),
                                                      seq [if Vector.length clauses = 1
                                                              then str "function "
                                                              else str "function clause ",
                                                           str "expression and result constraint disagree"],
                                                      align [seq [str "expression: ", l2],
                                                             seq [str "constraint: ", l1],
                                                             ctxtFb ()]))
                                               | NONE =>
                                                    if resultTypeConstraint
                                                       then unify
                                                            (resTy, Cexp.ty body, fn (l1, l2) =>
                                                             (regionBody,
                                                              str "function clause expression and result constraint disagree",
                                                              align [seq [str "expression: ", l2],
                                                                     seq [str "constraint: ", l1],
                                                                     ctxtFb ()]))
                                                       else unify
                                                            (resTy, Cexp.ty body, fn (l1, l2) =>
                                                             (regionBody,
                                                              str "function clause with expression of different type",
                                                              align [seq [str "expression: ", l2],
                                                                     seq [str "previous:   ", l1],
                                                                     ctxtFb ()]))
                                        in
                                           {exp = body,
                                            layPat = SOME layPats,
                                            pat = Cpat.tuple pats,
                                            regionPat = regionPats}
                                        end)
                                    val args =
                                       Vector.map
                                       (argTys, fn argTy =>
                                        (Var.newNoname (), argTy))
                                    fun check () =
                                       unify
                                       (Vector.foldr (argTys, resTy, Type.arrow), funTy, fn (l1, l2) =>
                                        (Avar.region func,
                                         seq [str "recursive use of function disagrees with function declaration type: ",
                                              Avar.layout func],
                                         align [seq [str "recursive use: ", l2],
                                                seq [str "function type: ", l1],
                                                ctxt ()]))
                                    val body =
                                       Cexp.casee
                                       {ctxt = ctxtFb,
                                        kind = ("function", "clause"),
                                        nest = nest,
                                        matchDiags = matchDiagsFromNoMatch Cexp.RaiseMatch,
                                        noMatch = Cexp.RaiseMatch,
                                        region = regionFb,
                                        rules = rules,
                                        test = Cexp.tuple (Vector.map (args, Cexp.var))}
                                    val body =
                                       Cexp.enterLeave
                                       (body,
                                        profileBody,
                                        fn () =>
                                        SourceInfo.function
                                        {name = nest,
                                         region = regionFb})
                                    val lambda =
                                       Vector.foldr
                                       (args, body, fn ((arg, argTy), body) =>
                                        Cexp.make
                                        (Cexp.Lambda
                                         (Lambda.make
                                          {arg = arg,
                                           argType = argTy,
                                           body = body,
                                           mayInline = true}),
                                         Type.arrow (argTy, Cexp.ty body)))
                                    val lambda =
                                       case Cexp.node lambda of
                                          Cexp.Lambda lambda => lambda
                                        | _ => Lambda.bogus
                                 in
                                    {check = check,
                                     func = func,
                                     funTy = funTy,
                                     lambda = lambda,
                                     var = var}
                                 end)
                             val _ =
                                Vector.foreach
                                (fbs, fn {check, ...} =>
                                 check ())
                             val {bound, schemes} =
                                close
                                (tyvars',
                                 Vector.map
                                 (fbs, fn {func, funTy, ...} =>
                                  {isExpansive = false,
                                   ty = funTy,
                                   var = func}),
                                 {error = generalizeError,
                                  layoutPrettyTycon = layoutPrettyTycon,
                                  layoutPrettyTyvar = layoutPrettyTyvar})
                             val _ =
                                checkSchemes
                                (Vector.zip
                                 (Vector.map (fbs, #func),
                                  schemes))
                             val _ = setBound bound
                             val _ =
                                Vector.foreach2
                                (fbs, schemes,
                                 fn ({func, var, ...}, scheme) =>
                                 (Env.extendVar
                                  (E, func, var, scheme,
                                   {isRebind = true})
                                  ; unmarkFunc var))
                             val decs =
                                Vector.map
                                (fbs, fn {lambda, var, ...} =>
                                 {lambda = lambda,
                                  var = var})
                          in
                             Decs.single
                             (Cdec.Fun {decs = decs,
                                        tyvars = bound})
                          end)
                      end
                 | Adec.Local (d, d') =>
                      let
                         val res =
                            Env.localCore
                            (E,
                             fn () => elabDec (d, false),
                             fn decs => Decs.append (decs, elabDec (d', isTop)))
                      in
                         res
                      end
                 | Adec.Open paths =>
                      let
                         (* The following code is careful to first lookup all of the
                          * paths in the current environment, and then extend the
                          * environment with all of the results.
                          * See rule 22 of the Definition.
                          *)
                         val _ =
                            Vector.foreach
                            (Vector.map (paths, fn p => Env.lookupLongstrid (E, p)),
                             fn so => Option.app (so, fn s =>
                                                  Env.openStructure (E, s)))
                      in
                         Decs.empty
                      end
                 | Adec.Overload (p, x, tyvars, ty, xs) =>
                      TyvarEnv.scope
                      (tyvars, fn tyvars' =>
                       let
                          val {unify, ...} = DiagUtils.make E
                          val () = check (ElabControl.allowOverload, "_overload", region)
                          (* Lookup the overloads before extending the var in case
                           * x appears in the xs.
                           *)
                          val ovlds =
                             Vector.concatV
                             (Vector.map
                              (xs, fn x =>
                               case Env.lookupLongvid (E, x) of
                                  NONE => Vector.new0 ()
                                | SOME (Vid.Var v, t) =>
                                     Vector.new1 (Longvid.region x, (v, t))
                                | SOME (Vid.Overload (_, vs), _) =>
                                     Vector.map (vs, fn vt => (Longvid.region x, vt))
                                | _ =>
                                     (Control.error
                                      (Longvid.region x,
                                       str "cannot overload",
                                       seq [str "constructor: ", Longvid.layout x])
                                      ; Vector.new0 ())))
                          val s =
                             Scheme.make {canGeneralize = false,
                                          tyvars = tyvars',
                                          ty = elabType (ty, {bogusAsUnknown = false})}
                          val _ =
                             Vector.foreach
                             (ovlds,
                              fn (r, (_, s')) =>
                              let
                                 val is = Scheme.instantiate s
                                 val is' = Scheme.instantiate s'
                              in
                                 unify
                                 (#instance is,
                                  #instance is',
                                  fn (l1, l2) =>
                                  (r,
                                   str "variant does not unify with overload",
                                   align [seq [str "overload: ", l1],
                                          seq [str "variant:  ", l2],
                                          ctxt ()]))
                              end)
                          val _ =
                             Env.extendOverload
                             (E, p, x, Vector.map (ovlds, fn (_, vt) => vt), s)
                       in
                          Decs.empty
                       end)
                 | Adec.SeqDec ds =>
                      Vector.fold (ds, Decs.empty, fn (d, decs) =>
                                   Decs.append (decs, elabDec (d, isTop)))
                 | Adec.Type typBind =>
                      (ignore (elabTypBind typBind)
                       ; Decs.empty)
                 | Adec.Val {tyvars, rvbs, vbs} =>
                      let
                         val close = TypeEnv.close {region = region}
                      in
                         TyvarEnv.scope
                         (tyvars, fn tyvars' =>
                          let
                             val {layoutPrettyTycon, layoutPrettyTyvar, unify, ...} =
                                DiagUtils.make E
                             val {vbs = layVbs, rvbs = layRvbs} =
                                Adec.layoutVal {tyvars = tyvars, vbs = vbs, rvbs = rvbs}
                             (* Must do all the es and rvbs before the ps because of
                              * scoping rules.
                              *)
                             val vbs =
                                Vector.map2
                                (vbs, layVbs, fn ({exp, pat, ...}, layVb) =>
                                 let
                                    fun ctxtVb () =
                                       seq [str "in: ", approximate (layVb ())]
                                    fun layPat () = Apat.layout pat
                                    val regionPat = Apat.region pat
                                    val regionExp = Aexp.region exp
                                    val exp = elabExp (exp, nest, Apat.getName pat)
                                    val exp =
                                       Cexp.enterLeave
                                       (exp,
                                        profileBody
                                        andalso !Control.profileVal
                                        andalso Cexp.isExpansive exp, fn () =>
                                        let
                                           val name =
                                              concat ["<val ",
                                                      Layout.toString
                                                      (approximatePrefix
                                                       (Apat.layout pat)),
                                                      ">"]
                                        in
                                           SourceInfo.function {name = name :: nest,
                                                                region = regionExp}
                                        end)
                                 in
                                    {ctxtVb = ctxtVb,
                                     exp = exp,
                                     layPat = layPat,
                                     pat = pat,
                                     regionExp = regionExp,
                                     regionPat = regionPat}
                                 end)
                             val {markFunc, setBound, unmarkFunc} = recursiveFun ()
                             val elaboratePat = elaboratePat ()
                             val rvbs =
                                Vector.map2
                                (rvbs, layRvbs, fn ({pat, match}, layRvb) =>
                                 let
                                    fun ctxtRvb () =
                                       seq [str "in: ", approximate (layRvb ())]
                                    val regionPat = Apat.region pat
                                    val (pat, bound) =
                                       elaboratePat (pat, E, {bind = false, isRvb = true})
                                    val (nest, var) =
                                       if Vector.length bound = 1
                                          andalso (Type.isUnknown (Cpat.ty pat)
                                                   orelse Type.isArrow (Cpat.ty pat))
                                          then let
                                                  val (x, x', _) = Vector.first bound
                                               in
                                                  (Avar.toString x :: nest, x')
                                               end
                                          else ("_" :: nest, Var.newNoname ())
                                    val _ = markFunc var
                                    val bound =
                                       Vector.map
                                       (bound, fn (x, _, ty) =>
                                        let
                                           val xVid = Avid.fromVar x
                                           val _ =
                                              checkConRedefine
                                              (xVid, "val rec", ctxtRvb)
                                           val _ =
                                              Env.extendVar
                                              (E, x, var, Scheme.fromType ty,
                                               {isRebind = false})
                                        in
                                           (x, var, ty)
                                        end)
                                 in
                                    {bound = bound,
                                     ctxtRvb = ctxtRvb,
                                     match = match,
                                     nest = nest,
                                     pat = pat,
                                     regionPat = regionPat,
                                     patIsConstrained = not (Type.isUnknown (Cpat.ty pat)),
                                     var = var}
                                 end)
                             val vbs =
                                Vector.map
                                (vbs,
                                 fn {ctxtVb, exp, layPat, pat, regionExp, regionPat, ...} =>
                                 let
                                    val (pat, bound) =
                                       elaboratePat (pat, E, {bind = false, isRvb = false})
                                    val _ =
                                       unify
                                       (Cpat.ty pat, Cexp.ty exp, fn (p, e) =>
                                        (Region.append (regionPat, regionExp),
                                         str "pattern and expression disagree",
                                         align [seq [str "pattern:    ", p],
                                                seq [str "expression: ", e],
                                                ctxtVb ()]))
                                 in
                                    {bound = bound,
                                     ctxtVb = ctxtVb,
                                     exp = exp,
                                     layPat = layPat,
                                     pat = pat,
                                     regionPat = regionPat}
                                 end)
                             val rvbs =
                                Vector.map
                                (rvbs, fn {bound, ctxtRvb, match, nest, pat, patIsConstrained, regionPat, var, ...} =>
                                 let
                                    val {argType, region, resultType, rules} =
                                       elabMatch (match, nest)
                                    fun check () =
                                       unify
                                       (Cpat.ty pat,
                                        Type.arrow (argType, resultType),
                                        fn (l1, l2) =>
                                        if patIsConstrained
                                           then (Region.append (regionPat, Amatch.region match),
                                                 str "recursive function pattern and expression disagree",
                                                 align [seq [str "pattern:    ", l1],
                                                        seq [str "expression: ", l2],
                                                        ctxt ()])
                                           else (Avar.region (#1 (Vector.first bound)),
                                                 seq [str "recursive use of function disagrees with function expression type: ",
                                                      Avar.layout (#1 (Vector.first bound))],
                                                 align [seq [str "recursive use: ", l1],
                                                        seq [str "function type: ", l2],
                                                        ctxt ()]))
                                    val arg = Var.newNoname ()
                                    val body =
                                       Cexp.enterLeave
                                       (Cexp.casee {ctxt = ctxtRvb,
                                                    kind = ("recursive function", "rule"),
                                                    nest = nest,
                                                    matchDiags = matchDiagsFromNoMatch Cexp.RaiseMatch,
                                                    noMatch = Cexp.RaiseMatch,
                                                    region = region,
                                                    rules = rules,
                                                    test = Cexp.var (arg, argType)},
                                        profileBody,
                                        fn () => SourceInfo.function {name = nest,
                                                                      region = region})
                                    val lambda =
                                       Lambda.make {arg = arg,
                                                    argType = argType,
                                                    body = body,
                                                    mayInline = true}
                                 in
                                    {check = check,
                                     bound = bound,
                                     lambda = lambda,
                                     var = var}
                                 end)
                             val _ =
                                Vector.foreach
                                (rvbs, fn {check, ...} =>
                                 check ())
                             val boundVars =
                                Vector.concat
                                [Vector.concatV
                                 (Vector.map
                                  (rvbs, fn {bound, ...} =>
                                   ((Vector.rev o Vector.map)
                                    (bound, fn z =>
                                     (z, {isExpansive = false,
                                          isRebind = true}))))),
                                 Vector.concatV
                                 (Vector.map
                                  (vbs, fn {bound, exp, ...} =>
                                   ((Vector.rev o Vector.map)
                                    (bound, fn z =>
                                     (z, {isExpansive = Cexp.isExpansive exp,
                                          isRebind = false})))))]
                             val {bound, schemes} =
                                close
                                (tyvars',
                                 Vector.map
                                 (boundVars, fn ((var, _, ty), {isExpansive, ...}) =>
                                  {isExpansive = isExpansive,
                                   ty = ty,
                                   var = var}),
                                 {error = generalizeError,
                                  layoutPrettyTycon = layoutPrettyTycon,
                                  layoutPrettyTyvar = layoutPrettyTyvar})
                             val _ =
                                checkSchemes
                                (Vector.zip
                                 (Vector.map (boundVars, #1 o #1),
                                  schemes))
                             val _ = setBound bound
                             val _ =
                                Vector.foreach2
                                (boundVars, schemes,
                                 fn (((x, x', _), {isRebind, ...}), scheme) =>
                                 Env.extendVar
                                 (E, x, x', scheme,
                                  {isRebind = isRebind}))
                             val _ =
                                Vector.foreach
                                (rvbs, fn {var, ...} =>
                                 unmarkFunc var)
                             val vbs =
                                Vector.map
                                (vbs, fn {ctxtVb, exp, layPat, pat, regionPat, ...} =>
                                 {ctxt = ctxtVb,
                                  exp = exp,
                                  layPat = layPat,
                                  nest = nest,
                                  pat = pat,
                                  regionPat = regionPat})
                             val rvbs =
                                Vector.map
                                (rvbs, fn {lambda, var, ...} =>
                                 {lambda = lambda,
                                  var = var})
                             (* According to page 28 of the Definition, we should
                              * issue warnings for nonexhaustive valdecs only when it's
                              * not a top level dec.  It seems harmless enough to go
                              * ahead and always issue them.
                              *)
                          in
                             Decs.single
                             (Cdec.Val {matchDiags = matchDiagsFromNoMatch Cexp.RaiseBind,
                                        rvbs = rvbs,
                                        tyvars = bound,
                                        vbs = vbs})
                          end)
                      end
             val () =
                case resolveScope () of
                   Control.Elaborate.ResolveScope.Dec =>
                      (reportUnresolvedFlexRecords ()
                       ; resolveOverloads ())
                 | _ => ()
          in
             decs
          end) arg
      and elabExp (arg: Aexp.t * Nest.t * string option) : Cexp.t =
         Trace.traceInfo
         (elabExpInfo,
          Layout.tuple3
          (Aexp.layout,
           Nest.layout,
           Option.layout String.layout),
          Cexp.layoutWithType,
          Trace.assertTrue)
         (fn (e: Aexp.t, nest, maybeName) =>
          let
             fun elab e = elabExp (e, nest, NONE)
             val {layoutPrettyType, layoutPrettyTycon, layoutPrettyTyvar, unify} =
                DiagUtils.make E
             val layoutPrettyTypeBracket = fn ty =>
                seq [str "[", #1 (layoutPrettyType ty), str "]"]
             fun ctxt () = seq [str "in: ", approximate (Aexp.layout e)]
             val unify = fn (a, b, f) =>
                unify (a, b, fn z =>
                       let
                          val (r, m, d) = f z
                       in
                          (r, m, align [d, ctxt ()])
                       end)
             val region = Aexp.region e
          in
             case Aexp.node e of
                Aexp.Andalso (el, er) =>
                   let
                      fun doit (e, br) =
                         let
                            val ce = elab e
                            val _ =
                               unify
                               (Cexp.ty ce, Type.bool,
                                fn (l, _) =>
                                (Aexp.region e,
                                 str (concat
                                      [br, " branch of andalso not of type bool"]),
                                 seq [str "branch: ", l]))
                         in
                            ce
                         end
                      val cel = doit (el, "left")
                      val cer = doit (er, "right")
                      val e = Cexp.andAlso (cel, cer)
                   in
                      Cexp.make (Cexp.node e, Type.bool)
                   end
              | Aexp.App (ef, ea) =>
                   let
                      val cef = elab ef
                      val cea = elab ea
                      val isCon =
                         case Cexp.node cef of
                            Cexp.Con _ => true
                          | _ => false
                      val (argType, resultType) =
                         case Type.deArrowOpt (Cexp.ty cef) of
                            SOME types => types
                          | NONE =>
                               let
                                  val types = (Type.new (), Type.new ())
                                  val _ =
                                     unify (Cexp.ty cef, Type.arrow types,
                                            fn (l, _) =>
                                            if isCon
                                               then (Aexp.region ef,
                                                     str "constant constructor applied to argument",
                                                     seq [str "constructor: ", l])
                                               else (Aexp.region ef,
                                                     str "function not of arrow type",
                                                     seq [str "function: ", l]))
                               in
                                  types
                               end
                      val _ =
                         unify
                         (argType, Cexp.ty cea, fn (l1, l2) =>
                          (region,
                           seq [str (if isCon then "constructor" else "function"),
                                str " applied to incorrect argument"],
                           align [seq [str "expects: ", l1],
                                  seq [str "but got: ", l2]]))
                   in
                      Cexp.make (Cexp.App (cef, cea), resultType)
                   end
              | Aexp.Case (e, m) =>
                   let
                      val e = elab e
                      val {argType, rules, ...} = elabMatch (m, nest)
                      val _ =
                         unify
                         (Cexp.ty e, argType, fn (l1, l2) =>
                          (region,
                           str "case object and match argument disagree",
                           align [seq [str "case object:    ", l1],
                                  seq [str "match argument: ", l2]]))
                   in
                      Cexp.casee {ctxt = ctxt,
                                  kind = ("case", "rule"),
                                  nest = nest,
                                  matchDiags = matchDiagsFromNoMatch Cexp.RaiseMatch,
                                  noMatch = Cexp.RaiseMatch,
                                  region = Amatch.region m,
                                  rules = rules,
                                  test = e}
                   end
              | Aexp.Const c =>
                   elabConst
                   (c,
                    {layoutPrettyType = #1 o layoutPrettyType},
                    fn (resolve, ty) => Cexp.make (Cexp.Const resolve, ty),
                    {false = Cexp.falsee,
                     true = Cexp.truee})
              | Aexp.Constraint (e, t') =>
                   let
                      val e = elab e
                      val t' = elabType (t', {bogusAsUnknown = true})
                      val _ =
                         unify
                         (Cexp.ty e, t', fn (l1, l2) =>
                          (region,
                           str "expression and constraint disagree",
                           align [seq [str "expression: ", l1],
                                  seq [str "constraint: ", l2]]))
                   in
                      Cexp.make (Cexp.node e, t')
                   end
              | Aexp.FlatApp items => elab (Parse.parseExp (items, E, ctxt))
              | Aexp.Fn match =>
                   let
                      val nest =
                         case maybeName of
                            NONE => "fn" :: nest
                          | SOME s => s :: nest
                      val {arg, argType, body} =
                         elabMatchFn
                         (match, nest, ctxt,
                          ("function", "rule"), Cexp.RaiseMatch)
                      val body =
                         Cexp.enterLeave
                         (body,
                          profileBody,
                          fn () => SourceInfo.function {name = nest,
                                                        region = region})
                   in
                      Cexp.make (Cexp.Lambda (Lambda.make {arg = arg,
                                                           argType = argType,
                                                           body = body,
                                                           mayInline = true}),
                                 Type.arrow (argType, Cexp.ty body))
                   end
              | Aexp.Handle (try, match) =>
                   let
                      val try = elab try
                      val {arg, argType, body} =
                         elabMatchFn
                         (match, nest, ctxt,
                          ("handler", "rule"), Cexp.RaiseAgain)
                      val _ =
                         unify
                         (Cexp.ty try, Cexp.ty body, fn (l1, l2) =>
                          (region,
                           str "expression and handler disagree",
                           align [seq [str "expression: ", l1],
                                  seq [str "handler:    ", l2]]))
                      val _ =
                         unify
                         (argType, Type.exn, fn (l1, _) =>
                          (Amatch.region match,
                           str "handler match argument not of type exn",
                           seq [str "argument: ", l1]))
                   in
                      Cexp.make (Cexp.Handle {catch = (arg, Type.exn),
                                              handler = body,
                                              try = try},
                                 Cexp.ty try)
                   end
              | Aexp.If (a, b, c) =>
                   let
                      val a' = elab a
                      val b' = elab b
                      val c' = elab c
                      val _ =
                         unify
                         (Cexp.ty a', Type.bool, fn (l1, _) =>
                          (Aexp.region a,
                           str "if test not of type bool",
                           seq [str "test: ", l1]))
                      val _ =
                         unify
                         (Cexp.ty b', Cexp.ty c', fn (l1, l2) =>
                          (region,
                           str "then and else branches disagree",
                           align [seq [str "then: ", l1],
                                  seq [str "else: ", l2]]))
                      val (b', c') =
                         if not (!Control.profileBranch)
                            then (b', c')
                         else
                            let
                               fun wrap (e, e', name) =
                                  Cexp.enterLeave
                                  (e', profileBody, fn () =>
                                   SourceInfo.function
                                   {name = name :: nest,
                                    region = Aexp.region e})
                            in
                               (wrap (b, b', "<case true>"), wrap (c, c', "<case false>"))
                            end
                   in
                      Cexp.iff (a', b', c')
                   end
              | Aexp.Let (d, e) =>
                   let
                      val res =
                         Env.scope
                         (E, fn () =>
                          let
                             val time = Time.now ()
                             val d' = Decs.toVector (elabDec (d, nest, false))
                             val e' = elab e
                             val ty = Cexp.ty e'
                             val ty =
                                case Type.checkTime (ty, time,
                                                     {layoutPrettyTycon = layoutPrettyTycon,
                                                      layoutPrettyTyvar = layoutPrettyTyvar}) of
                                   NONE => ty
                                 | SOME (lay, ty, {tycons, ...}) =>
                                      let
                                         val tycons =
                                            List.map
                                            (tycons, fn c =>
                                             (c, layoutPrettyTycon c))
                                         val tycons =
                                            List.insertionSort
                                            (tycons, fn ((_, l1), (_, l2)) =>
                                             String.<= (Layout.toString l1,
                                                        Layout.toString l2))
                                         val _ =
                                            Control.error
                                            (region,
                                             seq [str "type of let has ",
                                                  if List.length tycons > 1
                                                     then str "local types that would escape their scope: "
                                                     else str "local type that would escape its scope: ",
                                                  seq (Layout.separate (List.map (tycons, #2), ", "))],
                                             align [seq [str "type: ", lay],
                                                    (align o List.map)
                                                    (tycons, fn (c, _) =>
                                                     seq [str "escape from: ",
                                                          Region.layout (Tycon.region c)]),
                                                    ctxt ()])
                                      in
                                         ty
                                      end
                          in
                             Cexp.make (Cexp.Let (d', e'), ty)
                          end)
                   in
                      res
                   end
              | Aexp.List es =>
                   let
                      val es' = Vector.map (es, elab)
                   in
                      Cexp.make (Cexp.List es',
                                 unifyList
                                 (Vector.map2 (es, es', fn (e, e') =>
                                               (Cexp.ty e', Aexp.region e)),
                                  unify))
                   end
              | Aexp.Orelse (el, er) =>
                   let
                      fun doit (e, br) =
                         let
                            val ce = elab e
                            val _ =
                               unify
                               (Cexp.ty ce, Type.bool,
                                fn (l, _) =>
                                (Aexp.region e,
                                 str (concat
                                      [br, " branch of orelse not of type bool"]),
                                 seq [str "branch: ", l]))
                         in
                            ce
                         end
                      val cel = doit (el, "left")
                      val cer = doit (er, "right")
                      val e = Cexp.orElse (cel, cer)
                   in
                      Cexp.make (Cexp.node e, Type.bool)
                   end
              | Aexp.Paren e => elab e
              | Aexp.Prim kind =>
                   let
                      fun elabAndExpandTy ty =
                         let
                            val elabedTy = elabType (ty, {bogusAsUnknown = false})
                            val expandedTy =
                               Type.hom
                               (elabedTy, {con = Type.con,
                                           expandOpaque = true,
                                           record = Type.record,
                                           replaceSynonyms = false,
                                           var = Type.var})
                         in
                            (elabedTy, expandedTy)
                         end
                      (* We use expandedTy to get the underlying primitive right
                       * but we use wrap in the end to make the result of the
                       * final expression be ty, because that is what the rest
                       * of the code expects to see.
                       *)
                      fun wrap (e, t) = Cexp.make (Cexp.node e, t)
                      fun etaExtraNoWrap {expandedTy,
                                          extra,
                                          prim: Type.t Prim.t}: Cexp.t =
                         case Type.deArrowOpt expandedTy of
                            NONE => primApp {args = extra,
                                             prim = prim,
                                             result = expandedTy}
                          | SOME (argType, bodyType) =>
                               let
                                  val arg = Var.newNoname ()
                                  fun app args =
                                     primApp {args = Vector.concat [extra, args],
                                              prim = prim,
                                              result = bodyType}
                                  val body =
                                     case Type.deTupleOpt argType of
                                        NONE =>
                                           app (Vector.new1
                                                (Cexp.var (arg, argType)))
                                      | SOME ts =>
                                           let
                                              val vars =
                                                 Vector.map
                                                 (ts, fn t =>
                                                  (Var.newNoname (), t))
                                           in
                                              Cexp.casee
                                              {ctxt = fn _ => empty,
                                               kind = ("", ""),
                                               nest = [],
                                               matchDiags = matchDiagsFromNoMatch Cexp.Impossible,
                                               noMatch = Cexp.Impossible,
                                               region = Region.bogus,
                                               rules = Vector.new1
                                                       {exp = app (Vector.map
                                                                   (vars, Cexp.var)),
                                                        layPat = NONE,
                                                        pat = Cpat.tuple
                                                              (Vector.map
                                                               (vars, Cpat.var)),
                                                        regionPat = Region.bogus},
                                               test = Cexp.var (arg, argType)}
                                           end
                               in
                                  (Cexp.lambda o Lambda.make)
                                  {arg = arg,
                                   argType = argType,
                                   body = body,
                                   mayInline = true}
                               end
                      fun etaNoWrap {expandedTy,
                                     prim: Type.t Prim.t} : Cexp.t =
                         etaExtraNoWrap {expandedTy = expandedTy,
                                         extra = Vector.new0 (),
                                         prim = prim}
                      fun eta {elabedTy, expandedTy,
                               prim: Type.t Prim.t} : Cexp.t =
                         wrap (etaNoWrap {expandedTy = expandedTy,
                                          prim = prim},
                               elabedTy)
                      fun lookConst {default: string option,
                                     elabedTy, expandedTy,
                                     name: string} =
                         let
                            fun bug () =
                               let
                                  val _ =
                                     Control.error
                                     (region,
                                      seq [str "strange constant type: ",
                                           Type.layout expandedTy],
                                      empty)
                               in
                                  Error.bug "ElaborateCore.elabExp.lookConst"
                               end
                         in
                            case Type.deConOpt expandedTy of
                               NONE => bug ()
                             | SOME (c, ts) =>
                                  let
                                     val ct =
                                        if Tycon.equals (c, Tycon.bool)
                                           then ConstType.Bool
                                        else if Tycon.isIntX c
                                           then case Tycon.deIntX c of
                                                   NONE => bug ()
                                                 | SOME is =>
                                                      ConstType.Word
                                                      (WordSize.fromBits (IntSize.bits is))
                                        else if Tycon.isRealX c
                                           then ConstType.Real (Tycon.deRealX c)
                                        else if Tycon.isWordX c
                                           then ConstType.Word (Tycon.deWordX c)
                                        else if Tycon.equals (c, Tycon.vector)
                                           andalso 1 = Vector.length ts
                                           andalso
                                           (case (Type.deConOpt
                                                  (Vector.first ts)) of
                                               NONE => false
                                             | SOME (c, _) =>
                                                  Tycon.isCharX c
                                                  andalso (Tycon.deCharX c = CharSize.C8))
                                           then ConstType.String
                                        else bug ()
                                  val finish =
                                     fn () => ! Const.lookup ({default = default,
                                                               name = name}, ct)
                                  in
                                     Cexp.make (Cexp.Const finish, elabedTy)
                                  end
                         end
                      val check = fn (c, n) => check (c, n, region)
                      datatype z = datatype Ast.PrimKind.t
                   in
                      case kind of
                         Address {attributes, name, ty} =>
                            let
                               val () =
                                  check (ElabControl.allowFFI, "_address")
                               val (elabedTy, expandedTy) =
                                  elabAndExpandTy ty
                            in
                               address {attributes = attributes,
                                        elabedTy = elabedTy,
                                        expandedTy = expandedTy,
                                        name = name,
                                        region = region,
                                        layoutPrettyType = #1 o layoutPrettyType}
                            end
                       | BuildConst {name, ty} =>
                            let
                               val () =
                                  check (ElabControl.allowConstant,
                                         "_build_const")
                               val (elabedTy, expandedTy) =
                                  elabAndExpandTy ty
                            in
                               lookConst {default = NONE,
                                          elabedTy = elabedTy,
                                          expandedTy = expandedTy,
                                          name = name}
                            end
                       | CommandLineConst {name, ty, value} =>
                            let
                               val () =
                                  check (ElabControl.allowConstant,
                                         "_command_line_const")
                               val (elabedTy, expandedTy) =
                                  elabAndExpandTy ty
                               val value =
                                  elabConst
                                  (value,
                                   {layoutPrettyType = #1 o layoutPrettyType},
                                   fn (resolve, _) =>
                                   case resolve () of
                                      Const.Word w =>
                                         IntInf.toString (WordX.toIntInf w)
                                    | c => Const.toString c,
                                   {false = "false", true = "true"})
                            in
                               lookConst {default = SOME value,
                                          elabedTy = elabedTy,
                                          expandedTy = expandedTy,
                                          name = name}
                            end
                       | Const {name, ty} =>
                            let
                               val () =
                                  check (ElabControl.allowConstant,
                                         "_const")
                               val (elabedTy, expandedTy) =
                                  elabAndExpandTy ty
                            in
                               lookConst {default = NONE,
                                          elabedTy = elabedTy,
                                          expandedTy = expandedTy,
                                          name = name}
                            end
                       | Export {attributes, name, ty} =>
                            let
                               val () =
                                  check (ElabControl.allowFFI, "_export")
                               val (elabedTy, expandedTy) =
                                  elabAndExpandTy ty
                               fun error () =
                                  Control.error
                                  (region,
                                   str "invalid type for _export",
                                   #1 (layoutPrettyType elabedTy))
                               val (expandedCfTy, elabedExportTy) =
                                  Exn.withEscape
                                  (fn escape =>
                                   let
                                      val error = fn () =>
                                         (error ()
                                          ; ignore (escape (Type.arrow (Type.unit, Type.unit),
                                                            elabedTy))
                                          ; Error.bug "ElaborateCore.elabExp.Export.escape")
                                   in
                                      case Type.deArrowOpt expandedTy of
                                         NONE => error ()
                                       | SOME (argTy, resTy) =>
                                            (case Type.deArrowOpt argTy of
                                                NONE => error ()
                                              | SOME _ =>
                                                   let
                                                      val () =
                                                         if Type.isUnit resTy
                                                            then ()
                                                            else error ()
                                                   in
                                                      (argTy, elabedTy)
                                                   end)
                                   end)
                               val exp =
                                  Env.scope
                                  (E, fn () =>
                                   (Env.openStructure
                                    (E, valOf (!Env.Structure.ffi))
                                    ; elab (export {attributes = attributes,
                                                    elabedTy = elabedTy,
                                                    expandedTy = expandedCfTy,
                                                    name = name,
                                                    region = region,
                                                    layoutPrettyType = #1 o layoutPrettyType})))
                               val _ =
                                  unify
                                  (Cexp.ty exp,
                                   Type.arrow (expandedCfTy, Type.unit),
                                   fn (l1, l2) =>
                                   (region,
                                    str "_export unify bug",
                                    align [seq [str "inferred: ", l1],
                                           seq [str "expanded: ", l2]]))
                            in
                               wrap (exp, elabedExportTy)
                            end
                       | IImport {attributes, ty} =>
                            let
                               val () =
                                  check (ElabControl.allowFFI, "_import")
                               val (elabedTy, expandedTy) =
                                  elabAndExpandTy ty
                               fun error () =
                                  Control.error
                                  (region,
                                   str "invalid type for _import",
                                   #1 (layoutPrettyType elabedTy))
                               val (expandedFPtrTy, expandedCfTy) =
                                  Exn.withEscape
                                  (fn escape =>
                                   let
                                      val error = fn () =>
                                         (error ()
                                          ; ignore (escape (Type.cpointer,
                                                            Type.arrow (Type.unit, Type.unit)))
                                          ; Error.bug "ElaborateCore.elabExp.IImport.escape")
                                   in
                                      case Type.deArrowOpt expandedTy of
                                         NONE => error ()
                                       | SOME (fptrTy, cfTy) => (fptrTy, cfTy)
                                   end)
                               val () =
                                  case Type.toCPtrType expandedFPtrTy of
                                     NONE => (error (); ())
                                   | SOME _ => ()
                               val fptr = Var.newNoname ()
                               val fptrArg = Cexp.var (fptr, expandedFPtrTy)
                            in
                               wrap
                               ((Cexp.lambda o Lambda.make)
                                {arg = fptr,
                                 argType = expandedFPtrTy,
                                 body = etaExtraNoWrap {expandedTy = expandedCfTy,
                                                        extra = Vector.new1 fptrArg,
                                                        prim = import
                                                               {attributes = attributes,
                                                                name = NONE,
                                                                region = region,
                                                                elabedTy = elabedTy,
                                                                expandedTy = expandedCfTy,
                                                                layoutPrettyType = #1 o layoutPrettyType}},
                                 mayInline = true},
                                elabedTy)
                            end
                       | Import {attributes, name, ty} =>
                            let
                               val () =
                                  check (ElabControl.allowFFI, "_import")
                               val (elabedTy, expandedTy) =
                                  elabAndExpandTy ty
                            in
                               eta ({elabedTy = elabedTy,
                                     expandedTy = expandedTy,
                                     prim = import {attributes = attributes,
                                                    name = SOME name,
                                                    region = region,
                                                    elabedTy = elabedTy,
                                                    expandedTy = expandedTy,
                                                    layoutPrettyType = #1 o layoutPrettyType}})
                            end
                       | ISymbol {ty} =>
                            let
                               val () =
                                  check (ElabControl.allowFFI, "_symbol")
                               val (elabedTy, expandedTy) =
                                  elabAndExpandTy ty
                            in
                               symbolIndirect {elabedTy = elabedTy,
                                               expandedTy = expandedTy,
                                               region = region,
                                               layoutPrettyType = #1 o layoutPrettyType}
                            end
                       | Prim {name, ty} =>
                            let
                               val () =
                                  check (ElabControl.allowPrim,
                                         "_prim")
                               val (elabedTy, expandedTy) =
                                  elabAndExpandTy ty
                               val prim =
                                  case Prim.fromString name of
                                     NONE =>
                                        (Control.error
                                         (region,
                                          str (concat ["unknown primitive: ",
                                                       name]),
                                          empty)
                                         ; Prim.bogus)
                                   | SOME p => p
                            in
                               eta {elabedTy = elabedTy,
                                    expandedTy = expandedTy,
                                    prim = prim}
                            end
                       | Symbol {attributes, name, ty} =>
                            let
                               val () =
                                  check (ElabControl.allowFFI, "_symbol")
                               val (elabedTy, expandedTy) =
                                  elabAndExpandTy ty
                            in
                               symbolDirect {attributes = attributes,
                                             elabedTy = elabedTy,
                                             expandedTy = expandedTy,
                                             name = name,
                                             region = region,
                                             layoutPrettyType = #1 o layoutPrettyType}
                            end
                   end
              | Aexp.Raise exn =>
                   let
                      val region = Aexp.region exn
                      val exn = elab exn
                      val _ =
                         unify
                         (Cexp.ty exn, Type.exn, fn (l1, _) =>
                          (region,
                           str "raise object not of type exn",
                           seq [str "object: ", l1]))
                      val resultType = Type.new ()
                   in
                      Cexp.enterLeave
                      (Cexp.make (Cexp.Raise exn, resultType),
                       profileBody andalso !Control.profileRaise,
                       fn () => SourceInfo.function {name = "<raise>" :: nest,
                                                     region = region})
                   end
              | Aexp.Record r =>
                   let
                      val r = Record.map (r, elab o #2)
                      val ty =
                         Type.record
                         (SortedRecord.fromVector
                          (Record.toVector (Record.map (r, Cexp.ty))))
                   in
                      Cexp.make (Cexp.Record r, ty)
                   end
              | Aexp.Selector f => elab (Aexp.selector (f, region))
              | Aexp.Seq es =>
                   let
                      val es' = Vector.map (es, elab)
                      val last = Vector.length es - 1
                      (* Diagnose expressions before a ; that don't return unit. *)
                      val _ =
                         let
                            (* Technically, wrong scope for region;
                             * but saving environment would probably
                             * be expensive.
                             *)
                            fun doit f =
                               Vector.foreachi2
                               (es, es', fn (i, e, e') =>
                                if i = last orelse Type.isUnit (Cexp.ty e')
                                   then ()
                                   else List.push
                                        (sequenceNonUnitChecks, fn () =>
                                         if Type.isUnit (Cexp.ty e')
                                            then ()
                                            else f (Aexp.region e,
                                                    str "sequence expression not of type unit",
                                                    align [seq [str "type: ", layoutPrettyTypeBracket (Cexp.ty e')],
                                                           ctxt ()])))
                         in
                            case sequenceNonUnit () of
                               Control.Elaborate.DiagEIW.Error => doit Control.error
                             | Control.Elaborate.DiagEIW.Ignore => ()
                             | Control.Elaborate.DiagEIW.Warn => doit Control.warning
                         end
                   in
                      Cexp.make (Cexp.Seq es', Cexp.ty (Vector.sub (es', last)))
                   end
              | Aexp.Var {name = id, ...} =>
                   let
                      fun dontCare () =
                         Cexp.var (Var.newNoname (), Type.new ())
                   in
                      case Env.lookupLongvid (E, id) of
                         NONE => dontCare ()
                       | SOME (vid, scheme) =>
                            let
                               val {args, instance} = Scheme.instantiate scheme
                               fun con c = Cexp.Con (c, args ())
                               val e =
                                  case vid of
                                     Vid.Con c => con c
                                   | Vid.Exn c => con c
                                   | Vid.Overload (p, yts) =>
                                        let
                                           val resolve =
                                              Promise.lazy
                                              (fn () =>
                                               case Vector.peekMap
                                                    (yts,
                                                     fn (x, s) =>
                                                     let
                                                        val is = Scheme.instantiate s
                                                     in
                                                        if Type.canUnify
                                                           (instance, #instance is)
                                                           then SOME (x, SOME is)
                                                           else NONE
                                                     end) of
                                                  NONE =>
                                                     let
                                                        (* Technically, wrong scope for region;
                                                         * but saving environment would probably
                                                         * be expensive.
                                                         *)
                                                        val _ =
                                                           Control.error
                                                           (region,
                                                            seq [str "variable not overloaded at type: ",
                                                                 str (Longvid.toString id)],
                                                            seq [str "type: ", #1 (layoutPrettyType instance)])
                                                     in
                                                        {id = Var.newNoname (),
                                                         args = Vector.new0 ()}
                                                     end
                                                | SOME (y, is) =>
                                                     (unify (instance,
                                                             #instance (valOf is), fn _ =>
                                                             Error.bug "ElaborateCore.elabExp: Var:overload unify")
                                                      ; {id = y, args = #args (valOf is) ()}))
                                           val _ =
                                              List.push (overloadChecks, (p, ignore o resolve))
                                        in
                                           Cexp.Var (#id o resolve, #args o resolve)
                                        end
                                   | Vid.Var x =>
                                        Cexp.Var (fn () => x,
                                                  case ! (recursiveTargs x) of
                                                     NONE => args
                                                   | SOME f => f)
                            in
                               Cexp.make (e, instance)
                            end
                   end
              | Aexp.Vector es =>
                   let
                      val _ = check (ElabControl.allowVectorExps, "Vector expressions", Aexp.region e)
                      val es' = Vector.map (es, elab)
                   in
                      Cexp.make (Cexp.Vector es',
                                 unifyVector
                                 (Vector.map2 (es, es', fn (e, e') =>
                                               (Cexp.ty e', Aexp.region e)),
                                  unify))
                   end
              | Aexp.While {expr, test} =>
                   let
                      val test' = elab test
                      val _ =
                         unify
                         (Cexp.ty test', Type.bool, fn (l1, _) =>
                          (Aexp.region test,
                           str "while test not of type bool",
                           seq [str "test: ", l1]))
                      val expr' = elab expr
                      (* Diagnose if expr is not of type unit. *)
                      val _ =
                         let
                            (* Technically, wrong scope for region;
                             * but saving environment would probably
                             * be expensive.
                             *)
                            fun doit f =
                               if Type.isUnit (Cexp.ty expr')
                                  then ()
                                  else List.push
                                       (sequenceNonUnitChecks, fn () =>
                                        if Type.isUnit (Cexp.ty expr')
                                           then ()
                                           else f (Aexp.region expr,
                                                   str "while body not of type unit",
                                                   align [seq [str "body: ", layoutPrettyTypeBracket (Cexp.ty expr')],
                                                          ctxt ()]))
                         in
                            case sequenceNonUnit () of
                               Control.Elaborate.DiagEIW.Error => doit Control.error
                             | Control.Elaborate.DiagEIW.Ignore => ()
                             | Control.Elaborate.DiagEIW.Warn => doit Control.warning
                         end
                   in
                      Cexp.whilee {expr = expr', test = test'}
                   end
          end) arg
      and elabMatchFn (m: Amatch.t, nest, ctxt, kind, noMatch) =
         let
            val arg = Var.newNoname ()
            val {argType, region, rules, ...} = elabMatch (m, nest)
            val body =
               Cexp.casee {ctxt = ctxt,
                           kind = kind,
                           nest = nest,
                           matchDiags = matchDiagsFromNoMatch noMatch,
                           noMatch = noMatch,
                           region = region,
                           rules = rules,
                           test = Cexp.var (arg, argType)}
         in
           {arg = arg,
            argType = argType,
            body = body}
         end
      and elabMatch (m: Amatch.t, nest: Nest.t) =
         let
            val {unify, ...} = DiagUtils.make E
            fun ctxt () =
               seq [str "in: ", approximate (Amatch.layout m)]
            val unify = fn (a, b, f) =>
               unify (a, b, fn z =>
                      let
                         val (r, m, d) = f z
                      in
                         (r, m, align [d, ctxt ()])
                      end)
            val region = Amatch.region m
            val Amatch.T rules = Amatch.node m
            val argType = Type.new ()
            val resultType = Type.new ()
            val rules =
               Vector.map
               (rules, fn (pat, exp) =>
                Env.scope
                (E, fn () =>
                 let
                    fun layPat () = approximate (Apat.layout pat)
                    val patOrig = pat
                    val (pat, _) =
                       elaboratePat () (pat, E, {bind = true, isRvb = false})
                    val _ =
                       unify
                       (Cpat.ty pat, argType, fn (l1, l2) =>
                        (Apat.region patOrig,
                         str "rule with pattern of different type",
                         align [seq [str "pattern:  ", l1],
                                seq [str "previous: ", l2]]))
                    val expOrig = exp
                    val exp = elabExp (exp, nest, NONE)
                    val _ =
                       unify
                       (Cexp.ty exp, resultType, fn (l1, l2) =>
                        (Aexp.region expOrig,
                         str "rule with result of different type",
                         align [seq [str "result:   ", l1],
                                seq [str "previous: ", l2]]))
                    val exp =
                       Cexp.enterLeave
                       (exp,
                        profileBody andalso !Control.profileBranch,
                        fn () =>
                        let
                           val name =
                              concat ["<case ",
                                      Layout.toString
                                      (approximatePrefix
                                       (Apat.layout patOrig)),
                                      ">"]
                        in
                           SourceInfo.function {name = name :: nest,
                                                region = Aexp.region expOrig}
                        end)
                 in
                    {exp = exp,
                     layPat = SOME layPat,
                     pat = pat,
                     regionPat = Apat.region patOrig}
                 end))
         in
            {argType = argType,
             region = region,
             resultType = resultType,
             rules = rules}
         end
      val ds = elabDec (Scope.scope d, nest, true)
   in
      ds
   end

end
