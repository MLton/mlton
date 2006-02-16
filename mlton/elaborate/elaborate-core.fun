(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor ElaborateCore (S: ELABORATE_CORE_STRUCTS): ELABORATE_CORE = 
struct

open S

local
   open Control.Elaborate
in
   val allowRebindEquals = fn () => current allowRebindEquals
   val nonexhaustiveExnMatch = fn () => current nonexhaustiveExnMatch
   val nonexhaustiveMatch = fn () => current nonexhaustiveMatch
   val redundantMatch = fn () => current redundantMatch
   val sequenceNonUnit = fn () => current sequenceNonUnit
end

local
   open Ast
in
   structure Acon = Con
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
   structure Longvid = Longvid
   structure Longtycon = Longtycon
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
   structure TypeEnv = TypeEnv
   structure TypeStr = TypeStr
   structure Vid = Vid
end

local
   open TypeStr
in
   structure Kind = Kind
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
                  then getName (Vector.sub (v, 0))
               else NONE
          | Layered {var, ...} => SOME (Avar.toString var)
          | _ => NONE

      val getName =
         Trace.trace ("ElaborateCore.Apat.getName", layout, Option.layout String.layout)
         getName
   end

structure Lookup =
   struct
      type t = Longtycon.t -> TypeStr.t option

      fun fromEnv (E: Env.t) longtycon = Env.lookupLongtycon (E, longtycon)
   end

fun elaborateType (ty: Atype.t, lookup: Lookup.t): Type.t =
   let 
      fun loop (ty: Atype.t): Type.t =
         case Atype.node ty of
            Atype.Var a => (* rule 44 *)
               Type.var a
          | Atype.Con (c, ts) => (* rules 46, 47 *)
               let
                  val ts = Vector.map (ts, loop)
                  fun normal () =
                     case lookup c of
                        NONE => Type.new ()
                      | SOME s => 
                           let
                              val kind = TypeStr.kind s
                              val numArgs = Vector.length ts
                           in
                              if (case kind of
                                     Kind.Arity n => n = numArgs
                                   | Kind.Nary => true)
                                 then TypeStr.apply (s, ts)
                              else
                                 let
                                    open Layout
                                    val _ = 
                                       Control.error
                                       (Atype.region ty,
                                        seq [str "type ",
                                             Ast.Longtycon.layout c,
                                             str " given ",
                                             Int.layout numArgs,
                                             str (if numArgs = 1
                                                     then " argument"
                                                  else " arguments"),
                                             str " but wants ",
                                             Kind.layout kind],
                                        empty)
                                 in
                                    Type.new ()
                                 end
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
          | Atype.Record r => (* rules 45, 49 *)
               Type.record (SortedRecord.map (r, loop))
   in
      loop ty
   end

val overloads: (Ast.Priority.t * (unit -> unit)) list ref = ref []

val freeTyvarChecks: (unit -> unit) list ref = ref []

val sequenceTypeChecks: (unit -> unit) list ref = ref []

val {hom = typeTycon: Type.t -> Tycon.t option, ...} =
   Type.makeHom {con = fn (c, _) => SOME c,
                 expandOpaque = false,
                 var = fn _ => NONE}

val typeTycon =
   Trace.trace 
   ("ElaborateCore.typeTycon", Type.layout, Option.layout Tycon.layout) 
   typeTycon

fun 'a elabConst (c: Aconst.t,
                  make: (unit -> Const.t) * Type.t -> 'a,
                  {false = f: 'a, true = t: 'a}): 'a =
   let
      fun error (ty: Type.t): unit =
         let
            open Layout
         in
            Control.error
            (Aconst.region c,
             seq [Type.layoutPretty ty, str " too big: ", Aconst.layout c],
             empty)
         end
      fun ensureChar (cs: CharSize.t, ch: IntInf.t): unit =
         if CharSize.isInRange (cs, ch)
            then ()
         else
            let
               open Layout
            in
               Control.error (Aconst.region c,
                              str (concat
                                   ["character too big: ",
                                    "#\"", Aconst.ordToString ch, "\""]),
                              empty)
            end
      fun choose (tycon, all, sizeTycon, make) =
         case List.peek (all, fn s => Tycon.equals (tycon, sizeTycon s)) of
            NONE => Const.string "<bogus>"
          | SOME s => make s
      fun delay (ty: unit -> Type.t, resolve: Type.t -> Const.t): 'a =
         let
            val ty = ty ()
            val resolve = Promise.lazy (fn () => resolve ty)
            val _ = List.push (overloads, (Priority.default, ignore o resolve))
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
       | Aconst.Char c =>
            delay
            (Type.unresolvedChar, fn ty =>
             choose (typeTycon ty,
                     List.map ([8, 16, 32], WordSize.fromBits o Bits.fromInt),
                     Tycon.word,
                     fn s =>
                     (ensureChar (CharSize.fromBits (WordSize.bits s), c)
                      ; Const.Word (WordX.fromIntInf (c, s)))))
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
                            else (error ty; WordX.zero s)))
             end)
       | Aconst.Real r =>
            delay
            (Type.unresolvedReal, fn ty =>
             choose (typeTycon ty, RealSize.all, Tycon.real, fn s =>
                     Const.Real (case RealX.make (r, s) of
                                    NONE => (error ty; RealX.zero s)
                                  | SOME r => r)))
       | Aconst.String v =>
            delay
            (Type.unresolvedString, fn ty =>
             choose (typeTycon (Type.deVector ty),
                     List.map ([8, 16, 32], WordSize.fromBits o Bits.fromInt),
                     Tycon.word,
                     fn s =>
                     let
                        val cs = CharSize.fromBits (WordSize.bits s)
                     in
                        Const.WordVector
                        (WordXVector.tabulate
                         ({elementSize = s}, Vector.length v, fn i =>
                          let
                             val ch = Vector.sub (v, i)
                             val () = ensureChar (cs, ch)
                          in
                             WordX.fromIntInf (ch, s)
                          end))
                     end))
       | Aconst.Word w =>
            delay
            (Type.unresolvedWord, fn ty =>
             choose (typeTycon ty, WordSize.all, Tycon.word, fn s =>
                     Const.Word
                     (if WordSize.isInRange (s, w, {signed = false})
                         then WordX.fromIntInf (w, s)
                      else (error ty; WordX.zero s))))
   end

local
   open Layout
in
   val align = align
   val empty = empty
   val seq = seq
   val str = str
end

val unify =
   fn (t, t', preError, error) =>
   Type.unify (t, t', {error = Control.error o error,
                       preError = preError})
   
fun unifyList (trs: (Type.t * Region.t) vector,
               z,
               lay: unit -> Layout.t): Type.t =
   if 0 = Vector.length trs
      then Type.list (Type.new ())
   else
      let
         val (t, _) = Vector.sub (trs, 0)
         val _ =
            Vector.foreach
            (trs, fn (t', r) =>
             unify (t, t', z, fn (l, l') =>
                    (r,
                     str "list element types disagree",
                     align [seq [str "element:  ", l'],
                            seq [str "previous: ", l],
                            lay ()])))
      in
         Type.list t
      end

val elabPatInfo = Trace.info "ElaborateCore.elabPat"

structure Var =
   struct
      open Var

      val fromAst = fromString o Avar.toString
   end

local
   val eq = Avar.fromSymbol (Symbol.equal, Region.bogus)
in
   fun ensureNotEquals x =
      if not (allowRebindEquals ()) andalso Avar.equals (x, eq)
         then
            let
               open Layout
            in
               Control.error (Avar.region x, str "= can't be redefined", empty)
            end
      else ()
end

fun approximate (l: Layout.t): Layout.t =
   let
      val s = Layout.toString l
      val n = String.size s
   in
      Layout.str
      (if n <= 60
          then s
       else concat [String.prefix (s, 35), "  ...  ", String.suffix (s, 25)])
   end

val elaboratePat:
   unit
   -> Apat.t * Env.t * {bind: bool, isRvb: bool} * (unit -> unit)
   -> Cpat.t * (Avar.t * Var.t * Type.t) vector =
   fn () =>
   let
      val others: (Apat.t * (Avar.t * Var.t * Type.t) vector) list ref = ref []
   in
      fn (p: Apat.t, E: Env.t, {bind, isRvb}, preError: unit -> unit) =>
      let
         val xts: (Avar.t * Var.t * Type.t) list ref = ref []
         fun bindToType (x: Avar.t, t: Type.t): Var.t =
            let
               val _ = ensureNotEquals x
               val x' = Var.fromAst x
               val _ =
                  if List.exists (!xts, fn (x', _, _) => Avar.equals (x, x'))
                     then
                        let
                           open Layout
                        in
                           Control.error (Avar.region x,
                                          seq [str "variable ",
                                               Avar.layout x,
                                               str " occurs more than once in pattern"],
                                          seq [str "in: ",
                                               approximate (Apat.layout p)])
                        end
                  else ()
               val _ =
                  case (List.peekMap
                        (!others, fn (p, v) =>
                         if Vector.exists (v, fn (x', _, _) =>
                                           Avar.equals (x, x'))
                            then SOME p
                         else NONE)) of
                     NONE => ()
                   | SOME p' => 
                        let
                           open Layout
                        in
                           Control.error
                           (Apat.region p,
                            seq [str "variable ",
                                 Avar.layout x,
                                 str " occurs in multiple patterns"],
                            align [seq [str "in: ",
                                        approximate (Apat.layout p)],
                                   seq [str "and in: ",
                                        approximate (Apat.layout p')]])

                        end
               val _ = List.push (xts, (x, x', t))
               val _ =
                  if bind
                     then Env.extendVar (E, x, x', Scheme.fromType t,
                                         {isRebind = false})
                  else ()
            in
               x'
            end
         fun bind (x: Avar.t): Var.t * Type.t =
            let
               val t = Type.new ()
            in
               (bindToType (x, t), t)
            end
         fun loop arg: Cpat.t =
            Trace.traceInfo' (elabPatInfo, Apat.layout, Cpat.layout)
            (fn p: Apat.t =>
             let
                val region = Apat.region p
                val unify = fn (t, t', f) => unify (t, t', preError, f)
                fun unifyPatternConstraint (p, lay, c) =
                   unify
                   (p, c, fn (l1, l2) =>
                    (region,
                     str "pattern and constraint disagree",
                     align [seq [str "expects: ", l2],
                            seq [str "but got: ", l1],
                            seq [str "in: ", lay ()]]))
                fun lay () = approximate (Apat.layout p)
                fun dontCare () =
                   Cpat.wild (Type.new ())
             in
                case Apat.node p of
                   Apat.App (c, p) =>
                      let
                         val (con, s) = Env.lookupLongcon (E, c)
                      in
                         case s of
                            NONE => dontCare ()
                          | SOME s => 
                               let
                                  val {args, instance} = Scheme.instantiate s
                                  val args = args ()
                                  val p = loop p
                                  val argType = Type.new ()
                                  val resultType = Type.new ()
                                  val _ =
                                     unify
                                     (instance, Type.arrow (argType, resultType),
                                      fn _ =>
                                      (region,
                                       str "constant constructor applied to argument",
                                       seq [str "in: ", lay ()]))
                                  val _ =
                                     unify
                                     (Cpat.ty p, argType, fn (l, l') =>
                                      (region,
                                       str "constructor applied to incorrect argument",
                                       align [seq [str "expects: ", l'],
                                              seq [str "but got: ", l],
                                              seq [str "in: ", lay ()]]))
                               in
                                  Cpat.make (Cpat.Con {arg = SOME p,
                                                       con = con,
                                                       targs = args},
                                             resultType)
                               end
                      end
                 | Apat.Const c =>
                      elabConst
                      (c,
                       fn (resolve, ty) => Cpat.make (Cpat.Const resolve, ty),
                       {false = Cpat.falsee,
                        true = Cpat.truee})
                 | Apat.Constraint (p, t) =>
                      let
                         val p' = loop p
                         val _ =
                            unifyPatternConstraint
                            (Cpat.ty p', fn () => Apat.layout p,
                             elaborateType (t, Lookup.fromEnv E))
                      in
                         p'
                      end
                 | Apat.FlatApp items =>
                      loop (Parse.parsePat
                            (items, E, fn () => seq [str "in: ", lay ()]))
                 | Apat.Layered {var = x, constraint, pat, ...} =>
                      let
                         val t =
                            case constraint of
                               NONE => Type.new ()
                             | SOME t => elaborateType (t, Lookup.fromEnv E)
                         val x = bindToType (x, t)
                         val pat' = loop pat
                         val _ =
                            unifyPatternConstraint (Cpat.ty pat',
                                                    fn () => Apat.layout pat,
                                                    t)
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
                                     preError,
                                     fn () => seq [str "in:  ", lay ()]))
                      end
                 | Apat.Record {flexible, items} =>
                      (* rules 36, 38, 39 and Appendix A, p.57 *)
                      let
                         val (fs, ps) =
                            Vector.unzip
                            (Vector.map
                             (items,
                              fn (f, i) =>
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
                                            seq [str "in: ", lay ()])
                                     val _ = List.push (overloads, (Priority.default, resolve))
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
                      let
                         val ps = Vector.map (ps, loop)
                      in
                         Cpat.make (Cpat.Tuple ps,
                                    Type.tuple (Vector.map (ps, Cpat.ty)))
                      end
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
                               let
                                  val _ =
                                     if not isRvb
                                        then ()
                                     else
                                        Control.error
                                        (region,
                                         seq [str "constructor can not be redefined by val rec: ",
                                              Ast.Longvid.layout name],
                                         empty)
                               in
                                  case s of
                                     NONE => dontCare ()
                                   | SOME s =>
                                        let 
                                           val {args, instance} =
                                              Scheme.instantiate s
                                        in
                                           if Type.canUnify
                                              (instance,
                                               Type.arrow (Type.new (),
                                                           Type.new ()))
                                              then
                                                 (Control.error
                                                  (region,
                                                   seq [str "contructor must be used with argument in pattern: ",
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
                      end
                 | Apat.Wild =>
                      Cpat.make (Cpat.Wild, Type.new ())
             end) arg
         val p' = loop p
         val xts = Vector.fromList (!xts)
         val _ = List.push (others, (p, xts))
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

      fun layoutPrettyBracket ty =
         let 
            open Layout
         in
            seq [str "[", layoutPretty ty, str "]"]
         end

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
             ("Pointer", CType.pointer, Tycon.pointer),
             ("Real32", CType.real RealSize.R32, Tycon.real RealSize.R32),
             ("Real64", CType.real RealSize.R64, Tycon.real RealSize.R64),
             ("Thread", CType.thread, Tycon.thread)]
            @ sized (Tycon.char o CharSize.fromBits,
                     let
                        open CType
                     in
                        [Int8, Int16, Int32]
                     end)
            @ sized (Tycon.int o IntSize.I,
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

      fun toCType (t: t): {ctype: CType.t, name: string} option =
         case deConOpt t of
            NONE => NONE
          | SOME (c, ts) =>
               case List.peek (nullary, fn {tycon = c', ...} =>
                               Tycon.equals (c, c')) of
                  NONE =>
                     if List.exists (unary, fn c' => Tycon.equals (c, c'))
                        andalso 1 = Vector.length ts
                        andalso isSome (toCType (Vector.sub (ts, 0)))
                        then SOME {ctype = CType.pointer, name = "Pointer"}
                     else NONE
                | SOME {ctype, name, ...} => SOME {ctype = ctype, name = name}

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

      fun parse (ty: t): (z vector * z option) option =
         case deArrowOpt ty of
            NONE => NONE
          | SOME (t1, t2) =>
               let
                  fun finish (ts: z vector) =
                     case toCType t2 of
                        NONE =>
                           if Type.isUnit t2
                              then SOME (ts, NONE)
                           else NONE
                      | SOME {ctype, name} =>
                           SOME (ts, SOME {ctype = ctype, name = name, ty = t2})
               in
                  case deTupleOpt t1 of 
                     NONE =>
                        (case toCType t1 of
                            NONE => NONE
                          | SOME {ctype, name} =>
                               finish (Vector.new1 {ctype = ctype,
                                                    name = name,
                                                    ty = t1}))
                   | SOME ts =>
                        let
                           val cts = Vector.map (ts, toCType)
                        in
                           if Vector.forall (cts, isSome)
                              then
                                 finish (Vector.map2
                                         (ts, cts, fn (ty, z) =>
                                          let
                                             val {ctype, name} = valOf z
                                          in
                                             {ctype = ctype,
                                              name = name,
                                              ty = ty}
                                          end))
                           else NONE
                        end
               end
   end

fun parseIEAttributes (attributes: ImportExportAttribute.t list): Convention.t option =
   case attributes of
      [] => SOME Convention.Cdecl
    | [a] =>
         SOME (case a of
                  ImportExportAttribute.Cdecl => Convention.Cdecl
                | ImportExportAttribute.Stdcall =>
                     if let
                           open Control
                        in
                           case !targetOS of
                              Cygwin => true
                            | MinGW => true
                            | _ => false
                        end
                        then Convention.Stdcall
                     else Convention.Cdecl)
    | _ => NONE

fun import {attributes: ImportExportAttribute.t list,
            elabedTy: Type.t,
            expandedTy: Type.t,
            name: string option,
            region: Region.t}: Type.t Prim.t =
   let
      fun error l = Control.error (region, l, Layout.empty)
      fun invalidAttributes () =
         error (seq [str "invalid attributes for _import: ",
                     List.layout ImportExportAttribute.layout attributes])
      fun invalidType () =
         Control.error 
         (region,
          str "invalid type for _import",
          Type.layoutPretty elabedTy)
   in
      case Type.parse expandedTy of
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
                  case parseIEAttributes attributes of
                     NONE => (invalidAttributes ()
                              ; Convention.Cdecl)
                   | SOME c => c
               val addrTy = Type.word (WordSize.pointer ())
               val func =
                  CFunction.T {args = let
                                         val args = Vector.map (args, #ty)
                                      in
                                         if isSome name
                                            then args
                                            else Vector.concat
                                                 [Vector.new1 addrTy, args]
                                      end,
                               bytesNeeded = NONE,
                               convention = convention,
                               ensuresBytesFree = false,
                               modifiesFrontier = true,
                               mayGC = true,
                               maySwitchThreads = false,
                               prototype = (Vector.map (args, #ctype),
                                            Option.map (result, #ctype)),
                               readsStackTop = true,
                               return = (case result of
                                            NONE => Type.unit
                                          | SOME {ty, ...} => ty),
                               target = (case name of
                                            NONE => Indirect
                                          | SOME name => Direct name),
                               writesStackTop = true}
            in
               Prim.ffi func
            end
   end

fun primApp {args, prim, result: Type.t} =
   let
      val targs = Prim.extractTargs (prim,
                                     {args = Vector.map (args, Cexp.ty),
                                      deArray = Type.deArray,
                                      deArrow = Type.deArrow,
                                      deVector = Type.deVector,
                                      deWeak = Type.deWeak,
                                      result = result})
   in
      Cexp.make (Cexp.PrimApp {args = args,
                               prim = prim,
                               targs = targs},
                 result)
   end

local
   val zeroExp = Cexp.make (Cexp.Const
                            (fn () => Const.word (WordX.zero WordSize.default)),
                            Type.defaultWord)
   val oneExp = Cexp.make (Cexp.Const
                           (fn () => Const.word (WordX.one WordSize.default)),
                           Type.defaultWord)

   fun mkAddress {expandedPtrTy: Type.t,
                  name: string}: Cexp.t =
      primApp {args = Vector.new0 (),
               prim = Prim.ffiSymbol {name = name},
               result = expandedPtrTy}

   fun mkFetch {ctypeCbTy, isBool,
                expandedCbTy,
                ptrExp: Cexp.t}: Cexp.t =
      let
         val fetchExp = 
            primApp {args = Vector.new2 (ptrExp, zeroExp),
                     prim = Prim.pointerGet ctypeCbTy,
                     result = if isBool 
                                 then Type.defaultWord 
                                 else expandedCbTy}
      in
         if not isBool then fetchExp else
         Cexp.casee {kind = "",
                     lay = fn () => Layout.empty,
                     noMatch = Cexp.Impossible,
                     nonexhaustiveExnMatch = Control.Elaborate.DiagDI.Default,
                     nonexhaustiveMatch = Control.Elaborate.DiagEIW.Ignore,
                     redundantMatch = Control.Elaborate.DiagEIW.Ignore,
                     region = Region.bogus,
                     rules = Vector.new2
                             ({exp = Cexp.truee, lay = NONE, pat = Cpat.falsee},
                              {exp = Cexp.falsee, lay = NONE, pat = Cpat.truee}),
                     test = primApp
                            {args = Vector.new2 (fetchExp, zeroExp),
                             prim = Prim.wordEqual WordSize.default,
                             result = expandedCbTy}}
      end

   fun mkStore {ctypeCbTy, isBool,
                ptrExp: Cexp.t, valueExp: Cexp.t}: Cexp.t =
      let
         val valueExp =
            if not isBool then valueExp else
            Cexp.casee {kind = "",
                        lay = fn () => Layout.empty,
                        noMatch = Cexp.Impossible,
                        nonexhaustiveExnMatch = Control.Elaborate.DiagDI.Default,
                        nonexhaustiveMatch = Control.Elaborate.DiagEIW.Ignore,
                        redundantMatch = Control.Elaborate.DiagEIW.Ignore,
                        region = Region.bogus,
                        rules = Vector.new2 
                                ({exp = oneExp, lay = NONE, pat = Cpat.truee},
                                 {exp = zeroExp, lay = NONE, pat = Cpat.falsee}),
                        test = valueExp}
      in
         primApp {args = Vector.new3 (ptrExp, zeroExp, valueExp),
                  prim = Prim.pointerSet ctypeCbTy,
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
in
   fun address {elabedTy: Type.t,
                expandedTy: Type.t,
                name: string,
                region: Region.t}: Cexp.t =
      let
         fun error () =
            Control.error
            (region, str "invalid type for _address",
             Type.layoutPretty elabedTy)
         val expandedPtrTy = expandedTy
         val () =
            case Type.toCType expandedPtrTy of
               SOME {ctype = CType.Pointer, ...} => ()
             | _ => (error (); ())
         val addrExp =
            mkAddress {expandedPtrTy = expandedPtrTy,
                       name = name}
         fun wrap (e, t) = Cexp.make (Cexp.node e, t)
      in
         wrap (addrExp, elabedTy)
      end

   fun symbolDirect {attributes: SymbolAttribute.t list,
                     elabedTy: Type.t,
                     expandedTy: Type.t,
                     name: string,
                     region: Region.t}: Cexp.t =
      let
         fun error () =
            Control.error
            (region, str "invalid type for _symbol",
             Type.layoutPretty elabedTy)
         val expandedCbTy =
            Exn.withEscape
            (fn escape =>
             let
                val error = fn () =>
                   (error ()
                    ; ignore (escape Type.defaultWord)
                    ; Error.bug "ElaborateCore.symbolDirect.escape")
             in
                case Type.deTupleOpt expandedTy of
                   NONE => error ()
                 | SOME tys => 
                      if Vector.length tys <> 2
                         then error ()
                         else let
                                 fun doit ty =
                                    case Type.deArrowOpt ty of
                                       NONE => error ()
                                     | SOME tys => tys
                                 val (getArgTy, getResTy) =
                                    doit (Vector.sub (tys, 0))
                                 val (setArgTy, setResTy) =
                                    doit (Vector.sub (tys, 1))
                                 val () =
                                    if Type.isUnit getArgTy 
                                       then ()
                                       else error ()
                                 val () =
                                    if Type.isUnit setResTy
                                       then ()
                                       else error ()
                                 val () =
                                    if Type.canUnify (getResTy, setArgTy)
                                       then ()
                                       else error ()
                              in
                                 getResTy
                              end
             end)
         val ctypeCbTy =
            case Type.toCType expandedCbTy of
               SOME {ctype, ...} => ctype
             | NONE => (error (); CType.word (WordSize.default, {signed = false}))
         val addrExp =
            mkAddress {expandedPtrTy = Type.word (WordSize.pointer ()),
                       name = name}
         val () =
            if List.exists (attributes, fn attr =>
                            attr = SymbolAttribute.Alloc)
               then Ffi.addSymbol {name = name, ty = ctypeCbTy}
               else ()
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
                       region: Region.t}: Cexp.t =
      let
         fun error () =
            Control.error
            (region, str "invalid type for _symbol",
             Type.layoutPretty elabedTy)
         val (expandedPtrTy, expandedCbTy) =
            Exn.withEscape
            (fn escape =>
             let
                val error = fn () =>
                   (error ()
                    ; ignore (escape (Type.pointer, Type.defaultWord))
                    ; Error.bug "ElaborateCore.symbolIndirect.escape")
             in
                case Type.deArrowOpt expandedTy of
                   NONE => error ()
                 | SOME (ptrTy, symTy) =>
                      (case Type.deTupleOpt symTy of
                          NONE => error ()
                        | SOME tys => 
                             if Vector.length tys <> 2
                                then error ()
                                else let
                                        fun doit ty =
                                           case Type.deArrowOpt ty of
                                              NONE => error ()
                                            | SOME tys => tys
                                        val (getArgTy, getResTy) =
                                           doit (Vector.sub (tys, 0))
                                        val (setArgTy, setResTy) =
                                           doit (Vector.sub (tys, 1))
                                        val () =
                                           if Type.isUnit getArgTy 
                                              then ()
                                              else error ()
                                        val () =
                                           if Type.isUnit setResTy
                                              then ()
                                              else error ()
                                        val () =
                                           if Type.canUnify (getResTy, setArgTy)
                                              then ()
                                              else error ()
                                     in
                                        (ptrTy, getResTy)
                                     end)
             end)
         val ctypeCbTy =
            case Type.toCType expandedCbTy of
               SOME {ctype, ...} => ctype
             | NONE => (error (); CType.word (WordSize.default, {signed = false}))
         val () =
            case Type.toCType expandedPtrTy of
               SOME {ctype = CType.Pointer, ...} => ()
             | _ => (error (); ())
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

   fun importSymbol {attributes: ImportExportAttribute.t list,
                     elabedTy: Type.t,
                     expandedTy: Type.t,
                     name: string,
                     region: Region.t}: Cexp.t =
      let
         val () =
            Control.warning 
            (region,
             str "_import of non-function is deprecated, use _symbol",
             empty)
         fun invalidAttributes () =
            Control.error 
            (region, seq [str "invalid attributes for _import: ",
                          List.layout ImportExportAttribute.layout attributes],
             Layout.empty)
         val () =
            if List.isEmpty attributes
               then ()
               else invalidAttributes ()
         fun error () =
            Control.error
            (region, str "invalid type for _import",
             Type.layoutPretty elabedTy)
         val expandedCbTy = expandedTy
         val ctypeCbTy =
            case Type.toCType expandedCbTy of
               SOME {ctype, ...} => ctype
             | NONE => (error (); CType.word (WordSize.default, {signed = false}))
         val isBool = Type.isBool expandedCbTy
         val addrExp =
            mkAddress {expandedPtrTy = Type.word (WordSize.pointer ()),
                       name = name}
         fun wrap (e, t) = Cexp.make (Cexp.node e, t)
      in
         wrap (mkFetch {ctypeCbTy = ctypeCbTy, 
                        isBool = isBool,
                        expandedCbTy = expandedCbTy,
                        ptrExp = addrExp},
               elabedTy)
      end
end

fun export {attributes: ImportExportAttribute.t list,
            elabedTy: Type.t,
            expandedTy: Type.t,
            name: string, 
            region: Region.t}: Aexp.t =
   let
      fun error l = Control.error (region, l, Layout.empty)
      fun invalidAttributes () =
         error (seq [str "invalid attributes for _export: ",
                     List.layout ImportExportAttribute.layout attributes])
      fun invalidType () =
         Control.error 
         (region,
          str "invalid type for _export",
          Type.layoutPretty elabedTy)
      val convention =
         case parseIEAttributes attributes of
            NONE => (invalidAttributes ()
                     ; Convention.Cdecl)
          | SOME c => c
      val (exportId, args, res) =
         case Type.parse expandedTy of
            NONE =>
               (invalidType ()
                ; (0, Vector.new0 (), NONE))
          | SOME (args, result) =>
               let
                  val id =
                     Ffi.addExport {args = Vector.map (args, #ctype),
                                    convention = convention,
                                    name = name,
                                    res = Option.map (result, #ctype)}
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
            (Pat.tuple (Vector.new0 ()),
             let
                val map = CType.memo (fn _ => Counter.new 0)
                val varCounter = Counter.new 0
                val (args, decs) =
                   Vector.unzip
                   (Vector.map
                    (args, fn {ctype, name, ...} =>
                     let
                        val x =
                           Var.fromSymbol
                           (Symbol.fromString
                            (concat ["x",
                                     Int.toString (Counter.next varCounter)]),
                            region)
                        val dec =
                           Dec.vall (Vector.new0 (),
                                     x,
                                     Exp.app (id (concat ["get", name]),
                                              int (Counter.next (map ctype))))
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
                  (Vector.new4
                   ((newVar (), Exp.app (id "atomicEnd", Exp.unit)),
                    (resVar, Exp.app (Exp.var f,
                                      Exp.tuple (Vector.map (args, Exp.var)))),
                    (newVar (), Exp.app (id "atomicBegin", Exp.unit)),
                    (newVar (),
                     (case res of
                         NONE => Exp.constraint (Exp.var resVar, Type.unit)
                       | SOME {name, ...} => 
                            Exp.app (id (concat ["set", name]),
                                     Exp.var resVar)))),
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
         val x = Symbol.fromString "x"
      in
         fun selector (f: Field.t, r: Region.t): t =
            let
               val x = Avar.fromSymbol (x, r)
            in
               fnn (Vector.new1
                    (Apat.makeRegion
                     (Apat.Record {flexible = true,
                                   items = (Vector.new1
                                            (f, Apat.Item.Field (Apat.var x)))},
                      r),
                     var x))
            end
      end
   end

structure Con =
   struct
      open Con

      val fromAst = fromString o Ast.Con.toString
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

structure ElabControl = Control.Elaborate
   
fun check (c: (bool,bool) ElabControl.t, keyword: string, region) =
   if ElabControl.current c
      then ()
   else
      let
         open Layout
      in
         Control.error 
         (region,
          str (concat (if ElabControl.expert c
                          then [keyword, " disallowed"]
                          else [keyword, " disallowed, compile with -default-ann '", 
                                ElabControl.name c, " true'"])),
          empty)
      end

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
      fun elabType (t: Atype.t): Type.t =
         elaborateType (t, Lookup.fromEnv E)
      fun elabTypBind (typBind: TypBind.t) =
         let
            val TypBind.T types = TypBind.node typBind
            val strs =
               Vector.map
               (types, fn {def, tyvars, ...} =>
                TypeStr.def (Scheme.make {canGeneralize = true,
                                          ty = elabType def,
                                          tyvars = tyvars},
                             Kind.Arity (Vector.length tyvars)))
         in
            Vector.foreach2
            (types, strs, fn ({tycon, ...}, str) =>
             Env.extendTycon (E, tycon, str, {forceUsed = false,
                                              isRebind = false}))
         end
      fun elabDatBind (datBind: DatBind.t, nest: string list)
         : Decs.t * {tycon: Ast.Tycon.t,
                     typeStr: TypeStr.t} vector =
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
                   val kind = Kind.Arity (Vector.length tyvars)
                   val tycon =
                      Env.newTycon
                      (concat (List.separate
                               (rev (Ast.Tycon.toString name :: nest),
                                ".")),
                       kind,
                       AdmitsEquality.Sometimes,
                       Ast.Tycon.region name)
                   val _ = Env.extendTycon (E, name, TypeStr.tycon (tycon, kind),
                                            {forceUsed = true,
                                             isRebind = false})
                   val cons =
                      Vector.map
                      (cons, fn (name, arg) =>
                       {con = Con.fromAst name,
                        name = name,
                        arg = arg})
                   val makeCons =
                      Env.newCons (E, Vector.map (cons, fn {con, name, ...} =>
                                                  {con = con, name = name}))
                in
                   {cons = cons,
                    makeCons = makeCons,
                    name = name,
                    tycon = tycon,
                    tyvars = tyvars}
                end)
            val change = ref false
            fun elabAll () =
               (elabTypBind withtypes
                ; (Vector.map
                   (datatypes,
                    fn {cons, makeCons, name, tycon, tyvars} =>
                    let
                       val resultType: Type.t =
                          Type.con (tycon, Vector.map (tyvars, Type.var))
                       val (schemes, datatypeCons) =
                          Vector.unzip
                          (Vector.map
                           (cons, fn {arg, con, ...} =>
                            let
                               val (arg, ty) =
                                  case arg of
                                     NONE => (NONE, resultType)
                                   | SOME t =>
                                        let
                                           val t = elabType t
                                        in
                                           (SOME t, Type.arrow (t, resultType))
                                        end
                               val scheme =
                                  Scheme.make {canGeneralize = true,
                                               ty = ty,
                                               tyvars = tyvars}
                            in
                               (scheme, {arg = arg, con = con})
                            end))
                       val _ =
                          let
                             val r = TypeEnv.tyconAdmitsEquality tycon
                             datatype z = datatype AdmitsEquality.t
                          in
                             case !r of
                                Always => Error.bug "ElaborateCore.elaborateDec.elabDatBind: Always"
                              | Never => ()
                              | Sometimes =>
                                   if Vector.forall
                                      (datatypeCons, fn {arg, ...} =>
                                       case arg of
                                          NONE => true
                                        | SOME ty =>
                                             Scheme.admitsEquality
                                             (Scheme.make {canGeneralize = true,
                                                           ty = ty,
                                                           tyvars = tyvars}))
                                      then ()
                                   else (r := Never; change := true)
                          end
                    val typeStr =
                       TypeStr.data (tycon,
                                     Kind.Arity (Vector.length tyvars),
                                     makeCons schemes)
                    val _ =
                       Env.extendTycon (E, name, typeStr,
                                        {forceUsed = false, isRebind = true})
                 in
                    ({cons = datatypeCons,
                      tycon = tycon,
                      tyvars = tyvars},
                     {tycon = name,
                      typeStr = typeStr})
                 end)))
            (* We don't want to re-elaborate the datatypes if there has been a
             * type error, because that will cause duplicate error messages.
             *)
            val numErrors = !Control.numErrors
            (* Maximize equality. *)
            fun loop () =
               let
                  val res = elabAll ()
               in
                  if !change andalso numErrors = !Control.numErrors
                     then (change := false; loop ())
                  else res
               end
            val (dbs, strs) = Vector.unzip (loop ())
         in
            (Decs.single (Cdec.Datatype dbs), strs)
         end
      fun elabDec arg : Decs.t =
         Trace.traceInfo
         (elabDecInfo,
          Layout.tuple3 (Ast.Dec.layout, Nest.layout, Bool.layout),
          Decs.layout, Trace.assertTrue)
         (fn (d, nest, isTop) =>
          let
             val region = Adec.region d
             fun lay () = seq [str "in: ", approximate (Adec.layout d)]
             val preError = Promise.lazy (fn () => Env.setTyconNames E)
             fun reportUnable (unable: Tyvar.t vector) =
               if 0 = Vector.length unable
                  then ()
               else
                  let
                     open Layout
                  in
                     Control.error
                     (region,
                      seq [str (concat
                                ["can't bind type variable",
                                 if Vector.length unable > 1 then "s" else "",
                                 ": "]),
                           seq (List.separate
                                (Vector.toListMap (unable, Tyvar.layout),
                                 str ", "))],
                      lay ())
                  end
             fun useBeforeDef (c: Tycon.t) =
                let
                   val _ = preError ()
                   open Layout
                in
                   Control.error
                   (region,
                    seq [str "type escapes the scope of its definition at ",
                         str (case ! (TypeEnv.tyconRegion c) of
                                 NONE => "<bogus>"
                               | SOME r =>
                                    case Region.left r of
                                       NONE => "<bogus>"
                                     | SOME p => SourcePos.toString p)],
                    align [seq [str "type: ", Tycon.layout c],
                           lay ()])
                end
             val () = TypeEnv.tick {useBeforeDef = useBeforeDef}
             val unify = fn (t, t', f) => unify (t, t', preError, f)
             fun checkSchemes (v: (Var.t * Scheme.t) vector): unit =
                if isTop
                   then
                      List.push
                      (freeTyvarChecks,
                       fn () =>
                       Vector.foreach2
                       (v, Scheme.haveFrees (Vector.map (v, #2)),
                        fn ((x, s), b) =>
                        if b
                           then
                              let
                                 val _ = preError ()
                                 open Layout
                              in
                                 Control.warning
                                 (region,
                                  seq [str "unable to locally determine type of variable: ",
                                       Var.layout x],
                                  align [seq [str "type: ", Scheme.layoutPretty s],
                                         lay ()])
                              end
                        else ()))
                else ()
             val elabDec = fn (d, isTop) => elabDec (d, nest, isTop)
          in
             case Adec.node d of
                Adec.Abstype {datBind, body} => (* rule 19 and p.57 *)
                   let
                      val ((decs, strs), decs') =
                         Env.localCore
                         (E,
                          fn () => elabDatBind (datBind, nest),
                          fn z => (z, elabDec (body, isTop)))
                      val _ =
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
                                 in
                                    Env.extendTycon (E, lhs, s,
                                                     {forceUsed = forceUsed,
                                                      isRebind = false})
                                 end)
                          in
                             Decs.empty
                          end)
              | Adec.Exception ebs =>
                   let
                      val decs =
                         Vector.fold
                         (ebs, Decs.empty, fn ((exn, rhs), decs) =>
                          let
                             val (decs, exn', scheme) =
                                case EbRhs.node rhs of
                                   EbRhs.Def c =>
                                      let
                                         val (c, s) = Env.lookupLongcon (E, c)
                                      in
                                         (decs, c, s)
                                      end
                                 | EbRhs.Gen arg =>
                                      let
                                         val exn' = Con.fromAst exn
                                         val (arg, ty) =
                                            case arg of
                                               NONE => (NONE, Type.exn)
                                             | SOME t =>
                                                  let
                                                     val t = elabType t
                                                  in
                                                     (SOME t,
                                                      Type.arrow (t, Type.exn))
                                                  end
                                         val scheme = Scheme.fromType ty
                                      in
                                         (Decs.add (decs,
                                                    Cdec.Exception {arg = arg,
                                                                    con = exn'}),
                                          exn',
                                          SOME scheme)
                                      end
                             val _ = Env.extendExn (E, exn, exn', scheme)
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
              | Adec.Fun (tyvars, fbs) =>
                   let
                      val fbs =
                         Vector.map
                         (fbs, fn clauses =>
                          Vector.map
                          (clauses, fn {body, pats, resultType} =>
                           let
                              fun lay () =
                                 approximate
                                 (let
                                     open Layout
                                  in
                                     seq [seq (List.separate
                                               (Vector.toListMap
                                                (pats, Apat.layoutDelimit),
                                                str " ")),
                                          str " = ",
                                          Aexp.layout body]
                                  end)
                              val {args, func} =
                                 Parse.parseClause (pats, E, region, lay)
                           in
                              {args = args,
                               body = body,
                               func = func,
                               lay = lay,
                               resultType = resultType}
                           end))
                      val close =
                         TypeEnv.close (tyvars, {useBeforeDef = useBeforeDef})
                      val {markFunc, setBound, unmarkFunc} = recursiveFun ()
                      val fbs =
                         Vector.map
                         (fbs, fn clauses =>
                          if Vector.isEmpty clauses
                             then Error.bug "ElaborateCore.elabDec: Fun:no clauses"
                          else
                             let
                                fun lay () =
                                   let
                                      open Layout
                                   in
                                      seq [str "in: ",
                                           approximate
                                           (seq
                                            (separate
                                             (Vector.toListMap
                                              (clauses, fn {lay, ...} => lay ()),
                                              " | ")))]
                                   end
                                val {args, func, lay = lay0, ...} =
                                   Vector.sub (clauses, 0)
                                val numArgs = Vector.length args
                                val _ =
                                   Vector.foreach
                                   (clauses, fn {args, lay = layN, ...} =>
                                    if numArgs = Vector.length args
                                       then  ()
                                    else
                                       let
                                          fun one lay =
                                             seq [str "clause: ",
                                                  approximate (lay ())]
                                       in
                                          Control.error
                                          (region,
                                           seq [str "function defined with different numbers of arguments"],
                                           align [one lay0, one layN])
                                       end)
                                val diff =
                                   Vector.fold
                                   (clauses, [], fn ({func = func', ...}, ac) =>
                                    if Avar.equals (func, func')
                                       then ac
                                    else func' :: ac)
                                val _ =
                                   case diff of
                                      [] => ()
                                    | _ =>
                                         let
                                            val diff =
                                               List.removeDuplicates
                                               (func :: diff, Avar.equals)
                                         in
                                            Control.error
                                            (region,
                                             seq [str "function defined with multiple names: ",
                                                  seq (Layout.separateRight
                                                       (List.map (diff,
                                                                  Avar.layout),
                                                        ", "))],
                                             lay ())
                                         end
                                val var = Var.fromAst func
                                val ty = Type.new ()
                                val _ = Env.extendVar (E, func, var,
                                                       Scheme.fromType ty,
                                                       {isRebind = false})
                                val _ = markFunc var
                                val _ =
                                   Acon.ensureRedefine
                                   (Avid.toCon (Avid.fromVar func))
                             in
                                {clauses = clauses,
                                 func = func,
                                 lay = lay,
                                 ty = ty,
                                 var = var}
                             end)
                      val _ =
                         Vector.fold
                         (fbs, [], fn ({func = f, ...}, ac) =>
                          if List.exists (ac, fn f' => Avar.equals (f, f'))
                             then
                                (Control.error
                                 (Avar.region f,
                                  seq [str "function ",
                                       Avar.layout f,
                                       str " defined multiple times: "],
                                  lay ())
                                 ; ac)
                          else f :: ac)
                      val decs =
                         Vector.map
                         (fbs, fn {clauses,
                                   func: Avar.t,
                                   lay,
                                   ty: Type.t,
                                   var: Var.t} =>
                          let
                             val nest = Avar.toString func :: nest
                             fun sourceInfo () =
                                SourceInfo.function {name = nest,
                                                     region = Avar.region func}
                             val rs =
                                Vector.map
                                (clauses, fn {args: Apat.t vector,
                                              body: Aexp.t,
                                              lay: unit -> Layout.t,
                                              resultType: Atype.t option, ...} =>
                                 Env.scope
                                 (E, fn () =>
                                  let
                                     val elaboratePat = elaboratePat ()
                                     val pats =
                                        Vector.map
                                        (args, fn p =>
                                         {pat = #1 (elaboratePat
                                                    (p, E,
                                                     {bind = true,
                                                      isRvb = false},
                                                     preError)),
                                          region = Apat.region p})
                                     val bodyRegion = Aexp.region body
                                     val body = elabExp (body, nest, NONE)
                                     val body =
                                        Cexp.enterLeave
                                        (body, 
                                         profileBody 
                                         andalso !Control.profileBranch,
                                         fn () =>
                                         SourceInfo.function
                                         {name = "<branch>" :: nest,
                                          region = bodyRegion})
                                     val _ =
                                        Option.app
                                        (resultType, fn t =>
                                         unify
                                         (elabType t, Cexp.ty body,
                                          fn (l1, l2) =>
                                          (Atype.region t,
                                           str "function result type disagrees with expression",
                                           align
                                           [seq [str "result type: ", l1],
                                            seq [str "expression:  ", l2],
                                            lay ()])))
                                  in
                                     {body = body,
                                      bodyRegion = bodyRegion,
                                      lay = lay,
                                      pats = pats}
                                  end))
                             val numArgs =
                                Vector.length (#pats (Vector.sub (rs, 0)))
                             val argTypes =
                                Vector.tabulate
                                (numArgs, fn i =>
                                 let
                                    val t =
                                       Cpat.ty
                                       (#pat (Vector.sub
                                              (#pats (Vector.sub (rs, 0)),
                                               i)))
                                    val _ =
                                       Vector.foreach
                                       (rs, fn {pats, ...} =>
                                        let
                                           val {pat, region} =
                                              Vector.sub (pats, i)
                                        in
                                           unify
                                           (t, Cpat.ty pat, fn (l1, l2) =>
                                            (region,
                                             str "function with argument of different types",
                                             align [seq [str "argument: ", l2],
                                                    seq [str "previous: ", l1],
                                                    lay ()]))
                                        end)
                                 in
                                    t
                                 end)
                             val t = Cexp.ty (#body (Vector.sub (rs, 0)))
                             val _ =
                                Vector.foreach
                                (rs, fn {body, bodyRegion, ...} =>
                                 unify
                                 (t, Cexp.ty body, fn (l1, l2) =>
                                  (bodyRegion,
                                   str "function with result of different types",
                                   align [seq [str "result:   ", l2],
                                          seq [str "previous: ", l1],
                                          lay ()])))
                             val xs =
                                Vector.tabulate (numArgs, fn _ =>
                                                 Var.newNoname ())
                             fun make (i: int): Cexp.t =
                                if i = Vector.length xs
                                   then
                                      let
                                         val e =
                                            Cexp.casee
                                            {kind = "function",
                                             lay = lay,
                                             noMatch = Cexp.RaiseMatch,
                                             nonexhaustiveExnMatch = nonexhaustiveExnMatch (),
                                             nonexhaustiveMatch = nonexhaustiveMatch (),
                                             redundantMatch = redundantMatch (),
                                             region = region,
                                             rules =
                                             Vector.map
                                             (rs, fn {body, lay, pats, ...} =>
                                              let
                                                 val pats =
                                                    Vector.map (pats, #pat)
                                              in
                                                 {exp = body,
                                                  lay = SOME lay,
                                                  pat =
                                                  (Cpat.make
                                                   (Cpat.Tuple pats,
                                                    Type.tuple
                                                    (Vector.map (pats, Cpat.ty))))}
                                              end),
                                             test = 
                                             Cexp.tuple
                                             (Vector.map2
                                              (xs, argTypes, Cexp.var))}
                                      in
                                         Cexp.enterLeave
                                         (e, profileBody, sourceInfo)
                                      end
                                else
                                   let
                                      val body = make (i + 1)
                                      val argType = Vector.sub (argTypes, i)
                                   in
                                      Cexp.make
                                      (Cexp.Lambda
                                       (Lambda.make
                                        {arg = Vector.sub (xs, i),
                                         argType = argType,
                                         body = body,
                                         mayInline = true}),
                                       Type.arrow (argType, Cexp.ty body))
                                   end
                             val lambda = make 0
                             val _ =
                                unify
                                (Cexp.ty lambda, ty, fn (l1, l2) =>
                                 (Avar.region func,
                                  str "Recursive use of function disagrees with its type",
                                  align [seq [str "expects: ", l1],
                                         seq [str "but got: ", l2],
                                         lay ()]))
                             val lambda =
                                case Cexp.node lambda of
                                   Cexp.Lambda l => l
                                 | _ => Lambda.bogus
                          in
                             {lambda = lambda,
                              ty = ty,
                              var = var}
                          end)
                      val {bound, schemes, unable} =
                         close (Vector.map (decs, fn {ty, ...} =>
                                            {isExpansive = false,
                                             ty = ty}))
                      val () = reportUnable unable
                      val _ = checkSchemes (Vector.zip
                                            (Vector.map (decs, #var),
                                             schemes))
                      val _ = setBound bound
                      val _ =
                         Vector.foreach3
                         (fbs, decs, schemes,
                          fn ({func, ...}, {var, ...}, scheme) =>
                          (Env.extendVar (E, func, var, scheme,
                                          {isRebind = true})
                           ; unmarkFunc var))
                      val decs =
                         Vector.map (decs, fn {lambda, var, ...} =>
                                     {lambda = lambda, var = var})
                   in
                      Decs.single (Cdec.Fun {decs = decs,
                                             tyvars = bound})
                   end
              | Adec.Local (d, d') =>
                   Env.localCore
                   (E,
                    fn () => elabDec (d, false),
                    fn decs => Decs.append (decs, elabDec (d', isTop)))
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
                   (check (ElabControl.allowOverload, "_overload", region)
                    ; let
                         (* Lookup the overloads before extending the var in case
                          * x appears in the xs.
                          *)
                         val ovlds =
                            Vector.map (xs, fn x => Env.lookupLongvar (E, x))
                         val _ =
                            Env.extendOverload
                            (E, p, x, 
                             Vector.map (ovlds, fn (x, s) =>
                                         (x, Option.map (s, Scheme.ty))),
                             Scheme.make {canGeneralize = false,
                                          tyvars = tyvars,
                                          ty = elabType ty})
                      in
                         Decs.empty
                      end)
              | Adec.SeqDec ds =>
                   Vector.fold (ds, Decs.empty, fn (d, decs) =>
                                Decs.append (decs, elabDec (d, isTop)))
              | Adec.Type typBind =>
                   (elabTypBind typBind
                    ; Decs.empty)
              | Adec.Val {tyvars, rvbs, vbs} =>
                   let
                      val close =
                         TypeEnv.close (tyvars, {useBeforeDef = useBeforeDef})
                      (* Must do all the es and rvbs before the ps because of
                       * scoping rules.
                       *)
                      val vbs =
                         Vector.map
                         (vbs, fn {exp, pat, ...} =>
                          let
                             fun lay () =
                                let
                                   open Layout
                                in
                                   seq [str "in: ",
                                        approximate
                                        (seq [Apat.layout pat,
                                              str " = ", Aexp.layout exp])]
                                end
                          in
                             {exp = elabExp (exp, nest, Apat.getName pat),
                              expRegion = Aexp.region exp,
                              lay = lay,
                              pat = pat,
                              patRegion = Apat.region pat}
                          end)
                      val {markFunc, setBound, unmarkFunc} = recursiveFun ()
                      val elaboratePat = elaboratePat ()
                      val rvbs =
                         Vector.map
                         (rvbs, fn {pat, match} =>
                          let
                             val region = Apat.region pat
                             val (pat, bound) =
                                elaboratePat (pat, E, {bind = false,
                                                       isRvb = true},
                                              preError)
                             val (nest, var, ty) =
                                if 0 = Vector.length bound
                                   then ("rec" :: nest,
                                         Var.newNoname (),
                                         Type.new ())
                                else
                                   let
                                      val (x, x', t) = Vector.sub (bound, 0)
                                   in
                                      (Avar.toString x :: nest, x', t)
                                   end
                             val _ = markFunc var
                             val scheme = Scheme.fromType ty
                             val bound =
                                Vector.map
                                (bound, fn (x, _, _) =>
                                 (Acon.ensureRedefine (Avid.toCon
                                                       (Avid.fromVar x))
                                  ; Env.extendVar (E, x, var, scheme,
                                                   {isRebind = false})
                                  ; (x, var, ty)))
                          in
                             {bound = bound,
                              match = match,
                              nest = nest,
                              pat = pat,
                              region = region,
                              var = var}
                          end)
                      val rvbs =
                         Vector.map
                         (rvbs, fn {bound, match, nest, pat, var, ...} =>
                          let
                             val {argType, region, resultType, rules} =
                                elabMatch (match, preError, nest)
                             val _ =
                                unify
                                (Cpat.ty pat,
                                 Type.arrow (argType, resultType),
                                 fn (l1, l2) =>
                                 (region,
                                  str "function type disagrees with recursive uses",
                                  align [seq [str "function type:  ", l1],
                                         seq [str "recursive uses: ", l2],
                                         lay ()]))
                             val arg = Var.newNoname ()
                             val body =
                                Cexp.enterLeave
                                (Cexp.casee {kind = "function",
                                             lay = lay,
                                             noMatch = Cexp.RaiseMatch,
                                             nonexhaustiveExnMatch = nonexhaustiveExnMatch (),
                                             nonexhaustiveMatch = nonexhaustiveMatch (),
                                             redundantMatch = redundantMatch (),
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
                             {bound = bound,
                              lambda = lambda,
                              var = var}
                          end)
                      val boundVars =
                         Vector.map
                         (Vector.concatV (Vector.map (rvbs, #bound)),
                          fn x => (x, {isExpansive = false,
                                       isRebind = true}))
                      val rvbs =
                         Vector.map
                         (rvbs, fn {bound, lambda, var} =>
                          (Vector.foreach (bound, unmarkFunc o #2)
                           ; {lambda = lambda,
                              var = var}))
                      val vbs =
                         Vector.map
                         (vbs,
                          fn {exp = e, expRegion, lay, pat, patRegion, ...} =>
                          let
                             val (p, bound) =
                                elaboratePat (pat, E, {bind = false,
                                                       isRvb = false}, preError)
                             val _ =
                                unify
                                (Cpat.ty p, Cexp.ty e, fn (p, e) =>
                                 (Apat.region pat,
                                  str "pattern and expression disagree",
                                  align [seq [str "pattern:    ", p],
                                         seq [str "expression: ", e],
                                         lay ()]))
                          in
                             {bound = bound,
                              exp = e,
                              expRegion = expRegion,
                              lay = lay,
                              pat = p,
                              patRegion = patRegion}
                          end)
                      val boundVars =
                         Vector.concat
                         [boundVars,
                          Vector.concatV
                          (Vector.map
                           (vbs, fn {bound, exp, ...} =>
                            (Vector.map
                             (bound, fn z =>
                              (z, {isExpansive = Cexp.isExpansive exp,
                                   isRebind = false})))))]
                      val {bound, schemes, unable} =
                         close
                         (Vector.map
                          (boundVars, fn ((_, _, ty), {isExpansive, ...}) =>
                           {isExpansive = isExpansive, ty = ty}))
                      val () = reportUnable unable
                      val () = checkSchemes (Vector.zip
                                            (Vector.map (boundVars, #2 o #1),
                                             schemes))
                      val () = setBound bound
                      val () =
                         Vector.foreach2
                         (boundVars, schemes,
                          fn (((x, x', _), {isRebind, ...}), scheme) =>
                          Env.extendVar (E, x, x', scheme,
                                         {isRebind = isRebind}))
                      val vbs =
                         Vector.map (vbs, fn {exp, lay, pat, patRegion, ...} =>
                                     {exp = exp,
                                      lay = lay,
                                      pat = pat,
                                      patRegion = patRegion})
                      (* According to page 28 of the Definition, we should
                       * issue warnings for nonexhaustive valdecs only when it's
                       * not a top level dec.   It seems harmless enough to go
                       * ahead and always issue them.
                       *)
                   in
                      Decs.single
                      (Cdec.Val {nonexhaustiveExnMatch = nonexhaustiveExnMatch (),
                                 nonexhaustiveMatch = nonexhaustiveMatch (),
                                 rvbs = rvbs,
                                 tyvars = bound,
                                 vbs = vbs})
                   end
          end) arg
      and elabExp (arg: Aexp.t * Nest.t * string option): Cexp.t =
         Trace.traceInfo
         (elabExpInfo,
          Layout.tuple3 (Aexp.layout, Nest.layout, Layout.ignore),
          Cexp.layout,
          Trace.assertTrue)
         (fn (e: Aexp.t, nest, maybeName) =>
          let
             val preError = Promise.lazy (fn () => Env.setTyconNames E)
             val unify = fn (t, t', f) => unify (t, t', preError, f)
             fun lay () = seq [str "in: ", approximate (Aexp.layout e)]
             val unify =
                fn (a, b, f) => unify (a, b, fn z =>
                                       let
                                          val (r, l, l') = f z
                                       in
                                          (r, l, align [l', lay ()])
                                       end)
             val region = Aexp.region e
             fun elab e = elabExp (e, nest, NONE)
          in
             case Aexp.node e of
                Aexp.Andalso (e, e') =>
                   let
                      val ce = elab e
                      val ce' = elab e'
                      fun doit (ce, br) =
                         unify
                         (Cexp.ty ce, Type.bool,
                          fn (l, _) =>
                          (Aexp.region e,
                           str (concat
                                [br, " branch of andalso not of type bool"]),
                           seq [str " branch: ", l]))
                      val _ = doit (ce, "left")
                      val _ = doit (ce', "right")
                   in
                      Cexp.andAlso (ce, ce')
                   end
              | Aexp.App (e1, e2) =>
                   let
                      val e1 = elab e1
                      val e2 = elab e2
                      val argType = Type.new ()
                      val resultType = Type.new ()
                      val _ =
                         unify (Cexp.ty e1, Type.arrow (argType, resultType),
                                fn (l, _) =>
                                (region,
                                 str "function not of arrow type",
                                 seq [str "function: ", l]))
                      val _ =
                         unify
                         (argType, Cexp.ty e2, fn (l1, l2) =>
                          (region,
                           str "function applied to incorrect argument",
                           align [seq [str "expects: ", l1],
                                  seq [str "but got: ", l2]]))
                   in
                      Cexp.make (Cexp.App (e1, e2), resultType)
                   end
              | Aexp.Case (e, m) =>
                   let
                      val e = elab e
                      val {argType, rules, ...} = elabMatch (m, preError, nest)
                      val _ =
                         unify
                         (Cexp.ty e, argType, fn (l1, l2) =>
                          (region,
                           str "case object and rules disagree",
                           align [seq [str "object type:  ", l1],
                                  seq [str "rules expect: ", l2]]))
                   in
                      Cexp.casee {kind = "case",
                                  lay = lay,
                                  noMatch = Cexp.RaiseMatch,
                                  nonexhaustiveExnMatch = nonexhaustiveExnMatch (),
                                  nonexhaustiveMatch = nonexhaustiveMatch (),
                                  redundantMatch = redundantMatch (),
                                  region = region,
                                  rules = rules,
                                  test = e}
                   end
              | Aexp.Const c =>
                   elabConst
                   (c,
                    fn (resolve, ty) => Cexp.make (Cexp.Const resolve, ty),
                    {false = Cexp.falsee,
                     true = Cexp.truee})
              | Aexp.Constraint (e, t') =>
                   let
                      val e = elab e
                      val _ =
                         unify
                         (Cexp.ty e, elabType t', fn (l1, l2) =>
                          (region,
                           str "expression and constraint disagree",
                           align [seq [str "expects: ", l2],
                                  seq [str "but got: ", l1]]))
                   in
                      e
                   end
              | Aexp.FlatApp items => elab (Parse.parseExp (items, E, lay))
              | Aexp.Fn m =>
                   let
                      val nest =
                         case maybeName of
                            NONE => "fn" :: nest
                          | SOME s => s :: nest
                      val {arg, argType, body} =
                         elabMatchFn (m, preError, nest, "function", lay,
                                      Cexp.RaiseMatch)
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
                         elabMatchFn (match, preError, nest, "handler", lay,
                                      Cexp.RaiseAgain)
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
                           seq [str "handler handles wrong type: ", l1],
                           empty))
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
                           seq [str "test type: ", l1]))
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
                               (wrap (b, b', "<true>"), wrap (c, c', "<false>"))
                            end
                   in
                      Cexp.iff (a', b', c')
                   end
              | Aexp.Let (d, e) =>
                   Env.scope
                   (E, fn () =>
                    let
                       val time = Time.now ()
                       val d = Decs.toVector (elabDec (d, nest, false))
                       val e = elab e
                       val ty = Cexp.ty e
                       val () = Type.minTime (ty, time)
                    in
                       Cexp.make (Cexp.Let (d, e), ty)
                    end)
              | Aexp.List es =>
                   let
                      val es' = Vector.map (es, elab)
                   in
                      Cexp.make (Cexp.List es',
                                 unifyList
                                 (Vector.map2 (es, es', fn (e, e') =>
                                               (Cexp.ty e', Aexp.region e)),
                                  preError, lay))
                   end
              | Aexp.Orelse (e, e') =>
                   let
                      val ce = elab e
                      val ce' = elab e'
                      fun doit (ce, br) =
                         unify
                         (Cexp.ty ce, Type.bool,
                          fn (l, _) =>
                          (Aexp.region e,
                           str (concat
                                [br, " branch of orelse not of type bool"]),
                           seq [str " branch: ", l]))
                      val _ = doit (ce, "left")
                      val _ = doit (ce', "right")
                   in
                      Cexp.orElse (ce, ce')
                   end
              | Aexp.Prim kind => 
                   let
                      fun elabAndExpandTy ty =
                         let
                            val elabedTy = elabType ty
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
                                              {kind = "",
                                               lay = fn _ => Layout.empty,
                                               noMatch = Cexp.Impossible,
                                               nonexhaustiveExnMatch = Control.Elaborate.DiagDI.Default,
                                               nonexhaustiveMatch = Control.Elaborate.DiagEIW.Ignore,
                                               redundantMatch = Control.Elaborate.DiagEIW.Ignore,
                                               region = Region.bogus,
                                               rules = Vector.new1
                                                       {exp = app (Vector.map
                                                                   (vars, Cexp.var)),
                                                        lay = NONE,
                                                        pat = Cpat.tuple 
                                                              (Vector.map 
                                                               (vars, Cpat.var))},
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
                                  open Layout
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
                                           then ConstType.Word
                                        else if Tycon.isRealX c
                                           then ConstType.Real
                                        else if Tycon.isWordX c
                                           then ConstType.Word
                                        else if Tycon.equals (c, Tycon.vector)
                                           andalso 1 = Vector.length ts
                                           andalso
                                           (case (Type.deConOpt
                                                  (Vector.sub (ts, 0))) of
                                               NONE => false
                                             | SOME (c, _) => 
                                                  Tycon.isCharX c)
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
                         Address {name, ty} =>
                            let
                               val () =
                                  check (ElabControl.allowFFI, "_address")
                               val (elabedTy, expandedTy) =
                                  elabAndExpandTy ty
                            in
                               address {elabedTy = elabedTy,
                                        expandedTy = expandedTy,
                                        name = name,
                                        region = region}
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
                                   Type.layoutPretty elabedTy)
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
                                                NONE => 
                                                   let
                                                      val elabedExportTy =
                                                         Type.arrow (elabedTy, Type.unit)
                                                      val () =
                                                         Control.warning
                                                         (region,
                                                          seq [str "_export with partial annotation is deprecated, ",
                                                               str "use ", 
                                                               Type.layoutPretty (elabedExportTy)],
                                                          empty)
                                                   in
                                                      (expandedTy, elabedExportTy)
                                                   end
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
                                                    region = region})))
                               val _ =
                                  unify
                                  (Cexp.ty exp,
                                   Type.arrow (expandedCfTy, Type.unit),
                                   fn (l1, l2) =>
                                   let
                                      open Layout
                                   in
                                      (region,
                                       str "_export unify bug",
                                       align [seq [str "inferred: ", l1],
                                              seq [str "expanded: ", l2]])
                                   end)
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
                                   Type.layoutPretty elabedTy)
                               val (expandedFPtrTy, expandedCfTy) =
                                  Exn.withEscape
                                  (fn escape =>
                                   let
                                      val error = fn () =>
                                         (error ()
                                          ; ignore (escape (Type.pointer, Type.arrow (Type.unit, Type.unit)))
                                          ; Error.bug "ElaborateCore.elabExp.IImport.escape")
                                   in
                                      case Type.deArrowOpt expandedTy of
                                         NONE => error ()
                                       | SOME (fptrTy, cfTy) => (fptrTy, cfTy)
                                   end)
                               val () =
                                  case Type.toCType expandedFPtrTy of
                                     SOME {ctype = CType.Pointer, ...} => ()
                                   | _ => (error (); ())
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
                                                                expandedTy = expandedCfTy}},
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
                               case Type.deArrowOpt expandedTy of
                                  NONE => 
                                     importSymbol {attributes = attributes,
                                                   elabedTy = elabedTy,
                                                   expandedTy = expandedTy,
                                                   name = name,
                                                   region = region}
                                | SOME _ =>
                                     eta ({elabedTy = elabedTy,
                                           expandedTy = expandedTy,
                                           prim = import {attributes = attributes,
                                                          name = SOME name,
                                                          region = region,
                                                          elabedTy = elabedTy,
                                                          expandedTy = expandedTy}})
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
                                               region = region}
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
                                             region = region}
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
                           str "raise of non exception",
                           seq [str "exp type: ", l1]))
                      val resultType = Type.new ()
                   in
                      Cexp.enterLeave
                      (Cexp.make (Cexp.Raise exn, resultType),
                       profileBody andalso !Control.profileRaise,
                       fn () => SourceInfo.function {name = "raise" :: nest,
                                                     region = region})
                   end
              | Aexp.Record r =>
                   let
                      val r = Record.map (r, elab)
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
                            fun doit f =
                               List.push
                               (sequenceTypeChecks, fn () =>
                                Vector.foreachi
                                (es', fn (i, e') =>
                                 if i = last 
                                    then ()
                                    else let
                                            val ty = Cexp.ty e'
                                         in
                                            if Type.isUnit ty
                                               then ()
                                               else let
                                                       val e = Vector.sub (es, i)
                                                       open Layout
                                                    in 
                                                       f (Aexp.region e,
                                                          str "sequence expression not of type unit",
                                                          align [seq [str "type: ", Type.layoutPrettyBracket ty],
                                                                 seq [str "in: ",
                                                                      approximate (Aexp.layout e)]])
                                                    end
                                         end))
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
                      val (vid, scheme) = Env.lookupLongvid (E, id)
                      fun dontCare () =
                         Cexp.var (Var.newNoname (), Type.new ())
                   in
                      case scheme of
                         NONE => dontCare ()
                       | SOME scheme => 
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
                                               case (Vector.peek
                                                     (yts, fn (_, t) =>
                                                      case t of
                                                         NONE => false
                                                       | SOME t => 
                                                            Type.canUnify
                                                            (instance, t))) of
                                                  NONE =>
                                                     let
                                                        val _ =
                                                           Control.error
                                                           (region,
                                                            seq [str "impossible use of overloaded var: ",
                                                                 str (Longvid.toString id)],
                                                            Type.layoutPretty instance)
                                                     in
                                                        Var.newNoname ()
                                                     end
                                                | SOME (y, t) =>  
                                                     (unify (instance,
                                                             valOf t, fn _ =>
                                                             Error.bug "ElaborateCore.elabExp: Var:overload unify")
                                                      ; y))
                                           val _ = 
                                              List.push (overloads, (p, ignore o resolve))
                                        in
                                           Cexp.Var (resolve, fn () => Vector.new0 ())
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
              | Aexp.While {expr, test} =>
                   let
                      val test' = elab test
                      val _ =
                         unify
                         (Cexp.ty test', Type.bool, fn (l1, _) =>
                          (Aexp.region test,
                           str "while test not of type bool",
                           seq [str "test type: ", l1]))
                      val expr' = elab expr
                      (* Diagnose if expr is not of type unit. *)
                      val _ =
                         let
                            fun doit f =
                               List.push
                               (sequenceTypeChecks, fn () =>
                                let
                                   val ty = Cexp.ty expr'
                                in
                                   if Type.isUnit ty 
                                      then ()
                                      else f (Aexp.region expr,
                                              str "while body not of type unit",
                                              seq [str "body type: ", Type.layoutPrettyBracket ty])
                                end)
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
      and elabMatchFn (m: Amatch.t, preError, nest, kind, lay, noMatch) =
         let
            val arg = Var.newNoname ()
            val {argType, region, rules, ...} = elabMatch (m, preError, nest)
            val body =
               Cexp.casee {kind = kind,
                           lay = lay,
                           noMatch = noMatch,
                           nonexhaustiveExnMatch = nonexhaustiveExnMatch (),
                           nonexhaustiveMatch = nonexhaustiveMatch (),
                           redundantMatch = redundantMatch (),
                           region = region,
                           rules = rules,
                           test = Cexp.var (arg, argType)}
         in
           {arg = arg,
            argType = argType,
            body = body}
         end
      and elabMatch (m: Amatch.t, preError, nest: Nest.t) =
         let
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
                    fun lay () =
                       let
                          open Layout
                       in
                          approximate
                          (seq [Apat.layout pat, str " => ", Aexp.layout exp])
                       end
                    val (p, _) =
                       elaboratePat () (pat, E, {bind = true, isRvb = false},
                                        preError)
                    val _ =
                       unify
                       (Cpat.ty p, argType, preError, fn (l1, l2) =>
                        (Apat.region pat,
                         str "rule patterns disagree",
                         align [seq [str "pattern:  ", l1],
                                seq [str "previous: ", l2],
                                seq [str "in: ", lay ()]]))
                    val e = elabExp (exp, nest, NONE)
                    val _ =
                       unify
                       (Cexp.ty e, resultType, preError, fn (l1, l2) =>
                        (Aexp.region exp,
                         str "rule results disagree",
                         align [seq [str "result:   ", l1],
                                seq [str "previous: ", l2],
                                seq [str "in: ", lay ()]]))
                    val e =
                       Cexp.enterLeave
                       (e, 
                        profileBody andalso !Control.profileBranch, 
                        fn () =>
                        SourceInfo.function {name = "<branch>" :: nest,
                                             region = Aexp.region exp})
                 in
                    {exp = e,
                     lay = SOME lay,
                     pat = p}
                 end))
         in
            {argType = argType,
             region = region,
             resultType = resultType,
             rules = rules}
         end
      val ds = elabDec (Scope.scope d, nest, true)
      (* List.insertionSort is anti-stable;
       * hence, it sorts and reverses the overloads. 
       *)
      val _ = List.foreach (List.insertionSort
                            (!overloads, fn ((x,_),(y,_)) =>
                             Priority.<= (y, x)),
                            fn (_,p) => (p (); ()))
      val _ = overloads := []
   in
      ds
   end

fun reportUndeterminedTypes () =
   (List.foreach (rev (!freeTyvarChecks), fn p => p ())
    ; freeTyvarChecks := [])

fun reportSequenceNonUnit () =
   (List.foreach (rev (!sequenceTypeChecks), fn p => p ())
    ; sequenceTypeChecks := [])

end
