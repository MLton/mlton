(* Copyright (C) 2009-2010,2015,2017 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor ElaborateEnv (S: ELABORATE_ENV_STRUCTS): ELABORATE_ENV =
struct

open S

local
   open Control.Elaborate
in
   val warnUnused = fn () => current warnUnused
end

local
   open Layout
in
   val align = align
   val alignPrefix = alignPrefix
   (* val empty = empty *)
   val mayAlign = mayAlign
   val seq = seq
   val str = str
   val bracket = fn l =>
      seq [str "[", l, str "]"]
end

local
   open Ast
in
   structure Basid = Basid
   structure Fctid = Fctid
   structure Strid = Strid
   structure Longtycon = Longtycon
   structure Priority = Priority
   structure Sigid = Sigid
   structure Strid = Strid
   structure Symbol = Symbol
end

fun layoutLong (ids: Layout.t list) =
   let
      open Layout
   in
      seq (separate (ids, "."))
   end

fun layoutStrids (ss: Strid.t list): Layout.t =
   layoutLong (List.map (ss, Strid.layout))

fun layoutLongRev (ss: Strid.t list, id: Layout.t) =
   (seq o List.fold)
   (ss, [id], fn (s, ls) =>
    Strid.layout s :: str "." :: ls)
fun toStringLongRev (ss: Strid.t list, id: Layout.t) =
   Layout.toString (layoutLongRev (ss, id))

local
   open CoreML
in
   structure Con = Con
   structure Dec = Dec
   structure Exp = Exp
   structure Pat = Pat
   structure Tycon = Tycon
   structure Tyvar = Tyvar
   structure Var = Var
end

local
   open Tycon
in
   structure AdmitsEquality = AdmitsEquality
   structure Kind = Kind
   structure Symbol = Symbol
end

local
   open TypeEnv
in
   structure Scheme = Scheme
   structure Type = Type
end

structure Decs = Decs (structure CoreML = CoreML)

structure Tycon =
   struct
      open Tycon
      open TypeEnv.TyconExt
   end

structure Tyvar =
   struct
      open Tyvar
      open TypeEnv.TyvarExt
      fun fromAst a =
         makeString (Ast.Tyvar.toString a,
                     {equality = Ast.Tyvar.isEquality a})
   end

structure TyvarEnv =
   struct
      datatype t = T of {cur: (Ast.Tyvar.t * Tyvar.t) list ref,
                         get: Ast.Tyvar.t -> Tyvar.t list ref}
      fun new () =
         let
            val {get: Ast.Tyvar.t -> Tyvar.t list ref, ...} =
               Property.get
               (Symbol.plist o Ast.Tyvar.toSymbol,
                Property.initFun (fn _ => ref []))
            val cur = ref []
         in
            T {get = get, cur = cur}
         end
      fun peekTyvar (T {get, ...}, a) =
         case !(get a) of
            [] => NONE
          | a'::_ => SOME a'
      fun lookupTyvar (env, a) =
         case peekTyvar (env, a) of
            NONE =>
               let
                  val _ =
                     Control.error
                     (Ast.Tyvar.region a,
                      seq [str "undefined type variable: ",
                           Ast.Tyvar.layout a],
                      Layout.empty)
               in
                  NONE
               end
          | SOME tv => SOME tv
      fun scope (T {cur, get, ...}, bs, th) =
         let
            val bs' = Vector.map (bs, Tyvar.fromAst)
            val () =
               Vector.foreach2
               (bs, bs', fn (b, b') =>
                (List.push (cur, (b, b'))
                 ; List.push (get b, b')))
            val res = th bs'
            val () =
               Vector.foreach
               (bs, fn b =>
                (ignore (List.pop cur)
                 ; ignore (List.pop (get b))))
         in
            res
         end

      val E = new ()
      val lookupTyvar = fn a =>
         lookupTyvar (E, a)
      val scope = fn (bs, th) =>
         scope (E, bs, th)
      (*
      val makeLayoutPretty = fn () =>
         let
            val {destroy, get = layoutPretty, set = setLayoutPretty, ...} =
               Property.destGetSet
               (Tyvar.plist, Property.initFun Tyvar.layout)
            val T {cur, ...} = E
            val pre = fn () =>
               List.foreach
               (!cur, fn (a, a') =>
                setLayoutPretty (a', Ast.Tyvar.layout a))
            val pre = ClearablePromise.delay pre
            val destroy = fn () =>
               (ClearablePromise.clear pre
                ; destroy ())
            val layoutPretty = fn a' =>
               (ClearablePromise.force pre
                ; layoutPretty a')
         in
            {destroy = destroy,
             layoutPretty = layoutPretty}
         end
      *)
      val makeLayoutPretty = fn () =>
         let
            fun layoutPretty a' =
               let
                  val T {cur, ...} = E
               in
                  case List.peek (!cur, fn (_, b') => Tyvar.equals (a', b')) of
                     NONE => Tyvar.layout a'
                   | SOME (a, _) => Ast.Tyvar.layout a
               end
         in
            {destroy = fn () => (),
             layoutPretty = layoutPretty}
         end
   end

val insideFunctor = ref false

fun amInsideFunctor () = !insideFunctor

structure Scope =
   struct
      structure Unique = UniqueId ()
      datatype t = T of {unique: Unique.t}

      local
         fun make f (T r) = f r
      in
         val unique = make #unique
      end

      fun new (): t =
         T {unique = Unique.new ()}

      fun equals (s, s') = Unique.equals (unique s, unique s')
   end

structure Uses:
   sig
      type 'a t

      structure Extend:
         sig
            val new: {rebind: {domain: 'a, uses: 'a t} option} -> 'a t option
            val old: 'a t -> {rebind: {domain: 'a, uses: 'a t} option} -> 'a t option
            val fromIsRebind: {isRebind: bool} -> {rebind: {domain: 'a, uses: 'a t} option} -> 'a t option
         end

      val add: 'a t * 'a -> unit
      val all: 'a t -> 'a list
      val clear: 'a t -> unit
      val forceUsed: 'a t -> unit
      val hasUse: 'a t -> bool
      val isUsed: 'a t -> bool
      val new: unit -> 'a t
   end =
   struct
      datatype 'a t = T of {direct: 'a list ref,
                            forceUsed: bool ref}

      fun new () = T {direct = ref [],
                      forceUsed = ref false}

      fun add (T {direct, ...}, a) = List.push (direct, a)

      fun forceUsed (T {forceUsed = r, ...}) = r := true

      fun clear (T {direct, ...}) = direct := []

      fun all (T {direct, ...}) = !direct

      fun hasUse (T {direct, ...}): bool =
         not (List.isEmpty (!direct))

      fun isUsed (u as T {forceUsed, ...}): bool =
         !forceUsed orelse hasUse u

      structure Extend =
         struct
            fun new _ = NONE
            fun old uses _ = SOME uses
            fun fromIsRebind {isRebind} =
               if isRebind
                  then (fn {rebind} =>
                        case rebind of
                           NONE =>
                              Error.bug "ElaborateEnv.Uses.Extend.fromIsRebind"
                         | SOME {domain = _, uses} =>
                              SOME uses)
                  else new
         end
   end

structure Class =
   struct
      datatype t = Bas | Con | Exn | Fix | Fct | Sig | Str | Typ | Var

      val toString =
         fn Bas => "basis"
          | Con => "constructor"
          | Exn => "exception"
          | Fix => "fixity"
          | Fct => "functor"
          | Sig => "signature"
          | Str => "structure"
          | Typ => "type"
          | Var => "variable"
   end

structure Vid =
   struct
      datatype t =
         Con of Con.t
       | Exn of Con.t
       | Overload of Priority.t * (Var.t * Scheme.t) vector
       | Var of Var.t

      val statusPretty =
         fn Con _ => "constructor"
          | Exn _ => "exception"
          | Overload _ => "overload"
          | Var _ => "variable"

      fun layout vid =
         let
            open Layout
            val (name, l) =
               case vid of
                  Con c => ("Con", Con.layout c)
                | Exn c => ("Exn", Con.layout c)
                | Overload (p,xts) =>
                     (concat ["Overload (",
                              Layout.toString (Priority.layout p),
                              ")"],
                      Vector.layout (tuple2 (Var.layout, Scheme.layout))
                      xts)
                | Var v => ("Var", Var.layout v)
         in
            paren (seq [str name, str " ", l])
         end

      val deVar =
         fn Var v => SOME v
          | _ => NONE

      val deCon =
         fn Con c => SOME c
          | Exn c => SOME c
          | _ => NONE

      val deExn =
         fn Exn c => SOME c
          | _ => NONE

      val class =
         fn Con _ => Class.Con
          | Exn _ => Class.Exn
          | Overload _ => Class.Var
          | Var _ => Class.Var
   end

structure TypeStr =
   struct
      structure Cons :
         sig
            type t
            val dest: t -> {con: Con.t,
                            name: Ast.Con.t,
                            scheme: Scheme.t,
                            uses: Ast.Vid.t Uses.t} vector
            val fromSortedVector: {con: Con.t,
                                   name: Ast.Con.t,
                                   scheme: Scheme.t,
                                   uses: Ast.Vid.t Uses.t} vector -> t
            val fromVector: {con: Con.t,
                             name: Ast.Con.t,
                             scheme: Scheme.t,
                             uses: Ast.Vid.t Uses.t} vector -> t
            val layout: t -> Layout.t
            val map: t * ({con: Con.t,
                           name: Ast.Con.t,
                           scheme: Scheme.t,
                           uses: Ast.Vid.t Uses.t}
                          -> {con: Con.t,
                              scheme: Scheme.t,
                              uses: Ast.Vid.t Uses.t}) -> t
         end =
         struct
            datatype t = T of {con: Con.t,
                               name: Ast.Con.t,
                               scheme: Scheme.t,
                               uses: Ast.Vid.t Uses.t} vector

            fun dest (T v) = v

            val fromSortedVector = T

            fun fromVector v =
               (fromSortedVector o QuickSort.sortVector)
               (v, fn ({name = name1, ...}, {name = name2, ...}) =>
                case Ast.Con.compare (name1, name2) of
                   LESS => true
                 | EQUAL => true
                 | GREATER => false)

            fun map (T v, f) =
               (T o Vector.map)
               (v, fn elt as {name, ...} =>
                let
                   val {con, scheme, uses} =
                      f elt
                in
                   {con = con,
                    name = name,
                    scheme = scheme,
                    uses = uses}
                end)

            fun layout (T v) =
               Vector.layout (fn {name, scheme, ...} =>
                              seq [Ast.Con.layout name,
                                   str ": ", Scheme.layout scheme])
               v
         end

      datatype node =
         Datatype of {cons: Cons.t,
                      tycon: Tycon.t}
       | Scheme of Scheme.t
       | Tycon of Tycon.t
      type t = node

      val node = fn s => s

      fun kind s =
         case node s of
            Datatype {tycon, ...} => Tycon.kind tycon
          | Scheme s => Scheme.kind s
          | Tycon c => Tycon.kind c

      fun layout t =
         let
            open Layout
         in
            case node t of
               Datatype {tycon, cons} =>
                  seq [str "Datatype ",
                       record [("tycon", Tycon.layout tycon),
                               ("cons", Cons.layout cons)]]
             | Scheme s => seq [str "Scheme ", Scheme.layout s]
             | Tycon c => seq [str "Tycon ", Tycon.layout c]
         end

      fun admitsEquality (s: t): AdmitsEquality.t =
         case node s of
            Datatype {tycon = c, ...} => Tycon.admitsEquality c
          | Scheme s => if Scheme.admitsEquality s
                           then AdmitsEquality.Sometimes
                        else AdmitsEquality.Never
          | Tycon c => Tycon.admitsEquality c

      fun explainDoesNotAdmitEquality (s: t, {layoutPrettyTycon}): Layout.t =
         let
            fun doitScheme s =
               case Scheme.checkEquality (s, {layoutPrettyTycon = layoutPrettyTycon}) of
                  SOME l => l
                | NONE => Error.bug "ElaborateEnv.TypeStr.explainDoesNotAdmitEquality.doitScheme: NONE"
         in
            case node s of
               Datatype {cons, ...} =>
                  let
                     val extra = ref false
                     val cons =
                        Vector.toList
                        (Vector.keepAllMap
                         (Cons.dest cons, fn {name, scheme, ...} =>
                          let
                             val (tyvars, ty) = Scheme.dest scheme
                          in
                             case Type.deArrowOpt ty of
                                NONE => (extra := true; NONE)
                              | SOME (arg, _) =>
                                   let
                                      val argScheme =
                                         Scheme.make {canGeneralize = true,
                                                      ty = arg,
                                                      tyvars = tyvars}
                                   in
                                      case Scheme.checkEquality (argScheme, {layoutPrettyTycon = layoutPrettyTycon}) of
                                         NONE => (extra := true; NONE)
                                       | SOME l => SOME (seq [Ast.Con.layout name, str " of ", l])
                                   end
                          end))
                     val cons =
                        if !extra
                           then List.snoc (cons, str "...")
                           else cons
                     val cons = alignPrefix (cons, "| ")
                  in
                     cons
                  end
             | Scheme s => doitScheme s
             | Tycon c => doitScheme (Scheme.fromTycon c)
         end

      fun apply (t: t, tys: Type.t vector): Type.t =
         case node t of
            Datatype {tycon, ...} => Type.con (tycon, tys)
          | Scheme s => Scheme.apply (s, tys)
          | Tycon c => Type.con (c, tys)

      fun toTyconOpt s =
         case node s of
            Datatype {tycon, ...} => SOME tycon
          | Scheme s =>
               let
                  val (tyvars, ty) = Scheme.dest s
               in
                  case Type.deEta (ty, tyvars) of
                   NONE => NONE
                 | SOME c =>
                      if Tycon.equals (c, Tycon.arrow)
                         orelse Tycon.equals (c, Tycon.tuple)
                         then NONE
                         else SOME c
               end
          | Tycon c => SOME c

      fun data (tycon, cons) =
         Datatype {tycon = tycon, cons = cons}

      val def = Scheme

      val tycon = Tycon

      fun abs t =
         case node t of
            Datatype {tycon = c, ...} => tycon c
          | _ => t
   end

local
   open TypeStr
in
   structure Cons = Cons
end

structure Interface = Interface (structure Ast = Ast
                                 structure AdmitsEquality = AdmitsEquality
                                 structure Kind = Kind
                                 structure EnvTycon = Tycon
                                 structure EnvTypeStr = TypeStr
                                 structure Tyvar = Tyvar)

structure Interface =
   struct
      structure Econs = Cons
      structure Escheme = Scheme
      structure Etycon = Tycon
      structure Etype = Type
      structure EtypeStr = TypeStr
      structure Etyvar = Tyvar
      open Interface

      fun flexibleTyconToEnv (fc: FlexibleTycon.t): EtypeStr.t =
         let
            datatype z = datatype FlexibleTycon.realization
         in
            case FlexibleTycon.realization fc of
               SOME (ETypeStr s) => s
             | SOME (TypeStr s) => typeStrToEnv s
             | NONE =>
                  let
                     (* A shadowed flexible tycon was not reported as
                      * a flexible tycon and was not realized. *)
                     val () =
                        Assert.assert
                        ("ElaborateEnv.Interface.flexibleTyconToEnv",
                         fn () => !Control.numErrors > 0)
                     val {admitsEquality = ae, kind = k,
                          prettyDefault = pd, ...} =
                        FlexibleTycon.dest fc
                     val pd = "??." ^ pd
                     val c =
                        Etycon.make {admitsEquality = ae,
                                     kind = k,
                                     name = "<bogus>",
                                     prettyDefault = pd,
                                     region = Region.bogus}
                     val tyStr = EtypeStr.tycon c
                     val () = FlexibleTycon.realize (fc, tyStr)
                  in
                     tyStr
                  end
         end
      and tyconToEnv (t: Tycon.t): EtypeStr.t =
         let
            open Tycon
         in
            case t of
               Flexible c => flexibleTyconToEnv c
             | Rigid c => EtypeStr.tycon c
         end
      and typeToEnv (t: Type.t): Etype.t =
         Type.hom (t, {con = fn (c, ts) => EtypeStr.apply (tyconToEnv c, ts),
                       record = Etype.record,
                       var = Etype.var})
      and schemeToEnv (Scheme.T {ty, tyvars}): Escheme.t =
         Escheme.make {canGeneralize = true,
                       ty = typeToEnv ty,
                       tyvars = tyvars}
      and consToEnv cons: Econs.t =
         (Econs.fromSortedVector o Vector.map)
         (Cons.dest cons, fn {name, scheme} =>
          {con = Con.newNoname (),
           name = name,
           scheme = schemeToEnv scheme,
           uses = Uses.new ()})
      and typeStrToEnv (s: TypeStr.t): EtypeStr.t =
         let
            datatype z = datatype TypeStr.node
         in
            case TypeStr.node s of
               Datatype {cons, tycon, ...} =>
                  let
                     fun data c =
                        EtypeStr.data (c, consToEnv cons)
                  in
                     case tycon of
                        Tycon.Flexible c =>
                           let
                              val typeStr = flexibleTyconToEnv c
                           in
                              case EtypeStr.toTyconOpt typeStr of
                                 SOME c => data c
                               | _ => Error.bug
                                      (Layout.toString
                                       (seq [str "ElaborateEnv.Interface.typeStrToEnv ",
                                             str "datatype ",
                                             TypeStr.layout s,
                                             str " realized with type structure ",
                                             EtypeStr.layout typeStr]))
                           end
                      | Tycon.Rigid c => data c
                  end
             | Scheme s =>
                  EtypeStr.def (schemeToEnv s)
             | Tycon {tycon, ...} =>
                  EtypeStr.abs (tyconToEnv tycon)
         end

      structure FlexibleTycon =
         struct
            open FlexibleTycon

            val toEnv = flexibleTyconToEnv

            fun dummyTycon (fc, name, strids, {prefix}) =
               let
                  val {admitsEquality = ae, kind = k, ...} =
                     FlexibleTycon.dest fc
                   val r = Ast.Tycon.region name
                   val n = Ast.Tycon.toString name
                   val pd =
                      prefix ^ toStringLongRev (strids, Ast.Tycon.layout name)
                   val c =
                      Etycon.make {admitsEquality = ae,
                                   kind = k,
                                   name = n,
                                   prettyDefault = pd,
                                   region = r}
               in
                  c
               end
         end

      structure Tycon =
         struct
            open Tycon

            val fromEnv = Rigid
         end

      structure Type =
         struct
            open Type

            fun fromEnv (t: Etype.t): t =
               let
                  fun con (c, ts) =
                     Type.con (Tycon.fromEnv c, ts)
               in
                  Etype.hom (t, {con = con,
                                 expandOpaque = false,
                                 record = record,
                                 replaceSynonyms = false,
                                 var = var})
               end
         end

      structure Scheme =
         struct
            open Scheme

            val toEnv = schemeToEnv

            fun fromEnv (s: Escheme.t): t =
               let
                  val (tyvars, ty) = Escheme.dest s
               in
                  Scheme.T {ty = Type.fromEnv ty,
                            tyvars = tyvars}
               end
         end

      structure Cons =
         struct
            open Cons

            fun fromEnv (cons): t =
               (fromSortedVector o Vector.map)
               (Econs.dest cons, fn {name, scheme, ...} =>
                {name = name,
                 scheme = Scheme.fromEnv scheme})
         end

      structure TypeStr =
         struct
            open TypeStr

            val toEnv = typeStrToEnv

            fun fromEnv (s: EtypeStr.t) =
               case EtypeStr.node s of
                  EtypeStr.Datatype {cons, tycon} =>
                     data (Tycon.fromEnv tycon,
                           Cons.fromEnv cons,
                           true)
                | EtypeStr.Scheme s => def (Scheme.fromEnv s)
                | EtypeStr.Tycon c => def (Scheme.fromTycon (Tycon.fromEnv c))

            structure Sort =
               struct
                  datatype t =
                     Datatype of {tycon: Etycon.t, cons: Econs.t, repl: bool}
                   | Scheme of Escheme.t
                   | Type of {admitsEquality: bool}
               end

            fun sort (sigStr, rlzStr, representative) =
               case (representative, node sigStr, EtypeStr.node rlzStr) of
                  (false, Datatype _, EtypeStr.Datatype {tycon = rlzTycon, cons = rlzCons}) =>
                     Sort.Datatype {tycon = rlzTycon, cons = rlzCons, repl = true}
                | (false, Datatype _, EtypeStr.Scheme _) =>
                     Error.bug "ElaborateEnv.Interface.TypeStr.sort: {repr = false, sigStr = Datatype _, rlzStr = Scheme _}"
                | (false, Datatype _, EtypeStr.Tycon _) =>
                     Error.bug "ElaborateEnv.Interface.TypeStr.sort: {repr = false, sigStr = Datatype _, rlzStr = Tycon _}"
                | (false, _, rlzStr) =>
                     Sort.Scheme (case rlzStr of
                                     EtypeStr.Datatype {tycon, ...} =>
                                        Escheme.fromTycon tycon
                                   | EtypeStr.Scheme s => s
                                   | EtypeStr.Tycon c =>
                                        Escheme.fromTycon c)
                | (true, Datatype {repl = false, ...}, EtypeStr.Datatype {tycon = rlzTycon, cons = rlzCons}) =>
                     Sort.Datatype {tycon = rlzTycon, cons = rlzCons, repl = false}
                | (true, Datatype {repl = false, ...}, EtypeStr.Scheme _) =>
                     Error.bug "ElaborateEnv.Interface.TypeStr.sort: {repr = true, sigStr = Datatype {repl = false, ...}, rlzStr = Scheme _}"
                | (true, Datatype {repl = false, ...}, EtypeStr.Tycon _) =>
                     Error.bug "ElaborateEnv.Interface.TypeStr.sort: {repr = true, sigStr = Datatype {repl = false, ...}, rlzStr = Tycon _}"
                | (true, Datatype {repl = true, ...}, _) =>
                     Error.bug "ElaborateEnv.Interface.TypeStr.sort: {repr = true, sigStr = Datatype {repl = true, ...}}"
                | (true, Scheme _, _) =>
                     Error.bug "ElaborateEnv.Interface.TypeStr.sort: {repr = true, sigStr = Scheme _}"
                | (true, Tycon _, _) =>
                     (case admitsEquality sigStr of
                         AdmitsEquality.Always => Sort.Type {admitsEquality = true}
                       | AdmitsEquality.Never => Sort.Type {admitsEquality = false}
                       | AdmitsEquality.Sometimes => Sort.Type {admitsEquality = true})

            val sort = fn (name, sigStr, rlzStr,
                           flexTyconMap: FlexibleTycon.t TyconMap.t) =>
               sort (sigStr, rlzStr,
                     Option.isSome (TyconMap.peekTycon (flexTyconMap, name)))
         end

      fun layouts {interfaceSigid, layoutPrettyTycon, setLayoutPrettyTycon} =
         let
            val empty = Layout.empty
            val indent = fn l => Layout.indent (l, 3)
            val isEmpty = Layout.isEmpty
            val tuple = Layout.tuple

            val {destroy = destroyLayoutPrettyTyvar,
                 layoutPretty = layoutPrettyTyvar,
                 localInit = localInitLayoutPrettyTyvar} =
               Etyvar.makeLayoutPretty ()
            val {destroy = destroyLayoutPrettyType,
                 layoutPretty = layoutPrettyType} =
               Etype.makeLayoutPretty
               {expandOpaque = false,
                layoutPrettyTycon = layoutPrettyTycon,
                layoutPrettyTyvar = layoutPrettyTyvar}
            fun layoutPrettyScheme s =
               let
                  val (bs, t) = Escheme.dest s
                  val () = localInitLayoutPrettyTyvar bs
               in
                  #1 (layoutPrettyType t)
               end

            fun layoutValSpec (strids, name, (sigStatus, sigScheme), {compact, con, def}) =
               let
                  val rlzScheme = Scheme.toEnv sigScheme
                  fun doit kw =
                     let
                        val lay =
                           mayAlign
                           [seq [str kw, str " ",
                                 layoutLongRev (strids, Ast.Vid.layout name),
                                 str (if Ast.Vid.isSymbolic name then " : " else ": "),
                                 layoutPrettyScheme rlzScheme],
                            indent (if def
                                       then seq [str "(* @ ",
                                                 Region.layout (Ast.Vid.region name),
                                                 str " *)"]
                                       else empty)]
                        val lay =
                           if compact
                              then Layout.compact lay
                              else lay
                     in
                        SOME lay
                     end
               in
                  case sigStatus of
                     Status.Con =>
                        if con
                           then doit "con"
                           else NONE
                   | Status.Exn =>
                        if con
                           then doit "exn"
                           else let
                                   val lay =
                                      mayAlign
                                      [seq [str "exception ",
                                            layoutLongRev (strids, Ast.Vid.layout name),
                                            case Etype.deArrowOpt (Escheme.ty rlzScheme) of
                                               NONE => empty
                                             | SOME (ty, _) => seq [str " of ",
                                                                    #1 (layoutPrettyType ty)]],
                                       indent (if def
                                                  then seq [str "(* @ ",
                                                            Region.layout (Ast.Vid.region name),
                                                            str " *)"]
                                                  else empty)]
                                   val lay =
                                      if compact
                                         then Layout.compact lay
                                         else lay
                                in
                                   SOME lay
                                end
                   | Status.Var =>
                        doit "val"
               end
            fun layoutTypeSpec (strids, name, sigStr,
                                {compact, def, flexTyconMap}) =
               let
                  val lay = #1 o layoutPrettyType
                  val rlzStr = TypeStr.toEnv sigStr
                  val sort = TypeStr.sort (name, sigStr, rlzStr, flexTyconMap)
                  val arity =
                     case Interface.TypeStr.kind sigStr of
                        Kind.Arity sigArity => sigArity
                      | _ => Error.bug "ElaborateEnv.transparentCut.layouts.layoutTypeSpec: sigArity"
                  val tyvars =
                     Vector.tabulate
                     (arity, fn _ =>
                      Etyvar.makeNoname {equality = false})
                  val () = localInitLayoutPrettyTyvar tyvars
                  val tyargs = Vector.map (tyvars, Etype.var)
                  val tyvars = Vector.map (tyvars, layoutPrettyTyvar)
                  val tyvars =
                     case Vector.length tyvars of
                        0 => empty
                      | 1 => Vector.first tyvars
                      | _ => tuple (Vector.toList tyvars)
                  datatype sort = datatype TypeStr.Sort.t
                  val (kw, rest) =
                     case sort of
                        Datatype {repl, cons, ...} =>
                           let
                              val cons =
                                 Vector.toListMap
                                 (Econs.dest cons, fn {name, scheme, ...} =>
                                  let
                                     val ty = Escheme.apply (scheme, tyargs)
                                  in
                                     seq [Ast.Con.layout name,
                                          case Etype.deArrowOpt ty of
                                             NONE => empty
                                           | SOME (ty, _) => seq [str " of ", lay ty]]
                                  end)
                              val cons =
                                 List.mapi
                                 (cons, fn (i, l) =>
                                  if i = 0
                                     then l
                                     else Layout.indent (seq [str "| ", l], ~2))
                              val rest =
                                 if repl
                                    then let
                                            val repl =
                                               seq [str "(* = datatype ",
                                                    lay (EtypeStr.apply (rlzStr, tyargs)),
                                                    str " *)"]
                                         in
                                            List.snoc (cons, Layout.indent (repl, ~2))
                                         end
                                    else cons
                           in
                              ("datatype",
                               SOME (mayAlign rest))
                           end
                      | Scheme scheme =>
                           ("type",
                            SOME (lay (Escheme.apply (scheme, tyargs))))
                      | Type {admitsEquality} =>
                           (if admitsEquality then "eqtype" else "type",
                            NONE)
                  val lay =
                     mayAlign
                     [seq [str kw, str " ",
                           tyvars,
                           if isEmpty tyvars then empty else str " ",
                           layoutLongRev (strids, Ast.Tycon.layout name),
                           case rest of
                              NONE => empty
                            | SOME rest => seq [str " = ", rest]],
                      indent (if def
                                 then seq [str "(* @ ",
                                           Region.layout (Ast.Tycon.region name),
                                           str " *)"]
                                 else empty)]
                  val lay =
                     if compact
                        then Layout.compact lay
                        else lay
               in
                  lay
               end
            fun layoutStrSpec (strids, name, I,
                               {compact, def, elide, flexTyconMap}) =
               let
                  val bind = seq [str "structure ",
                                  layoutLongRev (strids, Ast.Strid.layout name),
                                  str ":"]
                  val flexTyconMap =
                     Option.fold
                     (TyconMap.peekStrid (flexTyconMap, name),
                      TyconMap.empty (),
                      fn (flexTyconMap, _) => flexTyconMap)
                  val {abbrev, full} =
                     layoutSigRlz (I,
                                   {compact = compact,
                                    elide = elide,
                                    flexTyconMap = flexTyconMap})
                  val def =
                     if def
                        then seq [str "(* @ ",
                                  Region.layout (Ast.Strid.region name),
                                  str " *)"]
                        else empty
                  val full = fn () =>
                     align [bind, indent (full ()), indent def]
               in
                  case abbrev () of
                     NONE => full ()
                   | SOME sigg =>
                        let
                           val lay =
                              mayAlign
                              [seq [bind, str " ", sigg],
                               indent def]
                           val lay =
                              if compact
                                 then Layout.compact lay
                                 else lay
                        in
                           lay
                        end
               end
            and layoutSigFlex (I,
                               {compact, elide}) =
               let
                  fun realize (TyconMap.T {strs, types}, strids) =
                     let
                        val () =
                           Array.foreach
                           (strs, fn (name, tm) =>
                            realize (tm, name :: strids))
                        val () =
                           Array.foreach
                           (types, fn (name, fc) =>
                            let
                               val c =
                                  FlexibleTycon.dummyTycon
                                  (fc, name, strids, {prefix = "_sig."})
                               val () =
                                  setLayoutPrettyTycon
                                  (c, Etycon.layoutPrettyDefault c)
                               val () =
                                  FlexibleTycon.realize
                                  (fc, EtypeStr.tycon c)
                            in
                               ()
                            end)
                     in
                        ()
                     end
                  val rlzI = copy I
                  val flexTyconMap = flexibleTycons rlzI
                  val () = realize (flexTyconMap, [])
               in
                  layoutSigRlz (rlzI,
                                {compact = compact,
                                 elide = elide,
                                 flexTyconMap = flexTyconMap})
               end
            and layoutSigRlz (I,
                              {compact, elide, flexTyconMap}) =
               let
                  fun abbrev () =
                     case interfaceSigid (Interface.original I) of
                        NONE => NONE
                      | SOME (s, I') =>
                           SOME (layoutSigRlzAbbrev (s, I', I,
                                                     {compact = compact,
                                                      flexTyconMap = flexTyconMap}))
                  fun full () =
                     layoutSigRlzFull (I,
                                       {compact = compact,
                                        elide = elide,
                                        flexTyconMap = flexTyconMap})
               in
                  {abbrev = abbrev,
                   full = full}
               end
            and layoutSigRlzFull (I,
                                  {compact,
                                   elide: {strs: (int * int) option,
                                           types: (int * int) option,
                                           vals: (int * int) option},
                                   flexTyconMap}) =
               let
                  val {strs, types, vals} = Interface.dest I
                  fun doit (a, layout, elide) =
                     let
                        val specs =
                           Array.foldr
                           (a, [], fn ((name, range), ls) =>
                            case layout (name, range) of
                               NONE => ls
                             | SOME l => l :: ls)
                     in
                        case elide of
                           NONE => align specs
                         | SOME (n, m) =>
                              let
                                 val l = List.length specs
                              in
                                 if n + m + 1 < l
                                 then align [align (List.dropSuffix (specs, l - n)),
                                             str "...",
                                             align (List.dropPrefix (specs, l - m))]
                                 else align specs
                              end
                     end
                  val layoutTypeSpec =
                     fn (name, sigStr) =>
                     layoutTypeSpec
                     ([], name, sigStr,
                      {compact = compact,
                       def = false,
                       flexTyconMap = flexTyconMap})
                  val layoutValSpec =
                     fn (name, (sigStatus, sigScheme)) =>
                     layoutValSpec
                     ([], name, (sigStatus, sigScheme),
                      {compact = compact,
                       con = false,
                       def = false})
                  val layoutStrSpec =
                     fn (name, I) =>
                     layoutStrSpec
                     ([], name, I,
                      {compact = compact,
                       def = false,
                       elide = elide,
                       flexTyconMap = flexTyconMap})
               in
                  align [str "sig",
                         indent (align [doit (types, SOME o layoutTypeSpec, #types elide),
                                        doit (vals, layoutValSpec, #vals elide),
                                        doit (strs, SOME o layoutStrSpec, #strs elide)]),
                         str "end"]
               end
            and layoutSigRlzAbbrev (s, I', I, {compact, flexTyconMap}) =
               let
                  val flexTyconMap' =
                     Interface.flexibleTycons I'
                  val wheres = ref []
                  fun loop (strids, flexTyconMap', I, flexTyconMap) =
                     let
                        val TyconMap.T {strs = strs', types = types'} =
                           flexTyconMap'
                        val _ =
                           Array.foreach
                           (strs', fn (name, flexTyconMap') =>
                            let
                               val I =
                                  valOf (Interface.peekStrid (I, name))
                               val flexTyconMap =
                                  Option.fold
                                  (TyconMap.peekStrid (flexTyconMap, name),
                                   TyconMap.empty (),
                                   fn (flexTyconMap, _) => flexTyconMap)
                            in
                               loop (name::strids, flexTyconMap', I, flexTyconMap)
                            end)
                        val _ =
                           Array.foreach
                           (types', fn (name, _) =>
                            let
                               val (_, sigStr) = valOf (Interface.peekTycon (I, name))
                               val flexTycon = TyconMap.peekTycon (flexTyconMap, name)
                            in
                               case flexTycon of
                                  NONE =>
                                     List.push
                                     (wheres,
                                      seq [str "where ",
                                           layoutTypeSpec (strids,
                                                           name,
                                                           Interface.TypeStr.abs sigStr,
                                                           {compact = compact,
                                                            def = false,
                                                            flexTyconMap = flexTyconMap})])
                                | SOME _ => ()
                            end)
                     in
                        ()
                     end
                  val () = loop ([], flexTyconMap', I, flexTyconMap)
                  val wheres = rev (!wheres)
                  val lay =
                     align (Ast.Sigid.layout s :: wheres)
               in
                  lay
               end
            fun layoutSigDefn (name, I, {compact, def}) =
               let
                  val bind = seq [str "signature ", Ast.Sigid.layout name, str " ="]
                  val {abbrev, full} = layoutSigFlex (I,
                                                      {compact = compact,
                                                       elide = {strs = NONE,
                                                                types = NONE,
                                                                vals = NONE}})
                  val origI = Interface.original I
                  val def =
                     if def
                        then seq [str "(* @ ",
                                  Region.layout (Ast.Sigid.region name),
                                  str " *)"]
                        else empty
                  val full = fn () =>
                     align [bind, indent (full ()), indent def]
               in
                  if Interface.equals (I, origI)
                     then full ()
                     else (case abbrev () of
                              NONE => full ()
                            | SOME sigg =>
                                 let
                                    val lay =
                                       mayAlign
                                       [seq [bind, str " ", sigg],
                                        indent def]
                                    val lay =
                                       if compact
                                          then Layout.compact lay
                                          else lay
                                 in
                                    lay
                                 end)
               end
         in
            {destroy = fn () => (destroyLayoutPrettyType ()
                                 ; destroyLayoutPrettyTyvar ()),
             destroyLayoutPrettyType = destroyLayoutPrettyType,
             destroyLayoutPrettyTyvar = destroyLayoutPrettyTyvar,
             localInitLayoutPrettyTyvar = localInitLayoutPrettyTyvar,
             layoutPrettyScheme = layoutPrettyScheme,
             layoutPrettyType = layoutPrettyType,
             layoutPrettyTyvar = layoutPrettyTyvar,
             layoutSigDefn = layoutSigDefn,
             layoutSigFlex = layoutSigFlex,
             layoutSigRlz = layoutSigRlz,
             layoutStrSpec = layoutStrSpec,
             layoutTypeSpec = layoutTypeSpec,
             layoutValSpec = layoutValSpec}
         end

      fun layoutPretty I =
         let
            val {destroy, layoutSigFlex, ...} =
               layouts {interfaceSigid = fn _ => NONE,
                        layoutPrettyTycon = Etycon.layoutPrettyDefault,
                        setLayoutPrettyTycon = fn _ => ()}
            val {full, ...} =
               layoutSigFlex
               (I,
                {compact = false,
                 elide = {strs = NONE,
                          types = NONE,
                          vals = NONE}})
            val res = full ()
            val () = destroy ()
         in
            res
         end
   end

local
   open Interface
in
   structure FlexibleTycon = FlexibleTycon
   structure Status = Status
   structure TyconMap = TyconMap
end
structure Status =
   struct
      open Status

      val class =
         fn Con => Class.Con
          | Exn => Class.Exn
          | Var => Class.Var

      fun fromVid vid =
         case vid of
            Vid.Con _ => Con
          | Vid.Exn _ => Exn
          | Vid.Overload _ => Var
          | Vid.Var _ => Var

      val kw: t -> string =
         fn Con => "con"
          | Exn => "exn"
          | Var => "val"

      val pretty: t -> string =
         fn Con => "constructor"
          | Exn => "exception"
          | Var => "variable"
   end

structure Time:>
   sig
      type t

      val >= : t * t -> bool
      val next: unit -> t
   end =
   struct
      type t = int

      val layout = Int.layout

      val op >= : t * t -> bool = op >=

      val c = Counter.new 0

      fun next () = Counter.next c

      val next = 
         Trace.trace 
         ("ElaborateEnv.Time.next", Unit.layout, layout) 
         next
   end

structure Info =
   struct
      (* The array is sorted by domain element. *)
      datatype ('a, 'b) t = T of {domain: 'a,
                                  range: 'b,
                                  time: Time.t,
                                  uses: 'a Uses.t} array

      fun layout (layoutDomain, layoutRange) (T a) =
         Array.layout (fn {domain, range, ...} =>
                       Layout.tuple [layoutDomain domain, layoutRange range])
         a

      fun isEmpty (T a) = Array.isEmpty a

      fun foreach (T a, f) =
         Array.foreach (a, fn {domain, range, ...} => f (domain, range))

      fun foreachByTime (T a, f) =
         let
            val a = Array.copy a
            val _ =
               QuickSort.sortArray
               (a, fn ({time = t, ...}, {time = t', ...}) =>
                Time.>= (t, t'))
         in
            foreach (T a, f)
         end

      fun peek (T a, domain: 'a, toSymbol: 'a -> Symbol.t) =
         Option.map
         (BinarySearch.search (a, fn {domain = d, ...} =>
                               Symbol.compare (toSymbol domain, toSymbol d)),
          fn i => Array.sub (a, i))

      fun keepAll (T a, f) = T (Array.keepAll (a, f))

      val map: ('a, 'b) t * ('b -> 'b) -> ('a, 'b) t =
         fn (T a, f) =>
         T (Array.map (a, fn {domain, range, time, uses} =>
                       {domain = domain,
                        range = f range,
                        time = time,
                        uses = uses}))

      val map2: ('a, 'b) t * ('a, 'b) t * ('b * 'b -> 'b) -> ('a, 'b) t =
         fn (T a, T a', f) =>
         T (Array.map2
            (a, a', fn ({domain, range = r, time, uses}, {range = r', ...}) =>
             {domain = domain,
              range = f (r, r'),
              time = time,
              uses = uses}))
   end

fun foreach2Sorted (abs: ('a * 'b) array,
                    info: ('a, 'c) Info.t,
                    equals: ('a * 'a -> bool),
                    f: ('a * 'b * (int * 'c) option -> unit)): unit =
   let
      val Info.T acs = info
      val _ =
         Array.fold
         (abs, 0, fn ((a, b), i) =>
          let
             fun find j =
                if j = Array.length acs
                   then (i, NONE)
                else
                   let
                      val {domain = a', range = c, ...} = Array.sub (acs, j)
                   in
                      if equals (a, a')
                         then (j + 1, SOME (j, c))
                      else find (j + 1)
                   end
             val (i, co) = find i
             val () = f (a, b, co)
          in
             i
          end)
   in
      ()
   end

(* ------------------------------------------------- *)
(*                     Structure                     *)
(* ------------------------------------------------- *)

structure Structure =
   struct
      datatype t = T of {interface: Interface.t option,
                         plist: PropertyList.t,
                         strs: (Ast.Strid.t, t) Info.t,
                         types: (Ast.Tycon.t, TypeStr.t) Info.t,
                         vals: (Ast.Vid.t, Vid.t * Scheme.t) Info.t}

      val ffi: t option ref = ref NONE

      local
         fun make f (T r) = f r
      in
         val interface = make #interface
         val plist = make #plist
      end

      fun layout (T {interface, strs, vals, types, ...}) =
         Layout.record
         [("interface", Option.layout Interface.layout interface),
          ("types", Info.layout (Ast.Tycon.layout, TypeStr.layout) types),
          ("vals", (Info.layout (Ast.Vid.layout,
                                 Layout.tuple2 (Vid.layout, Scheme.layout))
                    vals)),
          ("strs", Info.layout (Strid.layout, layout) strs)]

      fun eq (s: t, s': t): bool = PropertyList.equals (plist s, plist s')

      (* ------------------------------------------------- *)
      (*                       peek                        *)
      (* ------------------------------------------------- *)

      local
         fun make (field, toSymbol) (T fields, domain) =
            Option.map
            (Info.peek (field fields, domain, toSymbol),
             fn v as {uses, ...} =>
             (Uses.add (uses, domain); v))
      in
         val peekStrid' = make (#strs, Ast.Strid.toSymbol)
         val peekVid' = make (#vals, Ast.Vid.toSymbol)
         val peekTycon' = make (#types, Ast.Tycon.toSymbol)
      end

      fun peekStrid z = Option.map (peekStrid' z, #range)
      fun peekTycon z = Option.map (peekTycon' z, #range)
      fun peekVid z = Option.map (peekVid' z, #range)

      local
         fun make (from, de) (S, x) =
            case peekVid (S, from x) of
               NONE => NONE
             | SOME (vid, s) => Option.map (de vid, fn z => (z, s))
      in
         val peekCon = make (Ast.Vid.fromCon, Vid.deCon)
         val peekExn = make (Ast.Vid.fromCon, Vid.deExn)
         val peekVar = make (Ast.Vid.fromVar, Vid.deVar)
      end

      structure PeekResult =
         struct
            datatype 'a t =
               Found of 'a
             | UndefinedStructure of Strid.t list
         end

      fun peekStrids (S, strids) =
         let
            fun loop (S, strids, ac) =
               case strids of
                  [] => PeekResult.Found S
                | strid :: strids =>
                     case peekStrid (S, strid) of
                        NONE => PeekResult.UndefinedStructure (rev (strid :: ac))
                      | SOME S => loop (S, strids, strid :: ac)
         in
            loop (S, strids, [])
         end

      (* ------------------------------------------------- *)
      (*                   layoutPretty                    *)
      (* ------------------------------------------------- *)

      fun layouts {interfaceSigid, layoutPrettyTycon, setLayoutPrettyTycon} =
         let
            val elide = {strs = NONE, types = NONE, vals = NONE}
            val flexTyconMap = TyconMap.empty ()

            val {destroy, destroyLayoutPrettyType, destroyLayoutPrettyTyvar,
                 layoutPrettyScheme,
                 layoutPrettyType, layoutPrettyTyvar,
                 layoutSigDefn, layoutSigFlex, layoutSigRlz,
                 layoutStrSpec, layoutTypeSpec, layoutValSpec, ...} =
               Interface.layouts {interfaceSigid = interfaceSigid,
                                  layoutPrettyTycon = layoutPrettyTycon,
                                  setLayoutPrettyTycon = setLayoutPrettyTycon}

            fun layoutTypeDefn (strids, name, strStr, {compact, def}) =
               layoutTypeSpec
               (strids, name,
                Interface.TypeStr.fromEnv strStr,
                {compact = compact,
                 def = def,
                 flexTyconMap = flexTyconMap})
            fun layoutValDefn (strids, name, (strVid, strScheme), {compact, con, def}) =
               layoutValSpec
               (strids, name,
                (Status.fromVid strVid, Interface.Scheme.fromEnv strScheme),
                {compact = compact, con = con, def = def})
            local
               fun toInterface (T {interface, strs, types, vals, ...}) =
                  case interface of
                     NONE =>
                        let
                           fun doit (Info.T a, f) =
                              Array.map (a, f)
                           val types =
                              doit
                              (types, fn {domain = name, range = strStr, ...} =>
                               (name, Interface.TypeStr.fromEnv strStr))
                           val vals =
                              doit
                              (vals, fn {domain = name, range = (strVid, strScheme), ...} =>
                               (name, (Status.fromVid strVid, Interface.Scheme.fromEnv strScheme)))
                           val strs =
                              doit
                              (strs, fn {domain = name, range = S, ...} =>
                               (name, toInterface S))
                        in
                           Interface.new
                           {isClosed = true,
                            original = NONE,
                            strs = strs,
                            types = types,
                            vals = vals}
                        end
                   | SOME I => I
            in
               fun layoutStrDefn (strids, name, S, {compact, def}) =
                  layoutStrSpec
                  (strids, name, toInterface S,
                   {compact = compact,
                    def = def,
                    elide = elide,
                    flexTyconMap = flexTyconMap})
               fun layoutStr (S, {compact}) =
                  layoutSigRlz
                  (toInterface S,
                   {compact = compact,
                    elide = elide,
                    flexTyconMap = flexTyconMap})
            end
         in
            {destroy = destroy,
             destroyLayoutPrettyType = destroyLayoutPrettyType,
             destroyLayoutPrettyTyvar = destroyLayoutPrettyTyvar,
             layoutPrettyScheme = layoutPrettyScheme,
             layoutPrettyType = layoutPrettyType,
             layoutPrettyTyvar = layoutPrettyTyvar,
             layoutSigDefn = layoutSigDefn,
             layoutSigFlex = layoutSigFlex,
             layoutSigRlz = layoutSigRlz,
             layoutStr = layoutStr,
             layoutStrDefn = layoutStrDefn,
             layoutStrSpec = layoutStrSpec,
             layoutTypeDefn = layoutTypeDefn,
             layoutTypeSpec = layoutTypeSpec,
             layoutValDefn = layoutValDefn,
             layoutValSpec = layoutValSpec}
         end

      fun layoutPretty S =
         let
            val {destroy, layoutStr, ...} =
               layouts {interfaceSigid = fn _ => NONE,
                        layoutPrettyTycon = Tycon.layoutPrettyDefault,
                        setLayoutPrettyTycon = fn _ => ()}
            val res = #full (layoutStr (S, {compact = false})) ()
            val () = destroy ()
         in
            res
         end

      (* ------------------------------------------------- *)
      (*                     forceUsed                     *)
      (* ------------------------------------------------- *)

      local
         datatype handleUses = Clear | Force
         fun make handleUses =
            let
               fun loop (T f) =
                  let
                     fun doit (sel, forceRange) =
                        let
                           val Info.T a = sel f
                        in
                           Array.foreach
                           (a, fn {range, uses, ...} =>
                            let
                               val _ =
                                  case handleUses of
                                     Clear => Uses.clear uses
                                   | Force => Uses.forceUsed uses
                               val _ = forceRange range
                            in
                               ()
                            end)
                        end
                     val _ = doit (#strs, loop)
                     val _ = doit (#types, ignore)
                     val _ = doit (#vals, ignore)
                  in
                     ()
                  end
            in
               loop
            end
      in
         val forceUsed = make Force
      end

      (* ------------------------------------------------- *)
      (*                      realize                      *)
      (* ------------------------------------------------- *)

      fun realize (S: t, tm: 'a TyconMap.t,
                   f: (Ast.Tycon.t
                       * 'a
                       * TypeStr.t option
                       * {nest: Strid.t list}) -> unit): unit =
         let
            fun allNone (TyconMap.T {strs, types}, nest) =
               (Array.foreach (strs, fn (name, tm) => allNone (tm, name :: nest))
                ; Array.foreach (types, fn (name, flex) =>
                                 f (name, flex, NONE, {nest = nest})))
            fun loop (TyconMap.T {strs, types},
                      T {strs = strs', types = types', ...},
                      nest: Strid.t list) =
               let
                  val () =
                     foreach2Sorted
                     (strs, strs', Ast.Strid.equals,
                      fn (name, tm, S) =>
                      case S of
                         NONE => allNone (tm, name :: nest)
                       | SOME (_, S) => loop (tm, S, name :: nest))
                  val () =
                     foreach2Sorted
                     (types, types', Ast.Tycon.equals,
                      fn (name, flex, opt) =>
                      f (name, flex, Option.map (opt, #2), {nest = nest}))
               in
                   ()
               end
         in
            loop (tm, S, [])
         end

      (* ------------------------------------------------- *)
      (*                       dummy                       *)
      (* ------------------------------------------------- *)

      fun dummy (I: Interface.t, {prefix: string})
         : t * (t * (Tycon.t * TypeStr.t -> unit) -> unit) =
         let
            val time = Time.next ()
            val I = Interface.copy I
            fun realizeLoop (TyconMap.T {strs, types}, strids) =
               let
                  val strs =
                     Array.map
                     (strs, fn (name, tm) =>
                      (name, realizeLoop (tm, name :: strids)))
                  val types =
                     Array.map
                     (types, fn (name, flex) =>
                      let
                         val c =
                            FlexibleTycon.dummyTycon
                            (flex, name, strids,
                             {prefix = prefix})
                         val () =
                            FlexibleTycon.realize
                            (flex, TypeStr.tycon c)
                      in
                         (name, c)
                      end)
               in
                  TyconMap.T {strs = strs, types = types}
               end
            val flexible = realizeLoop (Interface.flexibleTycons I, [])
            val {get, ...} =
               Property.get
               (Interface.plist,
                Property.initRec
                (fn (I, get) =>
                 let
                    val {strs, types, vals} = Interface.dest I
                    val strs =
                       Array.map (strs, fn (name, I) =>
                                  {domain = name,
                                   range = get I,
                                   time = time,
                                   uses = Uses.new ()})
                    val types =
                       Array.map (types, fn (name, s) =>
                                  {domain = name,
                                   range = Interface.TypeStr.toEnv s,
                                   time = time,
                                   uses = Uses.new ()})
                    val vals =
                       Array.map
                       (vals, fn (name, (status, scheme)) =>
                        let
                           val con = CoreML.Con.newString o Ast.Vid.toString
                           val var = CoreML.Var.newString o Ast.Vid.toString
                           val vid =
                              case status of
                                 Status.Con => Vid.Con (con name)
                               | Status.Exn => Vid.Exn (con name)
                               | Status.Var => Vid.Var (var name)
                        in
                           {domain = name,
                            range = (vid, Interface.Scheme.toEnv scheme),
                            time = time,
                            uses = Uses.new ()}
                        end)
                 in
                    T {interface = SOME I,
                       plist = PropertyList.new (),
                       strs = Info.T strs,
                       types = Info.T types,
                       vals = Info.T vals}
                 end))
            val S = get I
            fun instantiate (S, f) =
               realize (S, flexible, fn (_, c, so, _) =>
                        case so of
                           NONE => Error.bug "ElaborateEnv.Structure.dummy.instantiate"
                         | SOME s => f (c, s))
         in
            (S, instantiate)
         end

      val dummy =
         Trace.trace ("ElaborateEnv.Structure.dummy",
                      Interface.layoutPretty o #1,
                      layoutPretty o #1)
         dummy

   end

(* ------------------------------------------------- *)
(*                  FunctorClosure                   *)
(* ------------------------------------------------- *)

structure FunctorClosure =
   struct
      datatype t =
         T of {apply: Structure.t * string list -> Decs.t * Structure.t option,
               argInterface: Interface.t,
               resultStructure: Structure.t option,
               summary: Structure.t -> Structure.t option}

      local
         fun make f (T r) = f r
      in
         val argInterface = make #argInterface
      end

      fun layout _ = Layout.str "<functor closure>"

      fun apply (T {apply, ...}, S, nest) = apply (S, nest)

      val apply =
         Trace.trace3 ("ElaborateEnv.FunctorClosure.apply",
                       layout,
                       Structure.layout,
                       List.layout String.layout,
                       (Option.layout Structure.layout) o #2)
         apply

      fun forceUsed (T {resultStructure, ...}) =
         Option.app (resultStructure, Structure.forceUsed)
   end

(* ------------------------------------------------- *)
(*                       Basis                       *)
(* ------------------------------------------------- *)

structure Basis =
   struct
      datatype t = T of {plist: PropertyList.t,
                         bass: (Ast.Basid.t, t) Info.t, 
                         fcts: (Ast.Fctid.t, FunctorClosure.t) Info.t,
                         fixs: (Ast.Vid.t, Ast.Fixity.t) Info.t,
                         sigs: (Ast.Sigid.t, Interface.t) Info.t,
                         strs: (Ast.Strid.t, Structure.t) Info.t,
                         types: (Ast.Tycon.t, TypeStr.t) Info.t,
                         vals: (Ast.Vid.t, Vid.t * Scheme.t) Info.t}

      fun layout (T {bass, fcts, sigs, strs, types, vals, ...}) =
         Layout.record
         [("bass", Info.layout (Ast.Basid.layout, layout) bass),
          ("fcts", Info.layout (Ast.Fctid.layout, FunctorClosure.layout) fcts),
          ("sigs", Info.layout (Ast.Sigid.layout, Interface.layout) sigs),
          ("strs", Info.layout (Ast.Strid.layout, Structure.layout) strs),
          ("types", Info.layout (Ast.Tycon.layout, TypeStr.layout) types),
          ("vals", (Info.layout (Ast.Vid.layout, Layout.tuple2 (Vid.layout, Scheme.layout)) vals))]
   end

(* ------------------------------------------------- *)
(*                     NameSpace                     *)
(* ------------------------------------------------- *)

structure Values =
   struct
      type ('a, 'b) value = {domain: 'a,
                             range: 'b,
                             scope: Scope.t,
                             time: Time.t,
                             uses: 'a Uses.t}
      (* The domains of all elements in a values list have the same symbol. *)
      datatype ('a, 'b) t = T of ('a, 'b) value list ref

      fun new (): ('a, 'b) t = T (ref [])

      fun ! (T r) = Ref.! r

      fun pop (T r) = List.pop r
   end

structure NameSpace =
   struct
      datatype ('a, 'b) t =
         T of {class: 'b -> Class.t,
               current: ('a, 'b) Values.t list ref,
               defUses: {class: Class.t,
                         def: 'a,
                         range: 'b option,
                         uses: 'a Uses.t} list ref option,
               lookup: 'a -> ('a, 'b) Values.t,
               region: 'a -> Region.t,
               toSymbol: 'a -> Symbol.t}

      fun values (T {lookup, ...}, a) = lookup a

      (* ------------------------------------------------- *)
      (*                       empty                       *)
      (* ------------------------------------------------- *)

      fun empty {class, defUses, lookup, region, toSymbol} =
         T {class = class,
            current = ref [],
            defUses = if defUses then SOME (ref []) else NONE,
            lookup = lookup,
            region = region,
            toSymbol = toSymbol}

      (* ------------------------------------------------- *)
      (*                      newUses                      *)
      (* ------------------------------------------------- *)

      fun newUses (T {class, defUses, ...}, {def, forceUsed, range}) =
         let
            val u = Uses.new ()
            val _ =
               if not (warnUnused ()) orelse forceUsed
                  then Uses.forceUsed u
                  else ()
            val _ =
               case defUses of
                  NONE => ()
                | SOME defUses =>
                     let
                        val class = class range
                        val range =
                           if isSome (!Control.showDefUse)
                              andalso
                              (class = Class.Var
                               orelse
                               class = Class.Exn
                               orelse
                               class = Class.Con)
                              then SOME range
                              else NONE
                     in
                        List.push (defUses, {class = class,
                                             def = def,
                                             range = range,
                                             uses = u})
                     end
         in
            u
         end

      (* ------------------------------------------------- *)
      (*                       peek                        *)
      (* ------------------------------------------------- *)

      fun ('a, 'b) peek (ns, a: 'a, {markUse: 'b -> bool})
         : 'b option =
         case Values.! (values (ns, a)) of
            [] => NONE
          | {range, uses, ...} :: _ => 
               (if markUse range then Uses.add (uses, a) else ()
                ; SOME range)

      (* ------------------------------------------------- *)
      (*                      extend                       *)
      (* ------------------------------------------------- *)

      fun extend (ns as T {current, lookup, ...},
                  {domain, forceUsed, range, scope, time, uses}) =
         let
            val newUses = fn () =>
               newUses
               (ns,
                {def = domain,
                 range = range,
                 forceUsed = forceUsed})
            val values as Values.T r = lookup domain
            fun make uses =
               {domain = domain,
                range = range,
                scope = scope,
                time = time,
                uses = uses}
            fun new () =
               let
                  val _ = List.push (current, values)
                  val uses =
                     case uses {rebind = NONE} of
                        NONE => newUses ()
                      | SOME u => u
               in
                  make uses
               end
         in
            case !r of
               [] => r := [new ()]
             | all as ({domain = domain', scope = scope', uses = uses', ...} :: rest) =>
                  if Scope.equals (scope, scope')
                     then let
                             val rebind = SOME {domain = domain', uses = uses'}
                             val uses =
                                case uses {rebind = rebind} of
                                   NONE => newUses ()
                                 | SOME u => u
                          in
                             r := (make uses) :: rest
                          end
                     else r := new () :: all
         end

      (* ------------------------------------------------- *)
      (*                       scope                       *)
      (* ------------------------------------------------- *)

      fun scope (T {current, ...}: ('a, 'b) t)
         : unit -> unit =
         let
            val old = !current
            val _ = current := []
         in
            fn () =>
            let
               val c = !current
               val _ = List.foreach (c, ignore o Values.pop)
               val _ = current := old
            in
               ()
            end
         end

      (* ------------------------------------------------- *)
      (*                       local                       *)
      (* ------------------------------------------------- *)

      fun locall (T {current, ...}: ('a, 'b) t) =
         let
            val old = !current
            val _ = current := []
         in
            fn () =>
            let
               val c1 = !current
               val _ = current := []
            in
               fn () =>
               let
                  val c2 = !current
                  val elts = List.revMap (c2, fn values =>
                                          let
                                             val {domain, range, time, uses, ...} =
                                                Values.pop values
                                          in
                                             {domain = domain,
                                              range = range,
                                              time = time,
                                              uses = uses}
                                          end)
                  val _ = List.foreach (c1, ignore o Values.pop)
                  val _ = current := old
               in
                  elts
               end
            end
         end

      (* ------------------------------------------------- *)
      (*                      collect                      *)
      (* ------------------------------------------------- *)

      fun collect (T {current, toSymbol, ...}: ('a, 'b) t)
         : unit -> ('a, 'b) Info.t =
         let
            val old = !current
            val _ = current := []
         in
            fn () =>
            let
               val elts =
                  List.revMap (!current, fn values =>
                               let
                                  val {domain, range, time, uses, ...} =
                                     Values.pop values
                               in
                                  {domain = domain,
                                   range = range,
                                   time = time,
                                   uses = uses}
                               end)
               val _ = current := old
               val a = Array.fromList elts
               val () =
                  QuickSort.sortArray
                  (a, fn ({domain = d, ...}, {domain = d', ...}) =>
                   Symbol.<= (toSymbol d, toSymbol d'))
            in
               Info.T a
            end
         end
   end

(* ------------------------------------------------- *)
(*                 Main Env Datatype                 *)
(* ------------------------------------------------- *)

structure All =
   struct
      datatype t =
         Bas of (Basid.t, Basis.t) Values.t
       | Fct of (Fctid.t, FunctorClosure.t) Values.t
       | Fix of (Ast.Vid.t, Ast.Fixity.t) Values.t
       | IfcStr of (Strid.t, Interface.t) Values.t
       | IfcTyc of (Ast.Tycon.t, Interface.TypeStr.t) Values.t
       | IfcVal of (Ast.Vid.t, Interface.Status.t * Interface.Scheme.t) Values.t
       | Sig of (Sigid.t, Interface.t) Values.t
       | Str of (Strid.t, Structure.t) Values.t
       | Tyc of (Ast.Tycon.t, TypeStr.t) Values.t
       | Val of (Ast.Vid.t, Vid.t * Scheme.t) Values.t

      val basOpt = fn Bas z => SOME z | _ => NONE
      val fctOpt = fn Fct z => SOME z | _ => NONE
      val fixOpt = fn Fix z => SOME z | _ => NONE
      val ifcStrOpt = fn IfcStr z => SOME z | _ => NONE
      val ifcTycOpt = fn IfcTyc z => SOME z | _ => NONE
      val ifcValOpt = fn IfcVal z => SOME z | _ => NONE
      val sigOpt = fn Sig z => SOME z | _ => NONE
      val strOpt = fn Str z => SOME z | _ => NONE
      val tycOpt = fn Tyc z => SOME z | _ => NONE
      val valOpt = fn Val z => SOME z | _ => NONE
   end

datatype t =
   T of {currentScope: Scope.t ref,
         bass: (Ast.Basid.t, Basis.t) NameSpace.t, 
         fcts: (Ast.Fctid.t, FunctorClosure.t) NameSpace.t,
         fixs: (Ast.Vid.t, Ast.Fixity.t) NameSpace.t,
         interface: {strs: (Ast.Strid.t, Interface.t) NameSpace.t,
                     types: (Ast.Tycon.t, Interface.TypeStr.t) NameSpace.t,
                     vals: (Ast.Vid.t, Interface.Status.t * Interface.Scheme.t) NameSpace.t},
         lookup: Symbol.t -> All.t list ref,
         sigs: (Ast.Sigid.t, Interface.t) NameSpace.t,
         strs: (Ast.Strid.t, Structure.t) NameSpace.t,
         types: (Ast.Tycon.t, TypeStr.t) NameSpace.t,
         vals: (Ast.Vid.t, Vid.t * Scheme.t) NameSpace.t}

fun sizeMessage (E: t): Layout.t =
   let
      val size = MLton.size
      open Layout
   in
      record [("total", Int.layout (size E))]
   end
(* quell unused warning *)
val _ = sizeMessage

(* ------------------------------------------------- *)
(*                       empty                       *)
(* ------------------------------------------------- *)

fun empty () =
   let
      val {get = lookupAll: Symbol.t -> All.t list ref, ...} = 
         Property.get (Symbol.plist, Property.initFun (fn _ => ref []))
      fun ('a, 'b) make (class: 'b -> Class.t,
                         region: 'a -> Region.t,
                         toSymbol: 'a -> Symbol.t,
                         defUses: bool,
                         extract: All.t -> ('a, 'b) Values.t option,
                         make: ('a, 'b) Values.t -> All.t)
         : ('a, 'b) NameSpace.t  =
         let
            fun lookup (a: 'a): ('a, 'b) Values.t =
               let
                  val r = lookupAll (toSymbol a)
               in
                  case List.peekMap (!r, extract) of
                     NONE =>
                        let
                           val v = Values.new ()
                           val _ = List.push (r, make v)
                        in
                           v
                        end
                   | SOME v => v
               end
         in
            NameSpace.empty {class = class,
                             defUses = defUses,
                             lookup = lookup,
                             region = region,
                             toSymbol = toSymbol}
         end
      val bass = make (fn _ => Class.Bas, Basid.region, Basid.toSymbol,
                       false, All.basOpt, All.Bas)
      val fcts = make (fn _ => Class.Fct, Fctid.region, Fctid.toSymbol,
                       !Control.keepDefUse, All.fctOpt, All.Fct)
      val fixs = make (fn _ => Class.Fix, Ast.Vid.region, Ast.Vid.toSymbol,
                       false, All.fixOpt, All.Fix)
      val sigs = make (fn _ => Class.Sig, Sigid.region, Sigid.toSymbol,
                       !Control.keepDefUse, All.sigOpt, All.Sig)
      val strs = make (fn _ => Class.Str, Strid.region, Strid.toSymbol,
                       !Control.keepDefUse, All.strOpt, All.Str)
      val types = make (fn _ => Class.Typ, Ast.Tycon.region, Ast.Tycon.toSymbol,
                        !Control.keepDefUse, All.tycOpt, All.Tyc)
      val vals = make (Vid.class o #1, Ast.Vid.region, Ast.Vid.toSymbol,
                       !Control.keepDefUse, All.valOpt, All.Val)

      local
         val strs = make (fn _ => Class.Str, Strid.region, Strid.toSymbol,
                          false, All.ifcStrOpt, All.IfcStr)
         val types = make (fn _ => Class.Typ, Ast.Tycon.region, Ast.Tycon.toSymbol,
                           false, All.ifcTycOpt, All.IfcTyc)
         val vals = make (Status.class o #1, Ast.Vid.region, Ast.Vid.toSymbol,
                          false, All.ifcValOpt, All.IfcVal)
      in
         val interface = {strs = strs, types = types, vals = vals}
      end
   in
      T {currentScope = ref (Scope.new ()),
         bass = bass,
         fcts = fcts,
         fixs = fixs,
         interface = interface,
         lookup = lookupAll,
         sigs = sigs,
         strs = strs,
         types = types,
         vals = vals}
   end

(* ------------------------------------------------- *)
(*                      foreach                      *)
(* ------------------------------------------------- *)

local
   fun foreach (T {lookup, ...}, s,
                {bass, fcts, fixs,
                 interface = {strs = ifcStrs, types = ifcTypes, vals = ifcVals},
                 sigs, strs, types, vals}) =
      List.foreach
      (! (lookup s), fn a =>
       let
          datatype z = datatype All.t
       in
          case a of
             Bas vs => bass vs
           | Fct vs => fcts vs
           | Fix vs => fixs vs
           | IfcStr vs => ifcStrs vs
           | IfcTyc vs => ifcTypes vs
           | IfcVal vs => ifcVals vs
           | Sig vs => sigs vs
           | Str vs => strs vs
           | Tyc vs => types vs
           | Val vs => vals vs
       end)
in
   fun foreachDefinedSymbol (E, z) =
      Symbol.foreach (fn s => foreach (E, s, z))
end

(* ------------------------------------------------- *)
(*                      current                      *)
(* ------------------------------------------------- *)

fun current (E, keep: {hasUse: bool, scope: Scope.t} -> bool) =
   let
      val bass = ref []
      val fcts = ref []
      val ifcStrs = ref []
      val ifcTypes = ref []
      val ifcVals = ref []
      val sigs = ref []
      val strs = ref []
      val types = ref []
      val vals = ref []
      fun doit ac vs =
         case Values.! vs of
            [] => ()
          | (z as {scope, uses, ...}) :: _ =>
               if keep {hasUse = Uses.hasUse uses, scope = scope}
                  then List.push (ac, z)
               else ()
      val _ =
         foreachDefinedSymbol (E, {bass = doit bass,
                                   fcts = doit fcts,
                                   fixs = fn _ => (),
                                   interface = {strs = doit ifcStrs,
                                                types = doit ifcTypes,
                                                vals = doit ifcVals},
                                   sigs = doit sigs,
                                   strs = doit strs,
                                   types = doit types,
                                   vals = doit vals})
      fun ('a, 'b) finish (r: ('a, 'b) Values.value list ref, toSymbol: 'a -> Symbol.t) () =
         let
            val a =
               Array.fromListMap
               (!r, fn {domain, range, time, uses, ...} =>
                {domain = domain, range = range,
                 time = time, uses = uses})
            val () =
               QuickSort.sortArray
               (a, fn ({domain = d, ...}, {domain = d', ...}) =>
                Symbol.<= (toSymbol d, toSymbol d'))
         in
            Info.T a
         end
   in
      {bass = finish (bass, Basid.toSymbol),
       fcts = finish (fcts, Fctid.toSymbol),
       interface = {strs = finish (ifcStrs, Strid.toSymbol),
                    types = finish (ifcTypes, Ast.Tycon.toSymbol),
                    vals = finish (ifcVals, Ast.Vid.toSymbol)},
       sigs = finish (sigs, Sigid.toSymbol),
       strs = finish (strs, Strid.toSymbol),
       types = finish (types, Ast.Tycon.toSymbol),
       vals = finish (vals, Ast.Vid.toSymbol)}
   end

(* ------------------------------------------------- *)
(*                     snapshot                      *)
(* ------------------------------------------------- *)

fun snapshot (E as T {currentScope, bass, fcts, fixs, sigs, strs, types, vals, ...})
   : (unit -> 'a) -> 'a =
   let
      val add: (Scope.t -> unit) list ref = ref []
      (* Push onto add everything currently in scope. *)
      fun doit (NameSpace.T {current, ...}) (v as Values.T vs) =
         case ! vs of
            [] => ()
          | {domain, range, uses, ...} :: _ =>
               List.push
               (add, fn s0 =>
                (List.push (vs, {domain = domain,
                                 range = range,
                                 scope = s0,
                                 time = Time.next (),
                                 uses = uses})
                 ; List.push (current, v)))
      val _ =
         foreachDefinedSymbol (E, {bass = doit bass,
                                   fcts = doit fcts,
                                   fixs = doit fixs,
                                   interface = {strs = ignore,
                                                types = ignore,
                                                vals = ignore},
                                   sigs = doit sigs,
                                   strs = doit strs,
                                   types = doit types,
                                   vals = doit vals})
   in
      fn th =>
      let
         val s0 = Scope.new ()
         val restore: (unit -> unit) list ref = ref []
         fun doit (NameSpace.T {current, ...}) =
            let
               val current0 = !current
               val _ = current := []
            in
               List.push (restore, fn () =>
                          (List.foreach (!current, fn v => ignore (Values.pop v))
                           ; current := current0))
            end
         val _ = (doit bass; doit fcts; doit fixs; doit sigs
                  ; doit strs; doit types; doit vals)
         val _ = List.foreach (!add, fn f => f s0)
         (* Clear out any symbols that weren't available in the old scope. *)
         fun doit (Values.T vs) =
            let
               val cur = !vs
            in
               case cur of
                  [] => ()
                | {scope, ...} :: _ =>
                     if Scope.equals (s0, scope)
                        then ()
                     else (vs := []
                           ; List.push (restore, fn () => vs := cur))
            end
         val _ =
            (* Can't use foreachToplevelSymbol here, because a constructor C may
             * have been defined in a local scope but may not have been defined
             * at the snapshot point.  This will make the identifier C, which
             * originally would have elaborated as a variable instead elaborate
             * as a constructor.
             *)
            foreachDefinedSymbol (E, {bass = doit,
                                      fcts = doit,
                                      fixs = doit,
                                      interface = {strs = ignore,
                                                   types = ignore,
                                                   vals = ignore},
                                      sigs = doit,
                                      strs = doit,
                                      types = doit,
                                      vals = doit})
         val s1 = !currentScope
         val _ = currentScope := s0
         val res = th ()
         val _ = currentScope := s1
         val _ = List.foreach (!restore, fn f => f ())
      in
         res
      end
   end

(* ------------------------------------------------- *)
(*                       peek                        *)
(* ------------------------------------------------- *)

local
   fun make sel (T r, a) = NameSpace.peek (sel r, a, {markUse = fn _ => true})
in
   val peekBasid = make #bass
   val peekFctid = make #fcts
   val peekFix = make #fixs
   val peekIfcStrid = make (#strs o #interface)
   val peekIfcTycon= make (#types o #interface)
   val peekSigid = make #sigs
   val peekStrid = make #strs
   val peekTycon = make #types
   val peekVid = make #vals
   fun peekVar (E, x) =
      case peekVid (E, Ast.Vid.fromVar x) of
         NONE => NONE
       | SOME (vid, s) => Option.map (Vid.deVar vid, fn x => (x, s))
end

fun peekCon (T {vals, ...}, c: Ast.Con.t): (Con.t * Scheme.t) option =
   case NameSpace.peek (vals, Ast.Vid.fromCon c,
                        {markUse = fn (vid, _) => isSome (Vid.deCon vid)}) of
      NONE => NONE
    | SOME (vid, s) => Option.map (Vid.deCon vid, fn c => (c, s))

fun peekExn (T {vals, ...}, c: Ast.Con.t): (Con.t * Scheme.t) option =
   case NameSpace.peek (vals, Ast.Vid.fromCon c,
                        {markUse = fn (vid, _) => isSome (Vid.deExn vid)}) of
      NONE => NONE
    | SOME (vid, s) => Option.map (Vid.deExn vid, fn c => (c, s))

structure PeekResult =
   struct
      datatype 'a t =
         Found of 'a
       | UndefinedStructure of Strid.t list
       | Undefined

      val toOption: 'a t -> 'a option =
         fn Found z => SOME z
          | _ => NONE
   end

local
   fun make (split: 'a -> Strid.t list * 'b,
             peek: t * 'b -> 'c option,
             strPeek: Structure.t * 'b -> 'c option) (E, x) =
      let
         val (strids, x) = split x
      in
         case strids of
            [] => (case peek (E, x) of
                      NONE => PeekResult.Undefined
                    | SOME z => PeekResult.Found z)
          | strid :: strids =>
               case peekStrid (E, strid) of
                  NONE => PeekResult.UndefinedStructure [strid]
                | SOME S =>
                     case Structure.peekStrids (S, strids) of
                        Structure.PeekResult.Found S =>
                           (case strPeek (S, x) of
                               NONE => PeekResult.Undefined
                             | SOME z => PeekResult.Found z)
                      | Structure.PeekResult.UndefinedStructure ss =>
                           PeekResult.UndefinedStructure (strid :: ss)
      end
in
   val peekLongstrid =
      make (Ast.Longstrid.split, peekStrid, Structure.peekStrid)
   val peekLongtycon =
      make (Longtycon.split, peekTycon, Structure.peekTycon)
   val peekLongvar = make (Ast.Longvar.split, peekVar, Structure.peekVar)
   val peekLongvid = make (Ast.Longvid.split, peekVid, Structure.peekVid)
   val peekLongcon = make (Ast.Longcon.split, peekCon, Structure.peekCon)
   val peekLongexn = make (Ast.Longcon.split, peekExn, Structure.peekExn)
end

(* ------------------------------------------------- *)
(*                      lookup                       *)
(* ------------------------------------------------- *)

fun unbound (r: Region.t, className, x: Layout.t): unit =
   Control.error
   (r,
    seq [str "undefined ", str className, str ": ", x],
    Layout.empty)

fun lookupBasid (E, x) =
   case peekBasid (E, x) of
      NONE => (unbound (Ast.Basid.region x, "basis", Ast.Basid.layout x)
               ; NONE)
    | SOME f => SOME f

fun lookupFctid (E, x) =
   case peekFctid (E, x) of
      NONE => (unbound (Ast.Fctid.region x, "functor", Ast.Fctid.layout x)
               ; NONE)
    | SOME f => SOME f

fun lookupSigid (E, x) =
   case peekSigid (E, x) of
      NONE => (unbound (Ast.Sigid.region x, "signature", Ast.Sigid.layout x)
               ; NONE)
    | SOME I => SOME I

fun lookupStrid (E, x) =
   case peekStrid (E, x) of
      NONE => (unbound (Ast.Strid.region x, "structure", Ast.Strid.layout x)
               ; NONE)
    | SOME S => SOME S

local
   fun make (peek: t * 'a -> 'b PeekResult.t,
             className: string,
             region: 'a -> Region.t,
             layout: 'a -> Layout.t)
      (E: t, x: 'a): 'b option =
      let
         datatype z = datatype PeekResult.t
      in
         case peek (E, x) of
            Found z => SOME z
          | UndefinedStructure ss =>
               (unbound (region x, "structure", layoutStrids ss); NONE)
          | Undefined =>
               (unbound (region x, className, layout x); NONE)
      end
in
   val lookupLongcon =
      make (peekLongcon,
            "constructor",
            Ast.Longcon.region,
            Ast.Longcon.layout)
   val lookupLongexn =
      make (peekLongexn,
            "exception",
            Ast.Longcon.region,
            Ast.Longcon.layout)
   val lookupLongstrid =
      make (peekLongstrid,
            "structure",
            Ast.Longstrid.region,
            Ast.Longstrid.layout)
   val lookupLongtycon =
      make (peekLongtycon,
            "type",
            Ast.Longtycon.region,
            Ast.Longtycon.layout)
   val lookupLongvid =
      make (peekLongvid,
            "variable",
            Ast.Longvid.region,
            Ast.Longvid.layout)
   val lookupLongvar =
      make (peekLongvar,
            "variable",
            Ast.Longvar.region,
            Ast.Longvar.layout)
end

val peekLongcon = PeekResult.toOption o peekLongcon

(* ------------------------------------------------- *)
(*                      extend                       *)
(* ------------------------------------------------- *)

local
   fun extend (T (r as {currentScope, ...}), sel,
               domain: 'a, range: 'b, forceUsed: bool, uses) =
      NameSpace.extend
      (sel r,
       {domain = domain,
        forceUsed = forceUsed,
        range = range,
        scope = !currentScope,
        time = Time.next (),
        uses = uses})
in
   fun extendBasid (E, d, r) = extend (E, #bass, d, r, false, Uses.Extend.new)
   fun extendFctid (E, d, r) = extend (E, #fcts, d, r, false, Uses.Extend.new)
   fun extendFix (E, d, r) = extend (E, #fixs, d, r, false, Uses.Extend.new)
   fun extendSigid (E, d, r) = extend (E, #sigs, d, r, false, Uses.Extend.new)
   fun extendStrid (E, d, r) = extend (E, #strs, d, r, false, Uses.Extend.new)
   fun extendVals (E, d, r, eu) = extend (E, #vals, d, r, false, eu)
   fun extendTycon (E, d, s, {forceUsed, isRebind}) =
      let
         val () =
            let
               datatype z = datatype TypeStr.node
            in
               case TypeStr.node s of
                  Datatype {cons, ...} =>
                     Vector.foreach
                     (Cons.dest cons, fn {con, name, scheme, uses} =>
                      extendVals (E, Ast.Vid.fromCon name,
                                  (Vid.Con con, scheme),
                                  Uses.Extend.old uses))
                | _ => ()
            end
         val _ =
            extend (E, #types, d, s, forceUsed,
                    Uses.Extend.fromIsRebind {isRebind = isRebind})
      in
         ()
      end
end

fun extendExn (E, c, c', s) =
   extendVals (E, Ast.Vid.fromCon c, (Vid.Exn c', s), Uses.Extend.new)

fun extendVar (E, x, x', s, ir) =
   extendVals (E, Ast.Vid.fromVar x, (Vid.Var x', s),
               Uses.Extend.fromIsRebind ir)

val extendVar =
   Trace.trace
   ("ElaborateEnv.extendVar",
    fn (_, x, x', s, _) =>
    Layout.tuple [Ast.Var.layout x, Var.layout x', Scheme.layout s],
    Unit.layout)
   extendVar

fun extendOverload (E, p, x, yts, s) =
   extendVals (E, Ast.Vid.fromVar x, (Vid.Overload (p, yts), s),
               Uses.Extend.new)

(* ------------------------------------------------- *)
(*                       scope                       *)
(* ------------------------------------------------- *)

fun scopeAll (T {currentScope, bass, fcts, fixs, sigs, strs, types, vals, ...}, th) =
   let
      val b = NameSpace.scope bass
      val fc = NameSpace.scope fcts
      val f = NameSpace.scope fixs
      val si = NameSpace.scope sigs
      val s = NameSpace.scope strs
      val t = NameSpace.scope types
      val v = NameSpace.scope vals
      val s0 = !currentScope
      val _ = currentScope := Scope.new ()
      val res = th ()
      val _ = (b (); fc (); f (); si (); s (); t (); v ())
      val _ = currentScope := s0
   in
      res
   end

fun scope (T {currentScope, fixs, strs, types, vals, ...}, th) =
   let
      val f = NameSpace.scope fixs
      val s = NameSpace.scope strs
      val t = NameSpace.scope types
      val v = NameSpace.scope vals
      val s0 = !currentScope
      val _ = currentScope := Scope.new ()
      val res = th ()
      val _ = (f (); s (); t (); v ())
      val _ = currentScope := s0
   in
      res
   end

(* ------------------------------------------------- *)
(*                       local                       *)
(* ------------------------------------------------- *)

local
   fun locall (ns, s0) =
      let
         val f = NameSpace.locall ns
      in
         fn () =>
         let
            val f = f ()
         in
            fn () =>
            let
               val elts = f ()
               val _ =
                  List.foreach (elts, fn {domain, range, time, uses} =>
                                NameSpace.extend
                                (ns, {domain = domain,
                                      forceUsed = false,
                                      range = range,
                                      scope = s0,
                                      time = time,
                                      uses = Uses.Extend.old uses}))
            in
               ()
            end
         end
      end
in
   fun localAll (T {currentScope, bass, fcts, fixs, sigs, strs, types, vals, ...},
                 f1, f2) =
      let
         val s0 = !currentScope
         val bass = locall (bass, s0)
         val fcts = locall (fcts, s0)
         val fixs = locall (fixs, s0)
         val sigs = locall (sigs, s0)
         val strs = locall (strs, s0)
         val types = locall (types, s0)
         val vals = locall (vals, s0)
         val _ = currentScope := Scope.new ()
         val a1 = f1 ()
         val bass = bass ()
         val fcts = fcts ()
         val fixs = fixs ()
         val sigs = sigs ()
         val strs = strs ()
         val types = types ()
         val vals = vals ()
         val _ = currentScope := Scope.new ()
         val a2 = f2 a1
         val _ = (bass (); fcts (); fixs (); sigs (); strs (); types (); vals ())
         val _ = currentScope := s0
      in
         a2
      end

   fun localModule (T {currentScope, fixs, strs, types, vals, ...},
                    f1, f2) =
      let
         val s0 = !currentScope
         val fixs = locall (fixs, s0)
         val strs = locall (strs, s0)
         val types = locall (types, s0)
         val vals = locall (vals, s0)
         val _ = currentScope := Scope.new ()
         val a1 = f1 ()
         val fixs = fixs ()
         val strs = strs ()
         val types = types ()
         val vals = vals ()
         val _ = currentScope := Scope.new ()
         val a2 = f2 a1
         val _ = (fixs (); strs (); types (); vals ())
         val _ = currentScope := s0
      in
         a2
      end

   (* Can't eliminate the use of strs in localCore, because openn still modifies
    * module level constructs.
    *)
   val localCore = localModule
end

(* ------------------------------------------------- *)
(*             makeBasis / makeStructure             *)
(* ------------------------------------------------- *)

fun makeBasis (T {currentScope, bass, fcts, fixs, sigs, strs, types, vals, ...}, make) =
   let
      val bass = NameSpace.collect bass
      val fcts = NameSpace.collect fcts
      val fixs = NameSpace.collect fixs
      val sigs = NameSpace.collect sigs
      val strs = NameSpace.collect strs
      val types = NameSpace.collect types
      val vals = NameSpace.collect vals
      val s0 = !currentScope
      val _ = currentScope := Scope.new ()
      val res = make ()
      val B = Basis.T {plist = PropertyList.new (),
                       bass = bass (),
                       fcts = fcts (),
                       fixs = fixs (),
                       sigs = sigs (),
                       strs = strs (),
                       types = types (),
                       vals = vals ()}
      val _ = currentScope := s0
   in
      (res, B)
   end

fun makeStructure (T {currentScope, fixs, strs, types, vals, ...}, make) =
   let
      val f = NameSpace.collect fixs
      val s = NameSpace.collect strs
      val t = NameSpace.collect types
      val v = NameSpace.collect vals
      val s0 = !currentScope
      val _ = currentScope := Scope.new ()
      val res = make ()
      val _ = f ()
      val S = Structure.T {interface = NONE,
                           plist = PropertyList.new (),
                           strs = s (),
                           types = t (),
                           vals = v ()}
      val _ = currentScope := s0
   in
      (res, S)
   end

(* ------------------------------------------------- *)
(*                       open                        *)
(* ------------------------------------------------- *)

local
   fun openn (ns, Info.T a, s) =
      Array.foreach (a, fn {domain, range, time, uses} =>
                     NameSpace.extend (ns, {domain = domain,
                                            forceUsed = false,
                                            range = range,
                                            scope = s,
                                            time = time,
                                            uses = Uses.Extend.old uses}))
in
   fun openBasis (T {currentScope, bass, fcts, fixs, sigs, strs, vals, types, ...},
                  Basis.T {bass = bass',
                           fcts = fcts',
                           fixs = fixs',
                           sigs = sigs',
                           strs = strs',
                           vals = vals',
                           types = types', ...}): unit =
      let
         val s0 = !currentScope
         val _ = openn (bass, bass', s0)
         val _ = openn (fcts, fcts', s0)
         val _ = openn (fixs, fixs', s0)
         val _ = openn (sigs, sigs', s0)
         val _ = openn (strs, strs', s0)
         val _ = openn (vals, vals', s0)
         val _ = openn (types, types', s0)
      in
         ()
      end

   fun openStructure (T {currentScope, strs, vals, types, ...},
                      Structure.T {strs = strs',
                                   vals = vals',
                                   types = types', ...}): unit =
      let
         val s0 = !currentScope
         val _ = openn (strs, strs', s0)
         val _ = openn (vals, vals', s0)
         val _ = openn (types, types', s0)
      in
         ()
      end
end

(* ------------------------------------------------- *)
(*                     forceUsed                     *)
(* ------------------------------------------------- *)

(* Force everything that is currently in scope to be marked as used. *)
fun forceUsed E =
   let
      fun doit forceRange (Values.T r) =
         case !r of
            [] => ()
          | {uses, range, ...} :: _ =>
               (Uses.forceUsed uses
                ; forceRange range)
      val _ =
         foreachDefinedSymbol
         (E, {bass = doit ignore,
              fcts = doit FunctorClosure.forceUsed,
              fixs = doit ignore,
              interface = {strs = doit ignore,
                           types = doit ignore,
                           vals = doit ignore},
              sigs = doit ignore,
              strs = doit Structure.forceUsed,
              types = doit ignore,
              vals = doit ignore})
   in
      ()
   end

fun forceUsedLocal (T {currentScope, bass, fcts, fixs, sigs, strs, types, vals, ...},
                    th) =
   let
      fun doit (forceRange: 'b -> unit, ns as NameSpace.T {current, ...}, s0) =
         let
            val old = !current
            val _ = current := []
         in
            fn () =>
            let
               val c = !current
               val lift = List.revMap (c, Values.pop)
               val _ = current := old
               val _ =
                  List.foreach
                  (lift, fn {domain, range, time, uses, ...} =>
                   (Uses.forceUsed uses
                    ; forceRange range
                    ; NameSpace.extend (ns, {domain = domain,
                                             forceUsed = false,
                                             range = range,
                                             scope = s0,
                                             time = time,
                                             uses = Uses.Extend.old uses})))
            in
               ()
            end
         end
      val s0 = !currentScope
      val bass = doit (ignore, bass, s0)
      val fcts = doit (FunctorClosure.forceUsed, fcts, s0)
      val fixs = doit (ignore, fixs, s0)
      val sigs = doit (ignore, sigs, s0)
      val strs = doit (Structure.forceUsed, strs, s0)
      val types = doit (ignore, types, s0)
      val vals = doit (ignore, vals, s0)
      val _ = currentScope := Scope.new ()
      val res = th ()
      val _ = (bass(); fcts (); fixs (); sigs (); strs (); types (); vals ())
      val _ = currentScope := s0
   in
      res
   end

(* ------------------------------------------------- *)
(*                   InterfaceEnv                    *)
(* ------------------------------------------------- *)

structure InterfaceEnv =
   struct
      structure Env =
         struct
            val lookupLongtycon = lookupLongtycon
            val peekIfcStrid = peekIfcStrid
            val peekIfcTycon = peekIfcTycon
            val lookupSigid = lookupSigid
         end

      local
         open Interface
      in
         structure FlexibleTycon = FlexibleTycon
         structure Scheme = Scheme
         structure Status = Status
         structure TypeStr = TypeStr
      end

      type t = t

      (* ------------------------------------------------- *)
      (*                       peek                        *)
      (* ------------------------------------------------- *)

      val peekStrid = Env.peekIfcStrid
      val peekTycon = Env.peekIfcTycon

      (* ------------------------------------------------- *)
      (*                      lookup                       *)
      (* ------------------------------------------------- *)

      val lookupSigid = Env.lookupSigid

      fun lookupLongtycon (E: t, long: Longtycon.t): TypeStr.t option =
         let
            fun lookupEnv () =
               Option.map (Env.lookupLongtycon (E, long), TypeStr.fromEnv)
            val (strids, c) = Longtycon.split long
         in
            case strids of
               [] =>
                  (case peekTycon (E, c) of
                      NONE => lookupEnv ()
                    | SOME s => SOME s)
             | s :: ss =>
                  case peekStrid (E, s) of
                     NONE => lookupEnv ()
                   | SOME I =>
                        ((fn opt => Option.map (opt, #2)) o Interface.lookupLongtycon)
                        (I, Longtycon.long (ss, c), Longtycon.region long,
                         {prefix = [s]})
         end

      (* ------------------------------------------------- *)
      (*                      extend                       *)
      (* ------------------------------------------------- *)

      datatype z = MustExtend of Region.t | MustRebind

      fun extend (T {currentScope, interface, ...}, sel,
                  domain, range, kind, must) =
         NameSpace.extend
         (sel interface,
          {domain = domain,
           forceUsed = true,
           range = range,
           scope = !currentScope,
           time = Time.next (),
           uses = (case must of
                      MustExtend extendRegion =>
                         (fn {rebind} =>
                          let
                             val NameSpace.T {region, toSymbol, ...} = sel interface
                             val () =
                                case rebind of
                                   SOME {domain = domain', ...} =>
                                      let
                                         open Layout
                                      in
                                         Control.error
                                         (extendRegion,
                                          seq [str "duplicate ",
                                               str kind,
                                               str " specification: ",
                                               Symbol.layout (toSymbol domain)],
                                          (align o List.map)
                                          (if Region.equals (extendRegion,
                                                             region domain)
                                              then [domain']
                                              else [domain', domain],
                                           fn d => seq [str "spec at: ",
                                                        Region.layout (region d)]))
                                      end
                                 | _ => ()
                          in
                             NONE
                          end)
                    | MustRebind =>
                         (fn {rebind} =>
                          case rebind of
                             NONE =>
                                Error.bug "ElaborateEnv.InterfaceEnv.extend: MustRebind"
                           | SOME {uses, ...} =>
                                SOME uses))})

      fun extendStrid (E, s, I, r) =
         extend (E, #strs, s, I, "structure", MustExtend r)

      fun extendTycon (E, c, s, r) =
         extend (E, #types, c, s, "type", MustExtend r)

      fun extendVid (E, v, st, s, r) =
         extend (E, #vals, v, (st, s), "value", MustExtend r)

      fun rebindTycon (E, c, s) =
         extend (E, #types, c, s, "type", MustRebind)

      (* ------------------------------------------------- *)
      (*                   makeInterface                   *)
      (* ------------------------------------------------- *)

      fun makeInterface (T {currentScope, interface = {strs, types, vals}, ...},
                         {isTop}, make) =
         let
            val s = NameSpace.collect strs
            val t = NameSpace.collect types
            val v = NameSpace.collect vals
            val s0 = !currentScope
            val _ = currentScope := Scope.new ()
            val res = make ()
            val Info.T s = s ()
            val s = Array.map (s, fn {domain, range, ...} => (domain, range))
            val Info.T t = t ()
            val t = Array.map (t, fn {domain, range, ...} => (domain, range))
            val Info.T v = v ()
            val v = Array.map (v, fn {domain, range = (status, scheme), ...} =>
                               (domain, (status, scheme)))
            val I = Interface.new {isClosed = isTop,
                                   original = NONE,
                                   strs = s, types = t, vals = v}
            val _ = currentScope := s0
         in
            (I, res)
         end

      (* ------------------------------------------------- *)
      (*                   openInterface                   *)
      (* ------------------------------------------------- *)

      fun openInterface (E, I, r: Region.t) =
         let
            val {strs, vals, types} = Interface.dest I
            val _ = Array.foreach (strs, fn (s, I) => extendStrid (E, s, I, r))
            val _ = Array.foreach (types, fn (c, s) => extendTycon (E, c, s, r))
            val _ = Array.foreach (vals, fn (x, (s, sc)) =>
                                   extendVid (E, x, s, sc, r))
         in
            ()
         end

      (* ------------------------------------------------- *)
      (*                      extend                       *)
      (* ------------------------------------------------- *)

      val extendStrid = fn (E, s, I) => extendStrid (E, s, I, Strid.region s)

      val extendTycon = fn (E, c, s) => extendTycon (E, c, s, Ast.Tycon.region c)

      val extendVid = fn (E, v, st, s) => extendVid (E, v, st, s, Ast.Vid.region v)

      fun extendCon (E, c, s) =
         extendVid (E, Ast.Vid.fromCon c, Status.Con, s)

      fun extendExn (E, c, s) =
         extendVid (E, Ast.Vid.fromCon c, Status.Exn, s)

      (* ------------------------------------------------- *)
      (*             makeLayoutPrettyFlexTycon             *)
      (* ------------------------------------------------- *)

      fun genLayoutPrettyFlexTycon {prefixUnset} =
         let
            val {destroy = destroyLayoutPrettyFlexTycon: unit -> unit,
                 get = layoutPrettyFlexTycon: FlexibleTycon.t -> Layout.t,
                 set = setLayoutPrettyFlexTycon: FlexibleTycon.t * Layout.t -> unit} =
               Property.destGetSet
               (FlexibleTycon.plist,
                Property.initFun
                (fn f =>
                 let val l = FlexibleTycon.layoutPrettyDefault f
                 in if prefixUnset then seq [str "??.", l] else l
                 end))
            fun doFlexTycon (flex, name, strids: Strid.t list) =
               let
                  val name = layoutLongRev (strids, Ast.Tycon.layout name)
               in
                  setLayoutPrettyFlexTycon (flex, name)
               end
            fun loopFlexTyconMap (TyconMap.T {strs, types}, strids) =
               let
                  val () =
                     Array.foreach
                     (types, fn (name, flex) =>
                      doFlexTycon (flex, name, strids))
                  val () =
                     Array.foreach
                     (strs, fn (name, flexTyconMap) =>
                      loopFlexTyconMap (flexTyconMap, name::strids))
               in
                  ()
               end
         in
            {destroy = destroyLayoutPrettyFlexTycon,
             layoutPrettyFlexTycon = layoutPrettyFlexTycon,
             loopFlexTyconMap = loopFlexTyconMap}
         end

end

val makeInterfaceEnv = fn E => E

(* ------------------------------------------------- *)
(*               makeLayoutPrettyTycon               *)
(* ------------------------------------------------- *)

fun genLayoutPrettyTycon {prefixUnset} =
   let
      val {destroy = destroyLayoutPrettyTycon: unit -> unit,
           get = layoutPrettyTycon: Tycon.t -> Layout.t,
           set = setLayoutPrettyTycon: Tycon.t * Layout.t -> unit} =
         Property.destGetSet
         (Tycon.plist,
          Property.initFun
          (fn c =>
           let val l = Tycon.layoutPrettyDefault c
           in if prefixUnset then seq [str "?.", l] else l
           end))
      val {destroy = destroyTyconShortest,
           get = tyconShortest: Tycon.t -> (int * int) option ref, ...} =
         Property.destGet (Tycon.plist, Property.initFun (fn _ => ref NONE))
      fun doType (typeStr: TypeStr.t,
                  name: Ast.Tycon.t,
                  priority: int,
                  length: int,
                  strids: Strid.t list): unit =
         case TypeStr.toTyconOpt typeStr of
            NONE => ()
          | SOME c =>
               let
                  val r = tyconShortest c
                  fun doit () =
                     let
                        val _ = r := SOME (priority, length)
                        val name = layoutLongRev (strids, Ast.Tycon.layout name)
                     in
                        setLayoutPrettyTycon (c, name)
                     end
               in
                  case !r of
                     NONE => doit ()
                   | SOME (priority', length') =>
                        (case Int.compare (priority, priority') of
                            LESS => doit ()
                          | EQUAL => if length >= length'
                                        then ()
                                        else doit ()
                          | GREATER => ())
               end
      val {destroy = destroyStrShortest,
           get = strShortest: Structure.t -> (int * int) option ref, ...} =
         Property.destGet (Structure.plist, Property.initFun (fn _ => ref NONE))
      fun loopStr (s as Structure.T {strs, types, ...},
                   priority: int,
                   length: int,
                   strids: Strid.t list): unit =
         let
            val r = strShortest s
            fun doit () =
               let
                  val _ = r := SOME (priority, length)
                  (* Process the declarations in decreasing order of
                   * definition time so that later declarations will be
                   * processed first, and hence will take precedence.
                   *)
                  val _ =
                     Info.foreachByTime
                     (types, fn (name, typeStr) =>
                      doType (typeStr, name, priority, length, strids))
                  val _ =
                     Info.foreachByTime
                     (strs, fn (strid, str) =>
                      loopStr (str, priority, 1 + length, strid::strids))
               in
                  ()
               end
         in
            case !r of
               NONE => doit ()
             | SOME (priority', length') =>
                  (case Int.compare (priority, priority') of
                      LESS => doit ()
                    | EQUAL => if length >= length'
                                  then ()
                                  else doit ()
                    | GREATER => ())
         end
      fun loopFlexTyconMap (tm: FlexibleTycon.t TyconMap.t, priority, length: int, strids: Strid.t list): unit =
         let
            val TyconMap.T {strs, types} = tm
            val _ =
               Array.foreach
               (types, fn (name, flex) =>
                doType (FlexibleTycon.toEnv flex, name, priority, length, strids))
            val _ =
               Array.foreach
               (strs, fn (strid, tm) =>
                loopFlexTyconMap (tm, priority, 1 + length, strid::strids))
         in
            ()
         end
      fun mk loop (z, priority, strids) =
         loop (z, priority, length strids, strids)
   in
      {destroy = fn () => (destroyStrShortest ()
                           ; destroyTyconShortest ()
                           ; destroyLayoutPrettyTycon ()),
       layoutPrettyTycon = layoutPrettyTycon,
       setLayoutPrettyTycon = setLayoutPrettyTycon,
       loopStr = mk loopStr,
       loopFlexTyconMap = mk loopFlexTyconMap}
   end

fun makeLayoutPrettyTycon (E, {prefixUnset}) =
   let
      val {destroy = destroyLayoutPrettyTycon,
           layoutPrettyTycon, setLayoutPrettyTycon,
           loopStr, ...} =
         genLayoutPrettyTycon {prefixUnset = prefixUnset}
      fun pre () =
         let
            val {strs, types, ...} = current (E, fn _ => true)
         in
            loopStr (Structure.T {interface = NONE,
                                  plist = PropertyList.new (),
                                  strs = strs (),
                                  types = types (),
                                  vals = Info.T (Array.new0 ())},
                     0, [])
         end
      val pre = ClearablePromise.delay pre
   in
      {destroy = fn () => (ClearablePromise.clear pre
                           ; destroyLayoutPrettyTycon ()),
       layoutPrettyTycon = fn c => (ClearablePromise.force pre
                                    ; layoutPrettyTycon c),
       setLayoutPrettyTycon = setLayoutPrettyTycon,
       loopStr = loopStr}
   end

fun makeLayoutPrettyTyconAndFlexTycon (E, _, Io, {prefixUnset}) =
   let
      val {destroy = destroyLayoutPrettyFlexTycon,
           layoutPrettyFlexTycon, loopFlexTyconMap, ...} =
         InterfaceEnv.genLayoutPrettyFlexTycon {prefixUnset = prefixUnset}
      val {destroy = destroyLayoutPrettyTycon,
           layoutPrettyTycon, setLayoutPrettyTycon,
           loopStr, ...} =
         genLayoutPrettyTycon {prefixUnset = prefixUnset}
      fun pre () =
         let
            val {strs, types, interface = {strs = ifcStrs, types = ifcTypes, ...}, ...} =
                 current (E, fn _ => true)
            val strs = strs ()
            val types = types ()
            val ifcStrs = ifcStrs ()
            val ifcTypes = ifcTypes ()
            local
               fun doit (env, ifc, toSymbol) =
                  if Info.isEmpty ifc
                     then env
                     else Info.keepAll
                          (env, fn {domain, ...} =>
                           case Info.peek (ifc, domain, toSymbol) of
                              NONE => true
                            | SOME _ => false)
            in
               val () = loopStr (Structure.T {interface = NONE,
                                              plist = PropertyList.new (),
                                              strs = doit (strs, ifcStrs, Ast.Strid.toSymbol),
                                              types = doit (types, ifcTypes, Ast.Tycon.toSymbol),
                                              vals = Info.T (Array.new0 ())},
                                 0, [])
            end
            local
               fun doit ifc =
                  let val Info.T a = ifc
                  in Array.map (a, fn {domain, range, ...} => (domain, range))
                  end
               val I = Interface.new {isClosed = true,
                                      original = NONE,
                                      strs = doit ifcStrs,
                                      types = doit ifcTypes,
                                      vals = Array.new0 ()}
            in
               val () = loopFlexTyconMap (Interface.flexibleTycons I, [])
            end
            val () = Option.foreach
                     (Io, fn I =>
                      loopFlexTyconMap (Interface.flexibleTycons I,
                                        [Ast.Strid.uSig]))
         in
            ()
         end
      val pre = ClearablePromise.delay pre
   in
      {destroy = fn () => (ClearablePromise.clear pre
                           ; destroyLayoutPrettyFlexTycon ()
                           ; destroyLayoutPrettyTycon ()),
       layoutPrettyTycon = fn c => (ClearablePromise.force pre
                                    ; layoutPrettyTycon c),
       layoutPrettyFlexTycon = fn f => (ClearablePromise.force pre
                                        ; layoutPrettyFlexTycon f),
       setLayoutPrettyTycon = setLayoutPrettyTycon}
   end

fun output (E: t, out, {compact, def, flat, onlyCurrent, prefixUnset}): unit =
   let
      val keep =
         if onlyCurrent
            then let
                    val T {currentScope, ...} = E
                    val currentScope = !currentScope
                 in
                    fn {scope, ...} =>
                    Scope.equals (scope, currentScope)
                 end
            else fn _ => true
      val {bass, fcts, sigs, strs, types, vals, ...} = current (E, keep)
      val bass = bass ()
      val fcts = fcts ()
      val sigs = sigs ()
      val strs = strs ()
      val types = types ()
      val vals = vals ()

      val {get = interfaceSigid: Interface.t -> (Sigid.t * Interface.t) option,
           set = setInterfaceSigid, ...} =
         Property.getSet (Interface.plist, Property.initConst NONE)
      val _ = Array.foreach (let val Info.T sigs = sigs in sigs end,
                             fn {domain = s, range = I, ...} =>
                             setInterfaceSigid (I, SOME (s, I)))
      val {destroy = destroyLayoutPrettyTycon,
           layoutPrettyTycon, setLayoutPrettyTycon,
           loopStr, ...} =
         makeLayoutPrettyTycon (E, {prefixUnset = prefixUnset})

      val empty = Layout.empty
      val indent = fn l => Layout.indent (l, 3)
      val paren = Layout.paren

      val {destroy, layoutSigDefn, layoutSigFlex,
           layoutStr, layoutStrDefn,
           layoutTypeDefn, layoutValDefn, ...} =
         Structure.layouts {interfaceSigid = interfaceSigid,
                            layoutPrettyTycon = layoutPrettyTycon,
                            setLayoutPrettyTycon = setLayoutPrettyTycon}
      val destroy = fn () =>
         (destroy (); destroyLayoutPrettyTycon ())

      fun layoutFctDefn (name, FunctorClosure.T {argInterface, summary, ...},
                         {compact, def}) =
         let
            val bind =
               seq [str "functor ", Fctid.layout name]
            val argId = Strid.uArg (Fctid.toString name)
            val {abbrev = argAbbrev, full = argFull} =
               let
                  val bind =
                     seq [Strid.layout argId, str ":"]
                  val {abbrev, full} =
                     layoutSigFlex (argInterface,
                                    {compact = compact,
                                     elide = {strs = NONE, types = NONE, vals = NONE}})
                  val abbrev =
                     case abbrev () of
                        NONE => NONE
                      | SOME sigg => SOME (seq [bind, str " ", sigg])
                  val full = fn () =>
                     align [bind, indent (full ())]
               in
                  {abbrev = abbrev, full = full}
               end
            val arg = #1 (Structure.dummy (argInterface, {prefix = Strid.toString argId ^ "."}))
            val () = loopStr (arg, 1, [argId])
            val {abbrev = resAbbrev, full = resFull} =
               case summary arg of
                  NONE => {abbrev = SOME (str "???"), full = fn () => str "???"}
                | SOME res => let
                                 val resId = Strid.uRes (Fctid.toString name)
                                 val () = loopStr (res, 2, [resId])
                                 val {abbrev, full} = layoutStr (res, {compact = compact})
                                 val abbrev =
                                    case abbrev () of
                                       NONE => NONE
                                     | SOME sigg =>
                                          SOME (if compact
                                                   then Layout.compact sigg
                                                   else sigg)
                              in
                                 {abbrev = abbrev, full = full}
                              end
            val def =
               if def
                  then seq [str "(* @ ",
                            Region.layout (Fctid.region name),
                            str " *)"]
                  else empty
            val full = fn (arg, res) =>
               align
               [bind,
                indent (seq [paren arg, str ":"]),
                indent res,
                indent def]
         in
            case (argAbbrev, resAbbrev) of
               (NONE, NONE) => full (argFull (), resFull ())
             | (NONE, SOME resAbbrev) => full (argFull (), resAbbrev)
             | (SOME argAbbrev, NONE) => full (argAbbrev, resFull ())
             | (SOME argAbbrev, SOME resAbbrev) =>
                  let
                     val lay =
                        mayAlign
                        [seq [bind, str " ",
                              paren argAbbrev, str ": ",
                              resAbbrev],
                         indent def]
                     val lay =
                        if compact
                           then Layout.compact lay
                           else lay
                  in
                     lay
                  end
         end
      fun layoutBasDefn (name, _, {compact, def}) =
         let
            val lay =
               mayAlign
               [seq [str "basis ", Basid.layout name],
                indent (if def
                           then seq [str "(* @ ",
                                     Region.layout (Basid.region name),
                                     str " *)"]
                           else empty)]
            val lay =
               if compact
                  then Layout.compact lay
                  else lay
         in
            lay
         end

      val outputl = fn l => Layout.outputl (l, out)
      val maybeOutputl = fn lo =>
         case lo of
            NONE => ()
          | SOME l => outputl l
      val outputTypeDefn =
         fn (strids, name, tyStr) =>
         (outputl o layoutTypeDefn)
         (strids, name, tyStr,
          {compact = compact, def = def})
      val outputValDefn =
         fn (strids, name, (vid, scheme)) =>
         (maybeOutputl o layoutValDefn)
         (strids, name, (vid, scheme),
          {compact = compact, con = flat, def = def})
      val outputSigDefn =
         fn (name, I) =>
         (outputl o layoutSigDefn)
         (name, I,
          {compact = compact, def = def})
      val outputStrDefn =
         fn (strids, name, S) =>
         (outputl o layoutStrDefn)
         (strids, name, S,
          {compact = compact, def = def})
      fun outputStrDefnFlat (strids, name, S) =
         let
            val () = outputStrDefn (strids, name, S)
            val strids = name::strids
            val Structure.T {strs, types, vals, ...} = S
            fun doit (Info.T a, output) =
               Array.foreach
               (a, fn {domain, range, ...} =>
                output (strids, domain, range))
            val () = doit (types, outputTypeDefn)
            val () = doit (vals, outputValDefn)
            val () = doit (strs, outputStrDefnFlat)
         in
            ()
         end
      val outputFctDefn =
         fn (name, fctCls) =>
         (outputl o layoutFctDefn)
         (name, fctCls,
          {compact = compact, def = def})
      val outputBasDefn =
         fn (name, B) =>
         (outputl o layoutBasDefn)
         (name, B,
          {compact = compact, def = def})

      fun doit (Info.T a, output) =
         Array.foreach
         (a, fn {domain, range, ...} =>
          output (domain, range))
      val () = doit (types, fn (name, tyStr) =>
                     outputTypeDefn ([], name, tyStr))
      val () = doit (vals, fn (name, (vid, scheme)) =>
                     outputValDefn ([], name, (vid, scheme)))
      val () = doit (sigs, outputSigDefn)
      val () =  doit (strs, fn (name, S) =>
                      if flat
                         then outputStrDefnFlat ([], name, S)
                         else outputStrDefn ([], name, S))
      val () = doit (fcts, outputFctDefn)
      val () = doit (bass, outputBasDefn)
      val () = destroy ()
   in
      ()
   end

(* ------------------------------------------------- *)
(*                   processDefUse                   *)
(* ------------------------------------------------- *)

fun processDefUse (E as T f) =
   let
      val {destroy = destroyLayoutPrettyTycon,
           layoutPrettyTycon, ...} =
         makeLayoutPrettyTycon (E, {prefixUnset = false})
      val {destroy = destroyLayoutPrettyTyvar,
           layoutPretty = layoutPrettyTyvar,
           reset = resetLayoutPrettyTyvar} =
         Tyvar.makeLayoutPrettyLocal ()
      fun layoutPrettyScheme s =
         let
            val () = resetLayoutPrettyTyvar ()
         in
            (#1 o Type.layoutPretty)
            (Scheme.ty s,
             {expandOpaque = false,
              layoutPrettyTycon = layoutPrettyTycon,
              layoutPrettyTyvar = layoutPrettyTyvar})
         end
      val destroy = fn () =>
         (destroyLayoutPrettyTyvar ()
          ; destroyLayoutPrettyTycon ())

      val _ = forceUsed E
      val all: {class: Class.t,
                def: Layout.t,
                extra: Layout.t list,
                isUsed: bool,
                region: Region.t,
                uses: Region.t list} list ref = ref []
      fun doit (sel, mkExtra) =
         let
            val NameSpace.T {defUses, region, toSymbol, ...} = sel f
         in
            List.foreach
            (Option.fold (defUses, [], ! o #1),
             fn {class, def, uses, range, ...} =>
             List.push
             (all, {class = class,
                    def = Symbol.layout (toSymbol def),
                    extra = mkExtra range,
                    isUsed = Uses.isUsed uses,
                    region = region def,
                    uses = List.fold (Uses.all uses, [], fn (u, ac) =>
                                      region u :: ac)}))
         end
      val _ = doit (#fcts, fn _ => [])
      val _ = doit (#sigs, fn _ => [])
      val _ = doit (#strs, fn _ => [])
      val _ = doit (#types, fn _ => [])
      local
         fun mkExtraFromScheme so =
            case so of
               NONE => []
             | SOME (_, s) => [layoutPrettyScheme s]
      in
         val _ = doit (#vals, mkExtraFromScheme)
      end
      val a = Array.fromList (!all)
      val _ =
         QuickSort.sortArray (a, fn ({region = r, ...}, {region = r', ...}) =>
                              Region.<= (r, r'))
      val l =
         Array.foldr
         (a, [], fn (z as {class, def, extra, isUsed, region, uses}, ac) =>
          case ac of
             [] => [z]
           | {extra = e', isUsed = i', region = r', uses = u', ...} :: ac' =>
                if Region.equals (region, r')
                   then {class = class,
                         def = def,
                         extra = extra @ e',
                         isUsed = isUsed orelse i',
                         region = region,
                         uses = uses @ u'} :: ac'
                else z :: ac)
      val _ =
         List.foreach
         (l, fn {class, def, isUsed, region, ...} =>
          if isUsed orelse Option.isNone (Region.left region)
             then ()
          else
             Control.warning
             (region,
              seq [str (concat ["unused ", Class.toString class, ": "]), def],
              Layout.empty))
      val _ =
         case !Control.showDefUse of
            NONE => ()
          | SOME f =>
               File.withOut
               (f, fn out =>
                List.foreach
                (l, fn {class, def, extra, region, uses, ...} =>
                 case Region.left region of
                    NONE => ()
                  | SOME p =>
                       let
                          val uses = Array.fromList uses
                          val _ = QuickSort.sortArray (uses, Region.<=)
                          val uses =
                             Array.foldr
                             (uses, [], fn (r, ac) =>
                              case ac of
                                 [] => [r]
                               | r' :: _ =>
                                    if Region.equals (r, r')
                                       then ac
                                    else r :: ac)
                          open Layout
                       in
                          outputl
                          (align [seq [str (Class.toString class),
                                       str " ",
                                       def,
                                       str " ",
                                       str (SourcePos.toString p),
                                       case extra of
                                          [] => empty
                                        | ss => let
                                             val ts =
                                                 List.map (ss,
                                                           toString)
                                             val uts =
                                                 List.map (List.equivalence
                                                           (ts, String.equals),
                                                           hd)
                                             val sts =
                                                 List.insertionSort
                                                 (uts,
                                                  fn (l, r) =>
                                                     size l < size r
                                                     orelse size l = size r
                                                            andalso l < r)
                                          in
                                             str (concat
                                                  (" \"" ::
                                                   List.separate
                                                   (sts, " andalso ") @ ["\""]))
                                          end],
                                  indent
                                  (align
                                   (List.map
                                    (uses, fn r =>
                                     str (case Region.left r of
                                              NONE => "NONE"
                                            | SOME p =>
                                              SourcePos.toString p))),
                                   4)],
                           out)
                       end))
      val () = destroy ()
   in
      ()
   end

(* ------------------------------------------------- *)
(*                      newCons                      *)
(* ------------------------------------------------- *)

fun newCons (T {vals, ...}, v) =
   let
      val forceUsed = 1 = Vector.length v
   in
      (Cons.fromVector o Vector.map)
      (v, fn {con, name, scheme} =>
       let
          val uses =
             NameSpace.newUses
             (vals,
              {def = Ast.Vid.fromCon name,
               range = (Vid.Con con, scheme),
               forceUsed = forceUsed})
       in
          {con = con,
           name = name,
           scheme = scheme,
           uses = uses}
       end)
   end

(* ------------------------------------------------- *)
(*                      cut                          *)
(* ------------------------------------------------- *)

local

fun makeOpaque (S: Structure.t, I: Interface.t, {prefix: string}) =
   let
      fun fixCons (cs, cs') =
         Cons.map
         (cs', fn {name, scheme, ...} =>
          let
             val (con, uses) =
                case Vector.peek (Cons.dest cs, fn {name = n, ...} =>
                                  Ast.Con.equals (n, name)) of
                   NONE => (Con.bogus, Uses.new ())
                 | SOME {con, uses, ...} => (con, uses)
          in
             {con = con, scheme = scheme, uses = uses}
          end)
      val (S', instantiate) = Structure.dummy (I, {prefix = prefix})
      val _ = instantiate (S, fn (c, s) =>
                           Tycon.setOpaqueExpansion
                           (c, fn ts => TypeStr.apply (s, ts)))
      val {destroy, 
           get : Structure.t -> {formal: Structure.t, new: Structure.t} list ref,
           ...} =
         Property.destGet (Structure.plist, Property.initFun (fn _ => ref []))
(*
      fun replace (S, S'): Structure.t =
         reallyReplace (S, S')
*)
      fun replace (S, S'): Structure.t =
         let
            val seen = get S
         in
            case List.peek (!seen, fn {formal, ...} =>
                            Structure.eq (S', formal)) of
               NONE => let
                          val new = reallyReplace (S, S')
                          val _ = List.push (seen, {formal = S', new = new})
                       in
                          new
                       end
             | SOME {new, ...} => new
         end
      and reallyReplace (S, S'): Structure.t =
         let
            val Structure.T {strs, 
                             types, 
                             vals, ...} = S
            val Structure.T {strs = strs', 
                             types = types', 
                             vals = vals', ...} = S'
            val strs = Info.map2 (strs, strs', replace)
            val types =
               Info.map2
               (types, types', fn (s, s') =>
                let
                   datatype z = datatype TypeStr.node
                in
                   case TypeStr.node s' of
                      Datatype {cons = cs', tycon} =>
                         (case TypeStr.node s of
                             Datatype {cons = cs, ...} =>
                                TypeStr.data
                                (tycon, fixCons (cs, cs'))
                           | _ => s')
                    | Scheme _ => s'
                    | Tycon _ => s'
                end)
            val vals =
               Info.map2 
               (vals, vals', fn ((v, _), (_, s')) =>
                (v, s'))
         in
            Structure.T {interface = Structure.interface S',
                         plist = PropertyList.new (),
                         strs = strs,
                         types = types,
                         vals = vals}
         end
      val S'' = replace (S, S')
      val _ = destroy ()
   in
      S''
   end

fun transparentCut (E: t, S: Structure.t, I: Interface.t,
                    {isFunctor: bool, prefix: string},
                    region: Region.t): Structure.t * Decs.t =
   let
      val I = Interface.copy I
      val flexTyconMap = Interface.flexibleTycons I
      val () =
         Structure.realize
         (S, flexTyconMap,
          fn (name, flex, typeStr, {nest = strids}) =>
          let
             val {admitsEquality = a, hasCons, kind = k, ...} =
                FlexibleTycon.dest flex
             fun dummy () =
                TypeStr.tycon
                (FlexibleTycon.dummyTycon
                 (flex, name, strids, {prefix = prefix}))
             val typeStr =
                case typeStr of
                   NONE => dummy ()
                 | SOME typeStr =>
                      (* Only realize a plausible candidate for typeStr. *)
                      if Kind.equals (k, TypeStr.kind typeStr)
                         andalso AdmitsEquality.<= (a, TypeStr.admitsEquality typeStr)
                         andalso (not hasCons orelse Option.isSome (TypeStr.toTyconOpt typeStr))
                         then typeStr
                         else dummy ()
             val () = FlexibleTycon.realize (flex, typeStr)
          in
             ()
          end)
      (* This tick is so that the type schemes for any values that need to be
       * instantiated and then re-generalized will be at a new time, so we can
       * check if something should not be generalized.
       *)
      val () = TypeEnv.Time.tick {region = region}
      val sign =
         if isFunctor
            then "argument signature"
         else "signature"

      val {destroy = destroyInterfaceSigid,
           get = interfaceSigid: Interface.t -> (Sigid.t * Interface.t) option,
           set = setInterfaceSigid, ...} =
         Property.destGetSet (Interface.plist, Property.initConst NONE)
      val {destroy = destroyLayoutPrettyTycon,
           layoutPrettyTycon, setLayoutPrettyTycon,
           loopStr, loopFlexTyconMap, ...} =
         genLayoutPrettyTycon {prefixUnset = true}
      val pre =
         Promise.delay
         (fn () =>
          let
             val {sigs, strs, types, ...} = current (E, fn _ => true)
             val _ =
                Info.foreachByTime
                (sigs (), fn (s, I) =>
                 setInterfaceSigid (I, SOME (s, I)))
             val _ = loopFlexTyconMap (flexTyconMap, 2, [Strid.uSig])
             val _ = loopStr (S, 1, [Strid.uStr])
             val _ =
                loopStr (Structure.T {interface = NONE,
                                      plist = PropertyList.new (),
                                      strs = strs (),
                                      types = types (),
                                      vals = Info.T (Array.new0 ())},
                         0, [])
          in
             ()
          end)
      val interfaceSigid = fn I =>
         (Promise.force pre; interfaceSigid I)
      val layoutPrettyTycon = fn c =>
         (Promise.force pre; layoutPrettyTycon c)
      val {destroy = destroyLayouts,
           layoutPrettyType, layoutPrettyTyvar,
           layoutStrSpec, layoutTypeSpec, layoutValSpec,
           localInitLayoutPrettyTyvar, ...} =
         Interface.layouts {interfaceSigid = interfaceSigid,
                            layoutPrettyTycon = layoutPrettyTycon,
                            setLayoutPrettyTycon = setLayoutPrettyTycon}

      datatype sort = datatype Interface.TypeStr.Sort.t
      val sort = Interface.TypeStr.sort

      val decs = ref []
      fun map {strInfo: ('name, 'strRange) Info.t,
               ifcArray: ('name * 'ifcRange) array,
               strids: Strid.t list,
               nameEquals: 'name * 'name -> bool,
               nameLayout: 'name -> Layout.t,
               specs: 'name * 'ifcRange -> Region.t list,
               notFound: 'name * 'ifcRange -> {diag: {spec: Layout.t option,
                                                      thing: string} option,
                                               range: 'range},
               doit: 'name * 'strRange * 'name * 'ifcRange -> 'range}: ('name, 'range) Info.t =
         let
            val Info.T strArray = strInfo
            val n = Array.length strArray
            val r = ref 0
            val array =
               Array.map
               (ifcArray, fn (ifcName, ifcRange) =>
                let
                   fun find i =
                      if i = n
                         then
                            let
                               val {diag, range} = notFound (ifcName, ifcRange)
                               val _ =
                                  Option.app
                                  (diag, fn {thing, spec} =>
                                   Control.error
                                   (region,
                                    seq [str thing,
                                         str " in ",
                                         str sign,
                                         str " but not in structure: ",
                                         layoutLongRev (strids, nameLayout ifcName)],
                                    align ((case spec of
                                               NONE => Layout.empty
                                             | SOME spec => seq [str "signature: ", spec])::
                                           (List.map
                                            (specs (ifcName, ifcRange), fn r =>
                                             seq [str "spec at: ", Region.layout r])))))
                            in
                               {domain = ifcName,
                                range = range,
                                time = Time.next (),
                                uses = Uses.new ()}
                            end
                      else
                         let
                            val {domain = strName, range = strRange, time, uses} =
                               Array.sub (strArray, i)
                         in
                            if nameEquals (strName, ifcName)
                               then (r := i + 1
                                     ; {domain = strName,
                                        range = doit (strName, strRange, ifcName, ifcRange),
                                        time = time,
                                        uses = uses})
                            else find (i + 1)
                         end
                in
                   find (!r)
                end)
         in
            Info.T array
         end
      val {destroy, get: Structure.t -> (Interface.t * Structure.t) list ref,
           ...} =
         Property.destGet (Structure.plist, Property.initFun (fn _ => ref []))
(*
      fun cut (S, I, strids): Structure.t =
         reallyCut (S, I, strids)
*)
      fun cut (S, I, flexTyconMap, strids): Structure.t =
         let
            val seen = get S
         in
            case List.peek (!seen, fn (I', _) => Interface.equals (I, I')) of
               NONE =>
                  let
                     fun really () = reallyCut (S, I, flexTyconMap, strids)
                     val S = 
                        case Structure.interface S of
                           NONE => really ()
                         | SOME I' =>
                              if Interface.equals (I, I')
                                 then S
                              else really ()
                     val _ = List.push (seen, (I, S))
                  in
                     S
                  end
             | SOME (_, S) => S
         end
      and reallyCut (S, I, flexTyconMap, strids) =
         let
            val Structure.T {strs = strStrs, types = strTypes, vals = strVals, ...} = S
            val {strs = sigStrs, types = sigTypes, vals = sigVals} = Interface.dest I
            val types =
               map {strInfo = strTypes,
                    ifcArray = sigTypes,
                    strids = strids,
                    nameEquals = Ast.Tycon.equals,
                    nameLayout = Ast.Tycon.layout,
                    specs = fn (name, sigStr) =>
                            Interface.TypeStr.specs (sigStr, Ast.Tycon.region name),
                    notFound = fn (name, sigStr) =>
                    let
                       val spec =
                          layoutTypeSpec
                          (strids, name, sigStr,
                           {compact = false,
                            def = false,
                            flexTyconMap = flexTyconMap})
                       val thing = "type"

                       val rlzStr = Interface.TypeStr.toEnv sigStr
                    in
                       {diag = SOME {spec = SOME spec,
                                     thing = thing},
                        range = rlzStr}
                    end,
                    doit = fn (strName, strStr, sigName, sigStr) =>
                    let
                       val rlzStr = Interface.TypeStr.toEnv sigStr
                       val error: (Layout.t list * Layout.t * Layout.t) option ref = ref NONE
                       fun reportError () =
                          case !error of
                             NONE => ()
                           | SOME (msgs, strError, sigError) =>
                                Control.error
                                (region,
                                 seq [str "type in structure disagrees with signature (",
                                      (seq o List.separate) (List.rev msgs, str ", "),
                                      str "): ",
                                      layoutLongRev (strids, Ast.Tycon.layout sigName)],
                                 align ((seq [str "structure: ", strError]) ::
                                        (seq [str "defn at: ",
                                              Region.layout (Ast.Tycon.region strName)]) ::
                                        (seq [str "signature: ", sigError]) ::
                                        (List.map
                                         (Interface.TypeStr.specs
                                          (sigStr, Ast.Tycon.region sigName),
                                          fn r => seq [str "spec at: ", Region.layout r]))))
                       val error = fn (msg, strError, sigError) =>
                          let
                             val msgs =
                                case !error of
                                   NONE => [str msg]
                                 | SOME (msgs, _, _) => (str msg)::msgs
                          in
                             error := SOME (msgs, strError, sigError)
                          end

                       val strKind = TypeStr.kind strStr
                       val strArity =
                          case strKind of
                             Kind.Arity strArity => strArity
                           | _ => Error.bug "ElaborateEnv.transparentCut.reallyCut.<anon>: strArity"
                       val sigKind = Interface.TypeStr.kind sigStr
                       val sigArity =
                          case sigKind of
                             Kind.Arity sigArity => sigArity
                           | _ => Error.bug "ElaborateEnv.transparentCut.reallyCut.<anon>: sigArity"
                       local
                          val tyvars =
                             Vector.tabulate
                             (Int.max (strArity, sigArity), fn _ =>
                              Tyvar.makeNoname {equality = false})
                          val () = localInitLayoutPrettyTyvar tyvars
                       in
                          val strTyvars = Vector.prefix (tyvars, strArity)
                          val strTyargs = Vector.map (strTyvars, Type.var)
                          val sigTyvars = Vector.prefix (tyvars, sigArity)
                          val sigTyargs = Vector.map (sigTyvars, Type.var)
                       end
                       fun layoutTyvars tyvars =
                          let
                             open Layout
                             val tyvars =
                                case Vector.length tyvars of
                                   0 => empty
                                 | 1 => layoutPrettyTyvar (Vector.first tyvars)
                                 | _ => tuple (Vector.toListMap (tyvars, layoutPrettyTyvar))
                             val tyvars =
                                if strArity = sigArity
                                   then tyvars
                                   else bracket tyvars
                          in
                             if isEmpty tyvars
                                then str " "
                                else seq [str " ", tyvars, str " "]
                          end

                       val sort = sort (sigName, sigStr, rlzStr, flexTyconMap)

                       fun sigMsg (b, rest) =
                          let
                             val empty = Layout.empty
                             val indent = fn l => Layout.indent (l, 3)
                             val rest =
                                case rest of
                                   NONE => SOME (str "...")
                                 | SOME _ => rest
                             val (kw, rest) =
                                case sort of
                                   Datatype _ => ("datatype", rest)
                                 | Scheme _ => ("type", rest)
                                 | Type {admitsEquality} =>
                                      (if admitsEquality then "eqtype" else "type",
                                       NONE)
                          in
                             mayAlign [seq [if b then bracket (str kw) else str kw,
                                            layoutTyvars sigTyvars,
                                            layoutLongRev (strids, Ast.Tycon.layout sigName),
                                            if Option.isSome rest then str " =" else empty],
                                       indent (case rest of
                                                  NONE => empty
                                                | SOME rest => rest)]
                          end
                     fun strMsg (b, rest) =
                        let
                           val empty = Layout.empty
                           val indent = fn l => Layout.indent (l, 3)
                           val rest =
                              case rest of
                                 NONE => SOME (str "...")
                               | SOME _ => rest
                           val kw =
                              case TypeStr.node strStr of
                                 TypeStr.Datatype _ => "datatype"
                               | TypeStr.Scheme _ => "type"
                               | TypeStr.Tycon _ => "type"
                        in
                           mayAlign [seq [if b then bracket (str kw) else str kw,
                                          layoutTyvars strTyvars,
                                          layoutLongRev (strids, Ast.Tycon.layout strName),
                                          if Option.isSome rest then str " =" else empty],
                                     indent (case rest of
                                                NONE => empty
                                              | SOME rest => rest)]
                        end

                       val lay = #1 o layoutPrettyType

                       fun unify (t, t', error) =
                          let
                             val error = fn (l, l', _) =>
                                error (l, l')
                          in
                             Type.unify
                             (t, t', {error = error,
                                      layoutPretty = layoutPrettyType,
                                      layoutPrettyTycon = layoutPrettyTycon,
                                      layoutPrettyTyvar = layoutPrettyTyvar})
                          end

                       val () =
                          if Kind.equals (strKind, sigKind)
                             then ()
                             else error ("arity",
                                         strMsg (false, NONE),
                                         sigMsg (false, NONE))
                       val resStr =
                          case sort of
                             Type _ =>
                                let
                                   val sigEq = Interface.TypeStr.admitsEquality sigStr
                                   val strEq = TypeStr.admitsEquality strStr
                                   val _ =
                                      if AdmitsEquality.<= (sigEq, strEq)
                                         then ()
                                         else error ("admits equality",
                                                     strMsg (false, SOME (TypeStr.explainDoesNotAdmitEquality
                                                                          (strStr,
                                                                           {layoutPrettyTycon = layoutPrettyTycon}))),
                                                     sigMsg (true, NONE))
                                in
                                   rlzStr
                                end
                           | Scheme sigScheme =>
                                let
                                   fun chkScheme strScheme =
                                      unify
                                      (Scheme.apply (strScheme, strTyargs),
                                       Scheme.apply (sigScheme, sigTyargs),
                                       fn (l, l') => error ("type definition",
                                                            strMsg (false, SOME l),
                                                            sigMsg (false, SOME l')))
                                   val _ =
                                      case TypeStr.node strStr of
                                         TypeStr.Datatype {tycon = strTycon, ...} =>
                                            let
                                               val strScheme = Scheme.fromTycon strTycon
                                            in
                                               unify
                                               (Scheme.apply (strScheme, strTyargs),
                                                Scheme.apply (sigScheme, sigTyargs),
                                                fn _ =>
                                                error ("type structure",
                                                       strMsg (true, NONE),
                                                       sigMsg (false, SOME (bracket (lay (Scheme.apply (sigScheme, sigTyargs)))))))
                                            end
                                       | TypeStr.Scheme s =>
                                            chkScheme s
                                       | TypeStr.Tycon c =>
                                            chkScheme (Scheme.fromTycon c)
                                in
                                   rlzStr
                                end
                           | Datatype {repl = true, tycon = sigTycon, ...} =>
                                let
                                   val sigScheme = Scheme.fromTycon sigTycon
                                   fun nonDatatype strScheme =
                                      (error ("type structure",
                                              strMsg (false, SOME (bracket (lay (Scheme.apply (strScheme, strTyargs))))),
                                              sigMsg (false, SOME (bracket (seq [str "datatype ",
                                                                                 lay (Scheme.apply (sigScheme, sigTyargs))]))))
                                       ; rlzStr)
                                in
                                   case TypeStr.node strStr of
                                      TypeStr.Datatype {tycon = strTycon, ...} =>
                                         let
                                            val strScheme = Scheme.fromTycon strTycon
                                         in
                                            Exn.withEscape
                                            (fn escape =>
                                             (unify
                                              (Scheme.apply (strScheme, strTyargs),
                                               Scheme.apply (sigScheme, sigTyargs),
                                               fn _ =>
                                               (error ("type structure",
                                                       strMsg (true, NONE),
                                                       sigMsg (false, SOME (bracket (seq [str "datatype ",
                                                                                          lay (Scheme.apply (sigScheme, sigTyargs))]))))
                                                ; escape rlzStr))
                                              ; strStr))
                                         end
                                    | TypeStr.Scheme strScheme =>
                                         nonDatatype strScheme
                                    | TypeStr.Tycon strTycon =>
                                         nonDatatype (Scheme.fromTycon strTycon)
                                end
                           | Datatype {repl = false, cons = sigCons, ...} =>
                                let
                                   fun nonDatatype strScheme =
                                      (error ("type structure",
                                              strMsg (false, SOME (bracket (lay (Scheme.apply (strScheme, strTyargs))))),
                                              sigMsg (true, NONE))
                                       ; rlzStr)
                                in
                                   case TypeStr.node strStr of
                                      TypeStr.Datatype {cons = strCons, ...} =>
                                         let
                                            val extra: bool ref = ref false
                                            fun conScheme (scheme, tyvars) =
                                               case Type.deArrowOpt (Scheme.apply (scheme, tyvars)) of
                                                  NONE => NONE
                                                | SOME (ty, _) => SOME ty
                                            fun layCon (name, scheme, tyvars) =
                                               (bracket o seq)
                                               [Ast.Con.layout name,
                                                case conScheme (scheme, tyvars) of
                                                   NONE => Layout.empty
                                                 | SOME _ => str " of _"]
                                            fun loop (sigCons, strCons, sigConsAcc, strConsAcc) =
                                               case (sigCons, strCons) of
                                                  ([], []) => (List.rev sigConsAcc, List.rev strConsAcc)
                                                | ({name, scheme = sigScheme}::sigCons, []) =>
                                                     loop (sigCons,
                                                           [],
                                                           (layCon (name, sigScheme, sigTyargs))::sigConsAcc,
                                                           strConsAcc)
                                                | ([], {name, scheme = strScheme}::strCons) =>
                                                     loop ([],
                                                           strCons,
                                                           sigConsAcc,
                                                           (layCon (name, strScheme, strTyargs))::strConsAcc)
                                                | (sigCons as {name = sigName, scheme = sigScheme}::sigCons',
                                                   strCons as {name = strName, scheme = strScheme}::strCons') =>
                                                     (case Ast.Con.compare (sigName, strName) of
                                                         LESS =>
                                                            loop (sigCons',
                                                                  strCons,
                                                                  (layCon (sigName, sigScheme, sigTyargs))::sigConsAcc,
                                                                  strConsAcc)
                                                       | EQUAL =>
                                                            (case (conScheme (sigScheme, sigTyargs), conScheme (strScheme, strTyargs)) of
                                                                (NONE, NONE) => (extra := true
                                                                                 ; loop (sigCons', strCons',
                                                                                         sigConsAcc, strConsAcc))
                                                              | (NONE, SOME _) =>
                                                                   loop (sigCons', strCons',
                                                                         (Ast.Con.layout sigName)::sigConsAcc,
                                                                         (seq [Ast.Con.layout strName, str " [of _]"])::strConsAcc)
                                                              | (SOME _, NONE) =>
                                                                   loop (sigCons', strCons',
                                                                         (seq [Ast.Con.layout sigName, str " [of _]"])::sigConsAcc,
                                                                         (Ast.Con.layout strName)::strConsAcc)
                                                              | (SOME sigTy, SOME strTy) =>
                                                                   Exn.withEscape
                                                                   (fn escape =>
                                                                    (unify
                                                                     (sigTy, strTy,
                                                                      fn (sigLay, strLay) =>
                                                                      (escape o loop)
                                                                      (sigCons', strCons',
                                                                       (seq [Ast.Con.layout sigName, str " of ", sigLay])::sigConsAcc,
                                                                       (seq [Ast.Con.layout strName, str " of ", strLay])::strConsAcc))
                                                                     ; extra := true
                                                                     ; loop (sigCons', strCons',
                                                                             sigConsAcc, strConsAcc))))
                                                       | GREATER =>
                                                            loop (sigCons,
                                                                  strCons',
                                                                  sigConsAcc,
                                                                  (layCon (strName, strScheme, strTyargs))::strConsAcc))
                                            val (sigCons, strCons) =
                                               loop (Vector.toListMap
                                                     (Cons.dest sigCons, fn {name, scheme, ...} =>
                                                      {name = name, scheme = scheme}),
                                                     Vector.toListMap
                                                     (Cons.dest strCons, fn {name, scheme, ...} =>
                                                      {name = name, scheme = scheme}),
                                                     [],
                                                     [])
                                            val resStr =
                                               if List.isEmpty sigCons
                                                  andalso List.isEmpty strCons
                                                  then strStr
                                                  else let
                                                          fun layCons cons =
                                                             let
                                                                val cons =
                                                                   if !extra
                                                                      then List.snoc (cons, str "...")
                                                                      else cons
                                                                val cons = alignPrefix (cons, "| ")
                                                             in
                                                                SOME cons
                                                             end
                                                       in
                                                          error ("constructors",
                                                                 strMsg (false, layCons strCons),
                                                                 sigMsg (false, layCons sigCons))
                                                          ; rlzStr
                                                       end
                                         in
                                            resStr
                                         end
                                    | TypeStr.Scheme strScheme =>
                                         nonDatatype strScheme
                                    | TypeStr.Tycon strTycon =>
                                         nonDatatype (Scheme.fromTycon strTycon)
                                end
                       val () = reportError ()
                    in
                       resStr
                    end}
            val vals =
               map
               {strInfo = strVals,
                ifcArray = sigVals,
                strids = strids,
                nameEquals = Ast.Vid.equals,
                nameLayout = Ast.Vid.layout,
                specs = fn (name, _) => [Ast.Vid.region name],
                notFound = fn (name, (sigStatus, sigScheme)) =>
                let
                   val spec =
                      layoutValSpec
                      (strids, name, (sigStatus, sigScheme),
                       {compact = false, con = false, def = false})
                   val thing = Status.pretty sigStatus

                   val con = Con.newString o Ast.Vid.toString
                   val var = Var.newString o Ast.Vid.toString
                   val vid =
                      case sigStatus of
                         Status.Con => Vid.Con (con name)
                       | Status.Exn => Vid.Exn (con name)
                       | Status.Var => Vid.Var (var name)
                   val rlzScheme = Interface.Scheme.toEnv sigScheme
                in
                   {diag = Option.map (spec, fn spec =>
                                       {spec = SOME spec,
                                        thing = thing}),
                    range = (vid, rlzScheme)}
                end,
                doit = fn (strName, (strVid, strScheme), sigName, (sigStatus, sigScheme)) =>
                let
                   val rlzScheme = Interface.Scheme.toEnv sigScheme
                   val unifyError = ref NONE
                   val statusError = ref false
                   val (rlzTyvars, rlzType) = Scheme.fresh rlzScheme
                   val () = localInitLayoutPrettyTyvar rlzTyvars
                   val {args = strTyargs, instance = strType} =
                      Scheme.instantiate strScheme
                   val _ =
                      Type.unify
                      (strType, rlzType,
                       {error = fn (l, l', {notes, ...}) =>
                                unifyError := SOME (l, l', notes),
                        layoutPretty = layoutPrettyType,
                        layoutPrettyTycon = layoutPrettyTycon,
                        layoutPrettyTyvar = layoutPrettyTyvar})
                   val strTyargs = strTyargs ()
                   fun addDec (name: string, n: Exp.node): Vid.t =
                      let
                         val x = Var.newString name
                         val e = Exp.make (n, strType)
                         val _ =
                            List.push
                            (decs,
                             Dec.Val {matchDiags = {nonexhaustiveExn = Control.Elaborate.DiagDI.Default,
                                                    nonexhaustive = Control.Elaborate.DiagEIW.Ignore,
                                                    redundant = Control.Elaborate.DiagEIW.Ignore},
                                      rvbs = Vector.new0 (),
                                      tyvars = fn () => rlzTyvars,
                                      vbs = (Vector.new1
                                             {ctxt = fn _ => Layout.empty,
                                              exp = e,
                                              layPat = fn _ => Layout.empty,
                                              nest = [],
                                              pat = Pat.var (x, strType),
                                              regionPat = Region.bogus})})
                      in
                         Vid.Var x
                      end
                   fun con (c: Con.t): Vid.t =
                      addDec (Con.originalName c, Exp.Con (c, strTyargs))
                   val strStatus = Status.fromVid strVid
                   val vid =
                      case (strVid, sigStatus) of
                         (Vid.Con c, Status.Var) => con c
                       | (Vid.Exn c, Status.Var) => con c
                       | (Vid.Var x, Status.Var) =>
                            if 0 < Vector.length rlzTyvars
                               orelse 0 < Vector.length strTyargs
                               then addDec (Var.originalName x,
                                            Exp.Var (fn () => x, fn () => strTyargs))
                               else strVid
                       | (Vid.Con _, Status.Con) => strVid
                       | (Vid.Exn _, Status.Exn) => strVid
                       | _ => (statusError := true; strVid)
                   val () =
                      if Option.isNone (!unifyError) andalso not (!statusError)
                         then ()
                         else let
                                 val errors = []
                                 val errors =
                                    if Option.isSome (!unifyError)
                                       then str "type" :: errors
                                       else errors
                                 val errors =
                                    if !statusError
                                       then str "status" :: errors
                                       else errors
                                 val name =
                                    layoutLongRev (strids, Ast.Vid.layout sigName)
                                 val (strTy, sigTy, notes) =
                                    case !unifyError of
                                       NONE =>
                                          let
                                             val lay = #1 (layoutPrettyType rlzType)
                                          in
                                             (lay, lay, Layout.empty)
                                          end
                                     | SOME (strLay, sigLay, notes) =>
                                          (strLay, sigLay, notes ())
                                 fun doit (space, status, ty, kind, vid) =
                                    let
                                       val indent = fn l => Layout.indent (l, 3)
                                       val kw = str (Status.kw status)
                                       val kw =
                                          if !statusError then bracket kw else kw
                                    in
                                       align [seq [str space, str ": ",
                                                   mayAlign
                                                   [seq [kw, str " ",
                                                         name,
                                                         str (if Ast.Vid.isSymbolic sigName
                                                                 then " :"
                                                                 else ":")],
                                                    indent ty]],
                                              seq [str kind, str " at: ",
                                                   Region.layout (Ast.Vid.region vid)]]
                                    end
                              in
                                 Control.error
                                 (region,
                                  seq [if !statusError
                                          then str "value identifier"
                                          else str (Vid.statusPretty strVid),
                                       str " in structure disagrees with ",
                                       str sign,
                                       str " (",
                                       (seq o List.separate)
                                       (errors, str ", "),
                                       str "): ",
                                       name],
                                  align [doit ("structure", strStatus, strTy,
                                               "defn", strName),
                                         doit ("signature", sigStatus, sigTy,
                                               "spec", sigName),
                                         notes])
                              end
                in
                   (vid, rlzScheme)
                end}
            val strs =
               map {strInfo = strStrs,
                    ifcArray = sigStrs,
                    strids = strids,
                    nameEquals = Strid.equals,
                    nameLayout = Strid.layout,
                    specs = fn (name, _) => [Strid.region name],
                    notFound = fn (name, I) =>
                    let
                       val spec =
                          layoutStrSpec
                          (strids, name, I,
                           {compact = false,
                            def = false,
                            elide = {strs = SOME (2, 0),
                                     types = NONE,
                                     vals = SOME (3, 2)},
                            flexTyconMap = flexTyconMap})
                       val thing = "structure"

                       val (S, _) = Structure.dummy (I, {prefix = ""})
                    in
                       {diag = SOME {spec = SOME spec,
                                     thing = thing},
                        range = S}
                    end,
                    doit = fn (_, S, name, I) =>
                    let
                       val flexTyconMap =
                          Option.fold
                          (TyconMap.peekStrid (flexTyconMap, name),
                           TyconMap.empty (),
                           fn (flexTyconMap, _) => flexTyconMap)
                    in
                       cut (S, I, flexTyconMap, name :: strids)
                    end}
         in
            Structure.T {interface = SOME I,
                         plist = PropertyList.new (),
                         strs = strs,
                         types = types,
                         vals = vals}
         end
      val S = cut (S, I, flexTyconMap, [])
      val () = destroy ()
      val () = destroyLayouts ()
      val () = destroyLayoutPrettyTycon ()
      val () = destroyInterfaceSigid ()
   in
      (S, Decs.fromList (!decs))
   end

in

(* section 5.3, 5.5, 5.6 and rules 52, 53 *)
fun cut (E: t, S: Structure.t, I: Interface.t,
         {isFunctor: bool, opaque: bool, prefix: string}, region)
   : Structure.t * Decs.t =
   let
      val (S, decs) = transparentCut (E, S, I, {isFunctor = isFunctor, prefix = prefix}, region)
      val S =
         if opaque
            then makeOpaque (S, I, {prefix = prefix})
         else S
   in
      (S, decs)
   end

val cut =
   Trace.trace ("ElaborateEnv.cut",
                fn (_, S, I, _, _) =>
                Layout.tuple [Structure.layout S,
                              Interface.layout I],
                Structure.layout o #1)
   cut

end

(* ------------------------------------------------- *)
(*                  functorClosure                   *)
(* ------------------------------------------------- *)

fun functorClosure
   (E: t,
    name: Fctid.t,
    argInterface: Interface.t,
    makeBody: Structure.t * string list -> Decs.t * Structure.t option) =
   let
      val argId = Strid.uArg (Fctid.toString name)
      val resId = Strid.uRes (Fctid.toString name)
      val _ = insideFunctor := true
      (* Need to tick here so that any tycons created in the dummy structure
       * for the functor formal have a new time, and will therefore report an
       * error if they occur before the functor declaration.
       *)
      val _ =  TypeEnv.Time.tick {region = Fctid.region name}
      val (formal, instantiate) =
         Structure.dummy (argInterface, {prefix = Strid.toString argId ^ "."})
      (* Keep track of all tycons created during the instantiation of the
       * functor.  These will later become the generative tycons that will need
       * to be recreated for each functor application.
       *)
      val (resultStructure, generativeTycons) =
         Tycon.scopeNew
         (fn () =>
          let
             val nest = [Strid.toString resId]
             val (_, resultStructure) = makeBody (formal, nest)
             val _ = Option.app (resultStructure, Structure.forceUsed)
          in
             resultStructure
          end)
      val _ = insideFunctor := false
      val restore =
         if !Control.elaborateOnly
            then fn f => f ()
         else let 
                 val withSaved = Control.Elaborate.snapshot ()
                 val snapshot = snapshot E
              in 
                 fn f => snapshot (fn () => withSaved f)
              end
      fun summary actual =
         let
            val _ = Structure.forceUsed actual
            val {destroy = destroy1,
                 get = tyconTypeStr: Tycon.t -> TypeStr.t option,
                 set = setTyconTypeStr, ...} =
               Property.destGetSet (Tycon.plist, Property.initConst NONE)
            (* Match the actual against the formal, to set the tycons.
             * Then duplicate the result, replacing tycons.  Want to generate
             * new tycons just like the functor body did.
             *)
            val _ =
               instantiate (actual, fn (c, s) => setTyconTypeStr (c, SOME s))
            val _ =
               List.foreach
               (generativeTycons, fn c =>
                setTyconTypeStr
                (c, SOME (TypeStr.tycon (Tycon.makeLike c))))
            fun replaceType (t: Type.t): Type.t =
               let
                  fun con (c, ts) =
                     case tyconTypeStr c of
                        NONE => Type.con (c, ts)
                      | SOME s => TypeStr.apply (s, ts)
               in
                  Type.hom (t, {con = con,
                                expandOpaque = false,
                                record = Type.record,
                                replaceSynonyms = false,
                                var = Type.var})
               end
            fun replaceScheme (s: Scheme.t): Scheme.t =
               let
                  val (tyvars, ty) = Scheme.dest s
               in
                  Scheme.make {canGeneralize = true,
                               ty = replaceType ty,
                               tyvars = tyvars}
               end
            fun replaceCons cons: Cons.t =
               Cons.map
               (cons, fn {con, scheme, uses, ...} =>
                {con = con,
                 scheme = replaceScheme scheme,
                 uses = uses})
            fun replaceTypeStr (s: TypeStr.t): TypeStr.t =
               let
                  datatype z = datatype TypeStr.node
               in
                  case TypeStr.node s of
                     Datatype {cons, tycon} =>
                        let
                           val tycon =
                              case tyconTypeStr tycon of
                                 NONE => tycon
                               | SOME s =>
                                    (case TypeStr.toTyconOpt s of
                                        NONE => Error.bug "ElaborateEnv.functorClosure.apply: bad datatype"
                                      | SOME c => c)
                        in
                           TypeStr.data (tycon, replaceCons cons)
                        end
                   | Scheme s => TypeStr.def (replaceScheme s)
                   | Tycon c => (case tyconTypeStr c of
                                    NONE => s
                                  | SOME s => s)
               end
            val {destroy = destroy2,
                 get = replaceInterface: Interface.t -> Interface.t, ...} =
               Property.destGet
               (Interface.plist,
                Property.initRec
                (fn (I, replaceInterface) =>
                 let
                    val {strs, types, vals} = Interface.dest I
                    val replaceIScheme =
                       Interface.Scheme.fromEnv
                       o replaceScheme
                       o Interface.Scheme.toEnv
                    val replaceITypeStr =
                       Interface.TypeStr.fromEnv
                       o replaceTypeStr
                       o Interface.TypeStr.toEnv
                 in
                    Interface.new
                    {isClosed = true,
                     original = SOME (Interface.original I),
                     strs = Array.map (strs, fn (strid, I) =>
                                       (strid, replaceInterface I)),
                     types = Array.map (types, fn (tycon, s) =>
                                        (tycon, replaceITypeStr s)),
                     vals = Array.map (vals, fn (vid, (status, scheme)) =>
                                       (vid, (status, replaceIScheme scheme)))}
                 end))
            val {destroy = destroy3,
                 get = replaceStructure: Structure.t -> Structure.t, ...} =
               Property.destGet
               (Structure.plist,
                Property.initRec
                (fn (Structure.T {interface, strs, types, vals, ... },
                     replaceStructure) =>
                 Structure.T
                 {interface = Option.map (interface, replaceInterface),
                  plist = PropertyList.new (),
                  strs = Info.map (strs, replaceStructure),
                  types = Info.map (types, replaceTypeStr),
                  vals = Info.map (vals, fn (status, s) =>
                                   (status, replaceScheme s))}))
            val resultStructure = Option.map (resultStructure, replaceStructure)
            val _ = destroy1 ()
            val _ = destroy2 ()
            val _ = destroy3 ()
         in
            resultStructure
         end
      val summary =
         Trace.trace
         ("ElaborateEnv.functorClosure.summary",
          fn actual =>
          Layout.record [("argInterface", Interface.layout argInterface),
                         ("formal", Structure.layout formal),
                         ("resultStructure", Option.layout Structure.layout resultStructure),
                         ("actual", Structure.layout actual)],
          Option.layout Structure.layout)
         summary
      fun apply (actual, nest) =
         if not (!insideFunctor)
            andalso not (!Control.elaborateOnly)
            andalso !Control.numErrors = 0
            then restore (fn () => makeBody (actual, nest))
         else (Decs.empty, summary actual)
   in
      FunctorClosure.T {apply = apply,
                        argInterface = argInterface,
                        resultStructure = resultStructure,
                        summary = summary}
   end

end
