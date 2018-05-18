(* Copyright (C) 2009,2015,2017 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Interface (S: INTERFACE_STRUCTS): INTERFACE =
struct

open S

local
   open Layout
in
   val align = align
   val empty = empty
   val seq = seq
   val str = str
   val bracket = fn l =>
      seq [str "[", l, str "]"]
end

local
   open Ast
in
   structure Longstrid = Longstrid
   structure Longtycon = Longtycon
   structure Record = SortedRecord
   structure Strid = Strid
   structure Vid = Vid
end

structure Etycon = EnvTycon
structure EtypeStr = EnvTypeStr

structure Set = DisjointSet

structure Status:
   sig
      datatype t = Con | Exn | Var

      val layout: t -> Layout.t
      val toString: t -> string
   end =
   struct
      datatype t = Con | Exn | Var

      val toString =
         fn Con => "Con"
          | Exn => "Exn"
          | Var => "Var"

      val layout = Layout.str o toString
   end

(* only needed for debugging *)
structure TyconId = IntUniqueId ()

structure Defn =
   struct
      type t = exn

      val layoutRef: (t -> Layout.t) ref = ref (fn _ => Layout.empty)
      fun layout d = (!layoutRef) d
   end

structure Time:>
   sig
      type t

      val < : t * t -> bool
      val current: unit -> t
      val layout: t -> Layout.t
      val min: t * t -> t
      val tick: unit -> t
   end =
   struct
      type t = int

      val op < = Int.<

      val layout = Int.layout

      val min = Int.min

      val currentTime: int ref = ref 0

      fun current () = !currentTime

      fun tick () =
         let
            val n = 1 + !currentTime
            val _ = currentTime := n
         in
            n
         end
   end

structure FlexibleTycon =
   struct
      (* hasCons is true if this tycon occurs in any type structure where the
       * cons are nonempty.  This allows us to do a quick check of the side
       * condition on rule 64 that requires all type structures to be well-formed
       * when implementing "where type". 
       *)
      datatype t = T of {admitsEquality: AdmitsEquality.t ref,
                         copy: copy,
                         creationTime: Time.t,
                         defn: exn ref,
                         hasCons: bool,
                         id: TyconId.t,
                         kind: Kind.t,
                         plist: PropertyList.t,
                         prettyDefault: string,
                         specs: Region.t AppendList.t ref} Set.t
      withtype copy = t option ref

      fun fields (T s) = Set.! s

      local
         fun make f = f o fields
      in
         val admitsEquality = ! o make #admitsEquality
         val defnRef = make #defn
         val defn = ! o defnRef
         val hasCons = make #hasCons
         val kind = make #kind
         val plist = make #plist
         val prettyDefault = make #prettyDefault
         fun setAdmitsEquality (f, ae) =
            (make #admitsEquality f) := ae
         val specsRef = make #specs
         val specs = ! o specsRef
      end
      val layoutPrettyDefault = Layout.str o prettyDefault

      fun dest fc =
         let
            val {admitsEquality, hasCons, kind, prettyDefault, ...} =
               fields fc
         in
            {admitsEquality = !admitsEquality,
             hasCons = hasCons,
             kind = kind,
             prettyDefault = prettyDefault}
         end

      val equals = fn (T s, T s') => Set.equals (s, s')

      fun layout fc =
         let
            open Layout
            val {admitsEquality, creationTime, defn, hasCons, id, kind, prettyDefault, ...} = fields fc
         in
            record [("admitsEquality", AdmitsEquality.layout (!admitsEquality)),
                    ("creationTime", Time.layout creationTime),
                    ("defn", Defn.layout (!defn)),
                    ("hasCons", Bool.layout hasCons),
                    ("id", TyconId.layout id),
                    ("kind", Kind.layout kind),
                    ("prettyDefault", String.layout prettyDefault)]
         end

      val copies: copy list ref = ref []

      fun make {admitsEquality: AdmitsEquality.t, defn: Defn.t,
                hasCons: bool, kind: Kind.t, prettyDefault: string,
                specs: Region.t AppendList.t}: t =
         T (Set.singleton {admitsEquality = ref admitsEquality,
                           copy = ref NONE,
                           creationTime = Time.current (),
                           defn = ref defn,
                           hasCons = hasCons,
                           id = TyconId.new (),
                           kind = kind,
                           plist = PropertyList.new (),
                           prettyDefault = prettyDefault,
                           specs = ref specs})

      fun pushSpec (fc, region) =
         let
            val specsRef = specsRef fc
         in
            specsRef := AppendList.snoc (!specsRef, region)
         end
   end

structure Tycon =
   struct
      datatype t =
         Flexible of FlexibleTycon.t
       | Rigid of Etycon.t

      val fromEnv: Etycon.t -> t = Rigid

      fun admitsEquality c =
         case c of
            Flexible f => FlexibleTycon.admitsEquality f
          | Rigid c => Etycon.admitsEquality c

      val arrow = fromEnv Etycon.arrow

      val equals =
         fn (Flexible f, Flexible f') => FlexibleTycon.equals (f, f')
          | (Rigid c, Rigid c') => Etycon.equals (c, c')
          | _ => false

      val exn = Rigid Etycon.exn

      val kind =
         fn Flexible f => FlexibleTycon.kind f
          | Rigid c => Etycon.kind c

      val layout =
         fn Flexible f => FlexibleTycon.layout f
          | Rigid c => Etycon.layout c

      val tuple = Rigid Etycon.tuple

      fun layoutAppPretty (c, ts, {layoutPrettyEnvTycon, layoutPrettyFlexTycon}) =
         case c of
            Flexible f =>
               EnvTycon.layoutAppPrettyNormal
               (layoutPrettyFlexTycon f, ts)
          | Rigid c =>
               EnvTycon.layoutAppPretty
               (c, ts, {layoutPretty = layoutPrettyEnvTycon})
   end

structure Type =
   struct
      datatype t =
         Con of Tycon.t * t vector
       | Record of t Record.t
       | Var of Tyvar.t

      fun arrow (t1, t2) = Con (Tycon.arrow, Vector.new2 (t1, t2))

      val con = Con

      fun deArrowOpt (t: t): (t * t) option =
         case t of
            Con (c, ts) =>
               if Tycon.equals (c, Tycon.arrow)
                  then SOME (Vector.sub (ts, 0), Vector.sub (ts, 1))
               else NONE
          | _ => NONE

      fun deArrow t =
         case deArrowOpt t of
            NONE => Error.bug "Interface.Type.deArrow"
          | SOME z => z

      fun deEta (t: t, tyvars: Tyvar.t vector): Tycon.t option =
         case t of
            Con (c, ts) =>
               if Vector.length ts = Vector.length tyvars
                  andalso Vector.foralli (ts, fn (i, t) =>
                                          case t of
                                             Var a =>
                                                Tyvar.equals
                                                (a, Vector.sub (tyvars, i))
                                           | _ => false)
                  then SOME c
               else NONE
           | _ => NONE

      val exn = Con (Tycon.exn, Vector.new0 ())

      fun hom (t, {con, record, var}) =
         let
            val rec loop =
               fn Con (c, ts) => con (c, Vector.map (ts, loop))
                | Record r => record (Record.map (r, loop))
                | Var a => var a
         in
            loop t
         end

      local
         open Layout
      in
         fun layout t =
            case t of
               Con (c, ts) =>
                  paren (align [seq [str "Con ", Tycon.layout c],
                                Vector.layout layout ts])
             | Record r =>
                  Record.layout {record = r,
                                 separator = ": ",
                                 extra = "",
                                 layoutTuple = Vector.layout layout,
                                 layoutElt = layout}
             | Var a => paren (seq [str "Var ", Tyvar.layout a])
      end

      val record = Record

      fun substitute (t: t, sub: (Tyvar.t * t) vector): t =
         let
            fun var a =
               case Vector.peek (sub, fn (a', _) => Tyvar.equals (a, a')) of
                  NONE => Error.bug "Interface.Type.substitute"
                | SOME (_, t) => t
         in
            hom (t, {con = Con,
                     record = Record,
                     var = var})
         end

      val var = Var
   end

structure Scheme = GenericScheme (structure Type = Type
                                  structure Tyvar = Tyvar)

structure Scheme =
   struct
      open Scheme

      fun kind (T {tyvars, ...}) =
         Kind.Arity (Vector.length tyvars)

      fun make (tyvars, ty) = T {ty = ty, tyvars = tyvars}

      fun fromTycon tycon =
         let
            val kind = Tycon.kind tycon
            val arity =
               case kind of
                  Kind.Arity arity => arity
                | Kind.Nary => Error.bug "Interface.Scheme.fromTycon: Kind.Nary"
            val tyvars =
               Vector.tabulate
               (arity, fn _ =>
                Tyvar.makeNoname {equality = false})
         in
            make (tyvars, Type.con (tycon, Vector.map (tyvars, Type.var)))
         end
   end

structure Cons :
   sig
      type t
      val dest: t -> {name: Ast.Con.t,
                      scheme: Scheme.t} vector
      val empty: t
      val fromSortedVector: {name: Ast.Con.t,
                             scheme: Scheme.t} vector -> t
      val fromVector: {name: Ast.Con.t,
                       scheme: Scheme.t} vector -> t
      val layout: t -> Layout.t
      val map: t * ({name: Ast.Con.t,
                     scheme: Scheme.t}
                    -> {scheme: Scheme.t}) -> t
   end =
   struct
      datatype t = T of {name: Ast.Con.t,
                         scheme: Scheme.t} vector

      fun dest (T v) = v

      val fromSortedVector = T

      fun fromVector v =
         (fromSortedVector o QuickSort.sortVector)
         (v, fn ({name = name1, ...}, {name = name2, ...}) =>
          case Ast.Con.compare (name1, name2) of
             LESS => true
           | EQUAL => true
           | GREATER => false)

      val empty = T (Vector.new0 ())

      fun map (T v, f) =
         (T o Vector.map)
         (v, fn elt as {name, ...} =>
          let
             val {scheme} =
                f elt
          in
             {name = name,
              scheme = scheme}
          end)

      fun layout (T v) =
         Vector.layout (fn {name, scheme} =>
                        let
                           open Layout
                        in
                           seq [Ast.Con.layout name,
                                str ": ",
                                Scheme.layout scheme]
                        end)
         v
   end

structure TypeStr =
   struct
      datatype node =
         Datatype of {tycon: Tycon.t,
                      cons: Cons.t,
                      repl: bool}
       | Scheme of Scheme.t
       | Tycon of {eq: bool,
                   tycon: Tycon.t}
      type t = node

      val node = fn s => s

      fun kind s =
         case node s of
            Datatype {tycon, ...} => Tycon.kind tycon
          | Scheme s => Scheme.kind s
          | Tycon {tycon, ...} => Tycon.kind tycon

      fun layout t =
         let
            open Layout
         in
            case node t of
               Datatype {tycon, cons, repl} =>
                  seq [str "Datatype ",
                       record [("tycon", Tycon.layout tycon),
                               ("cons", Cons.layout cons),
                               ("repl", Bool.layout repl)]]
             | Scheme s => Scheme.layout s
             | Tycon {eq, tycon} =>
                  seq [str "Tycon ",
                       record [("eq", Bool.layout eq),
                               ("tycon", Tycon.layout tycon)]]
         end

      fun apply (t: t, tys: Type.t vector): Type.t =
         case node t of
            Datatype {tycon, ...} => Type.con (tycon, tys)
          | Scheme s => Scheme.apply (s, tys)
          | Tycon {tycon, ...} => Type.con (tycon, tys)

      val apply =
         Trace.trace ("Interface.TypeStr.apply", Layout.tuple2 (layout, Vector.layout Type.layout), Type.layout)
         apply

      fun cons t =
         case node t of
            Datatype {cons, ...} => cons
          | _ => Cons.empty

      fun data (tycon, cons, repl) =
         Datatype {tycon = tycon, cons = cons, repl = repl}

      val def = Scheme

      fun tycon (tycon, eq) =
         Tycon {eq = eq, tycon = tycon}

      local
         fun tyconAsScheme c =
            def (Scheme.fromTycon c)
      in
         fun repl (t: t) =
            case node t of
               Datatype {tycon, cons, ...} => data (tycon, cons, true)
             | Scheme _ => t
             | Tycon {tycon, ...} => tyconAsScheme tycon

         fun abs (t: t) =
            case node t of
               Datatype {tycon, ...} => tyconAsScheme tycon
             | Scheme _ => t
             | Tycon {tycon, ...} => tyconAsScheme tycon
      end
   end

structure Defn =
   struct
      open Defn

      datatype dest =
         Realized of EtypeStr.t
       | TypeStr of TypeStr.t
       | Undefined

      exception U of dest

      val realized = U o Realized
      val typeStr = U o TypeStr
      val undefined = U Undefined

      fun dest (d: t): dest =
         case d of
            U u => u
          | _ => Error.bug "Interface.Defn.dest"

      val () =
         layoutRef :=
         (fn d =>
          let
             open Layout
          in
             case dest d of
                Realized s => seq [str "Realized ", EtypeStr.layout s]
              | TypeStr s => seq [str "TypeStr ", TypeStr.layout s]
              | Undefined => str "Undefined"
          end)
   end

(* expandTy expands all type definitions in ty *)
local
   fun con (c, ts) =
      case c of
         Tycon.Flexible f =>
            (case Defn.dest (FlexibleTycon.defn f) of
                Defn.Realized _ => Error.bug "Interface.expandTy: Realized"
              | Defn.TypeStr s => expandTy (TypeStr.apply (s, ts))
              | Defn.Undefined => Type.Con (c, ts))
       | Tycon.Rigid _ => Type.Con (c, ts)
   and expandTy (ty: Type.t): Type.t =
      Type.hom (ty, {con = con,
                     record = Type.Record,
                     var = Type.Var})
in
   val expandTy = expandTy
end

structure Type =
   struct
      open Type

      fun layoutPretty (ty, {expand, layoutPrettyEnvTycon, layoutPrettyFlexTycon, layoutPrettyTyvar}) =
         let
            fun con (c, ts) =
               Tycon.layoutAppPretty
               (c, ts, {layoutPrettyEnvTycon = layoutPrettyEnvTycon,
                        layoutPrettyFlexTycon = layoutPrettyFlexTycon})
            fun record r =
               case Record.detupleOpt r of
                  NONE =>
                     (LayoutPretty.simple o seq)
                     [str "{",
                      Layout.mayAlign
                      (Layout.separateRight
                       (Vector.toListMap
                        (Record.toVector r, fn (f, (t, _)) =>
                         seq [Record.Field.layout f, str ": ", t]),
                        ",")),
                      str "}"]
                | SOME ts => con (Tycon.tuple, ts)
            fun var a = LayoutPretty.simple (layoutPrettyTyvar a)
            val ty = if expand then expandTy ty else ty
         in
            Type.hom (ty, {con = con,
                           record = record,
                           var = var})
         end

      fun explainDoesNotAdmitEquality (ty, {layoutPrettyEnvTycon, layoutPrettyFlexTycon}) =
         let
            val layoutAppPretty = fn (c, ls) =>
               Tycon.layoutAppPretty
               (c, ls,
                {layoutPrettyEnvTycon = layoutPrettyEnvTycon,
                 layoutPrettyFlexTycon = layoutPrettyFlexTycon})
            val bracket = LayoutPretty.bracket
            val dontCare = LayoutPretty.dontCare
            fun getLay lo = Option.fold (lo, dontCare, #1)
            fun con (c, los) =
               case Tycon.admitsEquality c of
                  AdmitsEquality.Always => NONE
                | AdmitsEquality.Sometimes =>
                     if Vector.forall (los, Option.isNone)
                        then NONE
                        else (SOME o layoutAppPretty)
                             (c, Vector.map (los, getLay))
                | AdmitsEquality.Never =>
                     (SOME o bracket o layoutAppPretty)
                     (c, Vector.map (los, fn _ => dontCare))
            fun record r =
               case Record.detupleOpt r of
                  NONE =>
                     let
                        val v = Record.toVector r
                        val (fls, extra) =
                           Vector.fold
                           (v, ([], false), fn ((f, lo), (fls, extra)) =>
                            case lo of
                               NONE => (fls, true)
                             | SOME l => ((f,l)::fls, extra))
                     in
                        if List.isEmpty fls
                           then NONE
                           else (SOME o LayoutPretty.simple o seq)
                                [str "{",
                                 Layout.mayAlign
                                 (Layout.separateRight
                                  (List.revMap
                                   (fls, fn (f, (l, _)) =>
                                    seq [Record.Field.layout f,
                                         str ": ", l]),
                                   ",")),
                                 if extra
                                    then str ", ...}"
                                    else str "}"]
                     end
                | SOME los => con (Tycon.tuple, los)
            fun var _ = NONE
            val ty = expandTy ty
            val res = Type.hom (ty, {con = con,
                                     record = record,
                                     var = var})
         in
            Option.map (res, #1)
         end
   end

structure TypeStr =
   struct
      open TypeStr

      fun toTyconOpt (s, {expand}) =
         let
            val s = if expand then abs s else s
         in
            case node s of
               Datatype {tycon, ...} => SOME tycon
             | Scheme s =>
                  let
                     val Scheme.T {tyvars, ty} = s
                     val ty = if expand then expandTy ty else ty
                  in
                     case Type.deEta (ty, tyvars) of
                        NONE => NONE
                      | SOME c =>
                           if Tycon.equals (c, Tycon.arrow)
                              orelse Tycon.equals (c, Tycon.tuple)
                              then NONE
                              else SOME c
                  end
             | Tycon {tycon, ...} => SOME tycon
         end
   end

fun copyCons cons: Cons.t =
   Cons.map (cons, fn {scheme, ...} =>
             {scheme = copyScheme scheme})
and copyDefn (d: Defn.t): Defn.t =
   let
      open Defn
   in
      case dest d of
         Realized _ =>
            (* This will never happen in a type-correct program, but it may
             * in a type-incorrect one.  So, we return d to avoid terminating
             * MLton.
             *)
            d
       | TypeStr s => Defn.typeStr (copyTypeStr s)
       | Undefined => Defn.undefined
   end
and copyFlexibleTycon (fc: FlexibleTycon.t): FlexibleTycon.t =
   let
      open FlexibleTycon
      val {admitsEquality, copy, defn, hasCons,
           kind, prettyDefault, specs, ...} =
         fields fc
   in
      case !copy of
         NONE => 
            let
               val fc' = make {admitsEquality = !admitsEquality,
                               defn = copyDefn (!defn),
                               hasCons = hasCons,
                               kind = kind,
                               prettyDefault = prettyDefault,
                               specs = !specs}
               val _ = List.push (copies, copy)
               val _ = copy := SOME fc'
            in
               fc'
            end
       | SOME fc' => fc'
   end
and copyTycon (t: Tycon.t): Tycon.t =
   let
      open Tycon
   in
      case t of
         Flexible c => Flexible (copyFlexibleTycon c)
       | Rigid _ => t
   end
and copyType (t: Type.t): Type.t =
   let
      open Type
   in
      hom (t, {con = fn (c, ts) => Con (copyTycon c, ts),
               record = Record,
               var = Var})
   end
and copyScheme (Scheme.T {tyvars, ty}): Scheme.t =
   Scheme.T {ty = copyType ty, tyvars = tyvars}
and copyTypeStr (s: TypeStr.t): TypeStr.t =
   let
      open TypeStr
   in
      case node s of
         Datatype {cons, tycon, repl} => data (copyTycon tycon, copyCons cons, repl)
       | Scheme s => def (copyScheme s)
       | Tycon {eq, tycon = c} => tycon (copyTycon c, eq)
   end

structure AdmitsEquality =
   struct
      open AdmitsEquality

      fun fromBool b = if b then Sometimes else Never
   end

fun flexibleTyconAdmitsEquality (fc: FlexibleTycon.t): AdmitsEquality.t =
   let
      val {admitsEquality, defn, ...} = FlexibleTycon.fields fc
      datatype z = datatype Defn.dest
   in
      case Defn.dest (!defn) of
         Realized _ => !admitsEquality
       | TypeStr s => typeStrAdmitsEquality s
       | Undefined => !admitsEquality
   end
and schemeAdmitsEquality (s: Scheme.t): bool =
   let
      fun con (c, bs) =
         let
            datatype z = datatype AdmitsEquality.t
         in
            case Tycon.admitsEquality c of
               Always => true
             | Never => false
             | Sometimes => Vector.forall (bs, fn b => b)
         end
   in
      Type.hom (expandTy (Scheme.ty s),
                {con = con,
                 record = fn r => Record.forall (r, fn b => b),
                 var = fn _ => true})
   end
and tyconAdmitsEquality (t: Tycon.t): AdmitsEquality.t =
   let
      datatype z = datatype Tycon.t
   in
      case t of
         Flexible c => flexibleTyconAdmitsEquality c
       | Rigid e => Etycon.admitsEquality e
   end
and typeStrAdmitsEquality (s: TypeStr.t): AdmitsEquality.t =
   let
      datatype z = datatype TypeStr.node
   in
      case TypeStr.node s of
         Datatype {tycon = c, ...} => tyconAdmitsEquality c
       | Scheme s => AdmitsEquality.fromBool (schemeAdmitsEquality s)
       | Tycon {tycon = c, ...} => tyconAdmitsEquality c
   end

structure FlexibleTycon =
   struct
      open FlexibleTycon

      fun new {admitsEquality: AdmitsEquality.t,
               hasCons: bool, kind: Kind.t,
               prettyDefault: string,
               region: Region.t}: t =
         make {admitsEquality = admitsEquality,
               defn = Defn.undefined,
               hasCons = hasCons,
               kind = kind,
               prettyDefault = prettyDefault,
               specs = AppendList.single region}

      fun realize (fc, typeStr) =
         let
            val defn = defnRef fc
         in
            case Defn.dest (!defn) of
               Defn.Undefined => defn := Defn.realized typeStr
             | _ => Error.bug "Interface.FlexibleTycon.realize"
         end

      fun share (fc1 as T s1, fc2 as T s2, sharingSpec) =
         let
            val {admitsEquality = ae1, creationTime = t1,
                 hasCons = hc1, specs = ss1,
                 id, kind, plist, prettyDefault, ...} =
               fields fc1
            val {admitsEquality = ae2, creationTime = t2,
                 hasCons = hc2, specs = ss2, ...} =
               fields fc2
            val _ = Set.union (s1, s2)
            val specs =
               AppendList.snoc
               (if Ref.equals (ss1, ss2)
                   then !ss1
                   else AppendList.append (!ss1, !ss2),
                sharingSpec)
            val _ = 
               Set.:=
               (s1, {admitsEquality = ref (AdmitsEquality.or (!ae1, !ae2)),
                     copy = ref NONE,
                     creationTime = Time.min (t1, t2),
                     defn = ref Defn.undefined,
                     specs = ref specs,
                     hasCons = hc1 orelse hc2,
                     id = id,
                     kind = kind,
                     plist = plist,
                     prettyDefault = prettyDefault})
         in
            ()
         end

      type typeStr = TypeStr.t

      datatype realization =
          ETypeStr of EnvTypeStr.t
        | TypeStr of typeStr

      fun realization (f: t): realization option =
         case Defn.dest (defn f) of
            Defn.Realized s => SOME (ETypeStr s)
          | Defn.TypeStr s => SOME (TypeStr s)
          | Defn.Undefined => NONE
   end

structure Scheme =
   struct
      open Scheme

      val admitsEquality = schemeAdmitsEquality

      val copy = copyScheme
   end

structure TypeStr =
   struct
      open TypeStr

      val admitsEquality = typeStrAdmitsEquality

      val copy = copyTypeStr

      fun specs (s, first) =
         let
            fun loop s =
               case toTyconOpt (s, {expand = false}) of
                  NONE => AppendList.empty
                | SOME c => loopTycon c
            and loopTycon c =
               case c of
                  Tycon.Flexible fc =>
                     AppendList.append
                     (FlexibleTycon.specs fc,
                      case Defn.dest (FlexibleTycon.defn fc) of
                         Defn.Realized _ => AppendList.empty
                       | Defn.TypeStr s => loop s
                       | Defn.Undefined => AppendList.empty)
                | Tycon.Rigid _ => AppendList.empty
         in
            first ::
            (List.rev o AppendList.fold)
            (loop s, [], fn (r, rs) =>
             if List.contains (first::rs, r, Region.equals)
                then rs
                else r :: rs)
         end

      fun pushSpec (s, region) =
         case TypeStr.toTyconOpt (s, {expand = false}) of
            NONE => ()
          | SOME (Tycon.Flexible flex) =>
               FlexibleTycon.pushSpec (flex, region)
          | SOME (Tycon.Rigid _) => ()

      fun getFlex {layoutPrettyEnvTycon,
                   layoutPrettyFlexTycon,
                   oper: string,
                   time: Time.t,
                   ty = {name: unit -> Layout.t,
                         region: Region.t,
                         spec: Region.t,
                         tyStr: t}}: FlexibleTycon.t option =
         let
            fun error what =
               let
                  val isEmpty = Layout.isEmpty
                  val tuple = Layout.tuple
                  val {layoutPretty = layoutPrettyTyvar,
                       localInit = localInitLayoutPrettyTyvar, ...} =
                     Tyvar.makeLayoutPretty ()
                  val arity =
                     case kind tyStr of
                        Kind.Arity arity => arity
                      | _ => Error.bug "Interface.TypeStr.getFlex: Kind.Nary"
                  val tyvars =
                     Vector.tabulate
                     (arity, fn _ =>
                      Tyvar.makeNoname {equality = false})
                  val () = localInitLayoutPrettyTyvar tyvars
                  val tyargs = Vector.map (tyvars, Type.var)
                  val tyvars = Vector.map (tyvars, layoutPrettyTyvar)
                  val tyvars =
                     case Vector.length tyvars of
                        0 => empty
                      | 1 => Vector.first tyvars
                      | _ => tuple (Vector.toList tyvars)
                  val (kw, mkRest) =
                     case TypeStr.node tyStr of
                        TypeStr.Datatype _ =>
                           ("datatype", fn l =>
                            seq [str "... (* = datatype ", l, str " *)"])
                      | TypeStr.Scheme _ =>
                           ("type", fn l => l)
                      | TypeStr.Tycon _ =>
                           ("type", fn l => l)
                  val defn = TypeStr.apply (tyStr, tyargs)
                  val () =
                     Control.error
                     (region,
                      seq [str "type cannot be ", str oper,
                           str " (", str what, str "): ",
                           name ()],
                      align
                      ((seq [str "type spec: ",
                             str kw, str " ",
                             tyvars,
                             if isEmpty tyvars then empty else str " ",
                             name (), str " = ",
                             mkRest ((#1 o Type.layoutPretty)
                                     (defn, {expand = true,
                                             layoutPrettyEnvTycon = layoutPrettyEnvTycon,
                                             layoutPrettyFlexTycon = layoutPrettyFlexTycon,
                                             layoutPrettyTyvar = layoutPrettyTyvar}))])::
                       (List.map
                        (specs (tyStr, spec), fn r =>
                         seq [str "spec at: ", Region.layout r]))))
               in
                  NONE
               end
         in
            case toTyconOpt (tyStr, {expand = true}) of
               NONE => error "defined"
             | SOME c =>
                  (case c of
                      Tycon.Flexible c =>
                         let
                            val {creationTime, defn, ...} = FlexibleTycon.fields c
                         in
                            case Defn.dest (!defn) of
                               Defn.Realized _ =>
                                  Error.bug "Interface.TypeStr.getFlex: Realized"
                             | Defn.TypeStr _ =>
                                  Error.bug "Interface.TypeStr.getFlex: TypeStr"
                             | Defn.Undefined =>
                                  if Time.< (creationTime, time)
                                     then error "not local"
                                     else SOME c
                         end
                    | Tycon.Rigid _ => error "defined")
         end

      fun share {layoutPrettyEnvTycon, layoutPrettyFlexTycon,
                 region: Region.t, time: Time.t,
                 ty1 as {name = name1, ...},
                 ty2 as {name = name2, ...}} =
         case (getFlex {layoutPrettyEnvTycon = layoutPrettyEnvTycon,
                        layoutPrettyFlexTycon = layoutPrettyFlexTycon,
                        oper = "shared", time = time, ty = ty1},
               getFlex {layoutPrettyEnvTycon = layoutPrettyEnvTycon,
                        layoutPrettyFlexTycon = layoutPrettyFlexTycon,
                        oper = "shared", time = time, ty = ty2}) of
            (NONE, _) => ()
          | (_, NONE) => ()
          | (SOME flex1, SOME flex2) =>
               if Kind.equals (FlexibleTycon.kind flex1, FlexibleTycon.kind flex2)
                  then FlexibleTycon.share (flex1, flex2, region)
                  else let
                          fun msg {name, region = _, spec, tyStr} =
                             let
                                val (keyword, rest) =
                                   case node tyStr of
                                      Datatype _ => ("datatype", str " = ...")
                                    | Scheme _ => ("type", str " = ...")
                                    | Tycon {eq, ...} =>
                                         if eq
                                            then ("eqtype", empty)
                                            else ("type", empty)
                                val arity =
                                   case kind tyStr of
                                      Kind.Arity arity => arity
                                    | _ => Error.bug "Interface.TypeStr.share.msg: arity"
                                val tyvars =
                                   Vector.tabulate
                                   (arity, fn _ =>
                                    Tyvar.makeNoname {equality = false})
                                val {layoutPretty = layoutPrettyTyvar,
                                     localInit = localInitLayoutPrettyTyvar, ...} =
                                   Tyvar.makeLayoutPretty ()
                                val () = localInitLayoutPrettyTyvar tyvars
                                val tyvars =
                                   case Vector.length tyvars of
                                      0 => empty
                                    | 1 => layoutPrettyTyvar (Vector.first tyvars)
                                    | _ => Layout.tuple (Vector.toListMap (tyvars, layoutPrettyTyvar))
                             in
                                (seq  [str "type spec: ",
                                       str keyword,
                                       str " [", tyvars, str "] ",
                                       name (),
                                       rest])::
                                (List.map
                                 (specs (tyStr, spec), fn r =>
                                  seq [str "spec at: ", Region.layout r]))
                             end
                       in
                          Control.error
                          (region,
                           seq [str "types cannot be shared (arity): ",
                                name1 (), str ", ", name2 ()],
                           align ((msg ty1) @ (msg ty2)))
                       end

      val share =
         Trace.trace
         ("Interface.TypeStr.share",
          fn {time, ty1, ty2, ...} =>
          Layout.record [("time", Time.layout time),
                         ("ty1", Layout.record [("tyStr", layout (#tyStr ty1))]),
                         ("ty2", Layout.record [("tyStr", layout (#tyStr ty2))])],
          Unit.layout)
         share

      fun wheree {layoutPrettyEnvTycon, layoutPrettyFlexTycon,
                  region: Region.t, realization: t, time: Time.t,
                  ty as {name: unit -> Layout.t,
                         region = _: Region.t,
                         spec: Region.t,
                         tyStr: t}}: unit =
         case getFlex {layoutPrettyEnvTycon = layoutPrettyEnvTycon,
                       layoutPrettyFlexTycon = layoutPrettyFlexTycon,
                       oper = "realized", time = time, ty = ty} of
            NONE => ()
          | SOME flex =>
               let
                  val error: (Layout.t list * Layout.t * Layout.t) option ref = ref NONE
                  val addError = fn (msg, tyError, rlError) =>
                     let
                        val msgs =
                           case !error of
                              NONE => [str msg]
                            | SOME (msgs, _, _) => (str msg)::msgs
                     in
                        error := SOME (msgs, tyError, rlError)
                     end

                  val {layoutPretty = layoutPrettyTyvar,
                       localInit = localInitLayoutPrettyTyvar, ...} =
                     Tyvar.makeLayoutPretty ()
                  fun layoutPrettyType t =
                     Type.layoutPretty
                     (t, {expand = true,
                          layoutPrettyEnvTycon = layoutPrettyEnvTycon,
                          layoutPrettyFlexTycon = layoutPrettyFlexTycon,
                          layoutPrettyTyvar = layoutPrettyTyvar})

                  val tyKind = TypeStr.kind tyStr
                  val tyArity =
                     case tyKind of
                        Kind.Arity tyArity => tyArity
                      | _ => Error.bug "Interface.TypeStr.wheree: tyArity"
                  val rlKind = TypeStr.kind realization
                  val rlArity =
                     case rlKind of
                        Kind.Arity rlArity => rlArity
                      | _ => Error.bug "Interface.TypeStr.wheree: rlArity"
                  local
                     val tyvars =
                        Vector.tabulate
                        (Int.max (tyArity, rlArity), fn _ =>
                         Tyvar.makeNoname {equality = false})
                     val () = localInitLayoutPrettyTyvar tyvars
                  in
                     val tyTyvars = Vector.prefix (tyvars, tyArity)
                     val tyTyargs = Vector.map (tyTyvars, Type.var)
                     val rlTyvars = Vector.prefix (tyvars, rlArity)
                     val rlTyargs = Vector.map (rlTyvars, Type.var)
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
                           if tyArity = rlArity
                              then tyvars
                              else bracket tyvars
                     in
                        if isEmpty tyvars
                           then str " "
                           else seq [str " ", tyvars, str " "]
                     end

                  fun tyMsg (b, rest) =
                     let
                        val empty = Layout.empty
                        val defn =
                           seq [str " = ",
                                case rest of
                                   NONE => str "..."
                                 | SOME rest => rest]
                        val (kw, defn) =
                           case TypeStr.node tyStr of
                              Datatype _ => ("datatype", defn)
                            | Scheme _ => ("type", defn)
                            | Tycon {eq, ...} =>
                                 (case rest of
                                     NONE =>
                                        (if eq then "eqtype" else "type",
                                         empty)
                                   | SOME _ =>
                                        ("type", defn))
                     in
                        seq [if b then bracket (str kw) else str kw,
                             layoutTyvars tyTyvars,
                             name (),
                             defn]
                     end
                  fun rlMsg (b, rest) =
                     let
                        val defn =
                           seq [str " = ",
                                case rest of
                                   NONE => str "..."
                                 | SOME rest => rest]
                        val kw = "type"
                     in
                        seq [if b then bracket (str kw) else str kw,
                             layoutTyvars rlTyvars,
                             name (),
                             defn]
                     end

                  val () =
                     if Kind.equals (tyKind, rlKind)
                        then ()
                        else addError ("arity",
                                       tyMsg (false, NONE),
                                       rlMsg (false, NONE))
                  val () =
                     if FlexibleTycon.hasCons flex
                        andalso
                        Option.isNone (TypeStr.toTyconOpt
                                       (realization, {expand = true}))
                        then let
                                fun tyDefn () =
                                   (SOME o bracket o #1 o layoutPrettyType)
                                   (TypeStr.apply (tyStr, tyTyargs))
                                val (tyKwErr, tyDefn) =
                                   case TypeStr.node tyStr of
                                      Datatype _ => (true, NONE)
                                    | Scheme _ => (false, tyDefn ())
                                    | Tycon _ => (false, tyDefn ())
                                val rlDefn =
                                   (SOME o bracket o #1 o layoutPrettyType)
                                   (TypeStr.apply (realization, rlTyargs))
                             in
                                addError ("type structure",
                                          tyMsg (tyKwErr, tyDefn),
                                          rlMsg (false, rlDefn))
                             end
                        else let
                                val tyAdmitsEquality = admitsEquality (#tyStr ty)
                                val rlAdmitsEquality = admitsEquality realization
                             in
                                if AdmitsEquality.<= (tyAdmitsEquality, rlAdmitsEquality)
                                   then ()
                                   else let
                                           fun tyDefn () =
                                              (SOME o bracket o #1 o layoutPrettyType)
                                              (TypeStr.apply (tyStr, tyTyargs))
                                           val (tyKwErr, tyDefn) =
                                              case TypeStr.node tyStr of
                                                 Datatype _ => (false, NONE)
                                               | Scheme _ => (false, tyDefn ())
                                               | Tycon {eq, ...} =>
                                                    if eq
                                                       then (true, NONE)
                                                       else (false, tyDefn ())
                                           val rlDefn =
                                              Type.explainDoesNotAdmitEquality
                                              (TypeStr.apply (realization, rlTyargs),
                                               {layoutPrettyEnvTycon = layoutPrettyEnvTycon,
                                                layoutPrettyFlexTycon = layoutPrettyFlexTycon})
                                        in
                                           addError ("admits equality",
                                                     tyMsg (tyKwErr, tyDefn),
                                                     rlMsg (false, rlDefn))
                                        end
                             end
               in
                  case !error of
                     NONE =>
                        (FlexibleTycon.defnRef flex := Defn.typeStr realization
                         ; FlexibleTycon.pushSpec (flex, region)
                         ; pushSpec (realization, region))
                   | SOME (msgs, tyError, rlError) =>
                        Control.error
                        (region,
                         seq [str "type cannot be realized (",
                              (seq o List.separate) (List.rev msgs, str ", "),
                              str "): ",
                              name ()],
                         align ((seq [str "type spec: ", tyError]) ::
                                (List.map
                                 (specs (tyStr, spec), fn r =>
                                  seq [str "spec at: ", Region.layout r])) @
                                [seq [str "type defn: ", rlError]]))
               end

      val wheree =
         Trace.trace ("Interface.TypeStr.wheree",
                      fn {realization, time, ty, ...} =>
                      Layout.record [("realization", layout realization),
                                     ("time", Time.layout time),
                                     ("ty", Layout.record [("tyStr", layout (#tyStr ty))])],
                      Unit.layout)
         wheree
   end

structure UniqueId = IntUniqueId ()

   structure TyconMap =
   struct
      datatype 'a t = T of {strs: (Strid.t * 'a t) array,
                            types: (Ast.Tycon.t * 'a) array}

      fun layout layoutA =
         let
            open Layout
            fun loop (T {strs, types}) =
               record [("strs",
                        Array.layout (Layout.tuple2 (Strid.layout, loop)) strs),
                       ("types",
                        Array.layout (Layout.tuple2 (Ast.Tycon.layout, layoutA))
                        types)]
         in
            loop
         end

      fun empty (): 'a t = T {strs = Array.new0 (),
                              types = Array.new0 ()}

      fun isEmpty (T {strs, types}) =
         0 = Array.length strs andalso 0 = Array.length types

      fun peekStrid (T {strs, ...}, strid) =
         Array.peekMap (strs, fn (strid', z) =>
                        if Strid.equals (strid, strid')
                           then SOME z
                           else NONE)

      fun peekTycon (T {types, ...}, tycon) =
         Array.peekMap (types, fn (tycon', z) =>
                        if Ast.Tycon.equals (tycon, tycon')
                           then SOME z
                           else NONE)

   end

(*---------------------------------------------------*)
(*                   Main Datatype                   *)
(*---------------------------------------------------*)

datatype t = T of {copy: copy,
                   flexible: FlexibleTycon.t TyconMap.t option ref,
                   isClosed: bool,
                   original: t option,
                   plist: PropertyList.t,
                   strs: (Strid.t * t) array,
                   types: (Ast.Tycon.t * TypeStr.t) array,
                   uniqueId: UniqueId.t,
                   vals: (Ast.Vid.t * (Status.t * Scheme.t)) array} Set.t
withtype copy = t option ref

fun dest (T s) = Set.! s

local
   fun make f = f o dest
in
   val plist = make #plist
end

fun original I =
   case #original (dest I) of
      NONE => I
    | SOME I => I

fun new {isClosed, original, strs, types, vals} =
   T (Set.singleton {copy = ref NONE,
                     flexible = ref NONE,
                     isClosed = isClosed,
                     original = original,
                     plist = PropertyList.new (),
                     strs = strs,
                     types = types,
                     uniqueId = UniqueId.new (),
                     vals = vals})

val empty = new {isClosed = true,
                 original = NONE,
                 strs = Array.new0 (),
                 types = Array.new0 (),
                 vals = Array.new0 ()}

local
   open Layout
in
   fun layout (T s) =
      let
         val {strs, types, uniqueId = u, vals, ...} = Set.! s
      in
         record [("uniqueId", UniqueId.layout u),
                 ("strs",
                  Array.layout (Layout.tuple2 (Strid.layout, layout)) strs),
                 ("types",
                  Array.layout (Layout.tuple2 (Ast.Tycon.layout, TypeStr.layout))
                  types),
                 ("vals",
                  Array.layout (Layout.tuple2 (Vid.layout,
                                               Layout.tuple2 (Status.layout,
                                                              Scheme.layout)))
                  vals)]
      end
end

fun equals (T s, T s') = Set.equals (s, s')

val equals =
   Trace.trace2 ("Interface.equals", layout, layout, Bool.layout) equals

fun sameShape (I, I') =
   case (#original (dest I), #original (dest I')) of
      (SOME I, SOME I') => equals (I, I')
    | _ => false

fun peekStrid (T s, strid: Strid.t): t option =
   let
      val {strs, ...} = Set.! s
   in
      Array.peekMap (strs, fn (strid', I) =>
                     if Strid.equals (strid, strid')
                        then SOME I
                     else NONE)
   end

datatype 'a peekResult =
   Found of 'a
  | UndefinedStructure of Strid.t list

fun peekStrids (I: t, strids: Strid.t list): t peekResult =
   let
      fun loop (I, strids, ac) =
         case strids of
            [] => Found I
          | strid :: strids =>
               case peekStrid (I, strid) of
                  NONE => UndefinedStructure (rev (strid :: ac))
                | SOME I => loop (I, strids, strid :: ac)
   in
      loop (I, strids, [])
   end

fun peekTycon (T s, tycon: Ast.Tycon.t): (Ast.Tycon.t * TypeStr.t) option =
   let
      val {types, ...} = Set.! s
   in
      Array.peekMap (types, fn (name, typeStr) =>
                     if Ast.Tycon.equals (tycon, name)
                        then SOME (name, typeStr)
                     else NONE)
   end

fun unbound (r: Region.t, className, x: Layout.t): unit =
   Control.error
   (r,
    let open Layout
    in seq [str "undefined ", str className, str " ", x]
    end,
    Layout.empty)

fun layoutStrids (ss: Strid.t list): Layout.t =
  Layout.str (concat (List.separate (List.map (ss, Strid.toString), ".")))

fun lookupLongtycon (I: t, long: Longtycon.t, r: Region.t,
                     {prefix: Strid.t list}) =
   let
      val (ss, c) = Longtycon.split long
   in
      case peekStrids (I, ss) of
         Found I =>
            (case peekTycon (I, c) of
                NONE => 
                   (unbound (r, "type",
                             Longtycon.layout (Longtycon.long (prefix @ ss, c)))
                    ; NONE)
              | SOME (name, s) => SOME (name, s))
       | UndefinedStructure ss =>
            (unbound (r, "structure", layoutStrids (prefix @ ss))
             ; NONE)
   end

fun lookupLongstrid (I: t, long: Longstrid.t, r: Region.t,
                     {prefix: Strid.t list}) =
   let
      val (ss, s) = Longstrid.split long
   in
      case peekStrids (I, ss) of
         Found I =>
            (case peekStrid (I, s) of
                NONE =>
                   (unbound (r, "structure",
                             Longstrid.layout (Longstrid.long (prefix @ ss, s)))
                    ; NONE)
              | SOME I => SOME I)
       | UndefinedStructure ss =>
            (unbound (r, "structure", layoutStrids (prefix @ ss))
             ; NONE)
   end

fun share {layoutPrettyEnvTycon, layoutPrettyFlexTycon,
           I1: t, long1: Longstrid.t, I2: t, long2: Longstrid.t,
           time, region}: unit =
   let
      fun mkTy (s, long, strids, name) =
         let
            val spec = Ast.Tycon.region name
            val region = Longstrid.region long
            val name = fn () =>
               let
                  val (ss, s) = Longstrid.split long
               in
                  Ast.Longtycon.layout
                  (Ast.Longtycon.long (List.concat [ss, [s], rev strids],
                                       name))
               end
         in
            {name = name,
             region = region,
             spec = spec,
             tyStr = s}
         end
      fun ensureFlexible (I: t, strids): unit =
         let
            val {get: t -> bool ref, destroy, ...} =
               Property.destGet (plist, Property.initFun (fn _ => ref false))
            fun loop (I: t, strids): unit =
               let
                  val r = get I
               in
                  if !r
                     then ()
                  else
                     let
                        val _ = r := true
                        val T s = I
                        val {strs, types, ...} = Set.! s
                        val _ =
                           Array.foreach
                           (strs, fn (strid, I) =>
                            ensureFlexible (I, strid :: strids))
                        val _ =
                           Array.foreach
                           (types, fn (name, s) =>
                            (ignore o TypeStr.getFlex)
                            {layoutPrettyEnvTycon = layoutPrettyEnvTycon,
                             layoutPrettyFlexTycon = layoutPrettyFlexTycon,
                             oper = "shared",
                             time = time,
                             ty = mkTy (s, long1, strids, name)})
                     in
                        ()
                     end
               end
            val () = loop (I, strids)
            val _ = destroy ()
         in
            ()
         end
      fun share (I1, I2, strids): unit =
         if equals (I1, I2)
            then ensureFlexible (I1, strids)
         else if sameShape (I1, I2)
            then
               let
                  fun loop (T s1, T s2, strids): unit =
                     let
                        val {isClosed, strs = strs1, types = types1, ...} = Set.! s1
                        val {strs = strs2, types = types2, ...} = Set.! s2
                        val _ =
                           Array.foreach2
                           (types1, types2, fn ((name, s1), (_, s2)) =>
                            TypeStr.share
                            {layoutPrettyEnvTycon = layoutPrettyEnvTycon,
                             layoutPrettyFlexTycon = layoutPrettyFlexTycon,
                             region = region,
                             time = time,
                             ty1 = mkTy (s1, long1, strids, name),
                             ty2 = mkTy (s2, long2, strids, name)})
                        val _ =
                           Array.foreach2
                           (strs1, strs2, fn ((name, I1), (_, I2)) =>
                            loop (I1, I2, name :: strids))
                        val _ =
                           (* Can't always union here.  I1 and I2 may have
                            * exactly the same shape, but may have free
                            * flxible tycons defined in other signatures that
                            * are different.
                            * However, if the interface is closed, that is, if
                            * all of the flexible tycons that appear in it are
                            * also defined in it, then sharing the structures
                            * implies that the structures are identical.  This
                            * also relies on the fact that the structures have
                            * the same shape, which means that they are copies
                            * of the same interface.  That is sufficient to
                            * guarantee that all rigid tycons are identical.
                            *)
                           if isClosed
                              then Set.union (s1, s2)
                              else ()
                     in
                        ()
                     end
               in
                  loop (I1, I2, strids)
               end
         else (* different shapes -- need to share pointwise *)
            let
               val T s1 = I1
               val T s2 = I2
               val {strs = strs1, types = types1, ...} = Set.! s1
               val {strs = strs2, types = types2, ...} = Set.! s2
               fun walk2 (a1, a2, compareNames, f: 'a * 'a * 'b -> unit) =
                  let
                     val n1 = Array.length a1
                     val n2 = Array.length a2
                     fun both (i1, i2) =
                        if i1 < n1 andalso i2 < n2
                           then compare (i1, Array.sub (a1, i1),
                                         i2, Array.sub (a2, i2))
                        else ()
                     and compare (i1, (name1, z1), i2, (name2, z2)) =
                        case compareNames (name1, name2) of
                           GREATER =>
                              let
                                 val i2 = i2 + 1
                              in
                                 if i2 < n2
                                    then compare (i1, (name1, z1),
                                                  i2, Array.sub (a2, i2))
                                 else ()
                              end
                         | EQUAL => (f (z1, z2, name1)
                                     ; both (i1 + 1, i2 + 1))
                         | LESS =>
                              let
                                 val i1 = i1 + 1
                              in
                                 if i1 < n1
                                    then compare (i1, Array.sub (a1, i1),
                                                  i2, (name2, z2))
                                 else ()
                              end
                  in
                     both (0, 0)
                  end
               val _ =
                  walk2 (strs1, strs2, Strid.compare,
                         fn (I1, I2, name) => share (I1, I2, name :: strids))
               val _ =
                  walk2 (types1, types2, Ast.Tycon.compare,
                         fn (s1, s2, name) =>
                         TypeStr.share
                         {layoutPrettyEnvTycon = layoutPrettyEnvTycon,
                          layoutPrettyFlexTycon = layoutPrettyFlexTycon,
                          region = region,
                          time = time,
                          ty1 = mkTy (s1, long1, strids, name),
                          ty2 = mkTy (s2, long2, strids, name)})
            in
               ()
            end
   in
      share (I1, I2, [])
   end

val share =
   Trace.trace
   ("Interface.share",
    fn {I1, I2, time, ...} =>
    Layout.tuple [layout I1, layout I2, Time.layout time],
    Unit.layout)
   share

fun copy (I: t): t =
   let
      (* Keep track of all nodes that have forward pointers to copies, so
       * that we can gc them when done.
       *)
      val copies: copy list ref = ref []
      fun loop (I as T s): t =
         let
            val r as {copy, ...} = Set.! s
         in
            case !copy of
               NONE =>
                  let
                     val {isClosed, original, strs, types, vals, ...} = r
                     val types =
                        Array.map (types, fn (name, typeStr) =>
                                   (name, TypeStr.copy typeStr))
                     val vals =
                        Array.map (vals, fn (name, (status, scheme)) =>
                                   (name, (status, Scheme.copy scheme)))
                     val strs =
                        Array.map (strs, fn (name, I) => (name, loop I))
                     val original =
                        SOME (case original of
                                 NONE => I
                               | SOME I => I)
                     val I = T (Set.singleton {copy = ref NONE,
                                               flexible = ref NONE,
                                               isClosed = isClosed,
                                               original = original,
                                               plist = PropertyList.new (),
                                               strs = strs,
                                               types = types,
                                               uniqueId = UniqueId.new (),
                                               vals = vals})
                     val _ = List.push (copies, copy)
                     val _ = copy := SOME I
                  in
                     I
                  end
             | SOME I => I
         end
      val I = loop I
      fun clear copies = List.foreach (!copies, fn copy => copy := NONE)
      val _ = clear copies
      val _ = clear FlexibleTycon.copies
      val _ = FlexibleTycon.copies := []
   in
      I
   end

val copy = Trace.trace ("Interface.copy", layout, layout) copy

fun flexibleTycons (I: t): FlexibleTycon.t TyconMap.t =
   let
      val {destroy = destroy1,
           get = tyconShortest: (FlexibleTycon.t
                                 -> {flex: FlexibleTycon.t option ref,
                                     length: int option} ref), ...} =
         Property.destGet (FlexibleTycon.plist,
                           Property.initFun (fn _ => ref {flex = ref NONE,
                                                          length = NONE}))
      val {destroy = destroy2,
           get = interfaceShortest: t -> int option ref, ...} =
         Property.destGet (plist, Property.initFun (fn _ => ref NONE))
      fun loop (I: t, length: int): FlexibleTycon.t option ref TyconMap.t =
         let
            val r = interfaceShortest I
         in
            if isSome (!r) andalso length >= valOf (!r)
               then TyconMap.empty ()
            else
               let
                  val _ = r := SOME length
                  val {strs, types, ...} = dest I
                  fun setTycon (tycon, isDatatype, isEqtype) =
                     case tycon of
                        Tycon.Flexible fc =>
                           let
                              val {admitsEquality, defn, hasCons, ...} = FlexibleTycon.fields fc
                              val admitsEquality =
                                 case !admitsEquality of
                                    AdmitsEquality.Always => true
                                  | AdmitsEquality.Never => false
                                  | AdmitsEquality.Sometimes => true
                           in
                              case Defn.dest (!defn) of
                                 Defn.Undefined =>
                                    let
                                       val r = tyconShortest fc
                                    in
                                       if (hasCons
                                           andalso not isDatatype)
                                          orelse
                                          (admitsEquality
                                           andalso not isEqtype)
                                          orelse
                                          (isSome (#length (!r))
                                           andalso length >= valOf (#length (!r)))
                                          then ref NONE
                                          else let
                                                  val _ = #flex (!r) := NONE
                                                  val flex = ref (SOME fc)
                                                  val _ = r := {flex = flex,
                                                                length = SOME length}
                                               in
                                                  flex
                                               end
                                    end
                               | _ => ref NONE
                           end
                    | _ => ref NONE
                  val types =
                     Array.map
                     (types, fn (tycon, typeStr) =>
                      (tycon,
                       case TypeStr.node typeStr of
                          TypeStr.Datatype {tycon, repl, ...} =>
                             setTycon (tycon, not repl, true)
                        | TypeStr.Tycon {eq, tycon, ...} =>
                             setTycon (tycon, false, eq)
                        | _ => ref NONE))
                  val strs =
                     Array.map
                     (strs, fn (s, I) =>
                      (s, loop (I, 1 + length)))
               in
                  TyconMap.T {strs = strs, types = types}
               end
         end
      val tm = loop (I, 0)
      val _ = (destroy1 (); destroy2 ())
      fun collapse (tm: FlexibleTycon.t option ref TyconMap.t)
         : FlexibleTycon.t TyconMap.t =
         let
            val TyconMap.T {strs, types} = tm
            val types = Array.keepAllMap (types, fn (c, r) =>
                                          Option.map (!r, fn f => (c, f)))
            val strs = Array.keepAllMap (strs, fn (s, m) =>
                                         let
                                            val m = collapse m
                                         in
                                            if TyconMap.isEmpty m
                                               then NONE
                                            else SOME (s, m)
                                         end)
         in
            TyconMap.T {strs = strs, types = types}
         end
   in
      collapse tm
   end

val flexibleTycons =
   fn I as T s =>
   let
      val {flexible, ...} = Set.! s
   in
      case !flexible of
         NONE =>
            let
               val f = flexibleTycons I
               val _ = flexible := SOME f
            in
               f
            end
       | SOME f => f
   end

val flexibleTycons =
   Trace.trace ("Interface.flexibleTycons", layout,
                TyconMap.layout FlexibleTycon.layout)
   flexibleTycons

fun dest (T s) =
   let
      val {strs, types, vals, ...} = Set.! s
   in
      {strs = strs,
       types = types,
       vals = vals}
   end

end
