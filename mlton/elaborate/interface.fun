(* Copyright (C) 2009,2015,2017 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
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
   structure Tyvar = Tyvar
   structure Vid = Vid
end

structure Field = Record.Field

structure EtypeStr = EnvTypeStr
local
   open EtypeStr
in
   structure AdmitsEquality = AdmitsEquality
   structure Kind = Kind
   structure Etycon = Tycon
end

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
                         specs: Region.t list ref} Set.t
      withtype copy = t option ref

      fun fields (T s) = Set.! s

      local
         fun make f = f o fields
      in
         val admitsEquality = make #admitsEquality
         val defnRef = make #defn
         val defn = ! o defnRef
         val hasCons = make #hasCons
         val kind = make #kind
         val plist = make #plist
         val specsRef = make #specs
         val specs = ! o specsRef
      end

      fun dest fc =
         let
            val {admitsEquality, hasCons, kind, ...} = fields fc
         in
            {admitsEquality = !admitsEquality,
             hasCons = hasCons,
             kind = kind}
         end

      val equals = fn (T s, T s') => Set.equals (s, s')

      fun layout fc =
         let
            open Layout
            val {admitsEquality, creationTime, hasCons, id, kind, ...} = fields fc
         in
            record [("admitsEquality", AdmitsEquality.layout (!admitsEquality)),
                    ("creationTime", Time.layout creationTime),
                    ("hasCons", Bool.layout hasCons),
                    ("id", TyconId.layout id),
                    ("kind", Kind.layout kind)]
         end

      fun layoutApp (t, _) =
          (layout t, ({isChar = false}, Etycon.BindingStrength.unit))

      val copies: copy list ref = ref []

      fun make {admitsEquality: AdmitsEquality.t, defn: Defn.t,
                hasCons: bool, kind: Kind.t, specs: Region.t list}: t =
         T (Set.singleton {admitsEquality = ref admitsEquality,
                           copy = ref NONE,
                           creationTime = Time.current (),
                           defn = ref defn,
                           hasCons = hasCons,
                           id = TyconId.new (),
                           kind = kind,
                           plist = PropertyList.new (),
                           specs = ref specs})

      fun new {defn: Defn.t, hasCons: bool, kind: Kind.t}: t =
         make {admitsEquality = AdmitsEquality.Sometimes,
               defn = defn, hasCons = hasCons, kind = kind,
               specs = []}
   end

structure Tycon =
   struct
      datatype t =
         Flexible of FlexibleTycon.t
       | Rigid of Etycon.t * Kind.t

      val fromEnv: Etycon.t * Kind.t -> t = Rigid

      fun admitsEquality c =
         case c of
            Flexible f => FlexibleTycon.admitsEquality f
          | Rigid (e, _) => Etycon.admitsEquality e

      val arrow = fromEnv (Etycon.arrow, Kind.Arity 2)

      val equals =
         fn (Flexible f, Flexible f') => FlexibleTycon.equals (f, f')
          | (Rigid (c, _), Rigid (c', _)) => Etycon.equals (c, c')
          | _ => false

      val exn = Rigid (Etycon.exn, Kind.Arity 0)

      val layout =
         fn Flexible c => FlexibleTycon.layout c
          | Rigid (c, _) => Etycon.layout c

      fun layoutApp (t: t, v) =
         case t of
            Flexible f => FlexibleTycon.layoutApp (f, v)
          | Rigid (c, _) => Etycon.layoutAppPretty (c, v)

      val tuple = Rigid (Etycon.tuple, Kind.Nary)
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
         fun simple l = (l, ({isChar = false}, Etycon.BindingStrength.unit))
         fun loop t =
            case t of
               Con (c, ts) => Tycon.layoutApp (c, Vector.map (ts, loop))
             | Record r =>
                  (case Record.detupleOpt r of
                      NONE =>
                         simple
                         (seq
                          [str "{",
                           mayAlign
                           (separateRight
                            (Vector.toListMap
                             (QuickSort.sortVector
                              (Record.toVector r, fn ((f, _), (f', _)) =>
                               Field.<= (f, f')),
                              fn (f, t) =>
                              seq [Field.layout f, str ": ", #1 (loop t)]),
                             ",")),
                           str "}"])
                    | SOME ts => Tycon.layoutApp (Tycon.tuple,
                                                  Vector.map (ts, loop)))
             | Var a => simple (Tyvar.layout a)
      in
         val layout = #1 o loop
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

      fun make (tyvars, ty) = T {ty = ty, tyvars = tyvars}
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
         Datatype of {cons: Cons.t,
                      tycon: Tycon.t}
       | Scheme of Scheme.t
       | Tycon of Tycon.t

      datatype t = T of {kind: Kind.t,
                         node: node}

      local
         fun make f (T r) = f r
      in
         val kind = make #kind
         val node = make #node
      end

      fun layout t =
         let
            open Layout
         in
            case node t of
               Datatype {tycon, cons} =>
                  seq [str "Datatype ",
                       record [("tycon", Tycon.layout tycon),
                               ("cons", Cons.layout cons)]]
             | Scheme s => Scheme.layout s
             | Tycon t => seq [str "Tycon ", Tycon.layout t]
         end

      fun apply (t: t, tys: Type.t vector): Type.t =
         case node t of
            Datatype {tycon, ...} => Type.con (tycon, tys)
          | Scheme s => Scheme.apply (s, tys)
          | Tycon t => Type.con (t, tys)

      val apply =
         Trace.trace ("Interface.TypeStr.apply", Layout.tuple2 (layout, Vector.layout Type.layout), Type.layout)
         apply

      fun cons t =
         case node t of
            Datatype {cons, ...} => cons
          | _ => Cons.empty

      fun data (tycon, kind, cons) =
         T {kind = kind,
            node = Datatype {tycon = tycon, cons = cons}}

      fun def (s: Scheme.t, k: Kind.t) =
         T {kind = k,
            node = Scheme s}

      fun tycon (c, kind) = T {kind = kind,
                               node = Tycon c}
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

structure TypeStr =
   struct
      open TypeStr

      fun toTyconOpt s =
         case node s of
            Datatype {tycon, ...} => SOME tycon
          | Scheme (Scheme.T {tyvars, ty}) =>
               (case Type.deEta (expandTy ty, tyvars) of
                   NONE => NONE
                 | SOME c => SOME c)
          | Tycon c => SOME c
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
      val {admitsEquality, copy, defn, hasCons, kind, specs, ...} =
         fields fc
   in
      case !copy of
         NONE => 
            let
               val fc' = make {admitsEquality = !admitsEquality,
                               defn = copyDefn (!defn),
                               hasCons = hasCons,
                               kind = kind,
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
      val kind = kind s
   in
      case node s of
         Datatype {cons, tycon} => data (copyTycon tycon, kind, copyCons cons)
       | Scheme s => def (copyScheme s, kind)
       | Tycon c => tycon (copyTycon c, kind)
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
         Realized _ => Error.bug "Interface.flexibleTyconAdmitsEquality: Realized"
       | TypeStr s => typeStrAdmitsEquality s
       | Undefined => !admitsEquality
   end
and schemeAdmitsEquality (s: Scheme.t): bool =
   let
      fun con (c, bs) =
         let
            datatype z = datatype AdmitsEquality.t
         in
            case ! (Tycon.admitsEquality c) of
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
       | Rigid (e, _) => ! (Etycon.admitsEquality e)
   end
and typeStrAdmitsEquality (s: TypeStr.t): AdmitsEquality.t =
   let
      datatype z = datatype TypeStr.node
   in
      case TypeStr.node s of
         Datatype {tycon = c, ...} => tyconAdmitsEquality c
       | Scheme s => AdmitsEquality.fromBool (schemeAdmitsEquality s)
       | Tycon c => tyconAdmitsEquality c
   end

structure FlexibleTycon =
   struct
      open FlexibleTycon

      fun realize (fc, typeStr) =
         let
            val defn = defnRef fc
         in
            case Defn.dest (!defn) of
               Defn.Undefined => defn := Defn.realized typeStr
             | _ => Error.bug "Interface.FlexibleTycon.realize"
         end

      fun share (fc as T s, fc' as T s', sharingSpec) =
         let
            val {admitsEquality = a, creationTime = t,
                 hasCons = h, specs = ss,
                 id, kind, plist, ...} =
               fields fc
            val {admitsEquality = a', creationTime = t',
                 hasCons = h', specs = ss', ...} =
               fields fc'
            val _ = Set.union (s, s')
            val specs =
               (List.rev o List.removeDuplicates)
               (sharingSpec :: (!ss @ !ss'),
                Region.equals)
            val _ = 
               Set.:=
               (s, {admitsEquality = ref (AdmitsEquality.or (!a, !a')),
                    copy = ref NONE,
                    creationTime = Time.min (t, t'),
                    defn = ref Defn.undefined,
                    specs = ref specs,
                    hasCons = h orelse h',
                    id = id,
                    kind = kind,
                    plist = plist})
         in
            ()
         end

      type typeStr = TypeStr.t

      datatype realization =
          ETypeStr of EnvTypeStr.t
        | TypeStr of typeStr

      fun realization (f: t): realization =
         case Defn.dest (defn f) of
            Defn.Realized s => ETypeStr s
          | Defn.TypeStr s => TypeStr s
          | _ => Error.bug "Interface.FlexibleTycon.realization"
   end

structure Tycon =
   struct
      open Tycon

      fun make {hasCons, kind} =
         Flexible (FlexibleTycon.new {defn = Defn.undefined,
                                      hasCons = hasCons,
                                      kind = kind})
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


      fun specs s =
         let
            fun specs c =
               case c of
                  Tycon.Flexible fc =>
                     List.rev (FlexibleTycon.specs fc)
                | _ => []
         in
            case node s of
               Datatype {tycon, ...} => specs tycon
             | Scheme _ => []
             | Tycon tycon => specs tycon
         end

      fun mkErrorExtra ({lay, region = _, spec, tyStr},
                        keywordErr, arityErr, defnErr) =
         let
            val defn =
               if defnErr
                  then str " = [...]"
                  else str " = ..."
            val (keyword, rest) =
               case node tyStr of
                  Datatype _ => ("datatype", defn)
                | Scheme _ => ("type", defn)
                | Tycon c =>
                     (case c of
                         Tycon.Flexible c =>
                            (case Defn.dest (FlexibleTycon.defn c) of
                                Defn.Realized _ =>
                                   Error.bug "Interface.TypeStr.mkErrorExtra: Defn.Realized"
                              | Defn.TypeStr _ =>
                                   if keywordErr andalso FlexibleTycon.hasCons c
                                      then ("datatype", defn)
                                      else ("type", defn)
                              | Defn.Undefined =>
                                   (case admitsEquality tyStr of
                                       AdmitsEquality.Always => "eqtype"
                                     | AdmitsEquality.Never => "type"
                                     | AdmitsEquality.Sometimes => "eqtype",
                                    empty))
                       | Tycon.Rigid _ => ("type", defn))
            val keyword =
               if keywordErr then bracket (str keyword) else str keyword
            val arity =
               case kind tyStr of
                  Kind.Arity arity => arity
                | _ => Error.bug "Interface.TypeStr.mkErrorExtra: Kind.Nary"
            val {destroy, lay = layTyvar} = Tyvar.makeLayoutPretty ()
            val tyvars =
               case arity of
                  0 => empty
                | 1 => layTyvar (Tyvar.newNoname {equality = false})
                | _ => (* Ensure tyvars get correct pretty names. *)
                       (Layout.tuple o List.rev o List.tabulate)
                       (arity, fn _ => layTyvar (Tyvar.newNoname {equality = false}))
            val _ = destroy ()
            val tyvars =
               if arityErr
                  then bracket tyvars
                  else tyvars
            val tyvars =
               if Layout.isEmpty tyvars
                  then str " "
                  else seq [str " ", tyvars, str " "]
         in
            (seq  [str "type spec: ", keyword, tyvars, lay (), rest])::
            (List.map
             (spec::(specs tyStr), fn r =>
              seq [str "spec at:   ", Region.layout r]))
         end

      fun getFlex {oper: string,
                   time: Time.t,
                   ty as {lay: unit -> Layout.t,
                          region: Region.t,
                          spec = _: Region.t,
                          tyStr: t}}: FlexibleTycon.t option =
         let
            fun error (what, defnErr) =
               let
                  val _ = 
                     Control.error
                     (region,
                      seq [str "type cannot be ",
                           str oper,
                           str " (",
                           str what,
                           str "): ",
                           lay ()],
                      align (mkErrorExtra (ty, false, false, defnErr)))
               in
                  NONE
               end
            fun loop (s: t): FlexibleTycon.t option =
               (case toTyconOpt s of
                   SOME c => loopTycon c
                 | NONE => error ("defined", true))
            and loopTycon (c: Tycon.t): FlexibleTycon.t option =
               case c of
                  Tycon.Flexible c =>
                     let
                        val {creationTime, defn, ...} = FlexibleTycon.fields c
                     in
                        case Defn.dest (!defn) of
                           Defn.Realized _ => 
                              Error.bug "Interface.TypeStr.loopTycon: Realized"
                         | Defn.TypeStr s => loop s
                         | Defn.Undefined =>
                              if Time.< (creationTime, time)
                                 then error ("not local", false)
                              else SOME c
                     end
                | Tycon.Rigid _ => error ("defined", true)
         in
            loop tyStr
         end

      fun share {region: Region.t, time: Time.t, ty1, ty2} =
         case (getFlex {oper = "shared", time = time, ty = ty1},
               getFlex {oper = "shared", time = time, ty = ty2}) of
            (NONE, _) => ()
          | (_, NONE) => ()
          | (SOME flex1, SOME flex2) =>
               if Kind.equals (FlexibleTycon.kind flex1, FlexibleTycon.kind flex2)
                  then FlexibleTycon.share (flex1, flex2, region)
                  else Control.error
                       (region,
                        seq [str "types cannot be shared (arity): ",
                             (#lay ty1) (), str ", ", (#lay ty2) ()],
                        align ((mkErrorExtra (ty1, false, true, false)) @
                               (mkErrorExtra (ty2, false, true, false))))

      val share =
         Trace.trace
         ("Interface.TypeStr.share",
          fn {time, ty1, ty2, ...} =>
          Layout.record [("time", Time.layout time),
                         ("ty1", Layout.record [("tyStr", layout (#tyStr ty1))]),
                         ("ty2", Layout.record [("tyStr", layout (#tyStr ty2))])],
          Unit.layout)
         share

      fun wheree {region: Region.t,
                  realization: t,
                  time: Time.t,
                  ty: {lay: unit -> Layout.t,
                       region: Region.t,
                       spec: Region.t,
                       tyStr: t}}: unit =
         case getFlex {oper = "realized", time = time, ty = ty} of
            NONE => ()
          | SOME flex =>
               let
                  val tyKind = kind (#tyStr ty)
                  val rlKind = kind realization
                  val arityError =
                     if Kind.equals (tyKind, rlKind)
                        then NONE
                        else SOME (str "arity", seq [str "<arity ", Kind.layout rlKind, str ">"])
                  val tyAdmitsEquality = admitsEquality (#tyStr ty)
                  val rlAdmitsEquality = admitsEquality realization
                  val admitsEqualityError =
                     if AdmitsEquality.<= (tyAdmitsEquality, rlAdmitsEquality)
                        then NONE
                        else SOME (str "admits equality", str "<non-equality>")
                  val typeStructureError =
                     if FlexibleTycon.hasCons flex
                        andalso
                        Option.isNone (TypeStr.toTyconOpt realization)
                        then SOME (str "type structure", str "<complex type>")
                        else NONE
                  val errors =
                     List.keepAllMap
                     ([arityError, admitsEqualityError, typeStructureError],
                      fn opt => opt)
               in
                  if List.isEmpty errors
                     then (List.push (FlexibleTycon.specsRef flex, region)
                           ; FlexibleTycon.defnRef flex := Defn.typeStr realization)
                     else let
                             val (msgs, defnMsgs) = List.unzip errors
                             val arityErr = Option.isSome arityError
                             val keywordErr =
                                Option.isSome admitsEqualityError
                                orelse
                                Option.isSome typeStructureError
                          in
                             Control.error
                             (region,
                              seq [str "type cannot be realized (",
                                   (seq o List.separate) (msgs, str ", "),
                                   str "): ",
                                   (#lay ty) ()],
                              align ((mkErrorExtra (ty, keywordErr, arityErr, false)) @
                                     [seq [str "type defn: ",
                                           (seq o List.separate) (defnMsgs, str ", ")]]))
                          end
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

fun new {isClosed, strs, types, vals} =
   T (Set.singleton {copy = ref NONE,
                     flexible = ref NONE,
                     isClosed = isClosed,
                     original = NONE,
                     plist = PropertyList.new (),
                     strs = strs,
                     types = types,
                     uniqueId = UniqueId.new (),
                     vals = vals})

val empty = new {isClosed = true,
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

fun share (I: t, ls: Longstrid.t, I': t, ls': Longstrid.t, time, sharingSpec): unit =
   let
      fun mkTy (s, ls, strids, name) =
         let
            fun lay () =
               let
                  val (ss, s) = Longstrid.split ls
               in
                  Ast.Longtycon.layout
                  (Ast.Longtycon.long (List.concat [ss, [s], rev strids],
                                       name))
               end
         in
            {lay = lay,
             region = Longstrid.region ls,
             spec = Ast.Tycon.region name,
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
                            ignore (TypeStr.getFlex {oper = "shared",
                                                     time = time,
                                                     ty = mkTy (s, ls, strids, name)}))
                     in
                        ()
                     end
               end
            val () = loop (I, strids)
            val _ = destroy ()
         in
            ()
         end
      fun share (I, I', strids): unit = 
         if equals (I, I')
            then ensureFlexible (I, strids)
         else if sameShape (I, I')
            then
               let
                  fun loop (T s, T s', strids): unit =
                     let
                        val {isClosed, strs, types, ...} = Set.! s
                        val {strs = strs', types = types', ...} = Set.! s'
                        val _ =
                           (* Can't always union here.  I and I' may have
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
                              then Set.union (s, s')
                           else ()
                        val _ =
                           Array.foreach2
                           (types, types', fn ((name, s), (_, s')) =>
                            TypeStr.share {region = sharingSpec,
                                           time = time,
                                           ty1 = mkTy (s, ls, strids, name),
                                           ty2 = mkTy (s', ls', strids, name)})
                        val _ =
                           Array.foreach2
                           (strs, strs', fn ((name, I), (_, I')) =>
                            loop (I, I', name :: strids))
                     in
                        ()
                     end
               in
                  loop (I, I', strids)
               end
         else (* different shapes -- need to share pointwise *)
            let
               val T s = I
               val T s' = I'
               val {strs, types, ...} = Set.! s
               val {strs = strs', types = types', ...} = Set.! s'
               fun walk2 (a, a', compareNames, f: 'a * 'a * 'b -> unit) =
                  let
                     val n = Array.length a
                     val n' = Array.length a'
                     fun both (i, i') =
                        if i < n andalso i' < n'
                           then compare (i, Array.sub (a, i),
                                         i', Array.sub (a', i'))
                        else ()
                     and compare (i, (name, z), i', (name', z')) =
                        case compareNames (name, name') of
                           GREATER =>
                              let
                                 val i' = i' + 1
                              in
                                 if i' < n'
                                    then compare (i, (name, z),
                                                  i', Array.sub (a', i'))
                                 else ()
                              end
                         | EQUAL => (f (z, z', name)
                                     ; both (i + 1, i' + 1))
                         | LESS =>
                              let
                                 val i = i + 1
                              in
                                 if i < n
                                    then compare (i, Array.sub (a, i),
                                                  i', (name', z'))
                                 else ()
                              end
                  in
                     both (0, 0)
                  end
               val _ =
                  walk2 (strs, strs', Strid.compare,
                         fn (I, I', name) => share (I, I', name :: strids))
               val _ =
                  walk2 (types, types', Ast.Tycon.compare,
                         fn (s, s', name) =>
                         TypeStr.share {region = sharingSpec,
                                        time = time,
                                        ty1 = mkTy (s, ls, strids, name),
                                        ty2 = mkTy (s', ls', strids, name)})
            in
               ()
            end
   in
      share (I, I', [])
   end

val share =
   Trace.trace
   ("Interface.share",
    fn (I, _, I', _, t, _) =>
    Layout.tuple [layout I, layout I', Time.layout t],
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
                  fun setTycon (tycon, isDatatype) =
                     case tycon of
                        Tycon.Flexible fc =>
                           let
                              val {defn, hasCons, ...} = FlexibleTycon.fields fc
                           in
                              case Defn.dest (!defn) of
                                 Defn.Undefined =>
                                    let
                                       val r = tyconShortest fc
                                    in
                                       if (hasCons
                                           andalso not isDatatype)
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
                          TypeStr.Datatype {tycon, ...} =>
                             setTycon (tycon, true)
                        | TypeStr.Tycon tycon =>
                             setTycon (tycon, false)
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
