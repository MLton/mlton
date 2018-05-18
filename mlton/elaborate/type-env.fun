(* Copyright (C) 2009-2010,2012,2017 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor TypeEnv (S: TYPE_ENV_STRUCTS): TYPE_ENV =
struct

open S

structure Layout =
   struct
      open Layout
      val bracket = fn l =>
         seq [str "[", l, str "]"]
   end
local
   open Layout
in
   val seq = seq
   val str = str
end

local
   open LayoutPretty
in
   val bracket = bracket
   val dontCare = dontCare
   val simple = simple
end

local
   open Tycon
in
   structure AdmitsEquality = AdmitsEquality
   structure Kind = Kind
end
structure Field = Record.Field
structure Srecord = SortedRecord
structure Set = DisjointSet

(*
 * Keep a clock that the elaborator ticks for each declaration.  Associate each
 * type with a time that indicates the earliest declaration at which the type
 * occurs.  The time is used for several things.
 *
 * 1. When we need to generalize a type, we can tell which unknowns appear
 *    only in the declaration under consideration, and can hence be generalized.
 *
 * 2. Similarly, for type variables, we can tell if they appear in an earlier
 *    declaration than the one in which they are to be bound, and hence can
 *    not be generalized.
 *
 * 3. For "FlexRecord" types, we can tell when it appears only in the declaration
 *    under consideration, and can hence be converted to a "GenFlexRecord" type
 *    which allows for generalization of fields not yet known to be in the
 *    flexRecord.
 *
 * 4. For type constructors, we can tell if they are used outside of the scope
 *    of their definition.  This handles the side conditions on rules 4, 17, and
 *    19.
 *)

structure Time:>
   sig
      type t

      val <= : t * t -> bool
      val equals: t * t -> bool
      val layout: t -> Layout.t
      val max: t * t -> t
      val min: t * t -> t
      val now: unit -> t
      val region: t -> Region.t
      val tick: {region: Region.t} -> unit
      val zero: t
   end =
   struct
      datatype t = T of {clock: int,
                         region: Region.t}

      local
         fun make f (T r) = f r
      in
         val clock = make #clock
         val region = make #region
      end

      fun layout t =
         Layout.tuple [Int.layout (clock t),
                       Region.layout (region t)]

      local
         fun make f (t, t') = f (clock t, clock t')
      in
         val equals = make Int.equals
         val op <= = make Int.<=
      end
      fun max (t, t') = if t <= t' then t' else t
      fun min (t, t') = if t <= t' then t else t'

      val zero = T {clock = 0, region = Region.bogus}

      local
         val current: t ref = ref zero
      in
         fun now () = !current
         fun tick {region} =
            current := T {clock = 1 + clock (!current),
                          region = region}
      end

      val tick = Trace.trace ("TypeEnv.Time.tick", Layout.ignore, Unit.layout) tick
   end

structure Tyvar =
   struct
      open Tyvar

      local
         val {get = info: Tyvar.t -> {isEquality: bool,
                                      time: Time.t},
              set = setInfo, ...} =
            Property.getSet
            (Tyvar.plist,
             Property.initRaise ("TypeEnv.Tyvar.info", Tyvar.layout))
         fun init (a, ie) =
            setInfo (a, {isEquality = ie,
                         time = Time.now ()})
      in
         local
            fun make f = f o info
         in
            val time = make #time
            val isEquality = make #isEquality
         end
         fun makeString (s, {equality}) =
            let
               val a = newString s
               val _ = init (a, equality)
            in
               a
            end
         fun makeNoname {equality} =
            let
               val a = newNoname ()
               val _ = init (a, equality)
            in
               a
            end
         fun makeLike a =
            let
               val a' = new a
               val _ = init (a', isEquality a)
            in
               a'
            end
      end

      local
         fun makeLocalNames () =
            let
               val a = Char.toInt #"a"
               val z = Char.toInt #"z"
               val cnt = Counter.new a
               fun reset () = Counter.reset (cnt, a)
               fun next b =
                  let
                     val n = Counter.next cnt
                  in
                     Layout.str
                     (concat [if isEquality b then "''" else "'",
                              if n > z
                                 then concat ["a", Int.toString (n - z)]
                                 else Char.toString (Char.fromInt n)])
                  end
            in
               {next = next,
                reset = reset}
            end
      in
         fun makeLayoutPretty () =
            let
               val {destroy, get = layoutPretty, set = setLayoutPretty, ...} =
                  Property.destGetSet (plist, Property.initFun Tyvar.layout)
               fun localInit bs =
                  let
                     val {next, ...} = makeLocalNames ()
                  in
                     Vector.foreach
                     (bs, fn b => setLayoutPretty (b, next b))
                  end
         in
            {destroy = destroy,
             layoutPretty = layoutPretty,
             localInit = localInit}
         end

         fun makeLayoutPrettyLocal () =
            let
               val {next, reset} = makeLocalNames ()
               val {destroy, get = layoutPretty, ...} =
                  Property.destGet (plist, Property.initFun next)
            in
               {destroy = destroy,
                layoutPretty = layoutPretty,
                reset = fn () => (reset (); destroy ())}
            end
      end
   end
structure TyvarExt = Tyvar

structure Tycon =
   struct
      open Tycon

      local
         val {get = info: t -> {admitsEquality: AdmitsEquality.t ref,
                                kind: Kind.t,
                                prettyDefault: string,
                                region: Region.t,
                                time: Time.t},
              set = setInfo, ...} =
            Property.getSet
            (Tycon.plist,
             Property.initRaise ("TypeEnv.Tycon.info", Tycon.layout))
         fun init (c, a, k, pd, r) =
            setInfo (c, {admitsEquality = ref a,
                         kind = k,
                         prettyDefault = pd,
                         region = r,
                         time = Time.now ()})
         val _ =
            List.foreach
            (Tycon.prims, fn {tycon = c, admitsEquality = a, kind = k, name = lpd, ...} =>
             init (c, a, k, lpd, Region.bogus))
         val made: Tycon.t list ref = ref []
      in
         local
            fun make f = f o info
         in
            val admitsEquality = ! o make #admitsEquality
            val kind = make #kind
            val prettyDefault = make #prettyDefault
            val region = make #region
            val time = make #time
            fun setAdmitsEquality (t, ae) =
               (make #admitsEquality t) := ae
         end
         val layoutPrettyDefault = Layout.str o prettyDefault
         fun make {admitsEquality, kind, name,
                   prettyDefault, region} =
            let
               val tycon = Tycon.newString name
               val _ = init (tycon, admitsEquality, kind,
                             prettyDefault, region)
               val _ = List.push (made, tycon)
            in
               tycon
            end
         fun makeLike c =
            make {admitsEquality = admitsEquality c,
                  kind = kind c,
                  name = originalName c,
                  prettyDefault = prettyDefault c,
                  region = region c}
         fun scopeNew th =
            let
               val oldMade = !made
               val () = made := []
               val res = th ()
               val newMade = !made
               val () = made := oldMade
            in
               (res, newMade)
            end
      end
      fun makeBogus {name, kind, region} =
         make {admitsEquality = AdmitsEquality.Sometimes,
               kind = kind,
               name = name,
               prettyDefault = concat ["<", name, ">"],
               region = Option.fold (region, Region.bogus, #1)}
   end

structure Equality:>
   sig
      datatype t = False | True | Unknown

      val and2: t * t -> t
      val andL: 'a list * ('a -> t) -> t
      val andV: 'a vector * ('a -> t) -> t
      val applyTycon: Tycon.t * t vector -> t
      val fromBool: bool -> t
      val join: t * t -> t
      val layout: t -> Layout.t
      val or2: t * t -> t
   end =
   struct
      datatype t = False | True | Unknown

      fun layout e =
         case e of
            False => Layout.str "False"
          | True => Layout.str "True"
          | Unknown => Layout.str "Unknown"

      fun and2 (e1, e2) =
         case (e1, e2) of
            (False, _) => False
          | (_, False) => False
          | (True, _) => e2
          | (_, True) => e1
          | (Unknown, Unknown) => Unknown

      fun andL (xs, f) = List.fold (xs, True, fn (x, e) => and2 (f x, e))
      fun andV (xs, f) = Vector.fold (xs, True, fn (x, e) => and2 (f x, e))

      fun or2 (e1, e2) =
         case (e1, e2) of
            (False, _) => e2
          | (_, False) => e1
          | (True, _) => True
          | (_, True) => True
          | (Unknown, Unknown) => Unknown

      fun join (e1, e2) =
         case (e1, e2) of
            (Unknown, _) => e2
          | (_, Unknown) => e1
          | (False, False) => False
          | (True, True) => True
          | _ => Error.bug "TypeEnv.Equality.join"

      fun applyTycon (c, es) =
         let
            datatype z = datatype AdmitsEquality.t
         in
            case Tycon.admitsEquality c of
               Always => True
             | Sometimes => andV (es, fn e => e)
             | Never => False
         end

      fun fromBool b = if b then True else False
   end

structure Unknown =
   struct
      datatype t = T of {canGeneralize: bool,
                         id: int}

      local
         fun make f (T r) = f r
      in
         val id = make #id
      end

      fun layout (T {canGeneralize, id, ...}) =
         seq [str "Unknown ",
              Layout.record [("canGeneralize", Bool.layout canGeneralize),
                             ("id", Int.layout id)]]

      fun layoutPretty _ = str "???"

      fun equals (u, u') = id u = id u'

      local
         val c = Counter.new 0
      in
         val newId = fn () => Counter.next c
      end

      fun new {canGeneralize} =
         T {canGeneralize = canGeneralize,
            id = newId ()}

      fun join (T r, T r'): t =
         T {canGeneralize = #canGeneralize r andalso #canGeneralize r',
            id = newId ()}
   end

(* Flexible record spine, i.e. a possibly extensible list of fields. *)
structure Spine:
   sig
      type t

      val canAddFields: t -> bool
      val equals: t * t -> bool
      val fields: t -> Field.t list
      (* ensureField checks if field is there.  If it is not, then ensureField
       * will add it unless no more fields are allowed in the spine.
       * It returns true iff it succeeds.
       *)
      val ensureField: t * Field.t -> bool
      val foldOverNew: t * (Field.t * 'a) list * 'b * (Field.t * 'b -> 'b) -> 'b
      val layout: t -> Layout.t
      val new: Field.t list -> t
      val noMoreFields: t -> unit
      val unify: t * t -> unit
   end =
   struct
      datatype t = T of {id: int,
                         body: {fields: Field.t list ref,
                                more: bool ref} Set.t}

      local
         val c = Counter.new 0
      in
         val newId = fn () => Counter.next c
      end
      
      fun new fields = T {id = newId (),
                          body = Set.singleton {fields = ref fields,
                                                more = ref true}}

      fun equals (T {id = id1,...}, T {id = id2,...}) = id1 = id2

      fun layout (T {body = s,...}) =
         let
            val {fields, more} = Set.! s
         in
            Layout.record [("fields", List.layout Field.layout (!fields)),
                           ("more", Bool.layout (!more))]
         end

      fun canAddFields (T {body = s,...}) = ! (#more (Set.! s))
      fun fields (T {body = s,...}) = ! (#fields (Set.! s))

      fun ensureFieldValue ({fields, more}, f) =
         List.contains (!fields, f, Field.equals)
         orelse (!more andalso (List.push (fields, f); true))

      fun ensureField (T {body = s,...}, f) = ensureFieldValue (Set.! s, f)

      fun noMoreFields (T {body = s,...}) = #more (Set.! s) := false

      fun unify (T {body = s1,...}, T {body = s2,...}) =
         let
            val {fields = fs1, more = m1} = Set.! s1
            val {fields = fs2, more = m2} = Set.! s2
            val _ = Set.union (s1, s2)
            val fs = List.union (!fs1, !fs2, Field.equals)
            val m = !m1 andalso !m2
            val _ = Set.:= (s1, {fields = ref fs, more = ref m})
         in
            ()
         end

      fun foldOverNew (spine: t, fs, ac, g) =
         List.fold
         (fields spine, ac, fn (f, ac) =>
          if List.exists (fs, fn (f', _) => Field.equals (f, f'))
             then ac
          else g (f, ac))
   end

structure Type =
   struct
      structure Overload =
         struct
            datatype t = Char | Int | Real | Word

            val equals: t * t -> bool = op =

            val toString =
               fn Char => "Char"
                | Int => "Int"
                | Real => "Real"
                | Word => "Word"

            val layout = Layout.str o toString

            val matchesTycon: t * Tycon.t -> bool =
               fn (ov, c) =>
               case ov of
                  Char => Tycon.isCharX c
                | Int => Tycon.isIntX c
                | Real => Tycon.isRealX c
                | Word => Tycon.isWordX c

            val defaultTycon: t -> Tycon.t =
               fn Char => Tycon.defaultChar ()
                | Int => Tycon.defaultInt ()
                | Real => Tycon.defaultReal ()
                | Word => Tycon.defaultWord ()

           val admitsEquality: t -> Equality.t =
               fn Char => Equality.True
                | Int => Equality.True
                | Real => Equality.False
                | Word => Equality.True
         end

      (* Tuples of length <> 1 are always represented as records.
       * There will never be tuples of length one.
       *)
      datatype t = T of {equality: Equality.t ref,
                         plist: PropertyList.t,
                         time: Time.t ref,
                         ty: ty} Set.t
      and ty =
          Con of Tycon.t * t vector
        | FlexRecord of {fields: fields,
                         spine: Spine.t}
        (* GenFlexRecord only appears in type schemes.
         * It will never be unified.
         * The fields that are filled in after generalization are stored in
         * extra.
         *)
        | GenFlexRecord of genFlexRecord
        | Overload of Overload.t
        | Record of t Srecord.t
        | Unknown of Unknown.t
        | Var of Tyvar.t
      withtype fields = (Field.t * t) list
      and genFlexRecord =
         {extra: unit -> {field: Field.t,
                          tyvar: Tyvar.t} list,
          fields: (Field.t * t) list,
          spine: Spine.t}

      local
         fun make f (T s) = f (Set.! s)
      in
         val equality = make #equality
         val plist: t -> PropertyList.t = make #plist
         val time: t -> Time.t ref = make #time
         val getTy: t -> ty = make #ty
      end

      local
         open Layout
      in
         fun layoutFields fs =
            List.layout (Layout.tuple2 (Field.layout, layout)) fs
         and layout (T s) =
            let
               val {equality, time, ty, ...} = Set.! s
            in
               record
               [("time", Time.layout (!time)),
                ("equality", Equality.layout (!equality)),
                ("ty",
                 case ty of
                    Con (c, ts) =>
                       paren (align [seq [str "Con ", Tycon.layout c],
                                     Vector.layout layout ts])
                  | FlexRecord {fields, spine} =>
                       seq [str "Flex ",
                            record [("fields", layoutFields fields),
                                    ("spine", Spine.layout spine)]]
                  | GenFlexRecord {extra, fields, spine} =>
                       seq [str "GenFlex ",
                            record [("extra", 
                                     List.layout
                                     (fn {field, tyvar} =>
                                      record [("field", Field.layout field),
                                              ("tyvar", Tyvar.layout tyvar)])
                                    (extra ())),
                                    ("fields", layoutFields fields),
                                    ("spine", Spine.layout spine)]]
                  | Overload ov => Overload.layout ov
                  | Record r => Srecord.layout {record = r,
                                                separator = ": ",
                                                extra = "",
                                                layoutTuple = Vector.layout layout,
                                                layoutElt = layout}
                  | Unknown u => Unknown.layout u
                  | Var a => paren (seq [str "Var ", Tyvar.layout a]))]
            end
      end
   end

structure Tycon =
   struct
      open Tycon

      val {get = opaqueExpansion: t -> (Type.t vector -> Type.t) option,
           set = setOpaqueExpansion, ...} =
         Property.getSet (Tycon.plist, Property.initConst NONE)

      val opaqueExpansion =
         Trace.trace
         ("TypeEnv.Tycon.opaqueExpansion", Tycon.layout, Layout.ignore)
         opaqueExpansion

      val setOpaqueExpansion = fn (c, f) =>
         setOpaqueExpansion (c, SOME f)
   end
structure TyconExt = Tycon

structure LayoutPretty =
   struct
      open LayoutPretty
      fun record (ds: (Field.t * bool * t) list, flexible: bool) =
         simple (case ds of
                    [] => if flexible then str "{...}" else str "{}"
                  | _ =>
                       seq [str "{",
                            Layout.mayAlign
                            (Layout.separateRight
                             (List.map
                              (QuickSort.sortList (ds, fn ((f, _, _), (f', _, _)) =>
                                                   Field.<= (f, f')),
                               fn (f, b, (l, _)) =>
                               let
                                  val f = Field.layout f
                                  val row = seq [f, str ": ", l]
                                  val row = if b then Layout.bracket row else row
                               in
                                  row
                               end),
                              ",")),
                            str (if flexible
                                    then ", ...}"
                                    else "}")])
      fun tuple (ls: t vector): t =
         Tycon.layoutAppPretty
         (Tycon.tuple, ls,
          {layoutPretty = fn _ =>
           Error.bug "TypeEnv.LayoutPretty.tuple: layoutPretty"})
   end

structure Type =
   struct
      open Type

      fun makeHom {con, expandOpaque, flexRecord, genFlexRecord,
                   guard, overload, record, recursive, unknown, var} =
         let
            datatype status = Processing | Seen | Unseen
            val {destroy = destroyStatus, get = status, ...} =
               Property.destGet (plist, Property.initFun (fn _ => ref Unseen))
            val {get, destroy = destroyProp} =
               Property.destGet
               (plist,
                Property.initRec
                (fn (t, get) =>
                 let
                    val r = status t
                 in
                    case !r of
                       Seen => Error.bug "TypeEnv.Type.makeHom: impossible"
                     | Processing => recursive t
                     | Unseen =>
                          (case guard t of
                              NONE =>
                                 let
                                    val _ = r := Processing
                                    fun loopFields fields =
                                       List.revMap (fields, fn (f, t) => (f, get t))
                                    val res =
                                       case getTy t of
                                          Con (c, ts) =>
                                             let
                                                fun no () =
                                                   con (t, c, Vector.map (ts, get))
                                                fun yes () =
                                                   (case Tycon.opaqueExpansion c of
                                                       NONE => no ()
                                                     | SOME f => get (f ts))
                                             in
                                                if expandOpaque then yes () else no ()
                                             end
                                        | FlexRecord {fields, spine} =>
                                             flexRecord
                                             (t, {fields = loopFields fields,
                                                  spine = spine})
                                        | GenFlexRecord {extra, fields, spine} =>
                                             genFlexRecord
                                             (t, {extra = extra,
                                                  fields = loopFields fields,
                                                  spine = spine})
                                        | Overload ov => overload (t, ov)
                                        | Record r => record (t, Srecord.map (r, get))
                                        | Unknown u => unknown (t, u)
                                        | Var a => var (t, a)
                                    val _ = r := Seen
                                 in
                                    res
                                 end
                            | SOME res => (r := Seen; res))
                 end))
            fun destroy () =
               (destroyStatus ()
                ; destroyProp ())
         in
            {hom = get, destroy = destroy}
         end

      fun hom (ty, z) =
         let
            val {hom, destroy} = makeHom z
         in
            Exn.finally (fn () => hom ty, destroy)
         end

      fun makeLayoutPretty {expandOpaque,
                            layoutPrettyTycon,
                            layoutPrettyTyvar} :
         {destroy: unit -> unit,
          layoutPretty: t -> LayoutPretty.t} =
         let
            val layoutAppPretty = fn (c, ts) =>
               Tycon.layoutAppPretty
               (c, ts, {layoutPretty = layoutPrettyTycon})
            fun con (_, c, ts) = layoutAppPretty (c, ts)
            fun con0 c = layoutAppPretty (c, Vector.new0 ())
            fun flexRecord (_, {fields, spine}) =
               LayoutPretty.record
               (List.fold
                (fields,
                 Spine.foldOverNew (spine, fields, [], fn (f, ac) =>
                                    (f, false, simple (str "#???"))
                                    :: ac),
                 fn ((f, t), ac) => (f, false, t) :: ac),
                Spine.canAddFields spine)
            fun genFlexRecord (_, {extra, fields, spine}) =
               LayoutPretty.record
               (List.fold
                (fields,
                 List.revMap (extra (), fn {field, tyvar} =>
                              (field, false, simple (layoutPrettyTyvar tyvar))),
                 fn ((f, t), ac) => (f, false, t) :: ac),
                Spine.canAddFields spine)
            fun overload (_, ov) = con0 (Overload.defaultTycon ov)
            fun record (_, r) =
               case Srecord.detupleOpt r of
                  NONE =>
                     LayoutPretty.record
                     (Vector.toListMap
                      (Srecord.toVector r,
                       fn (f, t) => (f, false, t)),
                      false)
                | SOME ts => LayoutPretty.tuple ts
            fun recursive _ = simple (str "<recur>")
            fun unknown (_, u) = simple (Unknown.layoutPretty u)
            fun var (_, a) = simple (layoutPrettyTyvar a)
            val {destroy, hom = layoutPretty} =
               makeHom {con = con,
                        expandOpaque = expandOpaque,
                        flexRecord = flexRecord,
                        genFlexRecord = genFlexRecord,
                        guard = fn _ => NONE,
                        overload = overload,
                        record = record,
                        recursive = recursive,
                        unknown = unknown,
                        var = var}
         in
            {destroy = destroy,
             layoutPretty = layoutPretty}
         end

      fun layoutPretty (t, {expandOpaque, layoutPrettyTycon, layoutPrettyTyvar}) =
         let
            val {destroy, layoutPretty} =
               makeLayoutPretty {expandOpaque = expandOpaque,
                                 layoutPrettyTycon = layoutPrettyTycon,
                                 layoutPrettyTyvar = layoutPrettyTyvar}
            val res = layoutPretty t
            val () = destroy ()
         in
            res
         end

      fun getEquality t =
         let
            (* guarded; only invoked if '!(Tycon.admitsEquality c) = Sometimes' *)
            fun con (_, c, es) = Equality.applyTycon (c, es)
            fun flexRecord (_, {fields: (Field.t * Equality.t) list, spine}) =
               if Spine.canAddFields spine
                  then Equality.Unknown
                  else Equality.andL (fields, #2)
            (* impossible; *)
            fun genFlexRecord _ = Error.bug "TypeEnv.Type.getEquality.genFlexRecord"
            (* guarded; an overload has known equality *)
            fun overload _ = Error.bug "TypeEnv.Type.getEquality.overload"
            fun record (_, r: Equality.t Srecord.t) =
               Equality.andV (Srecord.toVector r, #2)
            (* impossible *)
            fun recursive _ = Error.bug "TypeEnv.Type.getEquality.recursive"
            (* guarded; only invoked if '!(Type.equality t) = Unknown' *)
            fun unknown (_, _) = Equality.Unknown
            (* guarded; a tyvar has known equality *)
            fun var _ = Error.bug "TypeEnv.Type.getEquality.var"
            fun wrap (f, sel) arg =
               let
                  val res = f arg
                  val _ = equality (sel arg) := res
               in
                  res
               end
            fun guard t =
               let
                  val e = !(equality t)
               in
                  case e of
                     Equality.Unknown => NONE
                   | _ => SOME e
               end
         in
            hom (t, {con = wrap (con, #1),
                     expandOpaque = false,
                     flexRecord = wrap (flexRecord, #1),
                     genFlexRecord = genFlexRecord,
                     guard = guard,
                     overload = overload,
                     record = wrap (record, #1),
                     recursive = recursive,
                     unknown = unknown,
                     var = var})
         end

      val getEquality =
         Trace.trace
         ("TypeEnv.Type.getEquality", layout, Equality.layout)
         getEquality
      val _ = getEquality

      fun deConOpt t =
         case getTy t of
            Con x => SOME x
          | _ => NONE

      fun deEta (t: t, tyvars: Tyvar.t vector): Tycon.t option =
         case deConOpt t of
            SOME (c, ts) =>
               if Vector.length ts = Vector.length tyvars
                  andalso Vector.foralli (ts, fn (i, t) =>
                                          case getTy t of
                                             Var a =>
                                                Tyvar.equals
                                                (a, Vector.sub (tyvars, i))
                                           | _ => false)
                  then SOME c
               else NONE
           | _ => NONE

      fun make {equality, time, ty}: t =
         T (Set.singleton {equality = ref equality,
                           plist = PropertyList.new (),
                           time = ref time,
                           ty = ty})

      fun newTy (ty: ty): t =
         let
            val (equality, time) =
               case ty of
                  Con (c, ts) =>
                     (Equality.applyTycon
                      (c, Vector.map (ts, ! o equality)),
                      Vector.fold
                      (ts, Tycon.time c, fn (t, t') =>
                       Time.max (!(time t), t')))
                | GenFlexRecord _ =>
                     Error.bug "TypeEnv.Type.newTy: GenFlexRecord"
                | FlexRecord _ =>
                     (Equality.Unknown,
                      Time.now ())
                | Overload ov =>
                     (Overload.admitsEquality ov,
                      Time.zero)
                | Record r =>
                     (Srecord.fold
                      (r, Equality.True, fn (t, e') =>
                       Equality.and2 (!(equality t), e')),
                      Srecord.fold
                      (r, Time.zero, fn (t, t') =>
                       Time.max (!(time t), t')))
                | Unknown _ =>
                     Error.bug "TypeEnv.Type.newTy: Unknown"
                | Var a =>
                     (Equality.fromBool (Tyvar.isEquality a),
                      Tyvar.time a)
         in
            make {equality = equality,
                  time = time,
                  ty = ty}
         end

      fun setTy (T s, ty) =
         let
            val {equality, plist, time, ...} = Set.! s
         in
            Set.:= (s, {equality = equality,
                        plist = plist,
                        time = time,
                        ty = ty})
         end

      fun unknownAux {canGeneralize, equality, time} =
         let
            val u = Unknown.new {canGeneralize = canGeneralize}
            val t = make {equality = equality,
                          time = time,
                          ty = Unknown u}
         in
            (u, t)
         end
      val unknown = #2 o unknownAux

      fun new () =
         unknown
         {canGeneralize = true,
          equality = Equality.Unknown,
          time = Time.now ()}

      val new = Trace.trace ("TypeEnv.Type.new", Unit.layout, layout) new

      fun newFlex {fields, spine} =
         newTy (FlexRecord {fields = fields, spine = spine})

      fun flexRecord record =
         let
            val v = Srecord.toVector record
            val spine = Spine.new (Vector.toListMap (v, #1))
            fun isResolved (): bool = not (Spine.canAddFields spine)
            val t = newFlex {fields = Vector.toList v,
                             spine = spine}
         in
            (t, isResolved)
         end

      fun record r = newTy (Record r)

      fun tuple ts =
         if 1 = Vector.length ts
            then Vector.first ts
         else newTy (Record (Srecord.tuple ts))

      fun con (tycon, ts) =
         if Tycon.equals (tycon, Tycon.tuple)
            then tuple ts
         else newTy (Con (tycon, ts))

      fun var a = newTy (Var a)
   end

structure Ops = TypeOps (structure Tycon = Tycon
                         open Type)

structure UnifyResult =
   struct
      datatype ('a, 'b) t =
         NotUnifiable of 'a
       | Unified of 'b

      val layout =
         fn NotUnifiable _ => str "NotUnifiable"
          | Unified _ => str "Unified"
   end

structure Type =
   struct
      (* Order is important, since want specialized definitions in Type to
       * override general definitions in Ops.
       *)
      open Ops Type

      val unit = tuple (Vector.new0 ())

      fun isArrow t =
         case getTy t of
            Con (c, _) => Tycon.equals (c, Tycon.arrow)
          | _ => false

      fun isBool t =
         case getTy t of
            Con (c, _) => Tycon.isBool c
          | _ => false

      fun isCharX t =
         case getTy t of
            Con (c, _) => Tycon.isCharX c
          | Overload Overload.Char => true
          | _ => false

      fun isCPointer t =
         case getTy t of
            Con (c, _) => Tycon.isCPointer c
          | _ => false

      fun isInt t =
         case getTy t of
            Con (c, _) => Tycon.isIntX c
          | Overload Overload.Int => true
          | _ => false

      fun isUnit t =
         case getTy t of
            Record r =>
               (case Srecord.detupleOpt r of
                   NONE => false
                 | SOME v => Vector.isEmpty v)
          | _ => false

      fun isUnknown t =
         case getTy t of
            Unknown _ => true
          | _ => false

      local
         fun make ov () = newTy (Overload ov)
         datatype z = datatype Overload.t
      in
         val unresolvedChar = make Char
         val unresolvedInt = make Int
         val unresolvedReal = make Real
         val unresolvedWord = make Word
      end

      fun unresolvedString () = vector (unresolvedChar ())

      val traceCanUnify =
         Trace.trace2 
         ("TypeEnv.Type.canUnify", layout, layout, Bool.layout)

      fun canUnify arg = 
         traceCanUnify
         (fn (t, t') =>
          case (getTy t, getTy t') of
             (Unknown _,  _) => true
           | (_, Unknown _) => true
           | (Con (c, ts), t') => conAnd (c, ts, t')
           | (t', Con (c, ts)) => conAnd (c, ts, t')
           | (Overload o1, Overload o2) => Overload.equals (o1, o2)
           | (Record r, Record r') =>
                let
                   val fs = Srecord.toVector r
                   val fs' = Srecord.toVector r'
                in Vector.length fs = Vector.length fs'
                   andalso Vector.forall2 (fs, fs', fn ((f, t), (f', t')) =>
                                           Field.equals (f, f')
                                           andalso canUnify (t, t'))
                end
           | (Var a, Var a') => Tyvar.equals (a, a')
           | _ => false) arg
      and conAnd (c, ts, t') =
         case t' of
            Con (c', ts') =>
               Tycon.equals (c, c')
               andalso Vector.forall2 (ts, ts', canUnify)
          | Overload ov =>
               Vector.isEmpty ts andalso Overload.matchesTycon (ov, c)
          | _ => false

      (* checkEquality t checks that t is an equality type.  If t is
       * not an equality type, then checkEquality t returns:
       *  - a layout object highlighting violations in t
       *  - an alternate type, replacing violations in t with fresh
       *    equality unification variables and a layout object
       *    highlighting the fresh unification variables.
       *)
      fun checkEquality (t, {layoutPrettyTycon: Tycon.t -> Layout.t,
                             layoutPrettyTyvar: Tyvar.t -> Layout.t}) =
         let
            val layoutAppPretty = fn (c, ts) =>
               Tycon.layoutAppPretty
               (c, ts, {layoutPretty = layoutPrettyTycon})
            type ll = LayoutPretty.t * LayoutPretty.t
            local
               fun getLay sel (llo: ll option, _) =
                  Option.fold (llo, dontCare, sel o #1)
            in
               val getLay1 = getLay #1
               val getLay2 = getLay #2
            end
            fun getTy (_, ty) = ty
            (* guarded; only invoked if 'Tycon.admitsEquality c = Sometimes' *)
            fun con (_, c, rs) =
               if Vector.forall (rs, Option.isNone o #1)
                  then NONE
                  else SOME (layoutAppPretty
                             (c, Vector.map (rs, getLay1)),
                             layoutAppPretty
                             (c, Vector.map (rs, getLay2)),
                             Type.con
                             (c, Vector.map (rs, getTy)))
            fun doRecord (fls: (Field.t * ll) list,
                          extra: bool, mk: unit -> t) =
               if List.isEmpty fls
                  then NONE
                  else let
                          fun doit sel =
                             LayoutPretty.record
                             (List.map
                              (fls, fn (f, lll) =>
                               (f, false, sel lll)),
                              extra)
                       in
                          SOME (doit #1, doit #2, mk ())
                       end
            fun flexRecord (_, {fields, spine}) =
               doRecord (List.keepAllMap
                         (fields, fn (f, r) =>
                          Option.map (#1 r, fn ll => (f, ll))),
                         Spine.canAddFields spine,
                         fn () =>
                         let
                            val fields =
                               List.map (fields, fn (f, r) =>
                                         (f, getTy r))
                         in
                            Type.newFlex {fields = fields,
                                          spine = spine}
                         end)
            (* impossible *)
            fun genFlexRecord _ = Error.bug "TypeEnv.Type.checkEquality.genFlexRecord"
            (* guarded; an overload has known equality
             * only invoked if '!(Type.equality t) = False' *)
            fun overload (_, ov) =
               case ov of
                  Overload.Real =>
                     let
                        val ty =
                           Type.unknown {canGeneralize = true,
                                         equality = Equality.True,
                                         time = Time.now ()}
                     in
                        SOME (bracket (simple (str "real")),
                              bracket (simple (str "<equality>")),
                              ty)
                     end
                | _ => Error.bug "TypeEnv.Type.checkEquality.overload"
            fun record (_, r) =
               case Srecord.detupleOpt r of
                  NONE =>
                     let
                        val fields = Srecord.toVector r
                        val fields' =
                           Vector.keepAllMap
                           (fields, fn (f, r) =>
                            Option.map (#1 r, fn ll => (f, ll)))
                     in
                        doRecord (Vector.toList fields',
                                  not (Vector.length fields = Vector.length fields'),
                                  fn () =>
                                  let
                                     val fields =
                                        Vector.map (fields, fn (f, r) =>
                                                    (f, getTy r))
                                     val fields = Srecord.fromVector fields
                                  in
                                     Type.record fields
                                  end)
                     end
                | SOME rs =>
                     if Vector.forall (rs, Option.isNone o #1)
                        then NONE
                        else SOME (LayoutPretty.tuple (Vector.map (rs, getLay1)),
                                   LayoutPretty.tuple (Vector.map (rs, getLay2)),
                                   Type.tuple (Vector.map (rs, getTy)))
            (* impossible *)
            fun recursive _ = Error.bug "TypeEnv.Type.checkEquality.recursive"
            fun unknown (t, _) =
               case !(equality t) of
                  Equality.False =>
                     let
                        val ty =
                           Type.unknown {canGeneralize = true,
                                         equality = Equality.True,
                                         time = Time.now ()}
                     in
                        SOME (bracket (simple (str "<non-equality>")),
                              bracket (simple (str "<equality>")),
                              ty)
                     end
                | Equality.True => NONE
                | Equality.Unknown => NONE
            (* guarded; a tyvar has known equality
             * only invoked if '!(Type.equality t) = False' *)
            fun var (_, a) =
               if Tyvar.isEquality a
                  then Error.bug "TypeEnv.Type.checkEquality.var"
                  else let
                          val ty =
                             Type.unknown {canGeneralize = true,
                                           equality = Equality.True,
                                           time = Time.now ()}
                       in
                          SOME (bracket (simple (layoutPrettyTyvar a)),
                                bracket (simple (str "<equality>")),
                                ty)
                       end
            fun wrap (f, sel) arg =
               case f arg of
                  NONE =>
                     let
                        val t = sel arg
                     in
                        equality t := Equality.True
                        ; (NONE, t)
                     end
                | SOME (l1, l2, t) =>
                     (SOME (l1, l2), t)
            (* Need extra guarding of Con, because proceeding with
             * hom/con would recursively force all Unknowns in type
             * args to Equality.True, even if tycon is
             * AdmitsEquality.Never. *)
            fun guard t =
               case !(equality t) of
                  Equality.True => SOME (NONE, t)
                | _ => (case Type.getTy t of
                           Con (c, ts) =>
                              (case Tycon.admitsEquality c of
                                  AdmitsEquality.Always => SOME (NONE, t)
                                | AdmitsEquality.Sometimes => NONE
                                | AdmitsEquality.Never =>
                                     let
                                        val ty =
                                           Type.unknown {canGeneralize = true,
                                                         equality = Equality.True,
                                                         time = Time.now ()}
                                     in
                                        SOME (SOME ((bracket o layoutAppPretty)
                                                    (c, Vector.map (ts, fn _ => dontCare)),
                                                    bracket (simple (str "<equality>"))),
                                              ty)
                                     end)
                         | _ => NONE)
            val res : ll option * t =
               hom (t, {con = wrap (con, #1),
                        expandOpaque = false,
                        flexRecord = wrap (flexRecord, #1),
                        genFlexRecord = genFlexRecord,
                        guard = guard,
                        overload = wrap (overload, #1),
                        record = wrap (record, #1),
                        recursive = recursive,
                        unknown = wrap (unknown, #1),
                        var = wrap (var, #1)})
         in
            case res of
               (NONE, _) => NONE
             | (SOME (l1, l2), t) =>
                  SOME (l1, (t, l2))
         end

      (* checkTime (t, bound) checks that all components of t have
       * times no larger than bound.  If t has components with time
       * larger than bound, then checkTime (t, bound) returns:
       *  - a layout object highlighting violations in t
       *  - an alternate type, replacing violations in t with fresh
       *    unification variables at time bound and a layout object
       *    highlighting the fresh unification variables.
       *  - a list of violating tycons
       *  - a list of violating tyvars
       *)
      fun makeCheckTime {layoutPrettyTycon: Tycon.t -> Layout.t,
                         layoutPrettyTyvar: Tyvar.t -> Layout.t} =
         let
            val layoutAppPretty = fn (c, ts) =>
               Tycon.layoutAppPretty
               (c, ts, {layoutPretty = layoutPrettyTycon})
            val times: Time.t list ref = ref []
            val tycons: Tycon.t list ref = ref []
            val tyvars: Tyvar.t list ref = ref []
            type lll = LayoutPretty.t * LayoutPretty.t * LayoutPretty.t
            local
               fun getLay sel (lllo: lll option, _) =
                  Option.fold (lllo, dontCare, sel o #1)
            in
               val getLay1 = getLay #1
               val getLay2 = getLay #2
               val getLay3 = getLay #3
            end
            fun getTy (_, ty) = ty
            fun con bound (_, c, rs) =
               if Time.<= (Tycon.time c, bound)
                  then if Vector.forall (rs, Option.isNone o #1)
                          then NONE
                          else SOME (layoutAppPretty
                                     (c, Vector.map (rs, getLay1)),
                                     layoutAppPretty
                                     (c, Vector.map (rs, getLay2)),
                                     layoutAppPretty
                                     (c, Vector.map (rs, getLay3)),
                                     Type.con
                                     (c, Vector.map (rs, getTy)))
                  else let
                          val (u, ty) =
                             Type.unknownAux {canGeneralize = true,
                                              equality = Equality.Unknown,
                                              time = bound}
                       in
                          List.push (times, bound)
                          ; List.push (tycons, c)
                          ; SOME ((bracket o layoutAppPretty)
                                  (c, Vector.map (rs, getLay2)),
                                  layoutAppPretty
                                  (c, Vector.map (rs, getLay2)),
                                  bracket (simple (Unknown.layoutPretty u)),
                                  ty)
                       end
            fun doRecord (fls: (Field.t * lll) list,
                          extra: bool, mk: unit -> t) =
               if List.isEmpty fls
                  then NONE
                  else let
                          fun doit sel =
                             LayoutPretty.record
                             (List.map
                              (fls, fn (f, lll) =>
                               (f, false, sel lll)),
                              extra)
                       in
                          SOME (doit #1, doit #2, doit #3, mk ())
                       end
            fun flexRecord (_, {fields, spine}) =
               doRecord (List.keepAllMap
                         (fields, fn (f, r) =>
                          Option.map (#1 r, fn lll => (f, lll))),
                         Spine.canAddFields spine,
                         fn () =>
                         let
                            val fields =
                               List.map (fields, fn (f, r) =>
                                         (f, getTy r))
                         in
                            Type.newFlex {fields = fields,
                                          spine = spine}
                         end)
            fun record (_, r) =
               case Srecord.detupleOpt r of
                  NONE =>
                     let
                        val fields = Srecord.toVector r
                        val fields' =
                           Vector.keepAllMap
                           (fields, fn (f, r) =>
                            Option.map (#1 r, fn lll => (f, lll)))
                     in
                        doRecord (Vector.toList fields',
                                  not (Vector.length fields = Vector.length fields'),
                                  fn () =>
                                  let
                                     val fields =
                                        Vector.map (fields, fn (f, r) =>
                                                    (f, getTy r))
                                     val fields = Srecord.fromVector fields
                                  in
                                     Type.record fields
                                  end)
                     end
                | SOME rs =>
                     if Vector.forall (rs, Option.isNone o #1)
                        then NONE
                        else SOME (LayoutPretty.tuple (Vector.map (rs, getLay1)),
                                   LayoutPretty.tuple (Vector.map (rs, getLay2)),
                                   LayoutPretty.tuple (Vector.map (rs, getLay3)),
                                   Type.tuple (Vector.map (rs, getTy)))
            fun var bound (_, a) =
               if Time.<= (Tyvar.time a, bound)
                  then NONE
                  else let
                          val (u, ty) =
                             Type.unknownAux {canGeneralize = true,
                                              equality = Equality.Unknown,
                                              time = bound}
                       in
                          List.push (times, bound)
                          ; List.push (tyvars, a)
                          ; SOME (bracket (simple (layoutPrettyTyvar a)),
                                  simple (layoutPrettyTyvar a),
                                  bracket (simple (Unknown.layoutPretty u)),
                                  ty)
                       end
            fun genFlexRecord _ = Error.bug "TypeEnv.Type.checkTime.genFlexRecord"
            fun recursive _ = Error.bug "TypeEnv.Type.checkTime.recursive"
            fun checkTime (t, bound) =
               if Time.<= (!(time t), bound)
                  then NONE
                  else let
                          fun wrap (f, sel) arg =
                             case f arg of
                                NONE =>
                                   let
                                      val t = sel arg
                                   in
                                      time t := bound
                                      ; (NONE, t)
                                   end
                              | SOME (l1, l2, l3, t) =>
                                   (time t := bound
                                    ; (SOME (l1, l2, l3), t))
                          fun guard t =
                             if Time.<= (!(time t), bound)
                                then SOME (NONE, t)
                                else NONE
                          val res : lll option * t =
                             hom (t, {con = wrap (con bound, #1),
                                      expandOpaque = false,
                                      flexRecord = wrap (flexRecord, #1),
                                      genFlexRecord = genFlexRecord,
                                      guard = guard,
                                      overload = wrap (fn _ => NONE, #1),
                                      record = wrap (record, #1),
                                      recursive = recursive,
                                      unknown = wrap (fn _ => NONE, #1),
                                      var = wrap (var bound, #1)})
                       in
                          case res of
                             (NONE, _) => NONE
                           | (SOME (l1, _, l3), t) =>
                                SOME (l1, (t, l3))
                       end
            fun finishCheckTime () =
               {times = List.removeDuplicates (!times, Time.equals),
                tycons = List.removeDuplicates (!tycons, Tycon.equals),
                tyvars = List.removeDuplicates (!tyvars, Tyvar.equals)}
         in
            {checkTime = checkTime,
             finishCheckTime = finishCheckTime}
         end

      datatype z = datatype UnifyResult.t

      val traceUnify =
         Trace.trace2 
         ("TypeEnv.Type.unify", layout, layout,
          UnifyResult.layout:
          (LayoutPretty.t * LayoutPretty.t, unit) UnifyResult.t -> Layout.t)

      fun unify (t, t',
                 {layoutPretty: t -> LayoutPretty.t,
                  layoutPrettyTycon: Tycon.t -> Layout.t,
                  layoutPrettyTyvar: Tyvar.t -> Layout.t}) =
         let
            val layoutAppPretty = fn (c, ts) =>
               Tycon.layoutAppPretty
               (c, ts, {layoutPretty = layoutPrettyTycon})
            val checkEquality = fn t =>
               checkEquality (t, {layoutPrettyTycon = layoutPrettyTycon,
                                  layoutPrettyTyvar = layoutPrettyTyvar})
            val {checkTime, finishCheckTime} =
               makeCheckTime {layoutPrettyTycon = layoutPrettyTycon,
                              layoutPrettyTyvar = layoutPrettyTyvar}
            fun unify arg =
               traceUnify
               (fn (outer as T s, outer' as T s') =>
                if Set.equals (s, s')
                   then Unified ()
                else
                   let
                      fun notUnifiable (l: LayoutPretty.t, l': LayoutPretty.t) =
                         NotUnifiable (l, l')
                      fun notUnifiableBracket (l, l') =
                         notUnifiable (bracket l, bracket l')
                      fun flexToRecord (fields, spine) =
                         (Vector.fromList fields,
                          Spine.canAddFields spine)
                      fun rigidToRecord r =
                         (Srecord.toVector r,
                          false)
                      fun flexToFlexToRecord (fields, spine, equality, time, outer, spine') =
                         let
                            val () =
                               List.foreach
                               (Spine.fields spine', fn f' =>
                                ignore (Spine.ensureField (spine, f')))
                            val fields =
                               Spine.foldOverNew
                               (spine, fields, fields, fn (f, fields) =>
                                let
                                   val u =
                                      Type.unknown
                                      {canGeneralize = true,
                                       equality = Equality.or2 (equality, Equality.Unknown),
                                       time = time}
                                in
                                   (f, u) :: fields
                                end)
                            val _ = setTy (outer, FlexRecord {fields = fields, spine = spine})
                         in
                            flexToRecord (fields, spine)
                         end
                      fun flexToRigidToRecord (fields, spine, equality, time, outer, r') =
                         let
                            val () =
                               Vector.foreach
                               (Srecord.toVector r', fn (f', _) =>
                                ignore (Spine.ensureField (spine, f')))
                            val () = Spine.noMoreFields spine
                            val fields =
                               Spine.foldOverNew
                               (spine, fields, fields, fn (f, fields) =>
                                let
                                   val u =
                                      Type.unknown
                                      {canGeneralize = true,
                                       equality = Equality.or2 (equality, Equality.Unknown),
                                       time = time}
                                in
                                   (f, u) :: fields
                                end)
                            val r = Srecord.fromVector (Vector.fromList fields)
                            val _ = setTy (outer, Record r)
                         in
                            rigidToRecord r
                         end
                      fun oneFlex ({fields, spine}, equality, time, outer, r', swap) =
                         unifyRecords
                         (flexToRigidToRecord (fields, spine, equality, time, outer, r'),
                          rigidToRecord r',
                          fn () => Unified (Record r'),
                          notUnifiable o (fn (l, l') =>
                                          if swap
                                             then (l', l)
                                             else (l, l')))
                      fun genFlexError () =
                         Error.bug "TypeEnv.Type.unify: GenFlexRecord"
                      val {equality, time, ty = t, plist} = Set.! s
                      val {equality = equality', time = time', ty = t', ...} = Set.! s'
                      fun not () =
                         notUnifiableBracket (layoutPretty outer,
                                              layoutPretty outer')
                      fun unifys (ts, ts', yes, no) =
                         let
                            val us = Vector.map2 (ts, ts', unify)
                         in
                            if Vector.forall
                               (us, fn Unified _ => true | _ => false)
                               then yes ()
                            else
                               let
                                  val (ls, ls') =
                                     Vector.unzip
                                     (Vector.map
                                      (us, fn u =>
                                       case u of
                                          Unified _ => (dontCare, dontCare)
                                        | NotUnifiable (l, l') => (l, l')))
                               in
                                  no (ls, ls')
                               end
                         end
                      fun conAnd (c, ts, t, t', swap) =
                         let
                            fun maybe (z, z') =
                               if swap then (z', z) else (z, z')
                         in
                            case t of
                               Con (c', ts') =>
                                  if Tycon.equals (c, c')
                                     then
                                        if Vector.length ts <> Vector.length ts'
                                           then
                                              let
                                                 fun lay ts =
                                                    simple
                                                    (Layout.seq
                                                     [Layout.str
                                                      (concat ["<",
                                                               Int.toString
                                                               (Vector.length ts),
                                                               " args> "]),
                                                      Tycon.layout c])
                                              in
                                                 notUnifiableBracket
                                                 (maybe (lay ts, lay ts'))
                                              end
                                        else
                                           unifys
                                           (ts, ts',
                                            fn () => Unified t,
                                            fn (ls, ls') =>
                                            let
                                               fun lay ls = layoutAppPretty (c, ls)
                                            in
                                               notUnifiable
                                               (maybe (lay ls, lay ls'))
                                            end)
                                  else not ()
                             | Overload ov =>
                                  if Vector.isEmpty ts
                                     andalso Overload.matchesTycon (ov, c)
                                     then Unified t'
                                  else not ()
                             | _ => not ()
                         end
                      fun oneUnknown (u: Unknown.t,
                                      equality: Equality.t,
                                      time: Time.t,
                                      outer: t,
                                      t': Type.ty,
                                      outer': Type.t,
                                      swap: bool) =
                         let
                            (* This should fail if the unknown occurs in t. *)
                            fun con (_, _, ts) =
                               Vector.exists (ts, fn b => b)
                            fun doFields fields =
                               List.exists (fields, fn (_, b) => b)
                            fun flexRecord (_, {fields, spine = _}) =
                               doFields fields
                            fun record (_, r) = Srecord.exists (r, fn b => b)
                            fun unknown (_, u') = Unknown.equals (u, u')
                            fun no _ = false
                            val isCircular =
                               hom (outer',
                                    {con = con,
                                     expandOpaque = false,
                                     flexRecord = flexRecord,
                                     genFlexRecord = fn _ =>
                                     Error.bug "TypeEnv.Type.unify.oneUnknown: genFlexRecord",
                                     guard = fn _ => NONE,
                                     overload = no,
                                     record = record,
                                     recursive = fn _ => 
                                     Error.bug "TypeEnv.Type.unify.oneUnknown: recursive",
                                     unknown = unknown,
                                     var = no})
                         in
                            if isCircular
                               then not ()
                               else let
                                       fun err (l, (t'', l'')) =
                                          (setTy (outer, getTy t'')
                                           ; notUnifiable
                                             (if swap
                                                 then (l, l'')
                                                 else (l'', l)))
                                    in
                                       case equality of
                                          Equality.True =>
                                             (case checkEquality outer' of
                                                 NONE => Unified t'
                                               | SOME (l, (t'', l'')) =>
                                                    err (l, (t'', l'')))
                                        | _ =>
                                             (case checkTime (outer', time) of
                                                 NONE => Unified t'
                                               | SOME (l, (t'', l'')) =>
                                                    err (l, (t'', l'')))
                                    end
                         end
                      val res =
                         case (t, t') of
                            (Unknown r, Unknown r') =>
                               (case (!equality, !equality') of
                                   (Equality.True, Equality.False) =>
                                      notUnifiableBracket
                                      (simple (str "<equality>"),
                                       simple (str "<non-equality>"))
                                 | (Equality.False, Equality.True) =>
                                      notUnifiableBracket
                                      (simple (str "<non-equality>"),
                                       simple (str "<equality>"))
                                 | _ => Unified (Unknown (Unknown.join (r, r'))))
                          | (Unknown u, _) =>
                               oneUnknown (u, !equality, !time, outer, t', outer', false)
                          | (_, Unknown u') =>
                               oneUnknown (u', !equality', !time', outer', t, outer, true)
                          | (Con (c, ts), _) => conAnd (c, ts, t', t, false)
                          | (_, Con (c, ts)) => conAnd (c, ts, t, t', true)
                          | (FlexRecord f, Record r') =>
                               oneFlex (f, !equality, !time, outer, r', false)
                          | (Record r, FlexRecord f') =>
                               oneFlex (f', !equality', !time', outer', r, true)
                          | (FlexRecord {fields = fields, spine = spine},
                             FlexRecord {fields = fields', spine = spine'}) =>
                               let
                                  fun yes () =
                                     let
                                        val () = Spine.unify (spine, spine')
                                        val fields =
                                           List.fold
                                           (fields, fields', fn ((f, t), ac) =>
                                            if List.exists (fields', fn (f', _) =>
                                                            Field.equals (f, f'))
                                               then ac
                                               else (f, t) :: ac)
                                     in
                                        Unified (FlexRecord {fields = fields,
                                                             spine = spine})
                                     end
                               in
                                  unifyRecords
                                  (flexToFlexToRecord (fields, spine, !equality, !time, outer, spine'),
                                   flexToFlexToRecord (fields', spine', !equality', !time', outer', spine),
                                   yes, notUnifiable)
                               end
                          | (GenFlexRecord _, _) => genFlexError ()
                          | (_, GenFlexRecord _) => genFlexError ()
                          | (Overload ov1, Overload ov2) =>
                               if Overload.equals (ov1, ov2)
                                  then Unified t
                               else not ()
                          | (Record r, Record r') =>
                               (case (Srecord.detupleOpt r,
                                      Srecord.detupleOpt r') of
                                   (NONE, NONE) =>
                                      unifyRecords
                                      (rigidToRecord r, rigidToRecord r',
                                       fn () => Unified (Record r),
                                       notUnifiable)
                                 | (SOME ts, SOME ts') =>
                                      if Vector.length ts = Vector.length ts'
                                         then unifys
                                              (ts, ts',
                                               fn () => Unified (Record r),
                                               fn (ls, ls') =>
                                               notUnifiable
                                               (LayoutPretty.tuple ls,
                                                LayoutPretty.tuple ls'))
                                         else not ()
                                 | _ => not ())
                          | (Var a, Var a') =>
                               if Tyvar.equals (a, a')
                                  then Unified t
                                  else not ()
                          | _ => not ()
                      val res =
                         case res of
                            NotUnifiable (l, l') =>
                               NotUnifiable (l, l')
                          | Unified ty =>
                               let
                                  val () = Set.union (s, s')
                                  val () = time := Time.min (!time, !time')
                                  val () = equality := Equality.join (!equality, !equality')
                                  val () =
                                     Set.:= (s, {equality = equality,
                                                 plist = plist,
                                                 time = time,
                                                 ty = ty})
                               in
                                  Unified ()
                               end
                   in
                      res
                   end) arg
            and unifyRecords ((fields: (Field.t * t) vector,
                               dots: bool),
                              (fields': (Field.t * t) vector,
                               dots': bool),
                              yes, no) =
               let
                  fun subset (fields, fields',
                              ac, dots, ac', dots',
                              skipBoth) =
                     Vector.fold
                     (fields, (ac, dots, ac', dots'),
                      fn ((f, t), (ac, dots, ac', dots')) =>
                      case Vector.peek (fields', fn (f', _) =>
                                        Field.equals (f, f')) of
                         NONE =>
                            ((f, true, dontCare) :: ac, dots, ac', dots')
                       | SOME (_, t') =>
                            if skipBoth
                               then (ac, dots, ac', dots')
                            else
                               case unify (t, t') of
                                  NotUnifiable (l, l') =>
                                     ((f, false, l) :: ac, dots,
                                      (f, false, l') :: ac', dots')
                                | Unified _ => (ac, true, ac', true))
                  val (ac, dots, ac', dots') =
                     subset (fields, fields', [], dots, [], dots', false)
                  val (ac', dots', ac, dots) =
                     subset (fields', fields, ac', dots', ac, dots, true)
               in
                  case (ac, ac') of
                     ([], []) => yes ()
                   | _ => no (LayoutPretty.record (ac, dots),
                              LayoutPretty.record (ac', dots'))
               end
            val res = unify (t, t')
         in
            case res of
               NotUnifiable ((l, _), (l', _)) =>
                  let
                     val {times, tycons, tyvars} =
                        finishCheckTime ()
                     fun notes () =
                        if List.isEmpty tycons andalso List.isEmpty tyvars
                           then Layout.empty
                           else let
                                   fun doit (xs, lay) =
                                      List.insertionSort
                                      (List.map (xs, fn x => (x, lay x)),
                                       fn ((_, l1), (_, l2)) =>
                                       String.<= (Layout.toString l1,
                                                  Layout.toString l2))
                                   val tycons = doit (tycons, layoutPrettyTycon)
                                   val tyvars = doit (tyvars, layoutPrettyTyvar)
                                   val tys =
                                      List.map (tycons, #2)
                                      @ List.map (tyvars, #2)
                                in
                                   Layout.align
                                   [seq [str "note: ",
                                         if List.length tys > 1
                                            then str "types would escape their scope: "
                                            else str "type would escape its scope: ",
                                         seq (Layout.separate (tys, ", "))],
                                    (Layout.align o List.map)
                                    (tycons, fn (c, _) =>
                                     seq [str "escape from: ",
                                          Region.layout (Tycon.region c)]),
                                    (Layout.align o List.map)
                                    (times, fn t =>
                                     seq [str "escape to: ",
                                          Region.layout (Time.region t)])]
                                end
                  in
                     NotUnifiable (l, l',
                                   {notes = notes})
                  end
             | Unified () => Unified ()
         end

      local
         val {get: Tycon.t -> (t * Tycon.t) option, set, ...} =
            Property.getSetOnce (Tycon.plist, Property.initConst NONE)
      in
         fun setSynonym (c, c') = set (c, SOME (con (c, Vector.new0 ()), c'))
         val synonym = get
      end

      val () =
         List.foreach
         (CharSize.all, fn s =>
          setSynonym (Tycon.char s,
                      Tycon.word (WordSize.fromBits (CharSize.bits s))))
         
      val () =
         List.foreach
         (IntSize.all, fn s =>
          setSynonym (Tycon.int s,
                      Tycon.word (WordSize.fromBits (IntSize.bits s))))

      structure Overload =
         struct
            open Overload

            val defaultType =
               fn Char => con (Tycon.defaultChar (), Vector.new0 ())
                | Int => con (Tycon.defaultInt (), Vector.new0 ())
                | Real => con (Tycon.defaultReal (), Vector.new0 ())
                | Word => con (Tycon.defaultWord (), Vector.new0 ())
         end

      fun 'a simpleHom {con: t * Tycon.t * 'a vector -> 'a,
                        expandOpaque: bool,
                        record: t * (Field.t * 'a) vector -> 'a,
                        replaceSynonyms: bool,
                        var: t * Tyvar.t -> 'a} =
         let
            val unit = con (unit, Tycon.tuple, Vector.new0 ())
            val unknown = unit
            fun sortFields (fields: (Field.t * 'a) list) =
               let
                  val a = Array.fromList fields
                  val () =
                     QuickSort.sortArray (a, fn ((f, _), (f', _)) =>
                                          Field.<= (f, f'))
               in
                  Array.toVector a
               end
            fun unsorted (t, fields: (Field.t * 'a) list) =
               let
                  val v = sortFields fields
               in
                  record (t, v)
               end
            fun genFlexRecord (t, {extra, fields, spine = _}) =
               unsorted (t,
                         List.fold
                         (extra (), fields, fn ({field, tyvar}, ac) =>
                          (field, var (Type.var tyvar, tyvar)) :: ac))
            fun flexRecord (t, {fields, spine}) =
               if Spine.canAddFields spine
                  then Error.bug "TypeEnv.Type.simpleHom: flexRecord"
               else unsorted (t,
                              Spine.foldOverNew
                              (spine, fields, fields, fn (f, ac) =>
                               (f, unit) :: ac))
            fun recursive _ = Error.bug "TypeEnv.Type.simpleHom.recursive"
            val con =
               if not replaceSynonyms
                  then con
               else
                  fn (t, c, ts) =>
                  let
                     val (t, c) =
                        case synonym c of
                           NONE => (t, c)
                         | SOME (t, c) => (t, c)
                  in
                     con (t, c, ts)
                  end
            fun overload (t', ov) =
               let
                  val t = Overload.defaultType ov
                  val _ = unify (t, t',
                                 {layoutPretty = fn _ =>
                                  Error.bug "TypeEnv.Type.simpleHom.overload: layoutPretty",
                                  layoutPrettyTycon = fn _ =>
                                  Error.bug "TypeEnv.Type.simpleHom.overload: layoutPrettyTycon",
                                  layoutPrettyTyvar = fn _ =>
                                  Error.bug "TypeEnv.Type.simpleHom.overload: layoutPrettyTyvar"})
               in
                  con (t, Overload.defaultTycon ov, Vector.new0 ())
               end
         in
            makeHom {con = con,
                     expandOpaque = expandOpaque,
                     flexRecord = flexRecord,
                     genFlexRecord = genFlexRecord,
                     guard = fn _ => NONE,
                     overload = overload,
                     record = fn (t, r) => record (t, Srecord.toVector r),
                     recursive = recursive,
                     unknown = fn _ => unknown,
                     var = var}
         end
   end

structure Scheme =
   struct
      datatype t =
         General of {bound: unit -> Tyvar.t vector,
                     canGeneralize: bool,
                     flexes: Type.genFlexRecord list,
                     tyvars: Tyvar.t vector,
                     ty: Type.t}
       | Mono of Type.t

      val ty =
         fn General {ty, ...} => ty
          | Mono ty => ty

      val dest =
         fn General {bound, ty, ...} => (bound (), ty)
          | Mono ty => (Vector.new0 (), ty)

      val kind =
         fn General {bound, ...} => Kind.Arity (Vector.length (bound ()))
          | Mono _ => Kind.Arity 0

      fun layout s =
         case s of
            Mono t => Type.layout t
          | General {canGeneralize, tyvars, ty, ...} =>
               Layout.record [("canGeneralize", Bool.layout canGeneralize),
                              ("tyvars", Vector.layout Tyvar.layout tyvars),
                              ("ty", Type.layout ty)]

      fun make {canGeneralize, tyvars, ty} =
         if Vector.isEmpty tyvars
            then Mono ty
         else General {bound = fn () => tyvars,
                       canGeneralize = canGeneralize,
                       flexes = [],
                       tyvars = tyvars,
                       ty = ty}

      val fromType = Mono

      fun fromTycon (tycon: Tycon.t): t =
         let
            val kind = Tycon.kind tycon
            val arity =
               case kind of
                  Kind.Arity arity => arity
                | Kind.Nary => Error.bug "TypeEnv.Scheme.fromTycon: Kind.Nary"
            val tyvars =
               Vector.tabulate
               (arity, fn _ =>
                Tyvar.makeNoname {equality = false})
         in
            make
            {canGeneralize = true,
             ty = Type.con (tycon, Vector.map (tyvars, Type.var)),
             tyvars = tyvars}
         end

      fun instantiateAux (t: t, subst) =
         case t of
            Mono ty => {args = fn () => Vector.new0 (),
                        instance = ty}
          | General {canGeneralize, flexes, tyvars, ty, ...} =>
               (case Type.deEta (ty, tyvars) of
                   SOME tycon =>
                      let
                         val types =
                            Vector.mapi
                            (tyvars, fn (i, a) =>
                             subst {canGeneralize = canGeneralize,
                                    equality = Tyvar.isEquality a,
                                    index = i})
                      in
                         {args = fn () => types,
                          instance = Type.con (tycon, types)}
                      end
                 | NONE =>
               let
                  open Type
                  val {destroy = destroyTyvarInst,
                       get = tyvarInst: Tyvar.t -> Type.t option,
                       set = setTyvarInst} =
                     Property.destGetSetOnce (Tyvar.plist,
                                              Property.initConst NONE)
                  val types =
                     Vector.mapi
                     (tyvars, fn (i, a) =>
                      let
                         val t = subst {canGeneralize = canGeneralize,
                                        equality = Tyvar.isEquality a,
                                        index = i}
                         val _ = setTyvarInst (a, SOME t)
                      in
                         t
                      end)
                  type z = {isNew: bool, ty: Type.t}
                  fun isNew {isNew = b, ty = _} = b
                  fun keep ty = {isNew = false, ty = ty}
                  fun con (ty, c, zs) =
                     if Vector.exists (zs, isNew)
                        then {isNew = true,
                              ty = Type.con (c, Vector.map (zs, #ty))}
                     else keep ty
                  val flexInsts = ref []
                  fun genFlexRecord (_, {extra = _, fields, spine}) =
                     let
                        val fields = List.revMap (fields, fn (f, t: z) =>
                                                  (f, #ty t))
                        val flex = newFlex {fields = fields,
                                            spine = spine}
                        val _ = List.push (flexInsts, {flex = flex,
                                                       spine = spine})
                     in
                        {isNew = true,
                         ty = flex}
                     end
                  fun record (t, r) =
                     if Srecord.exists (r, isNew)
                        then {isNew = true,
                              ty = Type.record (Srecord.map (r, #ty))}
                     else keep t
                  fun recursive _ =
                     (* If we get here, there has already been a type error
                      * in the user's program, so we return a new type to avoid
                      * compounding the error.
                      *)
                     {isNew = true,
                      ty = Type.new ()}
                  fun var (ty, a) =
                     case tyvarInst a of
                        NONE => {isNew = false, ty = ty}
                      | SOME ty => {isNew = true, ty = ty}
                  val {ty: Type.t, ...} =
                     Type.hom (ty, {con = con,
                                    expandOpaque = false,
                                    flexRecord = keep o #1,
                                    genFlexRecord = genFlexRecord,
                                    guard = fn _ => NONE,
                                    overload = keep o #1,
                                    record = record,
                                    recursive = recursive,
                                    unknown = keep o #1,
                                    var = var})
                  val _ = destroyTyvarInst ()
                  val flexInsts = !flexInsts
                  fun args (): Type.t vector =
                     Vector.fromList
                     (List.fold
                      (flexes, Vector.toList types,
                       fn ({fields, spine, ...}, ac) =>
                       let
                          fun done peek =
                             Spine.foldOverNew
                             (spine, fields, ac, fn (f, ac) =>
                              (case peek f of
                                  NONE => Type.unit
                                | SOME t => t) :: ac)
                       in
                          case List.peek (flexInsts,
                                          fn {spine = spine', ...} =>
                                          Spine.equals (spine, spine')) of
                             NONE => done (fn _ => NONE)
                           | SOME {flex, ...} =>
                                let
                                   fun peekFields (fields, f) =
                                      Option.map
                                      (List.peek (fields, fn (f', _) =>
                                                  Field.equals (f, f')),
                                       #2)
                                in
                                   done
                                   (case Type.getTy flex of
                                      FlexRecord {fields, ...} =>
                                         (fn f => peekFields (fields, f))
                                    | GenFlexRecord {extra, fields, ...} =>
                                         (fn f =>
                                          case peekFields (fields, f) of
                                             NONE =>
                                                Option.map
                                                (List.peek
                                                 (extra (),
                                                  fn {field, ...} =>
                                                  Field.equals (f, field)),
                                                 Type.var o #tyvar)
                                           | SOME t => SOME t)
                                    | Record r =>
                                         (fn f => Srecord.peek (r, f))
                                    | _ => Error.bug "TypeEnv.instantiate': General:strange flexInst")
                                end
                       end))
               in
                  {args = args,
                   instance = ty}
               end)

      fun apply (s, ts) =
         #instance (instantiateAux (s, fn {index, ...} => Vector.sub (ts, index)))

      fun instantiate s =
         instantiateAux
         (s, fn {canGeneralize, equality, ...} =>
          Type.unknown {canGeneralize = canGeneralize,
                        equality = if equality
                                      then Equality.True
                                      else Equality.Unknown,
                        time = Time.now ()})

      val instantiate =
         Trace.trace ("TypeEnv.Scheme.instantiate", layout, Type.layout o #instance)
         instantiate

      fun fresh s =
         let
            val (tyvars, _) = dest s
            val tyvars = Vector.map (tyvars, Tyvar.makeLike)
         in
            (tyvars, apply (s, (Vector.map (tyvars, Type.var))))
         end

      fun freshEq s =
         let
            val (tyvars, _) = dest s
            val tyvars = Vector.map (tyvars, fn _ => Tyvar.makeNoname {equality = true})
         in
            (tyvars, apply (s, (Vector.map (tyvars, Type.var))))
         end

      fun admitsEquality s =
         let
            val (_, ty) = freshEq s
         in
            case !(Type.equality ty) of
               Equality.False => false
             | Equality.True => true
             | Equality.Unknown => Error.bug "TypeEnv.Scheme.admitsEquality: Unknown"
         end

      val admitsEquality =
         Trace.trace ("TypeEnv.Scheme.admitsEquality", layout, Bool.layout)
         admitsEquality

      fun checkEquality (s, {layoutPrettyTycon}) =
         let
            fun layoutPrettyTyvar _ =
               Error.bug "TypeEnv.Scheme.checkEquality.layoutPrettyTyvar"
            val (_, ty) = freshEq s
         in
            Option.map
            (Type.checkEquality
             (ty, {layoutPrettyTycon = layoutPrettyTycon,
                   layoutPrettyTyvar = layoutPrettyTyvar}),
             fn ((l, _), _) => l)
         end

      fun haveUnknowns s: bool =
         let
            fun con (_, _, bs) = Vector.exists (bs, fn b => b)
            fun no _ = false
            val {destroy, hom} =
               Type.makeHom
               {con = con,
                expandOpaque = false,
                flexRecord = fn (_, {fields, ...}) => List.exists (fields, #2),
                genFlexRecord = (fn (_, {fields, ...}) =>
                                 List.exists (fields, #2)),
                guard = fn _ => NONE,
                overload = no,
                record = fn (_, r) => Srecord.exists (r, fn b => b),
                recursive = no,
                unknown = fn _ => true,
                var = no}
            val res = hom (ty s)
            val _ = destroy ()
         in
            res
         end
   end

fun 'a close region =
   let
      val beforeGen = Time.now ()
      val () = Time.tick region
   in
      fn (ensure,
          varTypes,
          {error: 'a * Layout.t * Tyvar.t list -> unit,
           layoutPrettyTycon,
           layoutPrettyTyvar}) =>
      let
         local
            fun checkTime (t, bound) =
               let
                  val {checkTime, finishCheckTime} =
                     Type.makeCheckTime {layoutPrettyTycon = layoutPrettyTycon,
                                         layoutPrettyTyvar = layoutPrettyTyvar}
               in
                  Option.map (checkTime (t, bound), fn z =>
                              (z, finishCheckTime ()))
               end
         in
            val varTypes =
               Vector.map
               (varTypes, fn ({isExpansive, ty, var}) =>
                if not isExpansive
                   then {isExpansive = false,
                         ty = ty}
                   else (case checkTime (ty, beforeGen) of
                            NONE => {isExpansive = true,
                                     ty = ty}
                          | SOME (((l, _), _), {tyvars, ...}) =>
                               (error (var, l, tyvars)
                                ; {isExpansive = false,
                                   ty = ty})))
         end
         val tyvars = Vector.toList ensure
         (* Convert all the unknown types bound at this level into tyvars.
          * Convert all the FlexRecords bound at this level into
          * GenFlexRecords.
          *)
         val (flexes, tyvars) =
            if Vector.forall
               (varTypes, fn {ty, ...} =>
                Time.<= (!(Type.time ty), beforeGen))
               then ([], tyvars)
               else let
                       val flexes = ref []
                       val tyvars = ref tyvars
                       fun flexRecord (t, _) =
                          let
                             val (fields, spine) =
                                case Type.getTy t of
                                   Type.FlexRecord {fields, spine} =>
                                      (fields, spine)
                                 | _ => Error.bug "TypeEnv.close.flexRecord: not FlexRecord"
                             fun newField f =
                                {field = f,
                                 tyvar = Tyvar.makeNoname {equality = false}}
                             val extra =
                                let
                                   val all = ref []
                                   val fields =
                                      List.map (fields, fn (f, _) => (f, ()))
                                in
                                   fn () =>
                                   let
                                      val old = !all
                                      val fields =
                                         List.fold
                                         (old, fields, fn ({field, ...}, ac) =>
                                          (field, ()) :: ac)
                                      val new =
                                         Spine.foldOverNew
                                         (spine, fields, old, fn (f, ac) =>
                                          (newField f) :: ac)
                                      val () = all := new
                                   in
                                      new
                                   end
                                end
                             val gfr = {extra = extra,
                                        fields = fields,
                                        spine = spine}
                             val _ = List.push (flexes, gfr)
                          in
                             Type.setTy
                             (t, Type.GenFlexRecord gfr)
                          end
                       fun unknown (t, Unknown.T {canGeneralize, ...}) =
                          if not canGeneralize
                             then ()
                             else let
                                     val equality = Type.equality t
                                     val a =
                                        Tyvar.makeNoname
                                        {equality =
                                         case !equality of
                                            Equality.False => false
                                          | Equality.True => true
                                          | Equality.Unknown =>
                                               (equality := Equality.False
                                                ; false)}
                                     val _ = List.push (tyvars, a)
                                  in
                                     Type.setTy
                                     (t, Type.Var a)
                                  end
                       fun guard t =
                          if Time.<= (!(Type.time t), beforeGen)
                             then SOME ()
                             else NONE
                       val {destroy, hom} =
                          Type.makeHom
                          {con = fn _ => (),
                           expandOpaque = false,
                           flexRecord = flexRecord,
                           genFlexRecord = fn _ => (),
                           guard = guard,
                           overload = fn _ => (),
                           record = fn _ => (),
                           recursive = fn _ => (),
                           unknown = unknown,
                           var = fn _ => ()}
                       val _ = Vector.foreach (varTypes, hom o #ty)
                       val _ = destroy ()
                    in
                       (!flexes, !tyvars)
                    end
         (* For all fields that were added to the generalized flex records,
          * add a type variable.
          *)
         fun bound () =
            Vector.fromList
            (List.fold
             (flexes, tyvars, fn ({extra, fields, spine}, ac) =>
              let
                 val extra = extra ()
              in
                 Spine.foldOverNew
                 (spine, fields, ac, fn (f, ac) =>
                  case List.peek (extra, fn {field, ...} =>
                                  Field.equals (f, field)) of
                     NONE => Error.bug "TypeEnv.close.bound: GenFlex missing field"
                   | SOME {tyvar, ...} => tyvar :: ac)
              end))
         val schemes =
            Vector.map
            (varTypes, fn {isExpansive, ty} =>
             if isExpansive
                then Scheme.Mono ty
             else Scheme.General {bound = bound,
                                  canGeneralize = true,
                                  flexes = flexes,
                                  tyvars = Vector.fromList tyvars,
                                  ty = ty})
      in
         {bound = bound,
          schemes = schemes}
      end
   end

structure Type =
   struct
      open Type

      fun homConVar {con, expandOpaque, var} =
         let
            fun tuple (t, ts) =
               if 1 = Vector.length ts
                  then Vector.first ts
               else con (t, Tycon.tuple, ts)
         in
            simpleHom {con = con,
                       expandOpaque = expandOpaque,
                       record = fn (t, fs) => tuple (t, Vector.map (fs, #2)),
                       replaceSynonyms = true,
                       var = var}
         end

      fun makeHom {con, expandOpaque, var} =
         homConVar {con = fn (_, c, ts) => con (c, ts),
                    expandOpaque = expandOpaque,
                    var = fn (_, a) => var a}

      fun deRecord t =
         let
            val {hom, destroy} =
               simpleHom
               {con = fn (t, _, _) => (t, NONE),
                expandOpaque = false,
                record = fn (t, fs) => (t,
                                        SOME (Vector.map (fs, fn (f, (t, _)) =>
                                                          (f, t)))),
                replaceSynonyms = true,
                var = fn (t, _) => (t, NONE)}
            val res =
               case #2 (hom t) of
                  NONE => Error.bug "TypeEnv.Type.deRecord"
                | SOME fs => fs
            val _ = destroy ()
         in
            res
         end

      fun deTupleOpt t =
         let
            val {destroy, hom} =
               homConVar
               {con = fn (t, c, ts) => (t,
                                        if Tycon.equals (c, Tycon.tuple)
                                           then SOME (Vector.map (ts, #1))
                                        else NONE),
                expandOpaque = false,
                var = fn (t, _) => (t, NONE)}
            val res = #2 (hom t)
            val _ = destroy ()
         in
            res
         end

      val deTupleOpt =
         Trace.trace ("TypeEnv.Type.deTupleOpt", layout,
                      Option.layout (Vector.layout layout))
         deTupleOpt

      fun hom (t, {con, expandOpaque = e, record, replaceSynonyms = r,
                   var}) =
         let
            val {hom, destroy} =
               simpleHom {con = fn (_, c, v) => con (c, v),
                          expandOpaque = e,
                          record = fn (_, fs) => record (Srecord.fromVector fs),
                          replaceSynonyms = r,
                          var = fn (_, a) => var a}
            val res = hom t
            val _ = destroy ()
         in
            res
         end

      fun copy t =
         hom (t, {con = Type.con,
                  expandOpaque = false,
                  record = Type.record,
                  replaceSynonyms = false,
                  var = Type.var})

      val unify =
         fn (t1, t2, {error, layoutPretty, layoutPrettyTycon, layoutPrettyTyvar}) =>
         case unify (t1, t2, {layoutPretty = layoutPretty,
                              layoutPrettyTycon = layoutPrettyTycon,
                              layoutPrettyTyvar = layoutPrettyTyvar}) of
            NotUnifiable (l1, l2, extra) => error (l1, l2, extra)
          | Unified () => ()

      val checkTime =
         fn (t, bound, {layoutPrettyTycon, layoutPrettyTyvar}) =>
         let
            val {checkTime, finishCheckTime} =
               makeCheckTime {layoutPrettyTycon = layoutPrettyTycon,
                              layoutPrettyTyvar = layoutPrettyTyvar}
         in
            Option.map
            (checkTime (t, bound), fn ((l, _), (ty, _)) =>
             (l, ty, let val {tycons, tyvars, ...} = finishCheckTime ()
                     in {tycons = tycons, tyvars = tyvars}
                     end))
         end
   end

end
