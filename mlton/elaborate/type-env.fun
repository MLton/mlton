(* Copyright (C) 2009-2010,2012,2017 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
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

structure LayoutPretty =
   struct
      type t = Layout.t * ({isChar: bool} * Tycon.BindingStrength.t)

      fun simple (l: Layout.t): t =
         (l, ({isChar = false}, Tycon.BindingStrength.unit))

      val dontCare: t = simple (str "_")
      fun bracket ((l, ({isChar}, _)): t): t =
         (Layout.bracket l, ({isChar = isChar}, Tycon.BindingStrength.unit))
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

      fun makeLayoutPretty () =
         let
            val {destroy, get = layoutPretty, set = setLayoutPretty, ...} =
               Property.destGetSet (plist, Property.initFun Tyvar.layout)
            fun localInit bs =
               let
                  val c = Counter.new (Char.toInt #"a")
               in
                  Vector.foreach
                  (bs, fn b =>
                   let
                      val n = Counter.next c
                   in
                      setLayoutPretty
                      (b, Layout.str (concat
                                      [if isEquality b then "''" else "'",
                                       if n > Char.toInt #"z"
                                          then concat ["a", Int.toString (n - Char.toInt #"z")]
                                          else Char.toString (Char.fromInt n)]))
                   end)
               end
         in
            {destroy = destroy,
             layoutPretty = layoutPretty,
             localInit = localInit}
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
            val admitsEquality = make #admitsEquality
            val kind = make #kind
            val prettyDefault = make #prettyDefault
            val region = make #region
            val time = make #time
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
            make {admitsEquality = ! (admitsEquality c),
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
      type t

      val and2: t * t -> t
      val andd: t vector -> t
      val applyTycon: Tycon.t * t vector -> t
      val falsee: t
      val fromBool: bool -> t
      val or2: t * t -> t
      val toBoolOpt: t -> bool option
      val truee: t
      val unify: t * t -> bool
      val unknown: unit -> t
   end =
   struct
      datatype maybe =
         Known of bool
       | Unknown of {whenKnown: (bool -> bool) list ref}
      datatype t =
         False
       | Lazy of unit -> t
       | Maybe of maybe ref
       | True

      fun unknown () = Maybe (ref (Unknown {whenKnown = ref []}))

      fun set (e: t, b: bool): bool =
         case e of
            False => b = false
          | Lazy th => set (th (), b)
          | Maybe r =>
               (case !r of
                   Known b' => b = b'
                 | Unknown {whenKnown} =>
                      (r := Known b; List.forall (!whenKnown, fn f => f b)))
          | True => b = true

      fun when (e: t, f: bool -> bool): bool =
         case e of
            False => f false
          | Lazy th => when (th (), f)
          | Maybe r =>
               (case !r of
                   Known b => f b
                 | Unknown {whenKnown} => (List.push (whenKnown, f); true))
          | True => f true

      fun unify (e: t, e': t): bool =
         when (e, fn b => set (e', b))
         andalso when (e', fn b => set (e, b))

      fun and2 (e, e') =
         case (e, e') of
            (False, _) => False
          | (_, False) => False
          | (True, _) => e'
          | (_, True) => e
          | (Lazy th, e') => Lazy (fn () => and2 (th (), e'))
          | (e, Lazy th') => Lazy (fn () => and2 (e, th' ()))
          | (Maybe r, Maybe r') =>
               (case (!r, !r') of
                   (Known false, _) => False
                 | (_, Known false) => False
                 | (Known true, _) => e'
                 | (_, Known true) => e
                 | (Unknown _, Unknown _) =>
                      let
                         val e'' = unknown ()
                         val _ =
                            when
                            (e'', fn b =>
                             if b
                                then set (e, true) andalso set (e', true)
                             else
                                let
                                   fun dep (e, e') =
                                      when (e, fn b =>
                                            not b orelse set (e', false))
                                in
                                   dep (e, e') andalso dep (e', e)
                                end)
                         fun dep (e, e') =
                            when (e, fn b =>
                                  if b then unify (e', e'')
                                  else set (e'', false))
                         val _ = dep (e, e')
                         val _ = dep (e', e)
                      in
                         e''
                      end)

      fun or2 (e, e') =
         case (e, e') of
            (False, _) => e'
          | (_, False) => e
          | (True, _) => True
          | (_, True) => True
          | (Lazy th, e') => Lazy (fn () => or2 (th (), e'))
          | (e, Lazy th') => Lazy (fn () => or2 (e, th' ()))
          | (Maybe r, Maybe r') =>
               (case (!r, !r') of
                   (Known false, _) => e'
                 | (_, Known false) => e
                 | (Known true, _) => True
                 | (_, Known true) => True
                 | (Unknown _, Unknown _) =>
                      let
                         val e'' = unknown ()
                         val _ =
                            when
                            (e'', fn b =>
                             if not b
                                then set (e, false) andalso set (e', false)
                             else
                                let
                                   fun dep (e, e') =
                                      when (e, fn b =>
                                            b orelse set (e', true))
                                in
                                   dep (e, e') andalso dep (e', e)
                                end)
                         fun dep (e, e') =
                            when (e, fn b =>
                                  if not b then unify (e', e'')
                                  else set (e'', true))
                         val _ = dep (e, e')
                         val _ = dep (e', e)
                      in
                         e''
                      end)

      val falsee = False
      val truee = True

      val fromBool = fn false => False | true => True

      fun toBoolOpt (e: t): bool option =
         case e of
            False => SOME false
          | Lazy th => toBoolOpt (th ())
          | Maybe r =>
               (case !r of
                   Known b => SOME b
                 | Unknown _ => NONE)
          | True => SOME true

      fun andd (es: t vector): t = Vector.fold (es, truee, and2)

      val applyTycon: Tycon.t * t vector -> t =
         fn (c, es) =>
         let
            datatype z = datatype AdmitsEquality.t
         in
            case !(Tycon.admitsEquality c) of
               Always => truee
             | Sometimes =>
                  let
                     val e = andd es
                  in
                     case e of
                        False => falsee
                      | _ =>
                           Lazy
                           (fn () =>
                            case !(Tycon.admitsEquality c) of
                               Always => Error.bug "TypeEnv.Equality.applyTycon: Always"
                             | Sometimes => e
                             | Never => falsee)
                  end
             | Never => falsee
         end
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

      fun layoutPretty _ = simple (str "???")

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
               fn Char => Equality.truee
                | Int => Equality.truee
                | Real => Equality.falsee
                | Word => Equality.truee
         end

      (* Tuples of length <> 1 are always represented as records.
       * There will never be tuples of length one.
       *)
      datatype t = T of {equality: Equality.t,
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

      val newCloses: t list ref = ref []

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
               val {time, ty, ...} = Set.! s
            in
               record
               [("time", Time.layout (!time)),
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
         Tycon.makeLayoutAppPretty
         {layoutPretty = fn _ => Error.bug "TypeEnv.LayoutPretty.tuple: layoutPretty"}
         (Tycon.tuple, ls)
   end

structure Type =
   struct
      open Type

      fun admitsEquality t =
         case Equality.toBoolOpt (equality t) of
            NONE =>
               (* Could report an error here, but sometimes in a type-incorrect
                * program, there will be unknown equalities.  So it is better
                * to conservatively return equality true, which will cause fewer
                * spurious errors.
                *)
               true
          | SOME b => b

      val admitsEquality =
         Trace.trace 
         ("TypeEnv.Type.admitsEquality", layout, Bool.layout) 
         admitsEquality

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
            val layoutAppPretty =
               Tycon.makeLayoutAppPretty {layoutPretty = layoutPrettyTycon}
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
            fun unknown (_, u) = Unknown.layoutPretty u
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
            val res = #1 (layoutPretty t)
            val () = destroy ()
         in
            res
         end

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
         T (Set.singleton {equality = equality,
                           plist = PropertyList.new (),
                           time = ref time,
                           ty = ty})

      fun newTy (ty: ty): t =
         let
            val (equality, time) =
               case ty of
                  Con (c, ts) =>
                     (Equality.applyTycon
                      (c, Vector.map (ts, equality)),
                      Vector.fold
                      (ts, Tycon.time c, fn (t, t') =>
                       Time.max (!(time t), t')))
                | GenFlexRecord _ =>
                     Error.bug "TypeEnv.Type.newTy: GenFlexRecord"
                | FlexRecord {fields, ...} =>
                     (Equality.and2
                      (Equality.andd (Vector.fromListMap (fields, equality o #2)),
                       Equality.unknown ()),
                      Time.now ())
                | Overload ov =>
                     (Overload.admitsEquality ov,
                      Time.zero)
                | Record r =>
                     (Equality.andd
                      (Vector.map (Srecord.range r, equality)),
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

      fun unknown {canGeneralize, equality, time} =
         let
            val u = Unknown.new {canGeneralize = canGeneralize}
            val t = make {equality = equality,
                          time = time,
                          ty = Unknown u}
            val _ = List.push (newCloses, t)
         in
            t
         end

      fun newAt time = unknown {canGeneralize = true,
                                equality = Equality.unknown (),
                                time = time}

      fun new () = newAt (Time.now ())

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
            val _ = List.push (newCloses, t)
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

      fun explainAdmitsEquality (T s, {layoutPrettyTyvar: Tyvar.t -> Layout.t}) =
         let
            val {ty, ...} = Set.! s
         in
            case ty of
               Var a => bracket (simple (layoutPrettyTyvar a))
             | _ => simple (Layout.bracket (str "<equality>"))
         end
      fun explainDoesNotAdmitEquality (t , {layoutPrettyTycon: Tycon.t -> Layout.t,
                                            layoutPrettyTyvar: Tyvar.t -> Layout.t}) =
         let
            val layoutAppPretty =
               Tycon.makeLayoutAppPretty {layoutPretty = layoutPrettyTycon}
            fun deopt lo = Option.fold (lo, dontCare, fn (l, _) => l)
            val noneq = simple (Layout.bracket (str "<non-equality>"))
            fun con (_, c, los) =
               let
                  datatype z = datatype AdmitsEquality.t
               in
                  case ! (Tycon.admitsEquality c) of
                     Always => Error.bug "TypeEnv.Type.explainDoesNotAdmitEquality.con"
                   | Never =>
                        (SOME o bracket o layoutAppPretty)
                        (c, Vector.map (los, fn _ => dontCare))
                   | Sometimes =>
                        (SOME o layoutAppPretty)
                        (c, Vector.map (los, deopt))
               end
            fun doRecord (fields, extra) =
               SOME (LayoutPretty.record
                     (List.map
                      (fields, fn (f, l) =>
                       (f, false, l)),
                      extra))
            fun flexRecord (_, {fields, spine = _}) =
               doRecord (List.keepAllMap
                         (fields, fn (f, lo) =>
                          Option.map (lo, fn l => (f, l))),
                         true)
            fun genFlexRecord (_, {extra = _, fields, spine = _}) =
               doRecord (List.keepAllMap
                         (fields, fn (f, lo) =>
                          Option.map (lo, fn l => (f, l))),
                         true)
            fun overload (_, ov) =
               case ov of
                  Overload.Real => SOME (simple (Layout.bracket (str "<real?>")))
                | _ => Error.bug "TypeEnv.Type.explainDoesNotAdmitEquality.overload"
            fun record (_, r) =
               case Srecord.detupleOpt r of
                  NONE =>
                     let
                        val fields = Srecord.toVector r
                        val fields' =
                           Vector.keepAllMap
                           (fields, fn (f, lo) =>
                            Option.map (lo, fn l => (f, l)))
                     in
                        doRecord (Vector.toList fields',
                                  not (Vector.length fields = Vector.length fields'))
                     end
                | SOME los => SOME (LayoutPretty.tuple (Vector.map (los, deopt)))
            fun recursive _ = Error.bug "TypeEnv.Type.explainDoesNotAdmitEquality.recursive"
            fun unknown (_, _) = SOME noneq
            fun var (_, a) = SOME (bracket (simple (layoutPrettyTyvar a)))
            val res =
               hom (t, {con = con,
                        expandOpaque = false,
                        flexRecord = flexRecord,
                        genFlexRecord = genFlexRecord,
                        guard = fn t => if admitsEquality t
                                           then SOME NONE
                                           else NONE,
                        overload = overload,
                        record = record,
                        recursive = recursive,
                        unknown = unknown,
                        var = var})
         in
            case res of
               NONE => noneq
             | SOME l => l
         end

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
      fun makeCheckTime {layoutPretty: t -> LayoutPretty.t,
                         layoutPrettyTycon: Tycon.t -> Layout.t,
                         layoutPrettyTyvar: Tyvar.t -> Layout.t} =
         let
            val layoutAppPretty =
               Tycon.makeLayoutAppPretty {layoutPretty = layoutPrettyTycon}
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
                          val ty = Type.newAt bound
                       in
                          List.push (times, bound)
                          ; List.push (tycons, c)
                          ; SOME ((bracket o layoutAppPretty)
                                  (c, Vector.map (rs, getLay2)),
                                  layoutAppPretty
                                  (c, Vector.map (rs, getLay2)),
                                  bracket (layoutPretty ty),
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
                          val ty = newAt bound
                       in
                          List.push (times, bound)
                          ; List.push (tyvars, a)
                          ; SOME (bracket (simple (layoutPrettyTyvar a)),
                                  simple (layoutPrettyTyvar a),
                                  bracket (layoutPretty ty),
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
                          val res : lll option * t =
                             hom (t, {con = wrap (con bound, #1),
                                      expandOpaque = false,
                                      flexRecord = wrap (flexRecord, #1),
                                      genFlexRecord = genFlexRecord,
                                      guard = fn t => if Time.<= (!(time t), bound)
                                                         then SOME (NONE, t)
                                                         else NONE,
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
            val layoutAppPretty =
               Tycon.makeLayoutAppPretty {layoutPretty = layoutPrettyTycon}
            val {checkTime, finishCheckTime} =
               makeCheckTime {layoutPretty = layoutPretty,
                              layoutPrettyTycon = layoutPrettyTycon,
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
                      fun flexToFlexToRecord (fields, spine, time, outer, spine') =
                         let
                            val () =
                               List.foreach
                               (Spine.fields spine', fn f' =>
                                ignore (Spine.ensureField (spine, f')))
                            val fields =
                               Spine.foldOverNew
                               (spine, fields, fields, fn (f, fields) =>
                                (f, newAt time) :: fields)
                            val _ = setTy (outer, FlexRecord {fields = fields, spine = spine})
                         in
                            flexToRecord (fields, spine)
                         end
                      fun flexToRigidToRecord (fields, spine, time, outer, r') =
                         let
                            val () =
                               Vector.foreach
                               (Srecord.toVector r', fn (f', _) =>
                                ignore (Spine.ensureField (spine, f')))
                            val () = Spine.noMoreFields spine
                            val fields =
                               Spine.foldOverNew
                               (spine, fields, fields, fn (f, fields) =>
                                (f, newAt time) :: fields)
                            val r = Srecord.fromVector (Vector.fromList fields)
                            val _ = setTy (outer, Record r)
                         in
                            rigidToRecord r
                         end
                      fun oneFlex ({fields, spine}, time, outer, r', swap) =
                         unifyRecords
                         (flexToRigidToRecord (fields, spine, time, outer, r'),
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
                                      time: Time.t,
                                      outer: t,
                                      t': Type.ty,
                                      outer': Type.t,
                                      swap: bool) =
                         let
                            (* This should fail if the unknown occurs in t.
                             *)
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
                               else (case checkTime (outer', time) of
                                        NONE => Unified t'
                                      | SOME (l, (t'', l'')) =>
                                           (setTy (outer, getTy t'')
                                            ; notUnifiable
                                              (if swap
                                                  then (l, l'')
                                                  else (l'', l))))
                         end
                      val res =
                         case (t, t') of
                            (Unknown r, Unknown r') =>
                               Unified (Unknown (Unknown.join (r, r')))
                          | (Unknown u, _) =>
                               oneUnknown (u, !time, outer, t', outer', false)
                          | (_, Unknown u') =>
                               oneUnknown (u', !time', outer', t, outer, true)
                          | (Con (c, ts), _) => conAnd (c, ts, t', t, false)
                          | (_, Con (c, ts)) => conAnd (c, ts, t, t', true)
                          | (FlexRecord f, Record r') =>
                               oneFlex (f, !time, outer, r', false)
                          | (Record r, FlexRecord f') =>
                               oneFlex (f', !time', outer', r, true)
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
                                  (flexToFlexToRecord (fields, spine, !time, outer, spine'),
                                   flexToFlexToRecord (fields', spine', !time', outer', spine),
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
                            NotUnifiable _ => res
                          | Unified _ =>
                               (if Equality.unify (equality, equality')
                                   then res
                                   else let
                                           fun explain t =
                                              if admitsEquality t
                                                 then explainAdmitsEquality
                                                      (t, {layoutPrettyTyvar = layoutPrettyTyvar})
                                                 else explainDoesNotAdmitEquality
                                                      (t, {layoutPrettyTycon = layoutPrettyTycon,
                                                           layoutPrettyTyvar = layoutPrettyTyvar})
                                        in
                                           NotUnifiable (explain outer,
                                                         explain outer')
                                        end)
                      val res =
                         case res of
                            NotUnifiable (l, l') =>
                               NotUnifiable (l, l')
                          | Unified t =>
                               let
                                  val () = Set.union (s, s')
                                  val () =
                                     if Time.<= (!time, !time')
                                        then ()
                                        else time := !time'
                                  val () =
                                     Set.:= (s, {equality = equality,
                                                 plist = plist,
                                                 time = time,
                                                 ty = t})
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

      fun instantiate' (t: t, subst) =
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
         #instance (instantiate' (s, fn {index, ...} => Vector.sub (ts, index)))

      fun instantiate s =
         instantiate'
         (s, fn {canGeneralize, equality, ...} =>
          Type.unknown {canGeneralize = canGeneralize,
                        equality = if equality
                                      then Equality.truee
                                      else Equality.unknown (),
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

      fun admitsEquality s =
         Type.admitsEquality
         (#instance
          (instantiate'
           (s, fn {canGeneralize, ...} =>
            Type.unknown {canGeneralize = canGeneralize,
                          equality = Equality.truee,
                          time = Time.now ()})))

      val admitsEquality =
         Trace.trace ("TypeEnv.Scheme.admitsEquality", layout, Bool.layout)
         admitsEquality

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
      val genTime = Time.now ()
      val savedCloses = !Type.newCloses
      val () = Type.newCloses := []
   in
      fn (ensure,
          varTypes,
          {error: 'a * Layout.t * Tyvar.t list -> unit,
           layoutPrettyType,
           layoutPrettyTycon,
           layoutPrettyTyvar}) =>
      let
         local
            fun checkTime (t, bound) =
               let
                  val {checkTime, finishCheckTime} =
                     Type.makeCheckTime {layoutPretty = layoutPrettyType,
                                         layoutPrettyTycon = layoutPrettyTycon,
                                         layoutPrettyTyvar = layoutPrettyTyvar}
               in
                  Option.map (checkTime (t, bound), fn z =>
                              (z, finishCheckTime ()))
               end
         in
            val varTypes =
               Vector.map
               (varTypes, fn ({isExpansive, ty, var}) =>
                {isExpansive = isExpansive,
                 ty = if not isExpansive
                         then ty
                         else (case checkTime (ty, beforeGen) of
                                  NONE => ty
                                | SOME (((l, _), (ty', _)), {tyvars, ...}) =>
                                     (error (var, l, tyvars)
                                      ; ty'))})
         end
         val flexes = ref []
         val tyvars = ref (Vector.toList ensure)
         (* Convert all the unknown types bound at this level into tyvars.
          * Convert all the FlexRecords bound at this level into
          * GenFlexRecords.
          *)
         val newCloses =
            List.fold
            (!Type.newCloses, savedCloses, fn (t as Type.T s, ac) =>
             let
                val {equality, plist, time, ty, ...} = Set.! s
                val _ =
                   if true then () else
                      Layout.outputl (seq [str "considering ",
                                           Type.layout t,
                                           str " with time ",
                                           Time.layout (!time),
                                           str " where getTime is ",
                                           Time.layout genTime],
                                      Out.error)
             in
                if not (Time.<= (genTime, !time))
                   then t :: ac
                else
                   case ty of
                      Type.FlexRecord {fields, spine, ...} =>
                         let
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
                            val _ = 
                               Set.:=
                               (s, {equality = equality,
                                    plist = plist,
                                    time = time,
                                    ty = Type.GenFlexRecord gfr})
                         in
                            ac
                         end
                    | Type.Unknown (Unknown.T {canGeneralize, ...}) =>
                         if not canGeneralize
                            then t :: ac
                         else
                            let
                               val b =
                                  case Equality.toBoolOpt equality of
                                     NONE =>
                                        let
                                           val _ =
                                              Equality.unify
                                              (equality, Equality.falsee)
                                        in
                                           false
                                        end
                                   | SOME b => b
                               val a = Tyvar.makeNoname {equality = b}
                               val _ = List.push (tyvars, a)
                               val _ =
                                  Set.:= (s, {equality = equality,
                                              plist = PropertyList.new (),
                                              time = time,
                                              ty = Type.Var a})
                            in
                               ac
                            end
                    | _ => ac
             end)
         val _ = Type.newCloses := newCloses
         val flexes = !flexes
         val tyvars = !tyvars
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

      val unify =
         fn (t1, t2, {error, layoutPretty, layoutPrettyTycon, layoutPrettyTyvar}) =>
         case unify (t1, t2, {layoutPretty = layoutPretty,
                              layoutPrettyTycon = layoutPrettyTycon,
                              layoutPrettyTyvar = layoutPrettyTyvar}) of
            NotUnifiable (l1, l2, extra) => error (l1, l2, extra)
          | Unified () => ()

      val explainDoesNotAdmitEquality = #1 o explainDoesNotAdmitEquality

      val checkTime =
         fn (t, bound, {layoutPretty, layoutPrettyTycon, layoutPrettyTyvar}) =>
         let
            val {checkTime, finishCheckTime} =
               makeCheckTime {layoutPretty = layoutPretty,
                              layoutPrettyTycon = layoutPrettyTycon,
                              layoutPrettyTyvar = layoutPrettyTyvar}
            val res =
               Option.map
               (checkTime (t, bound), fn ((l, _), (ty, _)) =>
                (l, ty, let val {tycons, tyvars, ...} = finishCheckTime ()
                        in {tycons = tycons, tyvars = tyvars}
                        end))
         in
            res
         end
   end

end
