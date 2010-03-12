(* Copyright (C) 2009-2010 Matthew Fluet.
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

structure AdmitsEquality = Tycon.AdmitsEquality
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
      val useBeforeDef: t * Tycon.t -> unit
      val layout: t -> Layout.t
      val now: unit -> t
      val tick: {useBeforeDef: Tycon.t -> unit} -> unit
   end =
   struct
      datatype t = T of {clock: int,
                         useBeforeDef: Tycon.t -> unit}

      local
         fun make f (T r) = f r
      in
         val clock = make #clock
      end

      fun useBeforeDef (T {useBeforeDef = f, ...}, c) = f c

      val layout = Int.layout o clock

      fun t <= t' = Int.<= (clock t, clock t')

      local
         val current: t ref =
            ref (T {clock = 0,
                    useBeforeDef = fn _ => Error.bug "TypeEnv.Time: useBeforeDef clock 0"})
      in
         fun now () = !current
         fun tick {useBeforeDef} =
            current := T {clock = 1 + clock (!current),
                          useBeforeDef = useBeforeDef}
      end

      val tick = Trace.trace ("TypeEnv.Time.tick", Layout.ignore, Unit.layout) tick
   end

val tick = Time.tick

structure Lay =
   struct
      type t = Layout.t * ({isChar: bool} * Tycon.BindingStrength.t)

      fun simple (l: Layout.t): t =
         (l, ({isChar = false}, Tycon.BindingStrength.unit))
   end

structure UnifyResult =
   struct
      datatype t =
         NotUnifiable of Lay.t * Lay.t
       | Unified

      val layout =
         let
            open Layout
         in
            fn NotUnifiable _ => str "NotUnifiable"
             | Unified => str "Unified"
         end
   end

val {get = tyconInfo: Tycon.t -> {admitsEquality: AdmitsEquality.t ref,
                                  region: Region.t option ref,
                                  time: Time.t ref},
     set = setTyconInfo, ...} =
   Property.getSet (Tycon.plist, Property.initRaise ("info", Tycon.layout))

local
   fun make f = f o tyconInfo
in
   val tyconAdmitsEquality = make #admitsEquality
   val tyconRegion = make #region
   val tyconTime = make #time
end

fun initAdmitsEquality (c, a) =
   setTyconInfo (c, {admitsEquality = ref a,
                     region = ref NONE,
                     time = ref (Time.now ())})

val _ = List.foreach (Tycon.prims, fn {tycon = c, admitsEquality = a, ...} =>
                      initAdmitsEquality (c, a))

structure Equality:>
   sig
      type t

      val and2: t * t -> t
      val andd: t vector -> t
      val applyTycon: Tycon.t * t vector -> t
      val falsee: t
      val fromBool: bool -> t
      val toBoolOpt: t -> bool option
      val truee: t
      val unify: t * t -> UnifyResult.t
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
            case !(tyconAdmitsEquality c) of
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
                            case !(tyconAdmitsEquality c) of
                               Always => Error.bug "TypeEnv.Equality.applyTycon: Always"
                             | Sometimes => e
                             | Never => falsee)
                  end
             | Never => falsee
         end

      val unify: t * t -> UnifyResult.t =
         fn (e, e') =>
         if unify (e, e')
            then UnifyResult.Unified
         else
            let
               fun lay e =
                  Lay.simple
                  (Layout.str (case toBoolOpt e of
                                  NONE => Error.bug "TypeEnv.Equality.unify"
                                | SOME b =>
                                     if b
                                        then "[<equality>]"
                                     else "[<non-equality>]"))
            in
               UnifyResult.NotUnifiable (lay e, lay e')
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
         let
            open Layout
         in
            seq [str "Unknown ",
                 record [("canGeneralize", Bool.layout canGeneralize),
                         ("id", Int.layout id)]]
         end

      fun equals (u, u') = id u = id u'

      local
         val r: int ref = ref 0
      in
         fun newId () = (Int.inc r; !r)
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
         val r: int ref = ref 0
      in
         fun newId () = (Int.inc r; !r)
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

val {get = tyvarTime: Tyvar.t -> Time.t ref, ...} =
   Property.get (Tyvar.plist, Property.initFun (fn _ => ref (Time.now ())))

val tyvarTime =
   Trace.trace ("TypeEnv.tyvarTime", Tyvar.layout, Ref.layout Time.layout) tyvarTime

local
   type z = Layout.t * ({isChar: bool} * Tycon.BindingStrength.t)
   open Layout
in
   fun simple (l: Layout.t): z =
      (l, ({isChar = false}, Tycon.BindingStrength.unit))
   val dontCare: z = simple (str "_")
   fun bracket l = seq [str "[", l, str "]"]
   fun layoutRecord (ds: (Field.t * bool * z) list, flexible: bool) =
      simple (case ds of
                 [] => if flexible then str "{...}" else str "{}"
               | _ => 
                    seq [str "{",
                         mayAlign
                         (separateRight
                          (List.map
                           (QuickSort.sortList (ds, fn ((f, _, _), (f', _, _)) =>
                                                Field.<= (f, f')),
                            fn (f, b, (l, _)) =>
                            let
                               val f = Field.layout f
                               val f = if b then bracket f else f
                            in
                               seq [f, str ": ", l]
                            end),
                           ",")),
                         str (if flexible
                                 then ", ...}"
                              else "}")])
   fun layoutTuple (zs: z vector): z =
      Tycon.layoutApp (Tycon.tuple, zs)
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
         val toType: t -> ty = make #ty
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

      val {get = opaqueTyconExpansion: Tycon.t -> (t vector -> t) option,
           set = setOpaqueTyconExpansion, ...} =
         Property.getSet (Tycon.plist, Property.initConst NONE)

      val opaqueTyconExpansion =
         Trace.trace 
         ("TypeEnv.Type.opaqueTyconExpansion", Tycon.layout, Layout.ignore)
         opaqueTyconExpansion

      fun makeHom {con, expandOpaque, flexRecord, genFlexRecord, overload,
                   record, recursive, unknown, var} =
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
                          let
                             val _ = r := Processing
                             fun loopFields fields =
                                List.revMap (fields, fn (f, t) => (f, get t))
                             val res = 
                                case toType t of
                                   Con (c, ts) =>
                                      let
                                         fun no () =
                                            con (t, c, Vector.map (ts, get))
                                         fun yes () =
                                            (case opaqueTyconExpansion c of
                                                NONE => no ()
                                              | SOME f => get (f ts))
                                      in
                                         if expandOpaque then yes () else no ()
                                      end
                                 | FlexRecord {fields, spine} =>
                                      flexRecord (t, {fields = loopFields fields,
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

      fun makeLayoutPretty {expandOpaque, localTyvarNames} : 
         {destroy: unit -> unit,
          lay: t -> Layout.t * ({isChar: bool} * Tycon.BindingStrength.t)} =
         let
            val str = Layout.str
            fun con (_, c, ts) = Tycon.layoutApp (c, ts)
            fun con0 c = Tycon.layoutApp (c, Vector.new0 ())
            fun flexRecord (_, {fields, spine}) =
               layoutRecord
               (List.fold
                (fields,
                 Spine.foldOverNew (spine, fields, [], fn (f, ac) =>
                                    (f, false, simple (str "#???"))
                                    :: ac),
                 fn ((f, t), ac) => (f, false, t) :: ac),
                Spine.canAddFields spine)
            fun genFlexRecord (_, {extra, fields, spine}) =
               layoutRecord
               (List.fold
                (fields,
                 List.revMap (extra (), fn {field, tyvar} =>
                              (field, false, simple (Tyvar.layout tyvar))),
                 fn ((f, t), ac) => (f, false, t) :: ac),
                Spine.canAddFields spine)
            fun overload (_, ov) = con0 (Overload.defaultTycon ov)
            fun record (_, r) =
               case Srecord.detupleOpt r of
                  NONE =>
                     layoutRecord (Vector.toListMap (Srecord.toVector r,
                                                     fn (f, t) => (f, false, t)),
                                   false)
                | SOME ts => Tycon.layoutApp (Tycon.tuple, ts)
            fun recursive _ = simple (str "<recur>")
            fun unknown _ = simple (str "???")
            val (destroy, prettyTyvar) =
               if localTyvarNames
                  then let
                          val {destroy, get = prettyTyvar, ...} =
                             Property.destGet
                             (Tyvar.plist,
                              Property.initFun
                              (let
                                  val r = ref (Char.toInt #"a")
                               in
                                  fn _ =>
                                  let
                                     val n = !r
                                     val l =
                                        simple
                                        (str (concat
                                              ["'",
                                               if n > Char.toInt #"z" 
                                                  then concat ["a", Int.toString (n - Char.toInt #"z")]
                                               else Char.toString (Char.fromInt n )]))
                                     val _ = r := 1 + n
                                  in
                                     l
                                  end
                               end))
                       in
                          (destroy, prettyTyvar)
                       end
               else (fn () => (), simple o Tyvar.layout)
            fun var (_, a) = prettyTyvar a
            fun lay t =
               hom (t, {con = con,
                        expandOpaque = expandOpaque,
                        flexRecord = flexRecord,
                        genFlexRecord = genFlexRecord,
                        overload = overload,
                        record = record,
                        recursive = recursive,
                        unknown = unknown,
                        var = var})
         in
            {destroy = destroy,
             lay = lay}
         end

      fun layoutPrettyAux (t, {expandOpaque, localTyvarNames}) =
         let
            val {destroy, lay} = 
               makeLayoutPretty {expandOpaque = expandOpaque,
                                 localTyvarNames = localTyvarNames}
            val res = #1 (lay t)
            val _ = destroy ()
         in
            res
         end
      fun layoutPretty t = 
         layoutPrettyAux (t, {expandOpaque = false,
                              localTyvarNames = true})

      fun deConOpt t =
         case toType t of
            Con x => SOME x
          | _ => NONE

      fun deEta (t: t, tyvars: Tyvar.t vector): Tycon.t option =
         case deConOpt t of
            SOME (c, ts) =>
               if Vector.length ts = Vector.length tyvars
                  andalso Vector.foralli (ts, fn (i, t) =>
                                          case toType t of
                                             Var a =>
                                                Tyvar.equals
                                                (a, Vector.sub (tyvars, i))
                                           | _ => false)
                  then SOME c
               else NONE
           | _ => NONE


      fun newTy (ty: ty, eq: Equality.t): t =
         T (Set.singleton {equality = eq,
                           plist = PropertyList.new (),
                           time = ref (Time.now ()),
                           ty = ty})

      fun unknown {canGeneralize, equality} =
         let
            val t = newTy (Unknown (Unknown.new {canGeneralize = canGeneralize}),
                           equality)
            val _ = List.push (newCloses, t)
         in
            t
         end

      fun new () = unknown {canGeneralize = true,
                            equality = Equality.unknown ()}

      val new = Trace.trace ("TypeEnv.Type.new", Unit.layout, layout) new

      fun newFlex {fields, spine} =
         newTy (FlexRecord {fields = fields,
                            spine = spine},
                Equality.and2
                (Equality.andd (Vector.fromListMap (fields, equality o #2)),
                 Equality.unknown ()))

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

      fun record r =
         newTy (Record r,
                Equality.andd (Vector.map (Srecord.range r, equality)))

      fun tuple ts =
         if 1 = Vector.length ts
            then Vector.sub (ts, 0)
         else newTy (Record (Srecord.tuple ts),
                     Equality.andd (Vector.map (ts, equality)))

      fun con (tycon, ts) =
         if Tycon.equals (tycon, Tycon.tuple)
            then tuple ts
         else newTy (Con (tycon, ts),
                     Equality.applyTycon (tycon, Vector.map (ts, equality)))

      fun var a = newTy (Var a, Equality.fromBool (Tyvar.isEquality a))
   end

fun setOpaqueTyconExpansion (c, f) =
   Type.setOpaqueTyconExpansion (c, SOME f)

structure Ops = TypeOps (structure Tycon = Tycon
                         open Type)

structure Type =
   struct
      (* Order is important, since want specialized definitions in Type to
       * override general definitions in Ops.
       *)
      open Ops Type

      val unit = tuple (Vector.new0 ())

      fun isArrow t =
         case toType t of
            Con (c, _) => Tycon.equals (c, Tycon.arrow)
          | _ => false

      fun isBool t =
         case toType t of
            Con (c, _) => Tycon.isBool c
          | _ => false

      fun isCharX t =
         case toType t of
            Con (c, _) => Tycon.isCharX c
          | Overload Overload.Char => true
          | _ => false

      fun isCPointer t =
         case toType t of
            Con (c, _) => Tycon.isCPointer c
          | _ => false

      fun isInt t =
         case toType t of
            Con (c, _) => Tycon.isIntX c
          | Overload Overload.Int => true
          | _ => false

      fun isUnit t =
         case toType t of
            Record r =>
               (case Srecord.detupleOpt r of
                   NONE => false
                 | SOME v => 0 = Vector.length v)
          | _ => false

      local
         fun make (ov, eq) () = newTy (Overload ov, eq)
         datatype z = datatype Overload.t
      in
         val unresolvedChar = make (Char, Equality.truee)
         val unresolvedInt = make (Int, Equality.truee)
         val unresolvedReal = make (Real, Equality.falsee)
         val unresolvedWord = make (Word, Equality.truee)
      end

      fun unresolvedString () = vector (unresolvedChar ())

      val traceCanUnify =
         Trace.trace2 
         ("TypeEnv.Type.canUnify", layout, layout, Bool.layout)

      fun canUnify arg = 
         traceCanUnify
         (fn (t, t') =>
          case (toType t, toType t') of
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
               0 = Vector.length ts andalso Overload.matchesTycon (ov, c)
          | _ => false

      (* minTime (t, bound) ensures that all components of t have times no larger
       * than bound.  It calls the appropriate error function when it encounters
       * a tycon that is used before it defined.
       *)
      fun minTime (t, bound: Time.t): unit =
         let
            fun loop (T s): unit =
               let
                  val {time, ty, ...} = Set.! s
               in
                  if Time.<= (!time, bound)
                     then ()
                  else
                     let
                        val _ = time := bound
                     in
                        case ty of
                           Con (c, ts) =>
                              let
                                 val r = tyconTime c
                                 val _ =
                                    if Time.<= (!r, bound)
                                       then ()
                                    else
                                       let
                                          val _ = r := bound
                                          val _ = Time.useBeforeDef (bound, c)
                                       in
                                          ()
                                       end
                                 val _ = Vector.foreach (ts, loop)
                              in
                                 ()
                              end
                         | FlexRecord {fields, ...} => loopFields fields
                         | GenFlexRecord {fields, ...} => loopFields fields
                         | Overload _ => ()
                         | Record r => Srecord.foreach (r, loop)
                         | Unknown _ => ()
                         | Var a =>
                              let
                                 val r = tyvarTime a
                              in
                                 if Time.<= (!r, bound)
                                    then ()
                                 else r := bound
                              end
                     end
               end
            and loopFields (fs: (Field.t * t) list) =
               List.foreach (fs, loop o #2)
            val _ = loop t
         in
            ()
         end

      val minTime =
         Trace.trace2 
         ("TypeEnv.Type.minTime", layout, Time.layout, Unit.layout) 
         minTime

      datatype z = datatype UnifyResult.t

      val traceUnify = 
         Trace.trace2 
         ("TypeEnv.Type.unify", layout, layout, UnifyResult.layout)

      fun unify (t, t', {preError: unit -> unit}): UnifyResult.t =
         let
            val {destroy, lay = layoutPretty} = 
               makeLayoutPretty {expandOpaque = false, localTyvarNames = true}
            val dontCare' = fn _ => dontCare
            val layoutRecord = fn z => layoutRecord (z, true)
            fun unify arg =
               traceUnify
               (fn (outer as T s, outer' as T s') =>
                if Set.equals (s, s')
                   then Unified
                else
                   let
                      fun notUnifiable (l: Lay.t, l': Lay.t) =
                         (NotUnifiable (l, l'),
                          Unknown (Unknown.new {canGeneralize = true}))
                      val bracket =
                         fn (l, ({isChar}, _)) =>
                         (bracket l,
                          ({isChar = isChar}, Tycon.BindingStrength.unit))
                      fun notUnifiableBracket (l, l') =
                         notUnifiable (bracket l, bracket l')
                      fun flexToRecord (fields, spine) =
                         (Vector.fromList fields,
                          Vector.fromList
                          (List.fold
                           (Spine.fields spine, [], fn (f, ac) =>
                            if List.exists (fields, fn (f', _) =>
                                            Field.equals (f, f'))
                               then ac
                            else f :: ac)),
                          fn f => Spine.ensureField (spine, f))
                      fun rigidToRecord r =
                         (Srecord.toVector r,
                          Vector.new0 (),
                          fn f => isSome (Srecord.peek (r, f)))
                      fun oneFlex ({fields, spine}, time, r, outer, swap) =
                         unifyRecords
                         (flexToRecord (fields, spine),
                          rigidToRecord r,
                          fn () => (minTime (outer, time)
                                    ; Spine.noMoreFields spine
                                    ; (Unified, Record r)),
                          fn (l, l') => notUnifiable (if swap
                                                         then (l', l)
                                                      else (l, l')))
                      fun genFlexError () =
                         Error.bug "TypeEnv.Type.unify: GenFlexRecord"
                      val {equality = e, time, ty = t, plist} = Set.! s
                      val {equality = e', time = time', ty = t', ...} =
                         Set.! s'
                      fun not () =
                         (preError ()
                          ; notUnifiableBracket (layoutPretty outer,
                                                 layoutPretty outer'))
                      fun unifys (ts, ts', yes, no) =
                         let
                            val us = Vector.map2 (ts, ts', unify)
                         in
                            if Vector.forall
                               (us, fn Unified => true | _ => false)
                               then yes ()
                            else
                               let
                                  val (ls, ls') =
                                     Vector.unzip
                                     (Vector.mapi
                                      (us, fn (i, u) =>
                                       case u of
                                          Unified =>
                                             let
                                                val z =
                                                   dontCare' (Vector.sub (ts, i))
                                             in
                                                (z, z)
                                             end
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
                                                 val _ = preError ()
                                              in
                                                 notUnifiableBracket
                                                 (maybe (lay ts, lay ts'))
                                              end
                                        else
                                           unifys
                                           (ts, ts',
                                            fn () => (Unified, t),
                                            fn (ls, ls') =>
                                            let 
                                               fun lay ls =
                                                  Tycon.layoutApp (c, ls)
                                            in
                                               notUnifiable
                                               (maybe (lay ls, lay ls'))
                                            end)
                                  else not ()
                             | Overload ov =>
                                  if Vector.isEmpty ts
                                     andalso Overload.matchesTycon (ov, c)
                                     then (Unified, t')
                                  else not ()
                             | _ => not ()
                         end
                      fun oneUnknown (u: Unknown.t, time,
                                      t: Type.ty,
                                      outer: Type.t,
                                      _: bool) =
                         let
                            (* This should fail if the unknown occurs in t.
                             *)
                            fun con (_, _, ts) =
                               Vector.exists (ts, fn b => b)
                            fun doFields fields =
                               List.exists (fields, fn (_, b) => b)
                            fun flexRecord (_, {fields, spine = _}) =
                               doFields fields
                            fun genFlexRecord (_, {extra = _, fields,
                                                   spine = _}) =
                               doFields fields
                            fun record (_, r) = Srecord.exists (r, fn b => b)
                            fun unknown (_, u') = Unknown.equals (u, u')
                            fun no _ = false
                            val isCircular =
                               hom (outer,
                                    {con = con,
                                     expandOpaque = false,
                                     flexRecord = flexRecord,
                                     genFlexRecord = genFlexRecord,
                                     overload = no,
                                     record = record,
                                     recursive = fn _ => 
                                     Error.bug "TypeEnv.Type.unify.oneUnknown: recursive",
                                     unknown = unknown,
                                     var = no})
                         in
                            if isCircular
                               then not ()
                            else
                               let
                                  val () = minTime (outer, time)
                               in
                                  (Unified, t)
                               end
                         end
                      val (res, t) =
                         case (t, t') of
                            (Unknown r, Unknown r') =>
                               (Unified, Unknown (Unknown.join (r, r')))
                          | (Unknown u, _) =>
                               oneUnknown (u, !time, t', outer', false)
                          | (_, Unknown u') =>
                               oneUnknown (u', !time', t, outer, true)
                          | (Con (c, ts), _) => conAnd (c, ts, t', t, false)
                          | (_, Con (c, ts)) => conAnd (c, ts, t, t', true)
                          | (FlexRecord f, Record r') =>
                               oneFlex (f, !time, r', outer', false)
                          | (Record r, FlexRecord f') =>
                               oneFlex (f', !time', r, outer, true)
                          | (FlexRecord {fields = fields, spine = s},
                             FlexRecord {fields = fields', spine = s'}) =>
                            let
                               fun yes () =
                                  let
                                     val () = Spine.unify (s, s')
                                     val () = minTime (outer, !time')
                                     val () = minTime (outer', !time)
                                     val fields =
                                        List.fold
                                        (fields, fields', fn ((f, t), ac) =>
                                         if List.exists (fields', fn (f', _) =>
                                                         Field.equals (f, f'))
                                            then ac
                                         else (f, t) :: ac)
                                  in
                                     (Unified,
                                      FlexRecord {fields = fields,
                                                  spine = s})
                                  end
                            in
                               unifyRecords
                               (flexToRecord (fields, s),
                                flexToRecord (fields', s'),
                                yes, notUnifiable)
                            end
                          | (GenFlexRecord _, _) => genFlexError ()
                          | (_, GenFlexRecord _) => genFlexError ()
                          | (Overload o1, Overload o2) =>
                               if Overload.equals (o1, o2)
                                  then (Unified, t)
                               else not ()
                          | (Record r, Record r') =>
                               (case (Srecord.detupleOpt r,
                                      Srecord.detupleOpt r') of
                                   (NONE, NONE) =>
                                      unifyRecords
                                      (rigidToRecord r, rigidToRecord r',
                                       fn () => (Unified, Record r),
                                       notUnifiable)
                                 | (SOME ts, SOME ts') =>
                                      if Vector.length ts = Vector.length ts'
                                         then
                                            unifys
                                            (ts, ts',
                                             fn () => (Unified, Record r),
                                             fn (ls, ls') =>
                                             notUnifiable (layoutTuple ls,
                                                           layoutTuple ls'))
                                      else not ()
                                 | _ => not ())
                          | (Var a, Var a') =>
                               if Tyvar.equals (a, a')
                                  then (Unified, t)
                               else not ()
                          | _ => not ()
                      val res =
                         case res of
                            NotUnifiable _ => res
                          | Unified =>
                               let
                                  val res = Equality.unify (e, e')
                                  val () =
                                     case res of
                                        NotUnifiable _ => ()
                                      | Unified =>
                                           let
                                              val () = Set.union (s, s')
                                              val () =
                                                 if Time.<= (!time, !time')
                                                    then ()
                                                 else time := !time'
                                              val () =
                                                 Set.:= (s, {equality = e,
                                                             plist = plist,
                                                             time = time,
                                                             ty = t})
                                           in
                                              ()
                                           end
                               in
                                  res
                               end
                   in
                      res
                   end) arg
            and unifyRecords ((fields: (Field.t * t) vector,
                               extra: Field.t vector,
                               ensureField: Field.t -> bool),
                              (fields': (Field.t * t) vector,
                               extra': Field.t vector,
                               ensureField': Field.t -> bool),
                              yes, no) =
               let
                  fun extras (extra, ensureField') =
                     Vector.fold
                     (extra, [], fn (f, ac) =>
                      if ensureField' f
                         then ac
                      else (preError (); (f, true, dontCare) :: ac))
                  val ac = extras (extra, ensureField')
                  val ac' = extras (extra', ensureField)
                  fun subset (fields, fields', ensureField', ac, ac',
                              both, skipBoth) =
                     Vector.fold
                     (fields, (ac, ac', both), fn ((f, t), (ac, ac', both)) =>
                      case Vector.peek (fields', fn (f', _) =>
                                        Field.equals (f, f')) of
                         NONE =>
                            if ensureField' f
                               then (ac, ac', both)
                            else (preError ()
                                  ; ((f, true, dontCare' t) :: ac, ac', both))
                       | SOME (_, t') =>
                            if skipBoth
                               then (ac, ac', both)
                            else
                               case unify (t, t') of
                                  NotUnifiable (l, l') =>
                                     ((f, false, l) :: ac,
                                      (f, false, l') :: ac',
                                      both)
                                | Unified => (ac, ac', []))
                  val (ac, ac', both) =
                     subset (fields, fields', ensureField', ac, ac', [], false)
                  val (ac', ac, both) =
                     subset (fields', fields, ensureField, ac', ac, both, true)
               in
                  case (ac, ac') of
                     ([], []) => yes ()
                   | _ =>
                        let
                           val _ = preError ()
                           fun doit ac =
                              layoutRecord (List.fold
                                            (both, ac, fn ((f, t), ac) =>
                                             (f, false, layoutPretty t) :: ac))
                        in
                           no (doit ac, doit ac')
                        end
               end
            val _ = destroy ()
         in
            unify (t, t')
         end

      structure UnifyResult' =
         struct
            datatype t =
               NotUnifiable of Layout.t * Layout.t
             | Unified
         end

      datatype unifyResult = datatype UnifyResult'.t

      val unify =
         fn (t, t', z) =>
         case unify (t, t', z) of
            UnifyResult.NotUnifiable ((l, _), (l', _)) => NotUnifiable (l, l')
          | UnifyResult.Unified => Unified

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
            fun unsorted (t, fields: (Field.t *  'a) list) =
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
                                 {preError = fn _ => 
                                  Error.bug "TypeEnv.Type.simpleHom.overload"})
               in
                  con (t, Overload.defaultTycon ov, Vector.new0 ())
               end
         in
            makeHom {con = con,
                     expandOpaque = expandOpaque,
                     flexRecord = flexRecord,
                     genFlexRecord = genFlexRecord,
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
       | Type of Type.t

      fun layout s =
         case s of
            Type t => Type.layout t
          | General {canGeneralize, tyvars, ty, ...} =>
               Layout.record [("canGeneralize", Bool.layout canGeneralize),
                              ("tyvars", Vector.layout Tyvar.layout tyvars),
                              ("ty", Type.layout ty)]

      fun layoutPrettyAux (s, {expandOpaque, localTyvarNames}) =
         case s of
            Type ty => 
               Type.layoutPrettyAux 
               (ty, {expandOpaque = expandOpaque, 
                     localTyvarNames = localTyvarNames})
          | General {ty, ...} => 
               Type.layoutPrettyAux 
               (ty, {expandOpaque = expandOpaque, 
                     localTyvarNames = localTyvarNames})
      fun layoutPretty s =
         layoutPrettyAux (s, {expandOpaque = false, localTyvarNames = true})

      val bound =
         fn General {bound, ...} => bound ()
          | Type _ => Vector.new0 ()

      val bound =
         Trace.trace ("TypeEnv.Scheme.bound", layout, Vector.layout Tyvar.layout)
         bound

      val ty =
         fn General {ty, ...} => ty
          | Type ty => ty

      fun dest s = (bound s, ty s)

      fun make {canGeneralize, tyvars, ty} =
         if 0 = Vector.length tyvars
            then Type ty
         else General {bound = fn () => tyvars,
                       canGeneralize = canGeneralize,
                       flexes = [],
                       tyvars = tyvars,
                       ty = ty}

      val fromType = Type

      fun instantiate' (t: t, subst) =
         case t of
            Type ty => {args = fn () => Vector.new0 (),
                        instance = ty}
          | General {canGeneralize, flexes, tyvars, ty, ...} =>
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
                                   (case Type.toType flex of
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
               end

      fun apply (s, ts) =
         #instance (instantiate' (s, fn {index, ...} => Vector.sub (ts, index)))

      fun instantiate s =
         instantiate'
         (s, fn {canGeneralize, equality, ...} =>
          Type.unknown {canGeneralize = canGeneralize,
                        equality = if equality
                                      then Equality.truee
                                   else Equality.unknown ()})

      val instantiate =
         Trace.trace ("TypeEnv.Scheme.instantiate", layout, Type.layout o #instance)
         instantiate

      fun admitsEquality s =
         Type.admitsEquality
         (#instance
          (instantiate'
           (s, fn {canGeneralize, ...} =>
            Type.unknown {canGeneralize = canGeneralize,
                          equality = Equality.truee})))

      val admitsEquality =
         Trace.trace ("TypeEnv.Scheme.admitsEquality", layout, Bool.layout)
         admitsEquality

      fun haveFrees (v: t vector): bool vector =
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
                overload = no,
                record = fn (_, r) => Srecord.exists (r, fn b => b),
                recursive = no,
                unknown = fn _ => true,
                var = no}
            val res =
               Vector.map (v, fn s =>
                           case s of
                              General {ty, ...} => hom ty
                            | Type ty => hom ty)
            val _ = destroy ()
         in
            res
         end
   end

fun generalize (tyvars: Tyvar.t vector) =
   let
      val genTime = Time.now ()
      val () = Vector.foreach (tyvars, fn a => tyvarTime a := genTime)
   in
      fn () => {unable = (Vector.keepAll
                          (tyvars, fn a =>
                           not (Time.<= (genTime, !(tyvarTime a)))))}
   end

fun close (ensure: Tyvar.t vector, ubd) =
   let
      val beforeGen = Time.now ()
      val () = Time.tick ubd
      val genTime = Time.now ()
      val () = Vector.foreach (ensure, fn a => ignore (tyvarTime a))
      val savedCloses = !Type.newCloses
      val () = Type.newCloses := []
   in
      Trace.trace
      ("TypeEnv.close",
       let
          open Layout
       in
          Vector.layout
          (fn {isExpansive, ty} =>
           Layout.record [("isExpansive", Bool.layout isExpansive),
                          ("ty", Type.layout ty)])
       end,
       Layout.ignore)
      (fn varTypes =>
      let
         val () =
            Vector.foreach
            (varTypes, fn {isExpansive, ty} =>
             if isExpansive
                then Type.minTime (ty, beforeGen)
             else ())
         val unable = Vector.keepAll (ensure, fn a =>
                                      not (Time.<= (genTime, !(tyvarTime a))))
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
                      let
                         open Layout
                      in
                         outputl (seq [str "considering ",
                                       Type.layout t,
                                       str " with time ",
                                       Time.layout (!time),
                                       str " where getTime is ",
                                       Time.layout genTime],
                                  Out.error)
                      end
             in
                if not (Time.<= (genTime, !time))
                   then t :: ac
                else
                   case ty of
                      Type.FlexRecord {fields, spine, ...} =>
                         let
                            fun newField f =
                               {field = f,
                                tyvar = Tyvar.newNoname {equality = false}}
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
                               val a = Tyvar.newNoname {equality = b}
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
                then Scheme.Type ty
             else Scheme.General {bound = bound,
                                  canGeneralize = true,
                                  flexes = flexes,
                                  tyvars = Vector.fromList tyvars,
                                  ty = ty})
      in
         {bound = bound,
          schemes = schemes,
          unable = unable}
      end
   )
   end

structure Type =
   struct
      open Type

      fun homConVar {con, expandOpaque, var} =
         let
            fun tuple (t, ts) =
               if 1 = Vector.length ts
                  then Vector.sub (ts, 0)
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
         fn (t1: t, t2: t, {error: Layout.t * Layout.t -> unit,
                            preError: unit -> unit}) =>
         case unify (t1, t2, {preError = preError}) of
            NotUnifiable z => error z
          | Unified => ()
   end

end
