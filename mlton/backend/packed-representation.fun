(* Copyright (C) 2016-2017 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(* Has a special case to make sure that true is represented as 1
 * and false is represented as 0.
 *)

functor PackedRepresentation (S: REPRESENTATION_STRUCTS): REPRESENTATION =
struct

open S

local
   open Rssa
in
   structure Block = Block
   structure Kind = Kind
   structure Label = Label
   structure ObjectType = ObjectType
   structure Operand = Operand
   structure ObjptrTycon = ObjptrTycon
   structure Prim = Prim
   structure RealSize = RealSize
   structure Runtime = Runtime
   structure Scale = Scale
   structure Statement = Statement
   structure Switch = Switch
   structure Transfer = Transfer
   structure Type = Type
   structure Var = Var
   structure WordSize = WordSize
   structure WordX = WordX
end
structure S = Ssa
local
   open Ssa
in
   structure Base = Base
   structure Con = Con
   structure ObjectCon = ObjectCon
   structure Prod = Prod
   structure Tycon = Tycon
end

datatype z = datatype Operand.t
datatype z = datatype Statement.t
datatype z = datatype Transfer.t

structure Type =
   struct
      open Type

      local
         fun mkPadToCheck (t: t, mk): (Bits.t * (unit -> t) -> t) =
            let
               val b = width t
               fun check (b', continue) =
                  if Bits.< (b, b')
                     then let
                             val pad = zero (Bits.- (b', b))
                          in
                             mk (t, pad)
                          end
                  else if Bits.equals (b, b')
                          then t
                       else continue ()
            in
               check
            end
         fun mkPadToPrim (t: t, mk): t =
            let
               val check = mkPadToCheck (t, mk)
            in
               check (Bits.zero, fn () =>
               check (Bits.inWord8, fn () =>
               check (Bits.inWord16, fn () =>
               check (Bits.inWord32, fn () =>
               check (Bits.inWord64, fn () =>
               Error.bug "PackedRepresentation.Type.mkPadToPrim")))))
            end
         fun mkPadToWidth (t: t, b': Bits.t, mk): t =
            let
               val check = mkPadToCheck (t, mk)
            in
               check (b', fn () =>
               Error.bug "PackedRepresentation.Type.mkPadToWidth")
            end
         fun mk (t, pad) = seq (Vector.new2 (t, pad))
         fun mkLow (t, pad) = seq (Vector.new2 (pad, t))
      in
         fun padToPrim (t: t): t = mkPadToPrim (t, mk)
         fun padToPrimLow (t: t): t = mkPadToPrim (t, mkLow)
         fun padToWidth (t: t, b: Bits.t): t = mkPadToWidth (t, b, mk)
         fun padToWidthLow (t: t, b: Bits.t): t = mkPadToWidth (t, b, mkLow)
      end

      val padToPrim =
         Trace.trace
         ("PackedRepresentation.Type.padToPrim", layout, layout)
         padToPrim
      val padToPrimLow =
         Trace.trace
         ("PackedRepresentation.Type.padToPrimLow", layout, layout)
         padToPrimLow
      val padToWidth =
         Trace.trace2
         ("PackedRepresentation.Type.padToWidth", layout, Bits.layout, layout)
         padToWidth
      val padToWidthLow =
         Trace.trace2
         ("PackedRepresentation.Type.padToWidthLow", layout, Bits.layout, layout)
         padToWidthLow

   end

structure Rep =
   struct
      datatype rep =
         NonObjptr
       | Objptr of {endsIn00: bool}

      datatype t = T of {rep: rep,
                         ty: Type.t}

      fun layout (T {rep, ty}) =
         let
            open Layout
         in
            record [("rep",
                     case rep of
                        NonObjptr => str "NonObjptr"
                      | Objptr {endsIn00} =>
                           seq [str "Objptr ",
                                record [("endsIn00", Bool.layout endsIn00)]]),
                    ("ty", Type.layout ty)]
         end

      local
         fun make f (T r) = f r
      in
         val ty = make #ty
         val rep = make #rep
      end

      fun equals (r, r') = Type.equals (ty r, ty r')

      val equals =
         Trace.trace2
         ("PackedRepresentation.Rep.equals", layout, layout, Bool.layout)
         equals

      fun nonObjptr ty = T {rep = NonObjptr,
                            ty = ty}

      val bool = nonObjptr Type.bool

      val width = Type.width o ty

      val unit = T {rep = NonObjptr,
                    ty = Type.unit}

      fun isObjptr (T {rep, ...}) =
         case rep of
            Objptr _ => true
          | _ => false

      fun isObjptrEndingIn00 (T {rep, ...}) =
         case rep of
            Objptr {endsIn00} => endsIn00
          | _ => false

      fun padToWidth (r as T {rep, ty}, width: Bits.t) =
         if Bits.equals (Type.width ty, width)
            then r
         else
            case rep of
               NonObjptr =>
                  T {rep = NonObjptr,
                     ty = Type.padToWidth (ty, width)}
             | Objptr _ => Error.bug "PackedRepresentation.Rep.padToWidth"

      fun padToWidthLow (r as T {rep, ty}, width: Bits.t) =
         if Bits.equals (Type.width ty, width)
            then r
         else
            case rep of
               NonObjptr =>
                  T {rep = NonObjptr,
                     ty = Type.padToWidthLow (ty, width)}
             | Objptr _ => Error.bug "PackedRepresentation.Rep.padToWidth"
   end

structure Statement =
   struct
      open Statement

      local
         fun make prim (z1: Operand.t, z2: Operand.t) =
            let
               val ty = Operand.ty z1
               val tmp = Var.newNoname ()
            in
               (PrimApp {args = Vector.new2 (z1, z2),
                         dst = SOME (tmp, ty),
                         prim = prim (WordSize.fromBits (Type.width ty))},
                Var {ty = ty, var = tmp})
            end
      in
         val andb = make Prim.wordAndb
         val lshift = make Prim.wordLshift
         val orb = make Prim.wordOrb
         val rshift = make (fn s => Prim.wordRshift (s, {signed = false}))
      end
   end

structure WordRep =
   struct
      (* WordRep describes the representation of (some of) the components in a
       * tuple as a word.
       * Components are stored from lowest to highest, just like in Type.seq.
       * The width of the rep must be less than the width of an objptr.
       * The sum of the widths of the component reps must be equal to the
       * width of the rep.
       *)
      datatype t = T of {components: {index: int,
                                      rep: Rep.t} vector,
                         rep: Rep.t}

      fun layout (T {components, rep}) =
         let
            open Layout
         in
            record [("components",
                     Vector.layout (fn {index, rep} =>
                                    record [("index", Int.layout index),
                                            ("rep", Rep.layout rep)])
                     components),
                    ("rep", Rep.layout rep)]
         end

      local
         fun make f (T r) = f r
      in
         val rep = make #rep
      end

      val unit = T {components = Vector.new0 (),
                    rep = Rep.unit}

      fun equals (wr, wr') = Rep.equals (rep wr, rep wr')

      fun make {components, rep} =
         if Bits.<= (Rep.width rep, Control.Target.Size.objptr ())
            andalso Bits.equals (Vector.fold (components, Bits.zero,
                                              fn ({rep, ...}, ac) =>
                                              Bits.+ (ac, Rep.width rep)),
                                 Rep.width rep)
            then T {components = components,
                    rep = rep}
         else Error.bug "PackedRepresentation.WordRep.make"

      val make =
         Trace.trace
         ("PackedRepresentation.WordRep.make",
          layout o T,
          layout)
         make

      fun padToWidth (T {components, rep}, b: Bits.t): t =
         let
            val newRep = Rep.padToWidth (rep, b)
            val padBits = Bits.- (Rep.width newRep, Rep.width rep)
            val newComponent =
               {index = ~1,
                rep = Rep.nonObjptr (Type.bits padBits)}
            val newComponents =
               Vector.concat
               [components, Vector.new1 newComponent]
         in
            make {components = newComponents,
                  rep = newRep}
         end
      fun padToWidthLow (T {components, rep}, b: Bits.t): t =
         let
            val newRep = Rep.padToWidthLow (rep, b)
            val padBits = Bits.- (Rep.width newRep, Rep.width rep)
            val newComponent =
               {index = ~1,
                rep = Rep.nonObjptr (Type.bits padBits)}
            val newComponents =
               Vector.concat
               [Vector.new1 newComponent, components]
         in
            make {components = newComponents,
                  rep = newRep}
         end

      fun tuple (T {components, ...},
                 {dst = (dstVar, dstTy): Var.t * Type.t,
                  src: {index: int} -> Operand.t}): Statement.t list =
         let
            val bits = Type.width dstTy
            val (accOpt,_,statements) =
               Vector.fold
               (components, (NONE,Bits.zero,[]),
                fn ({index, rep, ...}, (accOpt,shift,statements)) =>
                if index < 0
                   then (accOpt, Bits.+ (shift, Rep.width rep), statements)
                else
                let
                   val (src, ss) = Statement.resize (src {index = index},
                                                     Type.bits bits)
                   val ss = List.rev ss
                   val (src, ss) =
                      if Bits.equals (shift, Bits.zero)
                         then (src, ss)
                      else let
                              val (s, src) =
                                 Statement.lshift
                                 (src,
                                  Operand.word
                                  (WordX.fromIntInf (Bits.toIntInf shift,
                                                     WordSize.shiftArg)))
                           in
                              (src, s :: ss)
                           end
                   val (acc, ss) =
                      case accOpt of
                         NONE => (src, ss)
                       | SOME acc =>
                            let
                               val (s, acc) = Statement.orb (src, acc)
                            in
                               (acc, s :: ss)
                            end
                in
                   (SOME acc, Bits.+ (shift, Rep.width rep), ss :: statements)
                end)
            val statements =
               case accOpt of
                  NONE => []
                | SOME src =>
                     [Bind {dst = (dstVar, dstTy),
                            isMutable = false,
                            src = src}]
                     :: statements
         in
            List.fold (statements, [], fn (ss, ac) => List.fold (ss, ac, op ::))
         end

      val tuple =
         Trace.trace
         ("PackedRepresentation.WordRep.tuple",
          layout o #1, List.layout Statement.layout)
         tuple
   end

structure Component =
   struct
      datatype t =
         Direct of {index: int,
                    rep: Rep.t}
       | Word of WordRep.t

      fun layout c =
         let
            open Layout
         in
            case c of
               Direct {index, rep} =>
                  seq [str "Direct ",
                       record [("index", Int.layout index),
                               ("rep", Rep.layout rep)]]
             | Word wr =>
                  seq [str "Word ", WordRep.layout wr]
         end

      val rep: t -> Rep.t =
         fn Direct {rep, ...} => rep
          | Word wr => WordRep.rep wr

      val ty = Rep.ty o rep

      val unit = Word WordRep.unit

      val equals: t * t -> bool =
         fn z =>
         case z of
            (Direct {rep = r, ...}, Direct {rep = r', ...}) => Rep.equals (r, r')
          | (Word wr, Word wr') => WordRep.equals (wr, wr')
          | _ => false

      local
         fun mkPadToWidth (c: t, b: Bits.t, repPadToWidth, wordRepPadToWidth): t =
            case c of
               Direct {index, rep} =>
                  Direct {index = index,
                          rep = repPadToWidth (rep, b)}
             | Word r => Word (wordRepPadToWidth (r, b))
      in
         fun padToWidth (c, b) =
            mkPadToWidth (c, b, Rep.padToWidth, WordRep.padToWidth)
         fun padToWidthLow (c, b) =
            mkPadToWidth (c, b, Rep.padToWidthLow, WordRep.padToWidthLow)
      end

      local
         fun mkPadToPrim (c: t, typePadToPrim, padToWidth) =
            let
               val ty = ty c
               val ty' = typePadToPrim ty
            in
               if Type.equals (ty, ty')
                  then c
               else padToWidth (c, Type.width ty')
            end
      in
         fun padToPrim c = mkPadToPrim (c, Type.padToPrim, padToWidth)
         fun padToPrimLow c = mkPadToPrim (c, Type.padToPrimLow, padToWidthLow)
      end

      fun tuple (c: t, {dst: Var.t * Type.t,
                        src: {index: int} -> Operand.t})
         : Statement.t list =
         case c of
            Direct {index, ...} =>
               let
                  val (src, ss) =
                     Statement.resize (src {index = index}, #2 dst)
               in
                  ss @ [Bind {dst = dst,
                              isMutable = false,
                              src = src}]
               end
          | Word wr => WordRep.tuple (wr, {dst = dst, src = src})

      val tuple =
         Trace.trace2
         ("PackedRepresentation.Component.tuple",
          layout,
          fn {dst = (dst, _), ...} => Var.layout dst,
          List.layout Statement.layout)
         tuple
   end

structure Unpack =
   struct
      datatype t = T of {shift: Bits.t,
                         ty: Type.t}

      fun layout (T {shift, ty}) =
         let
            open Layout
         in
            record [("shift", Bits.layout shift),
                    ("ty", Type.layout ty)]
         end

      val lshift: t * Bits.t -> t =
         fn (T {shift, ty}, b) =>
         T {shift = Bits.+ (shift, b),
            ty = ty}

      fun select (T {shift, ty},
                  {dst = (dst, dstTy),
                   src: Operand.t}): Statement.t list =
         let
            val (src, ss1) =
               if Bits.isZero shift
                  then (src, [])
               else
                  let
                     val shift =
                        WordX.fromIntInf (Bits.toIntInf shift, WordSize.shiftArg)
                     val (s, tmp) = Statement.rshift (src, Operand.word shift)
                  in
                     (tmp, [s])
                  end
            val w = Type.width ty
            val sz = WordSize.fromBits w
            val w' = Type.width dstTy
            val sz' = WordSize.fromBits w'
            val (src, ss2) = Statement.resize (src, dstTy)
            val (src, ss3) =
               if Bits.equals (w, w')
(*                orelse Type.isZero (Type.dropPrefix (Operand.ty src,
 *                                                     WordSize.bits sz))
 *)
                  then (src, [])
               else
                  let
                     val mask = WordX.resize (WordX.max (sz, {signed = false}), sz')
                     val (s, src) = Statement.andb (src, Operand.word mask)
                  in
                     (src, [s])
                  end
         in
            ss1 @ ss2 @ ss3 @ [Bind {dst = (dst, dstTy),
                                     isMutable = false,
                                     src = src}]
         end

      val select =
         Trace.trace2
         ("PackedRepresentation.Unpack.select",
          layout,
          fn {dst = (dst, _), src} =>
          Layout.record [("dst", Var.layout dst),
                         ("src", Operand.layout src)],
          List.layout Statement.layout)
         select

      fun update (T {shift, ty},
                  {chunk: Operand.t,
                   component: Operand.t}): Operand.t * Statement.t list =
         let
            val shift =
               WordX.fromIntInf (Bits.toIntInf shift, WordSize.shiftArg)
            val chunkTy = Operand.ty chunk
            val chunkWidth = Type.width chunkTy
            val mask =
               Operand.word
               (WordX.notb
                (WordX.lshift
                 (WordX.resize (WordX.allOnes (WordSize.fromBits (Type.width ty)),
                                WordSize.fromBits chunkWidth),
                  shift)))
            val (s1, chunk) = Statement.andb (chunk, mask)
            val (component, s2) = Statement.resize (component, chunkTy)
            val (s3, component) = Statement.lshift (component, Operand.word shift)
            val (s4, result) = Statement.orb (chunk, component)
         in
            (result, [s1] @ s2 @ [s3, s4])
         end

      val update =
         Trace.trace2
         ("PackedRepresentation.Unpack.update",
          layout,
          fn {chunk, component} =>
          Layout.record [("chunk", Operand.layout chunk),
                         ("component", Operand.layout component)],
          Layout.tuple2 (Operand.layout,
                         List.layout Statement.layout))
         update
   end

structure Base =
   struct
      open Base

      fun toOperand {base: Operand.t t,
                     eltWidth: Bytes.t option,
                     offset: Bytes.t,
                     ty: Type.t}: Operand.t * Statement.t list =
         case base of
            Object base =>
               (Offset {base = base,
                        offset = offset,
                        ty = ty},
                [])
          | VectorSub {index, vector} =>
               let
                  val eltWidth =
                     case eltWidth of
                        NONE => Error.bug "PackedRepresentation.Base.toOperand: eltWidth"
                      | SOME w => w
               in
                  case Scale.fromBytes eltWidth of
                     NONE =>
                        let
                           val seqIndexSize = WordSize.seqIndex ()
                           val seqIndexTy = Type.word seqIndexSize
                           val prod = Var.newNoname ()
                           val s =
                              PrimApp {args = (Vector.new2
                                               (index,
                                                Operand.word
                                                (WordX.fromIntInf
                                                 (Bytes.toIntInf eltWidth,
                                                  seqIndexSize)))),
                                       dst = SOME (prod, seqIndexTy),
                                       prim = (Prim.wordMul
                                               (seqIndexSize,
                                                {signed = false}))}
                        in
                           (ArrayOffset {base = vector,
                                         index = Var {var = prod, ty = seqIndexTy},
                                         offset = offset,
                                         scale = Scale.One,
                                         ty = ty},
                            [s])
                        end
                   | SOME s =>
                        (ArrayOffset {base = vector,
                                      index = index,
                                      offset = offset,
                                      scale = s,
                                      ty = ty},
                         [])
               end
   end

structure Select =
   struct
      datatype t =
         None
       | Direct of {ty: Type.t}
       | Indirect of {offset: Bytes.t,
                      ty: Type.t}
       | IndirectUnpack of {offset: Bytes.t,
                            rest: Unpack.t,
                            ty: Type.t}
       | Unpack of Unpack.t

      fun layout s =
         let
            open Layout
         in
            case s of
               None => str "None"
             | Direct {ty} => seq [str "Direct ",
                                   record [("ty", Type.layout ty)]]
             | Indirect {offset, ty} =>
                  seq [str "Indirect ",
                       record [("offset", Bytes.layout offset),
                               ("ty", Type.layout ty)]]
             | IndirectUnpack {offset, rest, ty} =>
                  seq [str "IndirectUnpack ",
                       record [("offset", Bytes.layout offset),
                               ("rest", Unpack.layout rest),
                               ("ty", Type.layout ty)]]
             | Unpack u => seq [str "Unpack ", Unpack.layout u]
         end

      val lshift: t * Bits.t -> t =
         fn (s, b) =>
         case s of
            None => None
          | Direct {ty} => Unpack (Unpack.T {shift = b, ty = ty})
          | Unpack u => Unpack (Unpack.lshift (u, b))
          | _ => Error.bug "PackedRepresentation.Select.lshift"

      fun select (s: t, {base: Operand.t Base.t,
                         dst: Var.t * Type.t,
                         eltWidth: Bytes.t option}): Statement.t list =
         let
            fun move (src, ss) =
               let
                  val (dst, dstTy) = dst
                  val (src, ss') = Statement.resize (src, dstTy)
               in
                  ss @ ss' @ [Bind {dst = (dst, dstTy),
                                    isMutable = false,
                                    src = src}]
               end
         in
            case s of
               None => []
             | Direct _ => move (Base.object base, [])
             | Indirect {offset, ty} =>
                  move (Base.toOperand {base = base,
                                        eltWidth = eltWidth,
                                        offset = offset,
                                        ty = ty})
             | IndirectUnpack {offset, rest, ty} =>
                  let
                     val tmpVar = Var.newNoname ()
                     val tmpOp = Var {ty = ty, var = tmpVar}
                     val (src, ss) =
                        Base.toOperand {base = base,
                                        eltWidth = eltWidth,
                                        offset = offset,
                                        ty = ty}
                  in
                     ss @ (Bind {dst = (tmpVar, ty),
                                 isMutable = false,
                                 src = src}
                           :: Unpack.select (rest, {dst = dst, src = tmpOp}))
                  end
             | Unpack u =>
                  Unpack.select (u, {dst = dst, src = Base.object base})
         end

      val select =
         Trace.trace
         ("PackedRepresentation.Select.select",
          layout o #1, List.layout Statement.layout)
         select

      fun update (s: t, {base: Operand.t Base.t,
                         eltWidth: Bytes.t option,
                         value: Operand.t}): Statement.t list =
         case s of
            Indirect {offset, ty} =>
               let
                  val (dst, ss) =
                     Base.toOperand {base = base,
                                     eltWidth = eltWidth,
                                     offset = offset,
                                     ty = ty}
               in
                  ss @ [Move {dst = dst, src = value}]
               end
          | IndirectUnpack {offset, rest, ty} =>
               let
                  val (chunk, ss) =
                     Base.toOperand {base = base,
                                     eltWidth = eltWidth,
                                     offset = offset,
                                     ty = ty}
                  val (newChunk, ss') =
                     Unpack.update (rest, {chunk = chunk,
                                           component = value})
               in
                  ss @ ss' @ [Move {dst = chunk, src = newChunk}]
               end
          | _ => Error.bug "PackedRepresentation.Select.update: non-indirect"

      val update =
         Trace.trace
         ("PackedRepresentation.Select.update",
          layout o #1, List.layout Statement.layout)
         update
   end

structure Selects =
   struct
      datatype t = T of {orig: S.Type.t,
                         select: Select.t} vector

      fun layout (T v) = Vector.layout (Select.layout o #select) v

      val empty = T (Vector.new0 ())

      fun map (T v, f) =
         T (Vector.map (v, fn {orig, select} =>
                        {orig = orig,
                         select = f select}))

      fun select (T v, {base: Operand.t Base.t,
                        dst: Var.t * Type.t,
                        eltWidth: Bytes.t option,
                        offset: int}): Statement.t list =
         Select.select (#select (Vector.sub (v, offset)),
                        {base = base, eltWidth = eltWidth, dst = dst})

      fun update (T v, {base, eltWidth, offset, value}) =
         Select.update (#select (Vector.sub (v, offset)),
                        {base = base, eltWidth = eltWidth, value = value})

      fun lshift (T v, b: Bits.t) =
         T (Vector.map (v, fn {orig, select} =>
                        {orig = orig,
                         select = Select.lshift (select, b)}))
   end

structure ObjptrRep =
   struct
      datatype t = T of {components: {component: Component.t,
                                      offset: Bytes.t} vector,
                         componentsTy: Type.t,
                         selects: Selects.t,
                         ty: Type.t,
                         tycon: ObjptrTycon.t}

      fun layout (T {components, componentsTy, selects, ty, tycon}) =
         let
            open Layout
         in
            record
            [("components",
              Vector.layout (fn {component, offset} =>
                             record [("component", Component.layout component),
                                     ("offset", Bytes.layout offset)])
              components),
             ("componentsTy", Type.layout componentsTy),
             ("selects", Selects.layout selects),
             ("ty", Type.layout ty),
             ("tycon", ObjptrTycon.layout tycon)]
         end

      local
         fun make f (T r) = f r
      in
         val componentsTy = make #componentsTy
         val ty = make #ty
      end

      fun equals (T {tycon = c, ...}, T {tycon = c', ...}) =
         ObjptrTycon.equals (c, c')

      fun rep (T {ty, ...}) =
         Rep.T {rep = Rep.Objptr {endsIn00 = true},
                ty = ty}

      fun make {components, isVector, selects, tycon} =
         let
            val width =
               Vector.fold
               (components, Bytes.zero, fn ({component = c, ...}, ac) =>
                Bytes.+ (ac, Type.bytes (Component.ty c)))
            val padBytes: Bytes.t =
               if isVector
                  then let
                          val alignWidth =
                             case !Control.align of
                                Control.Align4 => width
                              | Control.Align8 =>
                                   if (Vector.exists
                                       (components, fn {component = c, ...} =>
                                        (case Type.deReal (Component.ty c) of
                                            NONE => false
                                          | SOME s =>
                                               RealSize.equals (s, RealSize.R64))
                                        orelse
                                        (case Type.deWord (Component.ty c) of
                                            NONE => false
                                          | SOME s =>
                                               WordSize.equals (s, WordSize.word64))
                                        orelse
                                        (Type.isObjptr (Component.ty c)
                                         andalso WordSize.equals (WordSize.objptr (),
                                                                  WordSize.word64))))
                                      then Bytes.alignWord64 width
                                   else width
                       in
                          Bytes.- (alignWidth, width)
                       end
               else let
                       (* Note that with Align8 and objptrSize == 64bits,
                        * the following ensures that objptrs will be
                        * mod 8 aligned.
                        *)
                       val width' = Bytes.+ (width, Runtime.normalMetaDataSize ())
                       val alignWidth' =
                          case !Control.align of
                             Control.Align4 => Bytes.alignWord32 width'
                           | Control.Align8 => Bytes.alignWord64 width'
                       val alignWidth = Bytes.- (alignWidth', Runtime.normalMetaDataSize ())
                    in
                       Bytes.- (alignWidth, width)
                    end
            val (components, selects) =
               if Bytes.isZero padBytes
                  then (components, selects)
               else
                  (* Need to insert a pad before the first objptr. *)
                  let
                     val {no = nonObjptrs, yes = objptrs} =
                        Vector.partition
                        (components, fn {component = c, ...} =>
                         Rep.isObjptr (Component.rep c))
                     val padOffset =
                        if Vector.isEmpty objptrs
                           then width
                        else #offset (Vector.first objptrs)
                     val pad =
                        (#1 o Vector.unfoldi)
                        ((Bytes.toInt padBytes) div (Bytes.toInt Bytes.inWord32),
                         padOffset,
                         fn (_, padOffset) =>
                         ({component = (Component.padToWidth
                                        (Component.unit, Bits.inWord32)),
                           offset = padOffset},
                          Bytes.+ (padOffset, Bytes.inWord32)))
                     val objptrs =
                        Vector.map (objptrs, fn {component = c, offset} =>
                                    {component = c,
                                     offset = Bytes.+ (offset, padBytes)})
                     val components =
                        Vector.concat [nonObjptrs, pad, objptrs]
                     val selects =
                        Selects.map
                        (selects, fn s =>
                         case s of
                            Select.Indirect {offset, ty} =>
                               if Bytes.>= (offset, padOffset)
                                  then
                                     Select.Indirect
                                     {offset = Bytes.+ (offset, padBytes),
                                      ty = ty}
                               else s
                          | _ => s)
                  in
                     (components, selects)
                  end
            val componentsTy =
               Type.seq (Vector.map (components, Component.ty o #component))
         in
            T {components = components,
               componentsTy = componentsTy,
               selects = selects,
               ty = Type.objptr tycon,
               tycon = tycon}
         end

      val make =
         let
            open Layout
         in
            Trace.trace
            ("PackedRepresentation.ObjptrRep.make",
             fn {components, isVector, selects, tycon} =>
             record
             [("components",
               Vector.layout (fn {component, offset} =>
                              record [("component", Component.layout component),
                                      ("offset", Bytes.layout offset)])
               components),
              ("isVector", Bool.layout isVector),
              ("selects", Selects.layout selects),
              ("tycon", ObjptrTycon.layout tycon)],
             layout)
         end
         make

      fun box (component: Component.t, opt: ObjptrTycon.t, selects: Selects.t) =
         let
            val selects =
               Selects.map
               (selects, fn s =>
                let
                   datatype z = datatype Select.t
                in
                   case s of
                      None => None
                    | Direct {ty} => Indirect {offset = Bytes.zero, ty = ty}
                    | Unpack u => IndirectUnpack {offset = Bytes.zero,
                                                  rest = u,
                                                  ty = Component.ty component}
                    | _ => Error.bug "PackedRepresentation.ObjptrRep.box: cannot lift selects"
                end)
         in
            make {components = Vector.new1 {component = component,
                                            offset = Bytes.zero},
                  isVector = false,
                  selects = selects,
                  tycon = opt}
         end

      fun tuple (T {components, componentsTy, ty, tycon, ...},
                 {dst = dst: Var.t,
                  src: {index: int} -> Operand.t}) =
         let
            val object = Var {ty = ty, var = dst}
            val stores =
               Vector.foldr
               (components, [], fn ({component, offset}, ac) =>
                let
                   val tmpVar = Var.newNoname ()
                   val tmpTy = Component.ty component
                   val statements =
                      Component.tuple (component,
                                       {dst = (tmpVar, tmpTy), src = src})
                in
                   if List.isEmpty statements
                      then ac
                      else statements
                           @ (Move {dst = Offset {base = object,
                                                  offset = offset,
                                                  ty = tmpTy},
                                    src = Var {ty = tmpTy, var = tmpVar}}
                              :: ac)
                end)
         in
            Object {dst = (dst, ty),
                    header = Runtime.typeIndexToHeader (ObjptrTycon.index tycon),
                    size = Bytes.+ (Type.bytes componentsTy, Runtime.normalMetaDataSize ())}
            :: stores
         end

      val tuple =
         Trace.trace2
         ("PackedRepresentation.ObjptrRep.tuple",
          layout, Var.layout o #dst, List.layout Statement.layout)
         tuple
   end

structure TupleRep =
   struct
      datatype t =
         Direct of {component: Component.t,
                    selects: Selects.t}
       | Indirect of ObjptrRep.t

      fun layout tr =
         let
            open Layout
         in
            case tr of
               Direct {component, selects} =>
                  seq [str "Direct ",
                       record [("component", Component.layout component),
                               ("selects", Selects.layout selects)]]
             | Indirect pr =>
                  seq [str "Indirect ", ObjptrRep.layout pr]
         end

      val unit = Direct {component = Component.unit,
                         selects = Selects.empty}

      val equals: t * t -> bool =
         fn z =>
         case z of
            (Direct {component = c, ...}, Direct {component = c', ...}) =>
               Component.equals (c, c')
          | (Indirect pr, Indirect pr') => ObjptrRep.equals (pr, pr')
          | _ => false

      fun rep (tr: t): Rep.t =
         case tr of
            Direct {component, ...} => Component.rep component
          | Indirect p => ObjptrRep.rep p

      val ty = Rep.ty o rep

      fun selects (tr: t): Selects.t =
         case tr of
            Direct {selects, ...} => selects
          | Indirect (ObjptrRep.T {selects, ...}) => selects

      fun tuple (tr: t,
                 {dst: Var.t * Type.t,
                  src: {index: int} -> Operand.t}): Statement.t list =
         case tr of
            Direct {component = c, ...} =>
               Component.tuple (c, {dst = dst, src = src})
          | Indirect pr =>
               ObjptrRep.tuple (pr, {dst = #1 dst, src = src})

      val tuple =
         Trace.trace2
         ("PackedRepresentation.TupleRep.tuple",
          layout, Var.layout o #1 o #dst, List.layout Statement.layout)
         tuple

      (* TupleRep.make decides how to layout a sequence of types in an object,
       * or in the case of a vector, in a vector element.
       * Vectors are treated slightly specially because we don't require element
       * widths to be a multiple of the word32 size.
       * At the front of the object, we place all the word64s, followed by
       * all the word32s.  Then, we pack in all the types that are smaller than a
       * word32.  This is done by packing in a sequence of words, greedily,
       * starting with the largest type and moving to the smallest.  We pad to
       * ensure that a value never crosses a word32 boundary.  Finally, if there
       * are any objptrs, they go at the end of the object.
       *
       * There is some extra logic here to specially represent (boxed)
       * tuples that are entirely comprised of primitive types.  The
       * primary motivation is that "word8 ref" and "word16 ref" are
       * FFI types, and must have representations that are compatible
       * with C.  In particular, on a big-endian platform, such
       * sub-word32 components must be at the low byte offset (but
       * high bit offset) of the containing word32.
       *)
      fun make (objptrTycon: ObjptrTycon.t,
                rs: {isMutable: bool,
                     rep: Rep.t,
                     ty: S.Type.t} vector,
                {forceBox: bool,
                 isVector: bool}): t =
         let
            val objptrs = ref []
            val numObjptrs = ref 0
            val word64s = ref []
            val numWord64s = ref 0
            val word32s = ref []
            val numWord32s = ref 0
            val subword32s = Array.array (Bits.toInt Bits.inWord32, [])
            val widthSubword32s = ref 0
            val hasNonPrim = ref false
            val () =
               Vector.foreachi
               (rs, fn (i, {rep, ...}) =>
                let
                   fun addDirect (l, n) =
                      (List.push (l, {component = Component.Direct {index = i,
                                                                    rep = rep},
                                      index = i})
                       ; Int.inc n)
                   fun addSubword32 b =
                      (Array.update
                       (subword32s, b,
                        {index = i, rep = rep} :: Array.sub (subword32s, b))
                       ; widthSubword32s := !widthSubword32s + b)
                in
                   case Rep.rep rep of
                      Rep.NonObjptr =>
                         let
                            val b = Bits.toInt (Rep.width rep)
                         in
                            case b of
                               0 => ()
                             | 8 => addSubword32 b
                             | 16 => addSubword32 b
                             | 32 => addDirect (word32s, numWord32s)
                             | 64 => addDirect (word64s, numWord64s)
                             | _ => (addSubword32 b
                                     ; hasNonPrim := true)
                         end
                    | Rep.Objptr _ => addDirect (objptrs, numObjptrs)
                end)
            val selects = Array.array (Vector.length rs, Select.None)
            val hasNonPrim = !hasNonPrim
            val numComponents =
               !numObjptrs + !numWord64s + !numWord32s +
               (let
                   val widthSubword32s = !widthSubword32s
                in
                   Int.quot (widthSubword32s, 32)
                   + Int.min (1, Int.rem (widthSubword32s, 32))
                end)
            val needsBox =
               forceBox
               orelse Vector.exists (rs, #isMutable)
               orelse numComponents > 1
            val padToPrim = isVector andalso 1 = numComponents
            val isBigEndian = Control.Target.bigEndian ()
            fun byteShiftToByteOffset (compSz: Bytes.t, tySz: Bytes.t, shift: Bytes.t) =
               if not isBigEndian
                  then shift
               else Bytes.- (compSz, Bytes.+ (tySz, shift))
            fun simple (l, tyWidth: Bytes.t, offset: Bytes.t, components) =
               List.fold
               (l, (offset, components),
                fn ({component, index}, (offset, ac)) =>
                (Bytes.+ (offset, tyWidth),
                 let
                    val ty = Component.ty component
                    val () =
                       Array.update
                       (selects, index,
                        if needsBox
                           then Select.Indirect {offset = offset, ty = ty}
                        else Select.Direct {ty = ty})
                 in
                    {component = component,
                     offset = offset} :: ac
                 end))
            val offset = Bytes.zero
            val components = []
            val (offset, components) =
               simple (!word64s, Bytes.inWord64, offset, components)
            val (offset, components) =
               simple (!word32s, Bytes.inWord32, offset, components)
            (* j is the maximum index <= remainingWidth at which an
             * element of subword32s may be nonempty.
             *)
            fun getSubword32Components (j: int,
                                        remainingWidth: Bits.t,
                                        components) =
               if 0 = j
                  then Vector.fromListRev components
               else
                  let
                     val elts = Array.sub (subword32s, j)
                  in
                     case elts of
                        [] => getSubword32Components (j - 1, remainingWidth, components)
                      | {index, rep} :: elts =>
                           let
                              val () = Array.update (subword32s, j, elts)
                              val remainingWidth = Bits.- (remainingWidth, Rep.width rep)
                           in
                              getSubword32Components
                              (Bits.toInt remainingWidth,
                               remainingWidth,
                               {index = index, rep = rep} :: components)
                           end
                  end
            (* max is the maximum index at which an element of
             * subword32s may be nonempty.
             *)
            fun makeSubword32s (max: int, offset: Bytes.t, ac) =
               if 0 = max
                  then (offset, ac)
               else
                  if List.isEmpty (Array.sub (subword32s, max))
                     then makeSubword32s (max - 1, offset, ac)
                  else
                     let
                        val components =
                           getSubword32Components (max, Bits.inWord32, [])
                        val componentTy =
                           Type.seq (Vector.map (components, Rep.ty o #rep))
                        val component =
                           (Component.Word o WordRep.T)
                           {components = components,
                            rep = Rep.T {rep = Rep.NonObjptr,
                                         ty = componentTy}}
                        val (component, componentTy) =
                           if needsBox
                              then if padToPrim
                                      then (Component.padToPrim component,
                                            Type.padToPrim componentTy)
                                   else (Component.padToWidth (component, Bits.inWord32),
                                         Type.padToWidth (componentTy, Bits.inWord32))
                           else (component, componentTy)
                        val _ =
                           Vector.fold
                           (components, Bits.zero,
                            fn ({index, rep}, shift) =>
                            let
                               val repTy = Rep.ty rep
                               val repTyWidth = Type.width repTy
                               val repWidth = Rep.width rep
                               val unpack = Unpack.T {shift = shift,
                                                      ty = repTy}
                               fun getByteOffset () =
                                  Bytes.+
                                  (offset,
                                   byteShiftToByteOffset
                                   (Type.bytes componentTy,
                                    Bits.toBytes repTyWidth,
                                    Bits.toBytes shift))
                               val select =
                                  if needsBox
                                     then if ((Bits.isWord8Aligned shift
                                               andalso (Bits.equals
                                                        (repTyWidth,
                                                         Bits.inWord8)))
                                              orelse
                                              (Bits.isWord16Aligned shift
                                               andalso (Bits.equals
                                                        (repTyWidth,
                                                         Bits.inWord16))))
                                             then (Select.Indirect
                                                   {offset = getByteOffset (),
                                                    ty = repTy})
                                          else (Select.IndirectUnpack
                                                {offset = offset,
                                                 rest = unpack,
                                                 ty = componentTy})
                                  else Select.Unpack unpack
                               val () =
                                  Array.update
                                  (selects, index, select)
                            in
                               Bits.+ (shift, repWidth)
                            end)
                        val ac = {component = component,
                                  offset = offset} :: ac
                     in
                        makeSubword32s
                        (max,
                         (* Either the width of the word rep component
                          * is 32 bits, or this is the only
                          * component, so offset doesn't matter.
                          *)
                         Bytes.+ (offset, Bytes.inWord32),
                         ac)
                     end
            fun makeSubword32sAllPrims (max: int, offset: Bytes.t, ac) =
               (* hasNonPrim = false, needsBox = true *)
               if 0 = max
                  then (offset, ac)
               else
                  if List.isEmpty (Array.sub (subword32s, max))
                     then makeSubword32sAllPrims (max - 1, offset, ac)
                  else
                     let
                        val origComponents =
                           getSubword32Components (max, Bits.inWord32, [])
                        val components =
                           if isBigEndian
                              then Vector.rev origComponents
                           else origComponents
                        val componentTy =
                           Type.seq (Vector.map (components, Rep.ty o #rep))
                        val component =
                           (Component.Word o WordRep.T)
                           {components = components,
                            rep = Rep.T {rep = Rep.NonObjptr,
                                         ty = componentTy}}
                        val component =
                           if padToPrim
                              then if isBigEndian
                                      then Component.padToPrimLow component
                                      else Component.padToPrim component
                           else if isBigEndian
                                   then Component.padToWidthLow (component, Bits.inWord32)
                                   else Component.padToWidth (component, Bits.inWord32)
                        val _ =
                           Vector.fold
                           (origComponents, offset,
                            fn ({index, rep}, offset) =>
                            let
                               val () =
                                  Array.update
                                  (selects, index,
                                   Select.Indirect
                                   {offset = offset,
                                    ty = Rep.ty rep})
                            in
                               Bytes.+ (offset, Bits.toBytes (Rep.width rep))
                            end)
                        val ac = {component = component,
                                  offset = offset} :: ac
                     in
                        makeSubword32sAllPrims
                        (max,
                         (* Either the width of the word rep component
                          * is 32 bits, or this is the only
                          * component, so offset doesn't matter.
                          *)
                         Bytes.+ (offset, Bytes.inWord32),
                         ac)
                     end
            val (offset, components) =
               if (not hasNonPrim) andalso needsBox
                  then makeSubword32sAllPrims (Array.length subword32s - 1, offset, components)
               else makeSubword32s (Array.length subword32s - 1, offset, components)
            val (_, components) =
               simple (!objptrs, Runtime.objptrSize (), offset, components)
            val components = Vector.fromListRev components
(*
            val () =
               Assert.assert
               ("PackedRepresentation.TupleRep.make", fn () =>
                numComponents = Vector.length components)
*)
            val getSelects =
               Selects.T (Vector.tabulate
                          (Array.length selects, fn i =>
                           {orig = #ty (Vector.sub (rs, i)),
                            select = Array.sub (selects, i)}))
         in
            if needsBox
               then Indirect (ObjptrRep.make {components = components,
                                              isVector = isVector,
                                              selects = getSelects,
                                              tycon = objptrTycon})
            else if numComponents = 0
                    then unit
                 else Direct {component = #component (Vector.first components),
                              selects = getSelects}
         end
      val make =
         Trace.trace3
         ("PackedRepresentation.TupleRep.make",
          ObjptrTycon.layout,
          Vector.layout (fn {isMutable, rep, ty} =>
                         Layout.record [("isMutable", Bool.layout isMutable),
                                        ("rep", Rep.layout rep),
                                        ("ty", S.Type.layout ty)]),
          fn {forceBox, isVector} =>
          Layout.record [("forceBox", Bool.layout forceBox),
                         ("isVector", Bool.layout isVector)],

          layout)
         make
   end

structure ConRep =
   struct
      datatype t =
         ShiftAndTag of {component: Component.t,
                         selects: Selects.t,
                         tag: WordX.t,
                         ty: Type.t (* alread padded to prim *)}
       | Tag of {tag: WordX.t,
                 ty: Type.t}
       | Tuple of TupleRep.t

      val layout =
         let
            open Layout
         in
            fn ShiftAndTag {component, selects, tag, ty} =>
                  seq [str "ShiftAndTag ",
                       record [("component", Component.layout component),
                               ("selects", Selects.layout selects),
                               ("tag", WordX.layout tag),
                               ("ty", Type.layout ty)]]
             | Tag {tag, ...} => seq [str "Tag ", WordX.layout tag]
             | Tuple tr => TupleRep.layout tr
         end

      val equals: t * t -> bool =
         fn (ShiftAndTag {component = c1, tag = t1, ...},
             ShiftAndTag {component = c2, tag = t2, ...}) =>
              Component.equals (c1, c2) andalso WordX.equals (t1, t2)
          | (Tag {tag = t1, ty = ty1}, Tag {tag = t2, ty = ty2}) =>
               WordX.equals (t1, t2) andalso Type.equals (ty1, ty2)
          | (Tuple tr1, Tuple tr2) => TupleRep.equals (tr1, tr2)
          | _ => false

      val rep: t -> Rep.t =
         fn ShiftAndTag {ty, ...} => Rep.nonObjptr ty
          | Tag {ty, ...} => Rep.nonObjptr ty
          | Tuple tr => TupleRep.rep tr

      val box = Tuple o TupleRep.Indirect

      local
         fun make i =
            let
               val tag = WordX.fromIntInf (i, WordSize.bool)
            in
               Tag {tag = tag, ty = Type.ofWordX tag}
            end
      in
         val falsee = make 0
         val truee = make 1
      end

      val unit = Tuple TupleRep.unit

      fun conApp (r: t, {dst: Var.t * Type.t,
                         src: {index: int} -> Operand.t}): Statement.t list =
         case r of
            ShiftAndTag {component, tag, ...} =>
               let
                  val (dstVar, dstTy) = dst
                  val shift = Operand.word (WordX.fromIntInf
                                            (Bits.toIntInf
                                             (WordSize.bits
                                              (WordX.size tag)),
                                             WordSize.shiftArg))
                  val tmpVar = Var.newNoname ()
                  val tmpTy =
                     Type.padToWidth (Component.ty component, Type.width dstTy)
                  val tmp = Var {ty = tmpTy, var = tmpVar}
                  val component =
                     Component.tuple (component, {dst = (tmpVar, tmpTy),
                                                  src = src})
                  val (s1, tmp) = Statement.lshift (tmp, shift)
                  val mask = Operand.word (WordX.resize
                                           (tag,
                                            WordSize.fromBits
                                            (Type.width
                                             (Operand.ty tmp))))
                  val (s2, tmp) = Statement.orb (tmp, mask)
                  val s3 = Bind {dst = (dstVar, dstTy),
                                 isMutable = false,
                                 src = tmp}
               in
                  component @ [s1, s2, s3]
               end
          | Tag {tag, ...} =>
               let
                  val (dstVar, dstTy) = dst
                  val src = Operand.word (WordX.resize
                                          (tag,
                                           WordSize.fromBits
                                           (Type.width dstTy)))
               in
                  [Bind {dst = (dstVar, dstTy),
                         isMutable = false,
                         src = src}]
               end
          | Tuple tr => TupleRep.tuple (tr, {dst = dst, src = src})

      val conApp =
         Trace.trace
         ("PackedRepresentation.ConRep.conApp",
          layout o #1, List.layout Statement.layout)
         conApp
   end

structure Block =
   struct
      open Block

      val extra: t list ref = ref []

      fun getExtra () = !extra before extra := []

      fun new {statements: Statement.t vector,
               transfer: Transfer.t}: Label.t =
         let
            val l = Label.newNoname ()
            val _ = List.push (extra,
                               Block.T {args = Vector.new0 (),
                                        kind = Kind.Jump,
                                        label = l,
                                        statements = statements,
                                        transfer = transfer})
         in
            l
         end
   end

structure Cases =
   struct
      type t = {con: Con.t, dst: Label.t, dstHasArg: bool} vector

      fun layout (v: t): Layout.t =
         Vector.layout
         (fn {con, dst, dstHasArg} =>
          Layout.record [("con", Con.layout con),
                         ("dst", Label.layout dst),
                         ("dstHasArg", Bool.layout dstHasArg)])
         v
   end

structure Objptrs =
   struct
      (* 1 < Vector.length variants *)
      datatype t = T of {rep: Rep.t,
                         variants: {con: Con.t,
                                    objptr: ObjptrRep.t} vector}

      fun layout (T {rep, variants}) =
         let
            open Layout
         in
            record [("rep", Rep.layout rep),
                    ("variants",
                     Vector.layout
                     (fn {con, objptr} =>
                      record [("con", Con.layout con),
                              ("objptr", ObjptrRep.layout objptr)])
                     variants)]
         end

      local
         fun make f (T r) = f r
      in
         val rep = make #rep
      end

      val ty = Rep.ty o rep

      fun make {rep, variants}: t =
         T {rep = rep,
            variants = variants}

      fun genCase (T {variants, ...},
                   {cases: Cases.t,
                    conRep: Con.t -> ConRep.t,
                    default: Label.t option,
                    test: Operand.t})
         : Statement.t list * Transfer.t =
         let
            val cases =
               Vector.keepAllMap
               (cases, fn {con, dst, dstHasArg} =>
                case conRep con of
                   ConRep.Tuple (TupleRep.Indirect (ObjptrRep.T {ty, tycon, ...})) =>
                      SOME (WordX.fromIntInf (Int.toIntInf (ObjptrTycon.index tycon),
                                              WordSize.objptrHeader ()),
                            Block.new
                            {statements = Vector.new0 (),
                             transfer = Goto {args = if dstHasArg
                                                        then (Vector.new1
                                                              (Operand.cast (test, ty)))
                                                     else Vector.new0 (),
                                              dst = dst}})
                 | _ => NONE)
            val default =
               if Vector.length variants = Vector.length cases
                  then NONE
               else default
            val cases =
               QuickSort.sortVector (cases, fn ((w, _), (w', _)) =>
                                     WordX.le (w, w', {signed = false}))
            val shift = Operand.word (WordX.one WordSize.shiftArg)
            val (s, tag) =
               Statement.rshift (Offset {base = test,
                                         offset = Runtime.headerOffset (),
                                         ty = Type.objptrHeader ()},
                                 shift)
         in
            ([s], Switch (Switch.T {cases = cases,
                                    default = default,
                                    size = WordSize.objptrHeader (),
                                    test = tag}))
         end
   end

structure Small =
   struct
      datatype t = T of {isEnum: bool,
                         rep: Rep.t,
                         tagBits: Bits.t,
                         variants: Con.t vector}

      fun layout (T {isEnum, rep, tagBits, variants}) =
         let
            open Layout
         in
            record [("isEnum", Bool.layout isEnum),
                    ("rep", Rep.layout rep),
                    ("tagBits", Bits.layout tagBits),
                    ("variants", Vector.layout Con.layout variants)]
         end

      local
         fun make f (T r) = f r
      in
         val rep = make #rep
      end

      val bool =
         T {isEnum = true,
            rep = Rep.bool,
            tagBits = Bits.one,
            variants = Vector.new2 (Con.falsee, Con.truee)}

      fun genCase (T {isEnum, tagBits, variants, ...},
                   {cases: Cases.t,
                    conRep: Con.t -> ConRep.t,
                    isObjptr: bool,
                    notSmall: Label.t option,
                    smallDefault: Label.t option,
                    test: Operand.t})
         : Statement.t list * Transfer.t =
         let
            val tagSize = WordSize.fromBits tagBits
            val testBits = Type.width (Operand.ty test)
            val testSize = WordSize.fromBits testBits
            val cases =
               Vector.keepAllMap
               (cases, fn {con, dst, dstHasArg} =>
                case conRep con of
                   ConRep.ShiftAndTag {tag, ty, ...} =>
                      let
                         val test = Operand.cast (test, Type.padToWidth (ty, testBits))
                         val (test, ss) = Statement.resize (test, ty)
                         val transfer =
                            Goto {args = if dstHasArg
                                            then Vector.new1 test
                                         else Vector.new0 (),
                                  dst = dst}
                      in
                         SOME (WordX.resize (tag, testSize),
                               Block.new {statements = Vector.fromList ss,
                                          transfer = transfer})
                      end
                 | ConRep.Tag {tag, ...} =>
                      let
                         val transfer =
                            Goto {args = if dstHasArg
                                            then Vector.new1 test
                                            else Vector.new0 (),
                                  dst = dst}
                      in
                         SOME (WordX.resize (tag, testSize),
                               Block.new {statements = Vector.new0 (),
                                          transfer = transfer})
                      end
                 | _ => NONE)
            val cases = QuickSort.sortVector (cases, fn ((w, _), (w', _)) =>
                                              WordX.le (w, w', {signed = false}))
            val tagOp =
               if isObjptr
                  then Operand.cast (test, Type.bits testBits)
               else test
            val (tagOp, ss) =
               if isEnum
                  then (tagOp, [])
               else
                  let
                     val mask =
                        Operand.word (WordX.resize
                                      (WordX.max (tagSize, {signed = false}),
                                       testSize))
                     val (s, tagOp) = Statement.andb (tagOp, mask)
                  in
                     (tagOp, [s])
                  end
            val default =
               if Vector.length variants = Vector.length cases
                  then notSmall
               else
                  case (notSmall, smallDefault) of
                     (NONE, _) => smallDefault
                   | (_, NONE) => notSmall
                   | (SOME notSmall, SOME smallDefault) =>
                        let
                           val (s, test) =
                              Statement.andb
                              (Operand.cast (test, Type.bits testBits),
                               Operand.word (WordX.fromIntInf (3, testSize)))
                           val t =
                              Switch
                              (Switch.T
                               {cases = Vector.new1 (WordX.zero testSize,
                                                     notSmall),
                                default = SOME smallDefault,
                                size = testSize,
                                test = test})
                        in
                           SOME (Block.new {statements = Vector.new1 s,
                                            transfer = t})
                        end
            val transfer =
               Switch (Switch.T {cases = cases,
                                 default = default,
                                 size = testSize,
                                 test = tagOp})
         in
            (ss, transfer)
         end

      val genCase =
         Trace.trace
         ("PackedRepresentation.Small.genCase",
          fn (s, {test, ...}) =>
          Layout.tuple [layout s,
                        Layout.record [("test", Operand.layout test)]],
          Layout.tuple2 (List.layout Statement.layout, Transfer.layout))
         genCase
   end

structure TyconRep =
   struct
      datatype t =
         One of {con: Con.t,
                 tupleRep: TupleRep.t}
       | Objptrs of Objptrs.t
       | Small of Small.t
       | SmallAndBox of {box: {con: Con.t,
                               objptr: ObjptrRep.t},
                         rep: Rep.t,
                         small: Small.t}
       | SmallAndObjptr of {objptr: {component: Component.t,
                                     con: Con.t},
                            rep: Rep.t,
                            small: Small.t}
       | SmallAndObjptrs of {objptrs: Objptrs.t,
                             rep: Rep.t,
                             small: Small.t}
       | Unit

      fun layout (r: t): Layout.t =
         let
            open Layout
         in
            case r of
               One {con, tupleRep} =>
                  seq [str "One ",
                       record [("con", Con.layout con),
                               ("tupleRep", TupleRep.layout tupleRep)]]
             | Objptrs ps =>
                  seq [str "Objptrs ", Objptrs.layout ps]
             | Small s =>
                  seq [str "Small ", Small.layout s]
             | SmallAndBox {box = {con, objptr}, rep, small} =>
                  seq [str "SmallAndBox ",
                       record [("box",
                                record [("con", Con.layout con),
                                        ("objptr", ObjptrRep.layout objptr)]),
                               ("rep", Rep.layout rep),
                               ("small", Small.layout small)]]
             | SmallAndObjptr {objptr = {component, con}, rep, small} =>
                  seq [str "SmallAndObjptr ",
                       record
                       [("objptr",
                         record [("component", Component.layout component),
                                 ("con", Con.layout con)]),
                        ("rep", Rep.layout rep),
                        ("small", Small.layout small)]]
             | SmallAndObjptrs {objptrs, rep, small} =>
                  seq [str "SmallAndObjptrs ",
                       record [("objptrs", Objptrs.layout objptrs),
                               ("rep", Rep.layout rep),
                               ("small", Small.layout small)]]
             | Unit => str "Unit"
         end

      val bool = Small Small.bool

      val unit = Unit

      val rep: t -> Rep.t =
         fn One {tupleRep, ...} => TupleRep.rep tupleRep
          | Objptrs p => Objptrs.rep p
          | Small s => Small.rep s
          | SmallAndBox {rep, ...} => rep
          | SmallAndObjptr {rep, ...} => rep
          | SmallAndObjptrs {rep, ...} => rep
          | Unit => Rep.unit

      fun equals (r, r') = Rep.equals (rep r, rep r')

      val objptrBytes = Runtime.objptrSize
      val objptrBits = Promise.lazy (fn () => Bytes.toBits (objptrBytes ()))
      val objptrBitsAsInt = Promise.lazy (fn () => Bits.toInt (objptrBits ()))

      local
         val aWithout =
            Promise.lazy
            (fn () => Array.tabulate (objptrBitsAsInt () + 1, fn i =>
                                      IntInf.pow (2, i)))
         (* If there is an objptr, then multiply the number of tags by
          * 3/4 to remove all the tags that have 00 as their low bits.
          *)
         val aWith =
            Promise.lazy
            (fn () => Array.tabulate (objptrBitsAsInt () + 1, fn i =>
                                      (Array.sub (aWithout (), i) * 3) div 4))
      in
         fun numTagsAvailable {tagBits: int, withObjptr: bool} =
            let
               val a = if withObjptr then aWith () else aWithout ()
            in
               Array.sub (a, tagBits)
            end

         val numTagsAvailable =
            Trace.trace
            ("PackedRepresentation.TyconRep.numTagsAvailable",
             fn {tagBits, withObjptr} =>
             Layout.record [("tagBits", Int.layout tagBits),
                            ("withObjptr", Bool.layout withObjptr)],
             IntInf.layout)
            numTagsAvailable

         fun tagBitsNeeded {numVariants: int, withObjptr: bool}: Bits.t =
            let
               val numVariants = Int.toIntInf numVariants
               val a = if withObjptr then aWith () else aWithout ()
            in
               case (BinarySearch.smallest
                     (a, fn numTags => numVariants <= numTags)) of
                  NONE => Error.bug "PackedRepresentation.TyconRep.tagBitsNeeded"
                | SOME i => Bits.fromInt i
            end

         val tagBitsNeeded =
            Trace.trace
            ("PackedRepresentation.TyconRep.tagBitsNeeded",
             fn {numVariants, withObjptr} =>
             Layout.record [("numVariants", Int.layout numVariants),
                            ("withObjptr", Bool.layout withObjptr)],
             Bits.layout)
            tagBitsNeeded
      end

      fun make (variants: {args: {isMutable: bool,
                                  rep: Rep.t,
                                  ty: S.Type.t} vector,
                           con: Con.t,
                           objptrTycon: ObjptrTycon.t} vector)
         : t * {con: Con.t, rep: ConRep.t} vector =
         if 0 = Vector.length variants
            then (Unit, Vector.new0 ())
         else if 1 = Vector.length variants
            then
               let
                  val {args, con, objptrTycon} = Vector.sub (variants, 0)
                  val tupleRep =
                     TupleRep.make (objptrTycon, args,
                                    {forceBox = false,
                                     isVector = false})
                  val conRep = ConRep.Tuple tupleRep
               in
                  (One {con = con, tupleRep = tupleRep},
                   Vector.new1 {con = con, rep = conRep})
               end
         else if (2 = Vector.length variants
                  andalso let
                             val c = #con (Vector.first variants)
                          in
                             Con.equals (c, Con.falsee)
                             orelse Con.equals (c, Con.truee)
                          end)
            then (bool, Vector.new2 ({con = Con.falsee, rep = ConRep.falsee},
                                     {con = Con.truee, rep = ConRep.truee}))
         else
         let
            val numSmall : IntInf.t ref = ref 0
            val small = Array.array (objptrBitsAsInt (), [])
            val big = ref []
            val () =
               Vector.foreach
               (variants, fn {args, con, objptrTycon} =>
                let
                   val tr =
                      TupleRep.make (objptrTycon, args,
                                     {forceBox = false,
                                      isVector = false})
                   fun makeBig () =
                      List.push (big,
                                 {con = con,
                                  objptrTycon = objptrTycon,
                                  tupleRep = tr})
                   val Rep.T {rep, ty} = TupleRep.rep tr
                in
                   case rep of
                      Rep.NonObjptr =>
                         let
                            val i = Bits.toInt (Type.width ty)
                         in
                            if i >= objptrBitsAsInt ()
                               then makeBig ()
                            else
                               let
                                  val {component, selects} =
                                     case tr of
                                        TupleRep.Direct z => z
                                      | TupleRep.Indirect _ =>
                                           Error.bug "PackedRepresentation.TyconRep.make: small Indirect"
                                  val () = IntInf.inc numSmall
                                  val () =
                                     Array.update
                                     (small, i,
                                      {component = component,
                                       con = con,
                                       objptrTycon = objptrTycon,
                                       selects = selects}
                                      :: Array.sub (small, i))
                               in
                                  ()
                               end
                         end
                    | Rep.Objptr _ => makeBig ()
                end)
            val big = !big
            val numSmall = !numSmall
            fun noLargerThan (i, ac) =
               if i < 0
                  then ac
               else (noLargerThan
                     (i - 1,
                      List.fold (Array.sub (small, i), ac, op ::)))
            (* Box as few things as possible so that the number of tags available
             * is >= the number of unboxed variants.
             *)
            fun loop (maxSmallWidth: int,
                      forced,
                      withObjptr: bool,
                      numSmall: IntInf.t) =
               if 0 = numSmall
                  then (maxSmallWidth, forced, [])
               else
                  let
                     val vs = Array.sub (small, maxSmallWidth)
                  in
                     if List.isEmpty vs
                        then loop (maxSmallWidth - 1, forced,
                                   withObjptr, numSmall)
                     else
                        let
                           val numTags =
                              numTagsAvailable
                              {tagBits = objptrBitsAsInt () - maxSmallWidth,
                               withObjptr = withObjptr}
                        in
                           if numSmall <= numTags
                              then
                                 (* There are enough tag bits available. *)
                                 (maxSmallWidth,
                                  forced,
                                  noLargerThan (maxSmallWidth - 1, vs))
                           else
                              let
                                 val z = Int.toIntInf (List.length vs)
                                 val remaining = numSmall - z
                              in
                                 if remaining <= numTags
                                    then
                                       let
                                          val (front, back) =
                                             List.splitAt
                                             (vs,
                                              IntInf.toInt
                                              (numSmall - numTags))
                                       in
                                          (maxSmallWidth,
                                           List.append (front, forced),
                                           noLargerThan (maxSmallWidth - 1,
                                                         back))
                                       end
                                 else loop (maxSmallWidth - 1,
                                            vs @ forced,
                                            true,
                                            remaining)
                              end
                        end
                  end
            val (maxSmallWidth, forced, small) =
               loop (objptrBitsAsInt () - 1, [],
                     not (List.isEmpty big),
                     numSmall)
            val maxSmallWidth = Bits.fromInt maxSmallWidth
            val withObjptr = not (List.isEmpty big andalso List.isEmpty forced)
            (* ShiftAndTag all the small. *)
            val (small: Small.t option, smallReps) =
               let
                  val numSmall = List.length small
               in
                  if 0 = numSmall
                     then (NONE, Vector.new0 ())
                  else
                     let
                        val tagBits =
                           tagBitsNeeded {numVariants = numSmall,
                                          withObjptr = withObjptr}
                        val r = ref 0w0
                        fun getTag (): IntInf.t =
                           let
                              val w = !r
                              val w =
                                 if withObjptr andalso
                                    0w0 = Word.andb (w, 0w3)
                                    then w + 0w1
                                 else w
                              val () = r := w + 0w1
                           in
                              Word.toIntInf w
                           end
                        val small =
                           Vector.fromListMap
                           (small, fn {component, con, selects, ...} =>
                            let
                               val tag =
                                  WordX.fromIntInf
                                  (getTag (), WordSize.fromBits tagBits)
                               val isUnit = Type.isUnit (Component.ty component)
                               val component =
                                  Component.padToWidth
                                  (component, maxSmallWidth)
                               val selects = Selects.lshift (selects, tagBits)
                               val ty =
                                  Type.seq
                                  (Vector.new2
                                   (Type.ofWordX tag,
                                    Component.ty component))
                               val ty =
                                  if withObjptr
                                     then Type.resize (ty, objptrBits ())
                                  else Type.padToPrim ty
                            in
                               {component = component,
                                con = con,
                                isUnit = isUnit,
                                selects = selects,
                                tag = tag,
                                ty = ty}
                            end)
                        val ty = Type.sum (Vector.map (small, #ty))
                        val rep = Rep.T {rep = Rep.NonObjptr, ty = ty}
                        val reps =
                           Vector.map
                           (small, fn {component, con, isUnit, selects, tag, ty,
                                       ...} =>
                            {con = con,
                             rep = if isUnit
                                      then ConRep.Tag {tag = tag, ty = ty}
                                   else (ConRep.ShiftAndTag
                                         {component = component,
                                          selects = selects,
                                          tag = tag,
                                          ty = ty})})
                        val isEnum =
                           Vector.forall
                           (reps, fn {rep, ...} =>
                            case rep of
                               ConRep.Tag _ => true
                             | _ => false)
                     in
                        (SOME (Small.T {isEnum = isEnum,
                                        rep = rep,
                                        tagBits = tagBits,
                                        variants = Vector.map (reps, #con)}),
                         reps)
                     end
               end
            fun makeSmallObjptr {component, con, objptrTycon, selects} =
               {con = con,
                objptr = (ObjptrRep.box
                          (Component.padToWidth (component, objptrBits ()),
                           objptrTycon, selects))}
            fun makeBigObjptr {con, objptrTycon, tupleRep} =
               let
                  val objptr =
                     case tupleRep of
                        TupleRep.Direct {component, selects} =>
                           ObjptrRep.box (component, objptrTycon, selects)
                      | TupleRep.Indirect p => p
               in
                  {con = con, objptr = objptr}
               end
            fun sumWithSmall (r: Rep.t): Rep.t =
               Rep.T {rep = Rep.Objptr {endsIn00 = false},
                      ty = Type.sum (Vector.new2
                                     (Rep.ty r,
                                      Rep.ty (Small.rep (valOf small))))}
            fun box () =
               let
                  val objptrs =
                     Vector.concat
                     [Vector.fromListMap (forced, makeSmallObjptr),
                      Vector.fromListMap (big, makeBigObjptr)]
                  val sumRep =
                     if 1 = Vector.length objptrs
                        then
                           let
                              val objptr = Vector.first objptrs
                              val small = valOf small
                              val rep =
                                 sumWithSmall (ObjptrRep.rep (#objptr objptr))
                           in
                              SmallAndBox {box = objptr,
                                           rep = rep,
                                           small = small}
                           end
                     else
                        let
                           val ty =
                              Type.sum
                              (Vector.map (objptrs, ObjptrRep.ty o #objptr))
                           val objptrs =
                              Objptrs.make
                              {rep = Rep.T {rep = Rep.Objptr {endsIn00 = true},
                                            ty = ty},
                               variants = objptrs}
                        in
                           case small of
                              NONE => Objptrs objptrs
                            | SOME small =>
                                 SmallAndObjptrs
                                 {objptrs = objptrs,
                                  rep = sumWithSmall (Objptrs.rep objptrs),
                                  small = small}
                        end
               in
                  (sumRep,
                   Vector.map (objptrs, fn {con, objptr} =>
                               {con = con,
                                rep = ConRep.box objptr}))
               end
            val (sumRep, objptrReps) =
               case (forced, big) of
                  ([], []) => (Small (valOf small), Vector.new0 ())
                | ([], [{con, tupleRep, ...}]) =>
                     (* If there is only one big and it is an objptr that
                      * ends in 00, then there is no need to box it.
                      *)
                     (case tupleRep of
                         TupleRep.Direct {component, ...} =>
                            let
                               val rep = TupleRep.rep tupleRep
                            in
                               if Rep.isObjptrEndingIn00 rep
                                  then
                                     let
                                        val small = valOf small
                                     in
                                        (SmallAndObjptr
                                         {objptr = {component = component,
                                                    con = con},
                                          rep = sumWithSmall rep,
                                          small = small},
                                         Vector.new1
                                         {con = con,
                                          rep = ConRep.Tuple tupleRep})
                                     end
                               else box ()
                            end
                       | _ => box ())
                | _ => box ()
         in
            (sumRep, Vector.concat [smallReps, objptrReps])
         end

      val make =
         Trace.trace
         ("PackedRepresentation.TyconRep.make",
          Vector.layout
          (fn {args, con, ...} =>
           Layout.record [("args", Vector.layout (Rep.layout o #rep) args),
                          ("con", Con.layout con)]),
          Layout.tuple2 (layout,
                         Vector.layout
                         (fn {con, rep} =>
                          Layout.record [("con", Con.layout con),
                                         ("rep", ConRep.layout rep)])))
         make

      fun genCase (r: t,
                   {cases: Cases.t,
                    conRep: Con.t -> ConRep.t,
                    default: Label.t option,
                    test: unit -> Operand.t})
         : Statement.t list * Transfer.t * Block.t list =
         let
            val (statements, transfer) =
               case r of
                  One {con, ...} =>
                     (case (Vector.length cases, default) of
                         (1, _) =>
                            (* Use _ instead of NONE for the default becuase
                             * there may be an unreachable default case.
                             *)
                            let
                               val {con = c, dst, dstHasArg} =
                                  Vector.first cases
                            in
                               if not (Con.equals (c, con))
                                  then Error.bug "PackedRepresentation.genCase: One"
                               else
                                  ([],
                                   Goto {args = (if dstHasArg
                                                    then Vector.new1 (test ())
                                                 else Vector.new0 ()),
                                         dst = dst})
                            end
                       | (0, SOME l) =>
                            ([], Goto {dst = l, args = Vector.new0 ()})
                       | _ => Error.bug "PackedRepresentation.genCase: One,prim datatype with more than one case")
                | Objptrs ps =>
                     Objptrs.genCase (ps, {cases = cases,
                                            conRep = conRep,
                                            default = default,
                                            test = test ()})
                | Small s =>
                     Small.genCase (s, {cases = cases,
                                        conRep = conRep,
                                        isObjptr = false,
                                        notSmall = NONE,
                                        smallDefault = default,
                                        test = test ()})
                | SmallAndBox {box = {con, objptr}, small, ...} =>
                     let
                        val notSmall =
                           case Vector.peek (cases, fn {con = c, ...} =>
                                             Con.equals (c, con)) of
                              NONE => default
                            | SOME {dst, dstHasArg, ...} =>
                                 let
                                    val test =
                                       Operand.cast (test (),
                                                     ObjptrRep.ty objptr)
                                 in
                                    SOME
                                    (Block.new
                                     {statements = Vector.new0 (),
                                      transfer =
                                      Goto {args = (if dstHasArg
                                                       then Vector.new1 test
                                                    else Vector.new0 ()),
                                            dst = dst}})
                                 end
                     in
                        Small.genCase (small, {cases = cases,
                                               conRep = conRep,
                                               isObjptr = true,
                                               notSmall = notSmall,
                                               smallDefault = default,
                                               test = test ()})
                     end
                | SmallAndObjptr {objptr = {component, con}, small, ...} =>
                     let
                        val notSmall =
                           case Vector.peek (cases, fn {con = c, ...} =>
                                             Con.equals (c, con)) of
                              NONE => default
                            | SOME {dst, dstHasArg, ...} =>
                                 let
                                    val args =
                                       if dstHasArg
                                          then (Vector.new1
                                                (Operand.cast
                                                 (test (),
                                                  Component.ty component)))
                                       else Vector.new0 ()
                                 in
                                    SOME (Block.new
                                          {statements = Vector.new0 (),
                                           transfer = Goto {args = args,
                                                            dst = dst}})
                                 end
                     in
                        Small.genCase (small, {cases = cases,
                                               conRep = conRep,
                                               isObjptr = true,
                                               notSmall = notSmall,
                                               smallDefault = default,
                                               test = test ()})
                     end
                | SmallAndObjptrs {objptrs, small, ...} =>
                     let
                        val test = test ()
                        val (ss, t) =
                           Objptrs.genCase
                           (objptrs, {cases = cases,
                                      conRep = conRep,
                                      default = default,
                                      test = (Operand.cast
                                              (test, Objptrs.ty objptrs))})
                        val objptr =
                           Block.new {statements = Vector.fromList ss,
                                      transfer = t}
                     in
                        Small.genCase (small, {cases = cases,
                                               conRep = conRep,
                                               isObjptr = true,
                                               notSmall = SOME objptr,
                                               smallDefault = default,
                                               test = test})
                     end
                | Unit => Error.bug "PackedRepresentation.TyconRep.genCase: Unit"
         in
            (statements, transfer, Block.getExtra ())
         end

      val genCase =
         Trace.trace
         ("PackedRepresentation.TyconRep.genCase",
          fn (r, {cases, default, ...}) =>
          Layout.tuple [layout r,
                        Layout.record
                        [("cases", Cases.layout cases),
                         ("default", Option.layout Label.layout default)]],
          Layout.tuple3 (List.layout Statement.layout,
                         Transfer.layout,
                         List.layout Block.layout))
         genCase
   end

structure Value:
   sig
      type 'a t

      val affect: 'a t * 'b t -> unit
      val constant: 'a -> 'a t
      val fixedPoint: unit -> unit
      val get: 'a t -> 'a
      val layout: ('a -> Layout.t) -> 'a t -> Layout.t
      val new: {compute: unit -> 'a,
                equals: 'a * 'a -> bool,
                init: 'a} -> 'a t
   end =
   struct
      structure Dep =
         struct
            datatype t = T of {affects: t list ref,
                               compute: unit -> {change: bool},
                               needToCompute: bool ref}

            (* A list of all ts such that !needToCompute = true. *)
            val todo: t list ref = ref []

            fun recompute (me as T {needToCompute, ...}) =
               if !needToCompute
                  then ()
               else (List.push (todo, me)
                     ; needToCompute := true)

            fun fixedPoint () =
               case !todo of
                  [] => ()
                | T {affects, compute, needToCompute, ...} :: l =>
                     let
                        val () = todo := l
                        val () = needToCompute := false
                        val {change} = compute ()
                        val () =
                           if change
                              then List.foreach (!affects, recompute)
                           else ()
                     in
                        fixedPoint ()
                     end

            fun affect (T {affects, ...}, z) = List.push (affects, z)

            fun new {compute: unit -> 'a,
                     equals: 'a * 'a -> bool,
                     init: 'a}: t * 'a ref =
               let
                  val r: 'a ref = ref init
                  val affects = ref []
                  val compute =
                     fn () =>
                     let
                        val old = !r
                        val new = compute ()
                        val () = r := new
                     in
                        {change = not (equals (old, new))}
                     end
                  val me = T {affects = affects,
                              compute = compute,
                              needToCompute = ref false}
                  val () = recompute me
               in
                  (me, r)
               end
         end

      datatype 'a t =
         Constant of 'a
       | Variable of Dep.t * 'a ref

      val get =
         fn Constant a => a
          | Variable (_, r) => !r

      fun layout l v = l (get v)

      val constant = Constant

      fun new z = Variable (Dep.new z)

      val affect =
         fn (Variable (d, _), Variable (d', _)) => Dep.affect (d, d')
          | (Constant _, _) => ()
          | (_, Constant _) => Error.bug "PackedRepresentation.Value.affect: Constant"

      val fixedPoint = Dep.fixedPoint
   end

fun compute (program as Ssa.Program.T {datatypes, ...}) =
   let
      type tyconRepAndCons =
         (TyconRep.t * {con: Con.t, rep: ConRep.t} vector) Value.t
      val {get = conInfo: Con.t -> {rep: ConRep.t ref,
                                    tyconRep: tyconRepAndCons},
           set = setConInfo, ...} =
         Property.getSetOnce (Con.plist, Property.initRaise ("info", Con.layout))
      val {get = tupleRep: S.Type.t -> TupleRep.t Value.t,
           set = setTupleRep, ...} =
         Property.getSetOnce (S.Type.plist,
                              Property.initRaise ("tupleRep", S.Type.layout))
      val setTupleRep =
         Trace.trace
         ("PackedRepresentation.setTupleRep",
          S.Type.layout o #1, Layout.ignore)
         setTupleRep
      fun vectorRep (t: S.Type.t): TupleRep.t = Value.get (tupleRep t)
      fun setVectorRep (t: S.Type.t, tr: TupleRep.t): unit =
         setTupleRep (t, Value.new {compute = fn () => tr,
                                    equals = TupleRep.equals,
                                    init = tr})
      val setVectorRep =
         Trace.trace2
         ("PackedRepresentation.setVectorRep",
          S.Type.layout, TupleRep.layout, Unit.layout)
         setVectorRep
      val {get = tyconRep: Tycon.t -> tyconRepAndCons, set = setTyconRep, ...} =
         Property.getSetOnce (Tycon.plist,
                              Property.initRaise ("tyconRep", Tycon.layout))
      (* Initialize the datatypes. *)
      val typeRepRef = ref (fn _ => Error.bug "PackedRepresentation.typeRep")
      fun typeRep t = !typeRepRef t
      val datatypes =
         Vector.map
         (datatypes, fn S.Datatype.T {cons, tycon} =>
          let
             val cons =
                Vector.map
                (cons, fn {args, con} =>
                 {args = args,
                  con = con,
                  objptrTycon = ObjptrTycon.new ()})
             fun compute () =
                let
                   val (tr, cons) =
                      TyconRep.make
                      (Vector.map
                       (cons, fn {args, con, objptrTycon} =>
                        {args = Vector.map (Prod.dest args,
                                            fn {elt, isMutable} =>
                                            {isMutable = isMutable,
                                             rep = Value.get (typeRep elt),
                                             ty = elt}),
                         con = con,
                         objptrTycon = objptrTycon}))
                   val () =
                      Vector.foreach
                      (cons, fn {con, rep} => #rep (conInfo con) := rep)
                in
                   (tr, cons)
                end
             fun equals ((r, v), (r', v')) =
                TyconRep.equals (r, r')
                andalso Vector.equals (v, v', fn ({con = c, rep = r},
                                                  {con = c', rep = r'}) =>
                                       Con.equals (c, c')
                                       andalso ConRep.equals (r, r'))
             val rep =
                Value.new {compute = compute,
                           equals = equals,
                           init = (TyconRep.unit, Vector.new0 ())}
             val () = setTyconRep (tycon, rep)
             val () = Vector.foreach (cons, fn {con, ...} =>
                                      setConInfo (con, {rep = ref ConRep.unit,
                                                        tyconRep = rep}))
          in
             {cons = cons,
              rep = rep,
              tycon = tycon}
          end)
      val delayedObjectTypes
         : (unit -> (ObjptrTycon.t * ObjectType.t) option) list ref =
         ref []
      val {get = typeRep: S.Type.t -> Rep.t Value.t, ...} =
         Property.get
         (S.Type.plist,
          Property.initRec
          (fn (t, typeRep: S.Type.t -> Rep.t Value.t) =>
           let
              val constant = Value.constant
              val nonObjptr = constant o Rep.nonObjptr
              datatype z = datatype S.Type.dest
           in
              case S.Type.dest t of
                 CPointer => nonObjptr (Type.cpointer ())
               | Datatype tycon =>
                    let
                       val r = tyconRep tycon
                       fun compute () = TyconRep.rep (#1 (Value.get r))
                       val r' = Value.new {compute = compute,
                                           equals = Rep.equals,
                                           init = Rep.unit}
                       val () = Value.affect (r, r')
                    in
                       r'
                    end
               | IntInf =>
                    constant (Rep.T {rep = Rep.Objptr {endsIn00 = false},
                                     ty = Type.intInf ()})
               | Object {args, con} =>
                    (case con of
                        ObjectCon.Con con =>
                           let
                              val {rep, tyconRep} = conInfo con
                              fun compute () = ConRep.rep (!rep)
                              val r = Value.new {compute = compute,
                                                 equals = Rep.equals,
                                                 init = Rep.unit}
                              val () = Value.affect (tyconRep, r)
                           in
                              r
                           end
                      | ObjectCon.Tuple =>
                           let
                              val opt = ObjptrTycon.new ()
                              val rs =
                                 Vector.map (Prod.dest args, typeRep o #elt)
                              fun compute () =
                                 TupleRep.make
                                 (opt,
                                  Vector.map2 (rs, Prod.dest args,
                                               fn (r, {elt, isMutable}) =>
                                               {isMutable = isMutable,
                                                rep = Value.get r,
                                                ty = elt}),
                                  {forceBox = false, isVector = false})
                              val tr =
                                 Value.new {compute = compute,
                                            equals = TupleRep.equals,
                                            init = TupleRep.unit}
                              val () = Vector.foreach (rs, fn r =>
                                                       Value.affect (r, tr))
                              val hasIdentity = Prod.someIsMutable args
                              val () =
                                 List.push
                                 (delayedObjectTypes, fn () =>
                                  case Value.get tr of
                                     TupleRep.Indirect opr =>
                                        SOME
                                        (opt, (ObjectType.Normal
                                               {hasIdentity = hasIdentity,
                                                ty = ObjptrRep.componentsTy opr}))
                                   | _ => NONE)
                              val () = setTupleRep (t, tr)
                              fun compute () = TupleRep.rep (Value.get tr)
                              val r = Value.new {compute = compute,
                                                 equals = Rep.equals,
                                                 init = Rep.unit}
                              val () = Value.affect (tr, r)
                           in
                              r
                           end
                      | ObjectCon.Vector =>
                           let
                              val hasIdentity = Prod.someIsMutable args
                              val args = Prod.dest args
                              fun tupleRep opt =
                                 let
                                    val tr =
                                       TupleRep.make
                                       (opt,
                                        Vector.map
                                        (args, fn {elt, isMutable} =>
                                         {isMutable = isMutable,
                                          rep = Value.get (typeRep elt),
                                          ty = elt}),
                                        {forceBox = true,
                                         isVector = true})
                                    val () = setVectorRep (t, tr)
                                 in
                                    tr
                                 end
                              fun now opt = (ignore (tupleRep opt); opt)
                              fun delay () =
                                 let
                                    val opt = ObjptrTycon.new ()
                                    val () =
                                       List.push
                                       (delayedObjectTypes, fn () =>
                                        let
                                           (* Delay computing tupleRep until the
                                            * delayedObjectTypes are computed
                                            * because the vector component types
                                            * may not be known yet.
                                            *)
                                           val tr = tupleRep opt
                                           val ty =
                                              case tr of
                                                 TupleRep.Direct _ =>
                                                    TupleRep.ty tr
                                               | TupleRep.Indirect opr =>
                                                    ObjptrRep.componentsTy opr
                                        in
                                           SOME (opt,
                                                 ObjectType.Array
                                                 {elt = ty,
                                                  hasIdentity = hasIdentity})
                                        end)
                                 in
                                    opt
                                 end
                              val opt =
                                 if 1 <> Vector.length args
                                    then delay ()
                                 else
                                    let
                                       val {elt, isMutable, ...} =
                                          Vector.sub (args, 0)
                                    in
                                       if isMutable
                                          then delay ()
                                       else
                                          (case S.Type.dest elt of
                                              S.Type.Word s =>
                                                 let
                                                    val nBits = WordSize.bits s
                                                    val nInt = Bits.toInt nBits
                                                 in
                                                    if nInt = 8
                                                       orelse nInt = 16
                                                       orelse nInt = 32
                                                       orelse nInt = 64
                                                       then
                                                          now
                                                          (ObjptrTycon.wordVector nBits)
                                                    else delay ()
                                                 end
                                            | _ => delay ())
                                    end
                           in
                              constant
                              (Rep.T {rep = Rep.Objptr {endsIn00 = true},
                                      ty = Type.objptr opt})
                           end)
               | Real s => nonObjptr (Type.real s)
               | Thread =>
                    constant (Rep.T {rep = Rep.Objptr {endsIn00 = true},
                                     ty = Type.thread ()})
               | Weak t =>
                    let
                       val opt = ObjptrTycon.new ()
                       val rep =
                          Rep.T {rep = Rep.Objptr {endsIn00 = true},
                                 ty = Type.objptr opt}
                       val r = typeRep t
                       fun compute () =
                          if Rep.isObjptr (Value.get r)
                             then rep
                          else Rep.unit
                       val r' = Value.new {compute = compute,
                                           equals = Rep.equals,
                                           init = Rep.unit}
                       val () = Value.affect (r, r')
                       val () =
                          List.push
                          (delayedObjectTypes, fn () =>
                           let
                              val r = Value.get r
                           in
                              if Rep.isObjptr r
                                 then SOME (opt, ObjectType.Weak (SOME (Rep.ty r)))
                              else NONE
                           end)
                    in
                       r'
                    end
             | Word s => nonObjptr (Type.word s)
           end))
      val () = typeRepRef := typeRep
      val _ = typeRep (S.Type.vector1 (S.Type.word WordSize.byte))
      (* Establish dependence between constructor argument type representations
       * and tycon representations.
       *)
      val () =
         Vector.foreach
         (datatypes, fn {cons, rep, ...} =>
          Vector.foreach
          (cons, fn {args, ...} =>
           Vector.foreach (Prod.dest args, fn {elt, ...} =>
                           Value.affect (typeRep elt, rep))))
      val typeRep =
         Trace.trace
         ("PackedRepresentation.typeRep",
          S.Type.layout, Value.layout Rep.layout)
         typeRep
      val () = S.Program.foreachVar (program, fn (_, t) => ignore (typeRep t))
      val () = Value.fixedPoint ()
      val conRep = ! o #rep o conInfo
      val tyconRep = #1 o Value.get o tyconRep
      val objectTypes =
         Vector.fold
         (datatypes, [], fn ({cons, ...}, ac) =>
          Vector.fold
          (cons, ac, fn ({args, con, objptrTycon, ...}, ac) =>
           case conRep con of
              ConRep.Tuple (TupleRep.Indirect opr) =>
                 (objptrTycon,
                  ObjectType.Normal {hasIdentity = Prod.someIsMutable args,
                                     ty = ObjptrRep.componentsTy opr}) :: ac
            | _ => ac))
      val objectTypes = ref objectTypes
      val () =
         List.foreach (!delayedObjectTypes, fn f =>
                       Option.app (f (), fn z => List.push (objectTypes, z)))
      val objectTypes = Vector.fromList (!objectTypes)
      fun diagnostic () =
         Control.diagnostics
         (fn display =>
          (display (Layout.str "Representations:")
           ; (Vector.foreach
              (datatypes, fn {cons, tycon, ...} =>
               let
                  open Layout
               in
                  display (seq [Tycon.layout tycon,
                                str " ", TyconRep.layout (tyconRep tycon)])
                  ; display (indent
                             (Vector.layout
                              (fn {con, ...} =>
                               record [("con", Con.layout con),
                                       ("rep", ConRep.layout (conRep con))])
                              cons,
                              2))
               end))))
      fun toRtype (t: S.Type.t): Type.t option =
         let
            val ty = Rep.ty (Value.get (typeRep t))
         in
            if Type.isUnit ty
               then NONE
            else SOME (Type.padToPrim ty)
         end
      fun makeSrc (v, oper) {index} = oper (Vector.sub (v, index))
      fun genCase {cases, default, test, tycon} =
         TyconRep.genCase (tyconRep tycon,
                           {cases = cases,
                            conRep = conRep,
                            default = default,
                            test = test})
      val tupleRep = Value.get o tupleRep
      val tupleRep =
         Trace.trace
         ("PackedRepresentation.tupleRep",
          S.Type.layout, TupleRep.layout)
         tupleRep
      fun object {args, con, dst, objectTy, oper} =
         let
            val src = makeSrc (args, oper)
         in
            case con of
               NONE => TupleRep.tuple (tupleRep objectTy, {dst = dst, src = src})
             | SOME con => ConRep.conApp (conRep con, {dst = dst, src = src})
         end
      fun getSelects (con, objectTy) =
         let
            datatype z = datatype ObjectCon.t
         in
            case con of
               Con con =>
                  (case conRep con of
                      ConRep.ShiftAndTag {selects, ...} => (selects, NONE)
                    | ConRep.Tuple tr => (TupleRep.selects tr, NONE)
                    | _ => Error.bug "PackedRepresentation.getSelects: Con,non-select")
             | Tuple => (TupleRep.selects (tupleRep objectTy), NONE)
             | Vector =>
                  case vectorRep objectTy of
                     tr as TupleRep.Indirect pr =>
                        (TupleRep.selects tr,
                         SOME (Type.bytes (ObjptrRep.componentsTy pr)))
                   | _ => Error.bug "PackedRepresentation.getSelects: Vector,non-Indirect"
         end
      fun select {base, baseTy, dst, offset} =
         case S.Type.dest baseTy of
            S.Type.Object {con, ...} =>
               let
                  val (ss, eltWidth) = getSelects (con, baseTy)
               in
                  Selects.select
                  (ss, {base = base,
                        eltWidth = eltWidth,
                        dst = dst,
                        offset = offset})
               end
          | _ => Error.bug "PackedRepresentation.select: non-object"
      fun update {base, baseTy, offset, value} =
         case S.Type.dest baseTy of
            S.Type.Object {con, ...} =>
               let
                  val (ss, eltWidth) = getSelects (con, baseTy)
               in
                  Selects.update (ss, {base = base,
                                       eltWidth = eltWidth,
                                       offset = offset,
                                       value = value})
               end
          | _ => Error.bug "PackedRepresentation.update: non-object"
   in
      {diagnostic = diagnostic,
       genCase = genCase,
       object = object,
       objectTypes = objectTypes,
       select = select,
       toRtype = toRtype,
       update = update}
   end

end
