(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
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
   structure PointerTycon = PointerTycon
   structure Prim = Prim
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

      fun padToPrim (t: t): t =
	 let
	    val b = Bits.toInt (width t)
	    fun check (b', continue) =
	       if b < b'
		  then seq (Vector.new2 (t, zero (Bits.fromInt (b' - b))))
	       else if b = b'
		       then t
		    else continue ()
	 in
	    if 0 = b
	       then t
	    else
	       check (8, fn () =>
		      check (16, fn () =>
			     check (32, fn () =>
				    if b = 64
				       then t
				    else Error.bug (concat ["Type.padToPrim ",
							    Int.toString b]))))
	 end

      fun padToWidth (t: t, b: Bits.t): t =
	 if Bits.< (b, width t)
	    then Error.bug "Type.padToWidth"
	 else seq (Vector.new2 (t, zero (Bits.- (b, width t))))

      val padToWidth =
	 Trace.trace2 ("Type.padToWidth", layout, Bits.layout, layout) padToWidth
   end

structure Rep =
   struct
      datatype rep =
	 NonPointer
       | Pointer of {endsIn00: bool}

      datatype t = T of {rep: rep,
			 ty: Type.t}

      fun layout (T {rep, ty}) =
	 let
	    open Layout
	 in
	    record [("rep",
		     case rep of
			NonPointer => str "NonPointer"
		      | Pointer {endsIn00} =>
			   seq [str "Pointer ",
				record [("endsIn00", Bool.layout endsIn00)]]),
		    ("ty", Type.layout ty)]
	 end

      local
	 fun make f (T r) = f r
      in
	 val ty = make #ty
      end

      fun equals (r, r') = Type.equals (ty r, ty r')

      val equals =
	 Trace.trace2 ("Rep.equals", layout, layout, Bool.layout) equals

      fun nonPointer ty = T {rep = NonPointer,
			     ty = ty}

      val bool = nonPointer Type.bool
	 
      val width = Type.width o ty

      val unit = T {rep = NonPointer,
		    ty = Type.unit}

      fun isPointer (T {rep, ...}) =
	 case rep of
	    Pointer _ => true
	  | _ => false

      fun isPointerEndingIn00 (T {rep, ...}) =
	 case rep of
	    Pointer {endsIn00} => endsIn00
	  | _ => false

      fun padToWidth (r as T {rep, ty}, width: Bits.t) =
	 if Bits.equals (Type.width ty, width)
	    then r
	 else
	    case rep of
	       NonPointer =>
		  T {rep = NonPointer,
		     ty = Type.padToWidth (ty, width)}
	     | Pointer _ => Error.bug "Rep.padToWidth"
   end

structure Statement =
   struct
      open Statement

      local
	 fun make (doType, prim) (z1: Operand.t, z2: Operand.t) =
	    let
	       val t1 = Operand.ty z1
	       val tmp = Var.newNoname ()
	       val tmpTy = doType (t1, Operand.ty z2)
	    in
	       (PrimApp {args = Vector.new2 (z1, z2),
			 dst = SOME (tmp, tmpTy),
			 prim = prim (WordSize.fromBits (Type.width t1))},
		Var {ty = tmpTy, var = tmp})
	    end
      in
	 val andb = make (valOf o Type.andb, Prim.wordAndb)
	 val lshift = make (Type.lshift, Prim.wordLshift)
	 val orb = make (valOf o Type.orb, Prim.wordOrb)
	 val rshift = make (Type.rshift, fn s =>
			    Prim.wordRshift (s, {signed = false}))
      end
   end

structure WordRep =
   struct
      (* WordRep describes the representation of (some of) the components in a
       * tuple as a word.
       * components are stored from lowest to highest, just like in Type.seq.
       * The width of the rep must be less than the width of a pointer.
       * The sum of the widths of the component reps must be less than the
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
	 if Bits.<= (Rep.width rep, Bits.inWord)
	    andalso Bits.<= (Vector.fold (components, Bits.zero,
					  fn ({rep, ...}, ac) =>
					  Bits.+ (ac, Rep.width rep)),
			     Rep.width rep)
	    then T {components = components,
		    rep = rep}
	 else Error.bug "WordRep.make"

      fun padToWidth (T {components, rep}, b: Bits.t): t =
	 make {components = components,
	       rep = Rep.padToWidth (rep, b)}
 
      fun tuple (T {components, ...},
		 {dst = (dstVar, dstTy): Var.t * Type.t,
		  src: {index: int} -> Operand.t}): Statement.t list =
	 let
	    val bits = Type.width dstTy
	    val wordSize = WordSize.fromBits bits
	    val z =
	       Vector.fold
	       (components, NONE, fn ({index, rep, ...}, z) =>
		let
		   val (src, ss) = Statement.resize (src {index = index}, bits)
		in
		   case z of
		      NONE => SOME (src, Rep.width rep, [rev ss])
		    | SOME (ac, shift, statements) =>
			 let
			    val (s1, tmp) =
			       Statement.lshift
			       (src,
				Operand.word
				(WordX.fromIntInf (Bits.toIntInf shift,
						   WordSize.default)))
			    val (s2, ac) = Statement.orb (tmp, ac)
			 in
			    SOME (ac, Bits.+ (shift, Rep.width rep),
				  ([s2, s1] @ rev ss) :: statements)
			 end
		end)
	    val (src, statements) =
	       case z of
		  NONE => (Operand.word (WordX.zero wordSize), [])
		| SOME (src, _, ss) => (src, ss)
	    val statements =
	       [Bind {dst = (dstVar, dstTy),
		      isMutable = false,
		      src = src}]
	       :: statements
	 in
	    List.fold (statements, [], fn (ss, ac) => List.fold (ss, ac, op ::))
	 end

      val tuple =
	 Trace.trace ("WordRep.tuple", layout o #1, List.layout Statement.layout)
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

      val width = Type.width o ty

      val unit = Word WordRep.unit

      val equals: t * t -> bool =
	 fn z =>
	 case z of
	    (Direct {rep = r, ...}, Direct {rep = r', ...}) => Rep.equals (r, r')
	  | (Word wr, Word wr') => WordRep.equals (wr, wr')
	  | _ => false

      fun padToWidth (c: t, b: Bits.t): t =
	 case c of
	    Direct {index, rep} =>
	       Direct {index = index,
		       rep = Rep.padToWidth (rep, b)}
	  | Word r => Word (WordRep.padToWidth (r, b))

      fun maybePadToWidth (c, b) =
	 if Bits.< (b, width c) then c else padToWidth (c, b)

      fun padToPrim (c: t): t =
	 let
	    val t = ty c
	    val t' = Type.padToPrim t
	 in
	    if Type.equals (t, t')
	       then c
	    else padToWidth (c, Type.width t')
	 end

      fun tuple (c: t, {dst: Var.t * Type.t,
			src: {index: int} -> Operand.t})
	 : Statement.t list =
	 case c of
	    Direct {index, ...} =>
	       let
		  val (src, ss) = Statement.resize (src {index = index},
						    Type.width (#2 dst))
	       in
		  ss @ [Bind {dst = dst,
			      isMutable = false,
			      src = src}]
	       end
	  | Word wr => WordRep.tuple (wr, {dst = dst, src = src})

      val tuple =
	 Trace.trace2 ("Component.tuple",
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
		     val (s, tmp) =
			Statement.rshift
			(src,
			 Operand.word (WordX.fromIntInf (Bits.toIntInf shift,
							 WordSize.default)))
		  in
		     (tmp, [s])
		  end
	    val w = Type.width ty
	    val s = WordSize.fromBits w
	    val w' = Type.width dstTy
	    val s' = WordSize.fromBits w'
	    val (src, ss2) = Statement.resize (src, w')
	    val (src, ss3) = 
	       if Bits.equals (w, w')
(* 		  orelse Type.isZero (Type.dropPrefix (Operand.ty src,
 * 						       WordSize.bits s))
 *)
		  then (src, [])
	       else
		  let
		     val (s, src) =
			Statement.andb
			(src,
			 Operand.word (WordX.resize
				       (WordX.max (s, {signed = false}), s')))
					    
		  in
		     (src, [s])
		  end
	 in
	    ss1 @ ss2 @ ss3 @ [Bind {dst = (dst, dstTy),
				     isMutable = false,
				     src = src}]
	 end

      val select =
	 Trace.trace2 ("Unpack.select", layout,
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
	       WordX.fromIntInf (Bits.toIntInf shift, WordSize.default)
	    val chunkWidth = Type.width (Operand.ty chunk)
	    val mask =
	       Operand.word
	       (WordX.notb
		(WordX.lshift
		 (WordX.resize (WordX.allOnes (WordSize.fromBits
					       (Type.width ty)),
				WordSize.fromBits chunkWidth),
		  shift)))
	    val (s1, chunk) = Statement.andb (chunk, mask)
	    val (component, s2) = Statement.resize (component, chunkWidth)
	    val (s3, component) =
	       Statement.lshift (component, Operand.word shift)
	    val (s4, result) = Statement.orb (chunk, component)
	 in
	    (result, [s1] @ s2 @ [s3, s4])
	 end

      val update =
	 Trace.trace2
	 ("Unpack.update",
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
			NONE => Error.bug "Base.toOperand missing eltWidth"
		      | SOME w => w
	       in
		  case Scale.fromInt (Bytes.toInt eltWidth) of
		     NONE =>
			let
			   val size = WordSize.default
			   val wty = Type.word (WordSize.bits size)
			   (* vector + (width * index) + offset *)
			   val prod = Var.newNoname ()
			   val s1 =
			      PrimApp {args = (Vector.new2
					       (index,
						Operand.word
						(WordX.fromIntInf
						 (Bytes.toIntInf eltWidth,
						  size)))),
				       dst = SOME (prod, wty),
				       prim = Prim.wordMul (size,
							    {signed = false})}
			   val eltBase = Var.newNoname ()
			   val s2 =
			      PrimApp {args = (Vector.new2
					       (vector,
						Operand.Var {ty = wty,
							     var = prod})),
				       dst = SOME (eltBase, wty),
				       prim = Prim.wordAdd size}
			in
			   (Offset {base = Operand.Var {ty = wty,
							var = eltBase},
				    offset = offset,
				    ty = ty},
			    [s1, s2])
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
       | IndirectUnpack of {offset: Words.t,
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
		       record [("offset", Words.layout offset),
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
	  | _ => Error.bug "Select.lshift"

      fun indirectUnpack {offset, rest as Unpack.T {shift, ty = ty'}, ty} =
	 if Bits.isByteAligned shift
	    andalso Bits.equals (Type.width ty', Bits.inByte)
	    then Indirect {offset = (Rssa.byteOffset
				     {offset = Bytes.+ (Words.toBytes offset,
							Bits.toBytes shift),
				      ty = ty'}),
			   ty = ty'}
	 else IndirectUnpack {offset = offset,
			      rest = rest,
			      ty = ty}

      fun select (s: t, {base: Operand.t Base.t,
			 dst: Var.t * Type.t,
			 eltWidth: Bytes.t option}): Statement.t list =
	 let
	    fun move (src, ss) =
	       let
		  val (dst, dstTy) = dst
		  val (src, ss') = Statement.resize (src, Type.width dstTy)
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
					offset = Words.toBytes offset,
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
	 Trace.trace ("Select.select", layout o #1, List.layout Statement.layout)
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
				     offset = Words.toBytes offset,
				     ty = ty}
		  val (newChunk, ss') = 
		     Unpack.update (rest, {chunk = chunk,
					   component = value})
	       in
		  ss @ ss' @ [Move {dst = chunk, src = newChunk}]
	       end
	  | _ => Error.bug "Select.update of non indirect"

      val update =
	 Trace.trace ("Select.update", layout o #1, List.layout Statement.layout)
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

structure PointerRep =
   struct
      datatype t = T of {components: {component: Component.t,
				      offset: Words.t} vector,
			 componentsTy: Type.t,
			 selects: Selects.t,
			 ty: Type.t,
			 tycon: PointerTycon.t}

      fun layout (T {components, componentsTy, selects, ty, tycon}) =
	 let
	    open Layout
	 in
	    record
	    [("components",
	      Vector.layout (fn {component, offset} =>
			     record [("component", Component.layout component),
				     ("offset", Words.layout offset)])
	      components),
	     ("componentsTy", Type.layout componentsTy),
	     ("selects", Selects.layout selects),
	     ("ty", Type.layout ty),
	     ("tycon", PointerTycon.layout tycon)]
	 end

      local
	 fun make f (T r) = f r
      in
	 val componentsTy = make #componentsTy
	 val ty = make #ty
	 val tycon = make #tycon
      end

      fun equals (T {tycon = c, ...}, T {tycon = c', ...}) =
	 PointerTycon.equals (c, c')

      fun rep (T {ty, ...}) =
	 Rep.T {rep = Rep.Pointer {endsIn00 = true},
		ty = ty}

      fun make {components, isVector, selects, tycon} =
	 let
	    val width =
	       Vector.fold
	       (components, Bytes.zero, fn ({component = c, ...}, ac) =>
		Bytes.+ (ac, Type.bytes (Component.ty c)))
	    val (components, selects) =
	       if isVector
		  orelse !Control.align = Control.Align4
		  orelse
		  let
		     val totalWidth = Bytes.+ (width, Runtime.normalHeaderSize)
		  in
		     Bytes.equals
		     (totalWidth,
		      Bytes.align (totalWidth, {alignment = Bytes.fromInt 8}))
		  end
		  then (components, selects)
	       else
		  (* Need to insert a pad word before the first pointer. *)
		  let
		     val {no = nonPointers, yes = pointers} =
			Vector.partition
			(components, fn {component = c, ...} =>
			 Rep.isPointer (Component.rep c))
		     val padOffset =
			if 0 = Vector.length pointers
			   then Bytes.toWords width
			else #offset (Vector.sub (pointers, 0))
		     val pad =			      
			{component = Component.padToWidth (Component.unit,
							   Bits.inWord),
			 offset = padOffset}
		     val pointers =
			Vector.map (pointers, fn {component = c, offset} =>
				    {component = c,
				     offset = Words.+ (offset, Words.one)})
		     val components = 
			Vector.concat [nonPointers, Vector.new1 pad, pointers]
		     val selects =
			Selects.map
			(selects, fn s =>
			 case s of
			    Select.Indirect {offset, ty} =>
			       if Bytes.>= (offset, Words.toBytes padOffset)
				  then
				     Select.Indirect
				     {offset = Bytes.+ (offset, Bytes.inWord),
				      ty = ty}
			       else s
			  | _ => s)
		  in
		     (components, selects)
		  end
	    val componentsTy =
	       Type.seq (Vector.map (components, Component.ty o #component))
	    (* If there are no components, then add a pad word. *)
	    val componentsTy =
	       if Bits.isZero (Type.width componentsTy)
		  then Type.zero Bits.inWord
	       else componentsTy
	 in
	    T {components = components,
	       componentsTy = componentsTy,
	       selects = selects,
	       ty = Type.pointer tycon,
	       tycon = tycon}
	 end

      val make =
	 let
	    open Layout
	 in
	    Trace.trace
	    ("PointerRep.make",
	     fn {components, isVector, selects, tycon} =>
	     record
	     [("components",
	       Vector.layout (fn {component, offset} =>
			      record [("component", Component.layout component),
				      ("offset", Words.layout offset)])
	       components),
	      ("isVector", Bool.layout isVector),
	      ("selects", Selects.layout selects),
	      ("tycon", PointerTycon.layout tycon)],
	     layout)
	 end
	 make

      fun box (component: Component.t, pt: PointerTycon.t, selects: Selects.t) =
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
		    | Unpack u =>
			 IndirectUnpack {offset = Words.zero,
					 rest = u,
					 ty = Component.ty component}
		    | _ => Error.bug "PointerRep.box cannot lift selects"
		end)
	 in
	    make {components = Vector.new1 {component = component,
					    offset = Words.zero},
		  isVector = false,
		  selects = selects,
		  tycon = pt}
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
		in
		   Component.tuple (component,
				    {dst = (tmpVar, tmpTy), src = src})
		   @ (Move {dst = Offset {base = object,
					  offset = Words.toBytes offset,
					  ty = tmpTy},
			    src = Var {ty = tmpTy, var = tmpVar}}
		      :: ac)
		end)
	 in
	    Object {dst = (dst, ty),
		    header = (Runtime.typeIndexToHeader
			      (PointerTycon.index tycon)),
		    size = Bytes.toWords (Bytes.+ (Type.bytes componentsTy,
						   Runtime.normalHeaderSize))}
	    :: stores
	 end

      val tuple =
	 Trace.trace2 ("PointerRep.tuple", layout, Var.layout o #dst,
		       List.layout Statement.layout)
	 tuple
   end

structure TupleRep =
   struct
      datatype t =
	 Direct of {component: Component.t,
		    selects: Selects.t}
       | Indirect of PointerRep.t

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
		  seq [str "Indirect ", PointerRep.layout pr]
	 end

      val unit = Direct {component = Component.unit,
			 selects = Selects.empty}

      val equals: t * t -> bool =
	 fn z =>
	 case z of
	    (Direct {component = c, ...}, Direct {component = c', ...}) =>
	       Component.equals (c, c')
	  | (Indirect pr, Indirect pr') => PointerRep.equals (pr, pr')
	  | _ => false

      fun rep (tr: t): Rep.t =
	 case tr of
	    Direct {component, ...} => Component.rep component
	  | Indirect p => PointerRep.rep p

      val ty = Rep.ty o rep
	 
      fun selects (tr: t): Selects.t =
	 case tr of
	    Direct {selects, ...} => selects
	  | Indirect (PointerRep.T {selects, ...}) => selects

      fun tuple (tr: t,
		 {dst: Var.t * Type.t,
		  src: {index: int} -> Operand.t}): Statement.t list =
	 case tr of
	    Direct {component = c, ...} =>
	       Component.tuple (c, {dst = dst, src = src})
	  | Indirect pr =>
	       PointerRep.tuple (pr, {dst = #1 dst, src = src})
 
      val tuple =
	 Trace.trace2 ("TupleRep.tuple",
		       layout,
		       Var.layout o #1 o #dst,
		       List.layout Statement.layout)
	 tuple

      (* TupleRep.make decides how to layout a sequence of types in an object,
       * or in the case of a vector, in a vector element.
       * Vectors are treated slightly specially because we don't require element
       * widths to be a multiple of the word size.
       * At the front of the object, we place all the doubleWords, followed by
       * all the words.  Then, we pack in all the types that are smaller than a
       * word.  This is done by packing in a sequence of words, greedily,
       * starting with the largest type and moving to the smallest.  We pad to
       * ensure that a value never crosses a word boundary.  Finally, if there
       * are any pointers, they go at the end of the object.
       *)
      fun make (pointerTycon: PointerTycon.t,
		rs: {isMutable: bool,
		     rep: Rep.t,
		     ty: S.Type.t} vector,
		{forceBox: bool,
		 isVector: bool}): t =
	 let
	    val pointers = ref []
	    val doubleWords = ref []
	    val words = ref []
	    val a = Array.array (Bits.toInt Bits.inWord, [])
	    val () =
	       Vector.foreachi
	       (rs, fn (i, {rep = r as Rep.T {rep, ty}, ...}) =>
		let
		   fun direct l =
		      List.push
		      (l, {component = Component.Direct {index = i, rep = r},
			   index = i})
		in
		   case rep of
		      Rep.NonPointer =>
			 let
			    val b = Bits.toInt (Type.width ty)
			 in
			    case b of
			       32 => direct words
			     | 64 => direct doubleWords
			     | _ =>
				  Array.update
				  (a, b,
				   {index = i, rep = r} :: Array.sub (a, b))
			 end
		    | Rep.Pointer _ => direct pointers
		end)
	    val selects = Array.array (Vector.length rs,
				       (Select.None, Select.None))
	    fun simple (l, width: Words.t, offset: Words.t, components) =
	       List.fold
	       (l, (offset, components),
		fn ({component, index}, (offset, ac)) =>
		(Words.+ (offset, width),
		 let
		    val ty = Component.ty component
		    val () =
		       Array.update
		       (selects, index,
			(Select.Direct {ty = ty},
			 Select.Indirect {offset = Words.toBytes offset,
					  ty = ty}))
		 in
		    {component = component,
		     offset = offset,
		     setSelects = fn _ => ()} :: ac
		 end))
	    val offset = Words.zero
	    val components = []
	    val (offset, components) =
	       simple (!doubleWords, Words.fromInt 2, offset, components)
	    val (offset, components) =
	       simple (!words, Words.one, offset, components)
	    (* j is the maximum index <= remainingWidth at which an element of a
	     * may be nonempty.
	     *)
	    fun wordComponents (j: int,
				remainingWidth: Bits.t,
				components) =
	       if 0 = j
		  then (remainingWidth, Vector.fromList components)
	       else
		  let
		     val elts = Array.sub (a, j)
		  in
		     case elts of
			[] => wordComponents (j - 1, remainingWidth, components)
		      | {index, rep} :: elts =>
			   let
			      val () = Array.update (a, j, elts)
			      val remainingWidth =
				 Bits.- (remainingWidth, Rep.width rep)
			   in
			      wordComponents
			      (Bits.toInt remainingWidth,
			       remainingWidth,
			       {index = index, rep = rep} :: components)
			   end
		  end
	    (* max is the maximum index at which an element of a may be nonempty.
	     *)
	    fun makeWords (max: int, offset: Words.t, ac) =
	       if 0 = max
		  then (offset, ac)
	       else
		  if List.isEmpty (Array.sub (a, max))
		     then makeWords (max - 1, offset, ac)
		  else
		     let
			val (_, components) =
			   wordComponents (max, Bits.inWord, [])
			val componentTy =
			   Type.seq (Vector.map (components, Rep.ty o #rep))
			fun setSelects (padToPrim: bool): unit =
			   let
			      val wordTy =
				 if padToPrim
				    then Type.padToPrim componentTy
				 else Type.padToWidth (componentTy, Bits.inWord)
			   in
			      ignore
			      (Vector.fold
			       (components, Bits.zero,
				fn ({index, rep}, shift) =>
				let
				   val unpack = Unpack.T {shift = shift,
							  ty = Rep.ty rep}
				   val () =
				      Array.update
				      (selects, index,
				       (Select.Unpack unpack,
					Select.indirectUnpack {offset = offset,
							       rest = unpack,
							       ty = wordTy}))
				in
				   Bits.+ (shift, Rep.width rep)
				end))
			   end
			val component =
			   Component.Word
			   (WordRep.T {components = components,
				       rep = Rep.T {rep = Rep.NonPointer,
						    ty = componentTy}})
			val ac = {component = component,
				  offset = offset,
				  setSelects = setSelects} :: ac
		     in
			makeWords (max, Words.+ (offset, Words.one), ac)
		     end
	    val (offset, components) =
	       makeWords (Bits.toInt Bits.inWord - 1, offset, components)
	    val (_, components) =
	       simple (!pointers, Words.inPointer, offset, components)
	    val components = Vector.fromListRev components
	    val padToPrim = isVector andalso 1 = Vector.length components
	    val () =
	       Vector.foreach
	       (components, fn {setSelects, ...} => setSelects padToPrim)
	    fun getSelects s =
	       Selects.T (Vector.tabulate
			  (Array.length selects, fn i =>
			   {orig = #ty (Vector.sub (rs, i)),
			    select = s (Array.sub (selects, i))}))
	    fun box () =
	       let
		  val components =
		     Vector.map
		     (components, fn {component = c, offset, ...} =>
		      let
			 val c =
			    if padToPrim
			       then Component.padToPrim c
			    else Component.maybePadToWidth (c, Bits.inWord)
		      in
			 {component = c,
			  offset = offset}
		      end)
	       in
		  Indirect (PointerRep.make {components = components,
					     isVector = isVector,
					     selects = getSelects #2,
					     tycon = pointerTycon})
	       end
	 in
	    if forceBox orelse Vector.exists (rs, #isMutable)
	       then box ()
	    else
	       case Vector.length components of
		  0 => unit
		| 1 =>
		     Direct {component = #component (Vector.sub (components, 0)),
			     selects = getSelects #1}
		| _ => box ()
	 end

      val make =
	 Trace.trace3
	 ("TupleRep.make",
	  PointerTycon.layout,
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

structure List =
   struct
      open List

      val splitAt: 'a t * int -> 'a t * 'a t =
	 fn (l, i) =>
	 let
	    fun loop (i, ac, l) =
	       if i = 0
		  then (rev ac, l)
	       else
		  case l of
		     [] => Error.bug "List.splitAt"
		   | x :: l => loop (i - 1, x :: ac, l)
	 in
	    loop (i, [], l)
	 end
   end

fun tagShift (tagBits: Bits.t): Operand.t =
   Operand.word (WordX.fromIntInf (Bits.toIntInf tagBits, WordSize.default))

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
	 fn ShiftAndTag {ty, ...} => Rep.nonPointer ty
	  | Tag {ty, ...} => Rep.nonPointer ty
	  | Tuple tr => TupleRep.rep tr

      val box = Tuple o TupleRep.Indirect

      local
	 fun make i = 
	    let
	       val tag = WordX.fromIntInf (i, WordSize.default)
	    in
	       Tag {tag = tag, ty = Type.constant tag}
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
		  val shift = tagShift (WordSize.bits (WordX.size tag))
		  val tmpVar = Var.newNoname ()
		  val tmpTy =
		     Type.padToWidth (Component.ty component, Type.width dstTy)
		  val tmp = Var {ty = tmpTy, var = tmpVar}
		  val component =
		     Component.tuple (component, {dst = (tmpVar, tmpTy),
						  src = src})

		  val (s1, tmp) = Statement.lshift (tmp, shift)
		  val (s2, tmp) =
		     Statement.orb
		     (tmp,
		      Operand.word
		      (WordX.resize
		       (tag, WordSize.fromBits (Type.width (Operand.ty tmp)))))
		  val s3 = Bind {dst = (dstVar, dstTy),
				 isMutable = false,
				 src = tmp}
	       in
		  component @ [s1, s2, s3]
	       end
	  | Tag {tag, ...} =>
	       let
		  val (dstVar, dstTy) = dst
	       in
		  [Bind {dst = (dstVar, dstTy),
			 isMutable = false,
			 src = (Operand.word
				(WordX.resize
				 (tag, WordSize.fromBits (Type.width dstTy))))}]
	       end
	  | Tuple tr => TupleRep.tuple (tr, {dst = dst, src = src})

      val conApp =
	 Trace.trace ("ConRep.conApp", layout o #1, List.layout Statement.layout)
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

structure Pointers =
   struct
      (* 1 < Vector.length variants *)
      datatype t = T of {headerTy: unit -> Type.t,
			 rep: Rep.t,
			 variants: {con: Con.t,
				    pointer: PointerRep.t} vector}

      fun layout (T {rep, variants, ...}) =
	 let
	    open Layout
	 in
	    record [("rep", Rep.layout rep),
		    ("variants",
		     Vector.layout
		     (fn {con, pointer} =>
		      record [("con", Con.layout con),
			      ("pointer", PointerRep.layout pointer)])
		     variants)]
	 end

      local
	 fun make f (T r) = f r
      in
	 val rep = make #rep
      end

      val ty = Rep.ty o rep

      fun make {rep, variants}: t =
	 let
	    (* headerTy must be delayed since the pointer tycon indices have not
	     * yet been determined.
	     *)
	    val headerTy =
	       Promise.lazy 
	       (fn () =>
		Type.sum (Vector.map
			  (variants, fn {pointer, ...} =>
			   Type.pointerHeader (PointerRep.tycon pointer))))
	 in
	    T {headerTy = headerTy,
	       rep = rep,
	       variants = variants}
	 end

      fun genCase (T {headerTy, variants, ...},
		   {cases: (Con.t * Label.t) vector,
		    conRep: Con.t -> ConRep.t,
		    default: Label.t option,
		    test: Operand.t})
	 : Statement.t list * Transfer.t =
	 let
	    val wordSize = WordSize.pointer ()
	    val cases =
	       Vector.keepAllMap
	       (cases, fn (c, l) =>
		case conRep c of
		   ConRep.Tuple (TupleRep.Indirect
				 (PointerRep.T {ty, tycon, ...})) =>
		      SOME (WordX.fromIntInf (Int.toIntInf
					      (PointerTycon.index tycon),
					      wordSize),
			    Block.new
			    {statements = Vector.new0 (),
			     transfer =
			     Goto {args = Vector.new1 (Cast (test, ty)),
				   dst = l}})
		 | _ => NONE)
	    val default =
	       if Vector.length variants = Vector.length cases
		  then NONE
	       else default
	    val cases =
	       QuickSort.sortVector (cases, fn ((w, _), (w', _)) =>
				     WordX.le (w, w', {signed = false}))
	    val headerTy = headerTy ()
	    val (s, tag) =
	       Statement.rshift (Offset {base = test,
					 offset = Runtime.headerOffset,
					 ty = headerTy},
				 Operand.word (WordX.one wordSize))
	 in
	    ([s], Switch (Switch.T {cases = cases,
				    default = default,
				    size = wordSize,
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
	    tagBits = Bits.fromInt 1,
	    variants = Vector.new2 (Con.falsee, Con.truee)}

      fun genCase (T {isEnum, tagBits, variants, ...},
		   {cases: (Con.t * Label.t) vector,
		    conRep: Con.t -> ConRep.t,
		    isPointer: bool,
		    notSmall: Label.t option,
		    smallDefault: Label.t option,
		    test: Operand.t})
	 : Statement.t list * Transfer.t =
	 let
	    val testBits = Type.width (Operand.ty test)
	    val wordSize = WordSize.fromBits testBits
	    val cases =
	       Vector.keepAllMap
	       (cases, fn (c, l) =>
		case conRep c of
		   ConRep.ShiftAndTag {tag, ty, ...} =>
		      let
			 val test = Cast (test, Type.padToWidth (ty, testBits))
			 val (test, ss) = Statement.resize (test, Type.width ty)
			 val transfer = Goto {args = Vector.new1 test, dst = l}
		      in
			 SOME (WordX.resize (tag, wordSize),
			       Block.new {statements = Vector.fromList ss,
					  transfer = transfer})
		      end
		 | ConRep.Tag {tag, ...} =>
		      SOME (WordX.resize (tag, wordSize), l)
		 | _ => NONE)
	    val cases = QuickSort.sortVector (cases, fn ((w, _), (w', _)) =>
					      WordX.le (w, w', {signed = false}))
	    val (tagOp, ss) =
	       if isEnum
		  then (test, [])
	       else
		  let
		     val (s, tag) =
			Statement.andb
			(test,
			 Operand.word (WordX.resize
				       (WordX.max (WordSize.fromBits tagBits,
						   {signed = false}),
					wordSize)))
		  in
		     (tag, [s])
		  end
	    val tagOp =
	       if isPointer
		  then Cast (tagOp, Type.word (WordSize.bits wordSize))
	       else tagOp
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
			      (Cast (test, Type.word testBits),
			       Operand.word (WordX.fromIntInf (3, wordSize)))
			   val t =
			      Switch
			      (Switch.T
			       {cases = Vector.new1 (WordX.zero wordSize,
						     notSmall),
				default = SOME smallDefault,
				size = wordSize,
				test = test})
			in
			   SOME (Block.new {statements = Vector.new1 s,
					    transfer = t})
			end
	    val transfer =
	       Switch (Switch.T {cases = cases,
				 default = default,
				 size = wordSize,
				 test = tagOp})
	 in
	    (ss, transfer)
	 end

      val genCase =
	 Trace.trace
	 ("Small.genCase",
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
       | Pointers of Pointers.t
       | Small of Small.t
       | SmallAndBox of {box: {con: Con.t,
			       pointer: PointerRep.t},
			 rep: Rep.t,
			 small: Small.t}
       | SmallAndPointer of {pointer: {component: Component.t,
				       con: Con.t},
			     rep: Rep.t,
			     small: Small.t}
       | SmallAndPointers of {pointers: Pointers.t,
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
	     | Pointers ps =>
		  seq [str "Pointers ", Pointers.layout ps]
	     | Small s =>
		  seq [str "Small ", Small.layout s]
	     | SmallAndBox {box = {con, pointer}, rep, small} =>
		  seq [str "SmallAndBox ",
		       record [("box",
				record [("con", Con.layout con),
					("pointer", PointerRep.layout pointer)]),
			       ("rep", Rep.layout rep),
			       ("small", Small.layout small)]]
	     | SmallAndPointer {pointer = {component, con}, rep, small} =>
		  seq [str "SmallAndPointer ",
		       record
		       [("pointer",
			 record [("component", Component.layout component),
				 ("con", Con.layout con)]),
			("rep", Rep.layout rep),
			("small", Small.layout small)]]
	     | SmallAndPointers {pointers, rep, small} =>
		  seq [str "SmallAndPointers ",
		       record [("pointers", Pointers.layout pointers),
			       ("rep", Rep.layout rep),
			       ("small", Small.layout small)]]
	     | Unit => str "Unit"
	 end

      val bool = Small Small.bool

      val unit = Unit

      val rep: t -> Rep.t =
	 fn One {tupleRep, ...} => TupleRep.rep tupleRep
	  | Pointers p => Pointers.rep p
	  | Small s => Small.rep s
	  | SmallAndBox {rep, ...} => rep
	  | SmallAndPointer {rep, ...} => rep
	  | SmallAndPointers {rep, ...} => rep
	  | Unit => Rep.unit

      fun equals (r, r') = Rep.equals (rep r, rep r')

      val wordBits = Bits.toInt Bits.inWord
	 
      local
	 val aWithout = Array.tabulate (wordBits + 1, fn i => IntInf.pow (2, i))
	 (* If there is a pointer, then multiply the number of tags by 3/4 to
	  * remove all the tags that have 00 as their low bits.
	  *)
	 val aWith = Array.tabulate (wordBits + 1, fn i =>
				     (Array.sub (aWithout, i) * 3) div 4)
      in
	 fun numTagsAvailable {tagBits: int, withPointer: bool} =
	    let
	       val a = if withPointer then aWith else aWithout
	    in
	       Array.sub (a, tagBits)
	    end

	 val numTagsAvailable =
	    Trace.trace
	    ("numTagsAvailable",
	     fn {tagBits, withPointer} =>
	     Layout.record [("tagBits", Int.layout tagBits),
			    ("withPointer", Bool.layout withPointer)],
	     IntInf.layout)
	    numTagsAvailable

	 fun tagBitsNeeded {numVariants: int, withPointer: bool}: Bits.t =
	    let
	       val numVariants = Int.toIntInf numVariants
	       val a = if withPointer then aWith else aWithout
	    in
	       case (BinarySearch.smallest
		     (a, fn numTags => numVariants <= numTags)) of
		  NONE => Error.bug "tagBitsNeeded"
		| SOME i => Bits.fromInt i
	    end
	 
	 val tagBitsNeeded =
	    Trace.trace ("tagBitsNeeded",
			 fn {numVariants, withPointer} =>
			 Layout.record [("numVariants", Int.layout numVariants),
					("withPointer", Bool.layout withPointer)],
			 Bits.layout)
	    tagBitsNeeded
      end

      fun make (variants: {args: {isMutable: bool,
				  rep: Rep.t,
				  ty: S.Type.t} vector,
			   con: Con.t,
			   pointerTycon: PointerTycon.t} vector)
	 : t * {con: Con.t, rep: ConRep.t} vector =
	 if 0 = Vector.length variants
	    then (Unit, Vector.new0 ())
	 else if 1 = Vector.length variants
	    then
	       let
		  val {args, con, pointerTycon} = Vector.sub (variants, 0)
		  val tupleRep =
		     TupleRep.make (pointerTycon, args,
				    {forceBox = false,
				     isVector = false})
		  val conRep = ConRep.Tuple tupleRep
	       in
		  (One {con = con, tupleRep = tupleRep},
		   Vector.new1 {con = con, rep = conRep})
	       end
	 else if (2 = Vector.length variants
		  andalso let
			     val c = #con (Vector.sub (variants, 0))
			  in
			     Con.equals (c, Con.falsee)
			     orelse Con.equals (c, Con.truee)
			  end)
	    then (bool, Vector.new2 ({con = Con.falsee, rep = ConRep.falsee},
				     {con = Con.truee, rep = ConRep.truee}))
	 else
	 let
	    val numSmall = ref 0
	    val small = Array.array (wordBits, [])
	    val big = ref []
	    val () =
	       Vector.foreach
	       (variants, fn {args, con, pointerTycon} =>
		let
		   val tr =
		      TupleRep.make (pointerTycon, args,
				     {forceBox = false,
				      isVector = false})
		   fun makeBig () =
		      List.push (big,
				 {con = con,
				  pointerTycon = pointerTycon,
				  tupleRep = tr})
		   val Rep.T {rep, ty} = TupleRep.rep tr
		in
		   case rep of
		      Rep.NonPointer =>
			 let
			    val i = Bits.toInt (Type.width ty)
			 in
			    if i >= wordBits
			       then makeBig ()
		            else
			       let
				  val {component, selects} =
				     case tr of
					TupleRep.Direct z => z
				      | TupleRep.Indirect _ =>
					   Error.bug "small Indirect"
				  val () = Int.inc numSmall
				  val () =
				     Array.update
				     (small, i,
				      {component = component,
				       con = con,
				       pointerTycon = pointerTycon,
				       selects = selects}
				      :: Array.sub (small, i))
			       in
				  ()
			       end
			 end
		    | Rep.Pointer _ => makeBig ()
		end)
	    val big = !big
	    val numSmall = !numSmall
	    fun noLargerThan (i, ac) =
	       if i < 0
		  then ac
	       else noLargerThan (i - 1,
				  List.fold (Array.sub (small, i), ac, op ::))
	    (* Box as few things as possible so that the number of tags available
	     * is >= the number of unboxed variants.
	     *)
	    fun loop (maxSmallWidth: int,
		      forced,
		      withPointer: bool,
		      numSmall: IntInf.t) =
	       if 0 = numSmall
		  then (maxSmallWidth, forced, [])
	       else
		  let
		     val vs = Array.sub (small, maxSmallWidth)
		  in
		     if List.isEmpty vs
			then loop (maxSmallWidth - 1, forced,
				   withPointer, numSmall)
		     else
			let
			   val numTags =
			      numTagsAvailable
			      {tagBits = wordBits - maxSmallWidth,
			       withPointer = withPointer}
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
	       loop (wordBits - 1, [],
		     not (List.isEmpty big),
		     Int.toIntInf numSmall)
	    val maxSmallWidth = Bits.fromInt maxSmallWidth
	    val withPointer = not (List.isEmpty big andalso List.isEmpty forced)
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
					  withPointer = withPointer}
			val r = ref 0w0
			fun getTag (): IntInf.t =
			   let
			      val w = !r
			      val w =
				 if withPointer andalso
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
				  WordX.fromIntInf (getTag (),
						    WordSize.fromBits tagBits)
			       val isUnit = Type.isUnit (Component.ty component)
			       val component =
				  Component.padToWidth (component,
							maxSmallWidth)
			       val selects = Selects.lshift (selects, tagBits)
			       val ty =
				  Type.seq
				  (Vector.new2 (Type.constant tag,
						Component.ty component))
			       val ty =
				  if withPointer
				     then Type.resize (ty, Bits.inPointer)
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
			val rep = Rep.T {rep = Rep.NonPointer, ty = ty}
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
	    fun makeSmallPointer {component, con, pointerTycon, selects} =
	       {con = con,
		pointer = (PointerRep.box
			   (Component.padToWidth (component, Bits.inWord),
			    pointerTycon, selects))}
	    fun makeBigPointer {con, pointerTycon, tupleRep} =
	       let
		  val pr =
		     case tupleRep of
			TupleRep.Direct {component, selects} =>
			   PointerRep.box (component, pointerTycon, selects)
		      | TupleRep.Indirect p => p
	       in
		  {con = con, pointer = pr}
	       end
	    fun sumWithSmall (r: Rep.t): Rep.t =
	       Rep.T {rep = Rep.Pointer {endsIn00 = false},
		      ty = Type.sum (Vector.new2
				     (Rep.ty r,
				      Rep.ty (Small.rep (valOf small))))}
	    fun box () =
	       let
		  val pointers =
		     Vector.concat
		     [Vector.fromListMap (forced, makeSmallPointer),
		      Vector.fromListMap (big, makeBigPointer)]
		  val sumRep =
		     if 1 = Vector.length pointers
			then
			   let
			      val pointer = Vector.sub (pointers, 0)
			      val small = valOf small
			      val rep =
				 sumWithSmall (PointerRep.rep (#pointer pointer))
			   in
			      SmallAndBox {box = pointer,
					   rep = rep,
					   small = small}
			   end
		     else
			let
			   val ty =
			      Type.sum
			      (Vector.map (pointers, PointerRep.ty o #pointer))
			   val ps =
			      Pointers.make
			      {rep = Rep.T {rep = Rep.Pointer {endsIn00 = true},
					    ty = ty},
			       variants = pointers}
			in
			   case small of
			      NONE => Pointers ps
			    | SOME small =>
				 SmallAndPointers
				 {pointers = ps,
				  rep = sumWithSmall (Pointers.rep ps),
				  small = small}
			end
	       in
		  (sumRep,
		   Vector.map (pointers, fn {con, pointer} =>
			       {con = con,
				rep = ConRep.box pointer}))
	       end
	    val (sumRep, pointerReps) =
	       case (forced, big) of
		  ([], []) => (Small (valOf small), Vector.new0 ())
		| ([], [{con, tupleRep, ...}]) =>
		     (* If there is only one big and it is a pointer that
		      * ends in 00, then there is no need to box it.
		      *)
		     (case tupleRep of
			 TupleRep.Direct {component, ...} =>
			    let
			       val rep = TupleRep.rep tupleRep
			    in
			       if Rep.isPointerEndingIn00 rep
				  then
				     let
					val small = valOf small
				     in
					(SmallAndPointer
					 {pointer = {component = component,
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
	    (sumRep, Vector.concat [smallReps, pointerReps])
	 end

      val make =
	 Trace.trace
	 ("TyconRep.make",
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
		   {cases: (Con.t * Label.t) vector,
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
			       val (c, l) = Vector.sub (cases, 0)
			    in
			       if not (Con.equals (c, con))
				  then Error.bug "genCase One"
			       else
				  ([], Goto {args = Vector.new1 (test ()),
					     dst = l})
			    end
		       | (0, SOME l) =>
			    ([], Goto {dst = l, args = Vector.new0 ()})
		       | _ => Error.bug "prim datatype with more than one case")
		| Pointers ps =>
		     Pointers.genCase (ps, {cases = cases,
					    conRep = conRep,
					    default = default,
					    test = test ()})
		| Small s =>
		     Small.genCase (s, {cases = cases,
					conRep = conRep,
					isPointer = false,
					notSmall = NONE,
					smallDefault = default,
					test = test ()})
		| SmallAndBox {box = {con, pointer}, small, ...} =>
		     let
			val notSmall =
			   case Vector.peek (cases, fn (c, _) =>
					     Con.equals (c, con)) of
			      NONE => default
			    | SOME (_, l) =>
				 let
				    val test =
				       Cast (test (), PointerRep.ty pointer)
				 in
				    SOME
				    (Block.new
				     {statements = Vector.new0 (),
				      transfer = Goto {args = Vector.new1 test,
						       dst = l}})
				 end
		     in
			Small.genCase (small, {cases = cases,
					       conRep = conRep,
					       isPointer = true,
					       notSmall = notSmall,
					       smallDefault = default,
					       test = test ()})
		     end
		| SmallAndPointer {pointer = {component, con}, small, ...} =>
		     let
			val notSmall =
			   case Vector.peek (cases, fn (c, _) =>
					     Con.equals (c, con)) of
			      NONE => default
			    | SOME (_, l) =>
				 SOME
				 (Block.new
				  {statements = Vector.new0 (),
				   transfer =
				   Goto {args = (Vector.new1
						 (Cast
						  (test (),
						   Component.ty component))),
					 dst = l}})
		     in
			Small.genCase (small, {cases = cases,
					       conRep = conRep,
					       isPointer = true,
					       notSmall = notSmall,
					       smallDefault = default,
					       test = test ()})
		     end
		| SmallAndPointers {pointers, small, ...} =>
		     let
			val test = test ()
			val (ss, t) =
			   Pointers.genCase
			   (pointers, {cases = cases,
				       conRep = conRep,
				       default = default,
				       test = Cast (test, Pointers.ty pointers)})
			val pointer =
			   Block.new {statements = Vector.fromList ss,
				      transfer = t}
		     in
			Small.genCase (small, {cases = cases,
					       conRep = conRep,
					       isPointer = true,
					       notSmall = SOME pointer,
					       smallDefault = default,
					       test = test})
		     end
		| Unit => Error.bug "TyconRep.genCase Unit"
	 in
	    (statements, transfer, Block.getExtra ())
	 end

      val genCase =
	 Trace.trace
	 ("TyconRep.genCase",
	  fn (r, {cases, default, ...}) =>
	  Layout.tuple [layout r,
			Layout.record
			[("cases",
			  Vector.layout
			  (Layout.tuple2 (Con.layout, Label.layout))
			  cases),
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
	  | (_, Constant _) => Error.bug "cannot affect constant"

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
      fun vectorRep (t: S.Type.t): TupleRep.t = Value.get (tupleRep t)
      fun setVectorRep (t: S.Type.t, tr: TupleRep.t): unit =
	 setTupleRep (t, Value.new {compute = fn () => tr,
				    equals = TupleRep.equals,
				    init = tr})
      val setVectorRep =
	 Trace.trace2 ("setVectorRep",
		       S.Type.layout, TupleRep.layout, Unit.layout)
	 setVectorRep
      val {get = tyconRep: Tycon.t -> tyconRepAndCons, set = setTyconRep, ...} =
	 Property.getSetOnce (Tycon.plist,
			      Property.initRaise ("tyconRep", Tycon.layout))
      (* Initialize the datatypes. *)
      val typeRepRef = ref (fn _ => raise Fail "typeRepRef not set")
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
		  pointerTycon = PointerTycon.new ()})
	     fun compute () =
		let
		   val (tr, cons) =
		      TyconRep.make
		      (Vector.map
		       (cons, fn {args, con, pointerTycon} =>
			{args = Vector.map (Prod.dest args,
					    fn {elt, isMutable} =>
					    {isMutable = isMutable,
					     rep = Value.get (typeRep elt),
					     ty = elt}),
			 con = con,
			 pointerTycon = pointerTycon}))
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
	 : (unit -> (PointerTycon.t * ObjectType.t) option) list ref =
	 ref []
      val {get = typeRep: S.Type.t -> Rep.t Value.t, ...} =
	 Property.get
	 (S.Type.plist,
	  Property.initRec
	  (fn (t, typeRep: S.Type.t -> Rep.t Value.t) =>
	   let
	      val constant = Value.constant
	      val nonPointer = constant o Rep.nonPointer
	      datatype z = datatype S.Type.dest
	   in
	      case S.Type.dest t of
		 Datatype tycon =>
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
		    constant (Rep.T {rep = Rep.Pointer {endsIn00 = false},
				     ty = Type.intInf})
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
			      val pt = PointerTycon.new ()
			      val rs =
				 Vector.map (Prod.dest args, typeRep o #elt)
			      fun compute () =
				 TupleRep.make
				 (pt,
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
			      val () =
				 List.push
				 (delayedObjectTypes, fn () =>
				  case Value.get tr of
				     TupleRep.Indirect pr =>
					SOME (pt, (ObjectType.Normal
						   (PointerRep.componentsTy pr)))
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
			      val args = Prod.dest args
			      fun new () =
				 let
				    val pt = PointerTycon.new ()
				    val () =
				       List.push
				       (delayedObjectTypes, fn () =>
					let
					   val tr =
					      TupleRep.make
					      (pt,
					       Vector.map
					       (args, fn {elt, isMutable} =>
						{isMutable = isMutable,
						 rep = Value.get (typeRep elt),
						 ty = elt}),
					       {forceBox = true,
						isVector = true})
					   val () = setVectorRep (t, tr)
					   val ty =
					      case tr of
						 TupleRep.Direct _ =>
						    TupleRep.ty tr
					       | TupleRep.Indirect pr =>
						    PointerRep.componentsTy pr
					   val ty =
					      if Type.isUnit ty
						 then Type.zero Bits.inByte
					      else ty
					in
					   SOME (pt, ObjectType.Array ty)
					end)
				 in
				    Type.pointer pt
				 end
			      datatype z = datatype S.Type.dest
			      val ty =
				 if 1 = Vector.length args
				    then
				       let
					  val {elt, ...} =
					     Vector.sub (args, 0)
				       in
					  case S.Type.dest elt of
					     Word s =>
						(case (Bits.toInt
						       (WordSize.bits s)) of
						    8 => Type.word8Vector
						  | 32 => Type.wordVector
						  | _ => new ())
					   | _ => new ()
				       end
				 else new ()
			   in
			      constant
			      (Rep.T {rep = Rep.Pointer {endsIn00 = true},
				      ty = ty})
			   end)
	       | Real s => nonPointer (Type.real s)
	       | Thread =>
		    constant (Rep.T {rep = Rep.Pointer {endsIn00 = true},
				     ty = Type.thread})
	       | Weak t =>
		    let
		       val pt = PointerTycon.new ()
		       val rep =
			  Rep.T {rep = Rep.Pointer {endsIn00 = true},
				 ty = Type.pointer pt}
		       val r = typeRep t
		       fun compute () =
			  if Rep.isPointer (Value.get r)
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
			      if Rep.isPointer r
				 then SOME (pt, ObjectType.Weak (Rep.ty r))
			      else NONE
			   end)
		    in
		       r'
		    end
	     | Word s => nonPointer (Type.word (WordSize.bits s))
	   end))
      val () = typeRepRef := typeRep
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
	 Trace.trace ("typeRep", S.Type.layout, Value.layout Rep.layout)
	 typeRep
      val () = S.Program.foreachVar (program, fn (_, t) => ignore (typeRep t))
      val () = Value.fixedPoint ()
      val conRep = ! o #rep o conInfo
      val tyconRep = #1 o Value.get o tyconRep
      val objectTypes =
	 Vector.fold
	 (datatypes, [], fn ({cons, ...}, ac) =>
	  Vector.fold
	  (cons, ac, fn ({con, pointerTycon, ...}, ac) =>
	   case conRep con of
	      ConRep.Tuple (TupleRep.Indirect pr) =>
		 (pointerTycon,
		  ObjectType.Normal (PointerRep.componentsTy pr)) :: ac
	    | _ => ac))
      val objectTypes = ref objectTypes
      val () =
	 List.foreach
	 ([(WordSize.fromBits Bits.inByte, PointerTycon.word8Vector),
	   (WordSize.default, PointerTycon.wordVector)],
	  fn (size, pt) =>
	  let
	     val elt = S.Type.word size
	  in
	     List.foreach
	     ([true, false], fn isMutable =>
	      setVectorRep (S.Type.vector (Prod.make (Vector.new1
						      {elt = elt,
						       isMutable = isMutable})),
			    TupleRep.make (pt,
					   Vector.new1
					   {isMutable = isMutable,
					    rep = Value.get (typeRep elt),
					    ty = elt},
					   {forceBox = true,
					    isVector = true})))
	  end)
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
	 Trace.trace ("tupleRep", S.Type.layout, TupleRep.layout) tupleRep
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
		    | _ => Error.bug "can't get con selects")
	     | Tuple => (TupleRep.selects (tupleRep objectTy), NONE)
	     | Vector =>
		  case vectorRep objectTy of
		     tr as TupleRep.Indirect pr =>
			(TupleRep.selects tr,
			 SOME (Type.bytes (PointerRep.componentsTy pr)))
		   | _ => Error.bug "Vector not Indirect"
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
	  | _ => Error.bug "select of non object"
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
	  | _ => Error.bug "update of non object"
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
