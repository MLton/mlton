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
structure R = Rssa
local
   open Rssa
in
   structure Block = Block
   structure CType = CType
   structure IntSize = IntSize
   structure IntX = IntX
   structure Kind = Kind
   structure Label = Label
   structure ObjectType = ObjectType
   structure Operand = Operand
   structure PointerTycon = PointerTycon
   structure Prim = Prim
   structure Runtime = Runtime
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
   structure Con = Con
   structure Tycon = Tycon
end

datatype z = datatype Operand.t
datatype z = datatype Statement.t
datatype z = datatype Transfer.t

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

      fun nonPointer ty = T {rep = NonPointer,
			     ty = ty}
	 
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
		     ty = (Type.seq
			   (Vector.new2
			    (ty,
			     Type.constant (WordX.zero
					    (WordSize.fromBits
					     (Bits.- (width,
						      Type.width ty)))))))}
	     | Pointer _ => Error.bug "Rep.padToWidth"
   end

structure Type =
   struct
      open Type

      fun padToPrim (t: t): t option =
	 let
	    val b = Bits.toInt (width t)
	    fun check (b', continue) =
	       if b < b'
		  then
		     SOME (seq
			   (Vector.new2
			    (t, constant (WordX.zero (WordSize.fromBits
						      (Bits.fromInt (8 - b)))))))
	       else if b = b'
		       then SOME t
		    else continue ()
	 in
	    if 0 = b
	       then NONE
	    else
	       check (8, fn () =>
		      check (16, fn () =>
			     check (32, fn () =>
				    if b = 64
				       then SOME t
				    else Error.bug (concat ["Type.padToPrim ",
							    Int.toString b]))))
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
		 src: {index: int} -> Operand.t)
	 : Operand.t * Statement.t list =
	 let
	    val (res, statements) =
	       Vector.foldr
	       (components, (Operand.bool false (* junk *), []),
		fn ({index, rep, ...}, (ac: Operand.t, statements)) =>
		let
		   val src = src {index = index}
		   val srcTy = Operand.ty src
		in
		   case statements of
		      [] =>
			 let
			    val tmp = Var {ty = srcTy, var = Var.newNoname ()}
			 in
			    (tmp, [Move {dst = tmp, src = src}])
			 end
		    | _ =>
			 let
			    val width = Rep.width rep
			    val shiftBits =
			       WordX.fromIntInf (Bits.toIntInf width,
						 WordSize.default)
			    val tmp = Var.newNoname ()
			    val tmpTy =
			       Type.lshift (Operand.ty ac,
					    Type.constant shiftBits)
			    val statements =
			       PrimApp
			       {args = (Vector.new2
					(ac, Operand.word shiftBits)),
				dst = SOME (tmp, tmpTy),
				prim = Prim.wordLshift WordSize.default}
			       :: statements
			    val tmp' = Var.newNoname ()
			    val tmp'Ty = valOf (Type.orb (tmpTy, srcTy))
			    val statements =
			       PrimApp
			       {args = Vector.new2 (Var {ty = tmpTy, var = tmp},
						    src),
				dst = SOME (tmp', tmp'Ty),
				prim = Prim.wordOrb WordSize.default}
			       :: statements
			 in
			    (Var {ty = tmp'Ty, var = tmp'},
			     statements)
			 end
		end)
	 in
	    (res, List.rev statements)
	 end

      val tuple =
	 Trace.trace
	 ("WordRep.tuple",
	  layout o #1,
	  Layout.tuple2 (Operand.layout, List.layout Statement.layout))
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

      fun padToWidth (c: t, b: Bits.t): t =
	 case c of
	    Direct {index, rep} =>
	       Direct {index = index,
		       rep = Rep.padToWidth (rep, b)}
	  | Word r => Word (WordRep.padToWidth (r, b))

      fun tuple (c: t, {dst: Operand.t,
			src: {index: int} -> Operand.t})
	 : Statement.t list =
	 let
	    fun move (src: Operand.t) = [Move {dst = dst, src = src}]
	 in
	    case c of
	       Direct {index, ...} => move (src {index = index})
	     | Word wr =>
		  let
		     val (res, statements) = WordRep.tuple (wr, src)
		  in
		     statements @ move res
		  end
	 end

      val tuple =
	 Trace.trace2 ("Component.tuple",
		       layout, Operand.layout o #dst,
		       List.layout Statement.layout)
	 tuple
   end

structure Unpack =
   struct
      datatype t = T of {mask: Operand.t,
			 shift: Bits.t,
			 ty: Type.t}

      fun layout (T {mask, shift, ty}) =
	 Layout.record [("mask", Operand.layout mask),
			("shift", Bits.layout shift),
			("ty", Type.layout ty)]
	 
      local
	 fun make f (T r) = f r
      in
	 val ty = make #ty
      end

      fun select (T {mask, shift, ty},
		  {dst: Var.t,
		   src: Operand.t}): Statement.t list =
	 let
	    val shift = WordX.fromIntInf (Bits.toIntInf shift, WordSize.default)
	    val tmpVar = Var.newNoname ()
	    val tmpTy = Type.rshift (Operand.ty src, Type.constant shift)
	 in
	    [PrimApp {args = Vector.new2 (src, Operand.word shift),
		      dst = SOME (tmpVar, tmpTy),
		      prim = Prim.wordRshift WordSize.default},
	     PrimApp {args = Vector.new2 (Var {ty = tmpTy, var = tmpVar}, mask),
		      dst = SOME (dst,
				  valOf (Type.andb (tmpTy, Operand.ty mask))),
		      prim = Prim.wordAndb WordSize.default}]
	 end

      val select =
	 Trace.trace2 ("Unpack.select", layout,
		       fn {dst, src} =>
		       Layout.record [("dst", Var.layout dst),
				      ("src", Operand.layout src)],
		       List.layout Statement.layout)
	 select
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
		  seq [str "IndirectOffset ",
		       record [("offset", Bytes.layout offset),
			       ("rest", Unpack.layout rest),
			       ("ty", Type.layout ty)]]
	     | Unpack u => seq [str "Unpack ", Unpack.layout u]
	 end

      val ty =
	 fn None => Error.bug "Select.ty"
	  | Direct {ty, ...} => ty
	  | Indirect {ty, ...} => ty
	  | IndirectUnpack {rest, ...} => Unpack.ty rest
	  | Unpack u => Unpack.ty u
 
      fun select (s: t, {dst: unit -> Var.t,
			 tuple: unit -> Operand.t}): Statement.t list =
	 let
	    fun dstOp ty = Var {ty = ty, var = dst ()}
	 in
	    case s of
	       None => []
	     | Direct {ty} => [Move {dst = dstOp ty, src = tuple ()}]
	     | Indirect {offset, ty} =>
		  let
		     val dst = dstOp ty
		  in
		     [Move {dst = dst,
			    src = Operand.Offset {base = tuple (),
						  offset = offset,
						  ty = ty}}]
		  end
	     | IndirectUnpack {offset, rest, ty} =>
		  let
		     val tmp = Var {ty = ty, var = Var.newNoname ()}
		  in
		     Move {dst = tmp,
			   src = Operand.Offset {base = tuple (),
						 offset = offset,
						 ty = ty}}
		     :: Unpack.select (rest, {dst = dst (), src = tmp})
		  end
	     | Unpack u => Unpack.select (u, {dst = dst (), src = tuple ()})
	 end

      val select =
	 Trace.trace ("Select.select", layout o #1, List.layout Statement.layout)
	 select
   end

structure Selects =
   struct
      datatype t = T of Select.t vector

      fun layout (T v) = Vector.layout Select.layout v

      val empty = T (Vector.new0 ())

      fun select (T v, {dst: unit -> Var.t,
			offset: int,
			tuple: unit -> Operand.t}): Statement.t list =
	 Select.select (Vector.sub (v, offset),
			{dst = dst, tuple = tuple})

      fun goto (T v,
		l: Label.t,
		tuple: unit -> Operand.t): Statement.t list * Transfer.t =
	 let
	    val args = ref []
	    val statements =
	       Vector.foldr
	       (v, [], fn (s, statements) =>
		let
		   fun dst () =
		      let
			 val x = Var.newNoname ()
			 val () =
			    List.push (args, Var {ty = Select.ty s, var = x})
		      in
			 x
		      end
		in
		   Select.select (s, {dst = dst, tuple = tuple}) @ statements
		end)
	 in
	    (statements,
	     Goto {args = Vector.fromList (!args), dst = l})
	 end
   end

structure PointerRep =
   struct
      datatype t = T of {components: {component: Component.t,
				      offset: Bytes.t} vector,
			 componentsTy: Type.t,
			 selects: Selects.t,
			 size: Bytes.t,
			 ty: Type.t,
			 tycon: PointerTycon.t}

      fun layout (T {components, componentsTy, selects, size, ty, tycon}) =
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
	     ("size", Bytes.layout size),
	     ("ty", Type.layout ty),
	     ("tycon", PointerTycon.layout tycon)]
	 end

      local
	 fun make f (T r) = f r
      in
	 val componentsTy = make #componentsTy
	 val selects = make #selects
	 val ty = make #ty
	 val tycon = make #tycon
      end

      fun equals (T {tycon = c, ...}, T {tycon = c', ...}) =
	 PointerTycon.equals (c, c')

      fun rep (T {ty, ...}) =
	 Rep.T {rep = Rep.Pointer {endsIn00 = true},
		ty = ty}

      fun box (component: Component.t, pt: PointerTycon.t, selects: Selects.t) =
	 T {components = Vector.new1 {component = component,
				      offset = Bytes.zero},
	    componentsTy = Component.ty component,
	    selects = selects,
	    size = Type.bytes (Component.ty component),
	    ty = Type.pointer pt,
	    tycon = pt}

      fun tuple (T {components, size, ty, tycon, ...},
		 {dst: Var.t,
		  src: {index: int} -> Operand.t}) =
	 let
	    val object = Var {ty = ty, var = dst}
	    val stores =
	       Vector.foldr
	       (components, [], fn ({component, offset}, ac) =>
		Component.tuple
		(component,
		 {dst = Operand.Offset {base = object,
					offset = offset,
					ty = Component.ty component},
		  src = src})
		@ ac)
	 in
	    Object {dst = (dst, ty),
		    header = (Runtime.typeIndexToHeader
			      (PointerTycon.index tycon)),
		    size = size,
		    stores = Vector.new0 ()}
	    :: stores
	 end

      val tuple =
	 Trace.trace2 ("PointerRep.tuple",
		       layout, Var.layout o #dst,
		       List.layout Statement.layout)
	 tuple
   end

structure TupleRep =
   struct
      datatype t =
	 Direct of {component: Component.t,
		    selects: Selects.t}
       | Indirect of PointerRep.t
       | Unit

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
	     | Unit => str "Unit"
	 end

      val unit = Unit
	 
      val equals: t * t -> bool =
	 fn z =>
	 case z of
	    (Direct {component = c, ...}, Direct {component = c', ...}) =>
	       Component.equals (c, c')
	  | (Indirect pr, Indirect pr') => PointerRep.equals (pr, pr')
	  | (Unit, Unit) => true
	  | _ => false

      fun rep (tr: t): Rep.t =
	 case tr of
	    Direct {component, ...} => Component.rep component
	  | Indirect p => PointerRep.rep p
	  | Unit => Rep.unit

      fun selects (tr: t): Selects.t =
	 case tr of
	    Direct {selects, ...} => selects
	  | Indirect (PointerRep.T {selects, ...}) => selects
	  | Unit => Selects.empty

      fun select (tr: t, z) =
	 Selects.select (selects tr, z)

      fun tuple (tr: t,
		 {dst: Var.t,
		  src: {index: int} -> Operand.t}): Statement.t list =
	 case tr of
	    Direct {component, ...} =>
	       Component.tuple
	       (component,
		{dst = Var {ty = Component.ty component, var = dst},
		 src = src})
	  | Indirect pr =>
		PointerRep.tuple (pr, {dst = dst, src = src})
	  | Unit => []
 
      val tuple =
	 Trace.trace2 ("TupleRep.tuple",
		       layout, Var.layout o #dst,
		       List.layout Statement.layout)
	 tuple

      val make: Rep.t vector * PointerTycon.t -> t =
	 fn (rs, pointerTycon) =>
	 let
	    val pointers = ref []
	    val doubleWords = ref []
	    val words = ref []
	    val a = Array.array (Bits.toInt Bits.inWord, [])
	    val () =
	       Vector.foreachi
	       (rs, fn (i, r as Rep.T {rep, ty}) =>
		case rep of
		   Rep.NonPointer =>
		      let
			 val b = Bits.toInt (Type.width ty)
			 fun direct l =
			    List.push
			    (l, {component = Component.Direct {index = i,
							       rep = r},
				 index = i})
		      in
			 if b = 64
			    then direct doubleWords
			 else if b = 32
				 then direct words
			      else
				 Array.update
				 (a, b, {index = i, rep = r} :: Array.sub (a, b))
		      end
		 | Rep.Pointer _ =>
		      List.push
		      (pointers,
		       {component = Component.Direct {index = i,
						      rep = r},
			index = i}))
	    val selects = Array.array (Vector.length rs,
				       (Select.None, Select.None))
	    fun simple (l, width, offset, components) =
	       List.fold
	       (l, (offset, components),
		fn ({component, index}, (offset, ac)) =>
		(Bytes.+ (offset, width),
		 let
		    val ty = Component.ty component
		    val () =
		       Array.update
		       (selects, index,
			(Select.Direct {ty = ty},
			 Select.Indirect {offset = offset,
					  ty = ty}))
		 in
		    {component = component, offset = offset} :: ac
		 end))
	    val offset = Bytes.zero
	    val components = []
	    (* Start with all the doubleWords followed by all the words. *)
	    val (offset, components) =
	       simple (!doubleWords, Bytes.scale (Bytes.inWord, 2),
		       offset, components)
	    val (offset, components) =
	       simple (!words, Bytes.inWord, offset, components)
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
	    fun makeWords (max: int, offset: Bytes.t, ac) =
	       if 0 = max
		  then ac
	       else
		  if List.isEmpty (Array.sub (a, max))
		     then makeWords (max - 1, offset, ac)
		  else
		     let
			val (remainingWidth, components) =
			   wordComponents (max, Bits.inWord, [])
			val componentsTy =
			   Type.seq (Vector.map (components, Rep.ty o #rep))
			val wordTy =
			   Type.seq
			   (Vector.new2
			    (componentsTy,
			     Type.constant
			     (WordX.zero (WordSize.fromBits remainingWidth))))
			val _ =
			   Vector.fold
			   (components, Bits.zero, fn ({index, rep}, shift) =>
			    let
			       val width = Rep.width rep
			       val mask =
				  Operand.word
				  (WordX.fromIntInf
				   (IntInf.<< (2, Bits.toWord width) - 1,
				    WordSize.default))
			       val unpack =
				  Unpack.T {mask = mask,
					    shift = shift,
					    ty = Rep.ty rep}
			       val () =
				  Array.update
				  (selects, index,
				   (Select.Unpack unpack,
				    Select.IndirectUnpack {offset = offset,
							   rest = unpack,
							   ty = wordTy}))
			    in
			       Bits.+ (shift, width)
			    end)
			val component =
			   Component.Word
			   (WordRep.T {components = components,
				       rep = Rep.T {rep = Rep.NonPointer,
						    ty = componentsTy}})
			val ac = {component = component, offset = offset} :: ac
		     in
			makeWords (max, Bytes.+ (offset, Bytes.inWord), ac)
		     end
	    val components =
	       makeWords (Bits.toInt Bits.inWord - 1, offset, components)
	    (* Add the pointers at the end. *)
	    val (offset, components) =
	       simple (!pointers, Bytes.inPointer, offset, components)
	    val components = Vector.fromListRev components
	    fun getSelects s =
	       Selects.T (Vector.tabulate (Array.length selects, fn i =>
					   s (Array.sub (selects, i))))
	 in
	    case Vector.length components of
	       0 => Unit
	     | 1 => Direct {component = #component (Vector.sub (components, 0)),
			    selects = getSelects #1}
	     | _ => 
		  let
		     val ty = Type.pointer pointerTycon
		     val componentsTy =
			Type.seq (Vector.map (components,
					      Component.ty o #component))
		  in
		     Indirect (PointerRep.T {components = components,
					     componentsTy = componentsTy,
					     selects = getSelects #2,
					     size = offset,
					     ty = ty,
					     tycon = pointerTycon})
		  end
	 end

      val make =
	 Trace.trace ("TupleRep.make",
		      (Vector.layout Rep.layout) o #1,
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

structure ConRep =
   struct
      datatype t =
	 Box of PointerRep.t
       | ShiftAndTag of {component: Component.t,
			 selects: Selects.t,
			 shift: Operand.t,
			 tag: WordX.t}
       | Transparent

      val layout =
	 let
	    open Layout
	 in
	    fn Box pr => seq [str "Box ", PointerRep.layout pr]
	     | ShiftAndTag {component, selects, shift, tag} =>
		  seq [str "ShiftAndTag ",
		       record [("component", Component.layout component),
			       ("selects", Selects.layout selects),
			       ("shift", Operand.layout shift),
			       ("tag", seq [str "0x", WordX.layout tag])]]
	     | Transparent => str "Transparent"
	 end

      fun conApp (r: t, {src: {index: int} -> Operand.t,
			 dst: unit -> Var.t * Type.t}): Statement.t list =
	 case r of
	    Box pr =>
	       PointerRep.tuple (pr, {dst = #1 (dst ()),
				      src = src})
	  | ShiftAndTag {component, shift, tag, ...} =>
	       let
		  val tmp = Var.newNoname ()
		  val tmpTy = Component.ty component
		  val tmpOp = Var {ty = tmpTy, var = tmp}
		  val tmpShift = Var.newNoname ()
		  val tmpShiftTy = Type.lshift (tmpTy, Operand.ty shift)
	       in
		  Component.tuple (component, {dst = tmpOp, src = src})
		  @ [PrimApp {args = Vector.new2 (tmpOp, shift),
			      dst = SOME (tmpShift, tmpShiftTy),
			      prim = Prim.wordLshift WordSize.default},
		     PrimApp {args = Vector.new2 (Var {ty = tmpShiftTy,
						       var = tmpShift},
						  Operand.word tag),
			      dst = SOME (dst ()),
			      prim = Prim.wordOrb WordSize.default}]
	       end
	  | Transparent =>
	       let
		  val (dst, dstTy) = dst ()
	       in
		  [Move {dst = Var {ty = dstTy, var = dst},
			 src = src {index = 0}}]
	       end

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
		   conRep: Con.t -> ConRep.t,
		   {cases: (Con.t * Label.t) vector,
		    default: Label.t option,
		    test: Operand.t}): Statement.t list * Transfer.t =
	 let
	    val cases =
	       Vector.keepAllMap
	       (cases, fn (c, l) =>
		case conRep c of
		   ConRep.Box (PointerRep.T {selects, tycon, ...}) =>
		      let
			 val tag = PointerTycon.index tycon
			 val pointerVar = Var.newNoname ()
			 val pointerTy = Type.pointer tycon
			 val pointerOp = Var {ty = pointerTy, var = pointerVar}
			 val (ss, transfer) =
			    Selects.goto (selects, l, fn () => pointerOp)
			 val ss =
			    Vector.fromList
			    (Bind {isMutable = false,
				   oper = Cast (test, pointerTy),
				   var = pointerVar}
			     :: ss)
			 val dst =
			    Block.new {statements = ss,
				       transfer = transfer}
		      in
			 SOME (WordX.fromIntInf (Int.toIntInf tag,
						 WordSize.default),
			       dst)
		      end
		 | _ => NONE)
	    val default =
	       if Vector.length variants = Vector.length cases
		  then NONE
	       else default
	    val cases =
	       QuickSort.sortVector 
	       (cases, fn ((w, _), (w', _)) => WordX.<= (w, w'))
	    val headerTy = headerTy ()
	    val tagVar = Var.newNoname ()
	    val tagTy = Type.rshift (headerTy,
				     Type.constant (WordX.one WordSize.default))
	 in
	    ([PrimApp {args = (Vector.new2
			       (Offset {base = test,
					offset = Runtime.headerOffset,
					ty = headerTy},
				Operand.word (WordX.one WordSize.default))),
		       dst = SOME (tagVar, tagTy),
		       prim = Prim.wordRshift WordSize.default}],
	     Switch (Switch.T {cases = cases,
			       default = default,
			       size = WordSize.default,
			       test = Var {ty = tagTy, var = tagVar}}))
	 end
   end

structure Small =
   struct
      datatype t = T of {mask: Operand.t,
			 rep: Rep.t,
			 shift: Operand.t,
			 tagTy: Type.t,
			 variants: {component: Component.t,
				    con: Con.t,
				    selects: Selects.t,
				    tag: WordX.t} vector}

      fun layout (T {mask, rep, shift, tagTy, variants}) =
	 let
	    open Layout
	 in
	    record [("mask", Operand.layout mask),
		    ("rep", Rep.layout rep),
		    ("shift", Operand.layout shift),
		    ("tagTy", Type.layout tagTy),
		    ("variants",
		     Vector.layout
		     (fn {component, con, selects, tag} =>
		      record [("component", Component.layout component),
			      ("con", Con.layout con),
			      ("selects", Selects.layout selects),
			      ("tag", seq [str "0x", WordX.layout tag])])
		     variants)]
	 end

      local
	 fun make f (T r) = f r
      in
	 val rep = make #rep
      end

      fun genCase (T {mask, shift, tagTy, variants, ...},
		   conRep,
		   {cases: (Con.t * Label.t) vector,
		    notSmall: Label.t option,
		    smallDefault: Label.t option,
		    test: Operand.t}): Statement.t list * Transfer.t =
	 let
	    val cases =
	       Vector.keepAllMap
	       (cases, fn (c, l) =>
		case conRep c of
		   ConRep.ShiftAndTag {component, selects, tag, ...} =>
		      let
			 val data = ref NONE
			 fun dataOp () =
			    let
			       val dataVar = Var.newNoname ()
			       val dataTy = Component.ty component
			       val () =
				  data :=
				  SOME
				  (PrimApp
				   {args = Vector.new2 (test, shift),
				    dst = SOME (dataVar, dataTy),
				    prim = Prim.wordRshift WordSize.default})
			    in
			       Var {ty = dataTy, var = dataVar}
			    end
			 val (ss, transfer) = Selects.goto (selects, l, dataOp)
			 val dst =
			    Block.new {statements = Vector.fromList ss,
				       transfer = transfer}
		      in
			 SOME (tag, dst)
		      end
		 | _ => NONE)
	    val tagVar = Var.newNoname ()
	    val ss =
	       [PrimApp {args = Vector.new2 (test, mask),
			 dst = SOME (tagVar, tagTy),
			 prim = Prim.wordAndb WordSize.default}]
	    val default =
	       if Vector.length variants = Vector.length cases
		  then notSmall
	       else
		  case (notSmall, smallDefault) of
		     (NONE, _) => smallDefault
		   | (_, NONE) => notSmall
		   | (SOME notSmall, SOME smallDefault) =>
			let
			   val tmp = Var.newNoname ()
			   val tmpTy = Type.defaultWord
			   val ss =
			      Vector.new1
			      (PrimApp
			       {args = (Vector.new2
					(Operand.word
					 (WordX.fromIntInf
					  (3, WordSize.default)),
					 Cast (test, Type.defaultWord))),
				dst = SOME (tmp, tmpTy),
				prim = Prim.wordAndb WordSize.default})
			   val t =
			      Switch
			      (Switch.T
			       {cases = (Vector.new1
					 (WordX.zero WordSize.default,
					  notSmall)),
				default = SOME smallDefault,
				size = WordSize.default,
				test = Var {ty = tmpTy, var = tmp}})
			in
			   SOME (Block.new {statements = ss,
					    transfer = t})
			end
	    val transfer =
	       Switch (Switch.T {cases = cases,
				 default = default,
				 size = WordSize.default,
				 test = Var {ty = tagTy, var = tagVar}})
	 in
	    (ss, transfer)
	 end
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
	     | SmallAndBox {box as {con, pointer}, rep, small} =>
		  seq [str "SmallAndBox ",
		       record [("box",
				record [("con", Con.layout con),
					("pointer", PointerRep.layout pointer)]),
			       ("rep", Rep.layout rep),
			       ("small", Small.layout small)]]
	     | SmallAndPointer {pointer as {component, con}, rep, small} =>
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
	 val aWithout = Array.tabulate (wordBits, fn i => IntInf.pow (2, i))
	 (* If there is a pointer, then multiply the number of tags by 3/4 to
	  * remove all the tags that have 00 as their low bits.
	  *)
	 val aWith = Array.tabulate (wordBits, fn i =>
				     (Array.sub (aWithout, i) * 3) div 4)
      in
	 fun numTagsAvailable {tagBits: int, withPointer: bool} =
	    let
	       val a = if withPointer then aWith else aWithout
	    in
	       Array.sub (a, tagBits)
	    end
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
      end

      fun make (variants: {args: Rep.t vector,
			   con: Con.t,
			   pointerTycon: PointerTycon.t} vector)
	 : t * {con: Con.t, rep: ConRep.t} vector =
	 if 1 = Vector.length variants
	    then
	       let
		  val {args, con, pointerTycon} = Vector.sub (variants, 0)
		  val tupleRep = TupleRep.make (args, pointerTycon)
	       in
		  (One {con = con,
			tupleRep = tupleRep},
		   Vector.new1 {con = con,
				rep = ConRep.Transparent})
	       end
	 else
	 let
	    val numSmall = ref 0
	    val small = Array.array (wordBits, [])
	    val numBig = ref 0
	    val big = ref []
	    val () =
	       Vector.foreach
	       (variants, fn {args, con, pointerTycon} =>
		let
		   val tr = TupleRep.make (args, pointerTycon)
		   fun makeBig () =
		      (Int.inc numBig
		       ; List.push (big,
				    {con = con,
				     pointerTycon = pointerTycon,
				     tupleRep = tr}))
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
				      | TupleRep.Unit =>
					   {component = Component.unit,
					    selects = Selects.empty}
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
	    val numBig = !numBig
	    val big = !big
	    val numSmall = !numSmall
	    val numVariants = Int.toIntInf (numBig + numSmall)
	    fun noLargerThan (i, ac) =
	       if i < 0
		  then ac
	       else noLargerThan (i - 1,
				  List.fold (Array.sub (small, i), ac, op ::))
	    (* Box as few things as possible so that the number of
	     * tags available is >= the number of unboxed variants.
	     *)
	    fun loop (maxSmallWidth: int,
		      forced,
		      withPointer: bool,
		      numSmall: IntInf.t) =
	       if 0 = numSmall orelse maxSmallWidth <= 0
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
	    val withPointer = not (List.isEmpty big)
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
			val shift =
			   Operand.word
			   (WordX.fromIntInf (Bits.toIntInf tagBits,
					      WordSize.default))
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
			val mask =
			   WordX.fromIntInf
			   (IntInf.pow (2, Bits.toInt tagBits) - 1,
			    WordSize.default)
			val small =
			   Vector.fromListMap
			   (small, fn {component, con, selects, ...} =>
			    let
			       val tag = getTag ()
			       val tagWord =
				  WordX.fromIntInf (tag, WordSize.default)
			       val component =
				  Component.padToWidth (component,
							maxSmallWidth)
			       val ty =
				  Type.seq
				  (Vector.new2
				   (Type.constant
				    (WordX.fromIntInf
				     (tag, WordSize.fromBits tagBits)),
				    Component.ty component))
			    in
			       {component = component,
				con = con,
				selects = selects,
				tag = tagWord,
				ty = ty}
			    end)
			val rep =
			   Rep.T {rep = Rep.NonPointer,
				  ty = Type.sum (Vector.map (small, #ty))}
			val tagTy =
			   valOf (Type.orb (Rep.ty rep, Type.constant mask))
			val variants =
			   Vector.map
			   (small,
			    fn {component, con, selects, tag, ...} =>
			    {component = component,
			     con = con,
			     selects = selects,
			     tag = tag})
			val reps =
			   Vector.map
			   (variants, fn {component, con, selects, tag, ...} =>
			    {con = con,
			     rep = (ConRep.ShiftAndTag
				    {component = component,
				     selects = selects,
				     shift = shift,
				     tag = tag})})
		     in
			(SOME (Small.T {mask = Operand.word mask,
					rep = rep,
					shift = shift,
					tagTy = tagTy,
					variants = variants}),
			 reps)
		     end
	       end
	    fun makeSmallPointer {component, con, pointerTycon, selects} =
	       let
		  val component =
		     Component.padToWidth (component, Bits.inWord)
	       in
		  {con = con,
		   pointer = PointerRep.box (component, pointerTycon, selects)}
	       end
	    fun makeBigPointer {con, pointerTycon, tupleRep} =
	       let
		  val pr =
		     case tupleRep of
			TupleRep.Direct {component, selects} =>
			   PointerRep.box (component, pointerTycon, selects)
		      | TupleRep.Indirect p => p
		      | TupleRep.Unit =>
			   PointerRep.box (Component.unit, pointerTycon,
					   Selects.empty)
	       in
		  {con = con, pointer = pr}
	       end
	    fun sum (r1, r2) =
	       Rep.T {rep = Rep.Pointer {endsIn00 = false},
		      ty = Type.sum (Vector.new2 (Rep.ty r1, Rep.ty r2))}
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
			      val rep = sum (PointerRep.rep (#pointer pointer),
					     Small.rep small)
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
				 SmallAndPointers {pointers = ps,
						   rep = sum (Pointers.rep ps,
							      Small.rep small),
						   small = small}
			end
	       in
		  (sumRep,
		   Vector.map (pointers, fn {con, pointer} =>
			       {con = con,
				rep = ConRep.Box pointer}))
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
					  rep = sum (Small.rep small, rep),
					  small = small},
					 Vector.new1 {con = con,
						      rep = ConRep.Transparent})
				     end
			       else box ()
			    end
		       | _ => box ())
		| _ => box ()
	 in
	    (sumRep, Vector.concat [smallReps, pointerReps])
	 end

      val make =
	 Trace.trace ("TyconRep.make",
		      Vector.layout
		      (fn {args, con, ...} =>
		       Layout.record [("args", Vector.layout Rep.layout args),
				      ("con", Con.layout con)]),
		      layout o #1)
	 make

      fun genCase (r: t,
		   conRep: Con.t -> ConRep.t,
		   {cases: (Con.t * Label.t) vector,
		    default: Label.t option,
		    test: unit -> Operand.t}): (Statement.t list
						* Transfer.t
						* Block.t list) =
	 let
	    val (statements, transfer) =
	       case r of
		  One {con, tupleRep} =>
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
			       else Selects.goto (TupleRep.selects tupleRep,
						  l, test)
			    end
		       | (0, SOME l) =>
			    ([], Goto {dst = l, args = Vector.new0 ()})
		       | _ => Error.bug "prim datatype with more than one case")
		| Pointers ps =>
		     Pointers.genCase (ps, conRep,
				       {cases = cases,
					default = default,
					test = test ()})
		| Small s =>
		     Small.genCase (s, conRep,
				    {cases = cases,
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
				    val (ss, t) =
				       Selects.goto
				       (PointerRep.selects pointer, l, test)
				 in
				    SOME (Block.new
					  {statements = Vector.fromList ss,
					   transfer = t})
				 end
		     in
			Small.genCase (small, conRep,
				       {cases = cases,
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
				   transfer = Goto {args = (Vector.new1
							    (test ())),
						    dst = l}})
		     in
			Small.genCase (small, conRep,
				       {cases = cases,
					notSmall = notSmall,
					smallDefault = default,
					test = test ()})
		     end
		| SmallAndPointers {pointers, small, ...} =>
		     let
			val test = test ()
			val (ss, t) =
			   Pointers.genCase
			   (pointers, conRep, {cases = cases,
					       default = default,
					       test = test})
			val pointer =
			   Block.new {statements = Vector.fromList ss,
				      transfer = t}
		     in
			Small.genCase (small, conRep,
				       {cases = cases,
					notSmall = SOME pointer,
					smallDefault = default,
					test = test})
		     end
		| Unit => Error.bug "TyconRep.genCase Unit"
	 in
	    (statements, transfer, Block.getExtra ())
	 end
   end

structure Value:
   sig
      type 'a t

      val affect: 'a t * 'b t -> unit
      val constant: 'a -> 'a t
      val fixedPoint: unit -> unit
      val get: 'a t -> 'a
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
	       List.foreach
	       (!todo, fn T {affects, compute, needToCompute, ...} =>
		let
		   val () = needToCompute := false
		   val {change} = compute ()
		in
		   if change
		      then List.foreach (!affects, recompute)
		   else ()
		end)
  
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
      val objectTypes = ref []
      val {get = refRep: S.Type.t -> TupleRep.t Value.t,
	   set = setRefRep, ...} =
	 Property.getSetOnce (S.Type.plist,
			      Property.initRaise ("refRep", S.Type.layout))
      val {get = tupleRep: S.Type.t -> TupleRep.t Value.t,
	   set = setTupleRep, ...} =
	 Property.getSetOnce (S.Type.plist,
			      Property.initRaise ("tupleRep", S.Type.layout))
      val {get = tyconRep: (Tycon.t
			    -> (TyconRep.t
				* {con: Con.t, rep: ConRep.t} vector) Value.t),
	   set = setTyconRep, ...} =
	 Property.getSetOnce (Tycon.plist,
			      Property.initRaise ("tyconRep", Tycon.layout))
      (* Initialize the datatypes. *)
      val datatypes =
	 Vector.map
	 (datatypes, fn S.Datatype.T {cons, tycon} =>
	  let
	     val computeRef = ref (fn () => raise Fail "can't compute")
	     val rep =
		Value.new
		{compute = fn () => ! computeRef (),
		 equals = fn ((r, _), (r', _)) => TyconRep.equals (r, r'),
		 init = (TyconRep.unit, Vector.new0 ())}
	     val () = setTyconRep (tycon, rep)
	  in
	     {computeRef = computeRef,
	      cons = cons,
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
	      fun array {mutable: bool, ty: S.Type.t}: Rep.t Value.t =
		 let
		    fun new () =
		       let
			  val pt = PointerTycon.new ()
			  val () =
			     List.push
			     (delayedObjectTypes, fn () =>
			      SOME (pt,
				    ObjectType.Array
				    (Rep.ty (Value.get (typeRep ty)))))
		       in
			  Type.pointer pt
		       end
		    datatype z = datatype S.Type.dest
		    val ty =
		       if mutable
			  then new ()
		       else
			  case S.Type.dest ty of
			     Word s =>
				(case Bits.toInt (WordSize.bits s) of
				    8 => Type.word8Vector
				  | 32 => Type.wordVector
				  | _ => new ())
			   | _ => new ()
		 in
		    constant (Rep.T {rep = Rep.Pointer {endsIn00 = true},
				     ty = ty})
		 end
	      datatype z = datatype S.Type.dest
	   in
	      case S.Type.dest t of
		 Array t => array {mutable = true, ty = t}
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
	       | Int s => nonPointer (Type.int s)
	       | IntInf =>
		    constant (Rep.T {rep = Rep.Pointer {endsIn00 = false},
				     ty = Type.intInf})
	       | Real s => nonPointer (Type.real s)
	       | Ref t =>
		    let
		       val r = typeRep t
		       val pt = PointerTycon.new ()
		       val () =
			  List.push
			  (delayedObjectTypes, fn () =>
			   SOME (pt, ObjectType.Normal (Rep.ty (Value.get r))))
		       val ty = Type.pointer pt
		    in
		       constant (Rep.T {rep = Rep.Pointer {endsIn00 = true},
					ty = ty})
		    end
	       | Thread =>
		    constant (Rep.T {rep = Rep.Pointer {endsIn00 = true},
				     ty = Type.thread})
	       | Tuple ts =>
		    let
		       val pt = PointerTycon.new ()
		       val rs = Vector.map (ts, typeRep)
		       fun compute () =
			  TupleRep.make (Vector.map (rs, Value.get), pt)
		       val tr = Value.new {compute = compute,
					   equals = TupleRep.equals,
					   init = TupleRep.unit}
		       val () =
			  List.push
			  (delayedObjectTypes, fn () =>
			   case Value.get tr of
			      TupleRep.Indirect pr =>
				 SOME (pt,
				       ObjectType.Normal
				       (PointerRep.componentsTy pr))
			    | _ => NONE)
		       val () = setTupleRep (t, tr)
		       val () = Vector.foreach (rs, fn r => Value.affect (r, tr))
		       fun compute () = TupleRep.rep (Value.get tr)
		       val r = Value.new {compute = compute,
					  equals = Rep.equals,
					  init = Rep.unit}
		       val () = Value.affect (tr, r)
		    in
		       r
		    end
	       | Vector t => array {mutable = false, ty = t}
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
      val () = S.Program.foreachVar (program, fn (_, t) => ignore (typeRep t))
      val datatypes =
	 Vector.map
	 (datatypes, fn {computeRef, cons, rep, tycon} =>
	  let
	     val cons =
		Vector.map
		(cons, fn {args, con} =>
		 let
		    val pt = PointerTycon.new ()
		 in
		    {args = Vector.map (args, fn t =>
					let
					   val r = typeRep t
					   val () = Value.affect (r, rep)
					in
					   r
					end),
		     con = con,
		     pointerTycon = pt}
		 end)
	     fun compute () =
		TyconRep.make
		(Vector.map (cons, fn {args, con, pointerTycon} =>
			     {args = Vector.map (args, Value.get),
			      con = con,
			      pointerTycon = pointerTycon}))
	     val () = computeRef := compute
	  in
	     {cons = cons,
	      rep = rep,
	      tycon = tycon}
	  end)
      val () = Value.fixedPoint ()
      val {get = conRep, set = setConRep, ...} =
	 Property.getSetOnce (Con.plist, Property.initRaise ("rep", Con.layout))
      val objectTypes =
	 Vector.fold
	 (datatypes, [], fn ({cons, rep, ...}, ac) =>
	  let
	     val (_, conReps) = Value.get rep
	     val () =
		Vector.foreach (conReps, fn {con, rep} => setConRep (con, rep))
	  in
	     Vector.fold
	     (cons, ac, fn ({con, pointerTycon, ...}, ac) =>
	      case conRep con of
		 ConRep.Box pr =>
		    (pointerTycon,
		     ObjectType.Normal (PointerRep.componentsTy pr)) :: ac
	       | _ => ac)
	  end)
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
				str " ",
				TyconRep.layout
				(#1 (Value.get (tyconRep tycon)))])
		  ; display (indent
			     (Vector.layout (fn {con, ...} =>
					     record
					     [("con", Con.layout con),
					      ("rep",
					       ConRep.layout (conRep con))])
			      cons,
			      2))
	       end))))
      fun makeSrc (v, oper) {index} = oper (Vector.sub (v, index))
      fun conApp {args, con, dst, oper, ty} =
	 ConRep.conApp (conRep con,
			{src = makeSrc (args, oper),
			 dst = fn () => (dst (), ty ())})
      fun genCase {cases, default, test, tycon} =
	 TyconRep.genCase
	 (#1 (Value.get (tyconRep tycon)), conRep,
	  {cases = cases,
	   default = default,
	   test = test})
      fun reff {arg: unit -> Rssa.Operand.t, dst: Rssa.Var.t, ty} =
	 TupleRep.tuple (Value.get (refRep ty),
			 {dst = dst,
			  src = fn _ => arg ()})
      fun select {dst, offset, tuple, tupleTy} =
	 TupleRep.select (Value.get (tupleRep tupleTy),
			  {dst = dst,
			   offset = offset,
			   tuple = tuple})
      fun toRtype t = Type.padToPrim (Rep.ty (Value.get (typeRep t)))
      fun tuple {components, dst = (dstVar, dstTy), oper} =
	 TupleRep.tuple (Value.get (tupleRep dstTy),
			 {dst = dstVar,
			  src = makeSrc (components, oper)})
   in
      {conApp = conApp,
       diagnostic = diagnostic,
       genCase = genCase,
       objectTypes = objectTypes,
       reff = reff,
       select = select,
       toRtype = toRtype,
       tuple = tuple}
   end

end
