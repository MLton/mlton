(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
(* Has a special case to make sure that true is represented as 1
 * and false is represented as 0.
 *)
functor Representation (S: REPRESENTATION_STRUCTS): REPRESENTATION = 
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

datatype z = datatype WordSize.prim

structure TupleRep =
   struct
      datatype t = T of {offsets: {offset: int,
				   ty: R.Type.t} option vector,
			 size: int,
			 ty: R.Type.t,
			 tycon: R.PointerTycon.t}

      fun layout (T {offsets, size, ty, tycon, ...}) =
	 let
	    open Layout
	 in record [("offsets",
		     Vector.layout (Option.layout
				    (fn {offset, ty} =>
				     record [("offset", Int.layout offset),
					     ("ty", R.Type.layout ty)]))
		     offsets),
		    ("size", Int.layout size),
		    ("ty", R.Type.layout ty),
		    ("tycon", R.PointerTycon.layout tycon)]
	 end

      local
	 fun make f (T r) = f r
      in
	 val tycon = make #tycon
      end

      fun select (T {offsets, ...}, {dst, offset, tuple}) =
	 case Vector.sub (offsets, offset) of
	    NONE => []
	  | SOME {offset, ty} =>
	       [R.Statement.Bind
		{isMutable = false,
		 oper = R.Operand.Offset {base = tuple (),
					  offset = offset,
					  ty = ty},
		 var = dst ()}]

      fun tuple (T {size, offsets, ty, tycon, ...}, {components, dst, oper}) =
	 let
	    val stores =
	       QuickSort.sortVector
	       (Vector.keepAllMap2
		(components, offsets, fn (x, offset) =>
		 Option.map (offset, fn {offset, ty = _} =>
			     {offset = offset,
			      value = oper x})),
		fn ({offset = i, ...}, {offset = i', ...}) => i <= i')
	 in
	    [R.Statement.Object {dst = dst,
				 size = size + Runtime.normalHeaderSize,
				 stores = stores,
				 ty = ty,
				 tycon = tycon}]
	 end

      fun conSelects (T {offsets, ...}, variant: Operand.t): Operand.t vector =
	 Vector.keepAllMap
	 (offsets, fn off =>
	  Option.map (off, fn {offset, ty} =>
		      Operand.Offset {base = variant,
				      offset = offset,
				      ty = ty}))
   end

structure ConRep =
   struct
      datatype t =
	 (* an integer representing a variant in a datatype *)
	 IntAsTy of {int: int,
		     ty: Rssa.Type.t}
       (* box the arg(s) and add the integer tag as the first word *)
       | TagTuple of {rep: TupleRep.t,
		      tag: int}
       (* just keep the value itself *)
       | Transparent of Rssa.Type.t
       (* box the arg(s) *)
       | Tuple of TupleRep.t
       (* need no representation *)
       | Void

      val layout =
	 let
	    open Layout
	 in
	    fn IntAsTy {int, ty} =>
	          seq [Int.layout int, str ": ", R.Type.layout ty]
	     | TagTuple {rep, tag} =>
		  seq [str "TagTuple ",
		       record [("rep", TupleRep.layout rep),
			       ("tag", Int.layout tag)]]
	     | Transparent t => seq [str "Transparent ", R.Type.layout t]
	     | Tuple r => seq [str "Tuple ", TupleRep.layout r]
	     | Void => str "Void"
	 end

      fun con (cr: t, {args, dst, oper, ty}) =
	 let
	    fun move (oper: Operand.t) =
	       [Statement.Bind {isMutable = false,
				oper = oper,
				var = dst ()}]
	    fun allocate (ys, tr) =
	       TupleRep.tuple (tr, {components = ys,
				    dst = dst (),
				    oper = oper})
	 in
	    case cr of
	       Void => []
	     | IntAsTy {int, ty} =>
		  move (Operand.Cast
			(Operand.int
			 (IntX.make (IntInf.fromInt int,
				     IntSize.default)),
			 ty))
	     | TagTuple {rep, ...} => allocate (args, rep)
	     | Transparent _ =>
		  move (Operand.cast (oper (Vector.sub (args, 0)), ty ()))
	     | Tuple rep => allocate (args, rep)
	 end
   end

structure TyconRep =
   struct
      datatype t =
	 (* Datatype has no representation (Void) or contains a single
	  * variant, and hence constructor requires no additional
	  * representation.
	  *) 
	 Direct
       (* All cons are non-value-carrying and are represented as ints. *)
       | Enum
       (* All cons except for one are non-value-carrying and are
	* represented as ints that are nonzero mod 4.  The value carrying
	* con is represented transparently, i.e. the value is known to be a
	* pointer and is left as such.
	*)
       | EnumDirect
       (* All cons except for one are non-value-carrying and are
	* represented as ints that are nonzero mod 4.  The value carrying
	* con is represented by boxing its arg.
	*)
       | EnumIndirect
       (* Non-value-carrying and are represented as ints that are nonzero
	* mod 4.  Value carrying cons are represented by boxing the args
	* and adding an integer tag.
	*)
       | EnumIndirectTag
       (* All cons are value carrying and are represented by boxing the
	* args and adding an integer tag.
	*)
       | IndirectTag
       | Void

      val layout =
	 let
	    open Layout
	 in
	    fn Direct => str "Direct"
	     | Enum => str "Enum"
	     | EnumDirect => str "EnumDirect"
	     | EnumIndirect => str "EnumIndirect"
	     | EnumIndirectTag => str "EnumIndirectTag"
	     | IndirectTag => str "IndirectTag"
	     | Void => str "Void"
	 end
      
      val equals:t * t -> bool = op =

      fun genCase (testRep: t,
		   {cases: (ConRep.t * Label.t) vector,
		    default: Label.t option,
		    test: unit -> Operand.t}) =
	 let
	    datatype z = datatype Operand.t
	    datatype z = datatype Transfer.t
	    val extraBlocks = ref []
	    fun newBlock {args, kind,
			  statements: Statement.t vector,
			  transfer: Transfer.t}: Label.t =
	       let
		  val l = Label.newNoname ()
		  val _ = List.push (extraBlocks,
				     Block.T {args = args,
					      kind = kind,
					      label = l,
					      statements = statements,
					      transfer = transfer})
	       in
		  l
	       end
	    fun enum (test: Operand.t): Transfer.t =
	       let
		  val cases =
		     Vector.keepAllMap
		     (cases, fn (c, j) =>
		      case c of
			 ConRep.IntAsTy {int, ...} => SOME (int, j)
		       | _ => NONE)
		  val numEnum =
		     case Operand.ty test of
			Type.EnumPointers {enum, ...} => Vector.length enum
		      | _ => Error.bug "strage enum"
		  val default =
		     if numEnum = Vector.length cases
			then NONE
		     else default
	       in
		  if 0 = Vector.length cases
		     then
			(case default of
			    NONE => Error.bug "no targets"
			  | SOME l => Goto {dst = l,
					    args = Vector.new0 ()})
		  else
		     let
			val l = #2 (Vector.sub (cases, 0))
		     in
			if Vector.forall (cases, fn (_, l') =>
					  Label.equals (l, l'))
			   andalso (case default of
				       NONE => true
				     | SOME l' => Label.equals (l, l'))
			   then Goto {dst = l,
				      args = Vector.new0 ()}
			else
			   let
			      val cases =
				 QuickSort.sortVector
				 (cases, fn ((i, _), (i', _)) => i <= i')
			      val cases =
				 Vector.map (cases, fn (i, l) =>
					     (IntX.make (IntInf.fromInt i,
							 IntSize.default),
					      l))
			   in
			      Switch
			      (Switch.Int {cases = cases,
					   default = default,
					   size = IntSize.default,
					   test = test})
			   end
		     end
	       end
	    fun switchEP
	       (makePointersTransfer: Operand.t -> Statement.t list * Transfer.t)
	       : Transfer.t =
	       let
		  val test = test ()
		  val {enum = e, pointers = p} =
		     case Operand.ty test of
			Type.EnumPointers ep => ep
		      | _ => Error.bug "strange switchEP"
		  val enumTy = Type.EnumPointers {enum = e,
						  pointers = Vector.new0 ()}
		  val enumVar = Var.newNoname ()
		  val enumOp = Var {var = enumVar,
				    ty = enumTy}
		  val pointersTy = Type.EnumPointers {enum = Vector.new0 (),
						      pointers = p}
		  val pointersVar = Var.newNoname ()
		  val pointersOp = Var {ty = pointersTy,
					var = pointersVar}
		  fun block (var, ty, statements, transfer) =
		     newBlock {args = Vector.new0 (),
			       kind = Kind.Jump,
			       statements = (Vector.fromList
					     (Statement.Bind
					      {isMutable = false,
					       oper = Cast (test, ty),
					       var = var}
					      :: statements)),
			       transfer = transfer}
		  val (s, t) = makePointersTransfer pointersOp
		  val pointers = block (pointersVar, pointersTy, s, t)
		  val enum = block (enumVar, enumTy, [], enum enumOp)
	       in
		  Switch (Switch.EnumPointers {enum = enum,
					       pointers = pointers,
					       test = test})
	       end
	    fun enumAndOne (): Transfer.t =
	       let
		  fun make (pointersOp: Operand.t)
		     : Statement.t list * Transfer.t =
		     let
			val (dst, args: Operand.t vector) =
			   case Vector.peekMap
			      (cases, fn (c, j) =>
			       case c of
				  ConRep.Transparent _ =>
				     SOME (j, Vector.new1 pointersOp)
				| ConRep.Tuple r =>
				     SOME (j,
					   TupleRep.conSelects (r, pointersOp))
				| _ => NONE) of
			      NONE =>
				 (case default of
				     NONE => Error.bug "enumAndOne: no default"
				   | SOME j => (j, Vector.new0 ()))
			    | SOME z => z
		     in
			([], Goto {args = args, dst = dst})
		     end
	       in
		  switchEP make
	       end
	    fun indirectTag (test: Operand.t): Statement.t list * Transfer.t =
	       let
		  val cases =
		     Vector.keepAllMap
		     (cases, fn (c, l) =>
		      case c of
			 ConRep.TagTuple {rep, tag} =>
			    let
			       val tycon = TupleRep.tycon rep
			       val tag = PointerTycon.index tycon
			       val pointerVar = Var.newNoname ()
			       val pointerTy = Type.pointer tycon
			       val pointerOp =
				  Operand.Var {ty = pointerTy,
					       var = pointerVar}
			       val statements =
				  Vector.new1
				  (Statement.Bind
				   {isMutable = false,
				    oper = Cast (test, pointerTy),
				    var = pointerVar})
			       val dst =
				  newBlock
				  {args = Vector.new0 (),
				   kind = Kind.Jump,
				   statements = statements,
				   transfer =
				   Goto
				   {args = TupleRep.conSelects (rep, pointerOp),
				    dst = l}}
			    in
			       SOME {dst = dst,
				     tag = tag,
				     tycon = tycon}
			    end
		       | _ => NONE)
		  val numTag =
		     case Operand.ty test of
			Type.EnumPointers {pointers, ...} =>
			   Vector.length pointers
		      | _ => Error.bug "strange indirecTag"
		  val default =
		     if numTag = Vector.length cases
			then NONE
		     else default
		  val cases =
		     QuickSort.sortVector
		     (cases, fn ({tycon = t, ...}, {tycon = t', ...}) =>
		      PointerTycon.<= (t, t'))
		  val headerOffset = ~4
		  val tagVar = Var.newNoname ()
		  val s =
		     Statement.PrimApp
		     {args = (Vector.new2
			      (Offset {base = test,
				       offset = headerOffset,
				       ty = Type.defaultWord},
			       Operand.word (WordX.one WordSize.default))),
		      dst = SOME (tagVar, Type.defaultWord),
		      prim = Prim.wordRshift WordSize.default}
		  val tag =
		     Cast (Var {ty = Type.defaultWord,
				var = tagVar},
			   Type.defaultInt)
	       in
		  ([s], Switch (Switch.Pointer {cases = cases,
						default = default,
						tag = tag,
						test = test}))
	       end
	    fun prim () =
	       case (Vector.length cases, default) of
		  (1, _) =>
		     (* We use _ instead of NONE for the default becuase
		      * there may be an unreachable default case.
		      *)
		     let
			val (c, l) = Vector.sub (cases, 0)
		     in
			case c of
			   ConRep.Void =>
			      Goto {dst = l,
				    args = Vector.new0 ()}
			 | ConRep.Transparent _ =>
			      Goto {dst = l,
				    args = Vector.new1 (test ())}
			 | ConRep.Tuple r =>
			      Goto {dst = l,
				    args = TupleRep.conSelects (r, test ())}
			 | _ => Error.bug "strange conRep for Prim"
		     end
		| (0, SOME l) => Goto {dst = l, args = Vector.new0 ()}
		| _ => Error.bug "prim datatype with more than one case"
	    val (ss, t) =
	       case testRep of
		  Direct => ([], prim ())
		| Enum => ([], enum (test ()))
		| EnumDirect => ([], enumAndOne ())
		| EnumIndirect => ([], enumAndOne ())
		| EnumIndirectTag => ([], switchEP indirectTag)
		| IndirectTag => indirectTag (test ())
		| Void => ([], prim ())
	 in
	    (ss, t, !extraBlocks)
	 end
   end

fun compute (program as Ssa.Program.T {datatypes, ...}) =
   let
      val {get = tyconRep, set = setTyconRep, ...} =
	 Property.getSet (Tycon.plist,
			  Property.initRaise ("rep", Tycon.layout))
      val tyconRep =
	 Trace.trace ("tyconRep", Tycon.layout, TyconRep.layout) tyconRep
      val {get = conRep, set = setConRep, ...} =
	 Property.getSetOnce (Con.plist,
			      Property.initRaise ("rep", Con.layout))
      fun isEmpty (t: S.Type.t): bool =
	 let
	    datatype z = datatype S.Type.dest
	 in
	    case S.Type.dest t of
	       Datatype c => (case tyconRep c of
				 TyconRep.Void => true
			       | _ => false)
	     | Tuple ts => Vector.isEmpty ts
	     | _ => false
	 end
      (* Split constructors into those that carry values and those that
       * don't.
       *)
      fun splitCons cons =
	 Vector.fold (cons, ([], []), fn ({con, args}, (no, have)) =>
		      if Vector.forall (args, isEmpty)
			 then (con :: no, have)
		      else (no, {con = con, args = args} :: have))
      (* Compute a least-fixed-point on tycon representations.  Initially
       * assume all datatype tycons are Void.  Change them if they have more
       * than one variant or contain a useful component.
       *)
      val _ =
	 Vector.foreach (datatypes, fn S.Datatype.T {tycon, ...} =>
			 setTyconRep (tycon, TyconRep.Void))
      val _ =
	 FixedPoint.fix'
	 (fn continue =>
	  Vector.foreach
	  (datatypes, fn S.Datatype.T {tycon, cons} =>
	   let
	      val (noArgs, haveArgs) = splitCons cons
	      val old = tyconRep tycon
	      val new =
		 case (noArgs, haveArgs) of
		    ([], []) => TyconRep.Void
		  | ([_], []) => TyconRep.Void
		  | (_, []) => TyconRep.Enum
		  | ([], [_]) => TyconRep.Direct
		  | (_, [{args, ...}]) =>
		       if (if 1 = Vector.length args
			      then
				 let
				    val a = Vector.sub (args, 0)
				    (* Which types are guaranteed to be
				     * translated to R.Type.Pointer and are
				     * represented as zero mod 4.
				     *)
				    datatype z = datatype S.Type.dest
				 in
				    case S.Type.dest a of
				       Array _ => true
				     | Datatype c =>
					  (case tyconRep c of
					      TyconRep.IndirectTag => true
					    | _ => false)
				     | Ref _ => true
				     | Tuple _ => true
				     | Vector _ => true
				     | _ => false
				 end
			   else true)
			  then TyconRep.EnumDirect
		       else TyconRep.EnumIndirect
		  | ([], _) => TyconRep.IndirectTag
		  | _ => TyconRep.EnumIndirectTag
	   in if TyconRep.equals (old, new)
		 then ()
	      else (continue ()
		    ; setTyconRep (tycon, new))
	   end))
      (* Accumulate all the ObjectTypes. *)
      val objectTypes = ref []
      (* Keep track of pointer types -- build them later, though. *)
      val {get = refRep: S.Type.t -> TupleRep.t, set = setRefRep, ...} =
	 Property.getSetOnce
	 (S.Type.plist, Property.initRaise ("refRep", S.Type.layout))
      val {get = tupleRep: S.Type.t -> TupleRep.t,
	   set = setTupleRep, ...} =
	 Property.getSetOnce
	 (S.Type.plist, Property.initRaise ("tupleRep", S.Type.layout))
      val {get = tyconCons, set = setTyconCons, ...} =
	 Property.getSetOnce
	 (Tycon.plist, Property.initRaise ("cons", Tycon.layout))
      val _ =
	 Vector.foreach (datatypes, fn S.Datatype.T {cons, tycon} =>
			 setTyconCons (tycon, cons))
      (* We have to break the cycle in recursive types to avoid an infinite
       * recursion when converting from S.Type.t to R.Type.t.  This is done
       * by creating pointer tycons and delaying building the corresponding
       * object types until after toRtype is done.  The "finish" list keeps
       * the list of things to do later.
       *)
      val finish: (unit -> unit) list ref = ref []
      val {get = toRtype: S.Type.t -> R.Type.t option, ...} =
	 Property.get
	 (S.Type.plist,
	  Property.initRec
	  (fn (t: S.Type.t, toRtype) =>
	   let
	      fun typesRep {isNormal: bool,
			    isTagged: bool,
			    mutable: bool,
			    pointerTycon: R.PointerTycon.t,
			    ty: R.Type.t,
			    tys: S.Type.t vector}: TupleRep.t =
		 let
		    val initialOffset = if isTagged then Runtime.wordSize else 0
		    val tys = Vector.map (tys, toRtype)
		    val bytes = ref []
		    val doubleWords = ref []
		    val halfWords = ref []
		    val words = ref []
		    val pointers = ref []
		    val _ =
		       Vector.foreachi
		       (tys, fn (i, t) =>
			case t of
			   NONE => ()
			 | SOME t =>
			      let
				 val r =
				    if let
					  datatype z = datatype R.Type.t
				       in
					  case t of
					     EnumPointers {pointers, ...} =>
						0 < Vector.length pointers
					   | IntInf => true
					   | _ => false
				       end
				       then pointers
				    else (case R.Type.size t of
					     1 => bytes
					   | 2 => halfWords
					   | 4 => words
					   | 8 => doubleWords
					   | _ => Error.bug "strange size")
			      in
				 List.push (r, (i, t))
			      end)
		    fun build (r, size, accum) =
		       List.fold
		       (!r, accum, fn ((index, ty), (res, offset)) =>
			({index = index, offset = offset, ty = ty} :: res,
			 offset + size))
		    val (accum, offset: int) =
		       build (bytes, 1,
		       build (halfWords, 2,
		       build (words, 4,
		       build (doubleWords, 8, 
			      ([], initialOffset)))))
		    val offset =
		       if isNormal
			  then
			     let
				val offset = CType.align (CType.pointer, offset)
			     in
				if !Control.align = Control.Align8
				andalso
				   0 < Int.rem (Runtime.normalHeaderSize
						+ offset
						+ (Runtime.pointerSize
						   * List.length (!pointers)),
						8)
				   then offset + 4
				else offset
			     end
		       else offset
		    val (components, size) = build (pointers, 4, (accum, offset))
		    val size = if 0 = size then 4 else size
		    val offsets =
		       Vector.mapi
		       (tys, fn (i, ty) =>
			Option.map
			(ty, fn _ =>
			 let
			    val {offset, ty, ...} =
			       valOf
			       (List.peek
				(components, fn {index, ...} => i = index))
			 in
			    {offset = offset, ty = ty}
			 end))
		    val components =
		       List.revMap
		       (components, fn {offset, ty, ...} =>
			{mutable = mutable, offset = offset, ty = ty})
		    val components =
		       if isTagged
			  then {mutable = false,
				offset = 0,
				ty = R.Type.int IntSize.default} :: components
		       else components
		    val components =
		       Vector.fromArray
		       (QuickSort.sortArray
			(Array.fromList components,
			 fn ({offset = i, ...}, {offset = i', ...}) =>
			 i <= i'))
		    val mc = R.MemChunk.T {components = components,
					   size = size}
		    val _ =
		       List.push
		       (objectTypes,
			(pointerTycon,
			 if isNormal
			    then R.ObjectType.Normal mc
			 else R.ObjectType.Array mc))
		 in
		    TupleRep.T {offsets = offsets,
				size = size,
				ty = ty,
				tycon = pointerTycon}
		 end
	      fun pointer {fin, isNormal, mutable, tys}: R.Type.t =
		 let
		    val pt = R.PointerTycon.new ()
		    val ty = R.Type.pointer pt
		    val _ =
		       List.push
		       (finish, fn () =>
			fin (typesRep {isNormal = isNormal,
				       isTagged = false,
				       mutable = mutable,
				       pointerTycon = pt,
				       ty = ty,
				       tys = tys}))
		 in
		    ty
		 end
	      fun convertDatatype (tycon: Tycon.t): R.Type.t option =
		 let
		    val (noArgs', haveArgs') = splitCons (tyconCons tycon)
		    val noArgs = Vector.fromList noArgs'
		    val haveArgs = Vector.fromList haveArgs'
		    fun pointers () =
		       Vector.tabulate (Vector.length haveArgs, fn _ =>
					R.PointerTycon.new ())
		    fun indirect {conRep, isTagged, pointerTycons, ty} =
		       List.push
		       (finish, fn () =>
			Vector.foreachi2
			(pointerTycons, haveArgs, fn (i, pt, {con, args}) =>
			 let
			    val rep =
			       typesRep {isNormal = true,
					 isTagged = isTagged,
					 mutable = false,
					 pointerTycon = pt,
					 ty = ty,
					 tys = args}
			 in
			    setConRep (con, conRep {rep = rep, tag = i})
			 end))
		    fun transparent {con, args} =
		       let
			  val ty =
			     case toRtype (Vector.sub (args, 0)) of
				NONE => Error.bug "strange transparent"
			      | SOME ty => ty
			  val _ = setConRep (con, ConRep.Transparent ty)
		       in
			  ty
		       end
		    fun enumAnd (pointers: R.PointerTycon.t vector): R.Type.t =
		       let
			  val enum =
			     Vector.tabulate
			     (Vector.length noArgs, fn i => 2 * i + 1)
			  val ty =
			     R.Type.EnumPointers {enum = enum,
						  pointers = pointers}
			  val _ =
			     Vector.foreach2
			     (noArgs, enum, fn (c, i) =>
			      setConRep (c, (ConRep.IntAsTy
					     {int = i, ty = ty})))
		       in
			  ty
		       end
		    fun indirectTag (): R.Type.t =
		       let
			  val pts = pointers ()
			  val ty = enumAnd pts
			  val _ = indirect {isTagged = false,
					    conRep = ConRep.TagTuple,
					    pointerTycons = pts,
					    ty = ty}
		       in
			  ty
		       end
		 in
		    case tyconRep tycon of
		       TyconRep.Direct =>
			  (case (noArgs', haveArgs') of
			      ([], []) => NONE
			    | ([con], []) =>
				 (setConRep (con, ConRep.Void)
				  ; NONE)
			    | ([], [ac as {args, con}]) =>
				 if 1 = Vector.length args
				    then SOME (transparent ac)
				 else
				    SOME
				    (pointer
				     {fin = (fn r =>
					     setConRep (con, ConRep.Tuple r)),
				      isNormal = true,
				      mutable = false,
				      tys = args})
			    | _ =>
				 Error.bug
				 (concat ["strange TyconRep.Direct for ",
					  Layout.toString (Tycon.layout tycon)]))
		     | TyconRep.Enum =>
			  let
			     val enum =
				Vector.tabulate
				(Vector.length noArgs, fn i => i)
			     val ty =
				R.Type.EnumPointers {enum = enum,
						     pointers = Vector.new0 ()}
			     fun set (i, c) =
				setConRep (c, (ConRep.IntAsTy
					       {int = i, ty = ty}))
			     val _ =
				if Tycon.equals (tycon, Tycon.bool)
				   then (set (0, Con.falsee)
					 ; set (1, Con.truee))
				else Vector.foreachi (noArgs, set)
			  in
			     SOME ty
			  end
		     | TyconRep.EnumDirect =>
			  (case haveArgs' of
			      [ca as {con, args}] =>
				 if 1 = Vector.length args
				    then
				       case transparent ca of
					  R.Type.EnumPointers {pointers, ...} =>
					     SOME (enumAnd pointers)
					| _ =>
					     Error.bug "EnumDirect of non pointer"
				 else
				    let
				       val pt = R.PointerTycon.new ()
				       val ty = enumAnd (Vector.new1 pt)
				       val _ =
					  List.push
					  (finish, fn () =>
					   setConRep
					   (con,
					    ConRep.Tuple
					    (typesRep
					     {isNormal = true,
					      isTagged = false,
					      mutable = false,
					      pointerTycon = pt,
					      ty = ty,
					      tys = args})))
				    in
				       SOME ty
				    end
					| _ =>
					     Error.bug "strange haveArgs for EnumDirect")
		     | TyconRep.EnumIndirect =>
			  let
			     val pts = pointers ()
			     val ty = enumAnd pts
			     val _ = indirect {conRep = ConRep.Tuple o #rep,
					       isTagged = false,
					       pointerTycons = pts,
					       ty = ty}
			  in
			     SOME ty
			  end
		     | TyconRep.EnumIndirectTag => SOME (indirectTag ())
		     | TyconRep.IndirectTag => SOME (indirectTag ())
		     | TyconRep.Void =>
			  let
			     val _ =
				case (noArgs', haveArgs') of
				   ([], []) => ()
				 | ([con], []) => setConRep (con, ConRep.Void)
				 | _ => Error.bug "strange TyconRep.Void"
			  in
			     NONE
			  end
		 end
	      fun array {mutable: bool, ty: S.Type.t}: R.Type.t =
		 let
		    fun new () =
		       pointer {fin = fn _ => (),  
				isNormal = false,
				mutable = mutable,
				tys = Vector.new1 ty}
		    datatype z = datatype S.Type.dest
		 in
		    if mutable
		       then new ()
		    else
		       case S.Type.dest ty of
			  Word s =>
			     (case WordSize.prim s of
				 W8 => R.Type.word8Vector
			       | W32 => R.Type.wordVector
			       | _ => new ())
			| _ => new ()
		 end
	      datatype z = datatype S.Type.dest
	   in
	      case S.Type.dest t of
		 Array t => SOME (array {mutable = true, ty = t})
	       | Datatype tycon => convertDatatype tycon
	       | Int s => SOME (R.Type.int (IntSize.roundUpToPrim s))
	       | IntInf => SOME R.Type.intInf
	       | PreThread => SOME R.Type.thread
	       | Real s => SOME (R.Type.real s)
	       | Ref t =>
		    SOME (pointer {fin = fn r => setRefRep (t, r),
				   isNormal = true,
				   mutable = true,
				   tys = Vector.new1 t})
	       | Thread => SOME R.Type.thread
	       | Tuple ts =>
		    if Vector.isEmpty ts
		       then NONE
		    else
		       SOME (pointer {fin = fn r => setTupleRep (t, r),
				      isNormal = true,
				      mutable = false,
				      tys = S.Type.deTuple t})
	       | Vector t => SOME (array {mutable = false, ty = t})
	       | Weak t =>
		    (case toRtype t of
			NONE => NONE
		      | SOME t =>
			   if R.Type.isPointer t
			      then
				 let
				     val pt = PointerTycon.new ()
				     val _ =
					List.push
					(objectTypes,
					 (pt, R.ObjectType.weak t))
				  in
				     SOME (R.Type.pointer pt)
				  end
			   else NONE)
	       | Word s => SOME (R.Type.word (WordSize.roundUpToPrim s))
	   end))
      val toRtype =
	 Trace.trace
	 ("toRtype", S.Type.layout, Option.layout R.Type.layout)
	 toRtype
      val _ = S.Program.foreachVar (program, fn (_, t) => ignore (toRtype t))
      val n = List.length (!finish)
      val _ = List.foreach (!finish, fn f => f ())
      val _ =
	 if n = List.length (!finish)
	    then ()
	 else Error.bug "missed finish"
      val objectTypes =
	 Vector.map
	 (QuickSort.sortVector
	  (Vector.concat [ObjectType.basic,
			  Vector.fromList (!objectTypes)],
	   fn ((pt, _), (pt', _)) => PointerTycon.<= (pt, pt')),
	  #2)
      val _ =
	 Control.diagnostics
	 (fn display =>
	  (display (Layout.str "Representations:")
	   ; (Vector.foreach
	      (datatypes, fn S.Datatype.T {tycon, cons} =>
	       let
		  open Layout
	       in
		  display (seq [Tycon.layout tycon,
				str " ",
				TyconRep.layout (tyconRep tycon)])
		  ; display (indent
			     (Vector.layout (fn {con, ...} =>
					     record
					     [("con", Con.layout con),
					      ("rep",
					       ConRep.layout (conRep con))])
			      cons,
			      2))
	       end))))
   in
      {conRep = conRep,
       objectTypes = objectTypes,
       refRep = refRep,
       toRtype = toRtype,
       tupleRep = tupleRep,
       tyconRep = tyconRep}
   end

end
