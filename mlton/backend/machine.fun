(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

functor Machine (S: MACHINE_STRUCTS): MACHINE =
struct

open S

structure Runtime = Runtime ()
local
   open Runtime
in
   structure CFunction = CFunction
   structure GCField = GCField
end

structure Atoms = MachineAtoms (structure Label = Label
				structure Prim = Prim
				structure Runtime = Runtime
				structure SourceInfo = SourceInfo)
open Atoms

structure ChunkLabel = IdNoAst (val noname = "ChunkLabel")

structure SmallIntInf =
   struct
      type t = word
   end

structure Register =
   struct
      datatype t = T of {index: int option ref,
			 ty: Type.t}

      local
	 fun make f (T r) = f r
      in
	 val indexOpt = ! o (make #index)
	 val ty = make #ty
      end

      fun layout (T {index, ty, ...}) =
	 let
	    open Layout
	 in
	    seq [str (concat ["R", Type.name ty]),
		 paren (case !index of
			   NONE => str "NONE"
			 | SOME i => Int.layout i),
		 str ": ",
		 Type.layout ty]
	 end

      val toString = Layout.toString o layout

      fun index (r as T {index, ...}) =
	 case !index of
	    NONE =>
	       Error.bug (concat ["register ", toString r, " missing index"])
	  | SOME i => i

      fun setIndex (r as T {index, ...}, i) =
	 case !index of
	    NONE => index := SOME i
	  | SOME _ =>
	       Error.bug (concat ["register ", toString r, " index already set"])

      local
	 val c = Counter.new 0
      in
	 fun new (ty, i) = T {index = ref i,
			      ty = ty}
      end

      fun equals (r, r') =
	 (case (indexOpt r, indexOpt r') of
	     (SOME i, SOME i') => i = i'
	   | _ => false)
	 andalso Runtime.Type.equals (Type.toRuntime (ty r),
				      Type.toRuntime (ty r'))

      val equals =
	 Trace.trace2 ("Register.equals", layout, layout, Bool.layout) equals
   end

structure Global =
   struct
      datatype t = T of {index: int,
			 isRoot: bool,
			 ty: Type.t}

      fun layout (T {index, isRoot, ty, ...}) =
	 let
	    open Layout
	 in
	    seq [str "glob ",
		 record [("index", Int.layout index),
			 ("isRoot", Bool.layout isRoot),
			 ("ty", Type.layout ty)]]
	 end
      
      val toString = Layout.toString o layout

      local
	 fun make f (T r) = f r
      in
	 val index = make #index
	 val isRoot = make #isRoot
	 val ty = make #ty
      end

      val nonRootCounter = Counter.new 0
      fun numberOfNonRoot () = Counter.value nonRootCounter

      val memo = Runtime.Type.memo (fn _ => Counter.new 0)
      fun numberOfType t = Counter.value (memo t)
	 
      fun new {isRoot, ty} =
	 let
	    val isRoot = isRoot orelse not (Type.isPointer ty)
	    val counter =
	       if isRoot
		  then memo (Type.toRuntime ty)
	       else nonRootCounter
	    val g = T {index = Counter.next counter,
		       isRoot = isRoot,
		       ty = ty}
	 in
	    g
	 end

      fun equals (T {index = i, isRoot = r, ty},
		  T {index = i', isRoot = r', ty = ty'}) =
	 i = i'
	 andalso r = r'
	 andalso Type.equals (ty, ty')
   end

structure StackOffset =
   struct
      type t = {offset: int,
		ty: Type.t}

      fun layout {offset, ty} =
	 let
	    open Layout
	 in
	    seq [str (concat ["S", Type.name ty]),
		 paren (Int.layout offset),
		 str ": ", Type.layout ty]
	 end

      fun equals ({offset = i, ty}, {offset = i', ty = ty'}) =
	 i = i' andalso Type.equals (ty, ty')

      fun interfere ({offset = off, ty = ty}, {offset = off', ty = ty'}): bool =
	 let 
	    val max = off + Type.size ty
	    val max' = off' + Type.size ty'
	 in
	    max > off' andalso max' > off
	 end
   end

structure Operand =
   struct
      datatype t =
	 ArrayOffset of {base: t,
			 index: t,
			 ty: Type.t}
       | Cast of t * Type.t
       | Char of char
       | Contents of {oper: t,
		      ty: Type.t}
       | File
       | GCState
       | Global of Global.t
       | Int of int
       | SmallIntInf of SmallIntInf.t
       | Label of Label.t
       | Line
       | Offset of {base: t, offset: int, ty: Type.t}
       | Register of Register.t
       | Real of string
       | Runtime of GCField.t
       | StackOffset of StackOffset.t
       | Word of Word.t
    
      val rec isLocation =
	 fn ArrayOffset _ => true
	  | Cast (z, _) => isLocation z
	  | Contents _ => true
	  | Global _ => true
	  | Offset _ => true
	  | Register _ => true
	  | Runtime z => true
	  | StackOffset _ => true
	  | _ => false

      fun layout (z: t): Layout.t =
	 let
	    open Layout 
	    fun constrain (ty: Type.t): Layout.t =
	       if !Control.showTypes
		  then seq [str ": ", Type.layout ty]
	       else empty
	 in
	    case z of
	       ArrayOffset {base, index, ty} =>
		  seq [str (concat ["X", Type.name ty, " "]),
		       tuple [layout base, layout index],
		       constrain ty]
	     | Cast (z, ty) =>
		  seq [str "Cast ", tuple [layout z, Type.layout ty]]
	     | Char c => str (Char.escapeC c)
	     | Contents {oper, ty} =>
		  seq [str (concat ["C", Type.name ty, " "]),
		       paren (layout oper)]
	     | File => str "<File>"
	     | GCState => str "<GCState>"
	     | Global g => Global.layout g
	     | Int i => Int.layout i
	     | Label l => Label.layout l
	     | Line => str "<Line>"
	     | Offset {base, offset, ty} =>
		  seq [str (concat ["O", Type.name ty, " "]),
		       tuple [layout base, Int.layout offset],
		       constrain ty]
	     | Real s => str s
	     | Register r => Register.layout r
	     | Runtime r => GCField.layout r
	     | SmallIntInf w => seq [str "SmallIntInf ", paren (Word.layout w)]
	     | StackOffset so => StackOffset.layout so
	     | Word w => seq [str "0x", Word.layout w]
	 end

    val toString = Layout.toString o layout

    val ty =
       fn ArrayOffset {ty, ...} => ty
	| Cast (_, ty) => ty
	| Char _ => Type.char
	| Contents {ty, ...} => ty
	| File => Type.cpointer
	| GCState => Type.cpointer
	| Global g => Global.ty g
	| Int _ => Type.int
	| Label l => Type.label l
	| Line => Type.int
	| Offset {ty, ...} => ty
	| Real _ => Type.real
	| Register r => Register.ty r
	| Runtime f =>
	     (case f of
		 GCField.ExnStack => Type.exnStack
	       | _ => Type.fromRuntime (GCField.ty f))
	| SmallIntInf _ => Type.intInf
	| StackOffset {ty, ...} => ty
	| Word _ => Type.word
	 
      val rec equals =
	 fn (ArrayOffset {base = b, index = i, ...},
	     ArrayOffset {base = b', index = i', ...}) =>
	        equals (b, b') andalso equals (i, i') 
	   | (Cast (z, t), Cast (z', t')) =>
		Type.equals (t, t') andalso equals (z, z')
	   | (Char c, Char c') => c = c'
	   | (Contents {oper = z, ...}, Contents {oper = z', ...}) =>
		equals (z, z')
	   | (File, File) => true
	   | (GCState, GCState) => true
	   | (Global g, Global g') => Global.equals (g, g')
	   | (Int i, Int i') => i = i'
	   | (Label l, Label l') => Label.equals (l, l')
	   | (Line, Line) => true
	   | (Offset {base = b, offset = i, ...},
	      Offset {base = b', offset = i', ...}) =>
	        equals (b, b') andalso i = i' 
	   | (Real s, Real s') => s = s'
	   | (Register r, Register r') => Register.equals (r, r')
	   | (Runtime f, Runtime f') => GCField.equals (f, f')
	   | (SmallIntInf w, SmallIntInf w') => Word.equals (w, w')
	   | (StackOffset so, StackOffset so') => StackOffset.equals (so, so')
	   | (Word w, Word w') => w = w'
	   | _ => false

      fun interfere (write: t, read: t): bool =
	 let
	    fun inter read = interfere (write, read)
	 in
	    case (read, write) 
	       of (ArrayOffset {base, index, ...}, _) => 
		  inter base orelse inter index
	     | (Contents {oper, ...}, _) => inter oper
	     | (Global g, Global g') => Global.equals (g, g')
	     | (Offset {base, offset, ...}, _) => inter base
	     | (Register r, Register r') => Register.equals (r, r')
	     | (StackOffset so, StackOffset so') =>
		  StackOffset.interfere (so, so')
	     | _ => false
	 end
   end

structure Switch = Switch (open Atoms
			   structure Use = Operand)

structure Statement =
   struct
      datatype t =
	 Move of {dst: Operand.t,
		  src: Operand.t}
       | Noop
       | Object of {dst: Operand.t,
		    header: word,
		    size: int,
		    stores: {offset: int,
			     value: Operand.t} vector}
       | PrimApp of {args: Operand.t vector,
		     dst: Operand.t option,
		     prim: Prim.t}
       | ProfileLabel of ProfileLabel.t

      val layout =
	 let
	    open Layout
	 in
	    fn Move {dst, src} =>
		  mayAlign [Operand.layout dst,
			    seq [str " = ", Operand.layout src]]
	     | Noop => str "Noop"
	     | Object {dst, header, size, stores} =>
		  mayAlign
		  [Operand.layout dst,
		   seq [str " = Object ",
			record [("header", Word.layout header),
				("size", Int.layout size)],
			str " ",
			Vector.layout (fn {offset, value} =>
				       record [("offset", Int.layout offset),
					       ("value", Operand.layout value)])
			stores]]
	     | PrimApp {args, dst, prim, ...} =>
		  let
		     val rest =
			seq [Prim.layout prim, str " ",
			     Vector.layout Operand.layout args]
		  in
		     case dst of
			NONE => rest
		      | SOME z =>
			   mayAlign [Operand.layout z,
				     seq [str " = ", rest]]
		  end
	     | ProfileLabel l =>
		  seq [str "ProfileLabel ", ProfileLabel.layout l]
	 end
 
      fun move (arg as {dst, src}) =
	 if Operand.equals (dst, src)
	    then Noop
	 else Move arg

      val move =
	 Trace.trace ("Statement.move",
		      fn {dst, src} =>
		      Layout.record [("dst", Operand.layout dst),
				     ("src", Operand.layout src)],
		      layout)
	 move
	 
      fun moves {srcs, dsts} =
	 Vector.fromListRev
	 (Vector.fold2 (srcs, dsts, [], fn (src, dst, ac)  =>
			move {src = src, dst = dst} :: ac))

      fun foldOperands (s, ac, f) =
	 case s of
	    Move {dst, src} => f (dst, f (src, ac))
	  | Object {dst, stores, ...} =>
	       Vector.fold
	       (stores, f (dst, ac), fn ({value, ...}, ac) => f (value, ac))
	  | PrimApp {args, dst, ...} =>
	       Vector.fold (args, Option.fold (dst, ac, f), f)
	  | _ => ac

      fun foldDefs (s, a, f) =
	 case s of
	    Move {dst, ...} => f (dst, a)
	  | Object {dst, ...} => f (dst, a)
	  | PrimApp {dst, ...} => (case dst of
				      NONE => a
				    | SOME z => f (z, a))
	  | _ => a
   end

structure FrameInfo =
   struct
      datatype t = T of {frameLayoutsIndex: int}

      local
	 fun make f (T r) = f r
      in
	 val frameLayoutsIndex = make #frameLayoutsIndex
      end

      fun layout (T {frameLayoutsIndex, ...}) =
	 Layout.record [("frameLayoutsIndex", Int.layout frameLayoutsIndex)]

      fun equals (T {frameLayoutsIndex = i}, T {frameLayoutsIndex = i'}) =
	 i = i'
   end

structure Transfer =
   struct
      datatype t =
	 Arith of {args: Operand.t vector,
		   dst: Operand.t,
		   overflow: Label.t,
		   prim: Prim.t,
		   success: Label.t,
		   ty: Type.t}
       | CCall of {args: Operand.t vector,
		   frameInfo: FrameInfo.t option,
		   func: CFunction.t,
		   return: Label.t option}
       | Call of {label: Label.t,
		  live: Operand.t vector,
		  return: {return: Label.t,
			   handler: Label.t option,
			   size: int} option}
       | Goto of Label.t
       | Raise
       | Return
       | Switch of Switch.t

      fun layout t =
	 let
	    open Layout
	 in
	    case t of
	       Arith {prim, args, dst, overflow, success, ...} =>
		  seq [str "Arith ",
		       record [("prim", Prim.layout prim),
			       ("args", Vector.layout Operand.layout args),
			       ("dst", Operand.layout dst),
			       ("overflow", Label.layout overflow),
			       ("success", Label.layout success)]]
	     | CCall {args, frameInfo, func, return} =>
		  seq [str "CCall ",
		       record
		       [("args", Vector.layout Operand.layout args),
			("frameInfo", Option.layout FrameInfo.layout frameInfo),
			("func", CFunction.layout func),
			("return", Option.layout Label.layout return)]]
	     | Call {label, live, return} => 
		  seq [str "Call ", 
		       record [("label", Label.layout label),
			       ("live", Vector.layout Operand.layout live),
			       ("return", Option.layout 
				(fn {return, handler, size} =>
				 record [("return", Label.layout return),
					 ("handler",
					  Option.layout Label.layout handler),
					 ("size", Int.layout size)])
				return)]]
	     | Goto l => seq [str "Goto ", Label.layout l]
	     | Raise => str "Raise"
	     | Return => str "Return "
	     | Switch s => Switch.layout s
	 end

       fun foldOperands (t, ac, f) =
 	 case t of
 	    Arith {args, dst, ...} => Vector.fold (args, f (dst, ac), f)
 	  | CCall {args, ...} => Vector.fold (args, ac, f)
 	  | Switch s =>
	       Switch.foldLabelUse
	       (s, ac, {label = fn (_, a) => a,
			use = f})
 	  | _ => ac

       fun foldDefs (t, a, f) =
	 case t of
	    Arith {dst, ...} => f (dst, a)
	  | _ => a
   end

structure Kind =
   struct
      datatype t =
	 Cont of {args: Operand.t vector,
		  frameInfo: FrameInfo.t}
       | CReturn of {dst: Operand.t option,
		     frameInfo: FrameInfo.t option,
		     func: CFunction.t}
       | Func
       | Handler of {frameInfo: FrameInfo.t,
		     handles: Operand.t vector}
       | Jump

      fun layout k =
	 let
	    open Layout
	 in
	    case k of
	       Cont {args, frameInfo} =>
		  seq [str "Cont ",
		       record [("args", Vector.layout Operand.layout args),
			       ("frameInfo", FrameInfo.layout frameInfo)]]
	     | CReturn {dst, frameInfo, func} =>
		  seq [str "CReturn ",
		       record
		       [("dst", Option.layout Operand.layout dst),
			("frameInfo", Option.layout FrameInfo.layout frameInfo),
			("func", CFunction.layout func)]]
	     | Func => str "Func"
	     | Handler {frameInfo, handles} =>
		  seq [str "Handler ",
		       record [("frameInfo", FrameInfo.layout frameInfo),
			       ("handles",
				Vector.layout Operand.layout handles)]]
	     | Jump => str "Jump"
	 end

      val frameInfoOpt =
	 fn Cont {frameInfo, ...} => SOME frameInfo
	  | CReturn {frameInfo, ...} => frameInfo
	  | Handler {frameInfo, ...} => SOME frameInfo
	  | _ => NONE
   end

structure Block =
   struct
      datatype t = T of {kind: Kind.t,
			 label: Label.t,
			 live: Operand.t vector,
			 raises: Operand.t vector option,
			 returns: Operand.t vector option,
			 statements: Statement.t vector,
			 transfer: Transfer.t}

      fun clear (T {label, ...}) = Label.clear label

      local
	 fun make g (T r) = g r
      in
	 val kind = make #kind
	 val label = make #label
      end

      fun layout (T {kind, label, live, raises, returns, statements, transfer}) =
	 let
	    open Layout
	 in
	    align [seq [Label.layout label, 
			str ": ",
			record [("kind", Kind.layout kind),
				("live", Vector.layout Operand.layout live),
				("raises",
				 Option.layout (Vector.layout Operand.layout)
				 raises),
				("returns",
				 Option.layout (Vector.layout Operand.layout)
				 returns)]],
		   indent (align
			   [align (Vector.toListMap
				   (statements, Statement.layout)),
			    Transfer.layout transfer],
			   4)]
	 end

      fun layouts (block, output' : Layout.t -> unit) = output' (layout block)

      fun foldDefs (T {kind, statements, transfer, ...}, a, f) =
	 let
	    val a =
	       case kind of
		  Kind.CReturn {dst, ...} =>
		     (case dst of
			 NONE => a
		       | SOME z => f (z, a))
		| _ => a
	    val a =
	       Vector.fold (statements, a, fn (s, a) =>
			    Statement.foldDefs (s, a, f))
	    val a = Transfer.foldDefs (transfer, a, f)
	 in
	    a
	 end
   end

structure Chunk =
   struct
      datatype t = T of {blocks: Block.t vector,
			 chunkLabel: ChunkLabel.t,
			 regMax: Runtime.Type.t -> int}

      fun layout (T {blocks, ...}) =
	 let
	    open Layout
	 in
	    align (Vector.toListMap (blocks, Block.layout))
	 end

      fun layouts (c as T {blocks, ...}, output : Layout.t -> unit) =
	 Vector.foreach (blocks, fn block => Block.layouts (block, output))

      fun clear (T {blocks, ...}) =
	 Vector.foreach (blocks, Block.clear)
   end

structure ProfileInfo =
   struct
      datatype t =
	 T of {frameSources: int vector,
	       labels: {label: ProfileLabel.t,
			sourceSeqsIndex: int} vector,
	       sourceSeqs: int vector vector,
	       sourceSuccessors: int vector,
	       sources: SourceInfo.t vector}

      fun clear (T {labels, ...}) =
	 Vector.foreach (labels, ProfileLabel.clear o #label)

      fun layout (T {frameSources, labels, sourceSeqs, sourceSuccessors,
		     sources}) =
	 Layout.record
	 [("frameSources", Vector.layout Int.layout frameSources),
	  ("labels",
	   Vector.layout (fn {label, sourceSeqsIndex} =>
			  Layout.record
			  [("label", ProfileLabel.layout label),
			   ("sourceSeqsIndex",
			    Int.layout sourceSeqsIndex)])
	   labels),
	  ("sourceSeqs", Vector.layout (Vector.layout Int.layout) sourceSeqs),
	  ("sourceSuccessors", Vector.layout Int.layout sourceSuccessors),
	  ("sources", Vector.layout SourceInfo.layout sources)]

      fun layouts (pi, output) = output (layout pi)

      fun isOK (T {frameSources,
		   labels,
		   sourceSeqs,
		   sourceSuccessors,
		   sources}): bool =
	 let
	    val sourceSeqsLength = Vector.length sourceSeqs
	    val sourcesLength = Vector.length sources
	 in
	    !Control.profile = Control.ProfileNone
	    orelse
	    (true
	     andalso (Vector.forall
		      (frameSources, fn i =>
		       0 <= i andalso i < sourceSeqsLength))
	     andalso (Vector.forall
		      (labels, fn {sourceSeqsIndex = i, ...} =>
		       0 <= i andalso i < sourceSeqsLength)))
	     andalso (Vector.forall
		      (sourceSeqs, fn v =>
		       Vector.forall
		       (v, fn i => 0 <= i andalso i < sourcesLength)))
	     andalso (Vector.length sourceSuccessors = Vector.length sources)
	     andalso (Vector.forall
		      (sourceSuccessors, fn i =>
		       0 <= i andalso i < sourceSeqsLength))
	 end

       fun modify (T {frameSources, labels, sourceSeqs, sourceSuccessors, sources}) :
	          {newProfileLabel: ProfileLabel.t -> ProfileLabel.t,
		   delProfileLabel: ProfileLabel.t -> unit,
		   getProfileInfo: unit -> t} =
	  let
	     val {get: ProfileLabel.t -> int, set, ...} =
	        Property.getSet
		(ProfileLabel.plist, 
		 Property.initRaise ("ProfileInfo.extend", ProfileLabel.layout))
	     val _ =
	        Vector.foreach
		(labels, fn {label, sourceSeqsIndex} =>
		 set (label, sourceSeqsIndex))
	     val new = ref []
	     fun newProfileLabel l =
	       let
		  val i = get l
		  val l' = ProfileLabel.new ()
		  val _ = set (l', i)
		  val _ = List.push (new, {label = l', sourceSeqsIndex = i})
	       in
		  l'
	       end
	     fun delProfileLabel l = set (l, ~1)
	     fun getProfileInfo () =
	        let
		   val labels = Vector.concat
		                [labels, Vector.fromList (!new)]
		   val labels = Vector.keepAll
		                (labels, fn {label, ...} =>
				 get label <> ~1)
		   val pi = T {frameSources = frameSources,
			       labels = Vector.concat
			                [labels, Vector.fromList (!new)],
			       sourceSeqs = sourceSeqs,
			       sourceSuccessors = sourceSuccessors,
			       sources = sources}
		in
		  Assert.assert ("newProfileInfo", fn () => isOK pi);
		  pi
		end
	  in
	     {newProfileLabel = newProfileLabel,
	      delProfileLabel = delProfileLabel,
	      getProfileInfo = getProfileInfo}
	  end
   end

structure Program =
   struct
      datatype t = T of {chunks: Chunk.t list,
			 frameLayouts: {frameOffsetsIndex: int,
					isC: bool,
					size: int} vector,
			 frameOffsets: int vector vector,
			 handlesSignals: bool,
			 intInfs: (Global.t * string) list,
			 main: {chunkLabel: ChunkLabel.t,
				label: Label.t},
			 maxFrameSize: int,
			 objectTypes: ObjectType.t vector,
			 profileInfo: ProfileInfo.t,
			 reals: (Global.t * string) list,
			 strings: (Global.t * string) list}

      fun clear (T {chunks, profileInfo, ...}) =
	 (List.foreach (chunks, Chunk.clear)
	  ; ProfileInfo.clear profileInfo)

      fun frameSize (T {frameLayouts, ...},
		     FrameInfo.T {frameLayoutsIndex, ...}) =
	 #size (Vector.sub (frameLayouts, frameLayoutsIndex))

      fun layouts (p as T {chunks, frameLayouts, frameOffsets, handlesSignals,
			   main = {label, ...},
			   maxFrameSize, objectTypes, profileInfo, ...},
		   output': Layout.t -> unit) =
	 let
	    open Layout
	    val output = output'
	 in
	    output (record
		    [("handlesSignals", Bool.layout handlesSignals),
		     ("main", Label.layout label),
		     ("maxFrameSize", Int.layout maxFrameSize),
		     ("frameOffsets",
		      Vector.layout (Vector.layout Int.layout) frameOffsets),
		     ("frameLayouts",
		      Vector.layout (fn {frameOffsetsIndex, isC, size} =>
				     record [("frameOffsetsIndex",
					      Int.layout frameOffsetsIndex),
					     ("isC", Bool.layout isC),
					     ("size", Int.layout size)])
		      frameLayouts)])
	    ; output (str "\nProfileInfo:")
	    ; ProfileInfo.layouts (profileInfo, output)
	    ; output (str "\nObjectTypes:")
	    ; Vector.foreachi (objectTypes, fn (i, ty) =>
			       output (seq [str "pt_", Int.layout i,
					    str " = ", ObjectType.layout ty]))
	    ; output (str "\n")
            ; List.foreach (chunks, fn chunk => Chunk.layouts (chunk, output))
	 end

      structure Alloc =
	 struct
	    datatype t = T of Operand.t list

	    fun layout (T zs) = List.layout Operand.layout zs
	       
	    val new = T

	    fun forall (T zs, f) = List.forall (zs, f)
	       
	    fun define (T zs, z) =
	       if (case z of
		      Operand.Global _ => true
		    | Operand.Register _ => true
		    | Operand.StackOffset _ => true
		    | _ => false)
		  then T (z :: zs)
	       else T zs

	    fun doesDefine (T zs, z): bool =
	       case List.peek (zs, fn z' => Operand.interfere (z, z')) of
		  NONE => false
		| SOME z' => Operand.equals (z, z')

	    val doesDefine =
	       Trace.trace2 ("Alloc.doesDefine", layout, Operand.layout,
			     Bool.layout)
	       doesDefine

	    fun peekOffset (T zs, i: int): {offset: int,
					    ty: Type.t} option =
	       List.peekMap
	       (zs, fn Operand.StackOffset (ot as {offset, ...}) =>
		          if i = offset
			     then SOME ot
			  else NONE
		     | _ => NONE)
	 end
      
      fun typeCheck (program as
		     T {chunks, frameLayouts, frameOffsets, intInfs, main,
			maxFrameSize, objectTypes,
			profileInfo as ProfileInfo.T {frameSources,
						      labels = profileLabels,
						      ...},
			reals, strings, ...}) =
	 let
	    val _ =
	       if !Control.profile = Control.ProfileTime
		  then
		     List.foreach
		     (chunks, fn Chunk.T {blocks, ...} =>
		      Vector.foreach
		      (blocks, fn Block.T {kind, label, statements, ...} =>
		       if (case kind of
			      Kind.Func => true
			    | _ => false)
			  orelse (0 < Vector.length statements
				  andalso (case Vector.sub (statements, 0) of
					      Statement.ProfileLabel _ => true
					    | _ => false))
			  then ()
		       else print (concat ["missing profile info: ",
					   Label.toString label, "\n"])))
	       else ()
	    val _ =
	       Err.check
	       ("frameSources length",
		fn () => (Vector.length frameSources
			  = (if !Control.profile <> Control.ProfileNone
				then Vector.length frameLayouts
			     else 0)),
		fn () => ProfileInfo.layout profileInfo)
	    val {get = profileLabelCount, ...} =
	       Property.get
	       (ProfileLabel.plist, Property.initFun (fn _ => ref 0))
	    val _ =
	       Vector.foreach (profileLabels, fn {label, ...} =>
			       let
				  val r = profileLabelCount label
			       in
				  case !r of
				     0 => r := 1
				   | _ => Error.bug "duplicate profile label"
			       end)
	    fun getFrameInfo (FrameInfo.T {frameLayoutsIndex, ...}) =
	       Vector.sub (frameLayouts, frameLayoutsIndex)
	    fun boolToUnitOpt b = if b then SOME () else NONE
	    val _ =
	       Vector.foreach
	       (frameLayouts, fn {frameOffsetsIndex, size, ...} =>
		Err.check
		("frameLayouts",
		 fn () => (0 <= frameOffsetsIndex
			   andalso frameOffsetsIndex < Vector.length frameOffsets
			   andalso size <= maxFrameSize
			   andalso size <= Runtime.maxFrameSize
			   andalso 0 = Int.rem (size, 4)),
		 fn () => Layout.record [("frameOffsetsIndex",
					  Int.layout frameOffsetsIndex),
					 ("size", Int.layout size)]))
	    val _ =
	       Vector.foreach
	       (objectTypes, fn ty =>
		Err.check ("objectType",
			   fn () => ObjectType.isOk ty,
			   fn () => ObjectType.layout ty))
	    fun tyconTy (pt: PointerTycon.t): ObjectType.t =
	       Vector.sub (objectTypes, PointerTycon.index pt)
	    open Layout
	    fun globals (name, gs, ty) =
	       List.foreach
	       (gs, fn (g, s) =>
		Err.check
		(concat ["global ", name],
		 fn () => Type.equals (ty, Global.ty g),
		 fn () => seq [String.layout s, str ": ", Type.layout ty]))
	    val _ = globals ("real", reals, Type.real)
	    val _ = globals ("intInf", intInfs, Type.intInf)
	    val _ = globals ("string", strings, Type.string)
	    (* Check for no duplicate labels. *)
	    local
	       val {get, ...} =
		  Property.get (Label.plist,
				Property.initFun (fn _ => ref false))
	    in
	       val _ =
		  List.foreach
		  (chunks, fn Chunk.T {blocks, ...} =>
		   Vector.foreach
		   (blocks, fn Block.T {label, ...} =>
		    let
		       val r = get label
		    in
		       if !r
			  then Error.bug "duplicate label"
		       else r := true
		    end))
	    end
	    val {get = labelBlock: Label.t -> Block.t,
		 set = setLabelBlock, ...} =
	       Property.getSetOnce (Label.plist,
				    Property.initRaise ("block", Label.layout))
	    val _ =
	       List.foreach
	       (chunks, fn Chunk.T {blocks, ...} =>
		Vector.foreach
		(blocks, fn b as Block.T {label, ...} =>
		 setLabelBlock (label, b)))
	    fun checkOperand (x: Operand.t, alloc: Alloc.t): unit =
	       let
		  datatype z = datatype Operand.t
		  fun ok () =
		     case x of
			ArrayOffset (z as {base, index, ...}) =>
			   (checkOperand (base, alloc)
			    ; checkOperand (index, alloc)
			    ; arrayOffsetIsOk z)
		      | Cast (z, t) =>
			   (checkOperand (z, alloc)
			    ; (castIsOk
			       {from = Operand.ty z,
				fromInt = (case z of
					      Int i => SOME i
					    | _ => NONE),
				to = t,
				tyconTy = tyconTy}))
		      | Char _ => true
		      | Contents {oper, ...} =>
			   (checkOperand (oper, alloc)
			    ; Type.equals (Operand.ty oper,
					   Type.cpointer))
		      | File => true
		      | GCState => true
		      | Global _ =>
			   (* For now, we don't check that globals are
			    * defined, because they aren't captured by
			    * liveness info.
			    *)
			   true
			   orelse Alloc.doesDefine (alloc, x)
		      | Int _ => true
		      | Label l => (labelBlock l; true)
		      | Line => true
		      | Offset (z as {base, ...}) =>
			   (checkOperand (base, alloc)
			    ; offsetIsOk z)
		      | Real _ => true
		      | Register _ => Alloc.doesDefine (alloc, x)
		      | Runtime _ => true
		      | SmallIntInf w => 0wx1 = Word.andb (w, 0wx1)
		      | StackOffset {offset, ty, ...} =>
			   offset + Type.size ty <= maxFrameSize
			   andalso Alloc.doesDefine (alloc, x)
		      | Word _ => true
	       in
		  Err.check ("operand", ok, fn () => Operand.layout x)
	       end
	    and arrayOffsetIsOk {base, index, ty} =
	       Type.equals (Operand.ty index, Type.int)
	       andalso
	       case Operand.ty base of
		  Type.CPointer => true (* needed for card marking *)
		| Type.EnumPointers {enum, pointers} =>
		     0 = Vector.length enum
		     andalso
		     Vector.forall
		     (pointers, fn p =>
		      case tyconTy p of
			 ObjectType.Array
			 (MemChunk.T {components, ...}) =>
			    1 = Vector.length components
			    andalso
			    let
			       val {offset, ty = ty', ...} =
				  Vector.sub (components, 0)
			    in
			       offset = 0
			       andalso Type.equals (ty, ty')
			    end
		       | _ => false)
		| _ => false
	    and offsetIsOk {base, offset, ty} =
	       let
		  fun memChunkIsOk (MemChunk.T {components, ...}) =
		     case (Vector.peek
			   (components, fn {offset = offset', ...} =>
			    offset = offset')) of
			NONE => false
		      | SOME {ty = ty', ...} => Type.equals (ty, ty')
				  
	       in
		  case Operand.ty base of
		     Type.EnumPointers {enum, pointers} =>
			0 = Vector.length enum
			andalso
			((* Vector_fromArray header update. *)
			 (offset = Runtime.headerOffset
			  andalso Type.equals (ty, Type.word))
			 orelse
			 Vector.forall
			 (pointers, fn p =>
			  case tyconTy p of
			     ObjectType.Normal m => memChunkIsOk m
			   | _ => false))
		   | Type.MemChunk m => memChunkIsOk m
		   | _ => false
	       end
	    fun checkOperands (v, a) =
	       Vector.foreach (v, fn z => checkOperand (z, a))
	    fun check' (x, name, isOk, layout) =
	       Err.check (name, fn () => isOk x, fn () => layout x)
	    val labelKind = Block.kind o labelBlock
	    fun labelIsJump (l: Label.t): bool =
	       case labelKind l of
		  Kind.Jump => true
		| _ => false
	    fun checkKind (k: Kind.t, alloc: Alloc.t): Alloc.t option =
	       let
		  datatype z = datatype Kind.t
		  exception No
		  fun frame (FrameInfo.T {frameLayoutsIndex},
			     useSlots: bool,
			     isC: bool): bool =
		     let
			val {frameOffsetsIndex, isC = isC', ...} =
			   Vector.sub (frameLayouts, frameLayoutsIndex)
			   handle Subscript => raise No
		     in
			isC = isC'
			andalso
			(not useSlots
			 orelse
			 let
			    val Alloc.T zs = alloc
			    val liveOffsets =
			       List.fold
			       (zs, [], fn (z, liveOffsets) =>
				case z of
				   Operand.StackOffset {offset, ty} =>
				      if Type.isPointer ty
					 then offset :: liveOffsets
				      else liveOffsets
				 | _ => raise No)
			    val liveOffsets =
			       Vector.fromArray
			       (QuickSort.sortArray
				(Array.fromList liveOffsets, op <=))
			    val liveOffsets' =
			       Vector.sub (frameOffsets, frameOffsetsIndex)
			       handle Subscript => raise No
			 in
			    liveOffsets = liveOffsets'
			 end)
		     end handle No => false
		  fun slotsAreInFrame (fi: FrameInfo.t): bool =
		     let
			val {size, ...} = getFrameInfo fi
		     in
			Alloc.forall
			(alloc, fn z =>
			 case z of
			    Operand.StackOffset {offset, ty} =>
			       offset + Type.size ty <= size
			  | _ => false)
		     end
	       in
		  case k of
		     Cont {args, frameInfo} =>
			if frame (frameInfo, true, false)
			   andalso slotsAreInFrame frameInfo
			   then SOME (Vector.fold
				      (args, alloc, fn (z, alloc) =>
				       Alloc.define (alloc, z)))
			else NONE
		   | CReturn {dst, frameInfo, func, ...} =>
			if (if CFunction.mayGC func
			       then (case frameInfo of
					NONE => false
				      | SOME fi => (frame (fi, true, true)
						    andalso slotsAreInFrame fi))
			    else if !Control.profile = Control.ProfileNone
				    then true
				 else (case frameInfo of
					  NONE => false
					| SOME fi => frame (fi, false, true)))
			   then SOME (case dst of
					 NONE => alloc
				       | SOME z => Alloc.define (alloc, z))
			else NONE
		   | Func => SOME alloc
		   | Handler {frameInfo, ...} =>
			if frame (frameInfo, false, false)
			   then SOME alloc
			else NONE
		   | Jump => SOME alloc
	       end
	    fun checkStatement (s: Statement.t, alloc: Alloc.t)
	       : Alloc.t option =
	       let
		  datatype z = datatype Statement.t
	       in
		  case s of
		     Move {dst, src} =>
			let
			   val _ = checkOperand (src, alloc)
			   val alloc = Alloc.define (alloc, dst)
			   val _ = checkOperand (dst, alloc)
			in
			   if Type.equals (Operand.ty dst, Operand.ty src)
			      andalso Operand.isLocation dst
			      then SOME alloc
			   else NONE
			end
		   | Noop => SOME alloc
		   | Object {dst, header, size, stores} =>
			let
			   val _ =
			      Vector.foreach
			      (stores, fn {value, ...} =>
			       checkOperand (value, alloc))
			   val alloc = Alloc.define (alloc, dst)
			   val _ = checkOperand (dst, alloc)
			in
			   (case Vector.sub (objectTypes,
					     Runtime.headerToTypeIndex
					     header) of
			       ObjectType.Normal mc =>
				  (if MemChunk.isValidInit
				      (mc, 
				       Vector.map
				       (stores, fn {offset, value} =>
					{offset = offset,
					 ty = Operand.ty value}))
				      then SOME alloc
				   else NONE)
			     | _ => NONE)
			       handle Subscript => NONE
			end
		   | PrimApp {args, dst, prim} =>
			let
			   val _ = checkOperands (args, alloc)
			in
			   case dst of
			      NONE => SOME alloc
			    | SOME z =>
				 let
				    val alloc = Alloc.define (alloc, z)
				    val _ = checkOperand (z, alloc)
				 in
				    SOME alloc
				 end
			end
		   | ProfileLabel l =>
			if !Control.profile = Control.ProfileTime
			   then let
				   val r = profileLabelCount l
				in
				   case !r of
				      1 => (r := 2; SOME alloc)
				    | _ => NONE
				end
			else SOME alloc
	       end
	    fun liveIsOk (live: Operand.t vector,
			  a: Alloc.t): bool =
	       Vector.forall (live, fn z => Alloc.doesDefine (a, z))
	    fun liveSubset (live: Operand.t vector,
			    live': Operand.t vector): bool =
	       Vector.forall
	       (live, fn z =>
		Vector.exists (live', fn z' =>
			       Operand.equals (z, z')))
	    fun goto (Block.T {live,
			       raises = raises',
			       returns = returns', ...},
		      raises,
		      returns,
		      alloc: Alloc.t): bool =
	       liveIsOk (live, alloc)
	       andalso
	       (case (raises, raises') of
		   (_, NONE) => true
		 | (SOME gs, SOME gs') =>
		      Vector.equals (gs, gs', Operand.equals)
		 | _ => false)
		   andalso
		   (case (returns, returns') of
		       (_, NONE) => true
		     | (SOME os, SOME os') =>
			  Vector.equals (os, os', Operand.equals)
		     | _ => false)
	    fun checkCont (cont: Label.t, size: int, alloc: Alloc.t) =
	       let
		  val Block.T {kind, live, ...} = labelBlock cont
	       in
		  if Vector.forall (live, fn z => Alloc.doesDefine (alloc, z))
		     then
			(case kind of
			    Kind.Cont {args, frameInfo, ...} =>
			       (if size = #size (getFrameInfo frameInfo)
				   then
				      SOME
				      (live,
				       SOME
				       (Vector.map
					(args, fn z =>
					 case z of
					    Operand.StackOffset {offset, ty} =>
					       Operand.StackOffset
					       {offset = offset - size,
						ty = ty}
					  | _ => z)))
				else NONE)
			  | _ => NONE)
		  else NONE
	       end
	    fun callIsOk {alloc: Alloc.t,
			  dst: Label.t,
			  live,
			  raises,
			  return,
			  returns} =
	       let
		  val {raises, returns, size} =
		     case return of
			NONE =>
			   {raises = raises,
			    returns = returns,
			    size = 0}
		      | SOME {handler, return, size} =>
			   let
			      val (contLive, returns) =
				 Err.check'
				 ("cont",
				  fn () => checkCont (return, size, alloc),
				  fn () => Label.layout return)
			      fun checkHandler () =
				 case handler of
				    NONE => SOME raises
				  | SOME h =>
				       let
					  val Block.T {kind, live, ...} =
					     labelBlock h
				       in
					  if liveSubset (live, contLive)
					     then
						(case kind of
						    Kind.Handler {handles, ...} =>
						       SOME (SOME handles)
						  | _ => NONE)
					  else NONE
				       end
			      val raises =
				 Err.check'
				 ("handler", checkHandler,
				  fn () => Option.layout Label.layout handler)
			   in
			      {raises = raises,
			       returns = returns,
			       size = size}
			   end
		  val b as Block.T {kind, ...} = labelBlock dst
		  val alloc =
		     Alloc.T
		     (Vector.fold
		      (live, [], fn (z, ac) =>
		       case z of
			  Operand.StackOffset {offset, ty} =>
			     if offset < size
				then ac
			     else (Operand.StackOffset
				   {offset = offset - size,
				    ty = ty} :: ac)
			| _ => ac))
	       in
		  goto (b, raises, returns, alloc)
	       end
	    fun transferOk
	       (t: Transfer.t,
		raises: Operand.t vector option,
		returns: Operand.t vector option,
		alloc: Alloc.t): bool =
	       let
		  fun jump (l: Label.t, a: Alloc.t) =
		     let
			val b as Block.T {kind, ...} = labelBlock l
		     in
			(case kind of
			    Kind.Jump => true
			  | _ => false)
			    andalso goto (b, raises, returns, a)
		     end
		  datatype z = datatype Transfer.t
	       in
		  case t of
		     Arith {args, dst, overflow, prim, success, ty} =>
			let
			   val _ = checkOperands (args, alloc)
			   val alloc = Alloc.define (alloc, dst)
			   val _ = checkOperand (dst, alloc)
			in
			   Type.equals (ty, Operand.ty dst)
			   andalso jump (overflow, alloc)
			   andalso jump (success, alloc)
			end
		   | CCall {args, frameInfo = fi, func, return} =>
			let
			   val _ = checkOperands (args, alloc)
			in
			   case return of
			      NONE => true
			    | SOME l =>
				 let 
				    val Block.T {kind, live, ...} =
				       labelBlock l
				 in
				    liveIsOk (live, alloc)
				    andalso
				    case labelKind l of
				       Kind.CReturn
				       {dst, frameInfo = fi', func = f, ...} => 
					  CFunction.equals (func, f)
					  andalso (Option.equals
						   (fi, fi', FrameInfo.equals))
					  andalso
					  (case (dst, CFunction.returnTy f) of
					      (NONE, _) => true
					    | (SOME x, SOME ty) =>
						 Runtime.Type.equals
						 (ty,
						  Type.toRuntime
						  (Operand.ty x))
					    | _ => false)
				     | _ => false
				 end
			end
		   | Call {label, live, return} =>
			Vector.forall
			(live, fn z => Alloc.doesDefine (alloc, z))
			andalso
			callIsOk {alloc = alloc,
				  dst = label,
				  live = live,
				  raises = raises,
				  return = return,
				  returns = returns}
		      | Goto l => jump (l, alloc)
		      | Raise =>
			   (case raises of
			       NONE => false
			     | SOME zs =>
				  Vector.forall (zs, fn z =>
						 Alloc.doesDefine
						 (alloc, z)))
		      | Return =>
			   (case returns of
			       NONE => false
			     | SOME zs =>
				  Vector.forall
				  (zs, fn z =>
				   Alloc.doesDefine (alloc, z)))
		      | Switch s =>
			   Switch.isOk
			   (s, {checkUse = fn z => checkOperand (z, alloc),
				labelIsOk = fn l => jump (l, alloc)})
	       end
	    val transferOk =
	       Trace.trace
	       ("transferOk",
		fn (t, _, _, a) =>
		Layout.tuple [Transfer.layout t, Alloc.layout a],
		Bool.layout)
	       transferOk
	    fun blockOk (Block.T {kind, label, live, raises, returns, statements,
				  transfer, ...}): bool =
	       let
		  val live = Vector.toList live
		  val _ =
		     Err.check
		     ("live",
		      fn () =>
		      let
			 fun loop zs =
			    case zs of
			       [] => true
			     | z :: zs =>
				  List.forall
				  (zs, fn z' =>
				   not (Operand.interfere (z, z')))
		      in
			 loop live
		      end,
		      fn () => List.layout Operand.layout live)
		  val alloc = Alloc.new live
		  val alloc =
		     Err.check'
		     ("kind",
		      fn () => checkKind (kind, alloc),
		      fn () => Kind.layout kind)
		  val alloc =
		     Vector.fold
		     (statements, alloc, fn (s, alloc) =>
		      Err.check'
		      ("statement",
		       fn () => checkStatement (s, alloc),
		       fn () => Statement.layout s))
		  val _ =
		     Err.check
		     ("transfer",
		      fn () => transferOk (transfer, raises, returns, alloc),
		      fn () => Transfer.layout transfer)
	       in
		  true
	       end
	    val _ =
	       List.foreach
	       (chunks,
		fn Chunk.T {blocks, ...} =>
		let
		in
		   Vector.foreach
		   (blocks, fn b =>
		    check' (b, "block", blockOk, Block.layout))
		end)
	    val _ = clear program
	 in
	    ()
	 end handle Err.E e => (Layout.outputl (Err.layout e, Out.error)
				; Error.bug "Machine type error")
   end

end
