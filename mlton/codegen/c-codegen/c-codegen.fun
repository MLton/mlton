(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor CCodegen (S: C_CODEGEN_STRUCTS): C_CODEGEN =
struct

open S

local
   open Machine
in
   structure Block = Block
   structure Chunk = Chunk
   structure ChunkLabel = ChunkLabel
   structure FrameInfo = FrameInfo
   structure Global = Global
   structure Kind = Kind
   structure Label = Label
   structure ObjectType = ObjectType
   structure Operand = Operand
   structure Prim = Prim
   structure ProfileInfo = ProfileInfo
   structure ProfileLabel = ProfileLabel
   structure Program = Program
   structure Register = Register
   structure Runtime = Runtime
   structure SourceInfo = SourceInfo
   structure Statement = Statement
   structure Switch = Switch
   structure Transfer = Transfer
   structure Type = Type
end

local
   open Runtime
in
   structure CFunction = CFunction
   structure GCField = GCField
end

structure Kind =
   struct
      open Kind

      fun isEntry (k: t): bool =
	 case k of
	    Cont _ => true
	  | CReturn {func, ...} => CFunction.mayGC func
	  | Func => true
	  | Handler _ => true
	  | _ => false
   end

val traceGotoLabel = Trace.trace ("gotoLabel", Label.layout, Unit.layout) 

val overhead = "**C overhead**"
   
structure C =
   struct
      val truee = "TRUE"
      val falsee = "FALSE"

      fun bool b = if b then truee else falsee
	 
      fun args (ss: string list): string
	 = concat ("(" :: List.separate (ss, ", ") @ [")"])
         
      fun callNoSemi (f: string, xs: string list, print: string -> unit): unit 
	 = (print f
	    ; print "("
	    ; (case xs 
		  of [] => ()
		| x :: xs => (print x
			      ; List.foreach (xs, 
					     fn x => (print ", "; print x))))
	    ; print ")")

      fun call (f, xs, print) =
	 (callNoSemi (f, xs, print)
	  ; print ";\n")

      fun int (n: int): string =
	 if n >= 0
	    then Int.toString n
	 else if n = Int.minInt
		 then "(int)0x80000000" (* because of goofy gcc warning *)
	      else concat ["-", String.dropPrefix (Int.toString n, 1)]

      fun char (c: char) =
	 concat [if Char.ord c >= 0x80 then "(uchar)" else "",
		 "'", Char.escapeC c, "'"]

      fun word (w: Word.t) = "0x" ^ Word.toString w

      (* The only difference between SML reals and C floats/doubles is that
       * SML uses "~" while C uses "-".
       *)
      fun real s = String.translate (s, fn #"~" => "-" | c => String.fromChar c)

      fun string s =
	 let val quote = "\""
	 in concat [quote, String.escapeC s, quote]
	 end

      fun bug (s: string, print) =
	 call ("MLton_bug", [concat ["\"", String.escapeC s, "\""]], print)

      fun push (i, print) =
	 call ("\tPush", [int i], print)

      fun move ({dst, src}, print) =
	 print (concat [dst, " = ", src, ";\n"])
   end

structure Label =
   struct
      open Label

      fun toStringIndex l = (toString l) ^ "_index"
   end

structure Operand =
   struct
      open Operand
	 

      val layout = Layout.str o toString
   end

fun creturn (t: Runtime.Type.t): string =
   concat ["CReturn", Runtime.Type.name t]

fun outputDeclarations
   {additionalMainArgs: string list,
    includes: string list,
    name: string,
    print: string -> unit,
    program = (Program.T
	       {chunks, frameLayouts, frameOffsets, intInfs, maxFrameSize,
		objectTypes,
		profileInfo,
		reals, strings, ...}),
    rest: unit -> unit
    }: unit =
   let
      fun outputIncludes () =
	 (List.foreach (includes, fn i => (print "#include <";
					   print i;
					   print ">\n"))
	  ; print "\n")
      fun declareGlobals () =
	 C.call ("Globals",
		 List.map (List.map (let open Runtime.Type
				     in [char, double, int, pointer, uint]
				     end, 
				     Global.numberOfType)
			   @ [Global.numberOfNonRoot ()],
			   C.int),
		 print)
      fun declareIntInfs () =
	 (print "BeginIntInfs\n"
	  ; List.foreach (intInfs, fn (g, s) =>
			  (C.callNoSemi ("IntInf",
					 [C.int (Global.index g),
					  C.string s],
					 print)
			   ; print "\n"))
	  ; print "EndIntInfs\n")
      fun declareStrings () =
	 (print "BeginStrings\n"
	  ; List.foreach (strings, fn (g, s) =>
			  (C.callNoSemi ("String",
					 [C.int (Global.index g),
					  C.string s,
					  C.int (String.size s)],
					 print)
			   ; print "\n"))
	  ; print "EndStrings\n")
      fun declareReals () =
	 (print "BeginReals\n"
	  ; List.foreach (reals, fn (g, f) =>
			  (C.callNoSemi ("Real",
					 [C.int (Global.index g),
					  C.real f],
					 print)
			   ; print "\n"))
	  ; print "EndReals\n")
      fun declareFrameOffsets () =
	 Vector.foreachi
	 (frameOffsets, fn (i, v) =>
	  (print (concat ["static ushort frameOffsets", C.int i, "[] = {"])
	   ; print (C.int (Vector.length v))
	   ; Vector.foreach (v, fn i => (print ","; print (C.int i)))
	   ; print "};\n"))
      fun declareArray (ty: string,
			name: string,
			v: 'a vector,
			toString: int * 'a -> string) =
	 (print (concat ["static ", ty, " ", name, "[] = {\n"])
	  ; Vector.foreachi (v, fn (i, x) =>
			     print (concat ["\t", toString (i, x), ",\n"]))
	  ; print "};\n")
      fun declareFrameLayouts () =
	 declareArray ("GC_frameLayout", "frameLayouts", frameLayouts,
		       fn (_, {frameOffsetsIndex, isC, size}) =>
		       concat ["{",
			       C.bool isC,
			       ", ", C.int size,
			       ", frameOffsets", C.int frameOffsetsIndex,
			       "}"])
      fun declareObjectTypes () =
	 declareArray
	 ("GC_ObjectType", "objectTypes", objectTypes,
	  fn (_, ty) =>
	  let
	     datatype z = datatype Runtime.ObjectType.t
	     val (tag, nonPointers, pointers) =
		case ObjectType.toRuntime ty of
		   Array {numBytesNonPointers, numPointers} =>
		      (0, numBytesNonPointers, numPointers)
		 | Normal {numPointers, numWordsNonPointers} =>
		      (1, numWordsNonPointers, numPointers)
		 | Stack =>
		      (2, 0, 0)
	  in
	     concat ["{ ", Int.toString tag, ", ",
		     Int.toString nonPointers, ", ",
		     Int.toString pointers, " }"]
	  end)
      fun declareMain () =
	 let
	    val magic = C.word (Random.useed ())
	 in 
	    C.callNoSemi ("Main",
			  [C.int (!Control.cardSizeLog2),
			   magic,
			   C.int maxFrameSize,
			   C.bool (!Control.mayLoadWorld),
			   C.bool (!Control.markCards),
			   C.bool (!Control.profileStack)]
			  @ additionalMainArgs,
			  print)
	    ; print "\n"
	 end
      fun declareProfileInfo () =
	 let
	    val ProfileInfo.T {frameSources, labels, sourceSeqs,
			       sourceSuccessors, sources} =
	       profileInfo
	 in
	    Vector.foreach (labels, fn {label, ...} =>
			    C.call ("DeclareProfileLabel",
				    [ProfileLabel.toString label],
				    print))
	    ; declareArray ("struct GC_sourceLabel", "sourceLabels", labels,
			    fn (_, {label, sourceSeqsIndex}) =>
			    concat ["{(pointer)", ProfileLabel.toString label,
				    ", ", C.int sourceSeqsIndex, "}"])
	    ; declareArray ("string", "sources", sources,
			    fn (_, si) =>
			    C.string (SourceInfo.toString' (si, "\t")))
	    ; Vector.foreachi (sourceSeqs, fn (i, v) =>
			       (print (concat ["static int sourceSeq",
					       Int.toString i,
					       "[] = {"])
				; print (C.int (Vector.length v))
				; Vector.foreach (v, fn i =>
						  (print (concat [",", C.int i])))
				; print "};\n"))
				      
	    ; declareArray ("uint", "*sourceSeqs", sourceSeqs, fn (i, _) =>
			    concat ["sourceSeq", Int.toString i])
	    ; declareArray ("uint", "frameSources", frameSources, C.int o #2)
	    ; declareArray ("uint", "sourceSuccessors", sourceSuccessors,
			    C.int o #2)
	 end
   in
      print (concat ["#define ", name, "CODEGEN\n\n"])
      ; outputIncludes ()
      ; declareGlobals ()
      ; declareIntInfs ()
      ; declareStrings ()
      ; declareReals ()
      ; declareFrameOffsets ()
      ; declareFrameLayouts ()
      ; declareObjectTypes ()
      ; declareProfileInfo ()
      ; rest ()
      ; declareMain ()
   end

fun output {program as Machine.Program.T {chunks,
					  frameLayouts,
					  main = {chunkLabel, label}, ...},
            includes,
	    outputC: unit -> {file: File.t,
			      print: string -> unit,
			      done: unit -> unit}} =
   let
      datatype status = None | One | Many
      val {get = labelInfo: Label.t -> {block: Block.t,
					chunkLabel: ChunkLabel.t,
					frameIndex: int option,
					status: status ref,
					layedOut: bool ref},
	   set = setLabelInfo, ...} =
	 Property.getSetOnce
	 (Label.plist, Property.initRaise ("CCodeGen.info", Label.layout))
      val entryLabels: (Label.t * int) list ref = ref []
      val indexCounter = Counter.new (Vector.length frameLayouts)
      val _ =
	 List.foreach
	 (chunks, fn Chunk.T {blocks, chunkLabel, ...} =>
	  Vector.foreach
	  (blocks, fn b as Block.T {kind, label, ...} =>
	   let
	      fun entry (index: int) =
		 List.push (entryLabels, (label, index))
	      val frameIndex = 
		 case Kind.frameInfoOpt kind of
		    NONE => (if Kind.isEntry kind
				then entry (Counter.next indexCounter)
			     else ()
		             ; NONE)
		  | SOME (FrameInfo.T {frameLayoutsIndex, ...}) =>
		       (entry frameLayoutsIndex
			; SOME frameLayoutsIndex)
	   in
	      setLabelInfo (label, {block = b,
				    chunkLabel = chunkLabel,
				    frameIndex = frameIndex,
				    layedOut = ref false,
				    status = ref None})
	   end))
      val entryLabels =
	 Vector.map
	 (Vector.fromArray
	  (QuickSort.sortArray
	   (Array.fromList (!entryLabels), fn ((_, i), (_, i')) => i <= i')),
	  #1)
      val labelChunk = #chunkLabel o labelInfo
      fun labelFrameInfo (l: Label.t): FrameInfo.t option =
	 let
	    val {block = Block.T {kind, ...}, ...} = labelInfo l
	 in
	    Kind.frameInfoOpt kind
	 end
      val {print, done, ...} = outputC ()
      fun declareChunks () =
	 List.foreach (chunks, fn Chunk.T {chunkLabel, ...} =>
		       C.call ("DeclareChunk",
			       [ChunkLabel.toString chunkLabel],
			       print))
      fun declareNextChunks () =
	 (print "static struct cont ( *nextChunks []) () = {"
	  ; Vector.foreach (entryLabels, fn l =>
			    let
			       val {chunkLabel, ...} = labelInfo l
			    in
			       print "\t"
			       ; C.callNoSemi ("Chunkp",
					       [ChunkLabel.toString chunkLabel],
					       print)
			       ; print ",\n"
			    end)
	  ; print "};\n")
      fun declareIndices () =
	 Vector.foreachi
	 (entryLabels, fn (i, l) =>
	  (print (concat ["#define ", Label.toStringIndex l, " ",
			  C.int i, "\n"])))
      local
	 datatype z = datatype Operand.t
      	 fun toString (z: Operand.t): string =
	    case z of
	       ArrayOffset {base, index, ty} =>
		  concat ["X", Type.name ty,
			  C.args [toString base, toString index]]
	     | Cast (z, ty) =>
		  concat ["(", Runtime.Type.toString (Type.toRuntime ty), ")",
			  toString z]
	     | Char c => C.char c
	     | Contents {oper, ty} =>
		  concat ["C", Type.name ty, "(", toString oper, ")"]
	     | File => "__FILE__"
	     | GCState => "&gcState"
	     | Global g =>
		  concat ["G", Type.name (Global.ty g),
			  if Global.isRoot g
			     then ""
			  else "NR",
			     "(", Int.toString (Global.index g), ")"]
	     | Int n => C.int n
	     | Label l => Label.toStringIndex l
	     | Line => "__LINE__"
	     | Offset {base, offset, ty} =>
		  concat ["O", Type.name ty,
			  C.args [toString base, C.int offset]]
	     | Real s => C.real s
	     | Register r =>
		  concat ["R", Type.name (Register.ty r),
			  "(", Int.toString (Register.index r), ")"]
	     | Runtime r =>
		  let
		     datatype z = datatype GCField.t
		  in
		     case r of
			CanHandle => "gcState.canHandle"
		      | CardMap => "gcState.cardMapForMutator"
		      | CurrentThread => "gcState.currentThread"
		      | ExnStack => "ExnStack"
		      | Frontier => "frontier"
		      | Limit => "gcState.limit"
		      | LimitPlusSlop => "gcState.limitPlusSlop"
		      | MaxFrameSize => "gcState.maxFrameSize"
		      | SignalIsPending => "gcState.signalIsPending"
		      | StackBottom => "gcState.stackBottom"
		      | StackLimit => "gcState.stackLimit"
		      | StackTop => "stackTop"
		  end
	     | SmallIntInf w =>
		  concat ["SmallIntInf", C.args [concat ["0x", Word.toString w]]]
	     | StackOffset {offset, ty} =>
		  concat ["S", Type.name ty, "(", C.int offset, ")"]
	     | Word w => C.word w
      in
	 val operandToString = toString
      end
   
      fun outputStatement s =
	 let
	    datatype z = datatype Statement.t
	 in
	    case s of
	       Noop => ()
	     | _ =>
		  (print "\t"
		   ; (case s of
			 Move {dst, src} =>
			    C.move ({dst = operandToString dst,
				     src = operandToString src},
				    print)
		       | Noop => ()
		       | Object {dst, header, size, stores} =>
			    (C.call ("Object", [operandToString dst,
						C.word header],
				     print)
			     ; print "\t"
			     ; (Vector.foreach
				(stores, fn {offset, value} =>
				 (C.call
				  (concat ["A", Type.name (Operand.ty value)],
				   [C.int offset, operandToString value], 
				   print)
				  ; print "\t")))
			     ; C.call ("EndObject", [C.int size], print))
		       | PrimApp {args, dst, prim} =>
			    let
			       val _ =
				  case dst of
				     NONE => ()
				   | SOME dst =>
					print
					(concat [operandToString dst, " = "])
			       fun doit () =
				  C.call
				  (Prim.toString prim,
				   Vector.toListMap (args, operandToString),
				   print)
			       val _ =
				  case Prim.name prim of
				     Prim.Name.FFI s =>
					(case Prim.numArgs prim of
					    NONE => print (concat [s, ";\n"])
					  | SOME _ => doit ())
				   | _ => doit ()
			    in 
			       ()
			    end
		       | ProfileLabel l =>
			    C.call ("ProfileLabel", [ProfileLabel.toString l],
				    print)
		       | SetExnStackLocal {offset} =>
			    C.call ("SetExnStackLocal", [C.int offset], print)
		       | SetExnStackSlot {offset} =>
			    C.call ("SetExnStackSlot", [C.int offset], print)
		       | SetSlotExnStack {offset} =>
			    C.call ("SetSlotExnStack", [C.int offset], print)
			    ))
	 end
      val profiling = !Control.profile <> Control.ProfileNone
      fun outputChunk (chunk as Chunk.T {chunkLabel, blocks, regMax, ...}) =
	 let
	    fun labelFrameSize (l: Label.t): int =
	       Program.frameSize (program, valOf (labelFrameInfo l))
	    (* Count how many times each label is jumped to. *)
	    fun jump l =
	       let
		  val {status, ...} = labelInfo l
	       in
		  case !status of
		     None => status := One
		   | One => status := Many
		   | Many => ()
	       end
	    fun force l = #status (labelInfo l) := Many
	    val _ =
		Vector.foreach
		(blocks, fn Block.T {kind, label, statements, transfer, ...} =>
		 let
		    val _ = if Kind.isEntry kind then jump label else ()
		    datatype z = datatype Transfer.t
		 in
		    case transfer of
		       Arith {overflow, success, ...} =>
			  (jump overflow; jump success)
		     | CCall {func, return, ...} =>
			  if CFunction.maySwitchThreads func
			     then ()
			  else Option.app (return, jump)
		     | Call {label, ...} => jump label
		     | Goto dst => jump dst
		     | Raise => ()
		     | Return => ()
		     | Switch s => Switch.foreachLabel (s, jump)
		 end)
	    fun push (return: Label.t, size: int) =
	       (print "\t"
		; C.move ({dst = operandToString
			   (Operand.StackOffset
			    {offset = size - Runtime.labelSize,
			     ty = Type.label return}),
			   src = operandToString (Operand.Label return)},
			  print)
		; C.push (size, print)
		; if profiling
		     then print "\tFlushStackTop();\n"
		  else ())
	    fun copyArgs (args: Operand.t vector): string list * (unit -> unit) =
	       if Vector.exists (args,
				 fn Operand.StackOffset _ => true
				  | _ => false)
		  then
		     let
			val _ = print "\t{\n"
			val c = Counter.new 0
			val args =
			   Vector.toListMap
			   (args, fn z =>
			    case z of
			       Operand.StackOffset {ty, ...} =>
				  let
				     val tmp =
					concat ["tmp",
						Int.toString (Counter.next c)]
				     val _ =
					print (concat
					       ["\t",
						Runtime.Type.toString
						(Type.toRuntime ty),
						" ", tmp,
						" = ", operandToString z,
						";\n"])
				  in
				     tmp
				  end
			     | _ => operandToString z)
		     in
			(args, fn () => print "\t}\n")
		     end
	       else (Vector.toListMap (args, operandToString),
		     fn () => ())
	    val tracePrintLabelCode =
	       Trace.trace
	       ("printLabelCode",
		fn {block, layedOut, status: status ref, ...} =>
		Layout.record [("block", Label.layout (Block.label block)),
			       ("layedOut", Bool.layout (!layedOut))],
		Unit.layout)
	    fun maybePrintLabel l =
	       if ! (#layedOut (labelInfo l))
		  then ()
	       else gotoLabel l
	    and gotoLabel arg =
	       traceGotoLabel
	       (fn l =>
		let
		   val info as {layedOut, ...} = labelInfo l
		in
		   if !layedOut 
		      then print (concat ["\tgoto ", Label.toString l, ";\n"])
		   else printLabelCode info
		end) arg
	    and printLabelCode arg =
	       tracePrintLabelCode
	       (fn {block = Block.T {kind, label = l, live, statements,
				     transfer, ...},
		    layedOut, status, ...} =>
		let
		  val _ = layedOut := true
		  val _ =
		     case !status of
			Many =>
			   let
			      val s = Label.toString l
			   in
			      print s
			      ; print ":\n"
			   end 
		      | _ => ()
		  fun pop (fi: FrameInfo.t) =
		     (C.push (~ (Program.frameSize (program, fi)), print)
		      ; if profiling
			   then print "\tFlushStackTop();\n"
			else ())
		  val _ =
		     case kind of
			Kind.Cont {frameInfo, ...} => pop frameInfo
		      | Kind.CReturn {dst, frameInfo, ...} =>
			   (case frameInfo of
			       NONE => ()
			     | SOME fi => pop (valOf frameInfo)
			    ; (Option.app
			       (dst, fn x =>
				print (concat
				       ["\t", operandToString x, " = ",
					creturn (Type.toRuntime (Operand.ty x)),
					";\n"]))))
		      | Kind.Func => ()
		      | Kind.Handler {frameInfo, ...} => pop frameInfo
		      | Kind.Jump => ()
		  val _ =
		     if 0 = !Control.Native.commented
			then ()
		     else
			if true
			   then
			      Vector.foreach
			      (live, fn z =>
			       if Type.isPointer (Operand.ty z)
				  then
				     print
				     (concat ["\tCheckPointer(",
					      operandToString z,
					      ");\n"])
			       else ())
			else
			   print (let open Layout
				  in toString
				     (seq [str "\t/* live: ",
					   Vector.layout Operand.layout live,
					   str " */\n"])
				  end)
		  val _ = Vector.foreach (statements, outputStatement)
		  val _ = outputTransfer (transfer, l)
	       in ()
	       end) arg
	    and outputTransfer (t, source: Label.t) =
	       let
		  fun iff (test, a, b) =
		     (force a
		      ; C.call ("\tBNZ", [test, Label.toString a], print)
		      ; gotoLabel b
		      ; maybePrintLabel a)
		  datatype z = datatype Transfer.t
	       in
		  case t of
		     Arith {prim, args, dst, overflow, success, ...} =>
			let
			   val prim =
			      let
				 datatype z = datatype Prim.Name.t
				 fun const i =
				    case Vector.sub (args, i) of
				       Operand.Int _ => true
				     | _ => false
				 fun const0 () = const 0
				 fun const1 () = const 1
			      in
				 case Prim.name prim of
				    Int_addCheck =>
				       if const0 ()
					  then "\tInt_addCheckCX"
				       else if const1 ()
					       then "\tInt_addCheckXC"
					    else "\tInt_addCheck"
				  | Int_mulCheck => "\tInt_mulCheck"
				  | Int_negCheck => "\tInt_negCheck"
				  | Int_subCheck =>
				       if const0 ()
					  then "\tInt_subCheckCX"
				       else if const1 ()
					       then "\tInt_subCheckXC"
					    else "\tInt_subCheck"
				  | Word32_addCheck =>
				       if const0 ()
					  then "\tWord32_addCheckCX"
				       else if const1 ()
					       then "\tWord32_addCheckXC"
					    else "\tWord32_addCheck"
				  | Word32_mulCheck => "\tWord32_mulCheck"  
				  | _ => Error.bug "strange overflow prim"
			      end
			   val _ = force overflow
			in
			   C.call (prim,
				   operandToString dst
				   :: (Vector.toListMap (args, operandToString)
				       @ [Label.toString overflow]),
				   print)
			   ; gotoLabel success 
			   ; maybePrintLabel overflow
			end
		   | CCall {args, frameInfo, func, return} =>
			let
			   val {maySwitchThreads,
				modifiesFrontier,
				modifiesStackTop,
				name,
				returnTy,
				...} = CFunction.dest func
			   val (args, afterCall) =
			      case frameInfo of
				 NONE =>
				    (Vector.toListMap (args, operandToString),
				     fn () => ())
			       | SOME frameInfo =>
				    let
				       val size =
					  Program.frameSize (program, frameInfo)
				       val res = copyArgs args
				       val _ = push (valOf return, size)
				    in
				       res
				    end
			   val _ =
			      if modifiesFrontier
				 then print "\tFlushFrontier();\n"
			      else ()
			   val _ =
			      if modifiesStackTop
				 andalso (Option.isNone frameInfo
					  orelse not profiling)
				 then print "\tFlushStackTop();\n"
			      else ()
			   val _ = print "\t"
			   val _ =
			      case returnTy of
				 NONE => ()
			       | SOME t => print (concat [creturn t, " = "])
			   val _ = C.call (name, args, print)
			   val _ = afterCall ()
			   val _ =
			      if modifiesFrontier
				 then print "\tCacheFrontier();\n"
			      else ()
			   val _ =
			      if modifiesStackTop
				 then print "\tCacheStackTop();\n"
			      else ()
			   val _ =
			      if maySwitchThreads
				 then print "\tReturn();\n"
			      else Option.app (return, gotoLabel)
			in
			   ()
			end
		   | Call {label, return, ...} =>
			let
			   val dstChunk = labelChunk label
			   val _ =
			      case return of
				 NONE => ()
			       | SOME {return, size, ...} =>
				    push (return, size)
			in
			   if ChunkLabel.equals (labelChunk source, dstChunk)
			      then gotoLabel label
			   else
			      C.call ("\tFarJump", 
				      [ChunkLabel.toString dstChunk, 
				       Label.toStringIndex label],
				      print)
			end
		   | Goto dst => gotoLabel dst
		   | Raise => C.call ("\tRaise", [], print)
		   | Return => C.call ("\tReturn", [], print)
		   | Switch switch =>
			let 
			   fun bool (test: Operand.t, t, f) =
			      iff (operandToString test, t, f)
			   fun doit {cases: (string * Label.t) vector,
				     default: Label.t option,
				     test: Operand.t}: unit =
			      let
				 val test = operandToString test
				 fun switch (cases: (string * Label.t) vector,
					     default: Label.t): unit =
				    (print "switch ("
				     ; print test
				     ; print ") {\n"
				     ; (Vector.foreach
					(cases, fn (n, l) => (print "case "
							      ; print n
							      ; print ":\n"
							      ; gotoLabel l)))
				     ; print "default:\n"
				     ; gotoLabel default
				     ; print "}\n")
			      in
				 case (Vector.length cases, default) of
				    (0, NONE) =>
				       Error.bug "switch: empty cases"
				  | (0, SOME l) => gotoLabel l
				  | (1, NONE) =>
				       gotoLabel (#2 (Vector.sub (cases, 0)))
				  | (_, NONE) =>
				       switch (Vector.dropPrefix (cases, 1),
					       #2 (Vector.sub (cases, 0)))
				  | (_, SOME l) => switch (cases, l)
			      end
			   fun simple ({cases, default, test}, f) =
			      doit {cases = Vector.map (cases, fn (c, l) =>
							(f c, l)),
				    default = default,
				    test = test}
			   datatype z = datatype Switch.t
			in
			   case switch of
			      Char z => simple (z, C.char)
			    | EnumPointers {enum, pointers, test} =>
			      iff (concat
				   ["IsInt (", operandToString test, ")"],
				   enum, pointers)
			    | Int (z as {cases, default, test}) =>
				 let
				    fun normal () = simple (z, C.int)
				 in
				    if 2 = Vector.length cases
				       then
					  let
					     val c0 = Vector.sub (cases, 0)
					     val c1 = Vector.sub (cases, 1)
					  in
					     case (c0, c1, default) of
						((0, f), (1, t), NONE) =>
						   bool (test, t, f)
					      | ((1, t), (0, f), NONE) =>
						   bool (test, t, f)
					      | _ => normal ()
					  end
				    else normal ()
				 end
			    | Pointer {cases, default, tag, ...} =>
				 doit {cases = (Vector.map
						(cases, fn {dst, tag, ...} =>
						 (Int.toString tag, dst))),
				       default = default,
				       test = tag}
			    | Word z => simple (z, C.word)
			end
	       end
	    fun declareRegisters () =
	       List.foreach
	       (Runtime.Type.all, fn t =>
		let
		   val d = concat ["D", Runtime.Type.name t]
		in
		   Int.for (0, 1 + regMax t, fn i =>
			    C.call (d, [C.int i], print))
		end)
	 in
	    C.callNoSemi ("Chunk", [ChunkLabel.toString chunkLabel], print)
	    ; print "\n"
	    ; declareRegisters ()
	    ; C.callNoSemi ("ChunkSwitch", [ChunkLabel.toString chunkLabel],
			    print)
	    ; print "\n"
	    ; Vector.foreach (blocks, fn Block.T {kind, label, ...} =>
			      if Kind.isEntry kind
				 then (print "case "
				       ; print (Label.toStringIndex label)
				       ; print ":\n"
				       ; gotoLabel label)
			      else ())
	    ; print "EndChunk\n"
	 end
      val additionalMainArgs =
	 [ChunkLabel.toString chunkLabel,
	  Label.toStringIndex label]
      fun rest () =
	 (declareChunks ()
	  ; declareNextChunks ()
	  ; declareIndices ()
	  ; List.foreach (chunks, outputChunk))
   in
      outputDeclarations {additionalMainArgs = additionalMainArgs,
			  includes = includes,
			  name = "C",
			  program = program,
			  print = print,
			  rest = rest}
      ; done ()
   end

end
