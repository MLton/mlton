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
   structure CFunction = CFunction
   structure CType = CType
   structure Chunk = Chunk
   structure ChunkLabel = ChunkLabel
   structure FrameInfo = FrameInfo
   structure Global = Global
   structure IntSize = IntSize
   structure IntX = IntX
   structure Kind = Kind
   structure Label = Label
   structure ObjectType = ObjectType
   structure Operand = Operand
   structure Prim = Prim
   structure ProfileInfo = ProfileInfo
   structure ProfileLabel = ProfileLabel
   structure Program = Program
   structure RealSize = RealSize
   structure RealX = RealX
   structure Register = Register
   structure Runtime = Runtime
   structure Statement = Statement
   structure Switch = Switch
   structure Transfer = Transfer
   structure Type = Type
   structure WordSize = WordSize
   structure WordX = WordX
end

datatype z = datatype IntSize.t
datatype z = datatype RealSize.t
datatype z = datatype WordSize.t

local
   open Runtime
in
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

structure IntX =
   struct
      open IntX
	 
      fun toC (i: t): string =
	 let
	    fun isPos () = i >= zero (size i)
	    fun neg () = concat ["-", String.dropPrefix (toString i, 1)]
	    fun simple s =
	       concat ["(Int", s, ")",
		       if isPos () then toString i else neg ()]
	    (* tricky writes min as a word to avoid a gcc warning. *)
	    fun tricky min =
	       if isPos ()
		  then toString i
	       else if IntX.isMin i
		       then min
		    else neg ()
	 in
	    case size i of
	       I8 => simple "8"
	     | I16 => simple "16"
	     | I32 => tricky ("0x80000000")
	     | I64 => concat [tricky "0x8000000000000000", "ll"]
	 end
   end

structure RealX =
   struct
      open RealX

      fun toC (r: t): string =
	 let
	    (* The only difference between SML reals and C floats/doubles is that
	     * SML uses "~" while C uses "-".
	     *)
	    val s =
	       String.translate (toString r,
				 fn #"~" => "-" | c => String.fromChar c)
	 in
	    case size r of
	       R32 => concat ["(Real32)", s]
	     | R64 => s
	 end
   end

structure WordX =
   struct
      open WordX

      fun toC (w: t): string =
	 let
	    fun simple s =
	       concat ["(Word", s, ")0x", toString w]
	 in
	    case size w of
	       W8 => simple "8"
	     | W16 => simple "16"
	     | W32 => concat ["0x", toString w]
	     | W64 => concat ["0x", toString w, "llu"]
	 end
   end
   
structure C =
   struct
      val truee = "TRUE"
      val falsee = "FALSE"

      fun bool b = if b then truee else falsee
	 
      fun args (ss: string list): string
	 = concat ("(" :: List.separate (ss, ", ") @ [")"])
         
      fun callNoSemi (f: string, xs: string list, print: string -> unit): unit 
	 = (print f
	    ; print " ("
	    ; (case xs 
		  of [] => ()
		| x :: xs => (print x
			      ; List.foreach (xs, 
					     fn x => (print ", "; print x))))
	    ; print ")")

      fun call (f, xs, print) =
	 (callNoSemi (f, xs, print)
	  ; print ";\n")

      fun int (i: int) =
	 IntX.toC (IntX.make (IntInf.fromInt i, IntSize.default))

      fun string s =
	 let val quote = "\""
	 in concat [quote, String.escapeC s, quote]
	 end

      fun word (w: Word.t) = "0x" ^ Word.toString w

      fun push (i, print) =
	 call ("\tPush", [int i], print)
   end

structure Operand =
   struct
      open Operand

      fun isMem (z: t): bool =
	 case z of
	    ArrayOffset _ => true
	  | Cast (z, _) => isMem z
	  | Contents _ => true
	  | Offset _ => true
	  | StackOffset _ => true
	  | _ => false
   end

fun creturn (t: CType.t): string =
   concat ["CReturn", CType.name t]

fun outputIncludes (includes, print) =
   (List.foreach (includes, fn i => (print "#include <";
				     print i;
				     print ">\n"))
    ; print "\n")

fun declareProfileLabel (l, print) =
   C.call ("DeclareProfileLabel", [ProfileLabel.toString l], print)

fun declareGlobals (prefix: string, print) =
   let
      (* gcState can't be static because stuff in mlton-lib.c refers to
       * it.
       *)
      val _ = print (concat [prefix, "struct GC_state gcState;\n"])
      val _ =
	 List.foreach
	 (CType.all, fn t =>
	  let
	     val s = CType.toString t
	  in		
	     print (concat [prefix, s, " global", s,
			    " [", C.int (Global.numberOfType t), "];\n"])
	     ; print (concat [prefix, s, " CReturn", CType.name t, ";\n"])
	  end)
      val _ =	       			    
	 print (concat [prefix, "Pointer globalPointerNonRoot [",
			C.int (Global.numberOfNonRoot ()),
			"];\n"])
   in
      ()
   end

fun outputDeclarations
   {additionalMainArgs: string list,
    includes: string list,
    print: string -> unit,
    program = (Program.T
	       {frameLayouts, frameOffsets, intInfs, maxFrameSize,
		objectTypes, profileInfo, reals, strings, ...}),
    rest: unit -> unit
    }: unit =
   let
      fun declareExports () =
	 Ffi.declareExports {print = print}
      fun declareLoadSaveGlobals () =
	 let
	    val _ =
	       (print "static void saveGlobals (int fd) {\n"
		; (List.foreach
		   (CType.all, fn t =>
		    print (concat ["\tSaveArray (global",
				   CType.toString t, ", fd);\n"])))
		; print "}\n")
	    val _ =
	       (print "static void loadGlobals (FILE *file) {\n"
		; (List.foreach
		   (CType.all, fn t =>
		    print (concat ["\tLoadArray (global",
				   CType.toString t, ", file);\n"])))
		; print "}\n")
	 in
	    ()
	 end
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
	 (print "static void real_Init() {\n"
	  ; List.foreach (reals, fn (g, r) =>
			  print (concat ["\tglobalReal",
					 RealSize.toString (RealX.size r),
					 "[", C.int (Global.index g), "] = ",
					 RealX.toC r, ";\n"]))
	  ; print "}\n")
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
      fun declareAtMLtons () =
	 declareArray ("string", "atMLtons", !Control.atMLtons, C.string o #2)
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
		 | Weak =>
		      (3, 2, 1)
		 | WeakGone =>
		      (3, 3, 0)
	  in
	     concat ["{ ", C.int tag, ", ",
		     C.int nonPointers, ", ",
		     C.int pointers, " }"]
	  end)
      fun declareMain () =
	 let
	    val align =
	       case !Control.align of
		  Control.Align4 => 4
		| Control.Align8 => 8
	    val magic = C.word (case Random.useed () of
				   NONE => String.hash (!Control.inputFile)
				 | SOME w => w)
	 in 
	    C.callNoSemi ("Main",
			  [C.int align,
			   C.int (!Control.cardSizeLog2),
			   magic,
			   C.int maxFrameSize,
			   C.bool (!Control.markCards),
			   C.bool (!Control.profileStack)]
			  @ additionalMainArgs,
			  print)
	    ; print "\n"
	 end
      fun declareProfileInfo () =
	 let
	    fun doit (ProfileInfo.T {frameSources, labels, names, sourceSeqs,
				     sources}) =
	       (Vector.foreach (labels, fn {label, ...} =>
				declareProfileLabel (label, print))
		; (Vector.foreachi
		   (sourceSeqs, fn (i, v) =>
		    (print (concat ["static int sourceSeq",
				    Int.toString i,
				    "[] = {"])
		     ; print (C.int (Vector.length v))
		     ; Vector.foreach (v, fn i =>
				       (print (concat [",", C.int i])))
		     ; print "};\n")))
		; declareArray ("uint", "*sourceSeqs", sourceSeqs, fn (i, _) =>
				concat ["sourceSeq", Int.toString i])
		; declareArray ("uint", "frameSources", frameSources, C.int o #2)
		; (declareArray
		   ("struct GC_sourceLabel", "sourceLabels", labels,
		    fn (_, {label, sourceSeqsIndex}) =>
		    concat ["{(pointer)", ProfileLabel.toString label, ", ",
			    C.int sourceSeqsIndex, "}"]))
		; declareArray ("string", "sourceNames", names, C.string o #2)
		; declareArray ("struct GC_source", "sources", sources,
				fn (_, {nameIndex, successorsIndex}) =>
				concat ["{ ", Int.toString nameIndex, ", ",
					Int.toString successorsIndex, " }"]))
	 in
	    case profileInfo of
	       NONE => doit ProfileInfo.empty
	     | SOME z => doit z
	 end
   in
      outputIncludes (includes, print)
      ; declareGlobals ("", print)
      ; declareExports ()
      ; declareLoadSaveGlobals ()
      ; declareIntInfs ()
      ; declareStrings ()
      ; declareReals ()
      ; declareFrameOffsets ()
      ; declareFrameLayouts ()
      ; declareObjectTypes ()
      ; declareProfileInfo ()
      ; declareAtMLtons ()
      ; rest ()
      ; declareMain ()
   end

structure Type =
   struct
      open Type

      local
	 fun make (name, memo, toString) =
	    memo (fn s => concat [name, toString s])
	 val int = make ("Int", IntSize.memoize, IntSize.toString)
	 val real = make ("Real", RealSize.memoize, RealSize.toString)
	 val word = make ("Word", WordSize.memoize, WordSize.toString)
	 val pointer = "Pointer"
      in
	 fun toC (t: t): string =
	    case t of
	       EnumPointers {pointers, ...} =>
		  if 0 = Vector.length pointers
		     then int I32
		  else pointer
	     | ExnStack => word W32
	     | Int s => int s
	     | IntInf => pointer
	     | Label _ => word W32
	     | Real s => real s
	     | Word s => word s
	     | _ => Error.bug (concat ["Type.toC strange type: ", toString t])
      end
   end

fun contents (ty, z) = concat ["C", C.args [Type.toC ty, z]]

fun output {program as Machine.Program.T {chunks,
					  frameLayouts,
					  main = {chunkLabel, label}, ...},
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
      val {get = chunkLabelIndex: ChunkLabel.t -> int, ...} =
	 Property.getSet (ChunkLabel.plist,
			  Property.initFun (let
					       val c = Counter.new 0
					    in
					       fn _ => Counter.next c
					    end))
      val chunkLabelToString = C.int o chunkLabelIndex
      fun declareChunk (Chunk.T {chunkLabel, ...}, print) =
	 C.call ("DeclareChunk",
		 [chunkLabelToString chunkLabel],
		 print)
      val {get = labelIndex, set = setLabelIndex, ...} =
	 Property.getSetOnce (Label.plist,
			      Property.initRaise ("index", Label.layout))
      val _ =
	 Vector.foreachi (entryLabels, fn (i, l) => setLabelIndex (l, i))
      fun labelToStringIndex (l: Label.t): string =
	 let
	    val s = C.int (labelIndex l)
	 in
	    if 0 = !Control.Native.commented
	       then s
	    else concat [s, " /* ", Label.toString l, " */"]
	 end
      val handleMisalignedReals =
	 let
	    open Control
	 in
	    !align = Align4 andalso !targetArch = Sparc
	 end
      fun addr z = concat ["&(", z, ")"]
      fun realFetch z = concat ["Real64_fetch(", addr z, ")"]
      fun realMove {dst, src} =
	 concat ["Real64_move(", addr dst, ", ", addr src, ");\n"]
      fun realStore {dst, src} =
	 concat ["Real64_store(", addr dst, ", ", src, ");\n"]
      fun move {dst: string, dstIsMem: bool,
		src: string, srcIsMem: bool,
		ty: Type.t}: string =
	 if handleMisalignedReals
	    andalso Type.equals (ty, Type.real R64)
	    then
	       case (dstIsMem, srcIsMem) of
		  (false, false) => concat [dst, " = ", src, ";\n"]
		| (false, true) => concat [dst, " = ", realFetch src, ";\n"]
		| (true, false) => realStore {dst = dst, src = src}
		| (true, true) => realMove {dst = dst, src = src}
	 else concat [dst, " = ", src, ";\n"]
      local
	 datatype z = datatype Operand.t
      	 fun toString (z: Operand.t): string =
	    case z of
	       ArrayOffset {base, index, ty} =>
		  concat ["X", C.args [Type.toC ty,
				       toString base,
				       toString index]]
	     | Cast (z, ty) => concat ["(", Type.toC ty, ")", toString z]
	     | Contents {oper, ty} => contents (ty, toString oper)
	     | File => "__FILE__"
	     | Frontier => "Frontier"
	     | GCState => "GCState"
	     | Global g =>
		  if Global.isRoot g
		     then concat ["G",
				  C.args [Type.toC (Global.ty g),
					  Int.toString (Global.index g)]]
		  else concat ["GPNR", C.args [Int.toString (Global.index g)]]
	     | Int i => IntX.toC i
	     | Label l => labelToStringIndex l
	     | Line => "__LINE__"
	     | Offset {base, offset, ty} =>
		  concat ["O", C.args [Type.toC ty, toString base, C.int offset]]
	     | Real r => RealX.toC r
	     | Register r =>
		  concat [Type.name (Register.ty r), "_",
			  Int.toString (Register.index r)]
	     | SmallIntInf w =>
		  concat ["SmallIntInf", C.args [concat ["0x", Word.toString w]]]
	     | StackOffset {offset, ty} =>
		  concat ["S", C.args [Type.toC ty, C.int offset]]
	     | StackTop => "StackTop"
	     | Word w => WordX.toC w
      in
	 val operandToString = toString
      end
      fun fetchOperand (z: Operand.t): string =
	 if handleMisalignedReals
	    andalso Type.equals (Operand.ty z, Type.real R64)
	    andalso Operand.isMem z
	    then realFetch (operandToString z)
	 else operandToString z
      fun outputStatement (s, print) =
	 let
	    datatype z = datatype Statement.t
	 in
	    case s of
	       Noop => ()
	     | _ =>
		  (print "\t"
		   ; (case s of
			 Move {dst, src} =>
			    print
			    (move {dst = operandToString dst,
				   dstIsMem = Operand.isMem dst,
				   src = operandToString src,
				   srcIsMem = Operand.isMem src,
				   ty = Operand.ty dst})
		       | Noop => ()
		       | Object {dst, header, size, stores} =>
			    (C.call ("Object", [operandToString dst,
						C.word header],
				     print)
			     ; (Vector.foreach
				(stores, fn {offset, value} =>
				 let
				    val ty = Operand.ty value
				    val dst =
				       contents
				       (Operand.ty value,
					concat ["Frontier + ",
						C.int
						(offset
						 + Runtime.normalHeaderSize)])
				 in
				    print "\t"
				    ; (print
				       (move {dst = dst,
					      dstIsMem = true,
					      src = operandToString value,
					      srcIsMem = Operand.isMem value,
					      ty = ty}))
				 end))
			     ; print "\t"
			     ; C.call ("EndObject", [C.int size], print))
		       | PrimApp {args, dst, prim} =>
			    let
			       fun call (): string =
				  concat
				  [Prim.toString prim,
				   "(",
				   concat
				   (List.separate
				    (Vector.toListMap (args, fetchOperand),
				     ", ")),
				   ")"]
			       fun app (): string =
				  case Prim.name prim of
				     Prim.Name.FFI_Symbol {name, ...} => name
				   | _ => call ()
			    in
			       case dst of
				  NONE => (print (app ())
					   ; print ";\n")
				| SOME dst =>
				     print (move {dst = operandToString dst,
						  dstIsMem = Operand.isMem dst,
						  src = app (),
						  srcIsMem = false,
						  ty = Operand.ty dst})
			    end
		       | ProfileLabel l =>
			    C.call ("ProfileLabel", [ProfileLabel.toString l],
				    print)
			    ))
	 end
      val profiling = !Control.profile <> Control.ProfileNone
      fun outputChunk (Chunk.T {chunkLabel, blocks, regMax, ...}) =
	 let
	    val {done, print, ...} = outputC ()
	    fun declareFFI () =
	       let
		  val seen = String.memoize (fn _ => ref false)
		  fun doit (name: string, declare: unit -> string): unit =
		     let
			val r = seen name
		     in
			if !r
			   then ()
			else (r := true; print (declare ()))
		     end
	       in
		  Vector.foreach
		  (blocks, fn Block.T {statements, transfer, ...} =>
		   let
		      val _ =
			 Vector.foreach
			 (statements, fn s =>
			  case s of
			     Statement.PrimApp {prim, ...} =>
				(case Prim.name prim of
				    Prim.Name.FFI_Symbol {name, ty} =>
				       doit
				       (name, fn () =>
					concat
					["extern ", CType.toString ty,
					    " ", name, ";\n"])
				  | _ => ())
			   | _ => ())
		      val _ =
			 case transfer of
			    Transfer.CCall {func, ...} =>
			       let
				  val CFunction.T {name, ...} = func
			       in
				  if name = "Thread_returnToC"
				     then ()
				  else
				     doit (name, fn () =>
					   concat [CFunction.prototype func,
						   ";\n"])
			       end
			  | _ => ()
		   in
		      ()
		   end)
	       end
	    fun declareChunks () =
	       let
		  val {get, ...} =
		     Property.get (ChunkLabel.plist,
				   Property.initFun (fn _ => ref false))
		  val _ =
		     Vector.foreach
		     (blocks, fn Block.T {transfer, ...} =>
		      case transfer of
			 Transfer.Call {label, ...} =>
			    get (labelChunk label) := true
		       | _ => ())
		  val _ =
		     List.foreach
		     (chunks, fn c as Chunk.T {chunkLabel, ...} =>
		      if ! (get chunkLabel)
			 then declareChunk (c, print)
		      else ())
	       in
		  ()
	       end
	    fun declareProfileLabels () =
	       Vector.foreach
	       (blocks, fn Block.T {statements, ...} =>
		Vector.foreach
		(statements, fn s =>
		 case s of
		    Statement.ProfileLabel l => declareProfileLabel (l, print)
		  | _ => ()))
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
		(blocks, fn Block.T {kind, label, transfer, ...} =>
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
		; print (move {dst = (operandToString
				      (Operand.StackOffset
				       {offset = size - Runtime.labelSize,
					ty = Type.label return})),
			       dstIsMem = true,
			       src = operandToString (Operand.Label return),
			       srcIsMem = false,
			       ty = Type.Label return})
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
					print
					(concat
					 ["\t", Type.toC ty, " ", tmp, " = ",
					  fetchOperand z, ";\n"])
				  in
				     tmp
				  end
			     | _ => fetchOperand z)
		     in
			(args, fn () => print "\t}\n")
		     end
	       else (Vector.toListMap (args, fetchOperand),
		     fn () => ())
	    val tracePrintLabelCode =
	       Trace.trace
	       ("printLabelCode",
		fn {block, layedOut, ...} =>
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
			     | SOME fi => pop fi
			    ; (Option.app
			       (dst, fn x =>
				let
				   val ty = Operand.ty x
				in
				   print
				   (concat
				    ["\t",
				     move {dst = operandToString x,
					   dstIsMem = Operand.isMem x,
					   src = creturn (Type.toCType ty),
					   srcIsMem = false,
					   ty = ty}])
				end)))
		      | Kind.Func => ()
		      | Kind.Handler {frameInfo, ...} => pop frameInfo
		      | Kind.Jump => ()
		  val _ =
		     if 0 = !Control.Native.commented
			then ()
		     else
			if false
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
		  val _ = Vector.foreach (statements, fn s =>
					  outputStatement (s, print))
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
				    Int_addCheck _ =>
				       concat [Prim.toString prim,
					       if const0 ()
						  then "CX"
					       else if const1 ()
						       then "XC"
						    else ""]
				  | Int_mulCheck _ => Prim.toString prim
				  | Int_negCheck _ => Prim.toString prim
				  | Int_subCheck _ =>
				       concat [Prim.toString prim,
					       if const0 ()
						  then "CX"
					       else if const1 ()
						       then "XC"
						    else ""]
				  | Word_addCheck _ =>
				       concat [Prim.toString prim,
					       if const0 ()
						  then "CX"
					       else if const1 ()
						       then "XC"
						    else ""]
				  | Word_mulCheck _ => Prim.toString prim
				  | _ => Error.bug "strange overflow prim"
			      end
			   val _ = force overflow
			in
                           print "\t"
			   ; C.call (prim,
				     operandToString dst
				     :: (Vector.toListMap (args, operandToString)
					 @ [Label.toString overflow]),
				     print)
			   ; gotoLabel success 
			   ; maybePrintLabel overflow
			end
		   | CCall {args, frameInfo, func, return} =>
			let
			   val CFunction.T {maySwitchThreads,
					    modifiesFrontier,
					    modifiesStackTop,
					    name, return = returnTy, ...} = func
			   val (args, afterCall) =
			      case frameInfo of
				 NONE =>
				    (Vector.toListMap (args, fetchOperand),
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
				      [chunkLabelToString dstChunk, 
				       labelToStringIndex label],
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
			   fun simple ({cases, default, size = _, test}, f) =
			      doit {cases = Vector.map (cases, fn (c, l) =>
							(f c, l)),
				    default = default,
				    test = test}
			   datatype z = datatype Switch.t
			in
			   case switch of
			      EnumPointers {enum, pointers, test} =>
			      iff (concat
				   ["IsInt (", operandToString test, ")"],
				   enum, pointers)
			    | Int (z as {cases, default, test, ...}) =>
				 let
				    fun normal () = simple (z, IntX.toC)
				 in
				    if 2 = Vector.length cases
				       andalso Option.isNone default
				       then
					  let
					     val (c0, l0) = Vector.sub (cases, 0)
					     val (c1, l1) = Vector.sub (cases, 1)
					  in
					     if IntX.isZero c0
						andalso IntX.isOne c1
						then bool (test, l1, l0)
					     else if (IntX.isOne c0
						      andalso IntX.isZero c1)
						     then bool (test, l0, l1)
						  else normal ()
					  end
				    else normal ()
				 end
			    | Pointer {cases, default, tag, ...} =>
				 doit {cases = (Vector.map
						(cases, fn {dst, tag, ...} =>
						 (Int.toString tag, dst))),
				       default = default,
				       test = tag}
			    | Word z => simple (z, WordX.toC)
			end
	       end
	    fun declareRegisters () =
	       List.foreach
	       (CType.all, fn t =>
		let
		   val pre = concat ["\t", CType.toString t, " ",
				     CType.name t, "_"]
		in
		   Int.for (0, 1 + regMax t, fn i =>
			    print (concat [pre, C.int i, ";\n"]))
		end)
	    fun outputOffsets () =
	       List.foreach
	       ([("ExnStackOffset", GCField.ExnStack),
		 ("FrontierOffset", GCField.Frontier),
		 ("StackBottomOffset", GCField.StackBottom),
		 ("StackTopOffset", GCField.StackTop)],
		fn (name, f) =>
		print (concat ["#define ", name, " ",
			       Int.toString (GCField.offset f), "\n"]))
	 in
	    outputIncludes (["c-chunk.h"], print)
	    ; outputOffsets ()
	    ; declareGlobals ("extern ", print)
	    ; declareFFI ()
	    ; declareChunks ()
	    ; declareProfileLabels ()
	    ; C.callNoSemi ("Chunk", [chunkLabelToString chunkLabel], print)
	    ; print "\n"
	    ; declareRegisters ()
	    ; C.callNoSemi ("ChunkSwitch", [chunkLabelToString chunkLabel],
			    print)
	    ; print "\n"
	    ; Vector.foreach (blocks, fn Block.T {kind, label, ...} =>
			      if Kind.isEntry kind
				 then (print "case "
				       ; print (labelToStringIndex label)
				       ; print ":\n"
				       ; gotoLabel label)
			      else ())
	    ; print "EndChunk\n"
	    ; done ()
	 end
      val additionalMainArgs =
	 [chunkLabelToString chunkLabel,
	  labelToStringIndex label]
      val {print, done, ...} = outputC ()
      fun rest () =
	 (List.foreach (chunks, fn c => declareChunk (c, print))
	  ; print "struct cont ( *nextChunks []) () = {"
	  ; Vector.foreach (entryLabels, fn l =>
			    let
			       val {chunkLabel, ...} = labelInfo l
			    in
			       print "\t"
			       ; C.callNoSemi ("Chunkp",
					       [chunkLabelToString chunkLabel],
					       print)
			       ; print ",\n"
			    end)
	  ; print "};\n")
      val _ = 
	 outputDeclarations {additionalMainArgs = additionalMainArgs,
			     includes = ["c-main.h"],
			     program = program,
			     print = print,
			     rest = rest}
      val _ = done ()
      val _ = List.foreach (chunks, outputChunk)
   in
      ()
   end

end
