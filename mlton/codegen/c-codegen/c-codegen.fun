(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor CCodeGen (S: C_CODEGEN_STRUCTS): C_CODEGEN =
struct

open S

local open MachineOutput
in
   structure Block = Block
   structure Cases = Cases
   structure ChunkLabel = ChunkLabel
   structure GCInfo = GCInfo
   structure Global = Global
   structure Label = Label
   structure Operand = Operand
   structure Prim = Prim
   structure PrimInfo = PrimInfo
   structure Register = Register
   structure Type = Type
end

val traceGotoLabel = Trace.trace ("gotoLabel", Label.layout, Unit.layout) 

val wordSize: int = 4
val pointerSize = wordSize
val objectHeaderSize = wordSize
val arrayHeaderSize = 2 * wordSize
val intInfOverhead = arrayHeaderSize + wordSize (* for the sign *)

val overhead = "**C overhead**"
   
structure C =
   struct
      val truee = "TRUE"
      val falsee = "FALSE"

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

      fun call (f, xs, print) = (callNoSemi (f, xs, print)
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

      (* The only difference between SML floats and C floats is that
       * SML uses "~" while C uses "-".
       *)
      fun float s = String.translate (s, fn #"~" => "-" | c => String.fromChar c)

      fun string s =
	 let val quote = "\""
	 in concat [quote, String.escapeC s, quote]
	 end

      fun bug (s: string, print) =
	 call ("MLton_bug", [concat ["\"", String.escapeC s, "\""]], print)

      local
	 val current = ref ""
      in
	 fun profile (detailed: string, nonDetailed: string,
		      print: string -> unit): unit =
	    if !Control.profile
	       then
		  if detailed <> !current
		     then (print "/* PROFILE: "
			   ; print detailed
			   ; print " & "
			   ; print nonDetailed
			   ; print " */\n"
			   ; current := detailed)
		  else ()
	    else ()
      end 
   end

structure Label =
   struct
      open MachineOutput.Label

      fun toStringIndex l = (toString l) ^ "_index"
   end

structure Operand =
   struct
      open Operand
	 
      val rec toString =
	 fn ArrayOffset {base, offset, ty} =>
	       concat ["X", Type.name ty,
		       C.args [toString base, toString offset]]
          | CastInt oper => concat ["PointerToInt", C.args [toString oper]]
          | Char c => C.char c
          | Contents {oper, ty} =>
	       concat ["C", Type.name ty, "(", toString oper, ")"]
          | Float s => C.float s
          | Global g => Global.toString g
          | GlobalPointerNonRoot n =>
	       concat ["globalpointerNonRoot [", C.int n, "]"]
          | Int n => C.int n
          | IntInf w =>
	       concat ["SmallIntInf", C.args [concat ["0x", Word.toString w]]]
          | Label l => Label.toStringIndex l
          | Offset {base, offset, ty} =>
	       concat ["O", Type.name ty, C.args [toString base, C.int offset]]
          | Pointer n => concat ["IntAsPointer", C.args [C.int n]]
          | Register r => Register.toString r
          | StackOffset {offset, ty} =>
	       concat ["S", Type.name ty, "(", C.int offset, ")"]
          | Uint w => C.word w

      val layout = Layout.str o toString
   end

structure GCInfo =
   struct
      open GCInfo
	 
      fun output (T {frameSize, return, ...}, name, extra, print) =
	 C.call (name,
		 Int.toString frameSize :: Label.toString return :: extra,
		 print)

      fun outputLimitCheck (info as T {frameSize, return, ...}, 
			   bytes, stackCheck, print) =
	 output (info, "LimitCheck",
		 [bytes, if stackCheck
			    then "StackOverflowCheck"
			 else "FALSE"],
		 print)
   end

structure Statement =
   struct
      open MachineOutput.Statement

      fun assign ({dst, prim, pinfo, args}, print) =
	 let
	    fun printDst () =
	       case dst of
		  NONE => ()
		| SOME dst => print (concat [Operand.toString dst, " = "])
	    fun doit () =
	       let 
		  val s = Prim.toString prim
		  val _ = printDst ()
		  val args = Vector.toListMap (args, Operand.toString)
		  val gcInfo =
		     case pinfo of
		        PrimInfo.Runtime gcInfo => SOME gcInfo
		      | _ => NONE
	       in if Prim.entersRuntime prim
		     then GCInfo.output (valOf gcInfo, s, args, print)
		  else C.call (s, args, print)
	       end
	    datatype z = datatype Prim.Name.t
	 in 
	    case Prim.name prim of
	       FFI s => (case Prim.numArgs prim of
			    NONE => (printDst (); print (concat [s, ";\n"]))
			  | SOME _ => doit ())
	     | _ => doit ()
	 end
 
      fun output (s, print) =
	 case s of
	    Noop => ()
	  | _ =>
	       (print "\t"
		; (case s of
		      LimitCheck {info, bytes, stackCheck} =>
			 GCInfo.outputLimitCheck (info,
						  C.int bytes,
						  stackCheck,
						  print)
		    | Noop => ()
		    | Move {dst, src} =>
			 print (concat [Operand.toString dst, " = ",
				      Operand.toString src, ";\n"])
		    | Push n => C.call ("Push", [C.int n], print)
		    | Assign z => assign (z, print)
		    | SetExnStackLocal {offset} =>
			 C.call ("SetExnStackLocal", [C.int offset], print)
		    | SetExnStackSlot {offset} =>
			 C.call ("SetExnStackSlot", [C.int offset], print)
		    | SetSlotExnStack {offset} =>
			 C.call ("SetSlotExnStack", [C.int offset], print)
		    | Allocate {dst, size, stores, numPointers,
				numWordsNonPointers} =>
			 (C.call ("Object", [Operand.toString dst,
					    C.int numWordsNonPointers,
					    C.int numPointers],
				 print)
			  ; print "\t"
			  ; (List.foreach
			     (stores, fn {offset, value} =>
			      (C.call
			       (concat ["A", Type.name (Operand.ty value)],
				[C.int offset, Operand.toString value], 
				print)
			       ; print "\t")))
			  ; C.call ("EndObject", [C.int size], print))
		    | AllocateArray {dst, numElts, numPointers,
				     numBytesNonPointers, live, limitCheck} =>
			 let
			    val dst = Operand.toString dst
			    val numElts = Operand.toString numElts
			 in case limitCheck
			    of NONE => ()
			     | SOME {gcInfo, bytesPerElt, bytesAllocated} 
			       => let
				     val bytes =
					concat [numElts, 
						" * ", C.int bytesPerElt, 
						" + ", 
						C.int bytesAllocated];
				  in
				     GCInfo.outputLimitCheck (gcInfo, bytes,
							     false, print);
				     print "\t"
				  end;
				  if numPointers = 0
				     then C.call ("ArrayNoPointers",
						 [dst, numElts,
						  C.int numBytesNonPointers],
						 print)
				  else if numBytesNonPointers = 0
					  then C.call ("ArrayPointers",
						      [dst, numElts,
						       C.int numPointers],
						      print)
				       else Error.unimplemented 
					  "tricky arrays"
			 end))
      fun toString s =
	 let
	    val ss = ref []
	    fun print s = List.push (ss, s)
	    val _ = output (s, print)
	 in concat (rev (!ss))
	 end

      val layout = Layout.str o toString
   end

structure Transfer =
   struct
      open MachineOutput.Transfer
	 
      fun output (t, print, gotoLabel: Label.t -> unit, maybePrintLabel) =
	 let
	    fun iff (test, a, b) =
	       (C.call ("\tBZ", [test, Label.toString b], print)
		; gotoLabel a
		; maybePrintLabel b)
	 in
	    case t of
	       Arith {prim, args, dst, overflow, success} =>
		  let
		     val prim =
			let
			   datatype z = datatype Prim.Name.t
			in
			   case Prim.name prim of
			      Int_addCheck => "\tInt_addCheckNew"
			    | Int_mulCheck => "\tInt_mulCheckNew"
			    | Int_negCheck => "\tInt_negCheckNew"
			    | Int_subCheck => "\tInt_subCheckNew"
			    | _ => Error.bug "strange overflow prim"
			end
		  in
		     C.call (prim,
			     Operand.toString dst
			     :: (Vector.toListMap (args, Operand.toString)
				 @ [Label.toString overflow]),
			     print)
		     ; gotoLabel success 
		     ; maybePrintLabel overflow
		  end
	     | Bug => (print "\t"; C.bug ("machine", print))
	     | FarJump {chunkLabel, label, return, ...} =>
		  (case return
		     of SOME {return, handler, size}
		      => (Statement.output (Statement.Push size, print);
			  Statement.output (Statement.Move
					    {dst = Operand.StackOffset 
					           {offset = 0, 
						    ty = Type.int},
					     src = Operand.Label return},
					    print))
		      | NONE => ();
		   C.call ("\tFarJump", 
			   [ChunkLabel.toString chunkLabel, 
			    Label.toString label], 
			   print))
	     | NearJump {label, return} => 
		  (case return
		     of SOME {return, handler, size}
		      => (Statement.output (Statement.Push size, print);
			  Statement.output (Statement.Move
					    {dst = Operand.StackOffset 
					           {offset = 0, 
						    ty = Type.int},
					     src = Operand.Label return},
					    print))
		      | NONE => ();
		   gotoLabel label)
	     | Raise => C.call ("\tRaise", [], print)
	     | Return {...} => C.call ("\tReturn", [], print)
	     | Switch {test, cases, default} =>
		  let 
		     val test = Operand.toString test
		     fun bool (t, f) = iff (test, t, f)
		     fun doit (cases, f) =
			let
			   fun switch (cases, l) =
			      (print "switch ("
			       ; print test
			       ; print ") {\n"
			       ; (List.foreach
				  (cases, fn (n, l) => (print "case "
							; print (f n)
							; print ":\n"
							; gotoLabel l)))
			       ; print "default:\n"
			       ; gotoLabel l
			       ; print "}\n")
			in
			   case (cases,            default) of
			      ([],               NONE) =>
				 Error.bug "switch: empty cases"
			    | ([(_, l)],         NONE)   => gotoLabel l
			    | ([],               SOME l) => gotoLabel l
			    | ((_, l) :: cases', NONE)   => switch (cases', l)
			    | (_,                SOME l) => switch (cases, l)
			end
		  in
		     case cases of
			Cases.Char l => doit (l, C.char)
		      | Cases.Int l =>
			   (case (l, default) of
			       ([(0, f), (1, t)], NONE) => bool (t, f)
			     | ([(1, t), (0, f)], NONE) => bool (t, f)
			     | _ => doit (l, C.int))
		      | Cases.Word l => doit (l, C.word)
		  end
	     | SwitchIP {test, int, pointer} =>
		  iff (concat ["IsInt (", Operand.toString test, ")"],
		       int, pointer)
	 end
   end

structure Chunk =
   struct
      open MachineOutput.Chunk
	 
      fun output (self as T {chunkLabel, entries, gcReturns, blocks, regMax},
		  print) =
	 let
	    datatype status = None | One | Many
	    val {get = labelInfo: Label.t -> {block: Block.t,
					      entry: bool ref,
					      status: status ref,
					      layedOut: bool ref},
		 set = setLabelInfo,
		 destroy = destroyLabelInfo} =
	       Property.destGetSetOnce
	       (Label.plist, Property.initRaise ("CCodeGen.info", Label.layout))
	    fun jump l =
	       let val {status, ...} = labelInfo l
	       in case !status of
		  None => status := One
		| One => status := Many
		| Many => ()
	       end
	    fun force l = #status (labelInfo l) := Many
	    (* Count how many times each label is jumped to. *)
	    val _ =
	       (List.foreach
		(blocks, fn b as Block.T {label, ...} =>
		 setLabelInfo (label, {block = b, 
				       entry = ref false,
				       status = ref None,
				       layedOut = ref false}))
		; List.foreach (entries, fn l =>
				let val {entry, ...} = labelInfo l
				in entry := true
				   ; jump l
				end)
		; (List.foreach
		   (blocks, fn Block.T {statements, transfer, ...} =>
		    let
		       datatype z = datatype Transfer.t
		    in
		       case transfer of
			  NearJump {label, ...} => jump label
			| Arith {overflow, success, ...} =>
			     (force overflow; jump success)
			| Switch {cases, default, ...} =>
			     (Cases.foreach (cases, force)
			      ; (case default
				    of NONE => ()
				  | SOME l => force l))
			| SwitchIP {int, pointer, ...} =>
			     (jump int; force pointer)
					| _ => ()
		    end)))
	    fun printGotoLabel l =
	       print (concat ["\tgoto ", Label.toString l, ";\n"])
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
		in if !layedOut 
		      then printGotoLabel l
		   else printLabelCode info
		end) arg
	    and printLabelCode arg =
	       tracePrintLabelCode
	       (fn {block = Block.T {label = l,
				     kind,
				     live, profileInfo = {func = profileInfoFunc, 
							  label = profileInfoLabel},
				     statements, transfer, ...},
				layedOut, status, ...} =>
	       let
		  val _ = layedOut := true
		  val _ = C.profile (profileInfoFunc, profileInfoFunc, print)
		  val _ =
		     case !status of
			Many =>
			   let val s = Label.toString l
			   in print s
			      ; print ":\n"
			   end 
		      | _ => ()

		  val _ =
		     print (let open Layout
			   in toString
			      (seq [str "/* live: ",
				   List.layout Operand.layout live,
				   str " */\n"])
			   end)
		     
		  val _ =
		     case kind
		       of Block.Kind.Func {args} => ()
			| Block.Kind.Jump => ()
			| Block.Kind.Cont {args, size}
			=> Statement.output (Statement.Push (~ size), print)
			| Block.Kind.Handler {offset}
			=> Statement.output (Statement.Push (~ offset), print)

		  val _ = 
		     Array.foreach (statements, fn s =>
				    Statement.output (s, print))
		  val _ = 
		     Transfer.output (transfer, print, gotoLabel,
				      maybePrintLabel)
	       in ()
	       end) arg
	    val numPointers = regMax Type.pointer
	    fun profChunkSwitch () =
	       C.profile ("ChunkSwitch (magic)", overhead, print)
	 in
	    C.profile ("Chunk (magic)", overhead, print);
	    C.call ("Chunk", [ChunkLabel.toString chunkLabel], print);
	    (* Declare registers. *)
	    List.foreach (let open Type
			 in [char, double, int, uint, pointer]
			 end,
			    fn ty => Int.for (0, regMax ty,
					     fn i => C.call (concat ["D", 
								   Type.name ty],
							    [C.int i],
							    print)));

	    profChunkSwitch ();
	    print "ChunkSwitch\n";
	    List.foreach (entries, fn l =>
			 (profChunkSwitch ()
			  ; print "case "
			  ; print (Label.toStringIndex l)
			  ; print ":\n"
			  ; gotoLabel l));
	    List.foreach (blocks,
			  Trace.trace
			  ("CLayoutBlock",
			   Label.layout o Block.label,
			   Unit.layout)
			  (fn Block.T {label, ...} =>
			   let
			      val info as {entry, layedOut, ...} = labelInfo label
			   in if !layedOut
			         orelse 
				 not (!entry)
				 then ()
			      else printLabelCode info
			   end))
	    ; profChunkSwitch ()
	    ; List.foreach (gcReturns, fn l =>
			    (print "case "
			     ; print (Label.toStringIndex l)
			     ; print ": goto "
			     ; print (Label.toString l)
			     ; print ";\n"))
	    ; C.profile ("EndChunk (magic)", overhead, print)
	    ; print "EndChunk\n"
	    ; destroyLabelInfo ()
	 end
   end

structure Program =
   struct
      open MachineOutput.Program

      fun output {program = program as T {globals, globalsNonRoot,
					 intInfs, strings, floats,
					 nextChunks,
					 frameOffsets, frameLayouts,
					 maxFrameSize,
					 chunks,
					 main = {chunkLabel, label}},
                  includes,
		  outputC: unit -> {file: File.t,
				    print: string -> unit,
				    done: unit -> unit}} =
	 let
	    val {print, done, ...} = outputC ()
	    fun outputIncludes () =
	       List.foreach (includes, fn i => (print "#include <";
						print i;
						print ">\n\n"))
	    fun declareGlobals () =
	       C.call ("Globals",
		      List.map (List.map (let open Type
					in [char, double, int, pointer, uint]
					end,
					   globals) @ [globalsNonRoot],
			       C.int),
		      print);
	    fun locals ty =
	       List.fold (chunks,
			 0,
			 fn (MachineOutput.Chunk.T {regMax, ...}, max)
			 => if regMax ty > max
			       then regMax ty
			    else max)
	    fun declareLocals () =
	       C.call ("Locals",
		      List.map (let open Type
			       in [char,double, int, pointer, uint]
			       end,
			       C.int o locals),
		      print)
	    fun declareIntInfs () =
	       (print "BeginIntInfs\n"; 
		List.foreach (intInfs, 
			     fn (g, s) 
			     => (C.callNoSemi ("IntInf",
					      [C.int (Global.index g),
					       C.string s],
					      print)
				 ; print "\n"));
		print "EndIntInfs\n")
	    fun declareStrings () =
	       (print "BeginStrings\n";
		List.foreach (strings, 
			     fn (g, s) 
			     => (C.callNoSemi ("String",
					      [C.int (Global.index g),
					       C.string s,
					       C.int (String.size s)],
					      print);
				 print "\n"));
		print "EndStrings\n");
	    fun declareFloats () =
	       (print "BeginFloats\n";
		List.foreach (floats, fn (g, f) =>
			     (C.callNoSemi ("Float",
					   [C.int (Global.index g),
					    C.float f],
					   print);
			      print "\n"));
		print "EndFloats\n");
	    fun declareChunks () =
	       List.foreach (chunks, fn Chunk.T {chunkLabel, ...} =>
			    C.call ("DeclareChunk",
				   [ChunkLabel.toString chunkLabel],
				   print));
	    (* Assign the entries of a chunk consecutive integers so that
	     * gcc will use a jump table.
	     *)
	    local
	       val allLabels = ref []
	       val {get: Label.t -> int, set, ...} =
		  Property.getSetOnce (Label.plist,
				      Property.initRaise ("index", Label.layout))
	       val indexCounter = Counter.new 0
	       val setIndex = fn l => (set (l, Counter.next indexCounter);
				       List.push (allLabels, l))
	       val _ =
		  List.foreach
		  (chunks, fn (Chunk.T {entries, gcReturns, ...}) =>
		   (List.foreach (entries, setIndex)
		    ; List.foreach (gcReturns, setIndex)))
	       val allLabels = List.rev (!allLabels)
	       val maxFrameIndex = Counter.value indexCounter
	       fun make (name, l, pr) =
		  (print (concat ["static ", name, " = {"])
		   ; List.foreachi (l, fn (i, x) =>
				   (if i > 0 then print ",\n\t" else ()
				    ; pr x))
		   ; print "};\n")
	    in
	       val maxFrameIndex = maxFrameIndex
	       fun declareNextChunks () =
		  make ("void ( *nextChunks []) ()",
		       allLabels,
		       fn l =>
		       case nextChunks l of
			  NONE => print "NULL"
			| SOME chunkLabel =>
			     C.callNoSemi ("Chunkp",
					  [ChunkLabel.toString chunkLabel],
					  print))
	       fun declareFrameOffsets () =
		  List.foreachi
		  (frameOffsets, fn (i, l) =>
		   (print (concat ["static ushort frameOffsets",
				  C.int i,
				  "[] = {"]);
		    print (C.int (List.length l));
		    List.foreach (l, 
				 fn i => (print ",";
					  print (C.int i)));
		    print "};\n"));
	       fun declareFrameLayouts () =
		  make ("GC_frameLayout frameLayouts []",
		       allLabels,
		       fn l => let
				  val {size, offsetIndex} 
				     = case frameLayouts l
				     of NONE => {size = "0", 
						 offsetIndex = "NULL"} 
				      | SOME {size, offsetIndex}
					=> {size = C.int size, 
					    offsetIndex
					    = "frameOffsets" ^ 
					    (C.int offsetIndex)}
			       in 
				  print (concat ["{", 
						size, ",", 
						offsetIndex, 
						"}"])
			       end)
	       fun declareIndices () =
		  List.foreach
		  (chunks, fn Chunk.T {entries, gcReturns,...} =>
		   List.foreach (entries @ gcReturns,
				fn l =>
				(print "#define ";
				 print (Label.toStringIndex l);
				 print " ";
				 print (C.int (get l));
				 print "\n")));
	    end
	    fun outputChunks () =
	       List.foreach (chunks, fn c => Chunk.output (c, print));
	    fun declareMain () =
	       let
		  val stringSizes =
		     List.fold (strings, 0, fn ((_, s), n)  =>
			       n + arrayHeaderSize
			       + Type.align (Type.pointer, String.size s))
		  val intInfSizes =
		     List.fold (intInfs, 
			       0, 
			       fn ((_, s), n) =>
			       n + intInfOverhead
			       + Type.align (Type.pointer, String.size s))
		  val liveSize = intInfSizes + stringSizes
		  val (useFixedHeap, fromSize) =
		     case !Control.fixedHeap of
			NONE => (C.falsee, 0)
		      | SOME n => (* div 2 for semispace *)
			   (if n > 0 andalso liveSize >= n div 2 
			       then Out.output (Out.error,
					       "Warning: heap size used with -h is too small to hold static data.\n")
			    else ();
			       (C.truee, n))
		  val magic = C.word (Random.useed ())
		  val (mainChunkId, mainLabel) 
		     = (ChunkLabel.toString chunkLabel, Label.toString label) 
	       in C.profile ("Main (magic)", overhead, print)
		  ; C.callNoSemi ("Main",
				 [useFixedHeap,
				  C.int fromSize,
				  C.int liveSize,
				  C.int maxFrameSize,
				  C.int maxFrameIndex,
				  magic,
				  mainChunkId,
				  mainLabel],
				 print)
		  ; print "\n"
	       end
	 in
	    print "#define CCODEGEN\n\n"
	    ; outputIncludes ()
	    ; declareGlobals ()
	    ; declareLocals ()
	    ; declareIntInfs ()
	    ; declareStrings ()
	    ; declareFloats ()
	    ; declareChunks ()
	    ; declareNextChunks ()
	    ; declareFrameOffsets ()
	    ; declareFrameLayouts ()
	    ; declareIndices ()
	    ; outputChunks ()
	    ; declareMain ()
	    ; done ()
	 end
   end

val output = Program.output
   
end
