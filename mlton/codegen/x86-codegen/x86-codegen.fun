(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor x86CodeGen(S: X86_CODEGEN_STRUCTS): X86_CODEGEN =
struct
  open S

  val wordSize: int = 4
  val pointerSize = wordSize
  val objectHeaderSize = wordSize
  val arrayHeaderSize = 2 * wordSize
  val intInfOverhead = arrayHeaderSize + wordSize (* for the sign *)

  structure x86 
    = x86(structure Label = MachineOutput.Label)

  structure x86MLton 
    = x86MLton(structure x86 = x86
	       structure MachineOutput = MachineOutput)

  structure x86Translate 
    = x86Translate(structure x86 = x86
		   structure x86MLton = x86MLton)

  structure x86Simplify
    = x86Simplify(structure x86 = x86)

  structure x86AllocateRegisters
    = x86AllocateRegisters(structure x86 = x86)

  structure x86Validate
    = x86Validate(structure x86 = x86)

  structure C =
    struct
      val truee = "TRUE"
      val falsee = "FALSE"
	
      fun args(ss: string list): string
	= concat("(" :: List.separate(ss, ", ") @ [")"])
         
      fun callNoSemi(f: string, xs: string list, print: string -> unit): unit 
	= (print f
	   ; print "("
	   ; (case xs 
		of [] => ()
		 | x :: xs => (print x
			       ; List.foreach(xs, 
					      fn x => (print ", "; print x))))
	   ; print ")")

      fun call(f, xs, print) = (callNoSemi(f, xs, print)
                                ; print ";\n")

      fun int(n: int): string 
	= if n >= 0
            then Int.toString n
	    else if n = Int.minInt
		   then "(int)0x80000000" (* because of goofy gcc warning *)
		   else "-" ^ String.dropPrefix(Int.toString n, 1)
      (* This overflows on Int32.minInt: Int32.toString(~ n) *)

      fun char(c: char) 
	= concat[if Char.ord c >= 0x80 then "(uchar)" else "",
		 "'", Char.escapeC c, "'"]

      fun word(w: Word.t) = "0x" ^ Word.toString w

      (* The only difference between SML floats and C floats is that
       * SML uses "~" while C uses "-".
       *)
      fun float s = String.translate(s, 
				     fn #"~" => "-" | c => String.fromChar c)

      fun string s 
	= let val quote = "\""
	  in concat[quote, String.escapeC s, quote]
	  end
    end

  open x86
  structure Type = MachineOutput.Type
  fun output {program as MachineOutput.Program.T 
	                 {globals,
			  globalsNonRoot,
			  intInfs,
			  strings,
			  floats, 
			  frameOffsets,
			  frameLayouts,
			  maxFrameSize,
			  chunks,
			  main,
			  ...}: MachineOutput.Program.t,
	      includes: string list,
	      outputC,
	      outputS}: unit
    = let
	val makeC = outputC
	val makeS = outputS

	val {get = getFrameLayoutIndex : Label.t -> int option, 
	     set = setFrameLayoutIndex}
	  = Property.getSetOnce(Label.plist,
				Property.initConst NONE)

	val return_labels
	  = List.fold
	    (chunks,
	     [],
	     fn (MachineOutput.Chunk.T {entries,gcReturns,...}, l)
	      => List.fold(entries @ gcReturns,
			   [],
			   fn (label, l')
			    => case frameLayouts label
				 of NONE => l'
			          | SOME _ => label::l') @ l)

	val frameLayoutsData
	  = List.removeDuplicates(List.map(return_labels, 
					   valOf o frameLayouts), 
				  op =)

	val _
	  = List.foreach
	    (return_labels,
	     fn return_label
	      => let
		   val info = valOf(frameLayouts return_label)
		 in
		   setFrameLayoutIndex(return_label,
				       List.index(frameLayoutsData,
						  fn info' => info = info'))
		 end)


	(* C specific *)
	fun outputC ()
	  = let
	      val {file, print, done} = makeC ()

	      fun make(name, l, pr)
		= (print (concat["static ", name, " = {"]);
		   List.foreachi(l,
				 fn (i,x) => (if i > 0 then print "," else ();
					      pr x));
		   print "};\n");

	      fun outputIncludes()
		= (List.foreach(includes,
				fn i => (print "#include <";
					 print i;
					 print ">\n"));
		   print "\n");

	      fun declareGlobals()
	        = C.call("Globals",
			 List.map(List.map(let 
					     open Type
					   in 
					     [char, double, int, pointer, uint]
					   end,
					   globals) @ [globalsNonRoot],
				  C.int),
			 print);

	      fun locals ty
		= List.fold(chunks,
			    0,
			    fn (MachineOutput.Chunk.T {regMax, ...},max)
			     => if regMax ty > max
				  then regMax ty
				  else max)
			  
	      fun declareLocals()
		= C.call("Locals",
			 List.map(List.map(let 
					     open Type
					   in 
					     [char, double, int, pointer, uint]
					   end,
					   locals),
				  C.int),
			 print);

	      fun declareIntInfs() 
		= (print "BeginIntInfs\n"; 
		   List.foreach
		   (intInfs, 
		    fn (g, s) 
		     => (C.callNoSemi("IntInf",
				      [C.int(MachineOutput.Global.index g),
				       C.string s],
				      print);
			 print "\n"));
		   print "EndIntInfs\n");
			  
	      fun declareStrings() 
		= (print "BeginStrings\n";
		   List.foreach
		   (strings, 
		    fn (g, s) 
		     => (C.callNoSemi("String",
				      [C.int(MachineOutput.Global.index g),
				       C.string s,
				       C.int(String.size s)],
				      print);
			 print "\n"));
		   print "EndStrings\n");

	      fun declareFloats()
		= (print "BeginFloats\n";
		   List.foreach
		   (floats,
		    fn (g, f)
		     => (C.callNoSemi("Float",
				      [C.int(MachineOutput.Global.index g),
				       C.float f],
				      print);
			 print "\n"));
		   print "EndFloats\n");

	      fun declareFrameOffsets()
		= List.foreachi
		  (frameOffsets,
		   fn (i,l) 
		    => (print (concat["static ushort frameOffsets",
				      C.int i,
				      "[] = {"]);
			print (C.int(List.length l));
			List.foreach(l, 
				     fn i => (print ",";
					      print (C.int i)));
			print "};\n"));

	      fun declareFrameLayouts()
		= make("GC_frameLayout frameLayouts[]",
		       frameLayoutsData,
		       fn {size, offsetIndex}
		        => print (concat["\n\t{", 
					 C.int size, ",", 
					 "frameOffsets" ^ (C.int offsetIndex), 
					 "}"]))

	      val maxFrameLayoutIndex = List.length frameLayoutsData
		
	      fun declareMain() 
		= let
		    val stringSizes 
		      = List.fold(strings, 
				  0, 
				  fn ((_, s), n) 
				  => n + arrayHeaderSize
				     + Type.align(Type.pointer,
						  String.size s))
		    val intInfSizes 
		      = List.fold(intInfs, 
				  0, 
				  fn ((_, s), n) 
				   => n + intInfOverhead
				        + Type.align(Type.pointer,
						     String.size s))
						     
		    val bytesLive = intInfSizes + stringSizes
		    val (usedFixedHeap, fromSize)
		      = case !Control.fixedHeap 
			  of NONE => (false, 0)
			   | SOME n 
			   => (* div 2 for semispace *)
			      (if n > 0 andalso bytesLive >= n div 2 
				 then Out.output(Out.error,
						 "Warning: heap size used with -h is too small to hold static data.\n")
				 else ();
			       (true, n))
		    val magic = C.word(Random.useed())
		    val mainLabel = Label.toString (#label main)
		  in 
		    C.callNoSemi("Main",
				 [if usedFixedHeap then C.truee else C.falsee,
				    C.int fromSize,
				    C.int bytesLive,
				    C.int maxFrameSize,
				    C.int maxFrameLayoutIndex,
				    magic,
				    mainLabel],
				 print);
		    print "\n"
		  end;
	    in
	      print "#define X86CODEGEN\n\n";
	      outputIncludes();
	      declareGlobals();
	      declareLocals();
	      declareIntInfs();
	      declareStrings();
	      declareFloats();
	      declareFrameOffsets();
	      declareFrameLayouts();
	      declareMain();
	      done ()
	    end 

        val outputC = Control.trace (Control.Pass, "outputC") outputC

	(* Assembly specific *)
	val block_begin
	  = [x86.Assembly.directive_assume
	     {register = x86.Register.edi,
	      memloc = x86MLton.gcState_stackTopContents,
	      weight = 2048,
	      sync = false,
	      reserve = false},
	     x86.Assembly.directive_assume
	     {register = x86.Register.esi,
	      memloc = x86MLton.gcState_frontierContents,
	      weight = 2048,
	      sync = false,
	      reserve = false}]
	val block_end
	  = [x86.Assembly.directive_cache
	     {register = x86.Register.edi,
	      memloc = x86MLton.gcState_stackTopContents,
	      reserve = true},
	     x86.Assembly.directive_cache
	     {register = x86.Register.esi,
	      memloc = x86MLton.gcState_frontierContents,
	      reserve = true}]
	val block_fall
	  = [x86.Assembly.directive_unreserve {register = x86.Register.edi},
	     x86.Assembly.directive_unreserve {register = x86.Register.esi}]
	val transferRegs
	  = [x86.Register.ebx,
	     x86.Register.bl,
	     x86.Register.ecx,
	     x86.Register.cl,
	     x86.Register.edx,
	     x86.Register.dl,
	     x86.Register.ebp]

	fun file_begin file
	  = [x86.Assembly.pseudoop_data (),
	     x86.Assembly.pseudoop_p2align 2,
	     x86.Assembly.label x86MLton.fileNameLabel,
	     x86.Assembly.pseudoop_string [file],
	     x86.Assembly.pseudoop_text ()]

	fun outputChunk (chunk as MachineOutput.Chunk.T {chunkLabel, ...},
			 print)
	  = let
	      val isMain 
		= MachineOutput.ChunkLabel.equals(#chunkLabel main, chunkLabel)

	      val {chunk as Chunk.T {exports, blocks},
		   liveInfo}
		= x86Translate.translateChunk 
		  {chunk = chunk}

	      (* for inlining frameLayouts info *)
	      fun block_pre label
		= Option.map(getFrameLayoutIndex label,
			     fn index
			      => [Assembly.pseudoop_long
				  [Immediate.const_int index]])

	      val pseudo_assembly : Assembly.t list list
		= x86Simplify.simplify 
		  {chunk = chunk,
		   (* don't perform optimizations on
		    * the main function (initGlobals)
		    *)
		   optimize = if isMain
				then 0
				else !Control.Native.optimize,
		   block_pre = block_pre,
		   block_begin = block_begin,
		   block_end = block_end,
		   block_fall = block_fall,
		   transferRegs = transferRegs,
		   liveInfo = liveInfo}

	      val allocated_assembly : Assembly.t list list
		= x86AllocateRegisters.allocateRegisters 
		  {assembly = pseudo_assembly,
		   (* don't calculate liveness info
		    * on the main function (initGlobals)
		    *)
		   liveness = not isMain}

	      val _ 
		= Assert.assert
		  ("x86CodeGen.output: invalid",
		   fn () => x86Validate.validate 
		            {assembly = allocated_assembly})

	      val validated_assembly : Assembly.t list
		= List.concat allocated_assembly
	    in
	      List.foreach(validated_assembly,
			   fn asm => print((Assembly.toString asm) ^ "\n"));
	      List.length validated_assembly
	    end
	  
	fun outputAssembly ()
	  = let
	      val split = !Control.Native.split
	      fun loop chunks
		= let
		    val {file, print, done} = makeS()
		    val _ = List.foreach
		            (file_begin file,
			     fn asm => print((Assembly.toString asm) ^ "\n"))
		    fun loop' (chunks, size) 
		      = case chunks
			  of [] => done ()
			   | chunk::chunks
			   => if (case split
				    of NONE => false
				     | SOME maxSize => size > maxSize)
				then (done (); loop (chunk::chunks))
				else loop'(chunks, 
					   size + outputChunk (chunk, print))
		  in 
		    loop' (chunks, 0)
		  end
	    in 
	      loop chunks;
	      x86Translate.translateChunk_totals ();
	      x86Simplify.simplify_totals ();
	      x86AllocateRegisters.allocateRegisters_totals ();
	      x86Validate.validate_totals ()
	    end

	val outputAssembly =
	   Control.trace (Control.Pass, "outputAssembly") outputAssembly
      in
	outputC();
	outputAssembly()
      end 
end
