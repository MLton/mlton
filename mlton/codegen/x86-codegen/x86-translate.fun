(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor x86Translate(S: X86_TRANSLATE_STRUCTS): X86_TRANSLATE =
struct

  open S

  val tracer = x86.tracer
  val tracerTop = x86.tracerTop

  val wordBytes = x86MLton.wordBytes
  val pointerBytes = x86MLton.pointerBytes
  val normalHeaderBytes = x86MLton.normalHeaderBytes
  val arrayHeaderBytes = x86MLton.arrayHeaderBytes
  val intInfOverheadBytes = x86MLton.intInfOverheadBytes

  fun argsToString(ss: string list): string
    = "(" ^ (concat (List.separate(ss, ", "))) ^ ")"

  structure Machine = x86MLton.Machine

  local
     open Machine
  in
     structure IntSize = IntSize
     structure IntX = IntX
     structure Label = Label
     structure Prim = Prim
     structure RealSize = RealSize
     structure RealX = RealX
     structure Register = Register
     structure Runtime = Runtime
     local
       open Runtime
     in
       structure GCField = GCField
     end
     structure Type = Type
     structure WordSize = WordSize
     structure WordX = WordX
  end

  datatype z = datatype IntSize.t
  datatype z = datatype RealSize.t
  datatype z = datatype WordSize.t
  
  structure Global =
     struct
	open Machine.Global

	fun toX86Operand (g: t) : (x86.Operand.t * x86.Size.t) vector =
	   let
	      val ty = Machine.Type.toCType (ty g)
	      val index = index g
	      val base =
		 x86.Immediate.label
		 (if isRoot g
		     then x86MLton.global_base ty
		  else x86MLton.globalPointerNonRoot_base)
	      val origin =
		 x86.MemLoc.imm
		 {base = base,
		  index = x86.Immediate.const_int index,
		  scale = x86.Scale.fromCType ty,
		  size = x86.Size.BYTE,
		  class = x86MLton.Classes.Globals}
	      val sizes = x86.Size.fromCType ty
	   in
	      (#1 o Vector.mapAndFold)
	      (sizes, 0, fn (size,offset) =>
	       (((x86.Operand.memloc o x86.MemLoc.shift)
		 {origin = origin,
		  disp = x86.Immediate.const_int offset,
		  scale = x86.Scale.One,
		  size = size}, size), offset + x86.Size.toBytes size))
	   end

	val toString = Layout.toString o layout
     end

  structure Operand =
    struct
      open Machine.Operand

      fun get (f: ('a * 'b) -> 'c) (i: int) (v: ('a * 'b) vector) =
	 f (Vector.sub (v, i))
	 handle _ => Error.bug (concat ["toX86Operand: get"])
      fun getOp0 v =
	 get #1 0 v

      val rec toX86Operand : t -> (x86.Operand.t * x86.Size.t) vector =
	 fn ArrayOffset {base, index, ty}
 	    => let
		  val base = toX86Operand base
		  val _ = Assert.assert("x86Translate.Operand.toX86Operand: Array/base",
					fn () => Vector.length base = 1)
		  val base = getOp0 base
		  val index = toX86Operand index
		  val _ = Assert.assert("x86Translate.Operand.toX86Operand: Array/index",
				       fn () => Vector.length index = 1)
		  val index = getOp0 index
		     
		  val ty = Type.toCType ty
		  val origin =
		     case (x86.Operand.deMemloc base,
			   x86.Operand.deImmediate index,
			   x86.Operand.deMemloc index) of
			(SOME base, SOME index, _) =>
			   x86.MemLoc.simple 
			   {base = base,
			    index = index,
			    scale = x86.Scale.fromCType ty,
			    size = x86.Size.BYTE,
			    class = x86MLton.Classes.Heap}
		      | (SOME base, _, SOME index) =>
			   x86.MemLoc.complex 
			   {base = base,
			    index = index,
			    scale = x86.Scale.fromCType ty,
			    size = x86.Size.BYTE,
			    class = x86MLton.Classes.Heap}
		      | _ => Error.bug (concat ["toX86Operand: strange Offset:",
						" base: ",
						x86.Operand.toString base,
						" index: ",
						x86.Operand.toString index])
		  val sizes = x86.Size.fromCType ty
	       in
		  (#1 o Vector.mapAndFold)
		  (sizes, 0, fn (size,offset) =>
		   (((x86.Operand.memloc o x86.MemLoc.shift)
		     {origin = origin,
		      disp = x86.Immediate.const_int offset,
		      scale = x86.Scale.One,
		      size = size}, size), offset + x86.Size.toBytes size))
	       end
	  | Cast (z, _) => toX86Operand z
	  | Contents {oper, ty} =>
	       let
		  val ty = Type.toCType ty
		  val base = toX86Operand oper
		  val _ = Assert.assert("x86Translate.Operand.toX86Operand: Contents/base",
					fn () => Vector.length base = 1)
		  val base = getOp0 base
		  val offset = x86.Immediate.const_int 0
		  val origin =
		     case x86.Operand.deMemloc base of
			SOME base =>
			   x86.MemLoc.simple 
			   {base = base,
			    index = x86.Immediate.const_int 0,
			    scale = x86.Scale.One,
			    size = x86.Size.BYTE,
			    class = x86MLton.Classes.Heap}
		      | _ => Error.bug (concat
					["toX86Operand: strange Contents",
					 " base: ",
					 x86.Operand.toString base])	
		  val sizes = x86.Size.fromCType ty
	       in
		  (#1 o Vector.mapAndFold)
		  (sizes, 0, fn (size,offset) =>
		   (((x86.Operand.memloc o x86.MemLoc.shift)
		     {origin = origin,
		      disp = x86.Immediate.const_int offset,
		      scale = x86.Scale.One,
		      size = size}, size), offset + x86.Size.toBytes size))
	       end
	  | File => Vector.new1 (x86MLton.fileName, x86MLton.pointerSize)
	  | Frontier => 
	       let 
		  val frontier = x86MLton.gcState_frontierContentsOperand ()
	       in
		  Vector.new1 (frontier, valOf (x86.Operand.size frontier))
	       end
	  | GCState => 
	       Vector.new1 (x86.Operand.label x86MLton.gcState_label,
			    x86MLton.pointerSize)
	  | Global g => Global.toX86Operand g
	  | Int i =>
	       let
		  val i'' = fn () => x86.Operand.immediate_const_int (IntX.toInt i)
	       in
		  case IntX.size i of
		     I8 => Vector.new1 (i'' (), x86.Size.BYTE)
		   | I16 => Vector.new1 (i'' (), x86.Size.WORD)
		   | I32 => Vector.new1 (i'' (), x86.Size.LONG)
		   | I64 => let
			       fun convert1 (ii: IntInf.t): Word.t * Word.t =
				  let
				     val lo = Word.fromIntInf ii
				     val ii = IntInf.~>> (ii, 0w32)
				     val hi = Word.fromIntInf ii
				  in
				     (lo, hi)
				  end
			       fun convert2 (ii: IntInf.t): Word.t * Word.t =
				  let
				     fun finish (iis: String.t, c: Char.t) =
					let
					   val s =
					      String.concat
					      [String.tabulate
					       (16 - String.size iis, fn _ => c),
					       iis]
					   fun cvt s = valOf (Word.fromString s)
					   val lo = cvt(String.extract(s, 8, SOME 8))
					   val hi = cvt(String.extract(s, 0, SOME 8))
					in
					   (lo, hi)
					end
				  in
				     if IntInf.<(ii, IntInf.fromInt 0)
					then let
						val ii = IntInf.-(IntInf.~ ii, IntInf.fromInt 1)
						val iis =
						   String.translate
						   (IntInf.format(ii, StringCvt.HEX),
						    fn #"0" => "F"
						     | #"1" => "E"
						     | #"2" => "D"
						     | #"3" => "C"
						     | #"4" => "B"
						     | #"5" => "A"
						     | #"6" => "9"
						     | #"7" => "8"
						     | #"8" => "7"
						     | #"9" => "6"
						     | #"A" => "5"
						     | #"B" => "4"
						     | #"C" => "3"
						     | #"D" => "2"
						     | #"E" => "1"
						     | #"F" => "0"
						     | #"a" => "5"
						     | #"b" => "4"
						     | #"c" => "3"
						     | #"d" => "2"
						     | #"e" => "1"
						     | #"f" => "0"
						     | c => "")
					     in
						finish (iis, #"F")
					     end
					else finish (IntInf.format(ii, StringCvt.HEX), #"0")
				  end
			       val ii = IntX.toIntInf i
			       val (lo, hi) = 
				 if MLton.isMLton 
				   then convert1 ii
				   else convert2 ii
			    in
			       Vector.new2
			       ((x86.Operand.immediate_const_word lo, x86.Size.LONG),
				(x86.Operand.immediate_const_word hi, x86.Size.LONG))
			    end
	       end
	  | Label l => 
	       Vector.new1 (x86.Operand.immediate_label l, x86MLton.pointerSize)
	  | Line => 
	       Vector.new1 (x86MLton.fileLine (), x86MLton.wordSize)
	  | Offset {base = GCState, offset, ty} =>
	       let
		  val ty = Type.toCType ty
		  val offset = x86MLton.gcState_offset {offset = offset, ty = ty}
	       in
		  Vector.new1 (offset, valOf (x86.Operand.size offset))
	       end
	  | Offset {base, offset, ty} =>
	       let
		 val ty = Type.toCType ty
		 val base = toX86Operand base
		 val _ = Assert.assert("x86Translate.Operand.toX86Operand: Contents/base",
				       fn () => Vector.length base = 1)
		 val base = getOp0 base
		 val origin =
		   case x86.Operand.deMemloc base of
		     SOME base =>
		       x86.MemLoc.simple 
		       {base = base,
			index = x86.Immediate.const_int offset,
			scale = x86.Scale.One,
			size = x86.Size.BYTE,
			class = x86MLton.Classes.Heap}
		   | _ => Error.bug (concat ["toX86Operand: strange Offset:",
					     " base: ",
					     x86.Operand.toString base])
		  val sizes = x86.Size.fromCType ty
	       in
		  (#1 o Vector.mapAndFold)
		  (sizes, 0, fn (size,offset) =>
		   (((x86.Operand.memloc o x86.MemLoc.shift)
		     {origin = origin,
		      disp = x86.Immediate.const_int offset,
		      scale = x86.Scale.One,
		      size = size}, size), offset + x86.Size.toBytes size))
	       end
	  | Real _ => Error.bug "toX86Operand: Real unimplemented"
	  | Register r =>
	       let
		  val ty = Machine.Type.toCType (Register.ty r)
		  val index = Machine.Register.index r
		  val base = x86.Immediate.label (x86MLton.local_base ty)
		  val sizes = x86.Size.fromCType ty
		  val origin =
		     x86.MemLoc.imm
		     {base = base,
		      index = x86.Immediate.const_int index,
		      scale = x86.Scale.fromCType ty,
		      size = x86.Size.BYTE,
		      class = x86MLton.Classes.Locals}
		  val sizes = x86.Size.fromCType ty
	       in
		  (#1 o Vector.mapAndFold)
		  (sizes, 0, fn (size,offset) =>
		   (((x86.Operand.memloc o x86.MemLoc.shift)
		     {origin = origin,
		      disp = x86.Immediate.const_int offset,
		      scale = x86.Scale.One,
		      size = size}, size), offset + x86.Size.toBytes size))
	       end
	  | SmallIntInf ii => 
	       Vector.new1 (x86.Operand.immediate_const_word ii,x86.Size.LONG)
	  | StackOffset {offset, ty} =>
	       let
		  val ty = Type.toCType ty
		  val origin =
		     x86.MemLoc.simple 
		     {base = x86MLton.gcState_stackTopContents (), 
		      index = x86.Immediate.const_int offset,
		      scale = x86.Scale.One,
		      size = x86.Size.BYTE,
		      class = x86MLton.Classes.Stack}
		  val sizes = x86.Size.fromCType ty
	       in
		  (#1 o Vector.mapAndFold)
		  (sizes, 0, fn (size,offset) =>
		   (((x86.Operand.memloc o x86.MemLoc.shift)
		     {origin = origin,
		      disp = x86.Immediate.const_int offset,
		      scale = x86.Scale.One,
		      size = size}, size), offset + x86.Size.toBytes size))
	       end
	  | StackTop => 
	       let 
		  val stackTop = x86MLton.gcState_stackTopContentsOperand ()
	       in
		  Vector.new1 (stackTop, valOf (x86.Operand.size stackTop))
	       end
	  | Word w =>
	       let
		  fun single size =
		     Vector.new1
		     (x86.Operand.immediate_const_word
		      (Word.fromLarge (WordX.toLargeWord w)),
		      size)
	       in
		  case WordX.size w of
		     W8 => single x86.Size.BYTE
		   | W16 => single x86.Size.WORD
		   | W32 => single x86.Size.LONG
		   | W64 =>
			let
			   val w = WordX.toLargeWord w
			   val lo = Word.fromLarge w
			   val hi = Word.fromLarge (LargeWord.>> (w, 0w32))
			in
			   Vector.new2
			   ((x86.Operand.immediate_const_word lo, x86.Size.LONG),
			    (x86.Operand.immediate_const_word hi, x86.Size.LONG))
			end
	       end
	       
      val toX86Operand =
	 fn operand =>
	 toX86Operand operand
	 handle exn => Error.reraise (exn, "x86Translate.Operand.toX86Operand")
    end

  type transInfo = x86MLton.transInfo

  structure Entry =
    struct
      structure Kind = Machine.Kind
	 
      fun toX86Blocks {label, kind, 
		       transInfo as {frameInfoToX86, live, liveInfo,
				     ...}: transInfo}
	= (
	   x86Liveness.LiveInfo.setLiveOperands
	   (liveInfo, label, live label);
	   case kind
	     of Kind.Jump
	      => let
		 in
		   AppendList.single
		   (x86.Block.mkBlock'
		    {entry = SOME (x86.Entry.jump {label = label}),
		     statements = [],
		     transfer = NONE})
		 end
	      | Kind.Func
	      => let
		   val args
		     = List.fold
		       (live label,
			x86.MemLocSet.empty,
			fn (operand, args)
			 => case x86.Operand.deMemloc operand
			      of SOME memloc => x86.MemLocSet.add(args, memloc)
			       | NONE => args)
		 in
		   AppendList.single
		   (x86.Block.mkBlock'
		    {entry = SOME (x86.Entry.func {label = label,
						   live = args}),
		     statements = [],
		     transfer = NONE})
		 end
	      | Kind.Cont {args, frameInfo, ...}
	      => let
		    val frameInfo = frameInfoToX86 frameInfo
		    val args =
		       Vector.fold
		       (args, x86.MemLocSet.empty,
			fn (operand,args) =>
			Vector.fold
			(Operand.toX86Operand operand, args,
			 fn ((operand,size),args) =>
			 case x86.Operand.deMemloc operand of
			    SOME memloc => x86.MemLocSet.add(args, memloc)
			  | NONE => args))
		 in
		   AppendList.single
		   (x86.Block.mkBlock'
		    {entry = SOME (x86.Entry.cont {label = label,
						   live = args,
						   frameInfo = frameInfo}),
		     statements = [],
		     transfer = NONE})
		 end
	      | Kind.Handler {frameInfo, ...}
	      => let
		 in 
		   AppendList.single
		   (x86.Block.mkBlock'
		    {entry = SOME (x86.Entry.handler
				   {frameInfo = frameInfoToX86 frameInfo,
				    label = label,
				    live = x86.MemLocSet.empty}),
		     statements = [],
		     transfer = NONE})
		 end
	      | Kind.CReturn {dst, frameInfo, func}
	      => let
		   val dsts =
		      case dst of
			 NONE => Vector.new0 ()
		       | SOME dst => Operand.toX86Operand dst
		 in
		   x86MLton.creturn
		   {dsts = dsts,
		    frameInfo = Option.map (frameInfo, frameInfoToX86),
		    func = func,
		    label = label,
		    transInfo = transInfo}
		 end)
    end

  structure Statement =
    struct
      open Machine.Statement

      fun comments statement
	= if !Control.Native.commented > 0
	    then let
		   val comment = (Layout.toString o layout) statement
		 in
		   (AppendList.single
		    (x86.Block.mkBlock'
		     {entry = NONE,
		      statements = [x86.Assembly.comment
				    (concat ["begin: ",
					     comment])],
		      transfer = NONE}),
		    AppendList.single
		    (x86.Block.mkBlock'
		     {entry = NONE,
		      statements = [x86.Assembly.comment
				    (concat ["end: ",
					     comment])],
		      transfer = NONE}))
		 end
	    else (AppendList.empty,AppendList.empty)

      fun toX86Blocks {statement,
		       transInfo as {liveInfo, ...} : transInfo}
	= (case statement
	     of Noop
	      => AppendList.empty
	      | Move {src, dst}
	      => let
		   val (comment_begin,
			comment_end) = comments statement
		     
		   val dsts = Operand.toX86Operand dst
		   val srcs = Operand.toX86Operand src
		 in
		   AppendList.appends
		   [comment_begin,
		    AppendList.single
		    (x86.Block.mkBlock'
		     {entry = NONE,
		      statements
		      = (Vector.toList o Vector.map2)
		        (dsts,srcs,fn ((dst,dstsize),(src,srcsize)) =>
			 (* dst = src *)
			 case x86.Size.class srcsize
			    of x86.Size.INT => x86.Assembly.instruction_mov 
			                       {dst = dst,
						src = src,
						size = srcsize}
			  | x86.Size.FLT => x86.Assembly.instruction_pfmov
					    {dst = dst,
					     src = src,
					     size = srcsize}
			  | _ => Error.bug "toX86Blocks: Move"),
		      transfer = NONE}),
		    comment_end]
		 end 
	      | PrimApp {dst, prim, args}
   	      => let
		   val (comment_begin, comment_end) = comments statement
		   val args = (Vector.concatV o Vector.map)
		              (args, Operand.toX86Operand)
		   val dsts = 
		      case dst of
			 NONE => Vector.new0 ()
		       | SOME dst => Operand.toX86Operand dst
		 in
		   AppendList.appends
		   [comment_begin,
		    (x86MLton.prim {prim = prim,
				    args = args,
				    dsts = dsts,
				    transInfo = transInfo}),
		    comment_end]
		 end
	      | ProfileLabel l =>
		   AppendList.single
		   (x86.Block.mkProfileBlock'
		    {profileLabel = l})
	      | Object {dst, header, size, stores}
	      => let
		   val (comment_begin,
			comment_end) = comments statement
		   val (dst,dstsize) = Vector.sub(Operand.toX86Operand dst, 0)
		   val dst' = case x86.Operand.deMemloc dst
				of SOME dst' => dst'
				 | NONE => Error.bug "Allocate: strange dst"
		       
		   val frontier = x86MLton.gcState_frontierContentsOperand ()
		   val frontierDeref = x86MLton.gcState_frontierDerefOperand ()
		   val frontierPlusOHW
		     = (x86.Operand.memloc o x86.MemLoc.simple)
		       {base = x86MLton.gcState_frontierContents (), 
			index = x86.Immediate.const_int normalHeaderBytes,
			scale = x86.Scale.One,
			size = x86MLton.pointerSize,
			class = x86MLton.Classes.Heap}
		       
		   fun stores_toX86Assembly ({offset, value}, l)
		     = let
			 val origin =
			    x86.MemLoc.simple
			    {base = dst',
			     index = x86.Immediate.const_int offset,
			     scale = x86.Scale.One,
			     size = x86.Size.BYTE,
			     class = x86MLton.Classes.Heap}
		       in
			 (
			 (Vector.toList o #1 o Vector.mapAndFold)
			 (Operand.toX86Operand value, 0, fn ((src,srcsize),offset) =>
			  let
			     val dst =
				(x86.Operand.memloc o x86.MemLoc.shift)
				{origin = origin,
				 disp = x86.Immediate.const_int offset,
				 scale = x86.Scale.One,
				 size = srcsize}
			  in
			     (case x86.Size.class srcsize of 
				 x86.Size.INT => 
				    x86.Assembly.instruction_mov 
				    {dst = dst,
				     src = src,
				     size = srcsize}
			       | x86.Size.FLT => 
				    x86.Assembly.instruction_pfmov
				    {dst = dst,
				     src = src,
				     size = srcsize}
			       | _ => Error.bug "toX86Blocks: Allocate",
			      offset + x86.Size.toBytes srcsize)
			  end)) @ l
		       end
		 in
		   AppendList.appends
		   [comment_begin,
		    AppendList.single
		    (x86.Block.mkBlock'
		     {entry = NONE,
		      statements
		      = ((* *(frontier) = header *)
			 x86.Assembly.instruction_mov 
			 {dst = frontierDeref,
			  src = x86.Operand.immediate_const_word header,
			  size = x86MLton.pointerSize})::
		        ((* dst = frontier + objectHeaderSize *)
			 x86.Assembly.instruction_lea
			 {dst = dst,
			  src = frontierPlusOHW,
			  size = x86MLton.pointerSize})::
			(Vector.foldr(stores,
				      [(* frontier += size *)
				       x86.Assembly.instruction_binal
				       {oper = x86.Instruction.ADD,
					dst = frontier,
					src = x86.Operand.immediate_const_int size,
					size = x86MLton.pointerSize}],
				      stores_toX86Assembly)),
		      transfer = NONE}),
		    comment_end]
		 end)
	  handle exn
	   => Error.reraise (exn, concat ["x86Translate.Statement.toX86Blocks::",
					  Layout.toString (layout statement)])
    end

  structure Transfer =
    struct
      open Machine.Transfer

      fun goto l
	= AppendList.single
	  (x86.Block.mkBlock'
	   {entry = NONE,
	    statements = [],
	    transfer = SOME (x86.Transfer.goto
			     {target = l})})
 
      fun iff (test, a, b)
	= let
	    val (test,testsize) =
	       Vector.sub (Operand.toX86Operand test, 0)
	  in
	    if Label.equals(a, b)
	      then AppendList.single
		   (x86.Block.mkBlock'
		    {entry = NONE,
		     statements = [],
		     transfer = SOME (x86.Transfer.goto {target = a})})
	      else AppendList.single
		   ((* if (test) goto a
		     * goto b
		     *)
		    x86.Block.mkBlock'
		    {entry = NONE,
		     statements 
		     = [x86.Assembly.instruction_test
			{src1 = test,
			 src2 = test,
			 size = testsize}],
		     transfer
		     = SOME (x86.Transfer.iff
			     {condition = x86.Instruction.NZ,
			      truee = a,
			      falsee = b})})
	  end

      fun cmp (test, k, a, b)
	= let
	    val (test,testsize) =
	       Vector.sub (Operand.toX86Operand test, 0)
	  in
	    if Label.equals(a, b)
	      then AppendList.single
		   (x86.Block.mkBlock'
		    {entry = NONE,
		     statements = [],
		     transfer = SOME (x86.Transfer.goto {target = a})})
	      else AppendList.single
		   ((* if (test = k) goto a
		     * goto b
		     *)
		    x86.Block.mkBlock'
		    {entry = NONE,
		     statements 
		     = [x86.Assembly.instruction_cmp
			{src1 = test,
			 src2 = x86.Operand.immediate k,
			 size = testsize}],
		     transfer
		     = SOME (x86.Transfer.iff
			     {condition = x86.Instruction.E,
			      truee = a,
			      falsee = b})})
	  end

      fun switch(test, cases, default)
	= let
	    val test = Operand.toX86Operand test
	    val (test,testsize) = Vector.sub(test, 0)
	  in
	    AppendList.single
	    (x86.Block.mkBlock'
	     {entry = NONE,
	      statements = [],
	      transfer = SOME (x86.Transfer.switch
			       {test = test,
				cases = cases,
				default = default})})
	  end

      fun doSwitchChar (test, cases, default)
	= (case (cases, default)
	     of ([],            NONE)
	      => Error.bug "toX86Blocks: doSwitchChar"
	      | ([(_,l)],       NONE) => goto l
	      | ([],            SOME l) => goto l
	      | ([(#"\000",f),(#"\001",t)], NONE) => iff(test,t,f)
	      | ([(#"\001",t),(#"\000",f)], NONE) => iff(test,t,f)
	      | ([(_,l),(k',l')],NONE) 
	      => cmp(test,x86.Immediate.const_char k',l',l)
	      | ([(k',l')],      SOME l)
	      => cmp(test,x86.Immediate.const_char k',l',l)
	      | ((_,l)::cases,  NONE) 
	      => switch(test, x86.Transfer.Cases.char cases, l)
	      | (cases,         SOME l) 
	      => switch(test, x86.Transfer.Cases.char cases, l))

      fun doSwitchInt (test, cases, default)
	= (case (cases, default)
	     of ([],             NONE)
	      => Error.bug "toX86Blocks: doSwitchInt"
	      | ([(_,l)],        NONE) => goto l
	      | ([],             SOME l) => goto l
	      | ([(0,f),(1,t)],  NONE) => iff(test,t,f)
	      | ([(1,t),(0,f)],  NONE) => iff(test,t,f)
	      | ([(_,l),(k',l')],NONE) 
	      => cmp(test,x86.Immediate.const_int k',l',l)
	      | ([(k',l')],      SOME l)
	      => cmp(test,x86.Immediate.const_int k',l',l)
	      | ((_,l)::cases,   NONE) 
	      => switch(test, x86.Transfer.Cases.int cases, l)
	      | (cases,          SOME l) 
	      => switch(test, x86.Transfer.Cases.int cases, l))

      fun doSwitchWord (test, cases, default)
	= (case (cases, default)
	     of ([],            NONE)
	      => Error.bug "toX86Blocks: doSwitchWord"
	      | ([(_,l)],       NONE) => goto l
	      | ([],            SOME l) => goto l
	      | ([(0wx0,f),(0wx1,t)], NONE) => iff(test,t,f)
	      | ([(0wx1,t),(0wx0,f)], NONE) => iff(test,t,f)
	      | ([(_,l),(k',l')],NONE) 
	      => cmp(test,x86.Immediate.const_word k',l',l)
	      | ([(k',l')],      SOME l)
	      => cmp(test,x86.Immediate.const_word k',l',l)
	      | ((_,l)::cases,  NONE) 
	      => switch(test, x86.Transfer.Cases.word cases, l)
	      | (cases,         SOME l) 
	      => switch(test, x86.Transfer.Cases.word cases, l))

      fun comments transfer
	= if !Control.Native.commented > 0
	    then let
		   val comment = (Layout.toString o layout) transfer
		 in
		   AppendList.single
		   (x86.Block.mkBlock'
		    {entry = NONE,
		     statements = [x86.Assembly.comment comment],
		      transfer = NONE})
		 end
	    else AppendList.empty

	 
      fun toX86Blocks {returns, transfer,
		       transInfo as {frameInfoToX86, ...}: transInfo}
	= (case transfer
	     of Arith {prim, args, dst, overflow, success, ty}
	      => let
		   val args = (Vector.concatV o Vector.map)
		              (args, Operand.toX86Operand)
		   val dsts = Operand.toX86Operand dst
		 in
		   AppendList.append
		   (comments transfer,
		    x86MLton.arith {prim = prim,
				    args = args,
				    dsts = dsts,
				    overflow = overflow,
				    success = success,
				    transInfo = transInfo})
		 end
	      | CCall {args, frameInfo, func, return}
	      => let
		   val args = (Vector.concatV o Vector.map)
		              (args, Operand.toX86Operand)
		 in
		   AppendList.append
		   (comments transfer,	
		    x86MLton.ccall {args = args,
				    frameInfo = (Option.map
						 (frameInfo, frameInfoToX86)),
				    func = func,
				    return = return,
				    transInfo = transInfo})
		 end
	      | Return
	      => AppendList.append
	         (comments transfer,
		  AppendList.single
		  (x86.Block.mkBlock'
		   {entry = NONE,
		    statements = [],
		    transfer 
		    = SOME (x86.Transfer.return 
			    {live 
			     = Vector.fold
			       ((case returns of
				    NONE => Error.bug "strange Return"
				  | SOME zs => zs),
				x86.MemLocSet.empty,
				fn (operand, live) =>
				Vector.fold
				(Operand.toX86Operand operand, live,
				 fn ((operand,size),live) =>
				 case x86.Operand.deMemloc operand of
				    SOME memloc => x86.MemLocSet.add(live, memloc)
				  | NONE => live))})}))
  	      | Raise
	      => AppendList.append
	         (comments transfer,
		  AppendList.single
		  (x86.Block.mkBlock'
		   {entry = NONE,
		    statements = [],
		    transfer 
		    = SOME (x86.Transfer.raisee 
			    {live 
			     = x86.MemLocSet.add
			       (x86.MemLocSet.add
				(x86.MemLocSet.empty,
				 x86MLton.gcState_stackBottomContents ()),
				x86MLton.gcState_exnStackContents ())})}))
	      | Switch switch
              => let
		    datatype z = datatype Machine.Switch.t
		    fun simple ({cases, default, test}, doSwitch) =
		       AppendList.append
		       (comments transfer,
			doSwitch (test, Vector.toList cases, default))
			
		 in
		    case switch of
		       EnumPointers {enum, pointers, test} =>
			  let
			     val (test,testsize) =
				Vector.sub(Operand.toX86Operand test, 0)
			  in
			     AppendList.append
			     (comments transfer,
			      AppendList.single
			      ((* if (test & 0x3) goto int 
				* goto pointer
				*)
			       x86.Block.mkBlock'
			       {entry = NONE,
				statements 
				= [x86.Assembly.instruction_test
				   {src1 = test,
				    src2 = x86.Operand.immediate_const_word 0wx3,
				    size = testsize}],
				transfer 
				= SOME (x86.Transfer.iff
					{condition = x86.Instruction.NZ,
					 truee = enum,
					 falsee = pointers})}))
			  end
		     | Int {cases, default, size, test} =>
			  (Assert.assert("x86Translate.Transfer.toX86Blocks: Switch/Int", 
					 fn () => size <> IntSize.I64)
			   ; simple ({cases = (Vector.map
					       (cases, fn (i, l) =>
						(IntX.toInt i, l))),
				      default = default,
				      test = test},
				     doSwitchInt))
		     | Pointer {cases, default, tag, ...} =>
			  simple ({cases = (Vector.map
					    (cases, fn {dst, tag, ...} =>
					     (tag, dst))),
				   default = default,
				   test = tag},
				  doSwitchInt)
		     | Word {cases, default, size, test} =>
			  simple ({cases = (Vector.map
					    (cases, fn (w, l) =>
					     (Word.fromLarge
					      (WordX.toLargeWord w),
					      l))),
				   default = default,
				   test = test},
				  doSwitchWord)
		 end
	      | Goto label
	      => (AppendList.append
		  (comments transfer,
		   AppendList.single
		   ((* goto label *)
		    x86.Block.mkBlock'
		    {entry = NONE,
		     statements = [],
		     transfer = SOME (x86.Transfer.goto {target = label})})))
	      | Call {label, live, return, ...}
	      => let
		    val live =
		       Vector.fold
		       (live, x86.MemLocSet.empty, fn (operand, live) =>
			Vector.fold
			(Operand.toX86Operand operand, live, fn ((operand,size),live) =>
			 case x86.Operand.deMemloc operand of
			    NONE => live
			  | SOME memloc => x86.MemLocSet.add (live, memloc)))
		    val com = comments transfer
		    val transfer =
		       case return of
			  NONE => x86.Transfer.tail {target = label,
						     live = live}
			| SOME {return, handler, size} =>
			     x86.Transfer.nontail {target = label,
						   live = live,
						   return = return,
						   handler = handler,
						   size = size}
		 in
		    AppendList.append
		    (com,
		     AppendList.single
		     (x86.Block.mkBlock' {entry = NONE,
				    statements = [],
				    transfer = SOME transfer}))
		 end)
	  handle exn
	   => Error.reraise (exn, "x86Translate.Transfer.toX86Blocks")
    end

  structure Block =
    struct
      open Machine.Block

      fun toX86Blocks {block as T {label, 
				   live, 
				   kind, 
				   raises,
				   returns,
				   statements, 
				   transfer,
				   ...},
		       transInfo as {...} : transInfo}
	= let
	    val pseudo_blocks
	      = AppendList.append
	        (AppendList.snoc
		 (Entry.toX86Blocks {label = label,
				     kind = kind,
				     transInfo = transInfo},
		  x86.Block.mkBlock'
		  {entry = NONE,
		   statements 
		   = if !Control.Native.commented > 0
		       then let
			      val comment
				= "Live: " ^
				  (argsToString
				   (Vector.toListMap
				    (live, fn l => Operand.toString l)))
			    in
			      [x86.Assembly.comment comment]
			    end
		       else [],
		    transfer = NONE}),
		 Vector.foldr(statements,
			      (Transfer.toX86Blocks {returns = returns,
						     transfer = transfer,
						     transInfo = transInfo}),
			      fn (statement,l)
			       => AppendList.append
			          (Statement.toX86Blocks 
				   {statement = statement,
				    transInfo = transInfo}, l)))

	    val pseudo_blocks = AppendList.toList pseudo_blocks

	    val blocks = x86.Block.compress pseudo_blocks
	  in
	    blocks
	  end
	  handle exn
	   => Error.reraise (exn, "x86Translate.Block.toX86Blocks")
    end

  structure Chunk =
    struct
      open Machine.Chunk

      fun toX86Chunk {chunk as T {blocks, ...}, 
		      frameInfoToX86,
		      liveInfo}
	= let
	    val data = ref []
	    val addData = fn l => List.push (data, l)
	    val _ = addData [x86.Assembly.pseudoop_data ()]
	    val {get = live : Label.t -> x86.Operand.t list,
		 set = setLive, 
		 rem = remLive, ...}
	      = Property.getSetOnce
	        (Label.plist, Property.initRaise ("live", Label.layout))
	    val _ = Vector.foreach
	            (blocks, fn Block.T {label, live, ...} =>
		     setLive (label,
			      (Vector.toList o #1 o Vector.unzip o 
			       Vector.concatV o Vector.map)
			      (live, Operand.toX86Operand)))
	    val transInfo = {addData = addData,
			     frameInfoToX86 = frameInfoToX86,
			     live = live,
			     liveInfo = liveInfo}
	    val x86Blocks 
	      = List.concat (Vector.toListMap
			     (blocks, 
				fn block
				 => Block.toX86Blocks 
				    {block = block,
				     transInfo = transInfo}))
	    val _ = Vector.foreach (blocks, fn Block.T {label, ...} =>
				    remLive label)
	    val _ = addData [x86.Assembly.pseudoop_text ()]
	    val data = List.concatRev (!data)
	  in
	    x86.Chunk.T {data = data, blocks = x86Blocks}
	  end
	  handle exn
	   => Error.reraise (exn, "x86Translate.Chunk.toX86Chunk")
    end

  fun translateChunk {chunk: x86MLton.Machine.Chunk.t,
		      frameInfoToX86,
		      liveInfo: x86Liveness.LiveInfo.t}:
                     {chunk: x86.Chunk.t}
    = {chunk = Chunk.toX86Chunk {chunk = chunk,
				 frameInfoToX86 = frameInfoToX86,
				 liveInfo = liveInfo}}

  val (translateChunk, translateChunk_msg)
    = tracerTop
      "translateChunk"
      translateChunk

  fun translateChunk_totals ()
    = (translateChunk_msg ();
       Control.indent ();
       Control.unindent ())

end
