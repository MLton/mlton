(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

functor CmmCodegen (S: CMM_CODEGEN_STRUCTS): CMM_CODEGEN =
struct

open S

type int = Int.t

local
   open Machine
in
   structure Block = Block
   structure CFunction = CFunction
   structure Chunk = Chunk
   structure ChunkLabel = ChunkLabel
   structure CType = CType
   structure FrameInfo = FrameInfo
   structure Global = Global
   structure IntSize = IntSize
   structure Kind = Kind
   structure Label = Label
   structure Live = Live
   structure Operand = Operand
   structure Prim = Prim
   structure ProfileLabel = ProfileLabel
   structure Program = Program
   structure RealSize = RealSize
   structure RealX = RealX
   structure Register = Register
   structure Runtime = Runtime
   structure GCField = Runtime.GCField
   structure Scale = Scale
   structure StackOffset = StackOffset
   structure Statement = Statement
   structure Switch = Switch
   structure Transfer = Transfer
   structure Type = Type
   structure WordSize = WordSize
   structure WordX = WordX
   datatype z = datatype RealSize.t
   datatype z = datatype WordSize.prim
end

structure Target = CFunction.Target

structure Print =
   struct
      fun utils print =
	 let
	    val indent = ref 0
	    fun pushIndent () = Int.inc indent
	    fun popIndent () = Int.dec indent
	    fun println ss =
	       (Int.for (0, !indent, fn _ => print "\t")
		; List.foreach (ss, print)
		; print "\n")
	    fun newline () = print "\n"
	 in
	    {pushIndent = pushIndent, 
	     popIndent = popIndent,
	     println = println, 
	     newline = newline}
	 end
   end

structure Cmm =
   struct
      structure Type =
	 struct
	    datatype t = T of Bits.t
	       
	    fun equals (T bits1, T bits2) =
	       Bits.equals (bits1, bits2)
	    fun fromBits bits = T bits
	    val fromBytes = fromBits o Bytes.toBits
	    fun toBits (T bits) = bits
	    fun toString (T bits) =
	       concat ["bits", Bits.toString bits]
	 end
      
      structure Kind =
	 struct
	    datatype t = Address | Data | Float
	       
	    fun toString k =
	       case k of
		  Address => "address"
		| Data => ""
		| Float => "float"
	 end

      fun bytes (b: Bytes.t) =
	 if Bytes.>= (b, Bytes.zero)
	    then Bytes.toString b
	    else concat ["-", Bytes.toString (Bytes.~ b)]

      fun int (i: int) =
	 if i >= 0
	    then Int.toString i
	 else concat ["-", Int.toString (~ i)]

      structure Export =
	 struct
	    fun new () =
	       let
		  val exports : string list ref = ref []
		  fun addExport export =
		     List.push (exports, export)
		  fun declareExports print =
		     let
			val {pushIndent, popIndent, println, newline} =
			   Print.utils print
			val exports = 
			   List.removeDuplicates (!exports, String.equals)
		     in
			List.foreach
			(exports, fn export =>
			 println ["export ", export, ";"])
		     end
	       in
		  {addExport = addExport,
		   declareExports = declareExports}
	       end
	 end

      structure Import =
	 struct
	    fun new () =
	       let
		  val imports : string list ref = ref []
		  fun addImport import =
		     List.push (imports, import)
		  fun declareImports print =
		     let
			val {pushIndent, popIndent, println, newline} =
			   Print.utils print
			val imports = 
			   List.removeDuplicates (!imports, String.equals)
		     in
			List.foreach
			(imports, fn import =>
			 println ["import ", import, ";"])
		     end
	       in
		  {addImport = addImport,
		   declareImports = declareImports}
	       end
	 end
      
      structure DataSection =
	 struct
	    fun new () =
	       let
		  val dataSection = ref []

		  fun addData (s, data) =
		     let
			val l = Label.newString s
		     in
			List.push (dataSection, (l, data))
			; l
		     end
		  fun addDatum (s, datum) =
		     addData (s, [datum])
		  fun declareDataSection print =
		     let
			val {pushIndent, popIndent, println, newline} =
			   Print.utils print
		     in
			println ["section \"data\" {"]
			; pushIndent ()
			; List.foreach 
			  (!dataSection, fn (l, data) =>
			   (println [Label.toString l, ":"]
			    ; pushIndent ()
			    ; List.foreach
			      (data, println)
			      ; popIndent ()))
		        ; popIndent ()
			; println ["}"]
		     end
	       in
		  {addData = addData,
		   addDatum = addDatum,
		   declareDataSection = declareDataSection}
	       end
	 end

      structure Target =
	 struct
	    fun declare {print, 
			 memsize, byteorder, 
			 pointersize, wordsize} =
	       let
		  val {pushIndent, popIndent, println, newline} =
		     Print.utils print
	       in
		  println ["target"]
		  ; pushIndent ()
		  ; println ["memsize ", Bits.toString (Type.toBits memsize)]
		  ; println ["byteorder ", byteorder]
		  ; println ["pointersize ", Bits.toString (Type.toBits pointersize)]
		  ; println ["wordsize ", Bits.toString (Type.toBits wordsize), ";"]
		  ; popIndent ()
	       end
	 end

      fun addKind (cmmk, s) =
	 concat ["\"", Kind.toString cmmk, "\"", " ", s]

      fun args (ss: string list): string = 
	 concat ("(" :: List.separate (ss, ", ") @ [")"])
      fun apply (f: string, ss: string list): string =
	 concat (f :: "(" :: List.separate (ss, ", ") @ [")"])

      fun memRef (cmmt: Type.t, addr: string) =
(*
		  aligned: Bytes.t, aliasing: string list) =
*)
	 let
	    infixr 5 ::?
	    fun (b,x) ::? l =
	       if b then x :: l else l
	 in
	    concat
	    ((Type.toString cmmt) ::
	     "[" ::
	     addr ::
(*
	     (not (Bits.equals (Type.toBits cmmt, Bytes.toBits aligned)),
	      concat [" aligned ", Bytes.toString aligned]) ::?
	     (List.length aliasing > 0, 
	      concat (" in " :: List.separate (aliasing, ","))) ::?
*)
	     "]" ::
	     nil)
	 end

      structure Debug =
	 struct
	    fun printf {println, addDatum, addImport,
			printfStr, args} =
	       let
		  val printfStr = 
		     addDatum ("printfStr", ["bits8[] \"", printfStr, "\";"])
		  val args =
		     (addKind (Kind.Address, Label.toString printfStr))::args
	       in
		  addImport "printf"
		  ; addImport "fflush"
		  ; println 
		    ["foreign \"C\" ",
		     apply ("printf", args),
		     ";"]
		  ; println
		    ["foreign \"C\" ", apply ("fflush", ["0"]), ";"]
	       end
	 end
   end

structure RealX =
   struct
      open RealX

      fun toCmm (r: t): string =
	 let
	    (* The only difference between SML reals and C-- floats/doubles 
	     * is that SML uses "~" while C uses "-".
	     *)
	    val str =
	       String.translate 
	       (toString r,
		fn #"~" => "-" | c => String.fromChar c)
	 in
	    concat
	    ["(", str, " :: ", 
	     Cmm.Type.toString (Cmm.Type.fromBits (RealSize.bits (size r))),
	     ")"]
	 end
   end

structure WordX =
   struct
      open WordX

      fun toCmm (w: t): string =
	 let
	    val str = toString w
	 in
	    concat
	    ["(0x", str, " :: ", 
	     Cmm.Type.toString (Cmm.Type.fromBits (WordSize.bits (size w))),
	     ")"]
	 end
   end

structure Type =
   struct
      open Type
      structure CType =
	 struct
	    open CType

	    val toCmmTypeKind =
	       memo
	       (fn ct =>
		(Cmm.Type.fromBytes (CType.size ct),
		 let datatype z = datatype Cmm.Kind.t
		 in
		    case ct of
		       Int8 => Data
		     | Int16 => Data
		     | Int32 => Data
		     | Int64 => Data
		     | Pointer => Address
		     | Real32 => Float
		     | Real64 => Float
		     | Word8 => Data
		     | Word16 => Data
		     | Word32 => Data
		     | Word64 => Data
		 end))
	       
	    val toCmmType = #1 o toCmmTypeKind
	    val toCmmKind = #2 o toCmmTypeKind
	 end
      
      val toCmmType = CType.toCmmType o toCType
      val toCmmKind = CType.toCmmKind o toCType
   end
structure CType = Type.CType

fun implementsPrim (p: 'a Prim.t) =
   let
      datatype z = datatype IntSize.prim
      datatype z = datatype RealSize.t
      datatype z = datatype WordSize.prim
      fun w32168 s =
	 case WordSize.prim s of
	    W8 => true
	  | W16 => true
	  | W32 => true
	  | W64 => false
      datatype z = datatype Prim.Name.t
   in
      case Prim.name p of
	 FFI_Symbol _ => true
(*
       | Real_Math_acos _ => true
       | Real_Math_asin _ => true
       | Real_Math_atan _ => true
       | Real_Math_atan2 _ => true
       | Real_Math_cos _ => true
       | Real_Math_exp _ => true
       | Real_Math_ln _ => true
       | Real_Math_log10 _ => true
       | Real_Math_sin _ => true
*)
       | Real_Math_sqrt _ => true
(*
       | Real_Math_tan _ => true
*)
       | Real_abs _ => true
       | Real_add _ => true
       | Real_div _ => true
       | Real_equal _ => true
(*
       | Real_ldexp _ => true
*)
       | Real_le _ => true
       | Real_lt _ => true
       | Real_mul _ => true
       | Real_muladd _ => true
       | Real_mulsub _ => true
       | Real_neg _ => true
(*
       | Real_qequal _ => true
       | Real_round _ => true
*)
       | Real_sub _ => true
       | Real_toReal _ => true
       | Real_toWord (s1, s2, {signed}) =>
	    signed
	    andalso (case (s1, WordSize.prim s2) of
			(R64, W32) => true
		      | (R64, W16) => true
		      | (R64, W8) => true
		      | (R32, W32) => true
		      | (R32, W16) => true
		      | (R32, W8) => true
		      | _ => false)
       | Word_add s => w32168 s
       | Word_addCheck (s,_) => w32168 s
       | Word_andb s => w32168 s
       | Word_equal s => w32168 s
       | Word_lshift s => w32168 s
       | Word_lt (s, _) => w32168 s
       | Word_mul (s, _) => w32168 s
       | Word_mulCheck (s, _) => w32168 s
       | Word_neg s => w32168 s
       | Word_negCheck s => w32168 s
       | Word_notb s => w32168 s
       | Word_orb s => w32168 s
       | Word_quot (s, _) => w32168 s
       | Word_rem (s, _) => w32168 s
       | Word_rol s => w32168 s
       | Word_ror s => w32168 s
       | Word_rshift (s, _) => w32168 s
       | Word_sub s => w32168 s
       | Word_subCheck (s,_) => w32168 s
       | Word_toReal (s1, s2, {signed}) =>
	    signed
	    andalso (case (WordSize.prim s1, s2) of
			(W32, R64) => true
		      | (W32, R32) => true
		      | (W16, R64) => true
		      | (W16, R32) => true
		      | (W8, R64) => true
		      | (W8, R32) => true
		      | _ => false)
       | Word_toWord (s1, s2, _) =>
	    (case (WordSize.prim s1, WordSize.prim s2) of
		(W32, W32) => true
	      | (W32, W16) => true
	      | (W32, W8) => true
	      | (W16, W32) => true
	      | (W16, W16) => true
	      | (W16, W8) => true
	      | (W8, W32) => true
	      | (W8, W16) => true
	      | (W8, W8) => true
	      | _ => false)
       | Word_xorb s => w32168 s
       | _ => false
   end

fun output {program as Program.T {chunks, main, ...},
	    outputC: unit -> {file: File.t,
			      print: string -> unit,
			      done: unit -> unit},
	    outputCmm: unit -> {file: File.t,
				print: string -> unit,
				done: unit -> unit}} =
   let
      datatype z = datatype Operand.t
      datatype z = datatype Statement.t
      datatype z = datatype Transfer.t

      fun frameSize fi =
	 Program.frameSize (program, fi)

      val memsize = Cmm.Type.fromBits Bits.inByte
      val pointersize = Cmm.Type.fromBits Bits.inPointer
      val wordsize = Cmm.Type.fromBits Bits.inWord

      fun declareTarget print =
	 let
	    val byteorder = 
	       if Control.targetIsBigEndian ()
		  then "big"
		  else "little"
	 in
	    Cmm.Target.declare
	    {print = print, 
	     memsize = memsize, 
	     byteorder = byteorder, 
	     pointersize = pointersize, 
	     wordsize = wordsize}
	 end
      val hwrm = "System.rounding_mode"
      fun declareGlobalRegisters print =
	 let
	 in
	    (print o concat)
	    ["bits2 ", hwrm, " = ",
	     "\"IEEE 754 rounding mode\";\n"]
	 end

      val {get = chunkLabelInfo: ChunkLabel.t -> {func: Label.t option},
	   set = setChunkLabelInfo, ...} =
	 Property.getSetOnce
	 (ChunkLabel.plist, 
	  Property.initConst {func = NONE})
      val {get = labelInfo: Label.t -> {block: Block.t,
					done: bool ref},
	   set = setLabelInfo, ...} =
	 Property.getSetOnce
	 (Label.plist, 
	  Property.initRaise ("CmmCodegen.labelInfo", Label.layout))

      val _ =
	 List.foreach
	 (chunks, fn Chunk.T {blocks, chunkLabel, ...} =>
	  Vector.foreach
	  (blocks, fn b as Block.T {kind, label, ...} =>
	   let 
	      val () =
		 case kind of
		    Kind.Func => 
		       setChunkLabelInfo (chunkLabel, {func = SOME label})
		  | _ => ()
	      val () =
		 setLabelInfo (label, {block = b,
				       done = ref false})
	   in
	      ()
	   end))
	      
      fun outputChunk (Chunk.T {chunkLabel, regMax, ...}) =
	 let
	    val {file, print, done, ...} = outputCmm ()
	    val fileName = file
	    val {pushIndent, popIndent, println, newline} =
	       Print.utils print
	    val {addData, addDatum, declareDataSection} = 
	       Cmm.DataSection.new ()
	    val {addExport, declareExports} = 
	       Cmm.Export.new ()
	    val {addImport, declareImports} = 
	       Cmm.Import.new ()

	    val func =
	       case chunkLabelInfo chunkLabel of
		  {func = SOME func, ...} => func
		| _ => Error.bug "CmmCodegen: missing func"

	    val fileNameLabel =
	       addDatum
	       ("fileLabel",
		["bits8[] \"", String.toString fileName, "\\0\";"])

	    val gcState = "gcState"
	    local
	       val global = "global"
	       val creturn = "CReturn"
	    in
	       val globalPNR = concat [global, "PointerNonRoot"]
	       val global = 
		  CType.memo
		  (fn ct =>
		   concat [global, CType.toString ct])
	       val creturn = 
		  CType.memo
		  (fn ct =>
		   concat [creturn, CType.name ct])
	    end
	    val frontier = "Frontier"
	    val stackTop = "StackTop"

	    local
	       val c = Counter.new 0
	       val temps = ref []
	    in
	       fun newTemp cmmt =
		  let
		     val temp =
			concat ["tmp", Int.toString (Counter.next c)]
		  in
		     List.push (temps, (cmmt, temp))
		     ; temp
		  end
	       fun declareTemps () =
		  let
		     fun decl (cmmt, s) =
			println [Cmm.Type.toString cmmt, " ", s, ";"]
		     val temps = !temps
		  in
		     if List.length temps > 0
			then (newline ()
			      ; List.foreach (temps, decl))
			else ()
		  end
	    end

	    local
	       val todoLabels : Label.t Queue.t ref = ref (Queue.empty ())
	    in
	       fun enqueLabel l =
		  todoLabels := Queue.enque (!todoLabels, l)
	       fun dequeLabel () =
		  case Queue.deque (!todoLabels) of
		     NONE => NONE
		   | SOME (todoLabels', l) => 
			(todoLabels := todoLabels'
			 ; SOME l)
	    end

 	    fun importGlobals () =
	       let
		  fun import s =
		     println ["import ", s, ";"]
		  fun importGlobalTy ct =
		     import (global ct)
	       in
		  import gcState
		  ; import globalPNR
		  ; List.foreach (CType.all, importGlobalTy)
	       end

 	    fun declareRegisters () =
	       let
		  fun decl (cmmt, s) =
		     println [Cmm.Type.toString cmmt, " ", s, ";"]
		  fun declPtr s =
		     decl (CType.toCmmType CType.Pointer, s)
	       in
		  declPtr frontier
		  ; declPtr stackTop
		  ; List.foreach
		    (CType.all, fn t =>
		     let
			val cmmt = CType.toCmmType t
		     in
			Int.for
			(0, 1 + regMax t, fn i =>
			 decl (cmmt, 
			       concat ["R", CType.name t, "_", Int.toString i]))
			; decl (cmmt, creturn t)
		     end)
	       end

	    local
	       datatype z = datatype Operand.t
	       val lineCount = Counter.new 0
	       fun toString (oper : Operand.t) : string =
		  case oper of
		     ArrayOffset {base, index, offset, scale, ty} => 
			Cmm.memRef
			(Type.toCmmType ty,
			 concat [toString base, " + (",
				 toString index, " * ", Scale.toString scale,
				 " ) + ", Cmm.bytes offset])
		   | Cast (t, ty) => 
			(Assert.assert 
			 ("CmmCodegen: Cast",
			  fn () => 
			  Cmm.Type.equals
			  (Type.toCmmType ty,
			   Type.toCmmType (Operand.ty t)))
			 ; toString t)
		   | Contents {oper, ty} =>
			Cmm.memRef
			(Type.toCmmType ty,
			 toString oper)
		   | File => Label.toString fileNameLabel
		   | Frontier => frontier
		   | GCState => gcState
		   | Global g => 
			let
			   val ty = Type.toCType (Global.ty g)
			   val index = Global.index g
			   val base =
			      if Global.isRoot g
				 then global ty
				 else globalPNR
			   val scale = CType.size ty
			in
			   Cmm.memRef
			   (CType.toCmmType ty,
			    concat [base,
				    " + (",
				    Int.toString index, " * ",
				    Cmm.bytes scale,
				    ")"])
			end
		   | Label l => Label.toString l
		   | Line => Cmm.int (Counter.next lineCount)
		   | Offset {base, offset, ty} =>
			Cmm.memRef
			(Type.toCmmType ty,
			 concat [toString base, 
				 " + ", 
				 Cmm.bytes offset])
		   | Real r => RealX.toCmm r
		   | Register reg =>
			let
			   val ty = Register.ty reg
			   val index = Register.index reg
			in
			   concat ["R", Type.name ty, "_",
				   Int.toString index]
			end
		   | StackOffset (StackOffset.T {offset, ty}) => 
			Cmm.memRef
			(Type.toCmmType ty,
			 concat [toString StackTop, 
				 " + ", 
				 Cmm.bytes offset])
		   | StackTop => stackTop
		   | Word w => WordX.toCmm w
	    in
	       val operandToString = toString
	    end
	    local
	       fun make (field: GCField.t, size) =
		  Cmm.memRef
		  (size, 
		   concat [gcState, 
			   " + ", 
			   Cmm.bytes (GCField.offset field)])
	    in
	       val exnStackMem = make (GCField.ExnStack, wordsize)
	       val frontierMem = make (GCField.Frontier, pointersize)
	       val stackBottomMem = make (GCField.StackBottom, pointersize)
	       val stackTopMem = make (GCField.StackTop, pointersize)
	    end
	    local
	       fun make (oper, mem) =
		  let
		     val oper = operandToString oper
		  in
		     (fn () => println [oper, " = ", mem, ";"],
		      fn () => println [mem, " = ", oper, ";"])
		  end
	    in
	       val (cacheFrontier, flushFrontier) =
		  make (Operand.Frontier, frontierMem)
	       val (cacheStackTop, flushStackTop) =
		  make (Operand.StackTop, stackTopMem)
	    end

	    fun outputStatement (s : Statement.t) : unit =
	       case s of
		  Move {dst, src} =>
		     println [operandToString dst, " = ",
			      operandToString src, ";"]
		| Noop => () 
		| PrimApp {args, dst, prim} =>
		     let 
			datatype z = datatype Prim.Name.t
			val dst = Option.map (dst, operandToString)
			val args = Vector.toList args
		        val (args, prim) =
			   case Prim.name prim of
			      Word_neg size =>
				 ((Operand.Word (WordX.zero size))::args,
				  Prim.wordSub size)
			    | _ => (args, prim)
			val args = List.map (args, operandToString)
			val println' = println
			val println =
			   case dst of
			      NONE => (fn ss => println ss)
			    | SOME dst => 
			         (fn ss => println (dst :: " = " :: ss))
			fun simple oper =
			   println [Cmm.apply (oper, args), ";"]
			val al = simple
			fun shift (oper,size) =
			   let
			      val b = WordSize.bits size
			      val (src1,src2) =
				 case args of
				    [src1,src2] => (src1,src2)
				  | _ => Error.bug "CmmCodegen: PrimApp: shift"
			   in
			      println [Cmm.apply 
				       (oper, [src1, Cmm.apply 
				       ("%lobits" ^ (Bits.toString b), [src2])]), ";"]
			   end
			fun fparith (oper,round) =
			   if round
			      then println [Cmm.apply (oper, args @ [hwrm]), ";"]
			      else println [Cmm.apply (oper, args), ";"]
			fun fpmul oper =
			   let
			      val (src1,src2,src3) =
				 case args of
				    [src1,src2,src3] => (src1,src2,src3)
				  | _ => Error.bug "CmmCodegen: PrimApp: fpmul"
			   in
			      println [Cmm.apply (oper, [
				       Cmm.apply ("%fpmul", [
						  src1, src2, hwrm]),
				       src3, hwrm])]
			   end
			fun compare oper =
			   println [Cmm.apply ("%zx32", [
				    Cmm.apply ("%bit", [
				    Cmm.apply (oper, args)])]), ";"]
			val fpcompare = compare
			fun checkSize s =
			   case WordSize.prim s of
			      W8 => ()
			    | W16 => ()
			    | W32 => ()
			    | W64 => Error.bug "CmmCodegen: PrimApp: W64"
		     in
			case Prim.name prim of
			   FFI_Symbol {name, ...} =>
			      (println [name, ";"]
			       ; addImport name)
			 | Real_Math_sqrt _ => fparith ("%fsqrt", true)
			 | Real_abs _ => fparith ("%fabs", false)
			 | Real_add _ => fparith ("%fadd", true)
			 | Real_div _ => fparith ("%fdiv", true)
			 | Real_equal _ => fpcompare "%feq"
			 | Real_le _ => fpcompare "%fle"
			 | Real_lt _ => fpcompare "%flt"
			 | Real_mul _ => fparith ("%fmul", true)
			 | Real_muladd _ => fpmul "%fadd"
			 | Real_mulsub _ => fpmul "%fsub"
			 | Real_neg _ => fparith ("%fneg", false)
			 | Real_qequal _ => fpcompare "%funordered"
			 | Real_sub _ => fparith ("%fsub", true)
			 | Real_toReal (s, s') =>
			      let
				 val b' = RealSize.bits s'
			      in
				 fparith ("%f2f" ^ (Bits.toString b'), true)
			      end
			 | Real_toWord (s, s', _) =>
			      let
				 val () = checkSize s'
				 val b' = WordSize.bits s'
			      in
				 fparith ("%f2i" ^ (Bits.toString b'), true)
			      end
			 | Word_add s => (checkSize s; al "%add")
			 | Word_andb s => (checkSize s; al "%and")
			 | Word_equal s => (checkSize s; compare "%eq")
			 | Word_lshift s => (checkSize s; shift ("%shl", s))
			 | Word_lt (s, {signed}) =>
			      (checkSize s
			       ; if signed 
				    then compare "%lt"
				    else compare "%ltu")
			 | Word_mul (s, _) => (checkSize s; al "%mul")
			 | Word_notb s => (checkSize s; al "%com")
			 | Word_orb s => (checkSize s; al "%or")
			 | Word_quot (s, {signed}) => 
			      (checkSize s
			       ; if signed
				    then al "%quot"
				    else al "%divu")
			 | Word_rem (s, {signed}) => 
			      (checkSize s
			       ; if signed
				    then al "%rem"
				    else al "%modu")
			 | Word_rol s => (checkSize s; shift ("%rotl", s))
			 | Word_ror s => (checkSize s; shift ("%rotr", s))
			 | Word_rshift (s, {signed}) => 
			      (checkSize s
			       ; if signed
				    then shift ("%shra", s)
				    else shift ("%shrl", s))
			 | Word_sub s => (checkSize s; al "%sub")
			 | Word_toReal (s, s', _) =>
			      let
				 val () = checkSize s
				 val b' = RealSize.bits s'
			      in
				 fparith ("%i2f" ^ (Bits.toString b'), true)
			      end
			 | Word_toWord (s, s', {signed}) =>
			      let 
				 val () = checkSize s
				 val () = checkSize s'
				 val b = WordSize.bits s
				 val b' = WordSize.bits s'
				 val simple = fn f =>
				    simple (f ^ (Bits.toString b'))
			      in
				 if Bits.< (b, b')
				    then if signed
					    then simple "%sx"
					    else simple "%zx"
				    else simple "%lobits"
			      end
			 | Word_xorb s => (checkSize s; al "%xor")
			 | _ => Error.bug (concat ["CmmCodegen: PrimApp:", 
						   Prim.toString prim])
		     end
		| ProfileLabel l =>
		     (addExport (ProfileLabel.toString l)
		      ; println [ProfileLabel.toString l, ":"])
	    local
	       val returnExp =
		  Cmm.memRef
		  (Cmm.Type.fromBits Bits.inPointer,
		   concat [stackTop, " - ", "(", 
			   Cmm.int 1, " * ", 
			   (Cmm.bytes Bytes.inPointer), 
			   ")"])
	       fun adjStackTop (oper, size) =
		  println [stackTop, " = ", Cmm.apply 
			   (oper, [stackTop, Cmm.bytes size]), ";"]
	    in
	       fun cutToReturnExp' alsoCutsTo =
		  println ["cut to ", "(", returnExp, ")", "()",
			   alsoCutsTo, 
			   " also aborts;"]
	       fun cutToReturnExp () = cutToReturnExp' ""
	       fun assignToReturnExp e =
		  let
		     val _ =
			if true andalso !Control.Cmm.debug
			   then let
				   val printfStr =
				      concat 
				      [String.toString fileName, ": ",
				       "assignToReturnExp(", e, 
				       " = 0x%08x)\\n\\0"]
				in
				   Cmm.Debug.printf
				   {println = println,
				    addDatum = addDatum,
				    addImport = addImport,
				    printfStr = printfStr,
				    args = [e]}
				end
			   else ()
		  in
		      println [returnExp, " = ", e, ";"]
		  end
	       fun pushStackTop size = adjStackTop ("%add", size)
	       fun popStackTop size = adjStackTop ("%sub", size)
	    end
	    fun outputTransfer (t : Transfer.t) : unit =
	       case t of
		  Arith {args, dst, overflow, prim, success} =>
		     let
			datatype z = datatype Prim.Name.t
			val tmpDst = newTemp (Type.toCmmType (Operand.ty dst))
			val dst = operandToString dst
			val args = Vector.toList args
		        val (args, prim) =
			   case Prim.name prim of
			      Word_negCheck size =>
				 ((Operand.Word (WordX.zero size))::args,
				  Prim.wordSubCheck (size, {signed=false}))
			    | _ => (args, prim)
			val args = List.map (args, operandToString)
			fun doIf cond =
			   (println ["if ", cond, " {"]
			    ; pushIndent ()
			    ; println [dst, " = ", tmpDst, ";"]
			    ; println ["goto ", Label.toString overflow, ";"]
			    ; popIndent ()
			    ; println ["} else {"]
			    ; pushIndent ()
			    ; println [dst, " = ", tmpDst, ";"]
			    ; println ["goto ", Label.toString success, ";"]
			    ; popIndent ()
			    ; println ["}"] 
			    ; enqueLabel overflow
			    ; enqueLabel success)
			fun a (prim,primOverflow) =
			   (println [tmpDst, " = ", Cmm.apply(prim, args), ";"]
			    ; doIf (Cmm.apply(primOverflow, args)))
			fun checkSize s =
			   case WordSize.prim s of
			      W8 => ()
			    | W16 => ()
			    | W32 => ()
			    | W64 => Error.bug "CmmCodegen: Arith: W64"
		     in
			case Prim.name prim of
			   Word_addCheck (s, _) => 
			      (checkSize s
			       ; a ("%add", "%add_overflows"))
			 | Word_mulCheck (s, _) => 
			      (checkSize s
			       ; a ("%mul", "%mul_overflows"))
			 | Word_subCheck (s, _) => 
			      (checkSize s
			       ; a ("%sub", "%sub_overflows"))
			 | _ => Error.bug "CmmCodegen: Arith"
		     end
		| CCall {args, frameInfo, func, return} =>
		     let
			val CFunction.T {maySwitchThreads,
					 modifiesFrontier,
					 readsStackTop,
					 return = returnTy,
					 target,
					 writesStackTop, ...} = func
			val kindArgs =
			   Vector.toListMap
			   (args, Type.toCmmKind o Operand.ty)
			val args =
			   case frameInfo of
			      NONE =>
				 Vector.toListMap(args, operandToString)
			    | SOME frameInfo =>
				 let
				    val FrameInfo.T {frameLayoutsIndex} = frameInfo
				    val size = frameSize frameInfo
				    val args =
				       Vector.toListMap
				       (args, fn arg =>
					let
					   val ty = Operand.ty arg
					   val cmmt = Type.toCmmType ty
					   val tmp = newTemp cmmt
					in
					   println [tmp, " = ", 
						    operandToString arg, ";"]
					   ; tmp
					end)
				 in
				    pushStackTop size
				    ; assignToReturnExp
				      (case !Control.Cmm.nonTail of
					  Control.Cmm.CutTo _ =>
					     Label.toString (valOf return)
					| Control.Cmm.Return =>
					     Cmm.int frameLayoutsIndex)
				    ; args
				 end
			val _ =
			   if modifiesFrontier
			      then flushFrontier ()
			      else ()
			val _ =
			   if readsStackTop
			      then flushStackTop ()
			      else ()
			fun doCache () =
			   (if modifiesFrontier
			       then cacheFrontier ()
			       else ()
			    ; if writesStackTop
				 then cacheStackTop ()
				 else ())

			fun doNeverReturns () =
			   (" never returns", fn () =>
			    ())
			fun doGotoLabel l =
			   ("", fn () =>
			    (doCache ()
			     ; println ["goto ", Label.toString l, ";"]
			     ; enqueLabel l))
			fun doCutToLabel l =
			   ("", fn () =>
			    (doCache ()
			     ; println ["cut to ",
					Label.toString l, "()",
					" also cuts to ",
					Label.toString l, ";"]
			     ; enqueLabel l))
			fun doCutToReturnExp l =
			   ("", fn () =>
			    (doCache ()
			     ; cutToReturnExp' 
			       (concat 
				[" also cuts to ", 
				 Label.toString l])
			     ; enqueLabel l))

			val (flow, finishTransfer) =
			   case return of
			      NONE => doNeverReturns ()
			    | SOME return =>
				 (enqueLabel return
				  ; case !Control.Cmm.nonTail of
				       Control.Cmm.CutTo _ =>
					  (if maySwitchThreads
					      then doCutToReturnExp return
					      else case frameInfo of
						      NONE => doGotoLabel return
						    | SOME _ => doCutToLabel return)
				     | Control.Cmm.Return =>
					      doGotoLabel return)

			val results =
			   if Type.isUnit returnTy
			      then ""
			      else let
				      val ct = Type.toCType returnTy
				      val cmmk = CType.toCmmKind ct
				   in
				      concat
				      [Cmm.addKind (cmmk, creturn ct), " = "]
				   end
			val conv = "foreign \"C\" "
			datatype z = datatype CFunction.Target.t
			val (target, args, kindArgs) =
			   case target of
			      Direct name => 
				 (addImport name
				  ; (name, args, kindArgs))
			    | Indirect =>
				 let
				    val (fptr,args) =
				       case args of
					  (fptr::args) => (fptr,args)
					| _ => Error.bug "CmmCodegen: CCall: Indirect"
				 in
				    (concat ["(",fptr,")"],
				     args, tl kindArgs)
				 end
			val args = List.map2 (kindArgs, args, Cmm.addKind)
			val _ =
			   println [results, 
				    conv,
				    Cmm.apply (target, args),
				    flow,
				    ";"]
			val _ = finishTransfer ()
		     in
			()
		     end
		| Call {label, return, ...} =>
		     (if Label.equals (label, func)
			 then ()
			 else addImport (Label.toString label)
		      ; case return of 
		           NONE => 
			      (flushFrontier ()
			       ; flushStackTop ()
			       ; println ["jump ", Label.toString label, "();"])
			 | SOME {return, handler, size, ...} =>
			      let
				 fun alsoCutsTo l =
				    (enqueLabel l
				     ; println ["also cuts to ", Label.toString l])
				 fun goto l =
				    (enqueLabel l
				     ; println ["goto ", Label.toString l, ";"])
				 val FrameInfo.T {frameLayoutsIndex} =
				    let
				       val Block.T {kind, ...} =
					  #block (labelInfo return)
				    in
				       case Kind.frameInfoOpt kind of
					  NONE => Error.bug "CmmCodegen: Call: fi"
					| SOME fi => fi
				    end
			      in
				 case !Control.Cmm.nonTail of
				    Control.Cmm.CutTo {neverReturns} =>
				       (pushStackTop size
					; assignToReturnExp (Label.toString return)
					; flushFrontier ()
					; flushStackTop ()
					; println [Label.toString label, "()"]
					; pushIndent ()
					; alsoCutsTo return
					; case handler of
					     NONE => println ["also aborts"]
					   | SOME handler => alsoCutsTo handler
					; if neverReturns
					     then (println ["never returns;"]
						   ; println [";"]
						   ; popIndent ())
					     else (println [";"]
						   ; popIndent ()
						   ; println ["return ();"]))
				  | Control.Cmm.Return =>
				       (pushStackTop size
					; assignToReturnExp (Cmm.int frameLayoutsIndex)
					; flushFrontier ()
					; flushStackTop ()
					; println [Label.toString label, "()"]
					; pushIndent ()
					; case handler of
					     NONE => println ["also aborts"]
					   | SOME handler => alsoCutsTo handler
					; println [";"]
					; popIndent ()
				        ; goto return)
			      end)
		| Goto label =>
		     (enqueLabel label
		      ; println ["goto ", Label.toString label, ";"])
		| Raise =>
		     (println [stackTop, " = ",
			       Cmm.apply("%add",
					 [stackBottomMem,
					  exnStackMem]), ";"]
		      ; flushFrontier ()
		      ; flushStackTop ()
		      ; if !Control.Cmm.debug
			   then let
				   val printfStr =
				      concat
				      [String.toString fileName, ": ",
				       "Raise\\n\\0"]
				in
				   Cmm.Debug.printf
				   {println = println,
				    addDatum = addDatum,
				    addImport = addImport,
				    printfStr = printfStr,
				    args = []}
				end
			   else ()
		      ; cutToReturnExp ())
		| Return =>
		     (flushFrontier ()
		      ; flushStackTop ()
		      ; if !Control.Cmm.debug
			   then let
				   val printfStr =
				      concat
				      [String.toString fileName, ": ",
				       "Return\\n\\0"]
				in
				   Cmm.Debug.printf
				   {println = println,
				    addDatum = addDatum,
				    addImport = addImport,
				    printfStr = printfStr,
				    args = []}
				end
			   else ()
		      ; case !Control.Cmm.nonTail of
			   Control.Cmm.CutTo _ =>
			      cutToReturnExp ()
			 | Control.Cmm.Return =>
			      println ["return ();"])
		| Switch (Switch.T {cases, default, test, size}) =>
		     let
			val test = operandToString test
			fun doArm (range, l) =
			   (println ["case ", range, " : ",
				     "{ goto ", Label.toString l, "; }"]
			   ; enqueLabel l)
		     in
			println ["switch ", test, " {"]
			; pushIndent ()
			; Vector.foreach
			  (cases, fn (w, l) =>
			   doArm (WordX.toCmm w, l))
			; Option.app
			  (default, fn l =>
			   let
			      val zero = WordX.zero size
			      val ones = WordX.allOnes size
			      val range =
				 concat [WordX.toCmm zero, 
					 " .. ",
					 WordX.toCmm ones]
			   in
			      doArm (range, l)
			   end)
			; popIndent ()
			; println ["}"]
		     end
	    fun outputBlock (Block.T {kind, label, statements, transfer, ...}) =
	       let
		  val () = newline ()
		  fun doLabel () =
		     println [Label.toString label, ":"]
		  fun doContinuation (FrameInfo.T {frameLayoutsIndex}) =
		     case !Control.Cmm.nonTail of
			Control.Cmm.CutTo _ =>
			   let
			      val l = 
				 addDatum
				 ("frameLayoutsIndexSpanData",
				  [Cmm.Type.toString
				   (Cmm.Type.fromBits Bits.inWord), 
				   " {", Int.toString frameLayoutsIndex, "};"])
			   in
			      println ["span ", 
				       Cmm.int 1, " " , 
				       Label.toString l, " {"]
			      ; println ["continuation ", Label.toString label, "():"]
			      ; println ["}"]
			   end
		      | Control.Cmm.Return =>
			   println ["continuation ", Label.toString label, "():"]
			   
		  val () =
		     case kind of
			Kind.Cont {frameInfo, ...} =>
			   (case !Control.Cmm.nonTail of
			       Control.Cmm.CutTo _ => doContinuation frameInfo
			     | Control.Cmm.Return => doLabel ()
			    ; pushIndent ()
			    ; cacheFrontier ()
			    ; cacheStackTop ()
			    ; popStackTop (frameSize frameInfo))
		      | Kind.CReturn {dst, frameInfo, ...} =>
			   (case frameInfo of
			       NONE => 
				  (doLabel ()
				   ; pushIndent ())
			     | SOME frameInfo => 
				  (case !Control.Cmm.nonTail of
				      Control.Cmm.CutTo _ => doContinuation frameInfo
				    | Control.Cmm.Return => doLabel ()
				   ; pushIndent ()
				   ; popStackTop (frameSize frameInfo))
			    ; Option.app
			      (dst, fn dst =>
			       let
				  val dst = Live.toOperand dst
				  val ty = Operand.ty dst
			       in 
				  println [operandToString dst,
					   " = ",
					   creturn (Type.toCType ty),
					   ";"]
			       end))
		      | Kind.Func => 
			   (pushIndent ()
			    ; cacheFrontier ()
			    ; cacheStackTop ())
		      | Kind.Handler {frameInfo, ...} =>
			   (doContinuation frameInfo
			    ; pushIndent ()
			    ; cacheFrontier ()
			    ; cacheStackTop ()
			    ; popStackTop (frameSize frameInfo))
		      | Kind.Jump =>
			   (doLabel ()
			    ; pushIndent ())
		  val _ =
		     if true
			andalso !Control.Cmm.debug
			andalso (case kind of
				    Kind.Cont _ => true
				  | Kind.CReturn _ => true
				  | Kind.Func => true
				  | Kind.Handler _ => true
				  | _ => false)
			then let
				val printfStr =
				   concat
				   [String.toString fileName, ": ",
				    Label.toString label,
				    "\\n\\0"]
			     in
				Cmm.Debug.printf
				{println = println,
				 addDatum = addDatum,
				 addImport = addImport,
				 printfStr = printfStr,
				 args = []}
			     end
			else ()
		  val () =
		     Vector.foreach
		     (statements, outputStatement)
		  val () =
		     outputTransfer transfer
		  val () = popIndent ()
	       in
		  ()
	       end

	    fun doBlocks () =
	       case dequeLabel () of
		  NONE => ()
		| SOME l => 
		     let
			val {block, done, ...} = labelInfo l
		     in
			if !done
			   then ()
			   else (done := true
				 ; outputBlock block)
			; doBlocks ()
		     end

	    val () = enqueLabel func
	 in
	    declareTarget print
	    ; declareGlobalRegisters print
	    ; newline ()
	    ; importGlobals ()
	    ; newline ()
	    ; println ["export ", Label.toString func, ";"]
	    ; println [Label.toString func, "() {"]
	    ; pushIndent ()
	    ; declareRegisters ()
	    ; popIndent ()
	    ; doBlocks ()
	    ; declareTemps ()
	    ; println ["}"]
	    ; newline ()
	    ; declareDataSection print
	    ; newline ()
	    ; declareImports print
	    ; declareExports print
	    ; done ()
	 end

      val _ = List.foreach (chunks, outputChunk)
      val mainCFun = Label.new (#label main)
      val () =
	 let
	    val {print, done, ...} = outputCmm ()
	 in
	    declareTarget print
	    ; declareGlobalRegisters print
	    ; print "\n"
	    ; print (concat ["export ", Label.toString mainCFun, ";\n"])
	    ; print (concat ["foreign \"C\" ", Label.toString mainCFun, "() {\n"])
	    ; print (concat ["\tjump ", Label.toString (#label main), "();\n"])
	    ; print (concat ["}\n"])
	    ; print (concat ["import ", Label.toString (#label main), ";\n"])
	    ; done ()
	 end
      val () =
	 let
	    val {print, done, ...} = outputC ()
	 in
	    CCodegen.outputDeclarations
	    {additionalMainArgs = [Label.toString mainCFun,
				   case !Control.Cmm.nonTail of
				      Control.Cmm.CutTo _ => "TRUE"
				    | Control.Cmm.Return => "FALSE"],
	     includes = ["cmm-main.h"],
	     print = print,
	     program = program,
	     rest = fn () => ()}
	    ; done ()
	 end 
   in
      ()
   end

end
