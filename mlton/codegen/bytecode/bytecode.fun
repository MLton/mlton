(* Copyright (C) 2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

functor Bytecode (S: BYTECODE_STRUCTS): BYTECODE = 
struct

open S

type int = Int.t

local
   open Machine
in
   structure Block = Block
   structure CFunction = CFunction
   structure Chunk = Chunk
   structure CType = CType
   structure Global = Global
   structure Label = Label
   structure Operand = Operand
   structure Prim = Prim
   structure Program = Program
   structure Register = Register
   structure Runtime = Runtime
   structure StackOffset = StackOffset
   structure Statement = Statement
   structure Switch = Switch
   structure Transfer = Transfer
   structure Type = Type
   structure WordSize = WordSize
   structure WordX = WordX
end

structure Target = CFunction.Target

val implementsPrim = CCodegen.implementsPrim

structure Opcode = Word8

structure CType =
   struct
      open CType

      val memo: (t -> 'a) -> t -> 'a =
	 fn f =>
	 let
	    val m =
	       CType.memo (fn t =>
			   case t of
			      CType.Pointer => NONE
			    | _ => SOME (f t))
	 in
	    CType.memo (fn t =>
			valOf (case t of
				  CType.Pointer => m CType.Word32
				| _ => m t))
	 end
   end

structure LoadStore =
   struct
      datatype t = Load | Store

      val toString =
	 fn Load => "load"
	  | Store => "store"

      val layout = Layout.str o toString
   end

fun output {program as Program.T {chunks, main, ...}, outputC} =
   let
      datatype z = datatype LoadStore.t
      datatype z = datatype Statement.t
      datatype z = datatype Transfer.t
      (* Build a table of the opcodes. *)
      val table = HashSet.new {hash = #hash}
      val _ =
	 File.withIn
	 (concat [!Control.libDir, "/opcodes"], fn ins =>
	  In.foldLines
	  (ins, 0, fn (l, i) =>
	   case String.tokens (l, Char.isSpace) of
	     [name] =>
		let
		   val hash = String.hash name
		   val _ =
		      HashSet.insertIfNew
		      (table, hash,
		       fn {name = name', ...} => name = name',
		       fn () => {hash = hash,
				 opcode = Word8.fromInt i,
				 name = name},
		       fn _ => Error.bug (concat ["duplicate opcode: ", name]))
		in
		   i + 1
		end
	   | _ => Error.bug "strange opcode file"))
      val opcode: string -> Opcode.t =
	 fn name =>
	 #opcode (HashSet.lookupOrInsert
		  (table, String.hash name,
		   fn {name = name', ...} => name = name',
		   fn () => Error.bug (concat ["missing opcode: ", name])))
      val cacheFrontier = opcode "CacheFrontier"
      val cacheStackTop = opcode "CacheStackTop"
      val call = opcode "Call"
      val cCall = opcode "CCall"
      local
	 val directCalls = HashSet.new {hash = #hash}
	 val counter = Counter.new 0
      in
	 val () =
	    (* Visit each direct C Call in the program. *)
	    List.foreach
	    (chunks, fn Chunk.T {blocks, ...} =>
	     Vector.foreach
	     (blocks, fn Block.T {transfer, ...} =>
	      case transfer of
		 CCall {func, ...} =>
		    let
		       val CFunction.T {prototype, target, ...} = func
		       datatype z = datatype Target.t
		    in
		       case target of
			  Direct name =>
			     let
				val hash = String.hash name
			     in
				ignore
				(HashSet.lookupOrInsert
				 (directCalls, hash,
				  fn {name = n, ...} => n = name,
				  fn () => {hash = hash,
					    index = Counter.next counter,
					    name = name}))
			     end
			| Indirect => ()
		    end
	       | _ => ()))
	 fun directIndex (name: string) =
	    #index (HashSet.lookupOrInsert
		    (directCalls, String.hash name,
		     fn {name = n, ...} => n = name,
		     fn () => Error.bug "directIndex"))
      end
      local
	 val c = Counter.new 0
	 val indirectCalls = ref []
      in
	 fun indirectIndex (prototype): int =
	    let
	       val () =
		  List.push (indirectCalls, {prototype = prototype})
	    in
	       Counter.next c
	    end
      end
      val jumpOnOverflow = opcode "JumpOnOverflow"
      val ffiSymbol =
	 CType.memo (fn t => opcode (concat [CType.toString t, "_FFI_Symbol"]))
      local
	 val inits = ref []
	 val ffiSymbolIndex =
	    CType.memo
	    (fn ty =>
	     let
		val all = ref []
		val () =
		   List.push
		   (inits, fn print =>
		    let
		       val ty = CType.toString ty
		    in
		       print (concat [ty, " FFISymbol", ty, "[] = {\n"])
		       ; List.foreach (rev (!all), fn s =>
				       print (concat ["\t", s, ",\n"]))
		       ; print "}\n"
		    end)
		val r = ref 0
	     in
		String.memoize
		(fn s =>
		 let
		    val i = !r
		    val () = Int.inc r
		    val () = List.push (all, s)
		 in
		    i
		 end)
	     end)
      in
	 val ffiSymbolIndex = fn (ty, s) => ffiSymbolIndex ty s
	 fun initializeFFISymbols print =
	    (print "static void initialize_FFI_Symbols () {\n"
	     ; List.foreach (!inits, fn init => init print)
      	     ; print "}\n")
      end
      val primOp: 'a Prim.t -> Opcode.t = fn p => opcode (Prim.toString p)
      val profileLabel = opcode "ProfileLabel"
      val raisee = opcode "Raise"
      val return = opcode "Return"
      datatype z = datatype WordSize.prim
      val switch: WordSize.t -> Opcode.t =
	 let
	    val s8 = opcode "Switch8"
	    val s16 = opcode "Switch16"
	    val s32 = opcode "Switch32"
	    val s64 = opcode "Switch64"
	 in
	    fn w =>
	    case WordSize.prim w of
	       W8 => s8
	     | W16 => s16
	     | W32 => s32
	     | W64 => s64
	 end
      val thread_returnToC = opcode "Thread_returnToC"
      local
	 fun make name (ls: LoadStore.t, cty: CType.t): Opcode.t =
	    opcode
	    (concat [CType.toString cty, "_", LoadStore.toString ls, name])
      in
	 val arrayOffset = make "ArrayOffset"
	 val contents = make "Contents"
	 val global = make "Global"
	 val offsetOp = make "Offset"
	 val register = make "Register"
	 val stackOffset = make "StackOffset"
	 val wordOpcode = make "Word"
      end
      local
	 fun make name (ls: LoadStore.t): Opcode.t =
	    opcode (concat [LoadStore.toString ls, name])
      in
   	 val frontier = make "Frontier"
	 val gcState = make "GCState"
	 val stackTop = make "StackTop"
      end
      val backpatches: {label: Label.t, offset: int} list ref = ref []
      val code: Word8.t list ref = ref []
      val offset = ref 0
      val emitByte: Word8.t -> unit =
	 fn w =>
	 (List.push (code, w)
	  ; Int.inc offset)
      val emitOpcode = emitByte
      local
	 fun make (bits: int, {signed}): IntInf.t -> unit =
	    let
	       val bits = Bits.fromInt bits
	    in
	       fn i =>
	       if WordSize.isInRange (WordSize.fromBits bits, i,
				      {signed = signed})
		  then
		     let
			fun loop (j, i) =
			   if 0 = j
			      then ()
			   else
			      let
				 val (q, r) = IntInf.quotRem (i, 0x100)
				 val () = emitByte (Word8.fromIntInf r)
			      in
				 loop (j - 1, q)
			      end
		     in
			loop (Bytes.toInt (Bits.toBytes bits), i)
		     end
	       else Error.bug (concat ["emitWord", Bits.toString bits,
				       " failed on ", IntInf.toString i])
	    end
      in
	 val emitWord8 = make (8, {signed = false})
	 val emitWord16 = make (16, {signed = false})
	 val emitWordS16 = make (16, {signed = true})
	 val emitWord32 = make (32, {signed = false})
	 val emitWord64 = make (64, {signed = false})
      end
      val emitWordX: WordX.t -> unit =
	 fn w =>
	 (case WordSize.prim (WordX.size w) of
	     W8 => emitWord8
	   | W16 => emitWord16
	   | W32 => emitWord32
	   | W64 => emitWord64) (WordX.toIntInf w)
      val emitDirectIndex = emitWord16
      val emitIndirectIndex = emitWord16
      val emitLabel: Label.t -> unit =
	 fn l =>
	 (List.push (backpatches, {label = l, offset = !offset})
	  ; emitWord32 0)
      val emitLabel =
	 Trace.trace ("emitLabel", Label.layout, Unit.layout) emitLabel
      val rec emitLoadOperand = fn z => emitOperand (z, Load)
      and emitStoreOperand = fn z => emitOperand (z, Store)
      and emitOperand: Operand.t * LoadStore.t -> unit =
	 fn (z, ls) =>
	 let
	    val cty = Type.toCType (Operand.ty z)
	    datatype z = datatype Operand.t
	 in
	    case z of
	       ArrayOffset {base, index, ...} =>
		  (emitLoadOperand base
		   ; emitLoadOperand index
		   ; emitOpcode (arrayOffset (ls, cty)))
	     | Cast (z, _) => emitOperand (z, ls)
	     | Contents {oper, ...} =>
		   (emitLoadOperand oper
		    ; emitOpcode (contents (ls, cty)))
	     | File => emitWord32 0
	     | Frontier => emitOpcode (frontier ls)
	     | GCState => emitOpcode (gcState ls)
	     | Global g =>
		  (emitOpcode (global (ls, cty))
		   ; emitWord16 (Int.toIntInf (Global.index g)))
	     | Label l => emitLabel l
	     | Line => emitWord32 0
	     | Offset {base, offset = off, ...} =>
		  (emitLoadOperand base
		   ; emitOpcode (offsetOp (ls, cty))
		   ; emitWordS16 (Bytes.toIntInf off))
	     | Real _ => Error.bug "shouldn't see Real operands in bytecode"
	     | Register r =>
		  (emitOpcode (register (ls, cty))
		   ; emitWord16 (Int.toIntInf (Register.index r)))
	     | StackOffset (StackOffset.T {offset, ...}) =>
		  (emitOpcode (stackOffset (ls, cty))
		   ; emitWord16 (Bytes.toIntInf offset))
	     | StackTop => emitOpcode (stackTop ls)
	     | Word w =>
		  case ls of
		     Load => (emitOpcode (wordOpcode (ls, cty)); emitWordX w)
		   | Store => Error.bug "can't store to word constant"
	 end
      val emitOperand =
	 Trace.trace2
	 ("emitOperand", Operand.layout, LoadStore.layout, Unit.layout)
	 emitOperand
      fun move {dst, src} =
	 (emitLoadOperand src
	  ; emitStoreOperand dst)
      fun emitArgs args = Vector.foreach (Vector.rev args, emitLoadOperand)
      fun primApp {args, dst, prim} =
	 (emitArgs args
	  ; (case Prim.name prim of
		Prim.Name.FFI_Symbol {name, ...} =>
		   Option.app
		   (dst, fn dst =>
		    let
		       val ty = Operand.ty dst
		       val cty = Type.toCType ty
		    in
		       emitOpcode (ffiSymbol cty)
		       ; emitWord16 (Int.toIntInf
				     (ffiSymbolIndex (cty, name)))
		       ; emitStoreOperand dst
		    end)
	      | _ => 
		   (emitOpcode (primOp prim)
		    ; Option.app (dst, emitStoreOperand))))
      val emitStatement: Statement.t -> unit =
	 fn s =>
	 case s of
	    Move z => move z
	  | Noop => ()
	  | PrimApp z => primApp z
	  | ProfileLabel _ => emitOpcode profileLabel
      val emitStatement =
	 Trace.trace ("emitStatement", Statement.layout, Unit.layout)
	 emitStatement
      local
	 val gotoOp = opcode "Goto"
      in
	 fun goto (l: Label.t): unit =
	    (emitOpcode gotoOp; emitLabel l)
      end
      val pointerSize = WordSize.pointer ()
      fun push (label: Label.t, size: Bytes.t): unit =
	 (move {dst = (Operand.StackOffset
		       (StackOffset.T
			{offset = Bytes.- (size, Runtime.labelSize),
			 ty = Type.label label})),
		src = Operand.Label label}
	  ; primApp {args = (Vector.new2
			     (Operand.StackTop,
			      Operand.Word (WordX.fromIntInf
					    (Bytes.toIntInf size,
					     pointerSize)))),
		     dst = SOME Operand.StackTop,
		     prim = Prim.wordAdd pointerSize})
      fun emitTransfer (t: Transfer.t): unit =
	 let
	    datatype z = datatype Transfer.t
	 in
	    case t of
	       Arith {args, dst, overflow, prim, success} =>
		  (emitArgs args
		   ; emitOpcode (primOp prim)
		   ; emitStoreOperand dst
		   ; emitOpcode jumpOnOverflow
		   ; emitLabel overflow
		   ; goto success)
	     | CCall {args, frameInfo, func, return} =>
		  let
		     val () = emitArgs args
		     val CFunction.T {prototype, target, ...} = func
		     val () =
			Option.app
			(frameInfo, fn frameInfo =>
			 let
			    val size = Program.frameSize (program, frameInfo)
			    val () = push (valOf return, size)
			 in
			    ()
			 end)
		     val () = emitOpcode cCall
		     datatype z = datatype Target.t
		     val () =
			case target of
			   Direct name =>
			      emitDirectIndex (Int.toIntInf (directIndex name))
			 | Indirect =>
			      emitIndirectIndex
			      (Int.toIntInf (indirectIndex prototype))
		  in
		     ()
		  end
	     | Call {label, return, ...} =>
		  (Option.app (return, fn {return, size, ...} =>
			       push (return, size))
		   ; goto label)
	     | Goto l => goto l
	     | Raise => emitOpcode raisee
	     | Return => emitOpcode return
	     | Switch (Switch.T {cases, default, size, test}) =>
		  (emitLoadOperand test
		   ; emitOpcode (switch size)
		   ; emitWord16 (Int.toIntInf (Vector.length cases))
		   ; Vector.foreach (cases, fn (w, l) =>
				     (emitWordX w; emitLabel l))
		   ; Option.app (default, emitLabel))
	 end
      val emitTransfer =
	 Trace.trace ("emitTransfer", Transfer.layout, Unit.layout)
	 emitTransfer
      val {get = labelOffset: Label.t -> int,
	   set = setLabelOffset, ...} =
	 Property.getSetOnce (Label.plist,
			      Property.initRaise ("offset", Label.layout))
      val () =
	 List.foreach
	 (chunks, fn Chunk.T {blocks, ...} =>
	  Vector.foreach
	  (blocks, fn Block.T {kind, label, statements, transfer, ...} =>
	   (setLabelOffset (label, !offset)
	    ; Vector.foreach (statements, emitStatement)
	    ; emitTransfer transfer)))
      val word8ArrayToString: Word8.t array -> string =
	 fn a => String.tabulate (Array.length a, fn i =>
				  Char.fromWord8 (Array.sub (a, i)))
      val code = Array.fromListRev (!code)
      val {done, print, ...} = outputC ()
      (* Backpatch all label references. *)
      val () =
	 List.foreach
	 (!backpatches, fn {label, offset} =>
	  let
	     fun loop (i, address) =
		if 0 = address
		   then ()
		else (Array.update (code, i,
				    Word8.fromInt (Int.rem (address, 0x100)))
		      ; loop (i + 1, Int.quot (address, 0x100)))
	  in
	     loop (offset, labelOffset label)
	  end)
      val () =
	 (print "char *MLton_bytecode = \""
	  ; print (String.escapeC (word8ArrayToString code))
	  ; print "\";\n")
      val () = done ()
      val {done, file, print} = outputC ()
      fun rest () = initializeFFISymbols print
      val () =
	 CCodegen.outputDeclarations
	 {additionalMainArgs = [Int.toString (labelOffset (#label main))],
          includes = ["bytecode-main.h"],
	  print = print,
	  program = program,
	  rest = rest}
      val () = done ()
   in
      ()
   end

end
