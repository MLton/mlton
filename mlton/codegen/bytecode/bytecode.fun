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
   structure StackOffset = StackOffset
   structure Statement = Statement
   structure Switch = Switch
   structure Transfer = Transfer
   structure Type = Type
   structure WordSize = WordSize
   structure WordX = WordX
end

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

fun output {program, outputC} =
   let
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
      val flushFrontier = opcode "FlushFrontier"
      val flushStackTop = opcode "FlushStackTop"
      val goto = opcode "Goto"
      val move = CType.memo (fn t => opcode (concat [CType.toString t, "_move"]))
      val move = move o Type.toCType
      val object = opcode "Object"
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
      val arrayOffset = opcode "ArrayOffset"
      val contents = opcode "Contents"
      val frontier = opcode "Frontier"
      val gcState = opcode "GCState"
      val global = opcode "Global"
      val offsetOp = opcode "Offset"
      val register = opcode "Register"
      val stackOffset = opcode "StackOffset"
      val stackTop = opcode "StackTop"
      val wordOpcode = opcode "Word"
      val Program.T {chunks, main, ...} = program
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
      val emitLabel: Label.t -> unit =
	 fn l =>
	 (List.push (backpatches, {label = l, offset = !offset})
	  ; emitWord32 0)
      val emitLabel =
	 Trace.trace ("emitLabel", Label.layout, Unit.layout) emitLabel
      val rec emitOperand: Operand.t -> unit =
	 fn z =>
	 let
	    datatype z = datatype Operand.t
	 in
	    case z of
	       ArrayOffset {base, index, ...} =>
		  (emitOpcode arrayOffset
		   ; emitOperand base
		   ; emitOperand index)
	     | Cast (z, _) => emitOperand z
	     | Contents {oper, ...} =>
		  (emitOpcode contents
		   ; emitOperand oper)
	     | File => emitWord32 0
	     | Frontier => emitOpcode frontier
	     | GCState => emitOpcode gcState
	     | Global g =>
		  (emitOpcode global
		   ; emitWord16 (Int.toIntInf (Global.index g)))
	     | Label l => emitLabel l
	     | Line => emitWord32 0
	     | Offset {base, offset = off, ...} =>
		  (emitOpcode offsetOp
		   ; emitOperand base
		   ; emitWordS16 (Bytes.toIntInf off))
	     | Real _ => Error.bug "shouldn't see Real operands in bytecode"
	     | Register r =>
		  (emitOpcode register
		   ; emitWord16 (Int.toIntInf (Register.index r)))
	     | StackOffset (StackOffset.T {offset, ...}) =>
		  (emitOpcode stackOffset
		   ; emitWord16 (Bytes.toIntInf offset))
	     | StackTop => emitOpcode stackTop
	     | Word w => (emitOpcode wordOpcode; emitWordX w)
	 end
      val emitOperand =
	 Trace.trace ("emitOperand", Operand.layout, Unit.layout) emitOperand
      val emitFFISymbol: string * CType.t -> unit =
	 fn (name, ty) =>
	 (emitOpcode (ffiSymbol ty)
	  ; emitWord16 (Int.toIntInf (ffiSymbolIndex (ty, name))))
      val emitStatement: Statement.t -> unit =
	 fn s =>
	 let
	    datatype z = datatype Statement.t
	 in
	    case s of
	       Move {dst, src} =>
		  (emitOpcode (move (Operand.ty dst))
		   ; emitOperand src
		   ; emitOperand dst)
	     | Noop => ()
	     | Object {dst, header, size, ...} =>
		  (emitOpcode object
		   ; emitWord32 (Word.toIntInf header)
		   ; emitWord16 (Bytes.toIntInf size))
	     | PrimApp {args, dst, prim} =>
		  (case Prim.name prim of
		      Prim.Name.FFI_Symbol {name, ...} =>
			 Option.app
			 (dst, fn dst =>
			  let
			     val ty = Operand.ty dst
			  in
			     emitOpcode (move ty)
			     ; emitFFISymbol (name, Type.toCType ty)
			     ; emitOperand dst
			  end)
		    | _ => 
			 (emitOpcode (primOp prim)
			  ; Vector.foreach (args, emitOperand)
			  ; Option.app (dst, emitOperand)))
	     | ProfileLabel _ => emitOpcode profileLabel
	 end
      val emitStatement =
	 Trace.trace ("emitStatement", Statement.layout, Unit.layout)
	 emitStatement
      fun emitTransfer (t: Transfer.t): unit =
	 let
	    datatype z = datatype Transfer.t
	 in
	    case t of
	       Arith {args, dst, overflow, prim, success} => ()
	     | CCall {args, frameInfo, func, return} =>
		  let
		     val CFunction.T {maySwitchThreads,
				      modifiesFrontier,
				      modifiesStackTop,
				      name, return = returnTy, ...} = func
		     val () = emitOpcode cCall
		     val () =
			Vector.foreach (args, fn a =>
					(emitOpcode (move (Operand.ty a))
					 ; emitOperand a))
		  in
		     ()
		  end
	     | Call {label, return, ...} =>
		  (case return of
		      NONE => (emitOpcode goto
			       ; emitLabel label)
		    | SOME {return, size, ...} =>
			 (emitOpcode call
			  ; emitWord16 (Bytes.toIntInf size)
			  ; emitLabel return
			  ; emitLabel label))
	     | Goto l => (emitOpcode goto; emitLabel l)
	     | Raise => emitOpcode raisee
	     | Return => emitOpcode return
	     | Switch (Switch.T {cases, default, size, test}) =>
		  (emitOpcode (switch size)
		   ; emitOperand test
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
