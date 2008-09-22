(* Copyright (C) 2004-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
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
   structure FrameInfo = FrameInfo
   structure Global = Global
   structure Kind = Kind
   structure Label = Label
   structure Live = Live
   structure Operand = Operand
   structure Prim = Prim
   structure Program = Program
   structure Register = Register
   structure Runtime = Runtime
   structure Scale = Scale
   structure StackOffset = StackOffset
   structure Statement = Statement
   structure Switch = Switch
   structure Transfer = Transfer
   structure Type = Type
   structure WordSize = WordSize
   structure WordX = WordX
end

structure Target = CFunction.Target

fun implementsPrim p =
   let
      datatype z = datatype Prim.Name.t
   in
      case Prim.name p of
         CPointer_add => true
       | CPointer_diff => true
       | CPointer_equal => true
       | CPointer_fromWord => true
       | CPointer_lt => true
       | CPointer_sub => true
       | CPointer_toWord => true
       | FFI_Symbol _ => true
       | Real_Math_acos _ => true
       | Real_Math_asin _ => true
       | Real_Math_atan _ => true
       | Real_Math_atan2 _ => true
       | Real_Math_cos _ => true
       | Real_Math_exp _ => true
       | Real_Math_ln _ => true
       | Real_Math_log10 _ => true
       | Real_Math_sin _ => true
       | Real_Math_sqrt _ => true
       | Real_Math_tan _ => true
       | Real_abs _ => true
       | Real_add _ => true
       | Real_castToWord _ => true
       | Real_div _ => true
       | Real_equal _ => true
       | Real_ldexp _ => false
       | Real_le _ => true
       | Real_lt _ => true
       | Real_mul _ => true
       | Real_muladd _ => false
       | Real_mulsub _ => false
       | Real_neg _ => true
       | Real_qequal _ => false
       | Real_rndToReal _ => true
       | Real_rndToWord _ => true
       | Real_round _ => true
       | Real_sub _ => true
       | Word_add _ => true
       | Word_addCheck _ => true
       | Word_andb _ => true
       | Word_castToReal _ => true
       | Word_equal _ => true
       | Word_extdToWord _ => true
       | Word_lshift _ => true
       | Word_lt _ => true
       | Word_mul _ => true
       | Word_mulCheck _ => true
       | Word_neg _ => true
       | Word_negCheck _ => true
       | Word_notb _ => true
       | Word_orb _ => true
       | Word_quot _ => true
       | Word_rem _ => true
       | Word_rndToReal _ => true
       | Word_rol _ => true
       | Word_ror _ => true
       | Word_rshift _ => true
       | Word_sub _ => true
       | Word_subCheck _ => true
       | Word_xorb _ => true
       | _ => false
   end

structure Opcode = IntInf

structure CType =
   struct
      open CType

      val memo: (t -> 'a) -> t -> 'a =
         fn f =>
         let
            val m =
               CType.memo (fn t =>
                           case t of
                              CType.CPointer => NONE
                            | CType.Objptr => NONE 
                            | _ => SOME (f t))
         in
            fn t =>
            valOf (case t of
                      CType.CPointer => m (CType.csize ())
                    | CType.Objptr => m (CType.csize ())
                    | _ => m t)
         end

      val noSigned =
         memo (fn t =>
               case t of
                  Int8 => Word8
                | Int16 => Word16
                | Int32 => Word32
                | Int64 =>  Word64
                | _ => t)

      val toStringOrig = toString
      val toString = memo toString
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
                                 opcode = Int.toIntInf i,
                                 name = name},
                       fn _ => Error.bug 
                               (concat ["Bytecode.output: duplicate opcode: ", 
                                        name]))
                in
                   i + 1
                end
           | _ => Error.bug "Bytecode.output: strange opcode file"))
      val opcode: string -> Opcode.t =
         fn name =>
         #opcode (HashSet.lookupOrInsert
                  (table, String.hash name,
                   fn {name = name', ...} => name = name',
                   fn () => Error.bug 
                            (concat ["Bytecode.output: missing opcode: ", 
                                     name])))
      val callCounter = Counter.new 0
      val callCs = ref []
      fun callC {function: string,
                 prototype}: string =
         let
            val (args, result) = prototype
            val c = Counter.new 0
            fun temp () = concat ["t", Int.toString (Counter.next c)]
            fun cast (cty, src) =
               concat ["(", cty, ")(", src, ")"]
            val args =
               Vector.map
               (args, fn cty =>
                let
                   val mty = CType.noSigned cty
                   val (declarePop,mtemp) =
                      let
                         val mty = CType.toString mty
                         val mtemp = temp ()
                      in
                         (concat ["\t", mty, " ", mtemp, 
                                  " = PopReg (", mty, ");\n"],
                          mtemp)
                      end
                   val (declareCast, ctemp) =
                      if mty = cty
                         then ("", mtemp)
                         else let
                                 val cty = CType.toString cty
                                 val ctemp = temp ()
                              in
                                 (concat ["\t", cty, " ", ctemp, " = ",
                                          cast (cty, mtemp), ";\n"],
                                  ctemp)
                              end
                in
                   {declare = concat [declarePop, declareCast],
                    temp = ctemp}
                end)
            val call =
               concat [function,
                       " (",
                       (concat o List.separate) 
                       (Vector.toListMap (args, #temp), ", "),
                       ");\n"]
            val result =
               case result of
                  NONE => concat ["\t", call]
                | SOME cty =>
                     let
                        val mty = CType.noSigned cty
                     in
                        if mty = cty
                           then concat 
                                ["\tPushReg (", CType.toString cty, ") = ", 
                                 call]
                           else let
                                   val cty = CType.toString cty
                                   val ctemp = temp ()
                                   val mty = CType.toString mty
                                in
                                   concat 
                                   ["\t", cty, " ", ctemp, " = ", call,
                                    "\tPushReg (", mty, ") = ", 
                                    cast (mty, ctemp), ";\n"]
                                end
                     end
         in
            concat
            ["{\n",
             concat (Vector.toListMap (args, #declare)),
             "\tassertRegsEmpty ();\n",
             result,
             "\t}\n"]
         end
      local
         val calls = HashSet.new {hash = #hash}
      in
         val () =
            (* Visit each direct C Call in the program. *)
            List.foreach
            (chunks, fn Chunk.T {blocks, ...} =>
             Vector.foreach
             (blocks, fn Block.T {statements, transfer, ...} =>
              (Vector.foreach
               (statements, fn s =>
                case s of
                   PrimApp {dst, prim, ...} =>
                      (case Prim.name prim of
                          Prim.Name.FFI_Symbol {name, ...} =>
                             Option.app
                             (dst, fn _ =>
                              let
                                 val hash = String.hash name
                              in
                                 ignore
                                 (HashSet.lookupOrInsert
                                  (calls, hash,
                                   fn {name = n, symbol, ...} => 
                                   n = name andalso symbol,
                                   fn () =>
                                   let
                                      val index = Counter.next callCounter
                                      val display =
                                         let
                                            val ptr = 
                                               CType.toString CType.CPointer
                                         in
                                            concat
                                            ["PushReg (",ptr,") = ",
                                             "((",ptr,")(&",name,"));\n"]
                                         end
                                      val () =
                                         List.push
                                         (callCs, {display = display,
                                                   index = index})
                                   in
                                      {hash = hash,
                                       index = index,
                                       name = name,
                                       symbol = true}
                                   end))
                              end)
                        | _ => ())
                 | _ => ())
               ; (case transfer of
                     CCall {func, ...} =>
                        let
                           val CFunction.T {prototype, target, ...} = func
                           datatype z = datatype Target.t
                        in
                           case target of
                              Direct "Thread_returnToC" => ()
                            | Direct name =>
                                 let
                                    val hash = String.hash name
                                 in
                                    ignore
                                    (HashSet.lookupOrInsert
                                     (calls, hash,
                                      fn {name = n, symbol, ...} => 
                                      n = name andalso (not symbol),
                                      fn () =>
                                      let
                                         val index = Counter.next callCounter
                                         val display =
                                            callC {function = name,
                                                   prototype = prototype}
                                         val () =
                                            List.push
                                            (callCs, {display = display,
                                                      index = index})
                                      in
                                         {hash = hash,
                                          index = index,
                                          name = name,
                                          symbol = false}
                                      end))
                                 end
                            | Indirect => ()
                        end
                   | _ => ()))))
         fun directIndex (name: string) =
            #index (HashSet.lookupOrInsert
                    (calls, String.hash name,
                     fn {name = n, symbol, ...} => 
                     n = name andalso (not symbol),
                     fn () => Error.bug "Bytecode.output.directIndex"))
         fun ffiSymbolIndex (name: string) = 
            #index (HashSet.lookupOrInsert
                    (calls, String.hash name,
                     fn {name = n, symbol, ...} => 
                     n = name andalso symbol,
                     fn () => Error.bug "Bytecode.output.ffiSymbolIndex"))
      end
      fun indirectIndex (f: 'a CFunction.t): int =
         let
            val index = Counter.next callCounter
            val function =
               concat ["(", "*(", CFunction.cPointerType f, " fptr)) "]
            val display =
               concat ["{\n\t", CType.toStringOrig (CType.csize ()), 
                       " fptr = PopReg (", CType.toStringOrig (CType.csize ()), 
                       ");\n\t",
                       callC {function = function,
                              prototype = CFunction.prototype f},
                       "\t}\n"]
            val () =
               List.push (callCs, {display = display,
                                   index = index})
         in
            index
         end
      val callC = opcode "CallC"
      val jumpOnOverflow = opcode "JumpOnOverflow"
      val raisee = opcode "Raise"
      val returnOp = opcode "Return"
      val returnToC = opcode "Thread_returnToC"
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
      local
         fun make (name, distinguishPointers: bool)
            (ls: LoadStore.t, cty: CType.t): Opcode.t =
            opcode
            (concat [if distinguishPointers
                        then CType.toStringOrig cty
                     else CType.toString cty,
                     "_", LoadStore.toString ls, name])
      in
         val arrayOffset = make ("ArrayOffset", false)
         val contents = make ("Contents", false)
         val global = make ("Global", true)
         val offsetOp = make ("Offset", false)
         val register = make ("Register", true)
         val stackOffset = make ("StackOffset", false)
         val wordOpcode = make ("Word", false)
      end
      val branchIfZero = opcode "BranchIfZero"
      fun gpnr ls = opcode (concat [LoadStore.toString ls, "GPNR"])
      local
         fun make name (ls: LoadStore.t): Opcode.t =
            opcode (concat [LoadStore.toString ls, name])
      in
         val frontier = make "Frontier"
         val gcState = make "GCState"
         val stackTop = make "StackTop"
      end
      val code: Word8.t list ref = ref []
      val offset = ref 0
      val emitByte: Word8.t -> unit =
         fn w =>
         (List.push (code, w)
          ; Int.inc offset)
      local
         fun make (bits: int, {signed}): IntInf.t -> unit =
            let
               val bits = Bits.fromInt bits
            in
               fn i =>
               if not (WordSize.isInRange (WordSize.fromBits bits, i,
                                           {signed = signed}))
                  then Error.bug (concat ["Bytecode.output: emitWord", 
                                          Bits.toString bits,
                                          " failed on ", 
                                          IntInf.toString i])
               else
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
                     loop (Bytes.toInt (Bits.toBytes bits),
                           IntInf.mod (i, IntInf.<< (1, Bits.toWord bits)))
                  end
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
      val emitOpcode = emitWord16
      val emitPrim: 'a Prim.t -> unit =
         fn p => emitOpcode (opcode (Prim.toString p))
      fun emitCallC (index: int): unit =
         (emitOpcode callC
          ; emitWord16 (Int.toIntInf index))
      val {get = labelInfo: Label.t -> {block: Block.t,
                                        emitted: bool ref,
                                        occurrenceOffsets: int list ref,
                                        offset: int option ref},
           set = setLabelInfo, ...} =
         Property.getSetOnce (Label.plist,
                              Property.initRaise ("info", Label.layout))
      val needToEmit: Label.t list ref = ref []
      val emitLabel: Label.t -> unit =
         fn l =>
         let
            val {emitted, occurrenceOffsets, ...} = labelInfo l
            val () = List.push (occurrenceOffsets, !offset)
            val () = if !emitted then () else List.push (needToEmit, l)
         in
            emitWordX (WordX.zero (WordSize.cpointer ()))
         end
      val emitLabel =
         Trace.trace ("Bytecode.emitLabel", Label.layout, Unit.layout) emitLabel
      fun loadStoreStackOffset (offset, cty, ls) =
         (emitOpcode (stackOffset (ls, cty))
          ; emitWord16 (Bytes.toIntInf offset))
      val rec emitLoadOperand = fn z => emitOperand (z, Load)
      and emitOperand: Operand.t * LoadStore.t -> unit =
         fn (z, ls) =>
         let
            val cty = Type.toCType (Operand.ty z)
            datatype z = datatype Operand.t
         in
            case z of
               ArrayOffset {base, index, offset, scale, ...} =>
                  (emitLoadOperand base
                   ; emitLoadOperand index
                   ; emitOpcode (arrayOffset (ls, cty))
                   ; emitWord16 (Bytes.toIntInf offset)
                   ; emitWord8 (Int.toIntInf (Scale.toInt scale)))
             | Cast (z, _) => emitOperand (z, ls)
             | Contents {oper, ...} =>
                   (emitLoadOperand oper
                    ; emitOpcode (contents (ls, cty)))
             | File => emitOperand (Null, ls)
             | Frontier => emitOpcode (frontier ls)
             | GCState => emitOpcode (gcState ls)
             | Global g =>
                  (if Global.isRoot g
                      then emitOpcode (global (ls, cty))
                   else emitOpcode (gpnr ls)
                   ; emitWord16 (Int.toIntInf (Global.index g)))
             | Label l =>
                  (emitOpcode (wordOpcode (ls, cty))
                   ; emitLabel l)
             | Line => (emitOpcode (wordOpcode (ls, cty))
                        ; emitWordX (WordX.zero (WordSize.cint ())))
             | Null => (emitOpcode (wordOpcode (ls, cty))
                        ; emitWordX (WordX.zero (WordSize.cpointer ())))
             | Offset {base, offset = off, ...} =>
                  (emitLoadOperand base
                   ; emitOpcode (offsetOp (ls, cty))
                   ; emitWordS16 (Bytes.toIntInf off))
             | Real _ => Error.bug "Bytecode.emitOperand: Real"
             | Register r =>
                  (emitOpcode (register (ls, cty))
                   ; emitWord16 (Int.toIntInf (Register.index r)))
             | StackOffset (StackOffset.T {offset, ...}) =>
                  loadStoreStackOffset (offset, cty, ls)
             | StackTop => emitOpcode (stackTop ls)
             | Word w =>
                  case ls of
                     Load => (emitOpcode (wordOpcode (ls, cty)); emitWordX w)
                   | Store => Error.bug "Bytecode.emitOperand: Word, Store"
         end
      val emitLoadOperand =
         Trace.trace
         ("Bytecode.emitLoadOperand", Operand.layout, Unit.layout)
         emitLoadOperand
      val emitOperand =
         Trace.trace2
         ("Bytecode.emitOperand", Operand.layout, LoadStore.layout, Unit.layout)
         emitOperand
      fun emitStoreOperand z = emitOperand (z, Store)
      fun move {dst, src} =
         (emitLoadOperand src
          ; emitStoreOperand dst)
      fun emitArgs args = Vector.foreach (Vector.rev args, emitLoadOperand)
      fun primApp {args, dst, prim} =
         case Prim.name prim of
            Prim.Name.FFI_Symbol {name, ...} =>
               Option.app
               (dst, fn dst =>
                (emitCallC (ffiSymbolIndex name)
                 ; emitStoreOperand dst))
          | _ => 
               (emitArgs args
                ; emitPrim prim
                ; Option.app (dst, emitStoreOperand))
      val emitStatement: Statement.t -> unit =
         fn s =>
         case s of
            Move z => move z
          | Noop => ()
          | PrimApp z => primApp z
          | ProfileLabel _ => Error.bug "Bytecode.output.emitStatement: profileLabel"
      val emitStatement =
         Trace.trace ("Bytecode.emitStatement", Statement.layout, Unit.layout)
         emitStatement
      val gotoOp = opcode "Goto"
      val pointerSize = WordSize.cpointer ()
      val flushStackTopOp = opcode "FlushStackTop"
      val amTimeProfiling = 
         !Control.profile = Control.ProfileTimeField
         orelse !Control.profile = Control.ProfileTimeLabel
      fun shiftStackTop (size: Bytes.t) =
         (primApp {args = (Vector.new2
                           (Operand.StackTop,
                            Operand.Word (WordX.fromIntInf
                                          (Bytes.toIntInf size,
                                           pointerSize)))),
                   dst = SOME Operand.StackTop,
                   prim = Prim.wordAdd pointerSize}
          ; if amTimeProfiling
               then emitOpcode flushStackTopOp
            else ())
      fun push (label: Label.t, size: Bytes.t): unit =
         (move {dst = (Operand.StackOffset
                       (StackOffset.T
                        {offset = Bytes.- (size, Runtime.labelSize ()),
                         ty = Type.label label})),
                src = Operand.Label label}
          ; shiftStackTop size)
      fun pop (size: Bytes.t) = shiftStackTop (Bytes.~ size)
      val () =
         List.foreach
         (chunks, fn Chunk.T {blocks, ...} =>
          Vector.foreach
          (blocks, fn block =>
           setLabelInfo (Block.label block,
                         {block = block,
                          emitted = ref false,
                          occurrenceOffsets = ref [],
                          offset = ref NONE})))
      val traceEmitTransfer =
         Trace.trace ("Bytecode.emitTransfer", Transfer.layout, Unit.layout)
      fun emitBlock (Block.T {kind, label, statements, transfer, ...}): unit =
         let
            val () =
               Option.app
               (Kind.frameInfoOpt kind,
                fn FrameInfo.T {frameLayoutsIndex} =>
                ((* This load will never be used.  We just have it there
                  * so the disassembler doesn't get confused when it
                  * sees the frameLayoutsIndex.
                  *)
                 emitOpcode (wordOpcode (Load, CType.Word32))
                 ; emitWord32 (Int.toIntInf frameLayoutsIndex)))
            val () = #offset (labelInfo label) := SOME (!offset)
            fun popFrame () =
               Option.app (Kind.frameInfoOpt kind, fn fi =>
                           pop (Program.frameSize (program, fi)))
            val () =
               case kind of
                  Kind.CReturn {dst, func, ...} =>
                     (case #2 (CFunction.prototype func) of
                         NONE => popFrame ()
                       | SOME cty => 
                            case dst of
                               NONE =>
                                  (* Even if there is no dst, we still need to
                                   * pop the value returned by the C function.
                                   * We write it to a bogus location in the
                                   * callee's frame before popping back to the
                                   * caller.
                                   * We mediated between the signed/unsigned treatment
                                   * in the stub.
                                   *)
                                  (loadStoreStackOffset 
                                   (Bytes.zero, CType.noSigned cty, Store)
                                   ; popFrame ())
                             | SOME z =>
                                  (popFrame ()
                                   ; emitStoreOperand (Live.toOperand z)))
                | _ => popFrame ()
            val () =
               (Vector.foreach (statements, emitStatement)
                ; emitTransfer transfer)
         in
            ()
         end
      and goto (l: Label.t): unit =
         let
            val {block as Block.T {kind, ...}, emitted, ...} = labelInfo l
         in
            if !emitted orelse isSome (Kind.frameInfoOpt kind)
               then (emitOpcode gotoOp; emitLabel l)
            else (emitted := true; emitBlock block)
         end
      and emitTransfer arg: unit =
         traceEmitTransfer
         (fn (t: Transfer.t) => 
          let
             datatype z = datatype Transfer.t
          in
             case t of
                Arith {args, dst, overflow, prim, success} =>
                   (emitArgs args
                    ; emitPrim prim
                    ; emitStoreOperand dst
                    ; emitOpcode jumpOnOverflow
                    ; emitLabel overflow
                    ; goto success)
              | CCall {args, frameInfo, func, return} =>
                   let
                      val () = emitArgs args
                      val CFunction.T {maySwitchThreads, target, ...} =
                         func
                      val () =
                         Option.app
                         (frameInfo, fn frameInfo =>
                          push (valOf return,
                                Program.frameSize (program, frameInfo)))
                      datatype z = datatype Target.t
                      val () =
                         case target of
                            Direct "Thread_returnToC" => emitOpcode returnToC
                          | Direct name => emitCallC (directIndex name)
                          | Indirect => emitCallC (indirectIndex func)
                      val () =
                         if maySwitchThreads
                            then emitOpcode returnOp
                            else Option.app (return, goto)
                   in
                      ()
                   end
              | Call {label, return, ...} =>
                   (Option.app (return, fn {return, size, ...} =>
                                push (return, size))
                    ; goto label)
              | Goto l => goto l
              | Raise => emitOpcode raisee
              | Return => emitOpcode returnOp
              | Switch (Switch.T {cases, default, size, test}) =>
                   let
                      val () = emitLoadOperand test
                      fun bool (a: Label.t, b: Label.t) =
                         (emitOpcode branchIfZero
                          ; emitLabel b
                          ; goto a)
                      fun normal () =
                         let
                            val numCases =
                               Vector.length cases
                               + (if isSome default then 1 else 0)
                               - 1
                            val () =
                               (emitOpcode (switch size)
                                ; emitWord16 (Int.toIntInf numCases))
                            fun emitCases cases =
                               Vector.foreach (cases, fn (w, l) =>
                                               (emitWordX w; emitLabel l))
                         in
                            case default of
                               NONE =>
                                  (emitCases (Vector.dropSuffix (cases, 1))
                                   ; goto (#2 (Vector.last cases)))
                             | SOME l =>
                                  (emitCases cases; goto l)
                         end
                   in
                      if 2 = Vector.length cases
                         andalso Option.isNone default
                         andalso WordSize.equals (size, WordSize.bool)
                         then
                            let
                               val (c0, l0) = Vector.sub (cases, 0)
                               val (c1, l1) = Vector.sub (cases, 1)
                               val i0 = WordX.toIntInf c0
                               val i1 = WordX.toIntInf c1
                            in
                               if i0 = 0 andalso i1 = 1
                                  then bool (l1, l0)
                                  else if i0 = 1 andalso i1 = 0
                                          then bool (l0, l1)
                                          else normal ()
                            end
                         else normal ()
                   end
          end) arg
      fun loop () =
         case !needToEmit of
            [] => ()
          | l :: ls =>
               let
                  val () = needToEmit := ls
                  val {block, emitted, ...} = labelInfo l
                  val () =
                     if !emitted
                        then ()
                     else (emitted := true; emitBlock block)
               in
                  loop ()
               end
      val () = List.push (needToEmit, #label main)
      val () = loop ()
      (* Discard unreachable blocks *)
      val chunks =
         List.map
         (chunks, fn Chunk.T {blocks, chunkLabel, regMax} =>
          let
             val blocks =
                Vector.keepAll
                (blocks, fn Block.T {label, ...} =>
                 ! (#emitted (labelInfo label)))
          in
             Chunk.T {blocks = blocks,
                      chunkLabel = chunkLabel,
                      regMax = regMax}
          end)
      fun labelOffset l = valOf (! (#offset (labelInfo l)))
      val code = Array.fromListRev (!code)
      (* Backpatch all label references. *)
      val () =
         List.foreach
         (chunks, fn Chunk.T {blocks, ...} =>
          Vector.foreach
          (blocks, fn Block.T {label, ...} =>
           let
              val {occurrenceOffsets = r, offset, ...} = labelInfo label
              val offset = valOf (!offset)
              fun loop (i, address) =
                 if 0 = address
                    then ()
                 else (Array.update (code, i,
                                     Word8.fromInt (Int.rem (address, 0x100)))
                       ; loop (i + 1, Int.quot (address, 0x100)))
           in
              List.foreach (!r, fn occ => loop (occ, offset))
          end))
      val {done, file = _, print} = outputC ()
      val print =
         Trace.trace ("Bytecode.print", String.layout, Unit.layout) print
      val () =
         CCodegen.outputDeclarations
         {additionalMainArgs = [Int.toString (labelOffset (#label main))],
          includes = ["bytecode-main.h"],
          print = print,
          program = program,
          rest = fn () => ()}
      val () = done ()
      val {done, print, ...} = outputC ()
      fun declareCallC () =
          (print "PRIVATE void MLton_callC (int i) {\n"
           ; print "switch (i) {\n"
           ; List.foreach (!callCs, fn {display, index} =>
                           (print (concat ["case ", Int.toString index, ":\n\t"])
                            ; print display
                            ; print "break;\n"))
           ; print "}}\n")
      val () =
          (print "#include \"bytecode.h\"\n\n"
           ; List.foreach (chunks, fn c =>
                           CCodegen.declareFFI (c, {print = print}))
           ; print "\n"
           ; declareCallC ()
           ; print "\n")
      val word8ArrayToString: Word8.t array -> string =
         fn a => String.tabulate (Array.length a, fn i =>
                                  Char.fromWord8 (Array.sub (a, i)))
      val {labels, offsets, ...} =
         List.fold
         (chunks, {labels = [], offset = 0, offsets = []},
          fn (Chunk.T {blocks, ...}, ac) =>
          Vector.fold
          (blocks, ac, fn (Block.T {label, ...}, {labels, offset, offsets}) =>
           let
              val offsets = {code = labelOffset label, name = offset} :: offsets
              val label = Label.toString label
           in
              {labels = label :: labels,
               offset = offset + String.size label + 1,
               offsets = offsets}
           end))
      val labels =
         concat (List.fold (labels, [], fn (l, ac) => l :: "\000" :: ac))
      val offsets = rev offsets
      fun printString s =
         (print "\t\""; print (String.escapeC s); print "\",\n")
      fun printInt i = print (concat ["\t", Int.toString i, ",\n"])
      val () =
         (print "static struct NameOffsets nameOffsets [] = {\n"
          ; List.foreach (offsets, fn {code, name} =>
                          print (concat ["\t{ ",
                                         Int.toString code, ", ",
                                         Int.toString name,
                                         " },\n"]))
          ; print "};\n"
          ; print "PRIVATE struct Bytecode MLton_bytecode = {\n"
          ; printString labels
          ; printString (word8ArrayToString code)
          ; printInt (Array.length code)
          ; print "\tnameOffsets,\n"
          ; printInt (List.length offsets)
          ; print "};\n")
      val () = done ()
   in
      ()
   end

end
