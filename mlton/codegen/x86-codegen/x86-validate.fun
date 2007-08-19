(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor x86Validate(S: X86_VALIDATE_STRUCTS): X86_VALIDATE =
struct

  open S;
  open x86;

  val tracerTop = x86.tracerTop

  structure Register =
    struct
      open Register

      fun validate {register}
        = if not (List.contains(registers (size register),
                                register,
                                eq))
            then Error.bug "x86Validate.Register.validate"
            else true

      fun validate_base {register}
        = if not (validate {register = register}
                  andalso
                  List.contains(baseRegisters,
                                register,
                                eq))
            then Error.bug "x86Validate.Register.validate_base"
            else true

      fun validate_index {register}
        = if not (validate {register = register}
                  andalso
                  List.contains(indexRegisters,
                                register,
                                eq))
            then Error.bug "x86Validate.Register.validate_index"
            else true
    end

  structure FltRegister =
    struct
      open FltRegister

      fun validate {fltregister = FltRegister.T i}
        = if 0 > i orelse i > 7
            then Error.bug "x86Validate.FltRegister.validate"
            else true
    end

  structure Address =
    struct
      open Address

      fun validate {address as Address.T {base, index, ...}}
        = let
            val _ = case base
                      of NONE => ()
                       | SOME r => if Register.validate_base {register = r}
                                     then ()
                                     else Error.bug "x86Validate.Address.validate: base"

            val _ = case index
                      of NONE => ()
                       | SOME r => if Register.validate_index {register = r}
                                     then ()
                                     else Error.bug "x86Validate.Address.validate: index"
          in
            case address
              of Address.T {disp = NONE, base = NONE, 
                            index = NONE, scale = NONE}
               => Error.bug "x86Validate.Address.validate"
               | Address.T {index = NONE, scale = SOME _, ...}
               => Error.bug "x86Validate.Address.validate: scale"
               | _ => true
          end
    end

  structure Operand =
    struct
      open Operand

      fun validate {operand: t}
        = case operand
            of Register r => Register.validate {register = r}
             | FltRegister f => FltRegister.validate {fltregister = f}
             | Immediate _ => true
             | Label _ => true
             | Address a => Address.validate {address = a}
             | MemLoc _ => Error.bug "x86Validate.Operand.validate: MemLoc"
    end

  structure Instruction =
    struct
      open x86.Instruction

      fun validate {instruction: t}
        = case instruction
            of NOP => true
             | HLT => true
             | BinAL {src, dst, size, ...}
               (* Integer binary arithmetic(w/o mult & div)/logic instructions.
                * Require src/dst operands as follows:
                *
                *              dst
                *          reg imm lab add 
                *      reg  X           X
                *  src imm  X           X
                *      lab
                *      add  X
                *
                * Require size modifier class as follows: INT
                *)
             => let
                  val _ = if Size.class size = Size.INT
                            then ()
                            else Error.bug "x86Validate.Instruction.validate: BinAL, size"
                  val _ = case Operand.size src
                            of NONE => ()
                             | SOME srcsize 
                             => if srcsize = size
                                  then ()
                                  else Error.bug "x86Validate.Instruction.validate: BinAL, srcsize"
                  val _ = case Operand.size dst
                            of NONE => ()
                             | SOME dstsize 
                             => if dstsize = size
                                  then ()
                                  else Error.bug "x86Validate.Instruction.validate: BinAL, dstsize"
                in
                  case (src,dst)
                    of (Operand.MemLoc _, _)
                     => Error.bug "x86Validate.Instruction.validate: BinAL, src:MemLoc"
                     | (_, Operand.MemLoc _)
                     => Error.bug "x86Validate.Instruction.validate: BinAL, dst:MemLoc"
                     | (Operand.FltRegister _, _)
                     => Error.bug "x86Validate.Instruction.validate: BinAL, src:FltRegister"
                     | (Operand.Label _, _)
                     => Error.bug "x86Validate.Instruction.validate: BinAL, src:Label"
                     | (_, Operand.FltRegister _)
                     => Error.bug "x86Validate.Instruction.validate: BinAL, dst:FltRegister"
                     | (_, Operand.Immediate _)
                     => Error.bug "x86Validate.Instruction.validate: BinAL, dst:Immediate"
                     | (_, Operand.Label _)
                     => Error.bug "x86Validate.Instruction.validate: BinAL, dst:Label"
                     | (Operand.Address _, Operand.Address _)
                     => Error.bug "x86Validate.Instruction.validate: BinAL, src,dst:Address"
                     | _ => (Operand.validate {operand = src}) andalso
                            (Operand.validate {operand = dst})
                end
             | MD {src, size, ...} 
               (* Integer multiplication and division.
                * Require src operand as follows:
                *
                *               src
                *           reg imm lab add
                *            X           X
                *
                * Require size modifier class as follows: INT
                *)
             => let
                  val _ = if Size.class size = Size.INT
                            then ()
                            else Error.bug "x86Validate.Instruction.validate: BinMD, size"
                  val _ = case Operand.size src
                            of NONE => ()
                             | SOME srcsize 
                             => if srcsize = size
                                  then ()
                                  else Error.bug "x86Validate.Instruction.validate: MD, srcsize"
                in
                  case src
                    of Operand.MemLoc _
                     => Error.bug "x86Validate.Instruction.validate: MD, src:MemLoc"
                     | Operand.FltRegister _
                     => Error.bug "x86Validate.Instruction.validate: MD, src:FltRegister"
                     | Operand.Immediate _
                     => Error.bug "x86Validate.Instruction.validate: MD, src:Immediate"
                     | Operand.Label _
                     => Error.bug "x86Validate.Instruction.validate: MD, src:Label"
                     | _ => (Operand.validate {operand = src})
                end
             | IMUL2 {src, dst, size}
               (* Integer signed/unsigned multiplication (two operand form).
                * Require src/dst operands as follows:
                *
                *              dst
                *          reg imm lab add 
                *      reg  X
                *  src imm  X
                *      lab
                *      add  X
                *
                * Require size modifier class as follows: INT(WORD, LONG)
                *)
             => let
                  val _ = case size
                            of Size.WORD => ()
                             | Size.LONG => ()
                             | _ => Error.bug "x86Validate.Instruction.validate: IMUL2, size"
                  val _ = case Operand.size src
                            of NONE => ()
                             | SOME srcsize 
                             => if srcsize = size
                                  then ()
                                  else Error.bug "x86Validate.Instruction.validate: IMUL2, srcsize"
                  val _ = case Operand.size dst
                            of NONE => ()
                             | SOME dstsize 
                             => if dstsize = size
                                  then ()
                                  else Error.bug "x86Validate.Instruction.validate: IMUL2, dstsize"
                in
                  case (src,dst)
                    of (Operand.MemLoc _, _)
                     => Error.bug "x86Validate.Instruction.validate: IMUL2, src:MemLoc"
                     | (_, Operand.MemLoc _)
                     => Error.bug "x86Validate.Instruction.validate: IMUL2, dst:MemLoc"
                     | (Operand.FltRegister _, _)
                     => Error.bug "x86Validate.Instruction.validate: IMUL2, src:FltRegister"
                     | (Operand.Label _, _)
                     => Error.bug "x86Validate.Instruction.validate: IMUL2, src:Label"
                     | (_, Operand.FltRegister _)
                     => Error.bug "x86Validate.Instruction.validate: IMUL2, dst:FltRegister"
                     | (_, Operand.Immediate _)
                     => Error.bug "x86Validate.Instruction.validate: IMUL2, dst:Immediate"
                     | (_, Operand.Label _)
                     => Error.bug "x86Validate.Instruction.validate: IMUL2, dst:Label"
                     | (Operand.Address _, _)
                     => Error.bug "x86Validate.Instruction.validate: IMUL2, src:Address"
                     | _ => (Operand.validate {operand = src}) andalso
                            (Operand.validate {operand = dst})
                end
             | UnAL {dst, size, ...}
               (* Integer unary arithmetic/logic instructions.
                * Require dst operand as follows:
                *
                *               dst
                *           reg imm lab add
                *            X           X
                *
                * Require size modifier class as follows: INT
                *)
             => let
                  val _ = if Size.class size = Size.INT
                            then ()
                            else Error.bug "x86Validate.Instruction.validate: UnAL, size"
                  val _ = case Operand.size dst
                            of NONE => ()
                             | SOME dstsize 
                             => if dstsize = size
                                  then ()
                                  else Error.bug "x86Validate.Instruction.validate: UnAL, dstsize"
                in
                  case dst
                    of Operand.MemLoc _
                     => Error.bug "x86Validate.Instruction.validate: UnAL, dst:MemLoc"
                     | Operand.FltRegister _
                     => Error.bug "x86Validate.Instruction.validate: UnAL, dst:FltRegister"
                     | Operand.Immediate _
                     => Error.bug "x86Validate.Instruction.validate: UnAL, dst:Immediate"
                     | Operand.Label _
                     => Error.bug "x86Validate.Instruction.validate: UnAL, dst:Label"
                     | _ => (Operand.validate {operand = dst})
                end
             | SRAL {count, dst, size, ...}
               (* Integer shift/rotate arithmetic/logic instructions.
                * Require count operand as follows:
                *
                *               src
                *           reg imm lab add
                *            *   X
                *  * only register %cl
                *
                * Require dst operand as follows:
                *
                *               dst
                *           reg imm lab add
                *            X           X
                *
                * Require size modifier class as follows: INT
                *)
             => let     
                  val _ = if Size.class size = Size.INT
                            then ()
                            else Error.bug "x86Validate.Instruction.validate: SRAL, size"
                  val _ = case Operand.size dst
                            of NONE => ()
                             | SOME dstsize 
                             => if dstsize = size
                                  then ()
                                  else Error.bug "x86Validate.Instruction.validate: SRAL, dstsize"

                  val _ = case count
                            of Operand.MemLoc _
                             => Error.bug "x86Validate.Instruction.validate: SRAL, count:MemLoc"
                             | Operand.FltRegister _
                             => Error.bug "x86Validate.Instruction.validate: SRAL, count:FltRegister"
                             | Operand.Label _
                             => Error.bug "x86Validate.Instruction.validate: SRAL, count:Label"
                             | Operand.Address _
                             => Error.bug "x86Validate.Instruction.validate: SRAL, count:Address"
                             | Operand.Register (Register.T {reg, part})
                             => if reg <> Register.ECX orelse
                                   part <> Register.L
                                  then Error.bug 
                                       "x86Validate.Instruction.validate: SRAL, count:Register"
                                  else ()
                             | _ => ()
                in
                  case dst
                    of Operand.MemLoc _
                     => Error.bug "x86Validate.Instruction.validate: SRAL, dst:MemLoc"
                     | Operand.FltRegister _
                     => Error.bug "x86Validate.Instruction.validate: SRAL, dst:FltRegister"
                     | Operand.Immediate _
                     => Error.bug "x86Validate.Instruction.validate: SRAL, dst:Immediate"
                     | Operand.Label _
                     => Error.bug "x86Validate.Instruction.validate: SRAL, dst:Label"
                     | _ => Operand.validate {operand = dst}
                end
             | CMP {src1, src2, size}
               (* Arithmetic compare;  p. 116
                * Require src1/src2 operands as follows:
                *
                *               src2
                *           reg imm lab add 
                *       reg  X   X       X
                *  src1 imm
                *       lab
                *       add  X   X
                *
                * Require size modifier class as follows: INT
                *)
             => let
                  val _ = if Size.class size = Size.INT
                            then ()
                            else Error.bug "x86Validate.Instruction.validate: CMP, size"
                  val _ = case Operand.size src1
                            of NONE => ()
                             | SOME src1size 
                             => if src1size = size
                                  then ()
                                  else Error.bug "x86Validate.Instruction.validate: CMP, src1size"
                  val _ = case Operand.size src2
                            of NONE => ()
                             | SOME src2size 
                             => if src2size = size
                                  then ()
                                  else Error.bug "x86Validate.Instruction.validate: CMP, src2size"
                in
                  case (src1,src2)
                    of (Operand.MemLoc _, _)
                     => Error.bug "x86Validate.Instruction.validate: CMP, src1:MemLoc"
                     | (_, Operand.MemLoc _)
                     => Error.bug "x86Validate.Instruction.validate: CMP, src2:MemLoc"
                     | (Operand.FltRegister _, _)
                     => Error.bug "x86Validate.Instruction.validate: CMP, src1: FltRegister"
                     | (Operand.Immediate _, _)
                     => Error.bug "x86Validate.Instruction.validate: CMP, src1:Immediate"
                     | (Operand.Label _, _)
                     => Error.bug "x86Validate.Instruction.validate: CMP, src1:Label"
                     | (_, Operand.FltRegister _)
                     => Error.bug "x86Validate.Instruction.validate: CMP, src2: FltRegister"
                     | (_, Operand.Label _)
                     => Error.bug "x86Validate.Instruction.validate: CMP, src2:Label"
                     | (Operand.Address _, Operand.Address _)
                     => Error.bug "x86Validate.Instruction.validate: CMP, src1,src2:Address"
                     | _ => (Operand.validate {operand = src1}) andalso
                            (Operand.validate {operand = src2})
                end
             | TEST {src1, src2, size}
               (* Logical compare; p. 728
                * Require src1/src2 operands as follows:
                *
                *               src2
                *           reg imm lab add 
                *       reg  X   X       X
                *  src1 imm  
                *       lab
                *       add  X   X    
                *
                * Require size modifier class as follows: INT
                *)
             => let
                  val _ = if Size.class size = Size.INT
                            then ()
                            else Error.bug "x86Validate.Instruction.validate: TEST, size"
                  val _ = case Operand.size src1
                            of NONE => ()
                             | SOME src1size 
                             => if src1size = size
                                  then ()
                                  else Error.bug "x86Validate.Instruction.validate: TEST, src1size"
                  val _ = case Operand.size src2
                            of NONE => ()
                             | SOME src2size 
                             => if src2size = size
                                  then ()
                                  else Error.bug "x86Validate.Instruction.validate: TEST, src2size"
                in
                  case (src1,src2)
                    of (Operand.MemLoc _, _)
                     => Error.bug "x86Validate.Instruction.validate: TEST, src1:MemLoc"
                     | (_, Operand.MemLoc _)
                     => Error.bug "x86Validate.Instruction.validate: TEST, src2:MemLoc"
                     | (Operand.FltRegister _, _)
                     => Error.bug "x86Validate.Instruction.validate: TEST, src1: FltRegister"
                     | (Operand.Immediate _, _)
                     => Error.bug "x86Validate.Instruction.validate: TEST, src1:Immediate"      
                     | (Operand.Label _, _)
                     => Error.bug "x86Validate.Instruction.validate: TEST, src1:Label"
                     | (_, Operand.FltRegister _)
                     => Error.bug "x86Validate.Instruction.validate: TEST, src2: FltRegister"
                     | (_, Operand.Label _)
                     => Error.bug "x86Validate.Instruction.validate: TEST, src2:Label"
                     | (Operand.Address _, Operand.Address _)
                     => Error.bug "x86Validate.Instruction.validate: TEST, src1,src2:Address"
                     | _ => (Operand.validate {operand = src1}) andalso
                            (Operand.validate {operand = src2})
                end
             | SETcc {dst, size, ...}
               (* Set byte on condition; p. 672
                * Require dst operand as follows:
                *
                *                dst
                *            reg imm lab add
                *             *           X
                *  * only byte registers
                *
                * Require size modifier class as follows: INT(BYTE)
                *)
             => let
                  val _ = case size
                            of Size.BYTE => ()
                             | _ => Error.bug "x86Validate.Instruction.validate: SETcc, size"
                  val _ = case Operand.size dst
                            of NONE => ()
                             | SOME dstsize 
                             => if dstsize = size
                                  then ()
                                  else Error.bug "x86Validate.Instruction.validate: SETcc, dstsize"
                in
                  case dst
                    of Operand.MemLoc _
                     => Error.bug "x86Validate.Instruction.validate: SETcc, dst:MemLoc"
                     | Operand.FltRegister _
                     => Error.bug "x86Validate.Instruction.validate: SETcc, dst:FltRegister"
                     | Operand.Immediate _
                     => Error.bug "x86Validate.Instruction.validate: SETcc, dst:Immediate"
                     | Operand.Label _
                     => Error.bug "x86Validate.Instruction.validate: SETcc, dst:Label"
                     | _ => (Operand.validate {operand = dst})
                end
             | JMP {target, ...}
               (* Jump; p. 373
                * Require target operand as follows:
                *
                *               target
                *            reg imm lab add
                *             X   X   X   X
                *)
             => let
                in
                  case target
                    of Operand.MemLoc _
                     => Error.bug "x86Validate.Instruction.validate: JMP, target:MemLoc"
                     | Operand.FltRegister _
                     => Error.bug "x86Validate.Instruction.validate: JMP, target:FltRegister"
                     | _ => (Operand.validate {operand = target})
                end
             | Jcc {target, ...}
               (* Jump if condition is met; p. 369
                * Require target operand as follows:
                *
                *               target
                *            reg imm lab add
                *                 X   X
                *)
             => let
                in
                  case target
                    of Operand.MemLoc _
                     => Error.bug "x86Validate.Instruction.validate: Jcc, target:MemLoc"
                     | Operand.Register _
                     => Error.bug "x86Validate.Instruction.validate: Jcc, target:Register"
                     | Operand.FltRegister _
                     => Error.bug "x86Validate.Instruction.validate: Jcc, target:FltRegister"
                     | Operand.Address _
                     => Error.bug "x86Validate.Instruction.validate: Jcc, target:Address"
                     | _ => (Operand.validate {operand = target})
                end
             | CALL {target, ...}
               (* Call procedure; p. 93 
                * Require target operand as follows:
                *
                *               target
                *            reg imm lab add
                *             X   X   X   X
                *)
             => let
                in
                  case target
                    of Operand.MemLoc _
                     => Error.bug "x86Validate.Instruction.validate: CALL, target:MemLoc"
                     | Operand.FltRegister _
                     => Error.bug "x86Validate.Instruction.validate: CALL, target:FltRegister"
                     | _ => (Operand.validate {operand = target})
                end
             | RET {src}
               (* Return from procedure; p. 648 
                * Require src operand as follows:
                *
                *                src
                *            reg imm lab add
                *                 X   
                *)
             => let
                in
                  case src
                    of SOME (Operand.MemLoc _)
                     => Error.bug "x86Validate.Instruction.validate: RET, src:MemLoc"
                     | SOME (Operand.Register _)
                     => Error.bug "x86Validate.Instruction.validate: RET, src:Register"
                     | SOME (Operand.FltRegister _)
                     => Error.bug "x86Validate.Instruction.validate: RET, src:FltRegister"
                     | SOME (Operand.Label _)
                     => Error.bug "x86Validate.Instruction.validate: RET, src:Label"
                     | SOME (Operand.Address _)
                     => Error.bug "x86Validate.Instruction.validate: RET, src:Address"
                     | SOME operand => (Operand.validate {operand = operand})
                     | NONE => true
                end
             | MOV {dst,src,size} 
               (* Move; p. 442 
                * Require src/dst operands as follows:
                *
                *              dst
                *          reg imm lab add 
                *      reg  X           X
                *  src imm  X           X
                *      lab
                *      add  X
                *
                * Require size modifier class as follows: INT
                *)
             => let
                  val _ = if Size.class size = Size.INT
                            then ()
                            else Error.bug "x86Validate.Instruction.validate: MOV, size"
                  val _ = case Operand.size src
                            of NONE => ()
                             | SOME srcsize
                             => if srcsize = size
                                  then ()
                                  else Error.bug "x86Validate.Instruction.validate: MOV, srcsize"
                  val _ = case Operand.size dst
                            of NONE => ()
                             | SOME dstsize 
                             => if dstsize = size
                                  then ()
                                  else Error.bug "x86Validate.Instruction.validate: MOV, dstsize"
                in
                  case (src,dst)
                    of (Operand.MemLoc _, _)
                     => Error.bug "x86Validate.Instruction.validate: MOV, src:MemLoc"
                     | (_, Operand.MemLoc _)
                     => Error.bug "x86Validate.Instruction.validate: MOV, dst:MemLoc"
                     | (Operand.FltRegister _, _)
                     => Error.bug "x86Validate.Instruction.validate: MOV, src:FltRegister"
                     | (Operand.Label _, _)
                     => Error.bug "x86Validate.Instruction.validate: MOV, src:Label"
                     | (_, Operand.FltRegister _)
                     => Error.bug "x86Validate.Instruction.validate: MOV, dst:FltRegister"
                     | (_, Operand.Immediate _)
                     => Error.bug "x86Validate.Instruction.validate: MOV, dst:Immediate"
                     | (_, Operand.Label _)
                     => Error.bug "x86Validate.Instruction.validate: MOV, dst:Label"
                     | (Operand.Address _, Operand.Address _)
                     => Error.bug "x86Validate.Instruction.validate: MOV, src,dst:Address"
                     | _ => (Operand.validate {operand = src}) andalso
                            (Operand.validate {operand = dst})
                end
             | CMOVcc {src, dst, size, ...}
               (* Conditional move; p. 112
                * Require src/dst operands as follows:
                *
                *              dst
                *          reg imm lab add 
                *      reg  X
                *  src imm           
                *      lab
                *      add  X
                *
                * Require size modifier class as follows: INT(WORD, LONG)
                *)
             => let
                  val _ = case size
                            of Size.WORD => ()
                             | Size.LONG => ()
                             | _ => Error.bug "x86Validate.Instruction.validate: CMOVcc, size"
                  val _ = case Operand.size src
                            of NONE => ()
                             | SOME srcsize 
                             => if srcsize = size
                                  then ()
                                  else Error.bug "x86Validate.Instruction.validate: CMOVcc, srcsize"
                  val _ = case Operand.size dst
                            of NONE => ()
                             | SOME dstsize 
                             => if dstsize = size
                                  then ()
                                  else Error.bug "x86Validate.Instruction.validate: CMOVcc, dstsize"
                in
                  case (src,dst)
                    of (Operand.MemLoc _, _)
                     => Error.bug "x86Validate.Instruction.validate: CMOVcc, src:MemLoc"
                     | (_, Operand.MemLoc _)
                     => Error.bug "x86Validate.Instruction.validate: CMOVcc, dst:MemLoc"
                     | (Operand.FltRegister _, _)
                     => Error.bug "x86Validate.Instruction.validate: CMOVcc, src:FltRegister"
                     | (Operand.Immediate _, _)
                     => Error.bug "x86Validate.Instruction.validate: CMOVcc, src:Immediate"
                     | (Operand.Label _, _)
                     => Error.bug "x86Validate.Instruction.validate: CMOVcc, src:Label" 
                     | (_, Operand.FltRegister _)
                     => Error.bug "x86Validate.Instruction.validate: CMOVcc, dst:FltRegister"
                     | (_, Operand.Immediate _)
                     => Error.bug "x86Validate.Instruction.validate: CMOVcc, dst:Immediate"
                     | (_, Operand.Label _)
                     => Error.bug "x86Validate.Instruction.validate: CMOVcc, dst:Label"
                     | (_, Operand.Address _)
                     => Error.bug "x86Validate.Instruction.validate: CMOVcc, dst:Address"
                     | _ => (Operand.validate {operand = src}) andalso
                            (Operand.validate {operand = dst})
                end
             | XCHG {src, dst, size}
               (* Exchange register/memory with register; p. 754
                * Require src/dst operands as follows:
                *
                *              dst
                *          reg imm lab add 
                *      reg  X           X
                *  src imm           
                *      lab
                *      add  X
                *
                * Require size modifier class as follows: INT
                *)
             => let
                  val _ = if Size.class size = Size.INT
                            then ()
                            else Error.bug "x86Validate.Instruction.validate: XCHG, size"
                  val _ = case Operand.size src
                            of NONE => ()
                             | SOME srcsize 
                             => if srcsize = size
                                  then ()
                                  else Error.bug "x86Validate.Instruction.validate: XCHG, srcsize"
                  val _ = case Operand.size dst
                            of NONE => ()
                             | SOME dstsize 
                             => if dstsize = size
                                  then ()
                                  else Error.bug "x86Validate.Instruction.validate: XCHG, dstsize"
                in
                  case (src,dst)
                    of (Operand.MemLoc _, _)
                     => Error.bug "x86Validate.Instruction.validate: XCHG, src:MemLoc"
                     | (_, Operand.MemLoc _)
                     => Error.bug "x86Validate.Instruction.validate: XCHG, dst:MemLoc"
                     | (Operand.FltRegister _, _)
                     => Error.bug "x86Validate.Instruction.validate: XCHG, src:FltRegister"
                     | (Operand.Immediate _, _)
                     => Error.bug "x86Validate.Instruction.validate: XCHG, src:Immediate"
                     | (Operand.Label _, _)
                     => Error.bug "x86Validate.Instruction.validate: XCHG, src:Label"
                     | (_, Operand.FltRegister _)
                     => Error.bug "x86Validate.Instruction.validate: XCHG, dst:FltRegister"
                     | (_, Operand.Immediate _)
                     => Error.bug "x86Validate.Instruction.validate: XCHG, dst:Immediate"
                     | (_, Operand.Label _)
                     => Error.bug "x86Validate.Instruction.validate: XCHG, dst:Label"
                     | (Operand.Address _, Operand.Address _)
                     => Error.bug "x86Validate.Instruction.validate: XCHG, src,dst:Address"
                     | _ => (Operand.validate {operand = src}) andalso
                            (Operand.validate {operand = dst})
                end
             | PUSH {src, size}
               (* Push a value onto the stack; p. 621
                * Require src operand as follows:
                *
                *               dst
                *           reg imm lab add
                *            *   X       X
                *   * only word or long registers
                *
                * Require size modifier class as follows: INT(WORD, LONG)
                *)
             => let
                  val _ = case size
                            of Size.WORD => ()
                             | Size.LONG => ()
                             | _ => Error.bug "x86Validate.Instruction.validate: PUSH, size"
                  val _ = case Operand.size src
                            of NONE => ()
                             | SOME srcsize 
                             => if srcsize = size
                                  then ()
                                  else Error.bug "x86Validate.Instruction.validate: PUSH, srcsize"
                in
                  case src
                    of Operand.MemLoc _
                     => Error.bug "x86Validate.Instruction.validate: PUSH, src:MemLoc"
                     | Operand.FltRegister _
                     => Error.bug "x86Validate.Instruction.validate: PUSH, src:FltRegister"
                     | Operand.Label _
                     => Error.bug "x86Validate.Instruction.validate: PUSH, src:Label"
                     | _ => (Operand.validate {operand = src}) 
                end
             | POP {dst, size}
               (* Pop a value from the stack; p. 571
                * Require dst operand as follows:
                *
                *               dst
                *           reg imm lab add
                *            *           X
                *   * only word or long registers
                *
                * Require size modifier as follows:
                * 
                *                size
                *      VOID BYTE WORD LONG DBLE
                *                 X    X
                *)
             => let
                  val _ = case size
                            of Size.WORD => ()
                             | Size.LONG => ()
                             | _ => Error.bug "x86Validate.Instruction.validate: POP, size"
                  val _ = case Operand.size dst
                            of NONE => ()
                             | SOME dstsize 
                             => if dstsize = size
                                  then ()
                                  else Error.bug "x86Validate.Instruction.validate: POP, dstsize"
                in
                  case dst
                    of Operand.MemLoc _
                     => Error.bug "x86Validate.Instruction.validate: POP, dst:MemLoc"
                     | Operand.FltRegister _
                     => Error.bug "x86Validate.Instruction.validate: POP, src:FltRegister"
                     | Operand.Immediate _
                     => Error.bug "x86Validate.Instruction.validate: POP, dst:Immediate"
                     | Operand.Label _
                     => Error.bug "x86Validate.Instruction.validate: POP, dst:Label"
                     | _ => (Operand.validate {operand = dst}) 
                end
             | CX {size}
               (* Convert X to 2X with sign extension; p. 104,181
                * Require size modifier class as follows: INT
                *)
             => let
                  val _ = if Size.class size = Size.INT
                            then ()
                            else Error.bug "x86Validate.Instruction.validate: CX, srcsize"
                in
                  true
                end
             | MOVX {src, dst, srcsize, dstsize, ...}
               (* Move with extention.
                * Require src/dst operands as follows:
                *
                *              dst
                *          reg imm lab add 
                *      reg  X
                *  src imm
                *      lab
                *      add  X
                *
                * Require srcsize/dstsize modifier class as follows: INT < INT
                *)
             => let
                  val _ = if Size.class srcsize = Size.INT
                            then ()
                            else Error.bug "x86Validate.Instruction.validate: MOVX, srcsize"
                  val _ = if Size.class dstsize = Size.INT
                            then ()
                            else Error.bug "x86Validate.Instruction.validate: MOVX, dstsize"
                  val _ = case Operand.size src
                            of NONE => ()
                             | SOME srcsize' 
                             => if srcsize' = srcsize
                                  then ()
                                  else Error.bug "x86Validate.Instruction.validate: MOVX, srcsize"
                  val _ = case Operand.size dst
                            of NONE => ()
                             | SOME dstsize' 
                             => if dstsize' = dstsize
                                  then ()
                                  else Error.bug "x86Validate.Instruction.validate: MOVX, dstsize"
                  val _ = if Size.lt(srcsize,dstsize)
                            then ()
                            else Error.bug 
                                 "x86Validate.Instruction.validate: MOVX, srcsize >= dstsize"
                in
                  case (src,dst)
                    of (Operand.MemLoc _, _)
                     => Error.bug "x86Validate.Instruction.validate: MOVX, src:MemLoc"
                     | (_, Operand.MemLoc _)
                     => Error.bug "x86Validate.Instruction.validate: MOVX, dst:MemLoc"
                     | (Operand.FltRegister _, _)
                     => Error.bug "x86Validate.Instruction.validate: MOVX, src:FltRegister"
                     | (Operand.Immediate _, _)
                     => Error.bug "x86Validate.Instruction.validate: MOVX, src:Immediate"
                     | (Operand.Label _, _)
                     => Error.bug "x86Validate.Instruction.validate: MOVX, src:Label"
                     | (_, Operand.FltRegister _)
                     => Error.bug "x86Validate.Instruction.validate: MOVX, dst:FltRegister"
                     | (_, Operand.Immediate _)
                     => Error.bug "x86Validate.Instruction.validate: MOVX, dst:Immediate"
                     | (_, Operand.Label _)
                     => Error.bug "x86Validate.Instruction.validate: MOVX, dst:Label"
                     | (_, Operand.Address _)
                     => Error.bug "x86Validate.Instruction.validate: MOVX, dst:Address"
                     | _ => (Operand.validate {operand = src}) andalso
                            (Operand.validate {operand = dst})
                end
             | LEA {src, dst, size}
               (* Load effective address; p. 393
                * Require src/dst operands as follows:
                *
                *              dst
                *          reg imm lab add 
                *      reg
                *  src imm
                *      lab
                *      add  X
                *
                * Require size modifier class as follows: INT(WORD, LONG)
                *)
             => let
                  val _ = case size
                            of Size.WORD => ()
                             | Size.LONG => ()
                             | _ => Error.bug "x86Validate.Instruction.validate: LEA, size"
                in
                  case (src,dst)
                    of (Operand.MemLoc _, _)
                     => Error.bug "x86Validate.Instruction.validate: LEA, src:MemLoc"
                     | (_, Operand.MemLoc _)
                     => Error.bug "x86Validate.Instruction.validate: LEA, dst:MemLoc"
                     | (Operand.Register _, _)
                     => Error.bug "x86Validate.Instruction.validate: LEA, src:Register"
                     | (Operand.FltRegister _, _)
                     => Error.bug "x86Validate.Instruction.validate: LEA, src:FltRegister"
                     | (Operand.Immediate _, _)
                     => Error.bug "x86Validate.Instruction.validate: LEA, src:Immediate"
                     | (Operand.Label _, _)
                     => Error.bug "x86Validate.Instruction.validate: LEA, src:Label"
                     | (_, Operand.FltRegister _)
                     => Error.bug "x86Validate.Instruction.validate: LEA, dst:FltRegister"
                     | (_, Operand.Immediate _)
                     => Error.bug "x86Validate.Instruction.validate: LEA, dst:Immediate"
                     | (_, Operand.Label _)
                     => Error.bug "x86Validate.Instruction.validate: LEA, dst:Label"
                     | (_, Operand.Address _)
                     => Error.bug "x86Validate.Instruction.validate: LEA, dst:Address"
                     | _ => (Operand.validate {operand = src}) andalso
                            (Operand.validate {operand = dst})
                end
             | FLD {src, size}
               (* Floating-point load real; p. 248
                * Require src operand as follows:
                *
                *              src
                *          fltreg add 
                *            X     X
                *
                * Require size modifier class as follows: FLT
                *)              
             => let
                  val _ = if Size.class size = Size.FLT
                            then ()
                            else Error.bug "x86Validate.Instruction.validate: FLD, size"
                in
                  case src
                    of Operand.MemLoc _
                     => Error.bug "x86Validate.Instruction.validate: FLD, src:MemLoc"
                     | Operand.Register _
                     => Error.bug "x86Validate.Instruction.validate: FLD, src:Register"
                     | Operand.Immediate _
                     => Error.bug "x86Validate.Instruction.validate: FLD, src:Immediate"
                     | Operand.Label _
                     => Error.bug "x86Validate.Instruction.validate: FLD, src:Label"
                     | _ => Operand.validate {operand = src}
                end
             | FST {dst, size, pop}
               (* Floating-point store real; p. 286
                * Require src operand as follows:
                *
                *              src
                *          fltreg add 
                *            X     X
                *
                * Require size modifier class as follows: FLT*
                *   * FLT(SNGL,DBLE)       if not pop
                *   * FLT(SNGL,DBLE,EXTD)  if pop
                *)
             => let
                  val _ = if Size.class size = Size.FLT
                            then (if not pop
                                    then case size
                                           of Size.SNGL => ()
                                            | Size.DBLE => ()
                                            | _
                                            => Error.bug "x86Validate.Instruction.validate: FST, size"
                                    else ())
                            else Error.bug "x86Validate.Instruction.validate: FST, size"
                in
                  case dst
                    of Operand.MemLoc _
                     => Error.bug "x86Validate.Instruction.validate: FST, dst:MemLoc"
                     | Operand.Register _
                     => Error.bug "x86Validate.Instruction.validate: FST, dst:Register"
                     | Operand.Immediate _
                     => Error.bug "x86Validate.Instruction.validate: FST, dst:Immediate"
                     | Operand.Label _
                     => Error.bug "x86Validate.Instruction.validate: FST, dst:Label"
                     | _ => Operand.validate {operand = dst}
                end
             | FILD {src, size}
               (* Floating-point load integer; p. 240
                * Require src operand as follows:
                *
                *              src
                *          fltreg add 
                *                  X
                *
                * Require size modifier class as follows: FPI
                *)              
             => let
                  val _ = if Size.class size = Size.FPI
                            then ()
                            else Error.bug "x86Validate.Instruction.validate: FILD, size"
                in
                  case src
                    of Operand.MemLoc _
                     => Error.bug "x86Validate.Instruction.validate: FILD, src:MemLoc"
                     | Operand.Register _
                     => Error.bug "x86Validate.Instruction.validate: FILD, src:Register"
                     | Operand.FltRegister _
                     => Error.bug "x86Validate.Instruction.validate: FILD, src:FltRegister"
                     | Operand.Immediate _
                     => Error.bug "x86Validate.Instruction.validate: FILD, src:Immediate"
                     | Operand.Label _
                     => Error.bug "x86Validate.Instruction.validate: FILD, src:Label"
                     | _ => Operand.validate {operand = src}
                end
             | FIST {dst, size, ...}
               (* Floating-point store integer; p. 245
                * Require dst operand as follows:
                *
                *              dst
                *          fltreg add 
                *                  X
                *
                * Require size modifier class as follows: FPI
                *)              
             => let
                  val _ = if Size.class size = Size.FPI
                            then ()
                            else Error.bug "x86Validate.Instruction.validate: FIST, size"
                in
                  case dst
                    of Operand.MemLoc _
                     => Error.bug "x86Validate.Instruction.validate: FIST, src:MemLoc"
                     | Operand.Register _
                     => Error.bug "x86Validate.Instruction.validate: FIST, src:Register"
                     | Operand.FltRegister _
                     => Error.bug "x86Validate.Instruction.validate: FIST, src:FltRegister"
                     | Operand.Immediate _
                     => Error.bug "x86Validate.Instruction.validate: FIST, src:Immediate"
                     | Operand.Label _
                     => Error.bug "x86Validate.Instruction.validate: FIST, src:Label"
                     | _ => Operand.validate {operand = dst}
                end
             | FXCH {src}
               (* Floating-point exchange; p. 313
                * Require src operand as follows:
                *
                *              src
                *          fltreg add 
                *            X
                *)
             => let
                in
                  case src
                    of Operand.MemLoc _
                     => Error.bug "x86Validate.Instruction.validate: FXCH, dst:MemLoc"
                     | Operand.Register _
                     => Error.bug "x86Validate.Instruction.validate: FXCH, dst:Register"
                     | Operand.Immediate _
                     => Error.bug "x86Validate.Instruction.validate: FXCH, dst:Immediate"
                     | Operand.Label _
                     => Error.bug "x86Validate.Instruction.validate: FXCH, dst:Label"
                     | _ => Operand.validate {operand = src}
                end
             | FLDC {...}
               (* Floating-point load constant; p. 250
                *)
             => true
             | FLDCW {src}
               (* Floating-point load control word; p. 252
                * Require src operand as follows:
                *
                *              dst
                *          reg imm lab add 
                *                       X
                *)
             => let                
                in
                  case src
                    of Operand.MemLoc _
                     => Error.bug "x86Validate.Instruction.validate: FLDCW, src:MemLoc"
                     | Operand.Register _ 
                     => Error.bug "x86Validate.Instruction.validate: FLDCW, src:Register"
                     | Operand.FltRegister _ 
                     => Error.bug "x86Validate.Instruction.validate: FLDCW, src:Register"
                     | Operand.Immediate _
                     => Error.bug "x86Validate.Instruction.validate: FLDCW, src:Immediate"
                     | Operand.Label _
                     => Error.bug "x86Validate.Instruction.validate: FLDCW, src:Label"
                     | _ => Operand.validate {operand = src}
                end
             | FSTCW {dst, ...}
               (* Floating-point store control word; p. 289
                * Require dst operand as follows:
                *
                *              dst
                *          reg imm lab add 
                *                       X
                *)
             => let                
                in
                  case dst
                    of Operand.MemLoc _
                     => Error.bug "x86Validate.Instruction.validate: FSTCW, dst:MemLoc"
                     | Operand.Register _ 
                     => Error.bug "x86Validate.Instruction.validate: FSTCW, dst:Register"
                     | Operand.FltRegister _ 
                     => Error.bug "x86Validate.Instruction.validate: FSTCW, dst:FltRegister"
                     | Operand.Immediate _
                     => Error.bug "x86Validate.Instruction.validate: FSTCW, dst:Immediate"
                     | Operand.Label _
                     => Error.bug "x86Validate.Instruction.validate: FSTCW, dst:Label"
                     | _ => Operand.validate {operand = dst}
                end
             | FSTSW {dst, ...}
               (* Floating-point store status word; p. 294
                * Require dst operand as follows:
                *
                *              dst
                *          reg imm lab add 
                *           *           X
                *   * only register %ax
                *)
             => let                
                in
                  case dst
                    of Operand.MemLoc _
                     => Error.bug "x86Validate.Instruction.validate: FSTSW, dst:MemLoc"
                     | Operand.Register (Register.T {reg = Register.EAX,
                                                     part = Register.X})
                     => Operand.validate {operand = dst}
                     | Operand.Register _ 
                     => Error.bug "x86Validate.Instruction.validate: FSTSW, dst:Register"
                     | Operand.FltRegister _ 
                     => Error.bug "x86Validate.Instruction.validate: FSTSW, dst:FltRegister"
                     | Operand.Immediate _
                     => Error.bug "x86Validate.Instruction.validate: FSTSW, dst:Immediate"
                     | Operand.Label _
                     => Error.bug "x86Validate.Instruction.validate: FSTSW, dst:Label"
                     | _ => Operand.validate {operand = dst}
                end
             | FCOM {src, size, pop, pop'}
               (* Floating-point compare real; p. 220
                * Require src operand as follows:
                *
                *               src
                *           fltreg add 
                *             *     X
                *   * only st(1) if pop and pop'
                *
                * Require size modifier class as follows: FLT(SNGL,DBLE)
                *)
             => let
                  val _ = if Size.class size = Size.FLT
                            then case src
                                   of Operand.Address _ 
                                    => (case size
                                          of Size.SNGL => ()
                                           | Size.DBLE => ()
                                           | _
                                           => Error.bug 
                                              "x86Validate.Instruction.validate: FCOM, size")
                                    | _ => ()
                            else Error.bug "x86Validate.Instruction.validate: FCOM, size"
                in
                  case src
                    of Operand.MemLoc _
                     => Error.bug "x86Validate.Instruction.validate: FCOM, src:MemLoc"
                     | Operand.Register _
                     => Error.bug "x86Validate.Instruction.validate: FCOM, src:Register"
                     | Operand.Immediate _
                     => Error.bug "x86Validate.Instruction.validate: FCOM, src:Immediate"
                     | Operand.Label _
                     => Error.bug "x86Validate.Instruction.validate: FCOM, src:Label"
                     | _ 
                     => if pop andalso pop'
                           andalso
                           not 
                           (Operand.eq(src,
                                       Operand.fltregister FltRegister.one))
                          then Error.bug "x86Validate.Instruction.validate: FCOM, pop, pop'"
                          else Operand.validate {operand = src}
                end
             | FUCOM {src, pop, pop'}
               (* Floating-point compare real; p. 307
                * Require src operand as follows:
                *
                *               src
                *           fltreg add 
                *             *
                *   * only st(1) if pop and pop'
                *)
             => let
                in
                  case src
                    of Operand.MemLoc _
                     => Error.bug "x86Validate.Instruction.validate: FUCOM, src:MemLoc"
                     | Operand.Register _
                     => Error.bug "x86Validate.Instruction.validate: FUCOM, src:Register"
                     | Operand.Immediate _
                     => Error.bug "x86Validate.Instruction.validate: FUCOM, src:Immediate"
                     | Operand.Label _
                     => Error.bug "x86Validate.Instruction.validate: FUCOM, src:Label"
                     | Operand.Address _
                     => Error.bug "x86Validate.Instruction.validate: FUCOM, src:Address"
                     | _ 
                     => if pop andalso pop'
                           andalso
                           not 
                           (Operand.eq(src,
                                       Operand.fltregister FltRegister.one))
                          then Error.bug "x86Validate.Instruction.validate: FUCOM, pop, pop'"
                          else Operand.validate {operand = src}
                end
             | FBinA {src, dst, size, pop, ...}
               (* Floating-point unary arithmetic instructions; p. 248
                * Require src operand as follows:
                *
                *               src
                *           fltreg add 
                *             *     X
                *   * only st(0) if pop
                *
                * Require dst operand as follows:
                *
                *               src
                *           fltreg add 
                *            *
                *   * only st(0) if src add
                *
                * Require size modifier class as follows: FLT*
                *   * FLT(SNGL,DBLE)       if src add
                *   * FLT(SNGL,DBLE,EXTD)
                *)              
             => let
                  val _ = if Size.class size = Size.FLT
                            then case src
                                   of Operand.Address _ 
                                    => (case size
                                          of Size.SNGL => ()
                                           | Size.DBLE => ()
                                           | _
                                           => Error.bug 
                                              "x86Validate.Instruction.validate: FBinA, size")
                                    | _ => ()
                            else Error.bug "x86Validate.Instruction.validate: FBinA, size"
                in
                  case (src,dst)
                    of (Operand.MemLoc _, _)
                     => Error.bug "x86Validate.Instruction.validate: FBinA, src:MemLoc"
                     | (_, Operand.MemLoc _)
                     => Error.bug "x86Validate.Instruction.validate: FBinA, dst:MemLoc"
                     | (Operand.Register _, _)
                     => Error.bug "x86Validate.Instruction.validate: FBinA, src:Register"
                     | (Operand.Immediate _, _)
                     => Error.bug "x86Validate.Instruction.validate: FBinA, src:Immediate"
                     | (Operand.Label _, _)
                     => Error.bug "x86Validate.Instruction.validate: FBinA, src:Label"
                     | (_, Operand.Register _)
                     => Error.bug "x86Validate.Instruction.validate: FBinA, dst:Register"
                     | (_, Operand.Immediate _)
                     => Error.bug "x86Validate.Instruction.validate: FBinA, dst:Immediate"
                     | (_, Operand.Label _)
                     => Error.bug "x86Validate.Instruction.validate: FBinA, dst:Label"
                     | (_, Operand.Address _)
                     => Error.bug "x86Validate.Instruction.validate: FBinA, dst:Address"
                     | (Operand.Address _, _)
                     => if Operand.eq(dst, 
                                      Operand.fltregister FltRegister.top)
                          then (Operand.validate {operand = src}) andalso
                               (Operand.validate {operand = dst})
                          else Error.bug "x86Validate.Instruction.validate: FBinA, src:Address"
                     | _
                     => if pop 
                           andalso
                           not 
                           (Operand.eq(src,
                                       Operand.fltregister FltRegister.top))
                          then Error.bug "x86Validate.Instruction.validate: FBinA, pop"
                          else (Operand.validate {operand = src}) andalso
                               (Operand.validate {operand = dst})
                end
             | FUnA {...}
               (* Floating-point unary arithmetic instructions.
                *)
             => true
             | FPTAN
               (* Floating-point partial tangent instruction.
                *)
             => true
             | FBinAS {...}
               (* Floating-point binary arithmetic stack instructions.
                *)
             => true
             | FBinASP {...}
               (* Floating-point binary arithmetic stack pop instructions.
                *)
             => true
             | _ => Error.bug (concat ["x86Validate.Instruction.validate: instruction :: ",
                                       toString instruction])
    end

  structure Assembly =
    struct
      open x86.Assembly

      fun validate {assembly: t list} : bool
        = List.fold(assembly,
                    true,
                    fn (Comment _, b)
                     => b
                     | (Directive _, _)
                     => Error.bug "x86Validate.Assembly.validate: Directive"
                     | (PseudoOp _, b)
                     => b
                     | (Label _, b)
                     => b
                     | (Instruction i, b)
                     => (Instruction.validate {instruction = i}) andalso b
                        handle Fail msg
                         => (print (toString (Instruction i));
                             print "\n";
                             Error.bug msg))
    end

  fun validate {assembly: Assembly.t list list} : bool
    = (if List.forall(assembly,
                      fn assembly
                       => Assembly.validate {assembly = assembly}
                          handle Fail _
                           => (List.foreach
                               (assembly,
                                fn assembly
                                 => (print (Assembly.toString assembly);
                                     print "\n"));
                               false))
         then true
         else Error.bug "x86Validate.validate")       

  val (validate, validate_msg)
    = tracerTop
      "validate"
      validate

  fun validate_totals ()
    = (validate_msg ();
       Control.indent ();
       Control.unindent ())
end
