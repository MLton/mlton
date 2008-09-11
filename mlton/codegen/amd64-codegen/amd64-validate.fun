(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor amd64Validate(S: AMD64_VALIDATE_STRUCTS): AMD64_VALIDATE =
struct

  open S;
  open amd64;

  val tracerTop = amd64.tracerTop

  structure Register =
    struct
      open Register

      fun validate {register}
        = if not (List.contains(registers (size register),
                                register,
                                eq))
            then Error.bug "amd64Validate.Register.validate"
            else true

      fun validate_base {register}
        = if not (eq (register, rip))
             andalso
             not (validate {register = register}
                  andalso
                  List.contains(baseRegisters,
                                register,
                                eq))
            then Error.bug "amd64Validate.Register.validate_base"
            else true

      fun validate_index {register}
        = if not (validate {register = register}
                  andalso
                  List.contains(indexRegisters,
                                register,
                                eq))
            then Error.bug "amd64Validate.Register.validate_index"
            else true
    end

  structure XmmRegister =
    struct
      open XmmRegister

      fun validate {register}
        = if not (List.contains(registers (size register),
                                register,
                                eq))
            then Error.bug "amd64Validate.XmmRegister.validate"
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
                                     else Error.bug "amd64Validate.Address.validate: base"

            val _ = case index
                      of NONE => ()
                       | SOME r => if Register.validate_index {register = r}
                                     then ()
                                     else Error.bug "amd64Validate.Address.validate: index"
          in
            case address
              of Address.T {disp = NONE, base = NONE, 
                            index = NONE, scale = NONE}
               => Error.bug "amd64Validate.Address.validate"
               | Address.T {index = NONE, scale = SOME _, ...}
               => Error.bug "amd64Validate.Address.validate: scale"
               | _ => true
          end
    end

  structure Operand =
    struct
      open Operand

      fun validate {operand: t}
        = case operand
            of Register r => Register.validate {register = r}
             | XmmRegister x => XmmRegister.validate {register = x}
             | Immediate _ => true
             | Label _ => true
             | Address a => Address.validate {address = a}
             | MemLoc _ => Error.bug "amd64Validate.Operand.validate: MemLoc"
    end

  structure Instruction =
    struct
      open amd64.Instruction

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
                            else Error.bug "amd64Validate.Instruction.validate: BinAL, size"
                  val _ = case Operand.size src
                            of NONE => ()
                             | SOME srcsize 
                             => if srcsize = size
                                  then ()
                                  else Error.bug "amd64Validate.Instruction.validate: BinAL, srcsize"
                  val _ = case Operand.size dst
                            of NONE => ()
                             | SOME dstsize 
                             => if dstsize = size
                                  then ()
                                  else Error.bug "amd64Validate.Instruction.validate: BinAL, dstsize"
                in
                  case (src,dst)
                    of (Operand.MemLoc _, _)
                     => Error.bug "amd64Validate.Instruction.validate: BinAL, src:MemLoc"
                     | (_, Operand.MemLoc _)
                     => Error.bug "amd64Validate.Instruction.validate: BinAL, dst:MemLoc"
                     | (Operand.XmmRegister _, _)
                     => Error.bug "amd64Validate.Instruction.validate: BinAL, src:XmmRegister"
                     | (Operand.Label _, _)
                     => Error.bug "amd64Validate.Instruction.validate: BinAL, src:Label"
                     | (_, Operand.XmmRegister _)
                     => Error.bug "amd64Validate.Instruction.validate: BinAL, dst:XmmRegister"
                     | (_, Operand.Immediate _)
                     => Error.bug "amd64Validate.Instruction.validate: BinAL, dst:Immediate"
                     | (_, Operand.Label _)
                     => Error.bug "amd64Validate.Instruction.validate: BinAL, dst:Label"
                     | (Operand.Address _, Operand.Address _)
                     => Error.bug "amd64Validate.Instruction.validate: BinAL, src,dst:Address"
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
                            else Error.bug "amd64Validate.Instruction.validate: BinMD, size"
                  val _ = case Operand.size src
                            of NONE => ()
                             | SOME srcsize 
                             => if srcsize = size
                                  then ()
                                  else Error.bug "amd64Validate.Instruction.validate: MD, srcsize"
                in
                  case src
                    of Operand.MemLoc _
                     => Error.bug "amd64Validate.Instruction.validate: MD, src:MemLoc"
                     | Operand.XmmRegister _
                     => Error.bug "amd64Validate.Instruction.validate: MD, src:XmmRegister"
                     | Operand.Immediate _
                     => Error.bug "amd64Validate.Instruction.validate: MD, src:Immediate"
                     | Operand.Label _
                     => Error.bug "amd64Validate.Instruction.validate: MD, src:Label"
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
                * Require size modifier class as follows: INT(WORD, LONG, QUAD)
                *)
             => let
                  val _ = case size
                            of Size.WORD => ()
                             | Size.LONG => ()
                             | Size.QUAD => ()
                             | _ => Error.bug "amd64Validate.Instruction.validate: IMUL2, size"
                  val _ = case Operand.size src
                            of NONE => ()
                             | SOME srcsize 
                             => if srcsize = size
                                  then ()
                                  else Error.bug "amd64Validate.Instruction.validate: IMUL2, srcsize"
                  val _ = case Operand.size dst
                            of NONE => ()
                             | SOME dstsize 
                             => if dstsize = size
                                  then ()
                                  else Error.bug "amd64Validate.Instruction.validate: IMUL2, dstsize"
                in
                  case (src,dst)
                    of (Operand.MemLoc _, _)
                     => Error.bug "amd64Validate.Instruction.validate: IMUL2, src:MemLoc"
                     | (_, Operand.MemLoc _)
                     => Error.bug "amd64Validate.Instruction.validate: IMUL2, dst:MemLoc"
                     | (Operand.XmmRegister _, _)
                     => Error.bug "amd64Validate.Instruction.validate: IMUL2, src:XmmRegister"
                     | (Operand.Label _, _)
                     => Error.bug "amd64Validate.Instruction.validate: IMUL2, src:Label"
                     | (_, Operand.XmmRegister _)
                     => Error.bug "amd64Validate.Instruction.validate: IMUL2, dst:XmmRegister"
                     | (_, Operand.Immediate _)
                     => Error.bug "amd64Validate.Instruction.validate: IMUL2, dst:Immediate"
                     | (_, Operand.Label _)
                     => Error.bug "amd64Validate.Instruction.validate: IMUL2, dst:Label"
                     | (Operand.Address _, _)
                     => Error.bug "amd64Validate.Instruction.validate: IMUL2, src:Address"
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
                            else Error.bug "amd64Validate.Instruction.validate: UnAL, size"
                  val _ = case Operand.size dst
                            of NONE => ()
                             | SOME dstsize 
                             => if dstsize = size
                                  then ()
                                  else Error.bug "amd64Validate.Instruction.validate: UnAL, dstsize"
                in
                  case dst
                    of Operand.MemLoc _
                     => Error.bug "amd64Validate.Instruction.validate: UnAL, dst:MemLoc"
                     | Operand.XmmRegister _
                     => Error.bug "amd64Validate.Instruction.validate: UnAL, dst:XmmRegister"
                     | Operand.Immediate _
                     => Error.bug "amd64Validate.Instruction.validate: UnAL, dst:Immediate"
                     | Operand.Label _
                     => Error.bug "amd64Validate.Instruction.validate: UnAL, dst:Label"
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
                            else Error.bug "amd64Validate.Instruction.validate: SRAL, size"
                  val _ = case Operand.size dst
                            of NONE => ()
                             | SOME dstsize 
                             => if dstsize = size
                                  then ()
                                  else Error.bug "amd64Validate.Instruction.validate: SRAL, dstsize"

                  val _ = case count
                            of Operand.MemLoc _
                             => Error.bug "amd64Validate.Instruction.validate: SRAL, count:MemLoc"
                             | Operand.XmmRegister _
                             => Error.bug "amd64Validate.Instruction.validate: SRAL, count:XmmRegister"
                             | Operand.Label _
                             => Error.bug "amd64Validate.Instruction.validate: SRAL, count:Label"
                             | Operand.Address _
                             => Error.bug "amd64Validate.Instruction.validate: SRAL, count:Address"
                             | Operand.Register (Register.T {reg, part})
                             => if reg <> Register.RCX orelse
                                   part <> Register.L
                                  then Error.bug 
                                       "amd64Validate.Instruction.validate: SRAL, count:Register"
                                  else ()
                             | _ => ()
                in
                  case dst
                    of Operand.MemLoc _
                     => Error.bug "amd64Validate.Instruction.validate: SRAL, dst:MemLoc"
                     | Operand.XmmRegister _
                     => Error.bug "amd64Validate.Instruction.validate: SRAL, dst:XmmRegister"
                     | Operand.Immediate _
                     => Error.bug "amd64Validate.Instruction.validate: SRAL, dst:Immediate"
                     | Operand.Label _
                     => Error.bug "amd64Validate.Instruction.validate: SRAL, dst:Label"
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
                            else Error.bug "amd64Validate.Instruction.validate: CMP, size"
                  val _ = case Operand.size src1
                            of NONE => ()
                             | SOME src1size 
                             => if src1size = size
                                  then ()
                                  else Error.bug "amd64Validate.Instruction.validate: CMP, src1size"
                  val _ = case Operand.size src2
                            of NONE => ()
                             | SOME src2size 
                             => if src2size = size
                                  then ()
                                  else Error.bug "amd64Validate.Instruction.validate: CMP, src2size"
                in
                  case (src1,src2)
                    of (Operand.MemLoc _, _)
                     => Error.bug "amd64Validate.Instruction.validate: CMP, src1:MemLoc"
                     | (_, Operand.MemLoc _)
                     => Error.bug "amd64Validate.Instruction.validate: CMP, src2:MemLoc"
                     | (Operand.XmmRegister _, _)
                     => Error.bug "amd64Validate.Instruction.validate: CMP, src1: XmmRegister"
                     | (Operand.Immediate _, _)
                     => Error.bug "amd64Validate.Instruction.validate: CMP, src1:Immediate"
                     | (Operand.Label _, _)
                     => Error.bug "amd64Validate.Instruction.validate: CMP, src1:Label"
                     | (_, Operand.XmmRegister _)
                     => Error.bug "amd64Validate.Instruction.validate: CMP, src2: XmmRegister"
                     | (_, Operand.Label _)
                     => Error.bug "amd64Validate.Instruction.validate: CMP, src2:Label"
                     | (Operand.Address _, Operand.Address _)
                     => Error.bug "amd64Validate.Instruction.validate: CMP, src1,src2:Address"
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
                            else Error.bug "amd64Validate.Instruction.validate: TEST, size"
                  val _ = case Operand.size src1
                            of NONE => ()
                             | SOME src1size 
                             => if src1size = size
                                  then ()
                                  else Error.bug "amd64Validate.Instruction.validate: TEST, src1size"
                  val _ = case Operand.size src2
                            of NONE => ()
                             | SOME src2size 
                             => if src2size = size
                                  then ()
                                  else Error.bug "amd64Validate.Instruction.validate: TEST, src2size"
                in
                  case (src1,src2)
                    of (Operand.MemLoc _, _)
                     => Error.bug "amd64Validate.Instruction.validate: TEST, src1:MemLoc"
                     | (_, Operand.MemLoc _)
                     => Error.bug "amd64Validate.Instruction.validate: TEST, src2:MemLoc"
                     | (Operand.XmmRegister _, _)
                     => Error.bug "amd64Validate.Instruction.validate: TEST, src1: XmmRegister"
                     | (Operand.Immediate _, _)
                     => Error.bug "amd64Validate.Instruction.validate: TEST, src1:Immediate"      
                     | (Operand.Label _, _)
                     => Error.bug "amd64Validate.Instruction.validate: TEST, src1:Label"
                     | (_, Operand.XmmRegister _)
                     => Error.bug "amd64Validate.Instruction.validate: TEST, src2: XmmRegister"
                     | (_, Operand.Label _)
                     => Error.bug "amd64Validate.Instruction.validate: TEST, src2:Label"
                     | (Operand.Address _, Operand.Address _)
                     => Error.bug "amd64Validate.Instruction.validate: TEST, src1,src2:Address"
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
                             | _ => Error.bug "amd64Validate.Instruction.validate: SETcc, size"
                  val _ = case Operand.size dst
                            of NONE => ()
                             | SOME dstsize 
                             => if dstsize = size
                                  then ()
                                  else Error.bug "amd64Validate.Instruction.validate: SETcc, dstsize"
                in
                  case dst
                    of Operand.MemLoc _
                     => Error.bug "amd64Validate.Instruction.validate: SETcc, dst:MemLoc"
                     | Operand.XmmRegister _
                     => Error.bug "amd64Validate.Instruction.validate: SETcc, dst:XmmRegister"
                     | Operand.Immediate _
                     => Error.bug "amd64Validate.Instruction.validate: SETcc, dst:Immediate"
                     | Operand.Label _
                     => Error.bug "amd64Validate.Instruction.validate: SETcc, dst:Label"
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
                     => Error.bug "amd64Validate.Instruction.validate: JMP, target:MemLoc"
                     | Operand.XmmRegister _
                     => Error.bug "amd64Validate.Instruction.validate: JMP, target:XmmRegister"
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
                     => Error.bug "amd64Validate.Instruction.validate: Jcc, target:MemLoc"
                     | Operand.Register _
                     => Error.bug "amd64Validate.Instruction.validate: Jcc, target:Register"
                     | Operand.XmmRegister _
                     => Error.bug "amd64Validate.Instruction.validate: Jcc, target:XmmRegister"
                     | Operand.Address _
                     => Error.bug "amd64Validate.Instruction.validate: Jcc, target:Address"
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
                     => Error.bug "amd64Validate.Instruction.validate: CALL, target:MemLoc"
                     | Operand.XmmRegister _
                     => Error.bug "amd64Validate.Instruction.validate: CALL, target:XmmRegister"
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
                     => Error.bug "amd64Validate.Instruction.validate: RET, src:MemLoc"
                     | SOME (Operand.Register _)
                     => Error.bug "amd64Validate.Instruction.validate: RET, src:Register"
                     | SOME (Operand.XmmRegister _)
                     => Error.bug "amd64Validate.Instruction.validate: RET, src:XmmRegister"
                     | SOME (Operand.Label _)
                     => Error.bug "amd64Validate.Instruction.validate: RET, src:Label"
                     | SOME (Operand.Address _)
                     => Error.bug "amd64Validate.Instruction.validate: RET, src:Address"
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
                            else Error.bug "amd64Validate.Instruction.validate: MOV, size"
                  val _ = case Operand.size src
                            of NONE => ()
                             | SOME srcsize
                             => if srcsize = size
                                  then ()
                                  else Error.bug "amd64Validate.Instruction.validate: MOV, srcsize"
                  val _ = case Operand.size dst
                            of NONE => ()
                             | SOME dstsize 
                             => if dstsize = size
                                  then ()
                                  else Error.bug "amd64Validate.Instruction.validate: MOV, dstsize"
                in
                  case (src,dst)
                    of (Operand.MemLoc _, _)
                     => Error.bug "amd64Validate.Instruction.validate: MOV, src:MemLoc"
                     | (_, Operand.MemLoc _)
                     => Error.bug "amd64Validate.Instruction.validate: MOV, dst:MemLoc"
                     | (Operand.XmmRegister _, _)
                     => Error.bug "amd64Validate.Instruction.validate: MOV, src:XmmRegister"
                     | (Operand.Label _, _)
                     => Error.bug "amd64Validate.Instruction.validate: MOV, src:Label"
                     | (_, Operand.XmmRegister _)
                     => Error.bug "amd64Validate.Instruction.validate: MOV, dst:XmmRegister"
                     | (_, Operand.Immediate _)
                     => Error.bug "amd64Validate.Instruction.validate: MOV, dst:Immediate"
                     | (_, Operand.Label _)
                     => Error.bug "amd64Validate.Instruction.validate: MOV, dst:Label"
                     | (Operand.Address _, Operand.Address _)
                     => Error.bug "amd64Validate.Instruction.validate: MOV, src,dst:Address"
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
                * Require size modifier class as follows: INT(WORD, LONG, QUAD)
                *)
             => let
                  val _ = case size
                            of Size.WORD => ()
                             | Size.LONG => ()
                             | Size.QUAD => ()
                             | _ => Error.bug "amd64Validate.Instruction.validate: CMOVcc, size"
                  val _ = case Operand.size src
                            of NONE => ()
                             | SOME srcsize 
                             => if srcsize = size
                                  then ()
                                  else Error.bug "amd64Validate.Instruction.validate: CMOVcc, srcsize"
                  val _ = case Operand.size dst
                            of NONE => ()
                             | SOME dstsize 
                             => if dstsize = size
                                  then ()
                                  else Error.bug "amd64Validate.Instruction.validate: CMOVcc, dstsize"
                in
                  case (src,dst)
                    of (Operand.MemLoc _, _)
                     => Error.bug "amd64Validate.Instruction.validate: CMOVcc, src:MemLoc"
                     | (_, Operand.MemLoc _)
                     => Error.bug "amd64Validate.Instruction.validate: CMOVcc, dst:MemLoc"
                     | (Operand.XmmRegister _, _)
                     => Error.bug "amd64Validate.Instruction.validate: CMOVcc, src:XmmRegister"
                     | (Operand.Immediate _, _)
                     => Error.bug "amd64Validate.Instruction.validate: CMOVcc, src:Immediate"
                     | (Operand.Label _, _)
                     => Error.bug "amd64Validate.Instruction.validate: CMOVcc, src:Label" 
                     | (_, Operand.XmmRegister _)
                     => Error.bug "amd64Validate.Instruction.validate: CMOVcc, dst:XmmRegister"
                     | (_, Operand.Immediate _)
                     => Error.bug "amd64Validate.Instruction.validate: CMOVcc, dst:Immediate"
                     | (_, Operand.Label _)
                     => Error.bug "amd64Validate.Instruction.validate: CMOVcc, dst:Label"
                     | (_, Operand.Address _)
                     => Error.bug "amd64Validate.Instruction.validate: CMOVcc, dst:Address"
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
                            else Error.bug "amd64Validate.Instruction.validate: XCHG, size"
                  val _ = case Operand.size src
                            of NONE => ()
                             | SOME srcsize 
                             => if srcsize = size
                                  then ()
                                  else Error.bug "amd64Validate.Instruction.validate: XCHG, srcsize"
                  val _ = case Operand.size dst
                            of NONE => ()
                             | SOME dstsize 
                             => if dstsize = size
                                  then ()
                                  else Error.bug "amd64Validate.Instruction.validate: XCHG, dstsize"
                in
                  case (src,dst)
                    of (Operand.MemLoc _, _)
                     => Error.bug "amd64Validate.Instruction.validate: XCHG, src:MemLoc"
                     | (_, Operand.MemLoc _)
                     => Error.bug "amd64Validate.Instruction.validate: XCHG, dst:MemLoc"
                     | (Operand.XmmRegister _, _)
                     => Error.bug "amd64Validate.Instruction.validate: XCHG, src:XmmRegister"
                     | (Operand.Immediate _, _)
                     => Error.bug "amd64Validate.Instruction.validate: XCHG, src:Immediate"
                     | (Operand.Label _, _)
                     => Error.bug "amd64Validate.Instruction.validate: XCHG, src:Label"
                     | (_, Operand.XmmRegister _)
                     => Error.bug "amd64Validate.Instruction.validate: XCHG, dst:XmmRegister"
                     | (_, Operand.Immediate _)
                     => Error.bug "amd64Validate.Instruction.validate: XCHG, dst:Immediate"
                     | (_, Operand.Label _)
                     => Error.bug "amd64Validate.Instruction.validate: XCHG, dst:Label"
                     | (Operand.Address _, Operand.Address _)
                     => Error.bug "amd64Validate.Instruction.validate: XCHG, src,dst:Address"
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
                * Require size modifier class as follows: INT(WORD, LONG, QUAD)
                *)
             => let
                  val _ = case size
                            of Size.WORD => ()
                             | Size.LONG => ()
                             | Size.QUAD => ()
                             | _ => Error.bug "amd64Validate.Instruction.validate: PUSH, size"
                  val _ = case Operand.size src
                            of NONE => ()
                             | SOME srcsize 
                             => if srcsize = size
                                  then ()
                                  else Error.bug "amd64Validate.Instruction.validate: PUSH, srcsize"
                in
                  case src
                    of Operand.MemLoc _
                     => Error.bug "amd64Validate.Instruction.validate: PUSH, src:MemLoc"
                     | Operand.XmmRegister _
                     => Error.bug "amd64Validate.Instruction.validate: PUSH, src:XmmRegister"
                     | Operand.Label _
                     => Error.bug "amd64Validate.Instruction.validate: PUSH, src:Label"
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
                *      VOID BYTE WORD LONG QUAD DBLE
                *                 X    X    X
                *)
             => let
                  val _ = case size
                            of Size.WORD => ()
                             | Size.LONG => ()
                             | Size.QUAD => ()
                             | _ => Error.bug "amd64Validate.Instruction.validate: POP, size"
                  val _ = case Operand.size dst
                            of NONE => ()
                             | SOME dstsize 
                             => if dstsize = size
                                  then ()
                                  else Error.bug "amd64Validate.Instruction.validate: POP, dstsize"
                in
                  case dst
                    of Operand.MemLoc _
                     => Error.bug "amd64Validate.Instruction.validate: POP, dst:MemLoc"
                     | Operand.XmmRegister _
                     => Error.bug "amd64Validate.Instruction.validate: POP, src:XmmRegister"
                     | Operand.Immediate _
                     => Error.bug "amd64Validate.Instruction.validate: POP, dst:Immediate"
                     | Operand.Label _
                     => Error.bug "amd64Validate.Instruction.validate: POP, dst:Label"
                     | _ => (Operand.validate {operand = dst}) 
                end
             | CX {size}
               (* Convert X to 2X with sign extension; p. 104,181
                * Require size modifier class as follows: INT
                *)
             => let
                  val _ = if Size.class size = Size.INT
                            then ()
                            else Error.bug "amd64Validate.Instruction.validate: CX, srcsize"
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
                            else Error.bug "amd64Validate.Instruction.validate: MOVX, srcsize"
                  val _ = if Size.class dstsize = Size.INT
                            then ()
                            else Error.bug "amd64Validate.Instruction.validate: MOVX, dstsize"
                  val _ = case Operand.size src
                            of NONE => ()
                             | SOME srcsize' 
                             => if srcsize' = srcsize
                                  then ()
                                  else Error.bug "amd64Validate.Instruction.validate: MOVX, srcsize"
                  val _ = case Operand.size dst
                            of NONE => ()
                             | SOME dstsize' 
                             => if dstsize' = dstsize
                                  then ()
                                  else Error.bug "amd64Validate.Instruction.validate: MOVX, dstsize"
                  val _ = if Size.lt(srcsize,dstsize)
                            then ()
                            else Error.bug 
                                 "amd64Validate.Instruction.validate: MOVX, srcsize >= dstsize"
                in
                  case (src,dst)
                    of (Operand.MemLoc _, _)
                     => Error.bug "amd64Validate.Instruction.validate: MOVX, src:MemLoc"
                     | (_, Operand.MemLoc _)
                     => Error.bug "amd64Validate.Instruction.validate: MOVX, dst:MemLoc"
                     | (Operand.XmmRegister _, _)
                     => Error.bug "amd64Validate.Instruction.validate: MOVX, src:XmmRegister"
                     | (Operand.Immediate _, _)
                     => Error.bug "amd64Validate.Instruction.validate: MOVX, src:Immediate"
                     | (Operand.Label _, _)
                     => Error.bug "amd64Validate.Instruction.validate: MOVX, src:Label"
                     | (_, Operand.XmmRegister _)
                     => Error.bug "amd64Validate.Instruction.validate: MOVX, dst:XmmRegister"
                     | (_, Operand.Immediate _)
                     => Error.bug "amd64Validate.Instruction.validate: MOVX, dst:Immediate"
                     | (_, Operand.Label _)
                     => Error.bug "amd64Validate.Instruction.validate: MOVX, dst:Label"
                     | (_, Operand.Address _)
                     => Error.bug "amd64Validate.Instruction.validate: MOVX, dst:Address"
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
                * Require size modifier class as follows: INT(WORD, LONG, QUAD)
                *)
             => let
                  val _ = case size
                            of Size.WORD => ()
                             | Size.LONG => ()
                             | Size.QUAD => ()
                             | _ => Error.bug "amd64Validate.Instruction.validate: LEA, size"
                in
                  case (src,dst)
                    of (Operand.MemLoc _, _)
                     => Error.bug "amd64Validate.Instruction.validate: LEA, src:MemLoc"
                     | (_, Operand.MemLoc _)
                     => Error.bug "amd64Validate.Instruction.validate: LEA, dst:MemLoc"
                     | (Operand.Register _, _)
                     => Error.bug "amd64Validate.Instruction.validate: LEA, src:Register"
                     | (Operand.XmmRegister _, _)
                     => Error.bug "amd64Validate.Instruction.validate: LEA, src:XmmRegister"
                     | (Operand.Immediate _, _)
                     => Error.bug "amd64Validate.Instruction.validate: LEA, src:Immediate"
                     | (Operand.Label _, _)
                     => Error.bug "amd64Validate.Instruction.validate: LEA, src:Label"
                     | (_, Operand.XmmRegister _)
                     => Error.bug "amd64Validate.Instruction.validate: LEA, dst:XmmRegister"
                     | (_, Operand.Immediate _)
                     => Error.bug "amd64Validate.Instruction.validate: LEA, dst:Immediate"
                     | (_, Operand.Label _)
                     => Error.bug "amd64Validate.Instruction.validate: LEA, dst:Label"
                     | (_, Operand.Address _)
                     => Error.bug "amd64Validate.Instruction.validate: LEA, dst:Address"
                     | _ => (Operand.validate {operand = src}) andalso
                            (Operand.validate {operand = dst})
                end
             | SSE_BinAS {src, dst, size, ...}
               (* Scalar SSE binary arithmetic instructions.
                * Require src/dst operands as follows:
                *
                *              dst
                *          reg xmm imm lab add 
                *      reg
                *      xmm      X
                *  src imm
                *      lab
                *      add      X
                *
                * Require size modifier class as follows: FLT
                *)
             => let
                  val _ = if Size.class size = Size.FLT
                            then ()
                            else Error.bug "amd64Validate.Instruction.validate: SSE_BinAS, size"
                  val _ = case Operand.size src
                            of NONE => ()
                             | SOME srcsize 
                             => if srcsize = size
                                  then ()
                                  else Error.bug "amd64Validate.Instruction.validate: SSE_BinAS, srcsize"
                  val _ = case Operand.size dst
                            of NONE => ()
                             | SOME dstsize 
                             => if dstsize = size
                                  then ()
                                  else Error.bug "amd64Validate.Instruction.validate: SSE_BinAS, dstsize"
                in
                  case (src,dst)
                    of (Operand.MemLoc _, _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_BinAS, src:MemLoc"
                     | (_, Operand.MemLoc _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_BinAS, dst:MemLoc"
                     | (Operand.Register _, _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_BinAS, src:Register"
                     | (Operand.Immediate _, _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_BinAS, src:Immediate"
                     | (Operand.Label _, _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_BinAS, src:Label"
                     | (_, Operand.Register _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_BinAS, dst:Register"
                     | (_, Operand.Immediate _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_BinAS, dst:Immediate"
                     | (_, Operand.Label _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_BinAS, dst:Label"
                     | (_, Operand.Address _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_BinAS, dst:Address"
                     | _ => (Operand.validate {operand = src}) andalso
                            (Operand.validate {operand = dst})
                end
             | SSE_UnAS {src, dst, size, ...}
               (* SSE scalar unary arithmetic instructions.
                * Require src/dst operands as follows:
                *
                *              dst
                *          reg xmm imm lab add 
                *      reg
                *      xmm      X
                *  src imm
                *      lab
                *      add      X
                *
                * Require size modifier class as follows: FLT
                *)
             => let
                  val _ = if Size.class size = Size.FLT
                            then ()
                            else Error.bug "amd64Validate.Instruction.validate: SSE_UnAS, size"
                  val _ = case Operand.size src
                            of NONE => ()
                             | SOME srcsize 
                             => if srcsize = size
                                  then ()
                                  else Error.bug "amd64Validate.Instruction.validate: SSE_UnAS, srcsize"
                  val _ = case Operand.size dst
                            of NONE => ()
                             | SOME dstsize 
                             => if dstsize = size
                                  then ()
                                  else Error.bug "amd64Validate.Instruction.validate: SSE_UnAS, dstsize"
                in
                  case (src,dst)
                    of (Operand.MemLoc _, _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_UnAS, src:MemLoc"
                     | (_, Operand.MemLoc _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_UnAS, dst:MemLoc"
                     | (Operand.Register _, _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_UnAS, src:Register"
                     | (Operand.Immediate _, _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_UnAS, src:Immediate"
                     | (Operand.Label _, _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_UnAS, src:Label"
                     | (_, Operand.Register _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_UnAS, dst:Register"
                     | (_, Operand.Immediate _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_UnAS, dst:Immediate"
                     | (_, Operand.Label _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_UnAS, dst:Label"
                     | (_, Operand.Address _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_UnAS, dst:Address"
                     | _ => (Operand.validate {operand = src}) andalso
                            (Operand.validate {operand = dst})
                end
             | SSE_BinLP {src, dst, size, ...}
               (* Packed SSE binary logical instructions (used as scalar).
                * Require src/dst operands as follows:
                *
                *              dst
                *          reg xmm imm lab add 
                *      reg
                *      xmm      X
                *  src imm
                *      lab
                *      add     (x)
                *
                * Require size modifier class as follows: FLT
                * Disallow address for src, since it would be a 128-bit load.
                *)
             => let
                  val _ = if Size.class size = Size.FLT
                            then ()
                            else Error.bug "amd64Validate.Instruction.validate: SSE_BinLP, size"
                  val _ = case Operand.size src
                            of NONE => ()
                             | SOME srcsize 
                             => if srcsize = size
                                  then ()
                                  else Error.bug "amd64Validate.Instruction.validate: SSE_BinLP, srcsize"
                  val _ = case Operand.size dst
                            of NONE => ()
                             | SOME dstsize 
                             => if dstsize = size
                                  then ()
                                  else Error.bug "amd64Validate.Instruction.validate: SSE_BinLP, dstsize"
                in
                  case (src,dst)
                    of (Operand.MemLoc _, _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_BinLP, src:MemLoc"
                     | (_, Operand.MemLoc _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_BinLP, dst:MemLoc"
                     | (Operand.Register _, _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_BinLP, src:Register"
                     | (Operand.Immediate _, _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_BinLP, src:Immediate"
                     | (Operand.Label _, _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_BinLP, src:Label"
                     | (Operand.Address _, _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_BinLP, src:Address"
                     | (_, Operand.Register _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_BinLP, dst:Register"
                     | (_, Operand.Immediate _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_BinLP, dst:Immediate"
                     | (_, Operand.Label _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_BinLP, dst:Label"
                     | (_, Operand.Address _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_BinLP, dst:Address"
                     | _ => (Operand.validate {operand = src}) andalso
                            (Operand.validate {operand = dst})
                end
             | SSE_MOVS {src, dst, size, ...}
               (* Scalar SSE move instruction.
                * Require src/dst operands as follows:
                *
                *              dst
                *          reg xmm imm lab add 
                *      reg
                *      xmm      X           X
                *  src imm
                *      lab
                *      add      X
                *
                * Require size modifier class as follows: FLT
                *)
             => let
                  val _ = if Size.class size = Size.FLT
                            then ()
                            else Error.bug "amd64Validate.Instruction.validate: SSE_MOVS, size"
                  val _ = case Operand.size src
                            of NONE => ()
                             | SOME srcsize 
                             => if srcsize = size
                                  then ()
                                  else Error.bug "amd64Validate.Instruction.validate: SSE_MOVS, srcsize"
                  val _ = case Operand.size dst
                            of NONE => ()
                             | SOME dstsize 
                             => if dstsize = size
                                  then ()
                                  else Error.bug "amd64Validate.Instruction.validate: SSE_MOVS, dstsize"
                in
                  case (src,dst)
                    of (Operand.MemLoc _, _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_MOVS, src:MemLoc"
                     | (_, Operand.MemLoc _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_MOVS, dst:MemLoc"
                     | (Operand.Register _, _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_MOVS, src:Register"
                     | (Operand.Immediate _, _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_MOVS, src:Immediate"
                     | (Operand.Label _, _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_MOVS, src:Label"
                     | (_, Operand.Register _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_MOVS, dst:Register"
                     | (_, Operand.Immediate _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_MOVS, dst:Immediate"
                     | (_, Operand.Label _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_MOVS, dst:Label"
                     | (Operand.Address _, Operand.Address _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_MOVS, src,dst:Address"
                     | _ => (Operand.validate {operand = src}) andalso
                            (Operand.validate {operand = dst})
                end
             | SSE_COMIS {src1, src2, size}
               (* Scalar SSE compare instruction.
                * Require src1/src2 operands as follows:
                *
                *               src2
                *           reg xmm imm lab add 
                *       reg
                *       xmm      X
                *  src1 imm
                *       lab
                *       add      X
                *
                * Require size modifier class as follows: FLT
                *)
             => let
                  val _ = if Size.class size = Size.FLT
                            then ()
                            else Error.bug "amd64Validate.Instruction.validate: SSE_COMIS, size"
                  val _ = case Operand.size src1
                            of NONE => ()
                             | SOME src1size 
                             => if src1size = size
                                  then ()
                                  else Error.bug "amd64Validate.Instruction.validate: SSE_COMIS, src1size"
                  val _ = case Operand.size src2
                            of NONE => ()
                             | SOME src2size 
                             => if src2size = size
                                  then ()
                                  else Error.bug "amd64Validate.Instruction.validate: SSE_COMIS, src2size"
                in
                  case (src1,src2)
                    of (Operand.MemLoc _, _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_COMIS, src1:MemLoc"
                     | (_, Operand.MemLoc _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_COMIS, src2:MemLoc"
                     | (Operand.Register _, _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_COMIS, src1:Register"
                     | (Operand.Immediate _, _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_COMIS, src1:Immediate"
                     | (Operand.Label _, _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_COMIS, src1:Label"
                     | (_, Operand.Register _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_COMIS, src2:Register"
                     | (_, Operand.Immediate _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_COMIS, src2:Immediate"
                     | (_, Operand.Label _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_COMIS, src2:Label"
                     | (_, Operand.Address _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_COMIS, src2:Address"
                     | _ => (Operand.validate {operand = src1}) andalso
                            (Operand.validate {operand = src2})
                end
             | SSE_UCOMIS {src1, src2, size}
               (* Scalar SSE unordered compare instruction.
                * Require src1/src2 operands as follows:
                *
                *               src2
                *           reg xmm imm lab add 
                *       reg
                *       xmm      X
                *  src1 imm
                *       lab
                *       add      X
                *
                * Require size modifier class as follows: FLT
                *)
             => let
                  val _ = if Size.class size = Size.FLT
                            then ()
                            else Error.bug "amd64Validate.Instruction.validate: SSE_UCOMIS, size"
                  val _ = case Operand.size src1
                            of NONE => ()
                             | SOME src1size 
                             => if src1size = size
                                  then ()
                                  else Error.bug "amd64Validate.Instruction.validate: SSE_UCOMIS, src1size"
                  val _ = case Operand.size src2
                            of NONE => ()
                             | SOME src2size 
                             => if src2size = size
                                  then ()
                                  else Error.bug "amd64Validate.Instruction.validate: SSE_UCOMIS, src2size"
                in
                  case (src1,src2)
                    of (Operand.MemLoc _, _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_UCOMIS, src1:MemLoc"
                     | (_, Operand.MemLoc _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_UCOMIS, src2:MemLoc"
                     | (Operand.Register _, _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_UCOMIS, src1:Register"
                     | (Operand.Immediate _, _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_UCOMIS, src1:Immediate"
                     | (Operand.Label _, _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_UCOMIS, src1:Label"
                     | (_, Operand.Register _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_UCOMIS, src2:Register"
                     | (_, Operand.Immediate _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_UCOMIS, src2:Immediate"
                     | (_, Operand.Label _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_UCOMIS, src2:Label"
                     | (_, Operand.Address _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_COMIS, src2:Address"
                     | _ => (Operand.validate {operand = src1}) andalso
                            (Operand.validate {operand = src2})
                end
             | SSE_CVTSFP2SFP {src, srcsize, dst, dstsize, ...}
               (* Scalar SSE floating-point/floating-point convert instruction.
                * Require src/dst operands as follows:
                *
                *              dst
                *          reg xmm imm lab add 
                *      reg
                *      xmm      X
                *  src imm
                *      lab
                *      add      X
                *
                * Require srcsize/dstsize modifier class as follows: FLT != FLT
                *)
             => let
                  val _ = if Size.class srcsize = Size.FLT
                            then ()
                            else Error.bug "amd64Validate.Instruction.validate: SSE_CVTSFP2SFP, srcsize"
                  val _ = if Size.class dstsize = Size.FLT
                            then ()
                            else Error.bug "amd64Validate.Instruction.validate: SSE_CVTSFP2SFP, dstsize"
                  val _ = case Operand.size src
                            of NONE => ()
                             | SOME srcsize' 
                             => if srcsize' = srcsize
                                  then ()
                                  else Error.bug "amd64Validate.Instruction.validate: SSE_CVTSFP2SFP, srcsize"
                  val _ = case Operand.size dst
                            of NONE => ()
                             | SOME dstsize' 
                             => if dstsize' = dstsize
                                  then ()
                                  else Error.bug "amd64Validate.Instruction.validate: SSE_CVTSFP2SFP, dstsize"
                  val _ = if srcsize <> dstsize
                            then ()
                            else Error.bug 
                                 "amd64Validate.Instruction.validate: SSE_CVTSFP2SFP, srcsize = dstsize"
                in
                  case (src,dst)
                    of (Operand.MemLoc _, _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_CVTSFP2SFP, src:MemLoc"
                     | (_, Operand.MemLoc _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_CVTSFP2SFP, dst:MemLoc"
                     | (Operand.Register _, _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_CVTSFP2SFP, src:Register"
                     | (Operand.Immediate _, _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_CVTSFP2SFP, src:Immediate"
                     | (Operand.Label _, _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_CVTSFP2SFP, src:Label"
                     | (_, Operand.Register _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_CVTSFP2SFP, dst:Register"
                     | (_, Operand.Immediate _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_CVTSFP2SFP, dst:Immediate"
                     | (_, Operand.Label _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_CVTSFP2SFP, dst:Label"
                     | (_, Operand.Address _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_CVTSFP2SFP, dst:Address"
                     | _ => (Operand.validate {operand = src}) andalso
                            (Operand.validate {operand = dst})
                end
             | SSE_CVTSFP2SI {src, srcsize, dst, dstsize, ...}
               (* Scalar SSE floating-point/signed-integer convert instruction.
                * Require src/dst operands as follows:
                *
                *              dst
                *          reg xmm imm lab add 
                *      reg  X
                *      xmm
                *  src imm
                *      lab
                *      add  X
                *
                * Require srcsize/dstsize modifier class as follows: FLT/INT
                *)
             => let
                  val _ = if Size.class srcsize = Size.FLT
                            then ()
                            else Error.bug "amd64Validate.Instruction.validate: SSE_CVTSFP2SI, srcsize"
                  val _ = if Size.class dstsize = Size.INT
                            then ()
                            else Error.bug "amd64Validate.Instruction.validate: SSE_CVTSFP2SI, dstsize"
                  val _ = case Operand.size src
                            of NONE => ()
                             | SOME srcsize' 
                             => if srcsize' = srcsize
                                  then ()
                                  else Error.bug "amd64Validate.Instruction.validate: SSE_CVTSFP2SI, srcsize"
                  val _ = case Operand.size dst
                            of NONE => ()
                             | SOME dstsize' 
                             => if dstsize' = dstsize
                                  then ()
                                  else Error.bug "amd64Validate.Instruction.validate: SSE_CVTSFP2SI, dstsize"
                  val _ = if Size.eq(dstsize, Size.LONG) orelse Size.eq(dstsize, Size.QUAD)
                            then ()
                            else Error.bug 
                                 "amd64Validate.Instruction.validate: SSE_CVTSFP2SI, dstsize"
                in
                  case (src,dst)
                    of (Operand.MemLoc _, _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_CVTSFP2SI, src:MemLoc"
                     | (_, Operand.MemLoc _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_CVTSFP2SI, dst:MemLoc"
                     | (Operand.Register _, _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_CVTSFP2SI, src:Register"
                     | (Operand.Immediate _, _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_CVTSFP2SI, src:Immediate"
                     | (Operand.Label _, _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_CVTSFP2SI, src:Label"
                     | (_, Operand.XmmRegister _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_CVTSFP2SI, dst:XmmRegister"
                     | (_, Operand.Immediate _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_CVTSFP2SI, dst:Immediate"
                     | (_, Operand.Label _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_CVTSFP2SI, dst:Label"
                     | (_, Operand.Address _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_CVTSFP2SI, dst:Address"
                     | _ => (Operand.validate {operand = src}) andalso
                            (Operand.validate {operand = dst})
                end
             | SSE_CVTSI2SFP {src, srcsize, dst, dstsize, ...}
               (* Scalar SSE floating-point/signed-integer convert instruction.
                * Require src/dst operands as follows:
                *
                *              dst
                *          reg xmm imm lab add 
                *      reg      X
                *      xmm
                *  src imm
                *      lab
                *      add      X
                *
                * Require srcsize/dstsize modifier class as follows: INT/FLT
                *)
             => let
                  val _ = if Size.class srcsize = Size.INT
                            then ()
                            else Error.bug "amd64Validate.Instruction.validate: SSE_CVTSI2SFP, srcsize"
                  val _ = if Size.class dstsize = Size.FLT
                            then ()
                            else Error.bug "amd64Validate.Instruction.validate: SSE_CVTSI2SFP, dstsize"
                  val _ = case Operand.size src
                            of NONE => ()
                             | SOME srcsize' 
                             => if srcsize' = srcsize
                                  then ()
                                  else Error.bug "amd64Validate.Instruction.validate: SSE_CVTSI2SFP, srcsize"
                  val _ = case Operand.size dst
                            of NONE => ()
                             | SOME dstsize' 
                             => if dstsize' = dstsize
                                  then ()
                                  else Error.bug "amd64Validate.Instruction.validate: SSE_CVTSI2SFP, dstsize"
                  val _ = if Size.eq(srcsize, Size.LONG) orelse Size.eq(srcsize, Size.QUAD)
                            then ()
                            else Error.bug 
                                 "amd64Validate.Instruction.validate: SSE_CVTSI2SFP, srcsize"
                in
                  case (src,dst)
                    of (Operand.MemLoc _, _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_CVTSI2SFP, src:MemLoc"
                     | (_, Operand.MemLoc _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_CVTSI2SFP, dst:MemLoc"
                     | (Operand.XmmRegister _, _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_CVTSI2SFP, src:XmmRegister"
                     | (Operand.Immediate _, _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_CVTSI2SFP, src:Immediate"
                     | (Operand.Label _, _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_CVTSI2SFP, src:Label"
                     | (_, Operand.Register _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_CVTSI2SFP, dst:Register"
                     | (_, Operand.Immediate _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_CVTSI2SFP, dst:Immediate"
                     | (_, Operand.Label _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_CVTSI2SFP, dst:Label"
                     | (_, Operand.Address _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_CVTSI2SFP, dst:Address"
                     | _ => (Operand.validate {operand = src}) andalso
                            (Operand.validate {operand = dst})
                end
             | SSE_MOVD {src, srcsize, dst, dstsize, ...}
               (* Scalar SSE move data instruction.
                * Require src/dst operands as follows:
                *
                *              dst
                *          reg xmm imm lab add 
                *      reg      X
                *      xmm  X               X
                *  src imm
                *      lab
                *      add      X
                *
                * Require size modifier class as follows: FLT/INT
                *)
             => let
                  val _ = if ((Size.class srcsize = Size.FLT
                               andalso Size.class dstsize = Size.INT)
                              orelse (Size.class srcsize = Size.INT
                                      andalso Size.class dstsize = Size.FLT))
                            then ()
                            else Error.bug "amd64Validate.Instruction.validate: SSE_MOVD, size"
                  val _ = case Operand.size src
                            of NONE => ()
                             | SOME srcsize' 
                             => if srcsize' = srcsize
                                  then ()
                                  else Error.bug "amd64Validate.Instruction.validate: SSE_MOVD, srcsize"
                  val _ = case Operand.size dst
                            of NONE => ()
                             | SOME dstsize'
                             => if dstsize' = dstsize
                                  then ()
                                  else Error.bug "amd64Validate.Instruction.validate: SSE_MOVD, dstsize"
                in
                  case (src,dst)
                    of (Operand.MemLoc _, _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_MOVD, src:MemLoc"
                     | (_, Operand.MemLoc _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_MOVD, dst:MemLoc"
                     | (Operand.Immediate _, _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_MOVD, src:Immediate"
                     | (Operand.Label _, _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_MOVD, src:Label"
                     | (_, Operand.Immediate _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_MOVD, dst:Immediate"
                     | (_, Operand.Label _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_MOVD, dst:Label"
                     | (Operand.Register _, Operand.Address _) 
                     => Error.bug "amd64Validate.Instruction.validate: SSE_MOVD, src: Register, dst:Addrss"
                     | (Operand.Address _, Operand.Register _) 
                     => Error.bug "amd64Validate.Instruction.validate: SSE_MOVD, src: Address, dst:Register"
                     | (Operand.Address _, Operand.Address _)
                     => Error.bug "amd64Validate.Instruction.validate: SSE_MOVD, src,dst:Address"
                     | _ => (Operand.validate {operand = src}) andalso
                            (Operand.validate {operand = dst})
                end
             | _ => Error.bug (concat ["amd64Validate.Instruction.validate: instruction :: ",
                                       toString instruction])
    end

  structure Assembly =
    struct
      open amd64.Assembly

      fun validate {assembly: t list} : bool
        = List.fold(assembly,
                    true,
                    fn (Comment _, b)
                     => b
                     | (Directive _, _)
                     => Error.bug "amd64Validate.Assembly.validate: Directive"
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
         else Error.bug "amd64Validate.validate")       

  val (validate, validate_msg)
    = tracerTop
      "validate"
      validate

  fun validate_totals ()
    = (validate_msg ();
       Control.indent ();
       Control.unindent ())
end
