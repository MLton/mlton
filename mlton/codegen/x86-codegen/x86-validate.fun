(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor x86Validate(S: X86_VALIDATE_STRUCTS): X86_VALIDATE =
struct

  open S;
  open x86;

  val tracer = x86.tracer
  val tracerTop = x86.tracerTop

  structure Register =
    struct
      open Register

      fun validate {register}
	= if not (List.contains(registers (size register),
				register,
				eq))
	    then Error.bug "validate: Register"
	    else true

      fun validate_base {register}
	= if not (validate {register = register}
		  andalso
		  List.contains(baseRegisters,
				register,
				eq))
	    then Error.bug "validate: Register, base"
	    else true

      fun validate_index {register}
	= if not (validate {register = register}
		  andalso
		  List.contains(indexRegisters,
				register,
				eq))
	    then Error.bug "validate: Register, index"
	    else true
    end

  structure FltRegister =
    struct
      open FltRegister

      fun validate {fltregister as FltRegister.T i}
	= if 0 > i orelse i > 7
	    then Error.bug "validate: FltRegister"
	    else true
    end

  structure Address =
    struct
      open Address

      fun validate {address as Address.T {disp, base, index, scale}}
	= let
	    val _ = case base
	              of NONE => ()
		       | SOME r => if Register.validate_base {register = r}
				     then ()
				     else Error.bug "validate: Address, base"

	    val _ = case index
	              of NONE => ()
		       | SOME r => if Register.validate_index {register = r}
				     then ()
				     else Error.bug "validate: Address, index"
	  in
	    case address
	      of Address.T {disp = NONE, base = NONE, 
			    index = NONE, scale = NONE}
	       => Error.bug "validate: Address"
	       | Address.T {disp, base, 
			    index = NONE, scale = SOME _}
	       => Error.bug "validate: Address, scale"
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
	     | MemLoc _ => Error.bug "validate: Operand, MemLoc"
    end

  structure Instruction =
    struct
      open x86.Instruction

      fun validate {instruction: t}
	= case instruction
	    of BinAL {oper, src, dst, size}
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
			    else Error.bug "validate: BinAL, size"
		  val _ = case Operand.size src
			    of NONE => ()
			     | SOME srcsize 
			     => if srcsize = size
				  then ()
				  else Error.bug "validate: BinAL, srcsize"
 		  val _ = case Operand.size dst
			    of NONE => ()
			     | SOME dstsize 
			     => if dstsize = size
				  then ()
				  else Error.bug "validate: BinAL, dstsize"
		in
		  case (src,dst)
		    of (Operand.MemLoc _, _)
		     => Error.bug "validate: BinAL, src:MemLoc"
		     | (_, Operand.MemLoc _)
		     => Error.bug "validate: BinAL, dst:MemLoc"
		     | (Operand.FltRegister _, _)
		     => Error.bug "validate: BinAL, src:FltRegister"
		     | (Operand.Label _, _)
		     => Error.bug "validate: BinAL, src:Label"
		     | (_, Operand.FltRegister _)
		     => Error.bug "validate: BinAL, dst:FltRegister"
		     | (_, Operand.Immediate _)
		     => Error.bug "validate: BinAL, dst:Immediate"
		     | (_, Operand.Label _)
                     => Error.bug "validate: BinAL, dst:Label"
		     | (Operand.Address _, Operand.Address _)
		     => Error.bug "validate: BinAL, src,dst:Address"
		     | _ => (Operand.validate {operand = src}) andalso
                            (Operand.validate {operand = dst})
		end
	     | MD {oper, src, size} 
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
			    else Error.bug "validate: BinMD, size"
		  val _ = case Operand.size src
			    of NONE => ()
			     | SOME srcsize 
			     => if srcsize = size
				  then ()
				  else Error.bug "validate: MD, srcsize"
		in
		  case src
		    of Operand.MemLoc _
		     => Error.bug "validate: MD, src:MemLoc"
		     | Operand.FltRegister _
		     => Error.bug "validate: MD, src:FltRegister"
		     | Operand.Immediate _
		     => Error.bug "validate: MD, src:Immediate"
		     | Operand.Label _
		     => Error.bug "validate: MD, src:Label"
		     | _ => (Operand.validate {operand = src})
		end
	     | UnAL {oper, dst, size}
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
			    else Error.bug "validate: UnAL, size"
 		  val _ = case Operand.size dst
			    of NONE => ()
			     | SOME dstsize 
			     => if dstsize = size
				  then ()
				  else Error.bug "validate: UnAL, dstsize"
		in
		  case dst
		    of Operand.MemLoc _
		     => Error.bug "validate: UnAL, dst:MemLoc"
		     | Operand.FltRegister _
		     => Error.bug "validate: UnAL, dst:FltRegister"
		     | Operand.Immediate _
		     => Error.bug "validate: UnAL, dst:Immediate"
		     | Operand.Label _
		     => Error.bug "validate: UnAL, dst:Label"
		     | _ => (Operand.validate {operand = dst})
		end
	     | SRAL {oper, count, dst, size}
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
			    else Error.bug "validate: SRAL, size"
 		  val _ = case Operand.size dst
			    of NONE => ()
			     | SOME dstsize 
			     => if dstsize = size
				  then ()
				  else Error.bug "validate: SRAL, dstsize"

		  val _ = case count
			    of Operand.MemLoc _
			     => Error.bug "validate: SRAL, count:MemLoc"
			     | Operand.FltRegister _
			     => Error.bug "validate: SRAL, count:FltRegister"
			     | Operand.Label _
			     => Error.bug "validate: SRAL, count:Label"
			     | Operand.Address _
			     => Error.bug "validate: SRAL, count:Address"
			     | Operand.Register (Register.T {reg, part})
			     => if reg <> Register.ECX orelse
			           part <> Register.L
				  then Error.bug 
				       "validate: SRAL, count:Register"
				  else ()
			     | _ => ()
		in
		  case dst
		    of Operand.MemLoc _
		     => Error.bug "validate: SRAL, dst:MemLoc"
		     | Operand.FltRegister _
		     => Error.bug "validate: SRAL, dst:FltRegister"
		     | Operand.Immediate _
		     => Error.bug "validate: SRAL, dst:Immediate"
		     | Operand.Label _
		     => Error.bug "validate: SRAL, dst:Label"
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
			    else Error.bug "validate: CMP, size"
		  val _ = case Operand.size src1
			    of NONE => ()
			     | SOME src1size 
			     => if src1size = size
				  then ()
				  else Error.bug "validate: CMP, src1size"
 		  val _ = case Operand.size src2
			    of NONE => ()
			     | SOME src2size 
			     => if src2size = size
				  then ()
				  else Error.bug "validate: CMP, src2size"
		in
		  case (src1,src2)
		    of (Operand.MemLoc _, _)
		     => Error.bug "validate: CMP, src1:MemLoc"
		     | (_, Operand.MemLoc _)
		     => Error.bug "validate: CMP, src2:MemLoc"
		     | (Operand.FltRegister _, _)
		     => Error.bug "validate: CMP, src1: FltRegister"
		     | (Operand.Immediate _, _)
		     => Error.bug "validate: CMP, src1:Immediate"
		     | (Operand.Label _, _)
		     => Error.bug "validate: CMP, src1:Label"
		     | (_, Operand.FltRegister _)
		     => Error.bug "validate: CMP, src2: FltRegister"
		     | (_, Operand.Label _)
		     => Error.bug "validate: CMP, src2:Label"
		     | (Operand.Address _, Operand.Address _)
                     => Error.bug "validate: CMP, src1,src2:Address"
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
			    else Error.bug "validate: TEST, size"
		  val _ = case Operand.size src1
			    of NONE => ()
			     | SOME src1size 
			     => if src1size = size
				  then ()
				  else Error.bug "validate: TEST, src1size"
 		  val _ = case Operand.size src2
			    of NONE => ()
			     | SOME src2size 
			     => if src2size = size
				  then ()
				  else Error.bug "validate: TEST, src2size"
		in
		  case (src1,src2)
		    of (Operand.MemLoc _, _)
		     => Error.bug "validate: TEST, src1:MemLoc"
		     | (_, Operand.MemLoc _)
		     => Error.bug "validate: TEST, src2:MemLoc"
		     | (Operand.FltRegister _, _)
		     => Error.bug "validate: TEST, src1: FltRegister"
		     | (Operand.Immediate _, _)
		     => Error.bug "validate: TEST, src1:Immediate"	
		     | (Operand.Label _, _)
		     => Error.bug "validate: TEST, src1:Label"
		     | (_, Operand.FltRegister _)
		     => Error.bug "validate: TEST, src2: FltRegister"
		     | (_, Operand.Label _)
		     => Error.bug "validate: TEST, src2:Label"
		     | (Operand.Address _, Operand.Address _)
                     => Error.bug "validate: TEST, src1,src2:Address"
		     | _ => (Operand.validate {operand = src1}) andalso
                            (Operand.validate {operand = src2})
		end
	     | SETcc {condition, dst, size}
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
			     | _ => Error.bug "validate: SETcc, size"
		  val _ = case Operand.size dst
			    of NONE => ()
			     | SOME dstsize 
			     => if dstsize = size
				  then ()
				  else Error.bug "validate: SETcc, dstsize"
		in
		  case dst
		    of Operand.MemLoc _
		     => Error.bug "validate: SETcc, dst:MemLoc"
		     | Operand.FltRegister _
		     => Error.bug "validate: SETcc, dst:FltRegister"
		     | Operand.Immediate _
		     => Error.bug "validate: SETcc, dst:Immediate"
		     | Operand.Label _
		     => Error.bug "validate: SETcc, dst:Label"
		     | _ => (Operand.validate {operand = dst})
		end
             | JMP {target, absolute}
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
		     => Error.bug "validate: JMP, target:MemLoc"
		     | Operand.FltRegister _
		     => Error.bug "validate: JMP, target:FltRegister"
		     | _ => (Operand.validate {operand = target})
		end
	     | Jcc {condition, target}
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
		     => Error.bug "validate: Jcc, target:MemLoc"
		     | Operand.Register _
		     => Error.bug "validate: Jcc, target:Register"
		     | Operand.FltRegister _
		     => Error.bug "validate: Jcc, target:FltRegister"
		     | Operand.Address _
		     => Error.bug "validate: Jcc, target:Address"
		     | _ => (Operand.validate {operand = target})
		end
             | CALL {target, absolute}
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
		     => Error.bug "validate: CALL, target:MemLoc"
		     | Operand.FltRegister _
		     => Error.bug "validate: CALL, target:FltRegister"
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
		     => Error.bug "validate: RET, src:MemLoc"
		     | SOME (Operand.Register _)
		     => Error.bug "validate: RET, src:Register"
		     | SOME (Operand.FltRegister _)
		     => Error.bug "validate: RET, src:FltRegister"
		     | SOME (Operand.Label _)
		     => Error.bug "validate: RET, src:Label"
		     | SOME (Operand.Address _)
		     => Error.bug "validate: RET, src:Address"
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
			    else Error.bug "validate: MOV, size"
		  val _ = case Operand.size src
			    of NONE => ()
			     | SOME srcsize
			     => if srcsize = size
				  then ()
				  else Error.bug "validate: MOV, srcsize"
 		  val _ = case Operand.size dst
			    of NONE => ()
			     | SOME dstsize 
			     => if dstsize = size
				  then ()
				  else Error.bug "validate: MOV, dstsize"
		in
		  case (src,dst)
		    of (Operand.MemLoc _, _)
		     => Error.bug "validate: MOV, src:MemLoc"
		     | (_, Operand.MemLoc _)
		     => Error.bug "validate: MOV, dst:MemLoc"
		     | (Operand.FltRegister _, _)
		     => Error.bug "validate: MOV, src:FltRegister"
		     | (Operand.Label _, _)
		     => Error.bug "validate: MOV, src:Label"
		     | (_, Operand.FltRegister _)
		     => Error.bug "validate: MOV, dst:FltRegister"
		     | (_, Operand.Immediate _)
		     => Error.bug "validate: MOV, dst:Immediate"
		     | (_, Operand.Label _)
                     => Error.bug "validate: MOV, dst:Label"
		     | (Operand.Address _, Operand.Address _)
		     => Error.bug "validate: MOV, src,dst:Address"
		     | _ => (Operand.validate {operand = src}) andalso
                            (Operand.validate {operand = dst})
		end
             | CMOVcc {condition, src, dst, size}
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
			     | _ => Error.bug "validate: CMOVcc, size"
		  val _ = case Operand.size src
			    of NONE => ()
			     | SOME srcsize 
			     => if srcsize = size
				  then ()
				  else Error.bug "validate: CMOVcc, srcsize"
 		  val _ = case Operand.size dst
			    of NONE => ()
			     | SOME dstsize 
			     => if dstsize = size
				  then ()
				  else Error.bug "validate: CMOVcc, dstsize"
		in
		  case (src,dst)
		    of (Operand.MemLoc _, _)
		     => Error.bug "validate: CMOVcc, src:MemLoc"
		     | (_, Operand.MemLoc _)
		     => Error.bug "validate: CMOVcc, dst:MemLoc"
		     | (Operand.FltRegister _, _)
		     => Error.bug "validate: CMOVcc, src:FltRegister"
		     | (Operand.Immediate _, _)
		     => Error.bug "validate: CMOVcc, src:Immediate"
		     | (Operand.Label _, _)
		     => Error.bug "validate: CMOVcc, src:Label"	
		     | (_, Operand.FltRegister _)
		     => Error.bug "validate: CMOVcc, dst:FltRegister"
		     | (_, Operand.Immediate _)
		     => Error.bug "validate: CMOVcc, dst:Immediate"
		     | (_, Operand.Label _)
                     => Error.bug "validate: CMOVcc, dst:Label"
		     | (_, Operand.Address _)
		     => Error.bug "validate: CMOVcc, dst:Address"
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
			    else Error.bug "validate: XCHG, size"
		  val _ = case Operand.size src
			    of NONE => ()
			     | SOME srcsize 
			     => if srcsize = size
				  then ()
				  else Error.bug "validate: XCHG, srcsize"
 		  val _ = case Operand.size dst
			    of NONE => ()
			     | SOME dstsize 
			     => if dstsize = size
				  then ()
				  else Error.bug "validate: XCHG, dstsize"
		in
		  case (src,dst)
		    of (Operand.MemLoc _, _)
		     => Error.bug "validate: XCHG, src:MemLoc"
		     | (_, Operand.MemLoc _)
		     => Error.bug "validate: XCHG, dst:MemLoc"
		     | (Operand.FltRegister _, _)
		     => Error.bug "validate: XCHG, src:FltRegister"
		     | (Operand.Immediate _, _)
		     => Error.bug "validate: XCHG, src:Immediate"
		     | (Operand.Label _, _)
		     => Error.bug "validate: XCHG, src:Label"
		     | (_, Operand.FltRegister _)
		     => Error.bug "validate: XCHG, dst:FltRegister"
		     | (_, Operand.Immediate _)
		     => Error.bug "validate: XCHG, dst:Immediate"
		     | (_, Operand.Label _)
                     => Error.bug "validate: XCHG, dst:Label"
		     | (Operand.Address _, Operand.Address _)
		     => Error.bug "validate: XCHG, src,dst:Address"
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
			     | _ => Error.bug "validate: PUSH, size"
 		  val _ = case Operand.size src
			    of NONE => ()
			     | SOME srcsize 
			     => if srcsize = size
				  then ()
				  else Error.bug "validate: PUSH, srcsize"
		in
		  case src
		    of Operand.MemLoc _
		     => Error.bug "validate: PUSH, src:MemLoc"
		     | Operand.FltRegister _
		     => Error.bug "validate: PUSH, src:FltRegister"
		     | Operand.Label _
		     => Error.bug "validate: PUSH, src:Label"
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
			     | _ => Error.bug "validate: POP, size"
 		  val _ = case Operand.size dst
			    of NONE => ()
			     | SOME dstsize 
			     => if dstsize = size
				  then ()
				  else Error.bug "validate: POP, dstsize"
		in
		  case dst
		    of Operand.MemLoc _
		     => Error.bug "validate: POP, dst:MemLoc"
		     | Operand.FltRegister _
		     => Error.bug "validate: POP, src:FltRegister"
		     | Operand.Immediate _
		     => Error.bug "validate: POP, dst:Immediate"
		     | Operand.Label _
		     => Error.bug "validate: POP, dst:Label"
		     | _ => (Operand.validate {operand = dst}) 
		end
             | CX {size}
	       (* Convert X to 2X with sign extension; p. 104,181
		* Require size modifier class as follows: INT
		*)
	     => let
		  val _ = if Size.class size = Size.INT
			    then ()
			    else Error.bug "validate: MOVX, srcsize"
		in
		  true
		end
	     | MOVX {oper, src, dst, srcsize, dstsize}
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
			    else Error.bug "validate: MOVX, srcsize"
		  val _ = if Size.class dstsize = Size.INT
			    then ()
			    else Error.bug "validate: MOVX, dstsize"
		  val _ = case Operand.size src
			    of NONE => ()
			     | SOME srcsize' 
			     => if srcsize' = srcsize
				  then ()
				  else Error.bug "validate: MOVX, srcsize"
 		  val _ = case Operand.size dst
			    of NONE => ()
			     | SOME dstsize' 
			     => if dstsize' = dstsize
				  then ()
				  else Error.bug "validate: MOVX, dstsize"
		  val _ = if Size.lt(srcsize,dstsize)
			    then ()
			    else Error.bug 
			         "validate: MOVX, srcsize >= dstsize"
		in
		  case (src,dst)
		    of (Operand.MemLoc _, _)
		     => Error.bug "validate: MOVX, src:MemLoc"
		     | (_, Operand.MemLoc _)
		     => Error.bug "validate: MOVX, dst:MemLoc"
		     | (Operand.FltRegister _, _)
		     => Error.bug "validate: MOVX, src:FltRegister"
		     | (Operand.Immediate _, _)
		     => Error.bug "validate: MOVX, src:Immediate"
		     | (Operand.Label _, _)
		     => Error.bug "validate: MOVX, src:Label"
		     | (_, Operand.FltRegister _)
		     => Error.bug "validate: MOVX, dst:FltRegister"
		     | (_, Operand.Immediate _)
		     => Error.bug "validate: MOVX, dst:Immediate"
		     | (_, Operand.Label _)
                     => Error.bug "validate: MOVX, dst:Label"
		     | (_, Operand.Address _)
		     => Error.bug "validate: MOVX, dst:Address"
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
			     | _ => Error.bug "validate: LEA, size"
		in
		  case (src,dst)
		    of (Operand.MemLoc _, _)
		     => Error.bug "validate: LEA, src:MemLoc"
		     | (_, Operand.MemLoc _)
		     => Error.bug "validate: LEA, dst:MemLoc"
		     | (Operand.Register _, _)
		     => Error.bug "validate: LEA, src:Register"
		     | (Operand.FltRegister _, _)
		     => Error.bug "validate: LEA, src:FltRegister"
		     | (Operand.Immediate _, _)
		     => Error.bug "validate: LEA, src:Immediate"
		     | (Operand.Label _, _)
		     => Error.bug "validate: LEA, src:Label"
		     | (_, Operand.FltRegister _)
		     => Error.bug "validate: LEA, dst:FltRegister"
		     | (_, Operand.Immediate _)
		     => Error.bug "validate: LEA, dst:Immediate"
		     | (_, Operand.Label _)
                     => Error.bug "validate: LEA, dst:Label"
		     | (_, Operand.Address _)
		     => Error.bug "validate: LEA, dst:Address"
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
			    else Error.bug "validate: FLD, size"
		in
		  case src
		    of Operand.MemLoc _
		     => Error.bug "validate: FLD, src:MemLoc"
		     | Operand.Register _
		     => Error.bug "validate: FLD, src:Register"
		     | Operand.Immediate _
		     => Error.bug "validate: FLD, src:Immediate"
		     | Operand.Label _
		     => Error.bug "validate: FLD, src:Label"
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
					    => Error.bug "validate: FST, size"
				    else ())
			    else Error.bug "validate: FST, size"
		in
		  case dst
		    of Operand.MemLoc _
		     => Error.bug "validate: FST, dst:MemLoc"
		     | Operand.Register _
		     => Error.bug "validate: FST, dst:Register"
		     | Operand.Immediate _
		     => Error.bug "validate: FST, dst:Immediate"
		     | Operand.Label _
		     => Error.bug "validate: FST, dst:Label"
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
			    else Error.bug "validate: FILD, size"
		in
		  case src
		    of Operand.MemLoc _
		     => Error.bug "validate: FILD, src:MemLoc"
		     | Operand.Register _
		     => Error.bug "validate: FILD, src:Register"
		     | Operand.FltRegister _
		     => Error.bug "validate: FILD, src:FltRegister"
		     | Operand.Immediate _
		     => Error.bug "validate: FILD, src:Immediate"
		     | Operand.Label _
		     => Error.bug "validate: FILD, src:Label"
		     | _ => Operand.validate {operand = src}
		end
             | FIST {dst, size, pop}
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
			    else Error.bug "validate: FIST, size"
		in
		  case dst
		    of Operand.MemLoc _
		     => Error.bug "validate: FIST, src:MemLoc"
		     | Operand.Register _
		     => Error.bug "validate: FIST, src:Register"
		     | Operand.FltRegister _
		     => Error.bug "validate: FIST, src:FltRegister"
		     | Operand.Immediate _
		     => Error.bug "validate: FIST, src:Immediate"
		     | Operand.Label _
		     => Error.bug "validate: FIST, src:Label"
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
		     => Error.bug "validate: FXCH, dst:MemLoc"
		     | Operand.Register _
		     => Error.bug "validate: FXCH, dst:Register"
		     | Operand.Immediate _
		     => Error.bug "validate: FXCH, dst:Immediate"
		     | Operand.Label _
		     => Error.bug "validate: FXCH, dst:Label"
		     | _ => Operand.validate {operand = src}
		end
             | FLDC {oper}
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
		     => Error.bug "validate: FLDCW, src:MemLoc"
		     | Operand.Register _ 
		     => Error.bug "validate: FLDCW, src:Register"
		     | Operand.FltRegister _ 
		     => Error.bug "validate: FLDCW, src:Register"
		     | Operand.Immediate _
		     => Error.bug "validate: FLDCW, src:Immediate"
		     | Operand.Label _
		     => Error.bug "validate: FLDCW, src:Label"
		     | _ => Operand.validate {operand = src}
		end
	     | FSTCW {dst, check}
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
		     => Error.bug "validate: FSTCW, dst:MemLoc"
		     | Operand.Register _ 
		     => Error.bug "validate: FSTCW, dst:Register"
		     | Operand.FltRegister _ 
		     => Error.bug "validate: FSTCW, dst:FltRegister"
		     | Operand.Immediate _
		     => Error.bug "validate: FSTCW, dst:Immediate"
		     | Operand.Label _
		     => Error.bug "validate: FSTCW, dst:Label"
		     | _ => Operand.validate {operand = dst}
		end
	     | FSTSW {dst, check}
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
		     => Error.bug "validate: FSTSW, dst:MemLoc"
		     | Operand.Register (Register.T {reg = Register.EAX,
						     part = Register.X})
		     => Operand.validate {operand = dst}
		     | Operand.Register _ 
		     => Error.bug "validate: FSTSW, dst:Register"
		     | Operand.FltRegister _ 
		     => Error.bug "validate: FSTSW, dst:FltRegister"
		     | Operand.Immediate _
		     => Error.bug "validate: FSTSW, dst:Immediate"
		     | Operand.Label _
		     => Error.bug "validate: FSTSW, dst:Label"
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
					      "validate: FCOM, size")
				    | _ => ()
			    else Error.bug "validate: FCOM, size"
		in
		  case src
		    of Operand.MemLoc _
		     => Error.bug "validate: FCOM, src:MemLoc"
		     | Operand.Register _
		     => Error.bug "validate: FCOM, src:Register"
		     | Operand.Immediate _
		     => Error.bug "validate: FCOM, src:Immediate"
		     | Operand.Label _
		     => Error.bug "validate: FCOM, src:Label"
		     | _ 
		     => if pop andalso pop'
			   andalso
			   not 
			   (Operand.eq(src,
				       Operand.fltregister FltRegister.one))
			  then Error.bug "validate: FCOM, pop, pop'"
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
		     => Error.bug "validate: FUCOM, src:MemLoc"
		     | Operand.Register _
		     => Error.bug "validate: FUCOM, src:Register"
		     | Operand.Immediate _
		     => Error.bug "validate: FUCOM, src:Immediate"
		     | Operand.Label _
		     => Error.bug "validate: FUCOM, src:Label"
		     | Operand.Address _
		     => Error.bug "validate: FUCOM, src:Address"
		     | _ 
		     => if pop andalso pop'
			   andalso
			   not 
			   (Operand.eq(src,
				       Operand.fltregister FltRegister.one))
			  then Error.bug "validate: FUCOM, pop, pop'"
			  else Operand.validate {operand = src}
		end
	     | FBinA {oper, src, dst, size, pop}
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
					      "validate: FBinA, size")
				    | _ => ()
			    else Error.bug "validate: FBinA, size"
		in
		  case (src,dst)
		    of (Operand.MemLoc _, _)
		     => Error.bug "validate: FBinA, src:MemLoc"
		     | (_, Operand.MemLoc _)
		     => Error.bug "validate: FBinA, dst:MemLoc"
		     | (Operand.Register _, _)
		     => Error.bug "validate: FBinA, src:Register"
		     | (Operand.Immediate _, _)
		     => Error.bug "validate: FBinA, src:Immediate"
		     | (Operand.Label _, _)
		     => Error.bug "validate: FBinA, src:Label"
		     | (_, Operand.Register _)
		     => Error.bug "validate: FBinA, dst:Register"
		     | (_, Operand.Immediate _)
		     => Error.bug "validate: FBinA, dst:Immediate"
		     | (_, Operand.Label _)
                     => Error.bug "validate: FBinA, dst:Label"
		     | (_, Operand.Address _)
		     => Error.bug "validate: FBinA, dst:Address"
		     | (Operand.Address _, _)
		     => if Operand.eq(dst, 
				      Operand.fltregister FltRegister.top)
			  then (Operand.validate {operand = src}) andalso
                               (Operand.validate {operand = dst})
			  else Error.bug "validate: FBinA, src:Address"
		     | _
		     => if pop 
			   andalso
			   not 
			   (Operand.eq(src,
				       Operand.fltregister FltRegister.top))
			  then Error.bug "validate: FBinA, pop"
			  else (Operand.validate {operand = src}) andalso
			       (Operand.validate {operand = dst})
		end
	     | FUnA {oper}
	       (* Floating-point unary arithmetic instructions.
		*)
	     => true
	     | FPTAN
	       (* Floating-point partial tangent instruction.
		*)
	     => true
	     | FBinAS {oper}
	       (* Floating-point binary arithmetic stack instructions.
		*)
	     => true
	     | FBinASP {oper}
	       (* Floating-point binary arithmetic stack pop instructions.
		*)
	     => true
             | _ => Error.bug (concat ["validate: instruction :: ",
				       toString instruction])
    end

  structure Assembly =
    struct
      open x86.Assembly

      fun validate {assembly: t list} : bool
 	= List.fold(assembly,
		    true,
		    fn (Comment s, b)
		     => b
		     | (Directive d, b)
		     => Error.bug "validate: Directive"
		     | (PseudoOp p, b)
		     => b
		     | (Label l, b)
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
		          handle Fail msg
			   => (List.foreach
			       (assembly,
				fn assembly
				 => (print (Assembly.toString assembly);
				     print "\n"));
			       Error.bug msg))
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
