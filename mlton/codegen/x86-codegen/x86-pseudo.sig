(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
type int = Int.t
type word = Word.t

signature X86_PSEUDO =
  sig
    structure Label : HASH_ID

    val tracer : string -> ('a -> 'b) -> (('a -> 'b) * (unit -> unit))
    val tracerTop : string -> ('a -> 'b) -> (('a -> 'b) * (unit -> unit))

    structure Size :
      sig
	datatype class = INT | FLT | FPI
	datatype t 
	  = BYTE | WORD | LONG 
	  | SNGL | DBLE | EXTD
	  | FPIS | FPIL | FPIQ
	val toBytes : t -> int
	val class : t -> class
	val eq : t * t -> bool
	val lt : t * t -> bool
      end

    structure Immediate :
      sig
	datatype const
	  = Char of char
	  | Int of int
	  | Word of word
	datatype un
	  = Negation
	  | Complementation
	datatype bin
	  = Multiplication
	  | Division
	  | Remainder
	  | ShiftLeft
          | ShiftRight
	  | BitOr
  	  | BitAnd
	  | BitXor
	  | BitOrNot
	  | Addition
	  | Subtraction
	type t

	val const : const -> t
	val const_char : char -> t
	val const_int : int -> t
	val const_word : word -> t
	val deConst : t -> const option
	val label : Label.t -> t
	val unexp : {oper: un,
		     exp: t} -> t
	val binexp : {oper: bin,
		      exp1: t,
		      exp2: t} -> t
      end

    structure Scale :
      sig
	datatype t = One | Two | Four | Eight
	val fromBytes : int -> t
      end

    structure MemLoc :
      sig
	structure Class :
          sig
	    type t
	    val new : {name: string} -> t
	    val Temp : t
	    val CStack : t
	    val Code : t

	    val eq : t * t -> bool
	  end

	type t

	val imm : {base: Immediate.t,
		   index: Immediate.t,
		   scale: Scale.t,
		   size: Size.t,
		   class: Class.t} -> t
	val basic : {base: Immediate.t,
		     index: t,
		     scale: Scale.t,
		     size: Size.t,
		     class: Class.t} -> t
	val simple : {base: t,
		      index: Immediate.t,
		      scale: Scale.t,
		      size: Size.t,
		      class: Class.t} -> t
	val complex : {base: t,
		       index: t,
		       scale: Scale.t,
		       size: Size.t,
		       class: Class.t} -> t
	  
	val class : t -> Class.t
	val compare : t * t -> order
      end

    structure ClassSet : SET
    sharing type ClassSet.Element.t = MemLoc.Class.t
    structure MemLocSet : SET
    sharing type MemLocSet.Element.t = MemLoc.t

    structure Operand : 
      sig
	type t

	val toString : t -> string

	val immediate : Immediate.t -> t
	val immediate_const_char : char -> t
	val immediate_const_int : int -> t
	val immediate_const_word : word -> t
	val immediate_label : Label.t -> t
	val deImmediate : t -> Immediate.t option
	val label : Label.t -> t
	val memloc : MemLoc.t -> t
	val deMemloc : t -> MemLoc.t option

	val size : t -> Size.t option
	val eq : t * t -> bool
      end

    structure Instruction : 
      sig
	(* Integer binary arithmetic(w/o mult & div)/logic instructions. *)
	datatype binal
	  = ADD (* signed/unsigned addition; p. 63 *)
          | ADC (* signed/unsigned addition with carry; p. 61 *)
          | SUB (* signed/unsigned subtraction; p. 713 *)
          | SBB (* signed/unsigned subtraction with borrow; p. 667 *)
          | AND (* logical and; p. 70 *)
          | OR  (* logical or; p. 499 *)
          | XOR (* logical xor; p. 758 *)
	(* Integer multiplication and division. *)
	datatype md
	  = IMUL (* signed multiplication (one operand form); p. 335 *)
	  | MUL  (* unsigned multiplication; p. 488 *)
	  | IDIV (* signed division; p. 332 *)
	  | DIV  (* unsigned division; p. 188 *)
	  | IMOD (* signed modulus; *)
	  | MOD  (* unsigned modulus; *)
	datatype unal
	  = INC (* increment by 1; p. 341 *)
	  | DEC (* decrement by 1; p. 186 *)
	  | NEG (* two's complement negation; p. 494 *)
	  | NOT (* one's complement negation; p. 497 *)
	(* Integer shift/rotate arithmetic/logic instructions. *)
	datatype sral
	  = SAL (* shift arithmetic left; p. 662 *)
	  | SHL (* shift logical left; p. 662 *)
	  | SAR (* shift arithmetic right; p. 662 *)
	  | SHR (* shift logical right; p. 662 *)
	  | ROL (* rotate left; p. 631 *)
	  | RCL (* rotate through carry left; p. 631 *)
	  | ROR (* rotate right; p. 631 *)
	  | RCR (* rotate through carry right; p. 631 *)
	(* Move with extention instructions. *)
	datatype movx
	  = MOVSX (* move with sign extention; p. 481 *)
	  | MOVZX (* move with zero extention; p. 486 *)
	(* Condition test field; p. 795 *)
	datatype condition
	  = O   (* overflow *)       | NO  (* not overflow *)
	  | B   (* below *)          | NB  (* not below *)
	  | AE  (* above or equal *) | NAE (* not above or equal *)
	  | C   (* carry *)          | NC  (* not carry *)
	  | E   (* equal *)          | NE  (* not equal *)
	  | Z   (* zero *)           | NZ  (* not zero *)
	  | BE  (* below or equal *) | NBE (* not below or equal *)
	  | A   (* above *)          | NA  (* not above *)
	  | S   (* sign *)           | NS  (* not sign *)
	  | P   (* parity *)         | NP  (* not parity *)
	  | PE  (* parity even *)    | PO  (* parity odd *)
	  | L   (* less than *)      
	  | NL  (* not less than *)
	  | LE  (* less than or equal *) 
	  | NLE (* not less than or equal *)
	  | G   (* greater than *)   
	  | NG  (* not greater than *)
	  | GE  (* greater than or equal *) 
	  | NGE (* not greater than or equal *)
	val condition_negate : condition -> condition
	val condition_reverse : condition -> condition
	(* Floating-point binary arithmetic instructions. *)
	datatype fbina
	  = FADD  (* addition; p. 205 *)
          | FSUB  (* subtraction; p. 297 *)
	  | FSUBR (* reversed subtraction; p. 301 *)
	  | FMUL  (* multiplication; p. 256 *)
	  | FDIV  (* division; p. 229 *)
	  | FDIVR (* reversed division; p. 233 *)
	val fbina_reverse : fbina -> fbina
	(* Floating-point unary arithmetic instructions. *)
	datatype funa
	  = F2XM1   (* compute 2^x-1; p. 201 *)
	  | FABS    (* absolute value; p. 203 *)
	  | FCHS    (* change sign; p. 214 *)
	  | FSQRT   (* square root; p. 284 *)
	  | FSIN    (* sine; p. 280 *)
	  | FCOS    (* cosine; p. 226 *)
	  | FRNDINT (* round to integer; p. 271 *)
	(* Floating-point binary arithmetic stack instructions. *)
	datatype fbinas
	  = FSCALE (* scale; p. 278 *)
	  | FPREM  (* partial remainder; p. 263 *)
	  | FPREM1 (* IEEE partial remainder; p. 266 *)
	(* floating point binary arithmetic stack pop instructions. *)
        datatype fbinasp
	  = FYL2X   (* compute y * log_2 x; p. 327 *)
	  | FYL2XP1 (* compute y * log_2 (x + 1.0); p. 329 *)
	  | FPATAN  (* partial arctangent; p. 261 *)
	(* Floating-point constants. *)
	datatype fldc
	  = ONE  (* +1.0; p. 250 *) 
	  | ZERO (* +0.0; p. 250 *)
	  | PI   (* pi; p. 250 *)
	  | L2E  (* log_2 e; p. 250 *)
	  | LN2  (* log_e 2; p. 250 *)
	  | L2T  (* log_2 10; p. 250 *)
	  | LG2  (* log_10 2; p. 250 *)

	type t
      end

    structure Assembly :
      sig
	type t

	val toString : t -> string

	val comment : string -> t
	val isComment : t -> bool
	val label : Label.t -> t
	val instruction : Instruction.t -> t
	val instruction_nop : unit -> t
	val instruction_binal : {oper: Instruction.binal,
				 src: Operand.t,
				 dst: Operand.t,
				 size: Size.t} -> t
	val instruction_pmd : {oper: Instruction.md,
			       src: Operand.t,
			       dst: Operand.t,
			       size: Size.t} -> t
	val instruction_unal : {oper: Instruction.unal,
				dst: Operand.t,
				size: Size.t} -> t
	val instruction_sral : {oper: Instruction.sral,
				count: Operand.t,
				dst: Operand.t,
				size: Size.t} -> t
	val instruction_cmp : {src1: Operand.t,
			       src2: Operand.t,
			       size: Size.t} -> t
	val instruction_test : {src1: Operand.t,
				src2: Operand.t,
				size: Size.t} -> t
	val instruction_setcc : {condition: Instruction.condition,
				 dst: Operand.t,
				 size: Size.t} -> t
	val instruction_jmp : {target: Operand.t,
			       absolute: bool} -> t
	val instruction_jcc : {condition: Instruction.condition, 
			       target: Operand.t} -> t
	val instruction_call : {target: Operand.t,
				absolute: bool} -> t
	val instruction_ret : {src: Operand.t option} -> t
	val instruction_mov : {src: Operand.t,
			       dst: Operand.t,
			       size: Size.t} -> t
	val instruction_cmovcc : {condition: Instruction.condition,
				  src: Operand.t,
				  dst: Operand.t,
				  size: Size.t} -> t
	val instruction_xchg : {src: Operand.t, 
				dst: Operand.t,
				size: Size.t} -> t
	val instruction_ppush : {src: Operand.t,
				 base: Operand.t,
				 size: Size.t} -> t
	val instruction_ppop : {dst: Operand.t,
				base: Operand.t,
				size: Size.t} -> t
	val instruction_movx : {oper: Instruction.movx,
				src: Operand.t,
				srcsize: Size.t,
				dst: Operand.t,
				dstsize: Size.t} -> t
	val instruction_xvom : {src: Operand.t,
				srcsize: Size.t,
				dst: Operand.t,
				dstsize: Size.t} -> t
	val instruction_lea : {src: Operand.t,
			       dst: Operand.t,
			       size: Size.t} -> t
	val instruction_pfmov : {src: Operand.t,
				 dst: Operand.t,
				 size: Size.t} -> t
	val instruction_pfldc : {oper: Instruction.fldc,
				 dst: Operand.t,
				 size: Size.t} -> t
	val instruction_pfmovfi : {src: Operand.t,
				   srcsize: Size.t,
				   dst: Operand.t,
				   dstsize: Size.t} -> t
	val instruction_pfmovti : {src: Operand.t,
				   srcsize: Size.t,
				   dst: Operand.t,
				   dstsize: Size.t} -> t
	val instruction_pfcom : {src1: Operand.t,
				 src2: Operand.t,
				 size: Size.t} -> t
	val instruction_pfucom : {src1: Operand.t,
				  src2: Operand.t,
				  size: Size.t} -> t
	val instruction_pfbina : {oper: Instruction.fbina,
				  src: Operand.t,
				  dst: Operand.t,
				  size: Size.t} -> t
	val instruction_pfuna : {oper: Instruction.funa,
				 dst: Operand.t,
				 size: Size.t} -> t
	val instruction_pfptan : {dst: Operand.t,
				  size: Size.t} -> t
	val instruction_pfbinas : {oper: Instruction.fbinas,
				   src: Operand.t,
				   dst: Operand.t,
				   size: Size.t} -> t
	val instruction_pfbinasp : {oper: Instruction.fbinasp,
				    src: Operand.t,
				    dst: Operand.t,
				    size: Size.t} -> t
	val instruction_fldcw : {src: Operand.t} -> t
	val instruction_fstcw : {dst: Operand.t,
				 check: bool} -> t
	val instruction_fstsw : {dst: Operand.t,
				 check: bool} -> t
      end

    structure Entry : 
      sig
	structure FrameInfo :
	  sig
	    type t
	    val frameInfo : {size: int, 
			     frameLayoutsIndex: int} -> t
	  end

	type t
	val label : t -> Label.t

	val jump : {label: Label.t} -> t
	val func : {label: Label.t,
		    live: MemLoc.t list} -> t
	val cont : {label: Label.t,
		    live: MemLoc.t list,
		    frameInfo: FrameInfo.t} -> t
	val handler : {label: Label.t,
		       live: MemLoc.t list,
		       frameInfo: FrameInfo.t} -> t
	val runtime : {label: Label.t,
		       frameInfo: FrameInfo.t} -> t
      end

    structure ProfileInfo :
      sig
	type t
	val none : t
	val add : t * {profileLevel: int, profileName: string} -> t
      end

    structure Transfer :
      sig
	structure Cases :
	  sig
	    type 'a t
	      
	    val char : (char * 'a) list -> 'a t
	    val int : (int * 'a) list -> 'a t
	    val word : (word * 'a) list -> 'a t
	  end

	type t

	val goto : {target: Label.t} -> t
	val iff : {condition: Instruction.condition,
		   truee: Label.t,
		   falsee: Label.t} -> t
	val switch : {test: Operand.t,
		      cases: Label.t Cases.t,
		      default: Label.t} -> t
	val tail : {target: Label.t,
		    live: MemLoc.t list} -> t
	val nontail : {target: Label.t, 
		       live: MemLoc.t list,
		       return: Label.t,
		       handler: Label.t option,
		       size: int} -> t
	val return : {live: MemLoc.t list} -> t 
	val raisee : {live: MemLoc.t list} -> t
	val runtime : {target: Label.t,
		       args: (Operand.t * Size.t) list,
		       live: MemLoc.t list,
		       return: Label.t,
		       size: int} -> t
	val ccall : {target: Label.t,
		     args: (Operand.t * Size.t) list,
		     dst: (Operand.t * Size.t) option,
		     live: MemLoc.t list,
		     return: Label.t} -> t		       
      end

    structure Block :
      sig
	datatype t' = T' of {entry: Entry.t option,
			     profileInfo: ProfileInfo.t,
			     statements: Assembly.t list,
			     transfer: Transfer.t option}
	datatype t = T of {entry: Entry.t,
			   profileInfo: ProfileInfo.t,
			   statements: Assembly.t list,
			   transfer: Transfer.t}
	val compress : t' list -> t list
      end

    structure Chunk :
      sig
	datatype t = T of {blocks: Block.t list}
			   
      end
  end
