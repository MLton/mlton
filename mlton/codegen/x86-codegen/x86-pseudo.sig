(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature X86_PSEUDO =
  sig
    structure CFunction: C_FUNCTION
    structure CType: C_TYPE
    structure Label: ID
    structure RepType: REP_TYPE
    structure Runtime: RUNTIME
    structure WordSize: WORD_SIZE
    structure WordX: WORD_X
    sharing CFunction = RepType.CFunction
    sharing CType = RepType.CType
    sharing WordSize = CType.WordSize = WordX.WordSize

    val tracer : string -> ('a -> 'b) -> 
                 (('a -> 'b) * (unit -> unit))
    val tracerTop : string -> ('a -> 'b) -> 
                    (('a -> 'b) * (unit -> unit))

    structure Size :
      sig
        datatype class = INT | FLT | FPI
        datatype t 
          = BYTE | WORD | LONG 
          | SNGL | DBLE | EXTD
          | FPIS | FPIL | FPIQ
        val fromBytes : int -> t
        val toBytes : t -> int
        val fromCType : CType.t -> t vector
        val class : t -> class
        val eq : t * t -> bool
        val lt : t * t -> bool
      end

    structure Immediate :
      sig
        type t

        val word : WordX.t -> t
        val int' : int * WordSize.t -> t
        val int : int -> t
        val zero : t
        val label : Label.t -> t
        val labelPlusWord : Label.t * WordX.t -> t
        val labelPlusInt : Label.t * int -> t
      end

    structure Scale :
      sig
        datatype t = One | Two | Four | Eight
        val fromBytes : int -> t
        val fromCType : CType.t -> t
      end

    structure MemLoc :
      sig
        structure Class :
          sig
            type t
            val new : {name: string} -> t
            val Temp : t
            val StaticTemp : t
            val CStack : t
            val Code : t

            val eq : t * t -> bool
          end

        type t
        val layout : t -> Layout.t

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
        val shift : {origin: t,
                     disp: Immediate.t,
                     scale: Scale.t,
                     size: Size.t} -> t

        val class : t -> Class.t
        val compare : t * t -> order
        (*
         * Static memory locations
         *)
        val makeContents : {base: Immediate.t,
                            size: Size.t,
                            class: Class.t} -> t
      end

    structure ClassSet : SET
    sharing type ClassSet.Element.t = MemLoc.Class.t
    structure MemLocSet : SET
    sharing type MemLocSet.Element.t = MemLoc.t

    structure Operand : 
      sig
        type t

        val layout : t -> Layout.t
        val toString : t -> string

        val immediate : Immediate.t -> t
        val immediate_word : WordX.t -> t
        val immediate_int' : int * WordSize.t -> t
        val immediate_int : int -> t
        val immediate_zero : t
        val immediate_label : Label.t -> t
        val deImmediate : t -> Immediate.t option
        val label : Label.t -> t
        val deLabel : t -> Label.t option
        val memloc : MemLoc.t -> t
        val memloc_label : Label.t -> t
        val deMemloc : t -> MemLoc.t option

        val size : t -> Size.t option
        val eq : t * t -> bool
        val mayAlias : t * t -> bool
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

    structure PseudoOp :
      sig
        type t

        val toString : t -> string

        val data : unit -> t
        val text : unit -> t
        val p2align : Immediate.t * Immediate.t option * Immediate.t option -> t
        val byte : Immediate.t list -> t
        val word : Immediate.t list -> t
        val long : Immediate.t list -> t
      end

    structure Assembly :
      sig
        type t

        val toString : t -> string

        val comment : string -> t
        val isComment : t -> bool
        val pseudoop : PseudoOp.t -> t
        val pseudoop_data : unit -> t
        val pseudoop_text : unit -> t
        val pseudoop_symbol_stub : unit -> t
        val pseudoop_non_lazy_symbol_pointer : unit -> t
        val pseudoop_p2align : Immediate.t * Immediate.t option * Immediate.t option -> t
        val pseudoop_byte : Immediate.t list -> t
        val pseudoop_global: Label.t -> t
        val pseudoop_hidden : Label.t -> t
        val pseudoop_indirect_symbol : Label.t -> t
        val pseudoop_word : Immediate.t list -> t
        val pseudoop_long : Immediate.t list -> t
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
        val instruction_imul2 : {src: Operand.t,
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
        val instruction_pfmovx : {src: Operand.t,
                                  dst: Operand.t,
                                  srcsize: Size.t,
                                  dstsize: Size.t} -> t
        val instruction_pfxvom : {src: Operand.t,
                                  dst: Operand.t,
                                  srcsize: Size.t,
                                  dstsize: Size.t} -> t
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

    structure FrameInfo:
       sig
          datatype t = T of {size: int, 
                             frameLayoutsIndex: int}
       end

    structure Entry:
      sig
        type t

        val cont: {label: Label.t,
                   live: MemLocSet.t,
                   frameInfo: FrameInfo.t} -> t
        val creturn: {dsts: (Operand.t * Size.t) vector,
                      frameInfo: FrameInfo.t option,
                      func: RepType.t CFunction.t,
                      label: Label.t} -> t
        val func: {label: Label.t,
                   live: MemLocSet.t} -> t
        val handler: {frameInfo: FrameInfo.t,
                      label: Label.t,
                      live: MemLocSet.t} -> t
        val jump: {label: Label.t} -> t
        val label: t -> Label.t
      end

    structure Transfer :
      sig
        structure Cases :
          sig
            type 'a t

            val word : (WordX.t * 'a) list -> 'a t
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
                    live: MemLocSet.t} -> t
        val nontail : {target: Label.t, 
                       live: MemLocSet.t,
                       return: Label.t,
                       handler: Label.t option,
                       size: int} -> t
        val return : {live: MemLocSet.t} -> t 
        val raisee : {live: MemLocSet.t} -> t
        val ccall : {args: (Operand.t * Size.t) list,
                     frameInfo: FrameInfo.t option,
                     func: RepType.t CFunction.t,
                     return: Label.t option} -> t
      end

    structure ProfileLabel :
      sig
        type t
      end

    structure Block :
      sig       
        type t'
        val mkBlock': {entry: Entry.t option,
                       statements: Assembly.t list,
                       transfer: Transfer.t option} -> t'
        val mkProfileBlock': {profileLabel: ProfileLabel.t} -> t'
        val printBlock' : t' -> unit

        type t
        val printBlock : t -> unit

        val compress: t' list -> t list
      end

    structure Chunk :
      sig
        datatype t = T of {data: Assembly.t list, blocks: Block.t list}

      end
  end

functor x86PseudoCheck(structure S : X86) : X86_PSEUDO = S
