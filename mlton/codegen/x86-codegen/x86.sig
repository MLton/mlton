(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature X86_STRUCTS =
  sig
    structure CFunction: C_FUNCTION
    structure CType: C_TYPE
    structure Label: ID
    structure ProfileLabel: PROFILE_LABEL 
    structure RepType: REP_TYPE
    structure Runtime: RUNTIME
    structure WordSize: WORD_SIZE
    structure WordX: WORD_X
    sharing CFunction = RepType.CFunction
    sharing CType = RepType.CType
    sharing WordSize = CType.WordSize = WordX.WordSize
  end

signature X86 =
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

        val toString : t -> string
        val fromBytes : int -> t
        val toBytes : t -> int
        val fromCType : CType.t -> t vector
        val class : t -> class
        val toFPI : t -> t
        val eq : t * t -> bool
        val lt : t * t -> bool
      end

    structure Register :
      sig
        datatype reg
          = EAX | EBX | ECX | EDX | EDI | ESI | EBP | ESP
        val allReg : reg list

        datatype part
          = E | X | L | H

        datatype t = T of {reg: reg, part: part}
        val all : t list

        val toString : t -> string
        val size : t -> Size.t
        val eq : t * t -> bool
        val valid  : t -> bool
        val coincide : t * t -> bool
        val coincident' : reg -> t list

(*
        val return : Size.t -> t
*)
        val eax : t
        val ebx : t
        val ecx : t
        val edx : t
        val al : t
        val bl : t
        val cl : t
        val dl : t
        val edi : t
        val esi : t
        val esp : t
        val ebp : t

        val registers : Size.t -> t list
        val baseRegisters : t list
        val indexRegisters : t list
        val callerSaveRegisters : t list
        val calleeSaveRegisters : t list

        val withLowPart : Size.t * Size.t -> t list
        val lowPartOf : t * Size.t -> t
      end

    structure FltRegister :
      sig
        datatype t = T of int
        val toString : t -> string
        val eq: t * t -> bool
(*
        val return : t
*)
        val top : t
        val one : t
        val total : int
        val push : t -> t
        val pop : t -> t
        val id : t -> t
      end

    structure Immediate :
      sig
        type t 

        datatype u
          = Word of WordX.t
          | Label of Label.t
          | LabelPlusWord of Label.t * WordX.t

        val word : WordX.t -> t
        val int' : int * WordSize.t -> t
        val int : int -> t
        val zero : t
        val label : Label.t -> t
        val labelPlusWord : Label.t * WordX.t -> t
        val labelPlusInt : Label.t * int -> t

        val deLabel : t -> Label.t option
        val destruct : t -> u
        val clearAll : unit -> unit

        val eval : t -> WordX.t option
        val isZero : t -> bool
        val eq : t * t -> bool
    end

    structure Scale : 
      sig
        datatype t 
          = One | Two | Four | Eight
        val eq : t * t -> bool
        val toWordX : t -> WordX.t
        val toImmediate : t -> Immediate.t
        val fromBytes : int -> t
        val fromCType : CType.t -> t
      end

    structure Address :
      sig
        datatype t = T of {disp: Immediate.t option,
                           base: Register.t option,
                           index: Register.t option,
                           scale: Scale.t option}
      end

    structure MemLoc :
      sig
        structure Class :
          sig
            type t

            val toString : t -> string

            val new : {name: string} -> t
            val Temp : t
            val StaticTemp : t
            val CStack : t
            val Code : t

            val eq : t * t -> bool
            val compare : t * t -> order
          end

        type t

        datatype u
          = U of {immBase: Immediate.t option,
                  memBase: t option,
                  immIndex: Immediate.t option,
                  memIndex: t option,
                  scale: Scale.t,
                  size: Size.t,
                  class: Class.t}

        val layout : t -> Layout.t
        val toString : t -> string

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
        val destruct : t -> u
        val clearAll : unit -> unit

        val size : t -> Size.t
        val class : t -> Class.t
        val eq : t * t -> bool
        val compare : t * t -> order

        val utilized : t -> t list
        val mayAlias : t * t -> bool
        val mayAliasOrd : t * t -> order option

        val replace : (t -> t) -> t -> t

        (*
         * Static memory locations
         *)
        val makeContents : {base: Immediate.t,
                            size: Size.t,
                            class: Class.t} -> t
        (* CReturn locations *)
(*
        val cReturnTempContent : Size.t -> t
        val cReturnTempContents : CFunction.CType.t -> t list
*)
    end

    structure ClassSet : SET
    sharing type ClassSet.Element.t = MemLoc.Class.t
    structure MemLocSet : SET
    sharing type MemLocSet.Element.t = MemLoc.t

    structure Operand :
      sig
        datatype t
          = Register of Register.t
          | FltRegister of FltRegister.t
          | Immediate of Immediate.t
          | Label of Label.t
          | Address of Address.t
          | MemLoc of MemLoc.t

        val layout : t -> Layout.t
        val toString : t -> string

        val register : Register.t -> t
        val deRegister : t -> Register.t option
        val fltregister : FltRegister.t -> t
        val deFltregister : t -> FltRegister.t option
        val immediate : Immediate.t -> t
        val immediate_word : WordX.t -> t
        val immediate_int' : int * WordSize.t -> t
        val immediate_int : int -> t
        val immediate_zero : t
        val immediate_label : Label.t -> t
        val deImmediate : t -> Immediate.t option
        val label : Label.t -> t
        val deLabel : t -> Label.t option
        val address : Address.t -> t
        val memloc : MemLoc.t -> t
        val memloc_label : Label.t -> t
        val deMemloc : t -> MemLoc.t option

        val size : t -> Size.t option
        val eq : t * t -> bool
        val mayAlias : t * t -> bool

        val cReturnTemps: RepType.t -> {src: t, dst: MemLoc.t} list
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
        (* Integer unary arithmetic/logic instructions. *)
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

        (* x86 Instructions.
         * src operands are not changed by the instruction.
         * dst operands are changed by the instruction.
         *)
        datatype t
          (* No operation *)
          = NOP
          (* Halt *)
          | HLT
          (* Integer binary arithmetic(w/o mult & div)/logic instructions.
           *)
          | BinAL of {oper: binal,
                      src: Operand.t,
                      dst: Operand.t,
                      size: Size.t}
          (* Psuedo integer multiplication and division.
           *)
          | pMD of {oper: md,
                    src: Operand.t,
                    dst: Operand.t,
                    size: Size.t}
          (* Integer multiplication and division. 
           *)
          | MD of {oper: md,
                   src: Operand.t,
                   size: Size.t}
          (* Integer signed/unsiged multiplication (two operand form); p. 335
           *)
          | IMUL2 of {src: Operand.t,
                      dst: Operand.t, 
                      size: Size.t}
          (* Integer unary arithmetic/logic instructions.
           *)
          | UnAL of {oper: unal,
                     dst: Operand.t,
                     size: Size.t}
          (* Integer shift/rotate arithmetic/logic instructions.
           *)
          | SRAL of {oper: sral,
                     count: Operand.t,
                     dst: Operand.t,
                     size: Size.t}
          (* Arithmetic compare.
           *)
          | CMP  of {src1: Operand.t,
                     src2: Operand.t,
                     size: Size.t}
          (* Logical compare.
           *)
          | TEST of {src1: Operand.t,
                     src2: Operand.t,
                     size: Size.t}
          (* Set byte on condition.
           *)
          | SETcc of {condition: condition,
                      dst: Operand.t,
                      size: Size.t}
          (* Jump; p. 373
           *)
          | JMP of {target: Operand.t,
                    absolute: bool}
          (* Jump if condition is met.
           *)
          | Jcc of {condition: condition,
                    target: Operand.t}
          (* Call procedure.
           *)
          | CALL of {target: Operand.t,
                     absolute: bool}
          (* Return from procedure.
           *)
          | RET of {src: Operand.t option}
          (* Move.
           *)
          | MOV of {src: Operand.t,
                    dst: Operand.t,
                    size: Size.t}
          (* Conditional move.
           *)
          | CMOVcc of {condition: condition,
                       src: Operand.t,
                       dst: Operand.t,
                       size: Size.t}
          (* Exchange register/memory with register.
           *)
          | XCHG of {src: Operand.t,
                     dst: Operand.t,
                     size: Size.t}
          (* Pseudo push a value onto a stack.
           *)
          | pPUSH of {src: Operand.t,
                      base: Operand.t,
                      size: Size.t}
          (* Pseudo pop a value from a stack.
           *)
          | pPOP of {dst: Operand.t,
                     base: Operand.t,
                     size: Size.t}
          (* Push a value onto the stack.
           *)
          | PUSH of {src: Operand.t,
                     size: Size.t}
          (* Pop a value from the stack.
           *)
          | POP of {dst: Operand.t,
                    size: Size.t}
          (* Convert X to 2X with sign extension.
           *)
          | CX of {size: Size.t}
          (* Move with extention.
           *)
          | MOVX of {oper: movx,
                     src: Operand.t,
                     srcsize: Size.t,
                     dst: Operand.t,
                     dstsize: Size.t}
          (* Move with contraction.
           *)
          | XVOM of {src: Operand.t,
                     srcsize: Size.t,
                     dst: Operand.t,
                     dstsize: Size.t}
          (* Load effective address.
           *)
          | LEA of {src: Operand.t,
                    dst: Operand.t,
                    size: Size.t}
          (* Pseudo floating-point move.
           *)
          | pFMOV of {src: Operand.t,
                      dst: Operand.t,
                      size: Size.t}
          (* Pseudo floating-point move with extension.
           *)
          | pFMOVX of {src: Operand.t,
                       dst: Operand.t,
                       srcsize: Size.t,
                       dstsize: Size.t}
          (* Pseudo floating-point move with contraction.
           *)
          | pFXVOM of {src: Operand.t,
                       dst: Operand.t,
                       srcsize: Size.t,
                       dstsize: Size.t}
          (* Pseudo floating-point load constant.
           *)
          | pFLDC of {oper: fldc,
                      dst: Operand.t,
                      size: Size.t}
          (* Pseudo floating-point move from integer.
           *)
          | pFMOVFI of {src: Operand.t,
                        dst: Operand.t,
                        srcsize: Size.t,
                        dstsize: Size.t}
          (* Pseudo floating-point move to integer.
           *)
          | pFMOVTI of {src: Operand.t,
                        dst: Operand.t,
                        srcsize: Size.t,
                        dstsize: Size.t}
          (* Pseudo floating-point compare.
           *)
          | pFCOM of {src1: Operand.t,
                      src2: Operand.t,
                      size: Size.t}
          (* Pseudo floating-point unordered compare.
           *)
          | pFUCOM of {src1: Operand.t,
                       src2: Operand.t,
                       size: Size.t}
          (* Pseudo floating-point binary arithmetic instructions.
           *)
          | pFBinA of {oper: fbina, 
                       src: Operand.t, 
                       dst: Operand.t,
                       size: Size.t}
          (* Pseudo floating-point unary arithmetic instructions.
           *)
          | pFUnA of {oper: funa,
                      dst: Operand.t,
                      size: Size.t}
          (* Pseudo floating-point partial tangetn instruction.
           *)
          | pFPTAN of {dst: Operand.t,
                       size: Size.t}
          (* Pseudo floating-point binary arithmetic stack instructions.
           *)
          | pFBinAS of {oper: fbinas, 
                        src: Operand.t, 
                        dst: Operand.t,
                        size: Size.t}
          (* Pseudo floating-point binary arithmetic stack pop instructions.
           *)
          | pFBinASP of {oper: fbinasp, 
                         src: Operand.t, 
                         dst: Operand.t,
                         size: Size.t}
          (* Floating-point load real.
           *)
          | FLD of {src: Operand.t,
                    size: Size.t}
          (* Floating-point store real.
           *)
          | FST of {dst: Operand.t,
                    size: Size.t,
                    pop: bool}
          (* Floating-point load integer.
           *)
          | FILD of {src: Operand.t,
                     size: Size.t}
          (* Floating-point store integer.
           *)
          | FIST of {dst: Operand.t,
                     size: Size.t,
                     pop: bool}
          (* Floating-point exchange.
           *)
          | FXCH of {src: Operand.t}
          (* Floating-point load constant.
           *)
          | FLDC of {oper: fldc}
          (* Floating-point load control word.
           *)
          | FLDCW of {src: Operand.t}
          (* Floating-point store control word.
           *)
          | FSTCW of {dst: Operand.t,
                      check: bool}
          (* Floating-point store status word.
           *)
          | FSTSW of {dst: Operand.t,
                      check: bool}
          (* Floating-point compare.
           *)
          | FCOM of {src: Operand.t,
                     size: Size.t,
                     pop: bool,
                     pop': bool}
          (* Floating-point unordered compare.
           *)
          | FUCOM of {src: Operand.t,
                      pop: bool,
                      pop': bool}
          (* Floating-point binary arithmetic instructions.
           *)
          | FBinA of {oper: fbina, 
                      src: Operand.t,
                      dst: Operand.t,
                      size: Size.t,
                      pop: bool}
          (* Floating-point unary arithmetic instructions.
           *)
          | FUnA of {oper: funa}
          (* Floating-point partial tangent instruction.
           *)
          | FPTAN
          (* Floating-point binary arithmetic stack instructions.
           *)
          | FBinAS of {oper: fbinas}
          (* Floating-point binary arithmetic stack pop instructions.
           *)
          | FBinASP of {oper: fbinasp}

        val toString : t -> string
        val uses_defs_kills : t -> {uses: Operand.t list,
                                    defs: Operand.t list,
                                    kills: Operand.t list}
        val hints : t -> (MemLoc.t * Register.t) list
        val srcs_dsts : t -> {srcs: Operand.t list option, 
                              dsts: Operand.t list option}
        val replace : ({use: bool, def: bool} -> Operand.t -> Operand.t) -> 
                      t -> t
      end

    structure Directive :
      sig
        structure Id :
          sig
            type t
            val new : unit -> t
            val plist : t -> PropertyList.t
          end

        datatype t
          (* Transfers *)
            (* Assert that a memloc is in a register with properties;
             * used at top of basic blocks to establish passing convention.
             *)
          = Assume of {assumes: {register: Register.t, 
                                 memloc: MemLoc.t, 
                                 weight: int,
                                 sync: bool,
                                 reserve: bool} list}
          | FltAssume of {assumes: {memloc: MemLoc.t, 
                                    weight: int,
                                    sync: bool} list}
            (* Ensure that memloc is in the register, possibly reserved; 
             * used at bot of basic blocks to establish passing convention,
             * also used before C calls to set-up %esp.
             *)
          | Cache of {caches: {register: Register.t,
                               memloc: MemLoc.t,
                               reserve: bool} list}
          | FltCache of {caches: {memloc: MemLoc.t} list}
            (* Reset the register allocation;
             * used at bot of basic blocks that fall-thru
             * to a block with multiple incoming paths of control.
             *)
          | Reset
            (* Ensure that memlocs are commited to memory;
             * used at bot of basic blocks to establish passing conventions
             *)
          | Force of {commit_memlocs: MemLocSet.t,
                      commit_classes: ClassSet.t,
                      remove_memlocs: MemLocSet.t,
                      remove_classes: ClassSet.t,
                      dead_memlocs: MemLocSet.t,
                      dead_classes: ClassSet.t}
          (* C calls *)
            (* Prepare for a C call; i.e., clear all caller save registers;
             * also, clear the flt. register stack;
             * used before C calls.
             *)
          | CCall
          (* Assert the return value;
           * used after C calls.
           *)
          | Return of {returns: {src:Operand.t, dst: MemLoc.t} list}
          (* Misc. *)
            (* Assert that the register is not free for the allocator;
             * used ???
             *)
          | Reserve of {registers: Register.t list}
            (* Assert that the register is free for the allocator;
             * used to free registers at fall-thru;
             * also used after C calls to free %esp.
             *)
          | Unreserve of {registers: Register.t list}
            (* Clear the floating point stack;
             * used at bot of basic blocks to establish passing convention,
             *)
          | ClearFlt
            (* Save the register allocation in id and
             *  assert that live are used at this point;
             * used at bot of basic blocks to delay establishment
             *  of passing convention to compensation block
             *)
          | SaveRegAlloc of {live: MemLocSet.t,
                             id: Id.t}
            (* Restore the register allocation from id and
             *  remove anything tracked that is not live;
             * used at bot of basic blocks to delay establishment
             *  of passing convention to compensation block
             *)
          | RestoreRegAlloc of {live: MemLocSet.t,
                                id: Id.t}

        val toString : t -> string
        val uses_defs_kills : t -> {uses: Operand.t list, 
                                    defs: Operand.t list, 
                                    kills: Operand.t list}
        val hints : t -> (MemLoc.t * Register.t) list
        val replace : ({use: bool, def: bool} -> Operand.t -> Operand.t) ->
                      t -> t
        val assume : {assumes: {register: Register.t,
                                memloc: MemLoc.t,
                                weight: int,
                                sync: bool,
                                reserve: bool} list} -> t
        val fltassume : {assumes: {memloc: MemLoc.t,
                                   weight: int,
                                   sync: bool} list} -> t
        val cache : {caches: {register: Register.t,
                              memloc: MemLoc.t,
                              reserve: bool} list} -> t
        val fltcache : {caches: {memloc: MemLoc.t} list} -> t
        val reset : unit -> t
        val force : {commit_memlocs: MemLocSet.t,
                     commit_classes: ClassSet.t,
                     remove_memlocs: MemLocSet.t,
                     remove_classes: ClassSet.t,
                     dead_memlocs: MemLocSet.t,
                     dead_classes: ClassSet.t} -> t
        val ccall : unit -> t
        val return : {returns: {src: Operand.t, dst: MemLoc.t} list} -> t
        val reserve : {registers: Register.t list} -> t
        val unreserve : {registers: Register.t list} -> t
        val clearflt : unit -> t
        val saveregalloc : {live: MemLocSet.t,
                            id: Id.t} -> t
        val restoreregalloc : {live: MemLocSet.t,
                               id: Id.t} -> t
    end

    structure PseudoOp :
      sig
        datatype t
          = Data
          | Text
          | SymbolStub
          | NonLazySymbolPointer
          | Balign of Immediate.t * Immediate.t option * Immediate.t option
          | P2align of Immediate.t * Immediate.t option * Immediate.t option
          | Space of Immediate.t * Immediate.t
          | Byte of Immediate.t list
          | Word of Immediate.t list
          | Long of Immediate.t list
          | String of string list
          | Global of Label.t
          | Hidden of Label.t
          | IndirectSymbol of Label.t
          | Local of Label.t
          | Comm of Label.t * Immediate.t * Immediate.t option

        val toString : t -> string

        val data : unit -> t
        val text : unit -> t
        val symbol_stub : unit -> t
        val non_lazy_symbol_pointer : unit -> t
        val balign : Immediate.t * Immediate.t option * Immediate.t option -> t
        val p2align : Immediate.t * Immediate.t option * Immediate.t option -> t
        val space : Immediate.t * Immediate.t -> t
        val byte : Immediate.t list -> t
        val word : Immediate.t list -> t
        val long : Immediate.t list -> t
        val string : string list -> t
        val global : Label.t -> t
        val hidden : Label.t -> t
        val indirect_symbol : Label.t -> t
        val locall : Label.t -> t
        val comm : Label.t * Immediate.t * Immediate.t option -> t
      end

    structure Assembly :
      sig
        datatype t 
          = Comment of string
          | Directive of Directive.t
          | PseudoOp of PseudoOp.t
          | Label of Label.t
          | Instruction of Instruction.t

        val layout : t -> Layout.t
        val toString : t -> string
        val uses_defs_kills : t -> {uses: Operand.t list, 
                                    defs: Operand.t list,
                                    kills: Operand.t list}
        val hints : t -> (MemLoc.t * Register.t) list
        val replace : ({use: bool, def: bool} -> Operand.t -> Operand.t) ->
                      t -> t

        val comment : string -> t
        val isComment : t -> bool
        val directive : Directive.t -> t
        val directive_assume : {assumes: {register: Register.t,
                                          memloc: MemLoc.t,
                                          weight: int,
                                          sync: bool,
                                          reserve: bool} list} -> t
        val directive_fltassume : {assumes: {memloc: MemLoc.t,
                                             weight: int,
                                             sync: bool} list} -> t
        val directive_cache : {caches: {register: Register.t,
                                        memloc: MemLoc.t,
                                        reserve: bool} list} -> t
        val directive_fltcache : {caches: {memloc: MemLoc.t} list} -> t
        val directive_reset : unit -> t
        val directive_force : {commit_memlocs: MemLocSet.t,
                               commit_classes: ClassSet.t,
                               remove_memlocs: MemLocSet.t,
                               remove_classes: ClassSet.t,
                               dead_memlocs: MemLocSet.t,
                               dead_classes: ClassSet.t} -> t
        val directive_ccall : unit -> t
        val directive_return : {returns: {src: Operand.t, dst: MemLoc.t} list} -> t
        val directive_reserve : {registers: Register.t list} -> t
        val directive_unreserve : {registers: Register.t list} -> t
        val directive_saveregalloc : {live: MemLocSet.t,
                                      id: Directive.Id.t} -> t
        val directive_restoreregalloc : {live: MemLocSet.t,
                                         id: Directive.Id.t} -> t
        val directive_clearflt : unit -> t
        val pseudoop : PseudoOp.t -> t
        val pseudoop_data : unit -> t
        val pseudoop_text : unit -> t
        val pseudoop_symbol_stub : unit -> t
        val pseudoop_non_lazy_symbol_pointer : unit -> t
        val pseudoop_balign : Immediate.t * Immediate.t option * Immediate.t option ->t 
        val pseudoop_p2align : Immediate.t * Immediate.t option * Immediate.t option -> t
        val pseudoop_space : Immediate.t * Immediate.t -> t
        val pseudoop_byte : Immediate.t list -> t
        val pseudoop_word : Immediate.t list -> t
        val pseudoop_long : Immediate.t list -> t
        val pseudoop_string : string list -> t
        val pseudoop_global : Label.t -> t
        val pseudoop_hidden : Label.t -> t
        val pseudoop_indirect_symbol : Label.t -> t
        val pseudoop_local : Label.t -> t
        val pseudoop_comm : Label.t * Immediate.t * Immediate.t option -> t
        val label : Label.t -> t
        val instruction : Instruction.t -> t
        val instruction_nop : unit -> t
        val instruction_hlt : unit -> t
        val instruction_binal : {oper: Instruction.binal, 
                                 src: Operand.t,
                                 dst: Operand.t,
                                 size: Size.t} -> t
        val instruction_pmd : {oper: Instruction.md,
                               src: Operand.t,
                               dst: Operand.t,
                               size: Size.t} -> t
        val instruction_md : {oper: Instruction.md,
                              src: Operand.t,
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
        val instruction_push : {src: Operand.t,
                                size: Size.t} -> t
        val instruction_pop : {dst: Operand.t,
                               size: Size.t} -> t
        val instruction_cx : {size: Size.t} -> t
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
        val instruction_fld : {src: Operand.t,
                               size: Size.t} -> t
        val instruction_fst : {dst: Operand.t,
                               size: Size.t,
                               pop: bool} -> t
        val instruction_fild : {src: Operand.t,
                                size: Size.t} -> t
        val instruction_fist : {dst: Operand.t,
                                size: Size.t,
                                pop: bool} -> t
        val instruction_fxch : {src: Operand.t} -> t
        val instruction_fldc : {oper: Instruction.fldc} -> t
        val instruction_fldcw : {src: Operand.t} -> t
        val instruction_fstcw : {dst: Operand.t,
                                 check: bool} -> t
        val instruction_fstsw : {dst: Operand.t,
                                 check: bool} -> t
        val instruction_fcom : {src: Operand.t,
                                size: Size.t,
                                pop: bool,
                                pop': bool} -> t
        val instruction_fucom : {src: Operand.t,
                                 pop: bool,
                                 pop': bool} -> t
        val instruction_fbina : {oper: Instruction.fbina,
                                 src: Operand.t,
                                 dst: Operand.t,
                                 size: Size.t,
                                 pop: bool} -> t
        val instruction_funa : {oper: Instruction.funa} -> t
        val instruction_fptan : unit -> t
        val instruction_fbinas : {oper: Instruction.fbinas} -> t
        val instruction_fbinasp : {oper: Instruction.fbinasp} -> t
    end

    structure FrameInfo:
       sig
          datatype t = T of {size: int, 
                             frameLayoutsIndex: int}
       end

    structure Entry:
      sig
        datatype t
          = Jump of {label: Label.t}
          | Func of {label: Label.t,
                     live: MemLocSet.t}
          | Cont of {label: Label.t,
                     live: MemLocSet.t,
                     frameInfo: FrameInfo.t}
          | Handler of {frameInfo: FrameInfo.t,
                        label: Label.t,
                        live: MemLocSet.t}
          | CReturn of {dsts: (Operand.t * Size.t) vector,
                        frameInfo: FrameInfo.t option,
                        func: RepType.t CFunction.t,
                        label: Label.t}

        val cont : {label: Label.t,
                    live: MemLocSet.t,
                    frameInfo: FrameInfo.t} -> t
        val creturn: {dsts: (Operand.t * Size.t) vector,
                      frameInfo: FrameInfo.t option,
                      func: RepType.t CFunction.t,
                      label: Label.t}  -> t
        val func : {label: Label.t,
                    live: MemLocSet.t} -> t
        val handler : {frameInfo: FrameInfo.t,
                       label: Label.t,
                       live: MemLocSet.t} -> t
        val isFunc : t -> bool
        val jump : {label: Label.t} -> t
        val label : t -> Label.t
        val live : t -> MemLocSet.t
        val toString : t -> string
        val uses_defs_kills : t -> {uses: Operand.t list, 
                                    defs: Operand.t list,
                                    kills: Operand.t list}
      end

    structure Transfer :
      sig
        structure Cases :
          sig
            datatype 'a t = Word of (WordX.t * 'a) list

            val word : (WordX.t * 'a) list -> 'a t

            val isEmpty : 'a t -> bool
            val isSingle : 'a t -> bool
            val extract : 'a t * (WordX.t * 'a -> 'b) -> 'b
            val count : 'a t * ('a -> bool) -> int
            val keepAll : 'a t * (WordX.t * 'a -> bool) -> 'a t
            val forall : 'a t * (WordX.t * 'a -> bool) -> bool
            val foreach : 'a t * (WordX.t * 'a -> unit) -> unit
            val map : 'a t * (WordX.t * 'a -> 'b) -> 'b t
            val mapToList : 'a t * (WordX.t * 'a -> 'b) -> 'b list
          end

        datatype t
          = Goto of {target: Label.t}
          | Iff of {condition: Instruction.condition,
                    truee: Label.t,
                    falsee: Label.t}
          | Switch of {test: Operand.t,
                       cases: Label.t Cases.t,
                       default: Label.t}
          | Tail of {target: Label.t,
                     live: MemLocSet.t}
          | NonTail of {target: Label.t,
                        live: MemLocSet.t,
                        return: Label.t,
                        handler: Label.t option,
                        size: int}
          | Return of {live: MemLocSet.t}
          | Raise of {live: MemLocSet.t}
          | CCall of {args: (Operand.t * Size.t) list,
                      frameInfo: FrameInfo.t option,
                      func: RepType.t CFunction.t,
                      return: Label.t option}

        val toString : t -> string

        val uses_defs_kills : t -> {uses: Operand.t list, 
                                    defs: Operand.t list,
                                    kills: Operand.t list}
        val nearTargets : t -> Label.t list
        val live : t -> MemLocSet.t
        val replace : ({use: bool, def: bool} -> Operand.t -> Operand.t) -> 
                      t -> t

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
        val ccall: {args: (Operand.t * Size.t) list,
                    frameInfo: FrameInfo.t option,
                    func: RepType.t CFunction.t,
                    return: Label.t option} -> t 
      end

    structure ProfileLabel :
      sig
        include PROFILE_LABEL
        val toAssembly : t -> Assembly.t list
        val toAssemblyOpt : t option -> Assembly.t list
      end

    structure Block :
      sig
        datatype t' = T' of {entry: Entry.t option,
                             profileLabel: ProfileLabel.t option,
                             statements: Assembly.t list,
                             transfer: Transfer.t option}
        val mkBlock': {entry: Entry.t option,
                       statements: Assembly.t list,
                       transfer: Transfer.t option} -> t'
        val mkProfileBlock': {profileLabel: ProfileLabel.t} -> t'
        val printBlock' : t' -> unit

        datatype t = T of {entry: Entry.t,
                           profileLabel: ProfileLabel.t option,
                           statements: Assembly.t list,
                           transfer: Transfer.t}
        val printBlock : t -> unit

        val compress : t' list -> t list
      end

    structure Chunk :
      sig
        datatype t = T of {data: Assembly.t list,
                           blocks: Block.t list}
      end
end
