(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature AMD64_STRUCTS =
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

signature AMD64 =
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
        datatype class = INT | FLT

        datatype t 
          = BYTE | WORD | LONG | QUAD
          | SNGL | DBLE 

        val toString : t -> string
        val fromBytes : int -> t
        val toBytes : t -> int
        val fromCType : CType.t -> t vector
        val class : t -> class
        val eq : t * t -> bool
        val lt : t * t -> bool
      end

    structure Register :
      sig
        datatype reg
          = RAX | RBX | RCX | RDX | RDI | RSI | RBP | RSP
          | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15 | RIP
        val allReg : reg list

        datatype part
          = R | E | X | L

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
        val rax : t
        val eax : t
        val al : t
        val rbx : t
        val ebx : t
        val bl : t
        val rcx : t
        val ecx : t 
        val cl : t
        val rdx : t
        val edx : t
        val dl : t
        val rdi : t
        val rsi : t
        val rsp : t
        val rbp : t
        val r8 : t
        val r8w : t
        val r9 : t
        val r9w : t
        val r10 : t
        val r10w : t
        val r11 : t
        val r11w : t
        val r12 : t
        val r12w : t
        val r13 : t
        val r13w : t
        val r14 : t
        val r14w : t
        val r15 : t
        val r15w : t
        val rip : t

        val registers : Size.t -> t list
        val baseRegisters : t list
        val indexRegisters : t list
        val callerSaveRegisters : t list
        val calleeSaveRegisters : t list

        val withLowPart : Size.t * Size.t -> t list
        val lowPartOf : t * Size.t -> t
      end

    structure XmmRegister :
      sig
        datatype reg
          = XMM0 | XMM1 | XMM2 | XMM3 | XMM4 | XMM5 | XMM6 | XMM7 
          | XMM8 | XMM9 | XMM10 | XMM11 | XMM12 | XMM13 | XMM14 | XMM15 
        val allReg : reg list

        datatype part
          = D | S

        datatype t = T of {reg: reg, part: part}
        val all : t list

        val toString : t -> string
        val size : t -> Size.t
        val eq : t * t -> bool
        val valid  : t -> bool
        val coincide : t * t -> bool
        val coincident' : reg -> t list
        val coincident : t -> t list

(*
        val return : Size.t -> t
*)

        val xmm0D : t
        val xmm0S : t
        val xmm1D : t
        val xmm1S : t
        val xmm2D : t
        val xmm2S : t
        val xmm3D : t
        val xmm3S : t
        val xmm4D : t
        val xmm4S : t
        val xmm5D : t
        val xmm5S : t
        val xmm6D : t
        val xmm6S : t
        val xmm7D : t
        val xmm7S : t
        val xmm8D : t
        val xmm8S : t
        val xmm9D : t
        val xmm9S : t
        val xmm10D : t
        val xmm10S : t
        val xmm11D : t
        val xmm11S : t
        val xmm12D : t
        val xmm12S : t
        val xmm13D : t
        val xmm13S : t
        val xmm14D : t
        val xmm14S : t
        val xmm15D : t
        val xmm15S : t

        val registers : Size.t -> t list
        val callerSaveRegisters : t list
        val calleeSaveRegisters : t list
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
            val CArg : t
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
          | XmmRegister of XmmRegister.t
          | Immediate of Immediate.t
          | Label of Label.t
          | Address of Address.t
          | MemLoc of MemLoc.t

        val layout : t -> Layout.t
        val toString : t -> string

        val register : Register.t -> t
        val deRegister : t -> Register.t option
        val xmmregister : XmmRegister.t -> t
        val deXmmregister : t -> XmmRegister.t option
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
          = ADD (* signed/unsigned addition; p. 58 *)
          | ADC (* signed/unsigned addition with carry; p. 56 *)
          | SUB (* signed/unsigned subtraction; p. 234 *)
          | SBB (* signed/unsigned subtraction with borrow; p. 216 *)
          | AND (* logical and; p. 60 *)
          | OR  (* logical or; p. 176 *)
          | XOR (* logical xor; p. 243 *)
        (* Integer multiplication and division. *)
        datatype md
          = IMUL (* signed multiplication (one operand form); p. 114 *)
          | MUL  (* unsigned multiplication; p. 170 *)
          | IDIV (* signed division; p. 112 *)
          | DIV  (* unsigned division; p. 108 *)
          | IMOD (* signed modulus; *)
          | MOD  (* unsigned modulus; *)
        (* Integer unary arithmetic/logic instructions. *)
        datatype unal
          = INC (* increment by 1; p. 117 *)
          | DEC (* decrement by 1; p. 106 *)
          | NEG (* two's complement negation; p. 172 *)
          | NOT (* one's complement negation; p. 175 *)
        (* Integer shift/rotate arithmetic/logic instructions. *)
        datatype sral
          = SAL (* shift arithmetic left; p. 211 *)
          | SHL (* shift logical left; p. 211 *)
          | SAR (* shift arithmetic right; p. 214 *)
          | SHR (* shift logical right; p. 214 *)
          | ROL (* rotate left; p. 206 *)
          | RCL (* rotate through carry left; p. 197 *)
          | ROR (* rotate right; p. 208 *)
          | RCR (* rotate through carry right; p. 199 *)
        (* Move with extention instructions. *)
        datatype movx
          = MOVSX (* move with sign extention; p. 167 *)
          | MOVZX (* move with zero extention; p. 169 *)
        (* Condition test field; p. 340 *)
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

        (* Scalar SSE binary arithmetic instructions. *)
        datatype sse_binas
          = SSE_ADDS (* addition; p. 7,10 *)
          | SSE_SUBS (* subtraction; p. 371,374 *)
          | SSE_MULS (* multiplication; p. 201,204 *)
          | SSE_DIVS (* division; p. 97,100 *)
          | SSE_MAXS (* maximum; p. 128, 130 *)
          | SSE_MINS (* minimum; p. 132, 134 *)
        (* Scalar SSE unary arithmetic instructions. *)
        datatype sse_unas
          = SSE_SQRTS (* square root; p. 360,362 *)
        (* Packed SSE binary logical instructions (used as scalar). *)
        datatype sse_binlp
          = SSE_ANDNP (* and-not; p. 17,19 *)
          | SSE_ANDP (* and; p. 21,23 *)
          | SSE_ORP (* or; p. 206,208 *)
          | SSE_XORP (* xor; p. 391,393 *)

        (* amd64 Instructions.
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
          (* Integer signed/unsiged multiplication (two operand form); p. 114
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
          (* Scalar SSE binary arithmetic instructions.
           *)
          | SSE_BinAS of {oper: sse_binas,
                          src: Operand.t,
                          dst: Operand.t,
                          size: Size.t}
          (* Scalar SSE unary arithmetic instructions.
           *)
          | SSE_UnAS of {oper: sse_unas,
                         src: Operand.t,
                         dst: Operand.t,
                         size: Size.t}
          (* Packed SSE binary logic instructions (used as scalar). 
           *)
          | SSE_BinLP of {oper: sse_binlp,
                          src: Operand.t,
                          dst: Operand.t,
                          size: Size.t}
          (* Scalar SSE move instruction.
           *)
          | SSE_MOVS of {src: Operand.t,
                         dst: Operand.t,
                         size: Size.t}
          (* Scalar SSE compare instruction.
           *)
          | SSE_COMIS of {src1: Operand.t,
                          src2: Operand.t,
                          size: Size.t}
          (* Scalar SSE unordered compare instruction.
           *)
          | SSE_UCOMIS of {src1: Operand.t,
                           src2: Operand.t,
                           size: Size.t}
          (* Scalar SSE floating-point/floating-point convert instruction.
           *)
          | SSE_CVTSFP2SFP of {src: Operand.t,
                               srcsize: Size.t,
                               dst: Operand.t,
                               dstsize: Size.t}
          (* Scalar SSE floating-point/signed-integer convert instruction.
           *)
          | SSE_CVTSFP2SI of {src: Operand.t,
                              srcsize: Size.t,
                              dst: Operand.t,
                              dstsize: Size.t}
          | SSE_CVTSI2SFP of {src: Operand.t,
                              srcsize: Size.t,
                              dst: Operand.t,
                              dstsize: Size.t}
          (* Scalar SSE move data instruction.
           *)
          | SSE_MOVD of {src: Operand.t,
                         srcsize: Size.t,
                         dst: Operand.t,
                         dstsize: Size.t}


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
          | XmmAssume of {assumes: {register: XmmRegister.t,
                                     memloc: MemLoc.t, 
                                     weight: int,
                                     sync: bool,
                                     reserve: bool} list}
            (* Ensure that memloc is in the register, possibly reserved; 
             * used at bot of basic blocks to establish passing convention,
             * also used before C calls to set-up %esp.
             *)
          | Cache of {caches: {register: Register.t,
                               memloc: MemLoc.t,
                               reserve: bool} list}
          | XmmCache of {caches: {register: XmmRegister.t,
                                   memloc: MemLoc.t,
                                   reserve: bool} list}
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
          | XmmReserve of {registers: XmmRegister.t list}
            (* Assert that the register is free for the allocator;
             * used to free registers at fall-thru;
             * also used after C calls to free %esp.
             *)
          | Unreserve of {registers: Register.t list}
          | XmmUnreserve of {registers: XmmRegister.t list}
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
        val xmmassume : {assumes: {register: XmmRegister.t,
                                    memloc: MemLoc.t,
                                    weight: int,
                                    sync: bool,
                                    reserve: bool} list} -> t
        val cache : {caches: {register: Register.t,
                              memloc: MemLoc.t,
                              reserve: bool} list} -> t
        val xmmcache : {caches: {register: XmmRegister.t,
                                  memloc: MemLoc.t,
                                  reserve: bool} list} -> t
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
        val xmmreserve : {registers: XmmRegister.t list} -> t
        val unreserve : {registers: Register.t list} -> t
        val xmmunreserve : {registers: XmmRegister.t list} -> t
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
          | Balign of Immediate.t * Immediate.t option * Immediate.t option
          | P2align of Immediate.t * Immediate.t option * Immediate.t option
          | Space of Immediate.t * Immediate.t
          | Byte of Immediate.t list
          | Word of Immediate.t list
          | Long of Immediate.t list
          | Quad of Immediate.t list
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
        val balign : Immediate.t * Immediate.t option * Immediate.t option -> t
        val p2align : Immediate.t * Immediate.t option * Immediate.t option -> t
        val space : Immediate.t * Immediate.t -> t
        val byte : Immediate.t list -> t
        val word : Immediate.t list -> t
        val long : Immediate.t list -> t
        val quad : Immediate.t list -> t
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
        val directive_xmmassume : {assumes: {register: XmmRegister.t,
                                              memloc: MemLoc.t,
                                              weight: int,
                                              sync: bool,
                                              reserve: bool} list} -> t
        val directive_cache : {caches: {register: Register.t,
                                        memloc: MemLoc.t,
                                        reserve: bool} list} -> t
        val directive_xmmcache : {caches: {register: XmmRegister.t,
                                            memloc: MemLoc.t,
                                            reserve: bool} list} -> t
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
        val directive_xmmreserve : {registers: XmmRegister.t list} -> t
        val directive_unreserve : {registers: Register.t list} -> t
        val directive_xmmunreserve : {registers: XmmRegister.t list} -> t
        val directive_saveregalloc : {live: MemLocSet.t,
                                      id: Directive.Id.t} -> t
        val directive_restoreregalloc : {live: MemLocSet.t,
                                         id: Directive.Id.t} -> t
        val pseudoop : PseudoOp.t -> t
        val pseudoop_data : unit -> t
        val pseudoop_text : unit -> t
        val pseudoop_symbol_stub : unit -> t
        val pseudoop_balign : Immediate.t * Immediate.t option * Immediate.t option ->t 
        val pseudoop_p2align : Immediate.t * Immediate.t option * Immediate.t option -> t
        val pseudoop_space : Immediate.t * Immediate.t -> t
        val pseudoop_byte : Immediate.t list -> t
        val pseudoop_word : Immediate.t list -> t
        val pseudoop_long : Immediate.t list -> t
        val pseudoop_quad : Immediate.t list -> t
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
        val instruction_sse_binas : {oper: Instruction.sse_binas,
                                     src: Operand.t,
                                     dst: Operand.t,
                                     size: Size.t} -> t
        val instruction_sse_unas : {oper: Instruction.sse_unas,
                                    src: Operand.t,
                                    dst: Operand.t,
                                    size: Size.t} -> t
        val instruction_sse_binlp : {oper: Instruction.sse_binlp,
                                     src: Operand.t,
                                     dst: Operand.t,
                                     size: Size.t} -> t
        val instruction_sse_movs : {src: Operand.t,
                                    dst: Operand.t,
                                    size: Size.t} -> t
        val instruction_sse_comis : {src1: Operand.t,
                                     src2: Operand.t,
                                     size: Size.t} -> t
        val instruction_sse_ucomis : {src1: Operand.t,
                                      src2: Operand.t,
                                      size: Size.t} -> t
        val instruction_sse_cvtsfp2sfp : {src: Operand.t,
                                          srcsize: Size.t,
                                          dst: Operand.t,
                                          dstsize: Size.t} -> t
        val instruction_sse_cvtsfp2si : {src: Operand.t,
                                         srcsize: Size.t,
                                         dst: Operand.t,
                                         dstsize: Size.t} -> t
        val instruction_sse_cvtsi2sfp : {src: Operand.t,
                                         srcsize: Size.t,
                                         dst: Operand.t,
                                         dstsize: Size.t} -> t
        val instruction_sse_movd : {src: Operand.t,
                                    srcsize: Size.t,
                                    dst: Operand.t,
                                    dstsize: Size.t} -> t
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
