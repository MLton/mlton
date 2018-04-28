(* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor x86 (S: X86_STRUCTS): X86 =
struct

   val tracerTop
    = fn s => Control.traceBatch (Control.Pass, s)
(*
    = fn s => fn f => (Control.trace (Control.Pass, s) f, fn () => ())
*)
    val tracer
    = fn s => Control.traceBatch (Control.Detail, s)
(*
    = fn s => fn f => (Control.trace (Control.Detail, s) f, fn () => ())
*)

  (* compensate for differences between 
   *  C-escape sequences and ASM-escape sequences
   *) 
  val Char_escapeASM = fn #"\000" => "\\000"
                        | #"\^G" => "\\007"
                        | #"\^K" => "\\013"
                        | #"?" => "?"
                        | #"'" => "'"
                        | c => Char.escapeC c
  fun String_escapeASM s = String.translate(s, Char_escapeASM)

  val rec lexical
    = fn [] => EQUAL
       | thunk::tl => let
                        val ord = thunk ()
                      in 
                        if Relation.equals(ord, EQUAL)
                          then lexical tl
                          else ord
                      end


  open S

  structure Label =
     struct
        open Label

        fun toString l =
           if !Control.labelsHaveExtra_
              then concat ["_", Label.toString l]
           else Label.toString l

        val layout = Layout.str o toString
     end

  structure Size =
    struct
      datatype class = INT | FLT | FPI

      datatype t 
        = BYTE | WORD | LONG
        | SNGL | DBLE | EXTD
        | FPIS | FPIL | FPIQ

      val layout
        = let
            open Layout
          in
            fn BYTE => str "b"
             | WORD => str "w"
             | LONG => str "l"
             | SNGL => str "S"
             | DBLE => str "L"
             | EXTD => str "T"
             | FPIS => str "s"
             | FPIL => str "l"
             | FPIQ => str "q"
          end
      val toString = Layout.toString o layout

      val fromBytes : int -> t
        = fn 1 => BYTE
           | 2 => WORD
           | 4 => LONG
           | _ => Error.bug "x86.Size.fromBytes"
      val toBytes : t -> int
        = fn BYTE => 1
           | WORD => 2
           | LONG => 4
           | SNGL => 4
           | DBLE => 8
           | EXTD => 10
           | FPIS => 2
           | FPIL => 4
           | FPIQ => 8

      local
         datatype z = datatype CType.t
      in
         fun fromCType t =
            case t of
               CPointer => Vector.new1 LONG
             | Int8 => Vector.new1 BYTE
             | Int16 => Vector.new1 WORD
             | Int32 => Vector.new1 LONG
             | Int64 => Vector.new2 (LONG, LONG)
             | Objptr => Vector.new1 LONG
             | Real32 => Vector.new1 SNGL
             | Real64 => Vector.new1 DBLE
             | Word8 => Vector.new1 BYTE
             | Word16 => Vector.new1 WORD
             | Word32 => Vector.new1 LONG
             | Word64 => Vector.new2 (LONG, LONG)
      end

      val class
        = fn BYTE => INT
           | WORD => INT
           | LONG => INT
           | SNGL => FLT
           | DBLE => FLT
           | EXTD => FLT
           | FPIS => FPI
           | FPIL => FPI
           | FPIQ => FPI

      val toFPI
        = fn WORD => FPIS
           | LONG => FPIL
           | FPIS => FPIS
           | FPIL => FPIL
           | FPIQ => FPIQ
           | _ => Error.bug "x86.Size.toFPI"

      val eq = fn (s1, s2) => s1 = s2
      val lt = fn (s1, s2) => (toBytes s1) < (toBytes s2)
    end

  structure Register =
    struct

      datatype reg
        = EAX | EBX | ECX | EDX | EDI | ESI | EBP | ESP
      val allReg = [EAX, EBX, ECX, EDX, EDI, ESI, EBP, ESP]

      datatype part
        = E | X | L | H

      datatype t = T of {reg: reg, part: part}

      fun size (T {part, ...})
        = case part
            of E => Size.LONG
             | X => Size.WORD
             | L => Size.BYTE
             | H => Size.BYTE

      fun layout (T {reg, part})
        = let
            open Layout
            val {prefix, suffix}
              = case part
                  of E => {prefix = str "%e", suffix = str "x"}
                   | X => {prefix = str "%", suffix = str "x"}
                   | L => {prefix = str "%", suffix = str "l"}
                   | H => {prefix = str "%", suffix = str "h"}
          in
            case reg
              of EAX => seq [prefix, str "a", suffix]
               | EBX => seq [prefix, str "b", suffix]
               | ECX => seq [prefix, str "c", suffix]
               | EDX => seq [prefix, str "d", suffix]
               | EDI => seq [prefix, str "di"]
               | ESI => seq [prefix, str "si"]
               | EBP => seq [prefix, str "bp"]
               | ESP => seq [prefix, str "sp"]
          end
      val toString = Layout.toString o layout

      fun eq(T r1, T r2) = r1 = r2

      val eax = T {reg = EAX, part = E}
      val ebx = T {reg = EBX, part = E}
      val ecx = T {reg = ECX, part = E}
      val edx = T {reg = EDX, part = E}
      val ax = T {reg= EAX, part = X}
      val al = T {reg = EAX, part = L}
      val bl = T {reg = EBX, part = L}
      val cl = T {reg = ECX, part = L}
      val dl = T {reg = EDX, part = L}
      val edi = T {reg = EDI, part = E}
      val esi = T {reg = ESI, part = E}
      val esp = T {reg = ESP, part = E}
      val ebp = T {reg = EBP, part = E}

      val byteRegisters = [T {reg = EAX, part = L},
                           T {reg = EAX, part = H},
                           T {reg = EBX, part = L},
                           T {reg = EBX, part = H},
                           T {reg = ECX, part = L},
                           T {reg = ECX, part = H},
                           T {reg = EDX, part = L},
                           T {reg = EDX, part = H}]
      val byteRegisters = List.rev byteRegisters

      val wordRegisters = [T {reg = EAX, part = X},
                           T {reg = EBX, part = X},
                           T {reg = ECX, part = X},
                           T {reg = EDX, part = X},
                           T {reg = EDI, part = X},
                           T {reg = ESI, part = X},
                           T {reg = EBP, part = X},
                           T {reg = ESP, part = X}]
      val wordRegisters = List.rev wordRegisters

      val longRegisters = [T {reg = EAX, part = E},
                           T {reg = EBX, part = E},
                           T {reg = ECX, part = E},
                           T {reg = EDX, part = E},
                           T {reg = EDI, part = E},
                           T {reg = ESI, part = E},
                           T {reg = EBP, part = E},
                           T {reg = ESP, part = E}]
      val longRegisters = List.rev longRegisters

      val all = List.concat [byteRegisters, wordRegisters, longRegisters]

      fun valid r = List.contains(all, r, eq)

      val contains 
        = fn (E, E) => true | (E, X) => true | (E, L) => true | (E, H) => true
           | (X, X) => true | (X, L) => true | (X, H) => true
           | (L, L) => true 
           | (H, H) => true
           | _      => false

      fun coincide (T {reg = reg1, part = part1}, 
                    T {reg = reg2, part = part2}) 
        = reg1 = reg2 andalso (contains(part1,part2) orelse 
                               contains(part2,part1))

      fun coincident' reg
        = List.keepAllMap([E, X, L, H],
                          fn part 
                           => let
                                val register' = T {reg = reg, part = part}
                              in 
                                if valid register' andalso 
                                   coincide(T {reg = reg, part = E}, register')
                                  then SOME register'
                                  else NONE
                              end)

      val registers
        = fn Size.BYTE => byteRegisters
           | Size.WORD => wordRegisters
           | Size.LONG => longRegisters
           | _ => Error.bug "x86.Register.registers"

      val baseRegisters = longRegisters
      val indexRegisters = [T {reg = EAX, part = E},
                            T {reg = EBX, part = E},
                            T {reg = ECX, part = E},
                            T {reg = EDX, part = E},
                            T {reg = EDI, part = E},
                            T {reg = ESI, part = E},
                            T {reg = EBP, part = E}]

      val callerSaveRegisters = [T {reg = EAX, part = E},
                                 T {reg = EAX, part = X},
                                 T {reg = EAX, part = L},
                                 T {reg = EAX, part = H},
                                 T {reg = ECX, part = E},
                                 T {reg = ECX, part = X},
                                 T {reg = ECX, part = L},
                                 T {reg = ECX, part = H},
                                 T {reg = EDX, part = E},
                                 T {reg = EDX, part = X},
                                 T {reg = EDX, part = L},
                                 T {reg = EDX, part = H}]
      val calleeSaveRegisters = [T {reg = EBX, part = E},
                                 T {reg = EBX, part = X},
                                 T {reg = EBX, part = L},
                                 T {reg = EBX, part = H},
                                 T {reg = EDI, part = E},
                                 T {reg = EDI, part = X},
                                 T {reg = ESI, part = E},
                                 T {reg = ESI, part = X}]

      val withLowPart (* (fullsize,lowsize) *)
        = fn (Size.WORD,Size.BYTE) => [T {reg = EAX, part = X},
                                       T {reg = EBX, part = X},
                                       T {reg = ECX, part = X},
                                       T {reg = EDX, part = X}]
           | (Size.LONG,Size.BYTE) => [T {reg = EAX, part = E},
                                       T {reg = EBX, part = E},
                                       T {reg = ECX, part = E},
                                       T {reg = EDX, part = E}]
           | (Size.LONG,Size.WORD) => longRegisters
           | _ => Error.bug "x86.Register.withLowPart: fullsize,lowsize"

      val lowPartOf (* (register,lowsize) *)
        = fn (T {reg,       part = L},Size.BYTE) => T {reg = reg, part = L}
           | (T {reg,       part = H},Size.BYTE) => T {reg = reg, part = H}
           | (T {reg = EAX, ...},     Size.BYTE) => T {reg = EAX, part = L}
           | (T {reg = EBX, ...},     Size.BYTE) => T {reg = EBX, part = L}
           | (T {reg = ECX, ...},     Size.BYTE) => T {reg = ECX, part = L}
           | (T {reg = EDX, ...},     Size.BYTE) => T {reg = EDX, part = L}
           | (T {reg,       part = X},Size.WORD) => T {reg = reg, part = X}
           | (T {reg,       ...},     Size.WORD) => T {reg = reg, part = X}
           | _ => Error.bug "x86.Register.lowPartOf: register,lowsize"
    end

  structure FltRegister =
    struct
      datatype t = T of int

      fun layout (T i)
        = let
            open Layout
          in if i = 0
               then str "%st"
               else seq [str "%st", paren (Int.layout i)]
          end
      val toString = Layout.toString o layout

      fun eq (T f1, T f2) = f1 = f2

      fun push (T i) = T (i + 1)
      fun pop (T i) = T (i - 1)
      fun id (T i) = T i

(*
      val return = T 0
*)
      val top = T 0
      val one = T 1
      val total = 8 : int
    end

  structure Immediate =
    struct
      datatype u
        = Word of WordX.t
        | Label of Label.t
        | LabelPlusWord of Label.t * WordX.t
      and t
        = T of {immediate: u,
                plist: PropertyList.t,
                hash: Word.t}

      local 
        open Layout
      in
        val rec layoutU
          = fn Word w => WordX.layout w
             | Label l => Label.layout l
             | LabelPlusWord (l, w) 
             => paren (seq [Label.layout l, str "+", WordX.layout w])
        and layout
          = fn T {immediate, ...} => layoutU immediate
      end

      val rec eqU
        = fn (Word w1, Word w2) => WordX.equals (w1, w2)
           | (Label l1, Label l2) => Label.equals(l1, l2)
           | (LabelPlusWord (l1, w1), LabelPlusWord (l2,w2))
           => Label.equals(l1,l2) andalso WordX.equals(w1, w2)
           | _ => false
      and eq
        = fn (T {plist = plist1, ...},
              T {plist = plist2, ...})
           => PropertyList.equals(plist1, plist2)

      local 
        open WordX
      in
        val rec evalU
          = fn Word w => SOME w
             | Label _ => NONE
             | LabelPlusWord _ => NONE
        and eval
          = fn T {immediate, ...} => evalU immediate
      end

      val isZero = fn i => case eval i of SOME w => WordX.isZero w | _ => false

      local 
        open Word
      in 
        val rec hashU
          = fn Word w => WordX.hash w
             | Label l => Label.hash l
             | LabelPlusWord (l,w)
             => Word.xorb(0wx5555 * (Label.hash l), WordX.hash w)
        and hash
          = fn T {hash, ...} => hash
      end

      local
        val table: t HashSet.t ref = ref (HashSet.new {hash = hash})
      in
        val construct
          = fn immediate
             => let
                  val hash = hashU immediate
                in
                  HashSet.lookupOrInsert
                  (!table,
                   hash,
                   fn T {immediate = immediate', ...}
                    => eqU(immediate', immediate),
                   fn () => T {immediate = immediate,
                               hash = hash,
                               plist = PropertyList.new ()})
                end

        val destruct
          = fn T {immediate, ...} => immediate

        fun clearAll ()
          = HashSet.foreach
            (!table, fn T {immediate, plist, ...} =>
             let in
               PropertyList.clear plist;
               case immediate
                 of Word _ => ()
                  | Label l => Label.clear l
                  | LabelPlusWord (l, _) => Label.clear l
             end)
      end

      val word = construct o Word
      val label = construct o Label
      val labelPlusWord = fn (l, w) =>
         if WordSize.equals (WordX.size w, WordSize.word32)
            then construct (LabelPlusWord (l, w))
         else Error.bug "x86.Immediate.labelPlusWord"

      val int' = fn (i, ws) => word (WordX.fromIntInf (IntInf.fromInt i, ws))
      val int = fn i => int' (i, WordSize.word32)
      val zero = int 0

      val labelPlusInt = fn (l, i) => 
         labelPlusWord (l, WordX.fromIntInf (IntInf.fromInt i, WordSize.word32))

      val deLabel
        = fn T {immediate = Label l, ...} => SOME l
           | _ => NONE
    end

  structure Scale = 
    struct
      datatype t 
        = One | Two | Four | Eight

      val layout
        = let
            open Layout
          in
            fn One => str "1"
             | Two => str "2"
             | Four => str "4"
             | Eight => str "8"
          end

      val fromBytes : int -> t
        = fn 1 => One
           | 2 => Two
           | 4 => Four
           | 8 => Eight
           | _ => Error.bug "x86.Scale.fromBytes"
      local
         datatype z = datatype CType.t
      in
         fun fromCType t =
            case t of 
               CPointer => Four
             | Int8 => One
             | Int16 => Two
             | Int32 => Four
             | Int64 => Eight
             | Objptr => Four
             | Real32 => Four
             | Real64 => Eight
             | Word8 => One
             | Word16 => Two
             | Word32 => Four
             | Word64 => Eight
      end

      fun eq(s1, s2) = s1 = s2

      val toWordX
        = fn One => WordX.fromIntInf (1, WordSize.word32)
           | Two => WordX.fromIntInf (2, WordSize.word32)
           | Four => WordX.fromIntInf (4, WordSize.word32)
           | Eight => WordX.fromIntInf (8, WordSize.word32)
      val toImmediate = Immediate.word o toWordX
    end

  structure Address =
    struct
      datatype t = T of {disp: Immediate.t option,
                         base: Register.t option,
                         index: Register.t option,
                         scale: Scale.t option}

      fun layout (T {disp, base, index, scale})
        = let
            open Layout
          in
            seq [case disp
                   of NONE => empty
                    | SOME disp => Immediate.layout disp,
                 if (isSome base orelse isSome index)
                   then paren (seq
                               [case base
                                  of NONE => empty
                                  | SOME base
                                  => Register.layout base,
                                case index
                                  of NONE => empty
                                   | SOME index
                                   => seq [str ",", Register.layout index],
                                case scale
                                  of NONE => empty
                                  | SOME scale
                                  => seq [str ",", Scale.layout scale]])
                   else empty]
          end

      fun eq(T {disp = disp,  base = base,  index = index,  scale = scale},
             T {disp = disp', base = base', index = index', scale = scale'})
        = (case (disp, disp')
             of (NONE, NONE) => true
              | (SOME disp, SOME disp') => Immediate.eq(disp, disp') 
              | _ => false) andalso
          base = base' andalso
          index = index' andalso
          scale = scale'
    end

  structure MemLoc =
    struct
      structure Class =
        struct
          val counter = Counter.new 0
          datatype t = T of {counter: int,
                             name: string}

          fun layout (T {name, ...})
            = let
                open Layout
              in
                str name
              end
          val toString = Layout.toString o layout

          fun new {name}
            = let
                val class = T {counter = Counter.next counter,
                               name = name}
              in
                class
              end

          val eq 
            = fn (T {counter = counter1, ...}, 
                  T {counter = counter2, ...}) 
               => counter1 = counter2
          val compare 
            = fn (T {counter = counter1, ...}, 
                  T {counter = counter2, ...}) 
               => Int.compare (counter1, counter2)
          val counter
            = fn (T {counter, ...}) => counter
          val mayAlias = eq

          val Temp = new {name = "Temp"}
          val StaticTemp = new {name = "StaticTemp"}
          val CStack = new {name = "CStack"}
          val Code = new {name = "Code"}
        end

      datatype u
        = U of {immBase: Immediate.t option,
                memBase: t option,
                immIndex: Immediate.t option,
                memIndex: t option,
                scale: Scale.t,
                size: Size.t,
                class: Class.t}
      and t
        = T of {memloc: u,
                hash: Word.t,
                plist: PropertyList.t,
                counter: Int.t,
                utilized: t list}

      local
        open Layout
      in
        val rec layoutImmMem 
          = fn (NONE, NONE) => str "0"
             | (SOME imm, NONE) => Immediate.layout imm
             | (NONE, SOME mem) => layout mem
             | (SOME imm, SOME mem) => seq [Immediate.layout imm,
                                            str "+",
                                            layout mem]

        and layoutImmMemScale
          = fn (NONE, NONE, _) => str "0"
             | (SOME imm, NONE, _) => Immediate.layout imm
             | (NONE, SOME mem, scale) => seq [layout mem,
                                               str "*",
                                               Scale.layout scale]
             | (SOME imm, SOME mem, scale) => seq [Immediate.layout imm,
                                                   str "+(",
                                                   layout mem,
                                                   str "*",
                                                   Scale.layout scale,
                                                   str ")"]
        and layoutU
          = fn U {immBase, memBase,
                  immIndex, memIndex,
                  scale,
                  size, class}
             => seq [str "MEM<",
                     Size.layout size,
                     str ">{",
                     Class.layout class,
                     str "}[(",
                     layoutImmMem (immBase, memBase),
                     str ")+(",
                     layoutImmMemScale (immIndex, memIndex, scale),
                     str ")]"]
        and layout
          = fn T {memloc, ...} => layoutU memloc
      end
      val toString = Layout.toString o layout

      val rec hashImmMem
        = fn (NONE, NONE) => 0wx55555555
           | (SOME imm, NONE) => Immediate.hash imm
           | (NONE, SOME mem) => hash mem
           | (SOME imm, SOME mem)
           => Word.xorb(0wx5555 * (Immediate.hash imm), hash mem)
      and hashU
        = fn U {immBase, memBase, immIndex, memIndex, ...}
           => let
                val hashBase = hashImmMem(immBase, memBase)
                val hashIndex = hashImmMem(immIndex, memIndex)
              in
                Word.xorb(0wx5555 * hashBase, hashIndex)
              end
      and hash
        = fn T {hash, ...} => hash

      val rec eqImm
        = fn (NONE, NONE) => true
           | (SOME imm1, SOME imm2) => Immediate.eq(imm1, imm2)
           | _ => false
      and eqMem
        = fn (NONE, NONE) => true
           | (SOME mem1, SOME mem2) => eq(mem1, mem2)
           | _ => false
      and eqU
        = fn (U {immBase = immBase1, memBase = memBase1,
                 immIndex = immIndex1, memIndex = memIndex1,
                 scale = scale1, size = size1, 
                 class = class1},
              U {immBase = immBase2, memBase = memBase2,
                 immIndex = immIndex2, memIndex = memIndex2,
                 scale = scale2, size = size2, 
                 class = class2})
           => Class.eq(class1, class2) andalso
              eqImm(immBase1, immBase2) andalso
              eqMem(memBase1, memBase2) andalso
              eqImm(immIndex1, immIndex2) andalso
              eqMem(memIndex1, memIndex2) andalso
              Scale.eq(scale1, scale2) andalso
              Size.eq(size1, size2)
      and eq
        = fn (T {plist = plist1, ...},
              T {plist = plist2, ...})
           => PropertyList.equals(plist1, plist2)

      val rec utilizedMem
        = fn NONE => []
           | SOME m => m::(utilized m)
      and utilizedU
        = fn U {memBase, memIndex, ...} 
           => (utilizedMem memBase) @ (utilizedMem memIndex)
      and utilized
        = fn T {utilized, ...}
           => utilized

      local
        val counter = Counter.new 0
        val table: t HashSet.t ref = ref (HashSet.new {hash = hash})
      in
        val construct 
          = fn memloc
             => let
                  val hash = hashU memloc
                in 
                  HashSet.lookupOrInsert
                  (!table,
                   hash,
                   fn T {memloc = memloc', ...} => eqU(memloc', memloc),
                   fn () => T {memloc = memloc,
                               hash = hash,
                               plist = PropertyList.new (),
                               counter = Counter.next counter,
                               utilized = utilizedU memloc})
                end

        val destruct 
          = fn T {memloc, ...}
             => memloc

        fun clearAll ()
          = HashSet.foreach
            (!table, fn T {plist, ...} =>
             let in
               PropertyList.clear plist
             end)
      end

      val rec mayAliasImmIndex 
        = fn ({immIndex = immIndex1, size = size1},
              {immIndex = immIndex2, size = size2})
           => let
                val size1 = IntInf.fromInt (Size.toBytes size1)
                val size2 = IntInf.fromInt (Size.toBytes size2)
              in
                case (Immediate.eval (case immIndex1
                                        of NONE => Immediate.zero
                                         | SOME immIndex => immIndex),
                      Immediate.eval (case immIndex2
                                        of NONE => Immediate.zero
                                         | SOME immIndex => immIndex))
                  of (SOME pos1, SOME pos2)
                   => (let
                         val pos1 = WordX.toIntInfX pos1
                         val pos2 = WordX.toIntInfX pos2
                       in 
                         if pos1 < pos2 
                           then pos2 < (pos1 + size1) 
                           else pos1 < (pos2 + size2)
                       end
                       handle Overflow => false)
                   | _ => true
          end
      and mayAliasU
        = fn (U {immBase = SOME immBase1, memBase = NONE,
                 immIndex = immIndex1, memIndex = NONE,
                 size = size1, ...},
              U {immBase = SOME immBase2, memBase = NONE,
                 immIndex = immIndex2, memIndex = NONE,
                 size = size2, ...})
           => Immediate.eq(immBase1, immBase2)
              andalso
              mayAliasImmIndex ({immIndex = immIndex1, 
                                 size = size1},
                                {immIndex = immIndex2,
                                 size = size2})
           | (U {immBase = SOME immBase1, memBase = NONE,
                 immIndex = immIndex1, memIndex = SOME memIndex1,
                 size = size1, ...},
              U {immBase = SOME immBase2, memBase = NONE,
                 immIndex = immIndex2, memIndex = SOME memIndex2,
                 size = size2, ...})
           => not (Immediate.eq(immBase1, immBase2))
              andalso
              (not (eq(memIndex1, memIndex2))
               orelse
               mayAliasImmIndex ({immIndex = immIndex1,
                                  size = size1},
                                 {immIndex = immIndex2,
                                  size = size2}))
           | (U {immBase = NONE, memBase = SOME memBase1,
                 immIndex = immIndex1, memIndex = NONE,
                 size = size1, ...},
              U {immBase = NONE, memBase = SOME memBase2,
                 immIndex = immIndex2, memIndex = NONE,
                 size = size2, ...})
           => not (eq(memBase1, memBase2))
              orelse
              mayAliasImmIndex ({immIndex = immIndex1,
                                 size = size1},
                                {immIndex = immIndex2,
                                 size = size2})
           | (U {immBase = NONE, memBase = SOME memBase1,
                 immIndex = immIndex1, memIndex = SOME memIndex1,
                 size = size1, ...},
              U {immBase = NONE, memBase = SOME memBase2,
                 immIndex = immIndex2, memIndex = SOME memIndex2,
                 size = size2, ...})
           => not (eq(memBase1, memBase2))
              orelse
              not (eq(memIndex1, memIndex2))
              orelse
              mayAliasImmIndex ({immIndex = immIndex1,
                                 size = size1},
                                {immIndex = immIndex2,
                                 size = size2})
           | _ => true
      and mayAlias
        = fn (T {memloc = memloc1 as U {class = class1, ...}, ...},
              T {memloc = memloc2 as U {class = class2, ...}, ...})
           => Class.mayAlias(class1, class2) andalso
              mayAliasU(memloc1, memloc2)

      val rec mayAliasOrdImmIndex 
        = fn ({immIndex = immIndex1, size = size1},
              {immIndex = immIndex2, size = size2})
           => let
                val size1 = IntInf.fromInt (Size.toBytes size1)
                val size2 = IntInf.fromInt (Size.toBytes size2)
              in
                case (Immediate.eval (case immIndex1
                                        of NONE => Immediate.zero
                                         | SOME immIndex => immIndex),
                      Immediate.eval (case immIndex2
                                        of NONE => Immediate.zero
                                         | SOME immIndex => immIndex))
                  of (SOME pos1, SOME pos2)
                   => (let
                         val pos1 = WordX.toIntInfX pos1
                         val pos2 = WordX.toIntInfX pos2
                       in 
                         if pos1 < pos2 
                           then if pos2 < (pos1 + size1) 
                                  then SOME LESS
                                  else NONE
                           else if pos1 < (pos2 + size2)
                                  then SOME GREATER
                                  else NONE
                       end
                       handle Overflow => NONE)
                   | _ => SOME EQUAL
          end
      and mayAliasOrdU
        = fn (U {immBase = SOME immBase1, memBase = NONE,
                 immIndex = immIndex1, memIndex = NONE,
                 size = size1, ...},
              U {immBase = SOME immBase2, memBase = NONE,
                 immIndex = immIndex2, memIndex = NONE,
                 size = size2, ...})
           => if Immediate.eq(immBase1, immBase2)
                then mayAliasOrdImmIndex ({immIndex = immIndex1, 
                                           size = size1},
                                          {immIndex = immIndex2,
                                           size = size2})
                else NONE
           | (U {immBase = SOME immBase1, memBase = NONE,
                 immIndex = immIndex1, memIndex = SOME memIndex1,
                 size = size1, ...},
              U {immBase = SOME immBase2, memBase = NONE,
                 immIndex = immIndex2, memIndex = SOME memIndex2,
                 size = size2, ...})
           => if Immediate.eq(immBase1, immBase2)
                then if not (eq(memIndex1, memIndex2))
                       then SOME EQUAL
                       else mayAliasOrdImmIndex ({immIndex = immIndex1,
                                                  size = size1},
                                                 {immIndex = immIndex2,
                                                  size = size2})
                else NONE
           | (U {immBase = NONE, memBase = SOME memBase1,
                 immIndex = immIndex1, memIndex = NONE,
                 size = size1, ...},
              U {immBase = NONE, memBase = SOME memBase2,
                 immIndex = immIndex2, memIndex = NONE,
                 size = size2, ...})
           => if not (eq(memBase1, memBase2))
                then SOME EQUAL
                else mayAliasOrdImmIndex ({immIndex = immIndex1,
                                           size = size1},
                                          {immIndex = immIndex2,
                                           size = size2})
           | (U {immBase = NONE, memBase = SOME memBase1,
                 immIndex = immIndex1, memIndex = SOME memIndex1,
                 size = size1, ...},
              U {immBase = NONE, memBase = SOME memBase2,
                 immIndex = immIndex2, memIndex = SOME memIndex2,
                 size = size2, ...})
           => if (not (eq(memBase1, memBase2))
                  orelse
                  not (eq(memIndex1, memIndex2)))
                then SOME EQUAL
                else mayAliasOrdImmIndex ({immIndex = immIndex1,
                                           size = size1},
                                          {immIndex = immIndex2,
                                           size = size2})
           | _ => SOME EQUAL
      and mayAliasOrd
        = fn (T {memloc = memloc1 as U {class = class1, ...}, ...},
              T {memloc = memloc2 as U {class = class2, ...}, ...})
           => if Class.mayAlias(class1, class2) 
                then mayAliasOrdU(memloc1, memloc2)
                else NONE

      val compare
        = fn (T {counter = counter1, ...},
              T {counter = counter2, ...})
           => Int.compare(counter1, counter2)

      fun replaceMem replacer
        = fn NONE => NONE
           | SOME mem => SOME (replace replacer mem)
      and replaceU replacer
        = fn memloc as T {memloc = U {immBase, memBase, immIndex, memIndex,
                                      scale, size, class}, ...}
           => let
                val memBase' = replaceMem replacer memBase
                val memIndex' = replaceMem replacer memIndex
              in 
                if eqMem(memBase, memBase') andalso eqMem(memIndex, memIndex')
                  then memloc
                  else construct (U {immBase = immBase,
                                     memBase = memBase',
                                     immIndex = immIndex,
                                     memIndex = memIndex',
                                     scale = scale,
                                     size = size,
                                     class = class})
              end
      and replace replacer
        = fn memloc 
           => let
                val memloc' = replacer memloc
              in
                if eq(memloc', memloc)
                  then replaceU replacer memloc
                  else memloc'
              end

      val rec sizeU = fn U {size, ...} => size
      and size = fn T {memloc, ...} => sizeU memloc
      val rec classU = fn U {class, ...} => class
      and class = fn T {memloc, ...} => classU memloc

      fun scaleImmediate (imm, scale) =
        case Immediate.destruct imm of
           Immediate.Word w => Immediate.word (WordX.mul (w, 
                                                          Scale.toWordX scale, 
                                                          {signed = true}))
         | _ => Error.bug "x86.MemLoc.scaleImmediate"

      fun addImmediate (imm1, imm2) =
        case (Immediate.destruct imm1, Immediate.destruct imm2) of
           (Immediate.Word w1, Immediate.Word w2) => Immediate.word (WordX.add (w1, w2))
         | _ => Error.bug "x86.MemLoc.scaleImmediate"

      val imm = fn {base, index, scale, size, class} 
        => construct (U {immBase = SOME base,
                         memBase = NONE,
                         immIndex = SOME (scaleImmediate (index, scale)),
                         memIndex = NONE,
                         scale = scale,
                         size = size,
                         class = class})
      val basic = fn {base, index, scale, size, class} 
        => construct (U {immBase = SOME base,
                         memBase = NONE,
                         immIndex = NONE,
                         memIndex = SOME index, 
                         scale = scale,
                         size = size,
                         class = class})
      val simple = fn {base, index, scale, size, class} 
        => construct (U {immBase = NONE,
                         memBase = SOME base,
                         immIndex = SOME (scaleImmediate (index, scale)),
                         memIndex = NONE,
                         scale = scale,
                         size = size,
                         class = class})

      val complex = fn {base, index, scale, size, class} 
        => construct (U {immBase = NONE,
                         memBase = SOME base,
                         immIndex = NONE,
                         memIndex = SOME index,
                         scale = scale,
                         size = size,
                         class = class})
      val shift = fn {origin, disp, scale, size} 
        => let  
              val disp = scaleImmediate (disp, scale)
              val U {immBase, memBase, 
                     immIndex, memIndex, 
                     scale, class, ...} =
                 destruct origin
           in
              construct (U {immBase = immBase,
                            memBase = memBase,
                            immIndex = 
                            case immIndex of
                               NONE => SOME disp
                             | SOME immIndex => SOME (addImmediate (immIndex, disp)),
                            memIndex = memIndex,
                            scale = scale,
                            size = size,
                            class = class})
           end

      local
        val num : int ref = ref 0
      in
        val temp = fn {size} => (Int.inc num;
                                 imm {base = Immediate.zero,
                                      index = Immediate.int (!num),
                                      scale = Scale.One,
                                      size = size,
                                      class = Class.Temp})
      end

      (*
       * Static memory locations
       *)
      fun makeContents {base, size, class}
        = imm {base = base,
               index = Immediate.zero,
               scale = Scale.Four,
               size = size,
               class = class}
(*
      local
        datatype z = datatype CType.t
        datatype z = datatype Size.t
      in
         fun cReturnTempContents sizes =
            (List.rev o #1)
            (List.fold
             (sizes, ([],0), fn (size, (contents, index)) =>
              ((cReturnTempContent (index, size))::contents,
               index + Size.toBytes size)))
         fun cReturnTempContent size =
            List.first(cReturnTempContents [size])
         val cReturnTempContents = fn size =>
            cReturnTempContents (
            case size of
               Int s => let datatype z = datatype IntSize.t
                        in case s of
                             I8 => [BYTE]
                           | I16 => [WORD]
                           | I32 => [LONG]
                           | I64 => [LONG, LONG]
                        end
             | Pointer => [LONG]
             | Real s => let datatype z = datatype RealSize.t
                         in case s of
                              R32 => [SNGL]
                            | R64 => [DBLE]
                         end
             | Word s => let datatype z = datatype WordSize.t
                         in case s of
                              W8 => [BYTE]
                            | W16 => [WORD]
                            | W32 => [LONG]
                         end)
      end
*)
    end

  local
    structure ClassElement =
      struct
        type t = MemLoc.Class.t
        val compare = MemLoc.Class.compare
        local
          fun make f = fn (a, b) => f (MemLoc.Class.counter a, MemLoc.Class.counter b)
        in
          val op < = make Int.<
          val op > = make Int.>
          val op >= = make Int.>=
          val op <= = make Int.<=
        end
        val min = fn (a, b) => if Int.<(MemLoc.Class.counter a, MemLoc.Class.counter b)
                                 then a
                                 else b
        val max = fn (a, b) => min (b, a)
        val equals = MemLoc.Class.eq
        val layout = MemLoc.Class.layout
      end
  in
    structure ClassSet = OrderedUniqueSet(open ClassElement)
  end
  local 
    structure MemLocElement =
      struct
        type t = MemLoc.t       
        val equals = MemLoc.eq
        val layout = MemLoc.layout
(*
        val compare = MemLoc.compare
        local
          fun make f = fn (a, b) => f (MemLoc.counter a, MemLoc.counter b)
        in
          val op < = make Int.<
          val op > = make Int.>
          val op >= = make Int.>=
          val op <= = make Int.<=
        end
        val min = fn (a, b) => if Int.<(MemLoc.counter a, MemLoc.counter b)
                                 then a
                                 else b
        val max = fn (a, b) => min (b, a)
        val hash = MemLoc.hash
*)
      end
  in
    structure MemLocSet = UnorderedSet(open MemLocElement)
(*
    structure MemLocSet = OrderedUniqueSet(open MemLocElement)
*)
(*
    structure MemLocSet' = UnorderedSet(open MemLocElement)
    structure MemLocSet = HashedUniqueSet(structure Set = MemLocSet'
                                          structure Element = MemLocElement)
*)
  end

  structure Operand =
    struct
      datatype t
        = Register of Register.t
        | FltRegister of FltRegister.t
        | Immediate of Immediate.t
        | Label of Label.t
        | Address of Address.t
        | MemLoc of MemLoc.t

      val size
        = fn Register r => SOME (Register.size r)
           | FltRegister _ => SOME Size.EXTD
           | Immediate _ => NONE
           | Label _ => NONE
           | Address _ => NONE
           | MemLoc m => SOME (MemLoc.size m)

      val layout
        = let
            open Layout
          in 
            fn Register r => Register.layout r
             | FltRegister f => FltRegister.layout f
             | Immediate i => seq [str "$", Immediate.layout i]
             | Label l => Label.layout l
             | Address a => Address.layout a
             | MemLoc m => MemLoc.layout m
          end
      val toString = Layout.toString o layout

      val eq
        = fn (Register r1,    Register r2)    => Register.eq(r1, r2)
           | (FltRegister f1, FltRegister f2) => FltRegister.eq(f1, f2)
           | (Immediate i1,   Immediate i2)   => Immediate.eq(i1, i2)
           | (Label l1,       Label l2)       => Label.equals(l1, l2)
           | (Address a1,     Address a2)     => Address.eq(a1, a2)
           | (MemLoc m1,      MemLoc m2)      => MemLoc.eq(m1, m2)
           | _                                => false

      val mayAlias
        = fn (Register r1,    Register r2)    => Register.eq(r1, r2)
           | (Register _,     _)              => false
           | (FltRegister f1, FltRegister f2) => FltRegister.eq(f1, f2)
           | (FltRegister _,  _)              => false
           | (Immediate i1,   Immediate i2)   => Immediate.eq(i1, i2)
           | (Immediate _,    _)              => false
           | (Label l1,       Label l2)       => Label.equals(l1, l2)
           | (Label _,        _)              => false
           | (Address _,      Address _)      => true
           | (Address _,      MemLoc _)       => true
           | (Address _,      _)              => false
           | (MemLoc m1,      MemLoc m2)      => MemLoc.mayAlias(m1, m2)
           | (MemLoc _,       Address _)      => true
           | (MemLoc _,       _)              => false

      val register = Register
      val deRegister
        = fn Register x => SOME x
           | _ => NONE
      val fltregister = FltRegister
      val deFltregister
        = fn FltRegister x => SOME x
           | _ => NONE
      val immediate = Immediate
      val deImmediate
        = fn Immediate x => SOME x
           | _ => NONE
      val immediate_word = immediate o Immediate.word
      val immediate_int' = immediate o Immediate.int'
      val immediate_int = immediate o Immediate.int
      val immediate_zero = immediate Immediate.zero
      val immediate_label = immediate o Immediate.label
      val label = Label
      val deLabel
        = fn Label x => SOME x
           | _ => NONE
      val address = Address
      val memloc = MemLoc
      fun memloc_label l =
         memloc (MemLoc.makeContents { base = Immediate.label l,
                                       size = Size.LONG,
                                       class = MemLoc.Class.Code })
      val deMemloc 
        = fn MemLoc x => SOME x
           | _ => NONE

      local
        val cReturnTemp = Label.fromString "cReturnTemp"
        fun cReturnTempContent (index, size) =
           MemLoc.imm
           {base = Immediate.label cReturnTemp,
            index = Immediate.int index,
            scale = Scale.One,
            size = size,
            class = MemLoc.Class.StaticTemp}
         datatype z = datatype CType.t
         datatype z = datatype Size.t
      in
         fun cReturnTemps ty =
            if RepType.isUnit ty
               then []
            else
               let
                  fun w (r, s) =
                     [{src = register r, dst = cReturnTempContent (0, s)}]
                  val w8 = w (Register.al, BYTE)
                  val w16 = w (Register.ax, WORD)
                  val w32 = w (Register.eax, LONG)
                  val w64 =[{src = register Register.eax,
                             dst = cReturnTempContent (0, LONG)},
                            {src = register Register.edx,
                             dst = cReturnTempContent (4, LONG)}]
               in
                  case RepType.toCType ty of
                     CPointer => w32
                   | Int8 => w8
                   | Int16 => w16
                   | Int32 => w32
                   | Int64 => w64
                   | Objptr => w32
                   | Real32 => [{src = fltregister FltRegister.top,
                                 dst = cReturnTempContent (0, SNGL)}]
                   | Real64 => [{src = fltregister FltRegister.top,
                                 dst = cReturnTempContent (0, DBLE)}]
                   | Word8 => w8
                   | Word16 => w16
                   | Word32 => w32
                   | Word64 => w64
               end
      end
    end

  structure Instruction =
    struct
      (* Integer binary arithmetic(w/o mult & div)/logic instructions. *)
      datatype binal
        = ADD (* signed/unsigned addition; p. 63 *)
        | ADC (* signed/unsigned addition with carry; p. 61 *)
        | SUB (* signed/unsigned subtraction; p. 713 *)
        | SBB (* signed/unsigned subtraction with borrow; p. 667 *)
        | AND (* logical and; p. 70 *)
        | OR  (* logical or; p. 499 *)
        | XOR (* logical xor; p. 758 *)
      val binal_layout
        = let
            open Layout
          in 
            fn ADD => str "add"
             | ADC => str "adc"
             | SUB => str "sub"
             | SBB => str "sbb"
             | AND => str "and"
             | OR  => str "or"
             | XOR => str "xor"
          end

      (* Integer multiplication and division. *)
      datatype md
        = IMUL (* signed multiplication (one operand form); p. 335 *)
        | MUL  (* unsigned multiplication; p. 488 *)
        | IDIV (* signed division; p. 332 *)
        | DIV  (* unsigned division; p. 188 *)
        | IMOD (* signed modulus; *)
        | MOD  (* unsigned modulus; *)
      val md_layout
        = let
            open Layout
          in 
            fn IMUL => str "imul"
             | MUL  => str "mul"
             | IDIV => str "idiv"
             | DIV  => str "div"
             | IMOD => str "imod"
             | MOD  => str "mod"
          end

      (* Integer unary arithmetic/logic instructions. *)
      datatype unal
        = INC (* increment by 1; p. 341 *)
        | DEC (* decrement by 1; p. 186 *)
        | NEG (* two's complement negation; p. 494 *)
        | NOT (* one's complement negation; p. 497 *)
      val unal_layout
        = let
            open Layout
          in 
            fn INC => str "inc"
             | DEC => str "dec"
             | NEG => str "neg"
             | NOT => str "not"
          end

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
      val sral_layout 
        = let
            open Layout
          in 
            fn SAL => str "sal"
             | SHL => str "shl"
             | SAR => str "sar"
             | SHR => str "shr"
             | ROL => str "rol"
             | RCL => str "rcl"
             | ROR => str "ror"
             | RCR => str "rcr"
          end

      (* Move with extention instructions. *)
      datatype movx
        = MOVSX (* move with sign extention; p. 481 *)
        | MOVZX (* move with zero extention; p. 486 *)
      val movx_layout
        = let
            open Layout
          in 
            fn MOVSX => str "movs"
             | MOVZX => str "movz"
          end

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

      val condition_negate
        = fn O   => NO  | NO  => O
           | B   => NB  | NB  => B
           | AE  => NAE | NAE => AE
           | C   => NC  | NC  => C
           | E   => NE  | NE  => E
           | Z   => NZ  | NZ  => Z
           | BE  => NBE | NBE => BE
           | A   => NA  | NA  => A
           | S   => NS  | NS  => S
           | P   => NP  | NP  => P
           | PE  => PO  | PO  => PE
           | L   => NL  | NL  => L
           | LE  => NLE | NLE => LE
           | G   => NG  | NG  => G
           | GE  => NGE | NGE => GE

      val condition_reverse
        = fn B   => A   | NB  => NA
           | AE  => BE  | NAE => NBE
           | E   => E   | NE  => NE
           | BE  => AE  | NBE => NAE
           | A   => B   | NA  => NB
           | L   => G   | NL  => NG
           | LE  => GE  | NLE => NGE
           | G   => L   | NG  => NL
           | GE  => LE  | NGE => NLE
           | c   => c

      local 
        open Layout
      in
        val rec condition_layout
        = fn O  => str "o"
           | B  => str "b"
           | AE => str "ae"
           | C  => str "c"
           | E  => str "e"
           | Z  => str "z"
           | BE => str "be"
           | A  => str "a"
           | S  => str "s"
           | P  => str "p"
           | PE => str "pe"
           | PO => str "po"
           | L  => str "l"
           | LE => str "le"
           | G  => str "g"
           | GE => str "ge"
           | c  => seq [str "n", condition_layout (condition_negate c)]
      end
      val condition_toString = Layout.toString o condition_layout

      (* Floating-point binary arithmetic instructions. *)
      datatype fbina
        = FADD  (* addition; p. 205 *)
        | FSUB  (* subtraction; p. 297 *)
        | FSUBR (* reversed subtraction; p. 301 *)
        | FMUL  (* multiplication; p. 256 *)
        | FDIV  (* division; p. 229 *)
        | FDIVR (* reversed division; p. 233 *)
      val fbina_layout
        = let
            open Layout
          in 
            fn FADD => str "fadd"
             | FSUB => str "fsub"
             | FSUBR => str "fsubr"
             | FMUL => str "fmul"
             | FDIV => str "fdiv"
             | FDIVR => str "fdivr"
          end
      val fbina_reverse
        = fn FADD  => FADD
           | FSUB  => FSUBR
           | FSUBR => FSUB
           | FMUL  => FMUL
           | FDIV  => FDIVR
           | FDIVR => FDIV

      (* Floating-point unary arithmetic instructions. *)
      datatype funa
        = F2XM1   (* compute 2^x-1; p. 201 *)
        | FABS    (* absolute value; p. 203 *)
        | FCHS    (* change sign; p. 214 *)
        | FSQRT   (* square root; p. 284 *)
        | FSIN    (* sine; p. 280 *)
        | FCOS    (* cosine; p. 226 *)
        | FRNDINT (* round to integer; p. 271 *)
      val funa_layout
        = let
            open Layout
          in 
            fn F2XM1 => str "f2xm1"
             | FABS => str "fabs"
             | FCHS => str "fchs"
             | FSQRT => str "fsqrt"
             | FSIN => str "fsin"
             | FCOS => str "fcos"
             | FRNDINT => str "frndint"
          end

      (* Floating-point binary arithmetic stack instructions. *)
      datatype fbinas
        = FSCALE (* scale; p. 278 *)
        | FPREM (* partial remainder; p. 263 *)
        | FPREM1 (* IEEE partial remainder; p. 266 *)
      val fbinas_layout
        = let
            open Layout
          in
            fn FSCALE => str "fscale"
             | FPREM=> str "fprem"
             | FPREM1 => str "fprem1"
          end

      (* floating point binary arithmetic stack pop instructions. *)
      datatype fbinasp
        = FYL2X (* compute y * log_2 x; p. 327 *)
        | FYL2XP1 (* compute y * log_2 (x + 1.0); p. 329 *)
        | FPATAN (* partial arctangent; p. 261 *)
      val fbinasp_layout
        = let
            open Layout
          in
            fn FYL2X => str "fyl2x"
             | FYL2XP1 => str "fyl2xp1"
             | FPATAN => str "fpatan"
          end

      (* Floating-point constants. *)
      datatype fldc
        = ONE  (* +1.0; p. 250 *) 
        | ZERO (* +0.0; p. 250 *)
        | PI   (* pi; p. 250 *)
        | L2E  (* log_2 e; p. 250 *)
        | LN2  (* log_e 2; p. 250 *)
        | L2T  (* log_2 10; p. 250 *)
        | LG2  (* log_10 2; p. 250 *)
      val fldc_layout
        = let
            open Layout
          in 
            fn ONE => str "fld1"
             | ZERO => str "fldz"
             | PI => str "fldpi"
             | L2E => str "fldl2e"
             | LN2 => str "fldln2"
             | L2T => str "fldl2t"
             | LG2 => str "fldlg2"
          end

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
        (* Jump.
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
        (* Pseudo-push a value onto a stack.
         *)
        | pPUSH of {src: Operand.t,
                    base: Operand.t,
                    size: Size.t}
        (* Pseudo-pop a value from a stack.
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
        (* Pseudo floating-point partial tangent instruction.
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

      val layout
        = let
            open Layout
            fun bin (oper, size, oper1, oper2)
              = seq [oper,
                     size,
                     str " ",
                     oper1,
                     str ",",
                     oper2]
            fun un (oper, size, oper1)
              = seq [oper,
                     size,
                     str " ",
                     oper1]
          in 
            fn NOP
             => str "nop"
             | HLT
             => str "hlt"
             | BinAL {oper, src, dst, size}
             => bin (binal_layout oper, 
                     Size.layout size,
                     Operand.layout src,
                     Operand.layout dst)
             | pMD {oper, src, dst, size}
             => bin (md_layout oper, 
                     Size.layout size,
                     Operand.layout src,
                     Operand.layout dst)
             | MD {oper, src, size}
             => un (md_layout oper, 
                    Size.layout size,
                    Operand.layout src)
             | IMUL2 {src, dst, size}
             => bin (str "imul",
                     Size.layout size,
                     Operand.layout src,
                     Operand.layout dst)
             | UnAL {oper, dst, size}
             => un (unal_layout oper, 
                    Size.layout size, 
                    Operand.layout dst)
             | SRAL {oper, count, dst, size}
             => bin (sral_layout oper, 
                     Size.layout size, 
                     Operand.layout count,
                     Operand.layout dst)
             | CMP {src1, src2, size}
             => bin (str "cmp", 
                     Size.layout size,
                     Operand.layout src2,
                     Operand.layout src1)
             | TEST {src1, src2, size}
             => bin (str "test", 
                     Size.layout size,
                     Operand.layout src2,
                     Operand.layout src1)
             | SETcc {condition, dst, ...}
             => seq [str "set", 
                     condition_layout condition, 
                     str " ",
                     Operand.layout dst]
             | JMP {target, absolute}
             => seq [str "jmp ", 
                     if absolute then str "*" else empty, 
                     Operand.layout target]
             | Jcc {condition, target}
             => seq [str "j", 
                     condition_layout condition, 
                     str " ",
                     Operand.layout target]
             | CALL {target, absolute}
             => seq [str "call ", 
                     if absolute then str "*" else empty, 
                     Operand.layout target]
             | RET {src}
             => seq [str "ret", 
                     case src
                       of NONE => empty
                        | SOME src => seq [str " ", Operand.layout src]]
             | MOV {src, dst, size}
             => bin (str "mov", 
                     Size.layout size,
                     Operand.layout src,
                     Operand.layout dst)
             | CMOVcc {condition, src, dst, size}
             => seq [str "cmov", 
                     condition_layout condition,
                     Size.layout size, 
                     str " ",
                     Operand.layout src,
                     str ",",
                     Operand.layout dst]
             | XCHG {src, dst, size}
             => bin (str "xchg", 
                     Size.layout size,
                     Operand.layout src,
                     Operand.layout dst)
             | pPUSH {src, base, size}
             => seq [str "ppush", 
                     Size.layout size, 
                     str " [",
                     Operand.layout base, 
                     str "] ",
                     Operand.layout src]
             | pPOP {dst, base, size}
             => seq [str "ppop", 
                     Size.layout size, 
                     str " [",
                     Operand.layout base, 
                     str " ]",
                     Operand.layout dst]
             | PUSH {src, size}
             => seq [str "push", 
                     Size.layout size, 
                     str " ",
                     Operand.layout src]
             | POP {dst, size}
             => seq [str "pop", 
                     Size.layout size, 
                     str " ",
                     Operand.layout dst]
             | CX {size}
             => (case size
                   of Size.BYTE => str "cbtw"
                    | Size.WORD => str "cwtd"
                    | Size.LONG => str "cltd"
                    | _ => Error.bug "x86.Instruction.layout: CX,unsupported conversion")
             | MOVX {oper, src, srcsize, dst, dstsize}
             => bin (movx_layout oper, 
                     seq [Size.layout srcsize, 
                          Size.layout dstsize],
                     Operand.layout src, 
                     Operand.layout dst)
             | XVOM {src, srcsize, dst, dstsize}
             => bin (str "xvom",
                     seq [Size.layout srcsize,
                          Size.layout dstsize],
                     Operand.layout src,
                     Operand.layout dst)
             | LEA {src, dst, size}
             => bin (str "lea", 
                     Size.layout size,
                     Operand.layout src,
                     Operand.layout dst)
             | pFMOV {src, dst, size}
             => bin (str "fmov", 
                     Size.layout size,
                     Operand.layout src,
                     Operand.layout dst)
             | pFMOVX {src, dst, srcsize, dstsize}
             => bin (str "fmovx", 
                     seq [Size.layout srcsize, 
                          Size.layout dstsize],
                     Operand.layout src,
                     Operand.layout dst)
             | pFXVOM {src, dst, srcsize, dstsize}
             => bin (str "fmov", 
                     seq [Size.layout srcsize,
                          Size.layout dstsize],
                     Operand.layout src,
                     Operand.layout dst)
             | pFLDC {oper, dst, size}
             => un (fldc_layout oper,
                    Size.layout size,
                    Operand.layout dst)
             | pFMOVFI {src, dst, srcsize, dstsize}
             => bin (str "fmovfi", 
                     seq [Size.layout srcsize,
                          Size.layout dstsize],
                     Operand.layout src,
                     Operand.layout dst)
             | pFMOVTI {src, dst, srcsize, dstsize}
             => bin (str "fmovti", 
                     seq [Size.layout srcsize, 
                          Size.layout dstsize],
                     Operand.layout src,
                     Operand.layout dst)
             | pFCOM {src1, src2, size}
             => bin (str "fcom",
                     Size.layout size,
                     Operand.layout src1,
                     Operand.layout src2)
             | pFUCOM {src1, src2, size}
             => bin (str "fucom",
                     Size.layout size,
                     Operand.layout src1,
                     Operand.layout src2)
             | pFBinA {oper, src, dst, size}
             => bin (fbina_layout oper, 
                     Size.layout size,
                     Operand.layout src,
                     Operand.layout dst)
             | pFUnA {oper, dst, size}
             => un (funa_layout oper,
                    Size.layout size,
                    Operand.layout dst)
             | pFPTAN {dst, size}
             => un (str "fptan",
                    Size.layout size,
                    Operand.layout dst)
             | pFBinAS {oper, src, dst, size}
             => bin (fbinas_layout oper,
                     Size.layout size,
                     Operand.layout src,
                     Operand.layout dst)
             | pFBinASP {oper, src, dst, size}
             => bin (fbinasp_layout oper,
                     Size.layout size,
                     Operand.layout src,
                     Operand.layout dst)
             | FLD {src, size}
             => un (str "fld", 
                    case src
                      of Operand.FltRegister _ => empty
                       | _ => Size.layout size,
                    Operand.layout src)
             | FST {dst, size, pop}
             => un (str "fst", 
                    seq [if pop then str "p" else empty, 
                         case dst
                           of Operand.FltRegister _ => empty
                            | _ => Size.layout size],
                    Operand.layout dst)
             | FILD {src, size}
             => un (str "fild", 
                    Size.layout size, 
                    Operand.layout src)
             | FIST {dst, size, pop}
             => un (str "fist", 
                    seq [if pop then str "p" else empty, 
                         Size.layout size],
                    Operand.layout dst)
             | FXCH {src}
             => seq [str "fxch ",
                     Operand.layout src]
             | FLDC {oper}
             => seq [fldc_layout oper]
             | FLDCW {src}
             => seq [str "fldcw ",
                     Operand.layout src]
             | FSTCW {dst, check}
             => seq [if check then str "fstcw " else str "fnstcw ",
                     Operand.layout dst]
             | FSTSW {dst, check}
             => seq [if check then str "fstsw " else str "fnstsw ",
                     Operand.layout dst]
             | FCOM {src, size, pop, pop'}
             => seq [str "fcom",
                     if pop andalso pop' 
                       then str "pp"
                       else seq [if pop then str "p" else empty,
                                 case src
                                   of Operand.FltRegister _
                                    => empty
                                    | _ => Size.layout size,
                                 str " ",
                                 Operand.layout src]]
             | FUCOM {src, pop, pop'}
             => seq [str "fucom",
                     if pop andalso pop' 
                       then str "pp"
                       else seq [if pop then str "p " else str " ",
                                 Operand.layout src]]
             | FBinA {oper, src, dst, size, pop}
             => seq [fbina_layout oper,
                     case src
                       of Operand.FltRegister _ 
                        => seq [if pop then str "p " else str " ",
                                Operand.layout src, 
                                str ", ",
                                Operand.layout dst]
                        | _ 
                        => seq [Size.layout size, 
                                str " ",
                                Operand.layout src]]
             | FUnA {oper}
             => seq [funa_layout oper]
             | FPTAN 
             => seq [str "fptan"]
             | FBinAS {oper}
             => seq [fbinas_layout oper]
             | FBinASP {oper}
             => seq [fbinasp_layout oper]
          end
      val toString = Layout.toString o layout

      val uses_defs_kills
        = fn NOP
           => {uses = [], defs = [], kills = []}
           | HLT
           => {uses = [], defs = [], kills = []}
           | BinAL {src, dst, ...}
           => {uses = [src, dst], defs = [dst], kills = []}
           | pMD {src, dst, ...}
           => {uses = [src, dst], defs = [dst], kills = []}
           | MD {oper, src, size}
           => let
                val (hi,lo) 
                  = case size
                      of Size.BYTE 
                       => (Register.T {reg = Register.EAX, part = Register.H},
                           Register.T {reg = Register.EAX, part = Register.L})
                       | Size.WORD 
                       => (Register.T {reg = Register.EDX, part = Register.X},
                           Register.T {reg = Register.EAX, part = Register.X})
                       | Size.LONG
                       => (Register.T {reg = Register.EDX, part = Register.E},
                           Register.T {reg = Register.EAX, part = Register.E})
                       | _ => Error.bug "x86.Instruction.uses_defs: MD, size"
              in 
                if oper = IMUL orelse oper = MUL
                  then {uses = [src, Operand.register lo],
                        defs = [Operand.register hi, Operand.register lo],
                        kills = []}
                  else {uses = [src, Operand.register hi, Operand.register lo],
                        defs = [Operand.register hi, Operand.register lo],
                        kills = []}
              end
           | IMUL2 {src, dst, ...}
           => {uses = [src, dst], defs = [dst], kills = []}
           | UnAL {dst, ...}
           => {uses = [dst], defs = [dst], kills = []}
           | SRAL {count, dst, size, ...}
           => if isSome (Operand.deMemloc count)
                then let
                       val reg
                         = case size
                             of Size.BYTE 
                               => Register.T {reg = Register.ECX, 
                                              part = Register.L}
                               | Size.WORD 
                               => Register.T {reg = Register.ECX, 
                                              part = Register.X}
                               | Size.LONG
                               => Register.T {reg = Register.ECX, 
                                              part = Register.E}
                               | _ => Error.bug "x86.Instruction.uses_defs: SRAL, size"
                     in
                       {uses = [count, dst, Operand.register reg], 
                        defs = [dst], 
                        kills = []}
                     end
                else {uses = [count, dst], 
                      defs = [dst], 
                      kills = []}
           | CMP {src1, src2, ...}
           => {uses = [src1, src2], defs = [], kills = []}
           | TEST {src1, src2, ...}
           => {uses = [src1, src2], defs = [], kills = []}
           | SETcc {dst, ...}
           => {uses = [], defs = [dst], kills = []}
           | JMP {target, ...}
           => {uses = [target], defs = [], kills = []}
           | Jcc {target, ...}
           => {uses = [target], defs = [], kills = []}
           | CALL {target, ...}
           => {uses = [target], defs = [], kills = []}
           | RET {src}
           => {uses = case src of NONE => [] | SOME src => [src], 
               defs = [], 
               kills = []}
           | MOV {src, dst, ...}
           => {uses = [src], defs = [dst], kills = []}
           | CMOVcc {src, dst, ...}
           => {uses = [src], defs = [dst], kills = []}
           | XCHG {src, dst, ...}
           => {uses = [src,dst], defs = [src,dst], kills = []}
           | pPUSH {src, base, size, ...}
           => {uses = [src,base], 
               defs = base::
                      (case base
                         of Operand.MemLoc base
                          => [Operand.MemLoc 
                              (MemLoc.simple {base = base,
                                              index = Immediate.zero,
                                              size = size,
                                              scale = Scale.One,
                                              class = MemLoc.Class.CStack})]
                          | _ => []),
               kills = []}
           | pPOP {dst, base, size, ...}
           => {uses = base::
                      (case base
                         of Operand.MemLoc base
                          => [Operand.MemLoc 
                              (MemLoc.simple {base = base,
                                              index = Immediate.zero,
                                              size = size,
                                              scale = Scale.One,
                                              class = MemLoc.Class.CStack})]
                          | _ => []),
               defs = [dst,base],
               kills = []}
           | PUSH {src, ...}
           => {uses = [src, Operand.register Register.esp],
               defs = [Operand.register Register.esp,
                       Operand.address (Address.T {disp = NONE,
                                                   base = SOME Register.esp,
                                                   index = NONE,
                                                   scale = NONE})], 
               kills = []}
           | POP {dst, ...}
           => {uses = [Operand.register Register.esp,
                       Operand.address (Address.T {disp = NONE,
                                                   base = SOME Register.esp,
                                                   index = NONE,
                                                   scale = NONE})],
               defs = [dst, Operand.register Register.esp], 
               kills = []}
           | CX {size}
           => let
                val (hi,lo) 
                  = case size
                      of Size.BYTE 
                       => (Register.T {reg = Register.EAX, part = Register.H},
                           Register.T {reg = Register.EAX, part = Register.L})
                       | Size.WORD 
                       => (Register.T {reg = Register.EDX, part = Register.X},
                           Register.T {reg = Register.EAX, part = Register.X})
                       | Size.LONG
                       => (Register.T {reg = Register.EDX, part = Register.E},
                           Register.T {reg = Register.EAX, part = Register.E})
                       | _ => Error.bug "x86.Instruction.uses_defs: CX, size"
              in
                {uses = [Operand.register lo],
                 defs = [Operand.register hi, Operand.register lo], 
                 kills = []}
              end
           | MOVX {src, dst, ...}
           => {uses = [src], defs = [dst], kills = []}
           | XVOM {src, dst, ...}
           => {uses = [src], defs = [dst], kills = []}
           | LEA {src, dst, ...}
           => {uses = [src], defs = [dst], kills = []}
           | pFMOV {src, dst, ...}
           => {uses = [src], defs = [dst], kills = []}
           | pFMOVX {src, dst, ...}
           => {uses = [src], defs = [dst], kills = []}
           | pFXVOM {src, dst, ...}
           => {uses = [src], defs = [dst], kills = []}
           | pFLDC {dst, ...}
           => {uses = [], defs = [dst], kills = []}
           | pFMOVFI {src, dst, ...}
           => {uses = [src], defs = [dst], kills = []}
           | pFMOVTI {src, dst, ...}
           => {uses = [src], defs = [dst], kills = []}
           | pFCOM {src1, src2, ...}
           => {uses = [src1, src2], defs = [], kills = []}
           | pFUCOM {src1, src2, ...}
           => {uses = [src1, src2], defs = [], kills = []}
           | pFBinA {src, dst, ...}
           => {uses = [src, dst], defs = [dst], kills = []}
           | pFUnA {dst, ...}
           => {uses = [dst], defs = [dst], kills = []}
           | pFPTAN {dst, ...}
           => {uses = [dst], defs = [dst], kills = []}
           | pFBinAS {src, dst, ...}
           => {uses = [src, dst], defs = [dst], kills = []}
           | pFBinASP {src, dst, ...}
           => {uses = [src, dst], 
               defs = [dst], 
               kills = if Operand.eq(src,dst)
                         then []
                         else [src]}
           | FLD {src, ...}
           => {uses = [src], 
               defs = [Operand.fltregister FltRegister.top], 
               kills = []}
           | FST {dst, pop, ...}
           => {uses = [Operand.fltregister FltRegister.top], 
               defs = [dst], 
               kills = if pop 
                         then [Operand.fltregister FltRegister.top] 
                         else []}
           | FILD {src, ...}
           => {uses = [src], 
               defs = [Operand.fltregister FltRegister.top], 
               kills = []}
           | FIST {dst, pop, ...}
           => {uses = [Operand.fltregister FltRegister.top], 
               defs = [dst], 
               kills = if pop 
                         then [Operand.fltregister FltRegister.top] 
                         else []}
           | FXCH {src}
           => {uses = [src, Operand.fltregister FltRegister.top],
               defs = [src, Operand.fltregister FltRegister.top], 
               kills = []}
           | FLDC {...}
           => {uses = [], 
               defs = [Operand.fltregister FltRegister.top], 
               kills = []}
           | FLDCW {src}
           => {uses = [src], defs = [], kills = []}
           | FSTCW {dst, ...}
           => {uses = [], defs = [dst], kills = []}
           | FSTSW {dst, ...}
           => {uses = [], defs = [dst], kills = []}
           | FCOM {src, pop, pop', ...}
           => {uses = [src, Operand.fltregister FltRegister.top],
               defs = [],
               kills = if pop andalso pop'
                         then [Operand.fltregister FltRegister.top, src]
                       else if pop
                         then [Operand.fltregister FltRegister.top]
                       else []}
           | FUCOM {src, pop, pop'}
           => {uses = [src, Operand.fltregister FltRegister.top],
               defs = [],
               kills = if pop andalso pop'
                         then [Operand.fltregister FltRegister.top, src]
                       else if pop
                         then [Operand.fltregister FltRegister.top]
                       else []}
           | FBinA {src, dst, pop, ...}
           => {uses = [src, dst], 
               defs = [dst], 
               kills = if pop then [src] else []}
           | FUnA {...}
           => {uses = [Operand.fltregister FltRegister.top],
               defs = [Operand.fltregister FltRegister.top], kills = []}
           | FPTAN
           => {uses = [Operand.fltregister FltRegister.top],
               defs = [Operand.fltregister FltRegister.top], kills = []}
           | FBinAS {...}
           => {uses = [Operand.fltregister FltRegister.top,
                       Operand.fltregister FltRegister.one],
               defs = [Operand.fltregister FltRegister.top,
                       Operand.fltregister FltRegister.one], 
               kills = []}
           | FBinASP {...}
           => {uses = [Operand.fltregister FltRegister.top,
                       Operand.fltregister FltRegister.one],
               defs = [Operand.fltregister FltRegister.one],
               kills = [Operand.fltregister FltRegister.top]}

      val hints
        = fn pMD {dst, size, ...}
           => let
                val (hi,lo) 
                  = case size
                      of Size.BYTE 
                       => (Register.T {reg = Register.EAX, part = Register.H},
                           Register.T {reg = Register.EAX, part = Register.L})
                       | Size.WORD 
                       => (Register.T {reg = Register.EDX, part = Register.X},
                           Register.T {reg = Register.EAX, part = Register.X})
                       | Size.LONG
                       => (Register.T {reg = Register.EDX, part = Register.E},
                           Register.T {reg = Register.EAX, part = Register.E})
                       | _ => Error.bug "x86.Instruction.hints: MD, size"

                val temp = MemLoc.temp {size = size}
              in 
                [(temp, hi),
                 (case Operand.deMemloc dst
                    of SOME memloc => (memloc, lo)
                     | NONE => (temp, lo))]
              end
          | MD {src, size, ...}
           => let
                val (hi,lo) 
                  = case size
                      of Size.BYTE 
                       => (Register.T {reg = Register.EAX, part = Register.H},
                           Register.T {reg = Register.EAX, part = Register.L})
                       | Size.WORD 
                       => (Register.T {reg = Register.EDX, part = Register.X},
                           Register.T {reg = Register.EAX, part = Register.X})
                       | Size.LONG
                       => (Register.T {reg = Register.EDX, part = Register.E},
                           Register.T {reg = Register.EAX, part = Register.E})
                       | _ => Error.bug "x86.Instruction.hints: MD, size"

                val temp = MemLoc.temp {size = size}
              in 
                [(temp, hi),
                 (case Operand.deMemloc src
                    of SOME memloc => (memloc, lo)
                     | NONE => (temp, lo))]
              end
           | SRAL {count, size, ...}
           => (case Operand.deMemloc count
                 of SOME memloc 
                  => let
                       val reg
                         = case size
                             of Size.BYTE 
                               => Register.T {reg = Register.ECX, 
                                              part = Register.L}
                               | Size.WORD 
                               => Register.T {reg = Register.ECX, 
                                              part = Register.X}
                               | Size.LONG
                               => Register.T {reg = Register.ECX, 
                                              part = Register.E}
                               | _ => Error.bug "x86.Instruction.hints: SRAL, size"
                     in
                       [(memloc, reg)]
                     end
                  | NONE => [])
           | pPUSH {base, ...}
           => (case Operand.deMemloc base
                 of SOME base => [(base,Register.esp)]
                  | NONE => [])
           | pPOP {base, ...}
           => (case Operand.deMemloc base
                 of SOME base => [(base,Register.esp)]
                  | NONE => [])
           | PUSH {...}
           => let
                val temp = MemLoc.temp {size = Size.LONG}
              in
                [(temp,Register.esp)]
              end
           | POP {...}
           => let
                val temp = MemLoc.temp {size = Size.LONG}
              in
                [(temp,Register.esp)]
              end
           | _ => []

      val srcs_dsts
        = fn NOP
           => {srcs = NONE, dsts = NONE}
           | HLT
           => {srcs = NONE, dsts = NONE}
           | BinAL {src, dst, ...}
           => {srcs = SOME [src, dst], dsts = SOME [dst]}
           | pMD {src, dst, ...}
           => {srcs = SOME [src, dst], dsts = SOME [dst]}
           | MD {oper, src, size, ...}
           => let
                val (hi,lo) 
                  = case size
                      of Size.BYTE 
                       => (Register.T {reg = Register.EAX, part = Register.H},
                           Register.T {reg = Register.EAX, part = Register.L})
                       | Size.WORD 
                       => (Register.T {reg = Register.EDX, part = Register.X},
                           Register.T {reg = Register.EAX, part = Register.X})
                       | Size.LONG
                       => (Register.T {reg = Register.EDX, part = Register.E},
                           Register.T {reg = Register.EAX, part = Register.E})
                       | _ => Error.bug "x86.Instruction.srcs_dsts: MD, size"
              in 
                if oper = IMUL orelse oper = MUL
                  then {srcs = SOME [src, 
                                     Operand.register lo],
                        dsts = SOME [Operand.register hi, 
                                     Operand.register lo]}
                  else {srcs = SOME [src, 
                                     Operand.register hi, 
                                     Operand.register lo],
                        dsts = SOME [Operand.register hi, 
                                     Operand.register lo]}
              end
           | IMUL2 {src, dst, ...}
           => {srcs = SOME [src, dst], dsts = SOME [dst]}
           | UnAL {dst, ...}
           => {srcs = SOME [dst], dsts = SOME [dst]}
           | SRAL {count, dst, size, ...}
           => if isSome (Operand.deMemloc count)
                then let
                       val reg
                         = case size
                             of Size.BYTE 
                               => Register.T {reg = Register.ECX, 
                                              part = Register.L}
                               | Size.WORD 
                               => Register.T {reg = Register.ECX, 
                                              part = Register.X}
                               | Size.LONG
                               => Register.T {reg = Register.ECX, 
                                              part = Register.E}
                               | _ => Error.bug "x86.Instruction.srcs_dsts: SRAL, size"
                     in
                       {srcs = SOME [count, dst, Operand.register reg], 
                        dsts = SOME [dst]} 
                     end
                else {srcs = SOME [count, dst],
                      dsts = SOME [dst]}
           | CMP {src1, src2, ...}
           => {srcs = SOME [src1, src2], dsts = NONE}
           | TEST {src1, src2, ...}
           => {srcs = SOME [src1, src2], dsts = NONE}
           | SETcc {dst, ...}
           => {srcs = NONE, dsts = SOME [dst]}
           | JMP {target, ...}
           => {srcs = SOME [target], dsts = NONE}
           | Jcc {target, ...}
           => {srcs = SOME [target], dsts = NONE}
           | CALL {target, ...}
           => {srcs = SOME [target], dsts = NONE}
           | RET {src}
           => {srcs = case src of NONE => NONE | SOME src => SOME [src], 
               dsts = NONE} 
           | MOV {src, dst, ...}
           => {srcs = SOME [src], dsts = SOME [dst]}
           | CMOVcc {src, dst, ...}
           => {srcs = SOME [src], dsts = SOME [dst]}
           | XCHG {src, dst, ...}
           => {srcs = SOME [src,dst], dsts = SOME [src,dst]}
           | pPUSH {src, base, ...}
           => {srcs = SOME [src,base], dsts = SOME [base]}
           | pPOP {dst, base, ...}
           => {srcs = SOME [base], dsts = SOME [dst,base]}
           | PUSH {src, ...}
           => {srcs = SOME [src, Operand.register Register.esp],
               dsts = SOME [Operand.register Register.esp]}
           | POP {dst, ...}
           => {srcs = SOME [Operand.register Register.esp],
               dsts = SOME [dst, Operand.register Register.esp]}
           | CX {size, ...}
           => let
                val (hi,lo) 
                  = case size
                      of Size.BYTE 
                       => (Register.T {reg = Register.EAX, part = Register.H},
                           Register.T {reg = Register.EAX, part = Register.L})
                       | Size.WORD 
                       => (Register.T {reg = Register.EDX, part = Register.X},
                           Register.T {reg = Register.EAX, part = Register.X})
                       | Size.LONG
                       => (Register.T {reg = Register.EDX, part = Register.E},
                           Register.T {reg = Register.EAX, part = Register.E})
                       | _ => Error.bug "x86.Instruction.srcs_dsts: CX, size"
              in
                {srcs = SOME [Operand.register lo],
                 dsts = SOME [Operand.register hi, Operand.register lo]}
              end
           | MOVX {src, dst, ...}
           => {srcs = SOME [src], dsts = SOME [dst]}
           | XVOM {src, dst, ...}
           => {srcs = SOME [src], dsts = SOME [dst]}
           | LEA {src, dst, ...}
           => {srcs = SOME [src], dsts = SOME [dst]}
           | pFMOV {src, dst, ...}
           => {srcs = SOME [src], dsts = SOME [dst]}
           | pFMOVX {src, dst, ...}
           => {srcs = SOME [src], dsts = SOME [dst]}
           | pFXVOM {src, dst, ...}
           => {srcs = SOME [src], dsts = SOME [dst]}
           | pFLDC {dst, ...}
           => {srcs = SOME [], dsts = SOME [dst]}
           | pFMOVFI {src, dst, ...}
           => {srcs = SOME [src], dsts = SOME [dst]}
           | pFMOVTI {src, dst, ...}
           => {srcs = SOME [src], dsts = SOME [dst]}
           | pFCOM {src1, src2, ...}
           => {srcs = SOME [src1, src2], dsts = NONE}
           | pFUCOM {src1, src2, ...}
           => {srcs = SOME [src1, src2], dsts = NONE}
           | pFBinA {src, dst, ...}
           => {srcs = SOME [src, dst], dsts = SOME [dst]}
           | pFUnA {dst, ...}
           => {srcs = SOME [dst], dsts = SOME [dst]}
           | pFPTAN {dst, ...}
           => {srcs = SOME [dst], dsts = SOME [dst]}
           | pFBinAS {src, dst, ...}
           => {srcs = SOME [src, dst], dsts = SOME [dst]}
           | pFBinASP {src, dst, ...}
           => {srcs = SOME [src, dst], 
               dsts = SOME [dst]}
           | FLD {src, ...}
           => {srcs = SOME [src], 
               dsts = SOME [Operand.fltregister FltRegister.top]}
           | FST {dst, ...}
           => {srcs = SOME [Operand.fltregister FltRegister.top], 
               dsts = SOME [dst]}
           | FILD {src, ...}
           => {srcs = SOME [src], 
               dsts = SOME [Operand.fltregister FltRegister.top]}
           | FIST {dst, ...}
           => {srcs = SOME [Operand.fltregister FltRegister.top], 
               dsts = SOME [dst]}
           | FXCH {src}
           => {srcs = SOME [src, Operand.fltregister FltRegister.top],
               dsts = SOME [src, Operand.fltregister FltRegister.top]}
           | FLDC {...}
           => {srcs = NONE, 
               dsts = SOME [Operand.fltregister FltRegister.top]}
           | FLDCW {src}
           => {srcs = SOME [src], dsts = NONE}
           | FSTCW {dst, ...}
           => {srcs = NONE, dsts = SOME [dst]}
           | FSTSW {dst, ...}
           => {srcs = NONE, dsts = SOME [dst]}
           | FCOM {src, ...}
           => {srcs = SOME [src, Operand.fltregister FltRegister.top],
               dsts = NONE}
           | FUCOM {src, ...}
           => {srcs = SOME [src, Operand.fltregister FltRegister.top],
               dsts = NONE}
           | FBinA {src, dst, ...}
           => {srcs = SOME [src, dst], 
               dsts = SOME [dst]}
           | FUnA {...}
           => {srcs = SOME [Operand.fltregister FltRegister.top],
               dsts = SOME [Operand.fltregister FltRegister.top]}
           | FPTAN
           => {srcs = SOME [Operand.fltregister FltRegister.top],
               dsts = SOME [Operand.fltregister FltRegister.top]}
           | FBinAS {...}
           => {srcs = SOME [Operand.fltregister FltRegister.top,
                            Operand.fltregister FltRegister.one],
               dsts = SOME [Operand.fltregister FltRegister.top,
                            Operand.fltregister FltRegister.one]}
           | FBinASP {...}
           => {srcs = SOME [Operand.fltregister FltRegister.top,
                            Operand.fltregister FltRegister.one],
               dsts = SOME [Operand.fltregister FltRegister.one]}

      fun replace replacer
        = fn NOP
           => NOP
           | HLT
           => HLT
           | BinAL {oper, src, dst, size}
           => BinAL {oper = oper,
                     src = replacer {use = true, def = false} src,
                     dst = replacer {use = true, def = true} dst,
                     size = size}
           | pMD {oper, src, dst, size}
           => pMD {oper = oper,
                   src = replacer {use = true, def = false} src,
                   dst = replacer {use = true, def = true} dst,
                   size = size}
           | MD {oper, src, size}
           => MD {oper = oper,
                  src = replacer {use = true, def = false} src,
                  size = size}
           | IMUL2 {src, dst, size}
           => IMUL2 {src = replacer {use = true, def = false} src,
                     dst = replacer {use = true, def = true} dst,
                     size = size}
           | UnAL {oper, dst, size}
           => UnAL {oper = oper,
                    dst = replacer {use = true, def = true} dst,
                    size = size}
           | SRAL {oper, count, dst, size}
           => SRAL {oper = oper,
                    count = replacer {use = true, def = false} count,
                    dst = replacer {use = true, def = true} dst,
                    size = size}
           | CMP {src1, src2, size}
           => CMP {src1 = replacer {use = true, def = false} src1,
                   src2 = replacer {use = true, def = false} src2,
                   size = size}
           | TEST {src1, src2, size}
           => TEST {src1 = replacer {use = true, def = false} src1,
                    src2 = replacer {use = true, def = false} src2,
                    size = size}
           | SETcc {condition, dst, size}
           => SETcc {condition = condition,
                     dst = replacer {use = false, def = true} dst,
                     size = size}
           | JMP {target, absolute}
           => JMP {target = replacer {use = true, def = false} target,
                   absolute = absolute}
           | Jcc {condition, target}
           => Jcc {condition = condition,
                   target = replacer {use = true, def = false} target}
           | CALL {target, absolute}
           => CALL {target = replacer {use = true, def = false} target,
                    absolute = absolute}
           | RET {src}
           => (case src 
                 of NONE => RET {src = NONE} 
                  | SOME src 
                  => RET {src = SOME (replacer {use = true, def = false} src)})
           | MOV {src, dst, size}
           => MOV {src = replacer {use = true, def = false} src,
                   dst = replacer {use = false, def = true} dst,
                   size = size}
           | CMOVcc {condition, src, dst, size}
           => CMOVcc {condition = condition,
                      src = replacer {use = true, def = false} src,
                      dst = replacer {use = false, def = true} dst,
                      size = size}
           | XCHG {src, dst, size}
           => XCHG {src = replacer {use = true, def = true} src,
                    dst = replacer {use = true, def = true} dst,
                    size = size}
           | pPUSH {src, base, size}
           => pPUSH {src = replacer {use = true, def = false} src,
                     base = replacer {use = true, def = true} base,
                     size = size}
           | pPOP {dst, base, size}
           => pPOP {dst = replacer {use = false, def = true} dst,
                    base = replacer {use = true, def = true} base,
                    size = size}
           | PUSH {src, size}
           => PUSH {src = replacer {use = true, def = false} src,
                    size = size}
           | POP {dst, size}
           => POP {dst = replacer {use = false, def = true} dst,
                   size = size}
           | CX {size}
           => CX {size = size}
           | MOVX {oper, src, srcsize, dst, dstsize}
           => MOVX {oper = oper,
                    src = replacer {use = true, def = false} src,
                    srcsize = srcsize,
                    dst = replacer {use = false, def = true} dst,
                    dstsize = dstsize}
           | XVOM {src, srcsize, dst, dstsize}
           => XVOM {src = replacer {use = true, def = false} src,
                    srcsize = srcsize,
                    dst = replacer {use = false, def = true} dst,
                    dstsize = dstsize}
           | LEA {src, dst, size}
           => LEA {src = replacer {use = true, def = false} src,
                   dst = replacer {use = false, def = true} dst,
                   size = size}
           | pFMOV {src, dst, size}
           => pFMOV {src = replacer {use = true, def = false} src,
                     dst = replacer {use = false, def = true} dst,
                     size = size}
           | pFMOVX {src, dst, srcsize, dstsize}
           => pFMOVX {src = replacer {use = true, def = false} src,
                      dst = replacer {use = false, def = true} dst,
                      srcsize = srcsize, dstsize = dstsize}
           | pFXVOM {src, dst, srcsize, dstsize}
           => pFXVOM {src = replacer {use = true, def = false} src,
                      dst = replacer {use = false, def = true} dst,
                      srcsize = srcsize, dstsize = dstsize}
           | pFLDC {oper, dst, size}
           => pFLDC {oper = oper,
                     dst = replacer {use = false, def = true} dst,
                     size = size}
           | pFMOVFI {src, srcsize, dst, dstsize}
           => pFMOVFI {src = replacer {use = true, def = false} src,
                       srcsize = srcsize,
                       dst = replacer {use = false, def = true} dst,
                       dstsize = dstsize}
           | pFMOVTI {src, dst, srcsize, dstsize}
           => pFMOVTI {src = replacer {use = true, def = false} src,
                       srcsize = srcsize,
                       dst = replacer {use = false, def = true} dst,
                       dstsize = dstsize}
           | pFCOM {src1, src2, size}
           => pFCOM {src1 = replacer {use = true, def = false} src1,
                     src2 = replacer {use = true, def = false} src2,
                     size = size}
           | pFUCOM {src1, src2, size}
           => pFUCOM {src1 = replacer {use = true, def = false} src1,
                      src2 = replacer {use = true, def = false} src2,
                      size = size}
           | pFBinA {oper, src, dst, size}
           => pFBinA {oper = oper,
                      src = replacer {use = true, def = false} src,
                      dst = replacer {use = true, def = true} dst,
                      size = size}
           | pFUnA {oper, dst, size}
           => pFUnA {oper = oper,
                     dst = replacer {use = true, def = true} dst,
                     size = size}
           | pFPTAN {dst, size}
           => pFPTAN {dst = replacer {use = true, def = true} dst,
                      size = size}
           | pFBinAS {oper, src, dst, size}
           => pFBinAS {oper = oper,
                       src = replacer {use = true, def = false} src,
                       dst = replacer {use = true, def = true} dst,
                       size = size}
           | pFBinASP {oper, src, dst, size}
           => pFBinASP {oper = oper,
                        src = replacer {use = true, def = true} src,
                        dst = replacer {use = true, def = true} dst,
                        size = size}
           | FLD {src, size}
           => FLD {src = replacer {use = true, def = false} src,
                   size = size}
           | FST {dst, size, pop}
           => FST {dst = replacer {use = false, def = true} dst,
                   size = size,
                   pop = pop}
           | FILD {src, size}
           => FILD {src = replacer {use = true, def = false} src,
                    size = size}
           | FIST {dst, size, pop}
           => FIST {dst = replacer {use = false, def = true} dst,
                    size = size,
                    pop = pop}
           | FXCH {src}
           => FXCH {src = replacer {use = true, def = true} src}
           | FLDC {oper}
           => FLDC {oper = oper}
           | FLDCW {src}
           => FLDCW {src = replacer {use = true, def = false} src}
           | FSTCW {dst, check}
           => FSTCW {dst = replacer {use = false, def = true} dst,
                     check = check}
           | FSTSW {dst, check}
           => FSTSW {dst = replacer {use = false, def = true} dst,
                     check = check}
           | FCOM {src, size, pop, pop'}
           => FCOM {src = replacer {use = true, def = false} src,
                    size = size, 
                    pop = pop,
                    pop' = pop'}
           | FUCOM {src, pop, pop'}
           => FUCOM {src = replacer {use = true, def = false} src,
                     pop = pop,
                     pop' = pop'}
           | FBinA {oper, src, dst, size, pop}
           => FBinA {oper = oper,
                     src = replacer {use = true, def = false} src,
                     dst = replacer {use = true, def = true} dst,
                     size = size,
                     pop = pop}
           | FUnA {oper}
           => FUnA {oper = oper}
           | FPTAN
           => FPTAN
           | FBinAS {oper}
           => FBinAS {oper = oper}
           | FBinASP {oper}
           => FBinASP {oper = oper}

      val nop = fn () => NOP
      val hlt = fn () => HLT
      val binal = BinAL
      val pmd = pMD
      val md = MD
      val imul2 = IMUL2
      val unal = UnAL
      val sral = SRAL
      val cmp = CMP
      val test = TEST
      val setcc = SETcc
      val jmp = JMP
      val jcc = Jcc
      val call = CALL
      val ret = RET
      val mov = MOV
      val cmovcc = CMOVcc
      val xchg = XCHG
      val ppush = pPUSH
      val ppop = pPOP
      val push = PUSH
      val pop = POP
      val cx = CX
      val movx = MOVX
      val xvom = XVOM
      val lea = LEA
      val pfmov = pFMOV
      val pfmovx = pFMOVX
      val pfxvom = pFXVOM
      val pfldc = pFLDC
      val pfmovfi = pFMOVFI
      val pfmovti = pFMOVTI
      val pfcom = pFCOM
      val pfucom = pFUCOM
      val pfbina = pFBinA
      val pfuna = pFUnA
      val pfptan = pFPTAN
      val pfbinas = pFBinAS
      val pfbinasp = pFBinASP
      val fld = FLD
      val fst = FST
      val fild = FILD
      val fist = FIST
      val fxch = FXCH
      val fldc = FLDC
      val fldcw = FLDCW
      val fstcw = FSTCW
      val fstsw = FSTSW
      val fcom = FCOM
      val fucom = FUCOM
      val fbina = FBinA
      val funa = FUnA
      val fptan = fn () => FPTAN
      val fbinas = FBinAS
      val fbinasp = FBinASP
    end

  structure Directive =
    struct
      structure Id = 
        struct
          val num : int ref = ref 0
          datatype t = T of {num : int,
                             plist: PropertyList.t}
          fun new () = let
                         val id = T {num = !num,
                                     plist = PropertyList.new ()}
                         val _ = Int.inc num
                       in
                         id
                       end
          val plist = fn T {plist, ...} => plist
          val layout
            = let
                open Layout
              in
                fn T {num, ...} => seq [str "RegAlloc", Int.layout num]
              end
          val toString = Layout.toString o layout
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
          (* Ensure that memloc is in the register, possibly reserverd; 
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
        | Return of {returns: {src: Operand.t, dst: MemLoc.t} list}
        (* Misc. *)
          (* Assert that the register is not free for the allocator;
           * used ???
           *)
        | Reserve of {registers: Register.t list}
          (* Assert that the register is free for the allocator;
           * used to free registers at fall-thru;
           * also used after C calls to free %esp.
           *)
        | Unreserve of {registers : Register.t list}
          (* Clear the floating point stack;
           * used at bot of basic blocks to establish passing convention
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

      val toString
        = fn Assume {assumes}
           => concat["Assume: ",
                     "assumes: ",
                     List.fold
                     (assumes,
                      "",
                      fn ({register, memloc, sync, reserve, ...}, s)
                       => concat[MemLoc.toString memloc, 
                                 " -> ", Register.toString register,
                                 if reserve then " (reserved)" else "",
                                 if sync then " (sync)" else "",
                                 " ",
                                 s])]
           | FltAssume {assumes}
           => concat["FltAssume: ",
                     "assumes: ",
                     List.fold
                     (assumes,
                      "",
                      fn ({memloc, sync, ...}, s)
                       => concat[MemLoc.toString memloc, 
                                 if sync then " (sync)" else "",
                                 " ",
                                 s])]
           | Cache {caches}
           => concat["Cache: ",
                     "caches: ",
                     List.fold
                     (caches,
                      "",
                      fn ({register, memloc, reserve}, s)
                       => concat[MemLoc.toString memloc, 
                                 " -> ", Register.toString register,
                                 if reserve then " (reserved)" else "",
                                 " ",
                                 s])]
           | FltCache {caches}
           => concat["FltCache: ",
                     "caches: ",
                     List.fold
                     (caches,
                      "",
                      fn ({memloc}, s)
                       => concat[MemLoc.toString memloc, 
                                 " ",
                                 s])]
           | Force {commit_memlocs, commit_classes,
                    remove_memlocs, remove_classes,
                    dead_memlocs, dead_classes}
           => concat["Force: ", 
                     "commit_memlocs: ",
                     MemLocSet.fold
                     (commit_memlocs,
                      "",
                      fn (memloc,s)
                       => concat[MemLoc.toString memloc, " ", s]),
                     "commit_classes: ",
                     ClassSet.fold
                     (commit_classes,
                      "",
                      fn (class,s)
                       => concat[MemLoc.Class.toString class, " ", s]),
                     "remove_memlocs: ",
                     MemLocSet.fold
                     (remove_memlocs,
                      "",
                      fn (memloc,s)
                       => concat[MemLoc.toString memloc, " ", s]),
                     "remove_classes: ",
                     ClassSet.fold
                     (remove_classes,
                      "",
                      fn (class,s)
                       => concat[MemLoc.Class.toString class, " ", s]),
                     "dead_memlocs: ",
                     MemLocSet.fold
                     (dead_memlocs,
                      "",
                      fn (memloc,s)
                       => concat[MemLoc.toString memloc, " ", s]),
                     "dead_classes: ",
                     ClassSet.fold
                     (dead_classes,
                      "",
                      fn (class,s)
                       => concat[MemLoc.Class.toString class, " ", s])]
           | Reset
           => concat["Reset"]
           | CCall
           => concat["CCall"]
           | Return {returns}
           => concat["Return: ", List.toString (fn {src,dst} =>
                                                concat ["(", Operand.toString src,
                                                        ",", MemLoc.toString dst, ")"]) returns]
           | Reserve {registers}
           => concat["Reserve: ", 
                     "registers: ",
                     List.fold(registers,
                               "",
                               fn (register,s)
                                => concat[Register.toString register, " ", s])]
           | Unreserve {registers}
           => concat["Unreserve: ", 
                     "registers: ",
                     List.fold(registers,
                               "",
                               fn (register,s)
                                => concat[Register.toString register, " ", s])]
           | ClearFlt
           => concat["ClearFlt"]
           | SaveRegAlloc {live, id}
           => concat["SaveRegAlloc: ", 
                     "live: ",
                     MemLocSet.fold
                     (live,
                      "",
                      fn (memloc,s)
                       => concat[MemLoc.toString memloc, " ", s]),
                     Id.toString id]
           | RestoreRegAlloc {live, id}
           => concat["RestoreRegAlloc: ", 
                     "live: ",
                     MemLocSet.fold
                     (live,
                      "",
                      fn (memloc,s)
                       => concat[MemLoc.toString memloc, " ", s]),
                     Id.toString id]
      val layout = Layout.str o toString

      val uses_defs_kills
        = fn Assume {assumes}
           => List.fold
              (assumes,
               {uses = [], defs = [], kills = []},
               fn ({register, memloc, ...},
                   {uses, defs, ...})
                => {uses = (Operand.memloc memloc)::uses,
                    defs = (Operand.register register)::defs, 
                    kills = []})
           | FltAssume {assumes}
           => List.fold
              (assumes,
               {uses = [], defs = [], kills = []},
               fn ({memloc, ...},
                   {uses, defs, ...})
                => {uses = (Operand.memloc memloc)::uses,
                    defs = defs, 
                    kills = []})
           | Cache {caches}
           => List.fold
              (caches,
               {uses = [], defs = [], kills = []},
               fn ({register, memloc, ...},
                   {uses, defs, ...})
                => {uses = (Operand.memloc memloc)::uses,
                    defs = (Operand.register register)::defs, 
                    kills = []})
           | FltCache {caches}
           => List.fold
              (caches,
               {uses = [], defs = [], kills = []},
               fn ({memloc, ...},
                   {uses, defs, ...})
                => {uses = (Operand.memloc memloc)::uses,
                    defs = defs, 
                    kills = []})
           | Reset => {uses = [], defs = [], kills = []}
           | Force {commit_memlocs, remove_memlocs, ...}
           => {uses = List.map(MemLocSet.toList commit_memlocs, Operand.memloc) @
                      List.map(MemLocSet.toList remove_memlocs, Operand.memloc),
               defs = [], 
               kills = []}
           | CCall => {uses = [], defs = [], kills = []}
           | Return {returns}
           => let 
                 val uses = List.map(returns, fn {src, ...} => src)
                 val defs = List.map(returns, fn {dst, ...} => Operand.memloc dst)
              in
                 {uses = uses, defs = defs, kills = []}
              end
           | Reserve {...} => {uses = [], defs = [], kills = []}
           | Unreserve {...} => {uses = [], defs = [], kills = []}
           | ClearFlt => {uses = [], defs = [], kills = []}
           | SaveRegAlloc {live, ...} 
           => {uses = List.map(MemLocSet.toList live, Operand.memloc), 
               defs = [], 
               kills = []}
           | RestoreRegAlloc {...} 
           => {uses = [], defs = [], kills = []}

      val hints
        = fn Cache {caches}
           => List.map
              (caches,
               fn {register, memloc, ...} 
                => (memloc, register))
           | _ => []

      fun replace replacer
        = fn Assume {assumes}
           => Assume {assumes
                      = List.map
                        (assumes,
                         fn {register, memloc, weight, sync, reserve}
                          => {register = register,
                              memloc = memloc,
                              weight = weight,
                              sync = sync,
                              reserve = reserve})}
           | FltAssume {assumes}
           => FltAssume {assumes
                         = List.map
                           (assumes,
                            fn {memloc, weight, sync}
                             => {memloc = memloc,
                                 weight = weight,
                                 sync = sync})}
           | Cache {caches}
           => Cache {caches
                     = List.map
                       (caches,
                        fn {register, memloc, reserve}
                         => {register = case replacer {use = false, def = true}
                                             (Operand.register register)
                                          of Operand.Register register => register
                                           | _ => Error.bug "x86.Directive.replace: Cache, register",
                             memloc = case replacer {use = true, def = false}
                                           (Operand.memloc memloc)
                                        of Operand.MemLoc memloc => memloc
                                         | _ => Error.bug "x86.Directive.replace: Cache, memloc",
                             reserve = reserve})}
           | FltCache {caches}
           => FltCache {caches
                        = List.map
                          (caches,
                           fn {memloc}
                            => {memloc = case replacer {use = true, def = false}
                                              (Operand.memloc memloc)
                                           of Operand.MemLoc memloc => memloc
                                            | _ => Error.bug "x86.Directive.replace: FltCache, memloc"})}
           | Reset => Reset
           | Force {commit_memlocs, commit_classes, 
                    remove_memlocs, remove_classes,
                    dead_memlocs, dead_classes}
           => Force {commit_memlocs = MemLocSet.map
                                      (commit_memlocs,
                                       fn memloc
                                        => case replacer 
                                                {use = true, def = false}
                                                (Operand.memloc memloc)
                                             of Operand.MemLoc memloc => memloc
                                              | _ => Error.bug "x86.Directive.replace: Force, commit_memlocs"),
                     commit_classes = commit_classes,
                     remove_memlocs = MemLocSet.map
                                      (remove_memlocs,
                                       fn memloc
                                        => case replacer 
                                                {use = true, def = false}
                                                (Operand.memloc memloc)
                                             of Operand.MemLoc memloc => memloc
                                              | _ => Error.bug "x86.Directive.replace: Force, remove_memlocs"),
                     remove_classes = remove_classes,
                     dead_memlocs = MemLocSet.map
                                    (dead_memlocs,
                                     fn memloc
                                      => case replacer 
                                              {use = false, def = false}
                                              (Operand.memloc memloc)
                                           of Operand.MemLoc memloc => memloc
                                            | _ => Error.bug "x86.Directive.replace: Force, dead_memlocs"),
                     dead_classes = dead_classes}
           | CCall => CCall
           | Return {returns}
           => Return {returns = List.map
                                (returns, fn {src,dst} =>
                                 {src = src,
                                  dst = 
                                  case replacer {use = true, def = false}
                                       (Operand.memloc dst)
                                    of Operand.MemLoc memloc => memloc
                                     | _ => Error.bug "x86.Directive.replace: Return, returns"})}
           | Reserve {registers} => Reserve {registers = registers}
           | Unreserve {registers} => Unreserve {registers = registers}
           | ClearFlt => ClearFlt
           | SaveRegAlloc {live, id} => SaveRegAlloc {live = live, id = id}
           | RestoreRegAlloc {live, id} => RestoreRegAlloc {live = live, id = id}

      val assume = Assume
      val fltassume = FltAssume
      val cache = Cache
      val fltcache = FltCache
      val reset = fn () => Reset
      val force = Force
      val ccall = fn () => CCall
      val return = Return
      val reserve = Reserve
      val unreserve = Unreserve
      val saveregalloc = SaveRegAlloc
      val restoreregalloc = RestoreRegAlloc
      val clearflt = fn () => ClearFlt
    end

  structure PseudoOp = 
    struct
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

      val layout
        = let
            open Layout
          in
            fn Data => str ".data"
             | Text => str ".text"
             | SymbolStub 
             => str ".section __IMPORT,__jump_table,symbol_stubs,self_modifying_code+pure_instructions,5"
             | NonLazySymbolPointer
             => str ".section __IMPORT,__pointers,non_lazy_symbol_pointers"
             | Balign (i,fill,max) 
             => seq [str ".balign ", 
                     Immediate.layout i,
                     case (fill, max)
                       of (NONE, NONE) => empty
                        | (SOME fill, NONE) => seq [str ",",
                                                    Immediate.layout fill]
                        | (NONE, SOME max) => seq [str ",,",
                                                   Immediate.layout max]
                        | (SOME fill, SOME max) => seq [str ",",
                                                        Immediate.layout fill,
                                                        str ",",
                                                        Immediate.layout max]]
             | P2align (i,fill,max)
             => seq [str ".p2align ", 
                     Immediate.layout i,
                     case (fill, max)
                       of (NONE, NONE) => empty
                        | (SOME fill, NONE) => seq [str ",",
                                                    Immediate.layout fill]
                        | (NONE, SOME max) => seq [str ",,",
                                                   Immediate.layout max]
                        | (SOME fill, SOME max) => seq [str ",",
                                                        Immediate.layout fill,
                                                        str ",",
                                                        Immediate.layout max]]
             | Space (i,f) 
             => seq [str ".space ", 
                     Immediate.layout i, 
                     str ",", 
                     Immediate.layout f]
             | Byte bs
             => seq [str ".byte ",
                     seq (separate(List.map (bs, Immediate.layout), ","))]
             | Word ws
             => seq [str ".word ",
                     seq (separate(List.map (ws, Immediate.layout), ","))]
             | Long ls
             => seq [str ".long ",
                     seq (separate(List.map (ls, Immediate.layout), ","))]
             | String ss 
             => seq [str ".ascii ",
                     seq (separate(List.map
                                   (ss,
                                    fn s => seq [str "\"",
                                                 str (String_escapeASM s),
                                                 str "\""]),
                                   ","))]
             | Global l 
             => seq [str ".globl ", 
                     Label.layout l]
             | Hidden l
             => (* visibility directive depends on target object file *)
                let
                   val elf = seq [str ".hidden ", Label.layout l]
                   val macho = seq [str ".private_extern ", Label.layout l]
                   val coff = seq [str "/* ", str ".hidden ", Label.layout l, str " */"]
                in
                   case !Control.Target.os of
                      MLton.Platform.OS.Cygwin => coff
                    | MLton.Platform.OS.Darwin => macho
                    | MLton.Platform.OS.MinGW => coff
                    | _ => elf
                end
             | IndirectSymbol l 
             => seq [str ".indirect_symbol ",
                     Label.layout l]
             | Local l 
             => seq [str ".local ", 
                     Label.layout l]
             | Comm (l, i, a) 
             => seq [str ".comm ", 
                     Label.layout l, 
                     str ",", 
                     Immediate.layout i,
                     case a of NONE => empty 
                             | SOME i => seq [str ",", Immediate.layout i]]
          end
      val toString = Layout.toString o layout

      fun replace replacer
        = let
            val replacerLabel
              = fn label 
                 => case Operand.deLabel 
                         (replacer {use = true, def = false} 
                                   (Operand.label label))
                      of SOME label => label
                       | NONE => Error.bug "x86.PseudoOp.replace.replacerLabel"
            val replacerImmediate
              = fn immediate
                 => case Operand.deImmediate
                         (replacer {use = true, def = false} 
                                   (Operand.immediate immediate))
                             of SOME immediate => immediate
                              | NONE => Error.bug "x86.PseudoOp.replace.replacerImmediate"
          in
            fn Data => Data
             | Text => Text
             | SymbolStub => SymbolStub
             | NonLazySymbolPointer => NonLazySymbolPointer
             | Balign (i,fill,max) => Balign (replacerImmediate i,
                                              Option.map(fill, replacerImmediate),
                                              Option.map(max, replacerImmediate))
             | P2align (i,fill,max) => P2align (replacerImmediate i,
                                                Option.map(fill, replacerImmediate),
                                                Option.map(max, replacerImmediate))
             | Space (i,f) => Space (replacerImmediate i, replacerImmediate f)
             | Byte bs => Byte (List.map(bs, replacerImmediate))
             | Word ws => Word (List.map(ws, replacerImmediate))
             | Long ls => Long (List.map(ls, replacerImmediate))
             | String ss => String ss
             | Global l => Global (replacerLabel l)
             | Hidden l => Hidden (replacerLabel l)
             | IndirectSymbol l => IndirectSymbol (replacerLabel l)
             | Local l => Local (replacerLabel l)
             | Comm (l, i, a) => Comm (replacerLabel l, 
                                       replacerImmediate i,
                                       Option.map(a, replacerImmediate))
          end

      val data = fn () => Data
      val text = fn () => Text
      val symbol_stub = fn () => SymbolStub
      val non_lazy_symbol_pointer = fn () => NonLazySymbolPointer
      val balign = Balign
      val p2align = P2align
      val space = Space
      val byte = Byte
      val word = Word
      val long = Long
      val string = String
      val global = Global
      val hidden = Hidden
      val indirect_symbol = IndirectSymbol
      val locall = Local
      val comm = Comm
    end

  structure Assembly =
    struct
      datatype t 
        = Comment of string
        | Directive of Directive.t
        | PseudoOp of PseudoOp.t
        | Label of Label.t
        | Instruction of Instruction.t

      val layout
        = let
            open Layout
          in
            fn Comment s => seq [str "/* ",  str s, str " */"]
             | Directive d => seq [str "# directive: ", Directive.layout d]
             | PseudoOp p => seq [PseudoOp.layout p]
             | Label l => seq [Label.layout l, str ":"]
             | Instruction i => seq [str "\t", Instruction.layout i]
          end
      val toString = Layout.toString o layout

      val uses_defs_kills
        = fn Comment _ => {uses = [], defs = [], kills = []}
           | Directive d => Directive.uses_defs_kills d
           | PseudoOp _ => {uses = [], defs = [], kills = []}
           | Label _ => {uses = [], defs = [], kills = []}
           | Instruction i => Instruction.uses_defs_kills i

      val hints
        = fn Comment _ => []
           | Directive d => Directive.hints d
           | PseudoOp _ => []
           | Label _ => []
           | Instruction i => Instruction.hints i

      fun replace replacer
        = fn Comment s => Comment s
           | Directive d => Directive (Directive.replace replacer d)
           | PseudoOp p => PseudoOp (PseudoOp.replace replacer p)
           | Label l => Label (case Operand.deLabel 
                                    (replacer {use = false, def = true}
                                              (Operand.label l))
                                 of SOME l => l
                                  | NONE => Error.bug "x86.Assembly.replace, Label")
           | Instruction i => Instruction (Instruction.replace replacer i)

      val comment = Comment
      val isComment = fn Comment _ => true | _ => false
      val directive = Directive
      val directive_assume = Directive o Directive.assume
      val directive_fltassume = Directive o Directive.fltassume
      val directive_cache = Directive o Directive.cache
      val directive_fltcache = Directive o Directive.fltcache
      val directive_reset = Directive o Directive.reset
      val directive_force = Directive o Directive.force
      val directive_ccall = Directive o Directive.ccall
      val directive_return = Directive o Directive.return
      val directive_reserve = Directive o Directive.reserve
      val directive_unreserve = Directive o Directive.unreserve
      val directive_saveregalloc = Directive o Directive.saveregalloc
      val directive_restoreregalloc = Directive o Directive.restoreregalloc
      val directive_clearflt = Directive o Directive.clearflt
      val pseudoop = PseudoOp
      val pseudoop_data = PseudoOp o PseudoOp.data
      val pseudoop_text = PseudoOp o PseudoOp.text
      val pseudoop_symbol_stub = PseudoOp o PseudoOp.symbol_stub
      val pseudoop_non_lazy_symbol_pointer =
         PseudoOp o PseudoOp.non_lazy_symbol_pointer
      val pseudoop_balign = PseudoOp o PseudoOp.balign
      val pseudoop_p2align = PseudoOp o PseudoOp.p2align
      val pseudoop_space = PseudoOp o PseudoOp.space
      val pseudoop_byte = PseudoOp o PseudoOp.byte
      val pseudoop_word = PseudoOp o PseudoOp.word
      val pseudoop_long = PseudoOp o PseudoOp.long
      val pseudoop_string = PseudoOp o PseudoOp.string
      val pseudoop_global = PseudoOp o PseudoOp.global
      val pseudoop_hidden = PseudoOp o PseudoOp.hidden
      val pseudoop_indirect_symbol = PseudoOp o PseudoOp.indirect_symbol
      val pseudoop_local = PseudoOp o PseudoOp.locall
      val pseudoop_comm = PseudoOp o PseudoOp.comm
      val label = Label
      val instruction = Instruction
      val instruction_nop = Instruction o Instruction.nop
      val instruction_hlt = Instruction o Instruction.hlt
      val instruction_binal = Instruction o Instruction.binal
      val instruction_pmd = Instruction o Instruction.pmd
      val instruction_md = Instruction o Instruction.md
      val instruction_imul2 = Instruction o Instruction.imul2
      val instruction_unal = Instruction o Instruction.unal
      val instruction_sral = Instruction o Instruction.sral
      val instruction_cmp = Instruction o Instruction.cmp
      val instruction_test = Instruction o Instruction.test
      val instruction_setcc = Instruction o Instruction.setcc
      val instruction_jmp = Instruction o Instruction.jmp
      val instruction_jcc = Instruction o Instruction.jcc
      val instruction_call = Instruction o Instruction.call
      val instruction_ret = Instruction o Instruction.ret
      val instruction_mov = Instruction o Instruction.mov
      val instruction_cmovcc = Instruction o Instruction.cmovcc
      val instruction_xchg = Instruction o Instruction.xchg
      val instruction_ppush = Instruction o Instruction.ppush
      val instruction_ppop = Instruction o Instruction.ppop
      val instruction_push = Instruction o Instruction.push
      val instruction_pop = Instruction o Instruction.pop
      val instruction_cx = Instruction o Instruction.cx
      val instruction_movx = Instruction o Instruction.movx
      val instruction_xvom = Instruction o Instruction.xvom
      val instruction_lea = Instruction o Instruction.lea
      val instruction_pfmov = Instruction o Instruction.pfmov
      val instruction_pfmovx = Instruction o Instruction.pfmovx
      val instruction_pfxvom = Instruction o Instruction.pfxvom
      val instruction_pfldc = Instruction o Instruction.pfldc
      val instruction_pfmovfi = Instruction o Instruction.pfmovfi
      val instruction_pfmovti = Instruction o Instruction.pfmovti
      val instruction_pfcom = Instruction o Instruction.pfcom
      val instruction_pfucom = Instruction o Instruction.pfucom
      val instruction_pfbina = Instruction o Instruction.pfbina
      val instruction_pfuna = Instruction o Instruction.pfuna
      val instruction_pfptan = Instruction o Instruction.pfptan
      val instruction_pfbinas = Instruction o Instruction.pfbinas
      val instruction_pfbinasp = Instruction o Instruction.pfbinasp
      val instruction_fld = Instruction o Instruction.fld
      val instruction_fst = Instruction o Instruction.fst
      val instruction_fild = Instruction o Instruction.fild
      val instruction_fist = Instruction o Instruction.fist
      val instruction_fxch = Instruction o Instruction.fxch
      val instruction_fldc = Instruction o Instruction.fldc
      val instruction_fldcw = Instruction o Instruction.fldcw
      val instruction_fstcw = Instruction o Instruction.fstcw
      val instruction_fstsw = Instruction o Instruction.fstsw
      val instruction_fcom = Instruction o Instruction.fcom
      val instruction_fucom = Instruction o Instruction.fucom
      val instruction_fbina = Instruction o Instruction.fbina
      val instruction_funa = Instruction o Instruction.funa
      val instruction_fptan = Instruction o Instruction.fptan
      val instruction_fbinas = Instruction o Instruction.fbinas
      val instruction_fbinasp = Instruction o Instruction.fbinasp
    end

  structure FrameInfo =
     struct
        datatype t = T of {size: int, 
                           frameLayoutsIndex: int}

        fun toString (T {size, frameLayoutsIndex})
           = concat ["{",
                     "size = ", Int.toString size, ", ",
                     "frameLayoutsIndex = ", 
                     Int.toString frameLayoutsIndex, "}"]
     end

  structure Entry =
    struct
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

      val toString
        = fn Jump {label} => concat ["Jump::",
                                     Label.toString label]
           | Func {label, live}
           => concat ["Func::",
                      Label.toString label,
                      " [",
                      (concat o List.separate)
                      (MemLocSet.fold
                       (live,
                        [],
                        fn (memloc, l) => (MemLoc.toString memloc)::l),
                       ", "),
                      "]"]
           | Cont {label, live, frameInfo} 
           => concat ["Cont::",
                      Label.toString label,
                      " [",
                      (concat o List.separate)
                      (MemLocSet.fold
                       (live,
                        [],
                        fn (memloc, l) => (MemLoc.toString memloc)::l),
                       ", "),
                      "] ",
                      FrameInfo.toString frameInfo]
           | Handler {frameInfo, label, live} 
           => concat ["Handler::",
                      Label.toString label,
                      " [",
                      (concat o List.separate)
                      (MemLocSet.fold
                       (live,
                        [],
                        fn (memloc, l) => (MemLoc.toString memloc)::l),
                       ", "),
                      "] (",
                      FrameInfo.toString frameInfo,
                      ")"]
           | CReturn {dsts, frameInfo, func, label} 
           => concat ["CReturn::",
                      Label.toString label,
                      " ",
                      Vector.toString (fn (dst,_) => Operand.toString dst) dsts,
                      " ",
                      (CFunction.Target.toString o CFunction.target) func,
                      " ",
                      case frameInfo of
                         NONE => ""
                       | SOME f => FrameInfo.toString f]

      val uses_defs_kills
        = fn CReturn {dsts, func, ...} 
           => let
                 val uses =
                    List.map (Operand.cReturnTemps (CFunction.return func),
                              fn {dst, ...} => Operand.memloc dst)
              in
                 {uses = uses, 
                  defs = Vector.toListMap(dsts, fn (dst, _) => dst), 
                  kills = []}
              end
           | _ => {uses = [], defs = [], kills = []}

      val label
        = fn Jump {label, ...} => label
           | Func {label, ...} => label
           | Cont {label, ...} => label
           | Handler {label, ...} => label
           | CReturn {label, ...} => label

      val live
        = fn Func {live, ...} => live
           | Cont {live, ...} => live
           | Handler {live, ...} => live
           | _ => MemLocSet.empty

      val jump = Jump
      val func = Func
      val isFunc = fn Func _ => true | _ => false
      val cont = Cont
      val handler = Handler
      val creturn = CReturn
    end

  structure Transfer =
    struct
      structure Cases =
        struct
          datatype 'a t = Word of (WordX.t * 'a) list

          val word = Word

          fun isEmpty cases
            = case cases
               of Word [] => true
                | _ => false

          fun isSingle cases
            = case cases
                of Word [_] => true
                 | _ => false

          fun extract(cases,f)
            = let
                fun doit [(k,target)] = f (k, target)
                  | doit _ = Error.bug "x86.Transfer.Cases.extract"
              in
                case cases
                  of Word cases => doit cases
              end

          fun count(cases, p)
            = let
                fun doit [] = (0 : int)
                  | doit ((_,target)::cases) = let
                                                 val n = doit cases
                                               in
                                                 if p target
                                                   then 1 + n
                                                   else n
                                               end
              in
                case cases
                  of Word cases => doit cases
              end

          fun keepAll(cases, p)
            = let
                fun doit l = List.keepAll(l, fn (k,target) => p (k,target))
              in
                case cases
                  of Word cases => Word(doit cases)
              end

          fun forall(cases, f)
            = let
                fun doit l = List.forall(l, fn (k, target) => f (k, target))
              in
                case cases
                  of Word cases => doit cases
              end

          fun foreach(cases, f)
            = let
                fun doit l = List.foreach(l, fn (k, target) => f (k, target))
              in
                case cases
                  of Word cases => doit cases
              end

          fun map(cases, f)
            = let
                fun doit l = List.map(l, fn (k,target) => (k, f (k, target)))
              in
                case cases
                  of Word cases => Word(doit cases)
              end

          fun mapToList(cases, f)
            = let
                fun doit l = List.map(l, fn (k,target) => f (k, target))
              in
                case cases
                  of Word cases => doit cases
              end
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

      val toString
        = fn Goto {target}
           => concat ["GOTO ",
                      Label.toString target]
           | Iff {condition, truee, falsee}
           => concat["IF ", 
                     Instruction.condition_toString condition,
                     " THEN GOTO ",
                     Label.toString truee,
                     " ELSE GOTO ",
                     Label.toString falsee]
           | Switch {test, cases, default}
           => (concat["SWITCH ",
                      Operand.toString test]) ^
              (concat o Cases.mapToList)
              (cases,
               fn (w, target) => concat[" (",
                                        WordX.toString w,
                                        " -> GOTO ",
                                        Label.toString target,
                                        ")"]) ^
              (concat[" GOTO ",
                      Label.toString default])
           | Tail {target, live} 
           => concat ["TAIL ", 
                      Label.toString target,
                      " [",
                      (concat o List.separate)
                      (MemLocSet.fold
                       (live,
                        [],
                        fn (memloc, l) => (MemLoc.toString memloc)::l),
                       ", "),
                      "]"]
           | NonTail {target, live, return, handler, size}
           => concat ["NONTAIL ",
                      Label.toString target,
                      " [",
                      (concat o List.separate)
                      (MemLocSet.fold
                       (live,
                        [],
                        fn (memloc, l) => (MemLoc.toString memloc)::l),
                       ", "),
                      "] <",
                      Label.toString return,
                      " ",
                      Int.toString size,
                      "> {",
                      case handler 
                        of SOME handler => Label.toString handler
                         | NONE => "",
                      "}"]
           | Return {live}
           => concat ["RETURN",
                      " [",
                      (concat o List.separate)
                      (MemLocSet.fold
                       (live,
                        [],
                        fn (memloc, l) => (MemLoc.toString memloc)::l),
                       ", "),
                      "]"]
           | Raise {live}
           => concat ["RAISE",  
                      " [",
                      (concat o List.separate)
                      (MemLocSet.fold
                       (live,
                        [],
                        fn (memloc, l) => (MemLoc.toString memloc)::l),
                       ", "),
                      "]"]
           | CCall {args, func, return, ...}
           => concat ["CCALL ",
                      (CFunction.Convention.toString o CFunction.convention) func,
                      " ",
                      (CFunction.Target.toString o CFunction.target) func,
                      "(",
                      (concat o List.separate)
                      (List.map(args, fn (oper,_) => Operand.toString oper),
                       ", "),
                      ") <",
                      Option.toString Label.toString return,
                      ">"]

      val uses_defs_kills
        = fn Switch {test, ...}
           => {uses = [test], defs = [], kills = []}
           | CCall {args, func, ...}
           => let
                 val defs =
                    List.map (Operand.cReturnTemps (CFunction.return func),
                              fn {dst, ...} => Operand.memloc dst)
              in
                 {uses = List.map(args, fn (oper,_) => oper),
                  defs = defs, kills = []}
              end
           | _ => {uses = [], defs = [], kills = []}

      val nearTargets
        = fn Goto {target} => [target]
           | Iff {truee,falsee,...} => [truee,falsee]
           | Switch {cases,default,...} 
           => default::(Cases.mapToList
                        (cases,
                         fn (_,target) => target))
           | NonTail {return,handler,...} => return::(case handler 
                                                        of NONE => nil
                                                         | SOME handler => [handler])
           | CCall {return, ...} 
           => (case return of
                 NONE => []
               | SOME l => [l])
           | _ => []

      val live
        = fn Tail {live,...} => live
           | NonTail {live,...} => live
           | Return {live,...} => live
           | Raise {live,...} => live
           | _ => MemLocSet.empty

      fun replace replacer
        = fn Switch {test, cases, default}
           => Switch {test = replacer {use = true, def = false} test,
                      cases = cases,
                      default = default}
           | CCall {args, frameInfo, func, return}
           => CCall {args = List.map(args,
                                     fn (oper,size) => (replacer {use = true,
                                                                  def = false}
                                                                 oper,
                                                        size)),
                     frameInfo = frameInfo,
                     func = func,
                     return = return}
           | transfer => transfer

      val goto = Goto
      val iff = Iff
      val switch = Switch
      val tail = Tail
      val nontail = NonTail
      val return = Return
      val raisee = Raise
      val ccall = CCall
    end

  structure ProfileLabel =
    struct
      open ProfileLabel

      fun toAssembly pl =
        let
          val label = Label.fromString (toString pl)
        in
          [Assembly.pseudoop_global label,
           Assembly.pseudoop_hidden label,
           Assembly.label label]
        end
      fun toAssemblyOpt pl =
        case pl of
          NONE => []
        | SOME pl => toAssembly pl
    end

  structure Block =
    struct
      datatype t' = T' of {entry: Entry.t option,
                           profileLabel: ProfileLabel.t option,
                           statements: Assembly.t list,
                           transfer: Transfer.t option}
      fun mkBlock' {entry, statements, transfer} =
        T' {entry = entry,
            profileLabel = NONE,
            statements = statements,
            transfer = transfer}
      fun mkProfileBlock' {profileLabel} =
        T' {entry = NONE,
            profileLabel = SOME profileLabel,
            statements = [],
            transfer = NONE}

      datatype t = T of {entry: Entry.t,
                         profileLabel: ProfileLabel.t option,
                         statements: Assembly.t list,
                         transfer: Transfer.t}

      fun printBlock (T {entry, profileLabel, statements, transfer, ...})
        = (print (Entry.toString entry);
           print ":\n";
           Option.app
           (profileLabel, fn profileLabel =>
            (print (ProfileLabel.toString profileLabel);
             print ":\n"));
           List.foreach
           (statements, fn asm => 
            (print (Assembly.toString asm);
             print "\n"));
           print (Transfer.toString transfer);
           print "\n")

      fun printBlock' (T' {entry, profileLabel, statements, transfer, ...})
        = (print (if isSome entry
                    then Entry.toString (valOf entry)
                    else "---");
           print ":\n";
           Option.app
           (profileLabel, fn profileLabel =>
            (print (ProfileLabel.toString profileLabel);
             print ":\n"));
           List.foreach
           (statements, fn asm => 
            (print (Assembly.toString asm);
             print "\n"));
           print (if isSome transfer
                    then Transfer.toString (valOf transfer)
                    else "NONE");
           print "\n")

      val compress': t' list -> t' list =
         fn l =>
         List.fold
         (rev l, [],
          fn (b' as T' {entry, profileLabel, statements, transfer}, ac) =>
          case transfer of
             SOME _ => b' :: ac
           | NONE =>
                case ac of
                   [] => Error.bug "x86.Block.compress': dangling transfer"
                 | b2' :: ac =>
                      let
                         val T' {entry = entry2,
                                 profileLabel = profileLabel2,
                                 statements = statements2,
                                 transfer = transfer2} = b2'
                      in
                         case entry2 of
                            SOME _ =>
                               Error.bug "x86.Block.compress': mismatched transfer"
                          | NONE =>
                               let
                                  val (pl, ss) =
                                     case (profileLabel, statements) of
                                        (NONE, []) =>
                                           (profileLabel2, statements2)
                                      | _ => 
                                           (profileLabel,
                                            statements
                                            @ (ProfileLabel.toAssemblyOpt
                                               profileLabel2)
                                            @ statements2)
                               in
                                  T' {entry = entry,
                                      profileLabel = pl,
                                      statements = ss,
                                      transfer = transfer2} :: ac
                               end
                      end)

      val compress: t' list -> t list =
         fn l =>
         List.map
         (compress' l, fn T' {entry, profileLabel, statements, transfer} =>
          case (entry, transfer) of
             (SOME e, SOME t) =>
                T {entry = e,
                   profileLabel = profileLabel,
                   statements = statements,
                   transfer = t}
           | _ => Error.bug "x86.Block.compress")
    end

  structure Chunk =
    struct
      datatype t = T of {data: Assembly.t list,
                         blocks: Block.t list}
    end
end
