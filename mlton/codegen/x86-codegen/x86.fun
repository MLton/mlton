(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
functor x86(S: X86_STRUCTS): X86 =
struct

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

  open S

  structure Size =
    struct
      datatype class = INT | FLT | FPI
      val class_toString
	= fn INT => "INT"
	   | FLT => "FLT"
	   | FPI => "FPI"

      datatype t 
	= BYTE | WORD | LONG
	| SNGL | DBLE | EXTD
	| FPIS | FPIL | FPIQ

      val toString 
	= fn BYTE => "b"
           | WORD => "w"
	   | LONG => "l"
	   | SNGL => "s"
	   | DBLE => "l"
	   | EXTD => "t"
	   | FPIS => "s"
	   | FPIL => "l"
	   | FPIQ => "q"

      val toString'
	= fn BYTE => "byte"
           | WORD => "word"
	   | LONG => "long"
	   | SNGL => "sngl"
	   | DBLE => "dble"
	   | EXTD => "extd"
	   | FPIS => "fpis"
	   | FPIL => "fpil"
	   | FPIQ => "fpiq"

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

      val class
	= fn BYTE => INT
	   | WORD => INT
	   | LONG => INT
	   | SNGL => FLT
	   | DBLE => FLT
	   | EXTD => FLT
	   | FPIS => FPI
	   | FPIL => FPI
	   | QUAD => FPI

      val toFPI
	= fn WORD => FPIS
	   | LONG => FPIL
	   | FPIS => FPIS
	   | FPIL => FPIL
	   | FPIQ => FPIQ
	   | _ => Error.bug "Size.toFPI"
	
      val eq = fn (s1, s2) => s1 = s2
      val lt = fn (s1, s2) => (toBytes s1) < (toBytes s2)
    end

  structure Register =
    struct

      datatype reg
	= EAX | EBX | ECX | EDX | EDI | ESI | EBP | ESP

      datatype part
	= E | X | L | H

      datatype t = T of {reg: reg, part: part}

      fun size (T {reg, part})
	= case part
	    of E => Size.LONG
	     | X => Size.WORD
	     | L => Size.BYTE
	     | H => Size.BYTE

      fun toString (T {reg, part})
        = let
	    val {prefix, suffix}
	      = case part
		  of E => {prefix = "%e", suffix = "x"}
		   | X => {prefix = "%", suffix = "x"}
		   | L => {prefix = "%", suffix = "l"}
		   | H => {prefix = "%", suffix = "h"}
	  in
	    case reg
	      of EAX => concat[prefix, "a", suffix]
	       | EBX => concat[prefix, "b", suffix]
	       | ECX => concat[prefix, "c", suffix]
	       | EDX => concat[prefix, "d", suffix]
	       | EDI => concat[prefix, "di"]
	       | ESI => concat[prefix, "si"]
	       | EBP => concat[prefix, "bp"]
	       | ESP => concat[prefix, "sp"]
	  end

      fun eq(T r1, T r2) = r1 = r2

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

      fun coincident (register as T {reg, ...})
	= List.keepAllMap([E, X, L, H],
			  fn part 
			   => let
				val register' = T {reg = reg, part = part}
			      in 
				if coincide(register,register')
				  then SOME register'
				  else NONE
			      end)

      fun return size
	= T {reg = EAX, part = case size
				 of Size.BYTE => L
				  | Size.WORD => X
				  | Size.LONG => E
				  | _ => Error.bug "Register.return"}

      val eax = T {reg = EAX, part = E}
      val ebx = T {reg = EBX, part = E}
      val ecx = T {reg = ECX, part = E}
      val edx = T {reg = EDX, part = E}
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

      val registers
	= fn Size.BYTE => byteRegisters
	   | Size.WORD => wordRegisters
	   | Size.LONG => longRegisters
	   | _ => Error.bug "Register.registers"

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
	   | _ => Error.bug "withLowPart: fullsize,lowsize"

      val lowPartOf (* (register,lowsize) *)
	= fn (T {reg,       part = L},Size.BYTE) => T {reg = reg, part = L}
	   | (T {reg,       part = H},Size.BYTE) => T {reg = reg, part = H}
	   | (T {reg = EAX, part},    Size.BYTE) => T {reg = EAX, part = L}
	   | (T {reg = EBX, part},    Size.BYTE) => T {reg = EBX, part = L}
	   | (T {reg = ECX, part},    Size.BYTE) => T {reg = ECX, part = L}
	   | (T {reg = EDX, part},    Size.BYTE) => T {reg = EDX, part = L}
	   | (T {reg,       part = X},Size.WORD) => T {reg = reg, part = X}
	   | (T {reg,       part},    Size.WORD) => T {reg = reg, part = X}
	   | _ => Error.bug "lowPartOf: register,lowsize"

      val fullPartOf (* (register,fullsize) *)
	= fn (T {reg, part = L},Size.BYTE) => T {reg = reg, part = L}
	   | (T {reg, part = H},Size.BYTE) => T {reg = reg, part = H}
	   | (T {reg, part = L},Size.WORD) => T {reg = reg, part = X}
	   | (T {reg, part = X},Size.WORD) => T {reg = reg, part = X}
	   | (T {reg, part = L},Size.LONG) => T {reg = reg, part = E}
	   | (T {reg, part = X},Size.LONG) => T {reg = reg, part = E}
	   | (T {reg, part = E},Size.LONG) => T {reg = reg, part = L}
	   | _ => Error.bug "fullPartOf: register,fullsize"
    end

  structure FltRegister =
    struct
      datatype t = T of int

      fun size _ = Size.EXTD

      fun toString (T i)
	= if i = 0
	    then "%st"
	    else concat ["%st(", Int.toString i, ")"]

      fun eq (T f1, T f2) = f1 = f2

      fun push (T i) = T (i + 1)
      fun pop (T i) = T (i - 1)
      fun id (T i) = T i

      val return = T 0
      val top = T 0
      val one = T 1
      val total = 8 : int
    end

  structure Immediate =
    struct
      datatype const
	= Char of char
	| Int of int
	| Word of word
      val const_toString
	= fn Char c 
	   => let
		val i = Char.ord c
	      in 
		(* GNU assembler bug in character constants: 
		 *   no octal char code escapes.
		 *) 
(*
		if i < 32 orelse i > 126
		  then Int.toString i
		  else "'" ^ (Char_escapeASM c)
*)
                (* GNU gcc preprocessor does not support assembly style 
		 *   character constants
		 *)
		 Int.toString i
	      end
	   | Int i => if i >= 0
			then Int.toString i
			else "-" ^ (String.dropPrefix(Int.toString i, 1))
	   | Word w => "0x" ^ (Word.toString w)

      datatype un
	= Negation
	| Complementation
      val un_toString
	= fn Negation => "-"
	   | Complementation => "~"

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
      val bin_toString
	= fn Multiplication => "*"
	   | Division => "/"
	   | Remainder => "%"
	   | ShiftLeft => "<<"
	   | ShiftRight => ">>"
	   | BitOr => "|"
	   | BitAnd => "&"
	   | BitXor => "^"
	   | BitOrNot => "!"
	   | Addition => "+"
	   | Subtraction => "-"

      datatype t 
	= Const of const
        | Label of Label.t
	| ImmedUnExp of {oper: un,
			 exp: t}
	| ImmedBinExp of {oper: bin,
			  exp1: t,
			  exp2: t}

      val rec toString 
	= fn Const c => const_toString c
           | Label l => Label.toString l
	   | ImmedUnExp {oper, exp}
	   => concat [un_toString oper, toString exp]
	   | ImmedBinExp {oper, exp1, exp2}
	   => concat ["(", 
		      toString exp1, 
		      bin_toString oper, 
		      toString exp2, 
		      ")"]

      val rec eq
	= fn (Const c1, Const c2) => c1 = c2
	   | (Label l1, Label l2) => Label.equals(l1, l2)
	   | (ImmedUnExp {oper = oper,  exp = exp},
	      ImmedUnExp {oper = oper', exp = exp'})
	   => oper = oper' andalso 
	      eq(exp, exp') 
	   | (ImmedBinExp {oper = oper,  exp1 = exp1,  exp2 = exp2},
	      ImmedBinExp {oper = oper', exp1 = exp1', exp2 = exp2'})
	   => oper = oper' andalso 
	      eq(exp1, exp1') andalso 
	      eq(exp2, exp2')
	   | _ => false

      local open Word in
      val rec eval
	= fn Const (Char c) => SOME ((Word.fromInt o Char.ord) c)
	   | Const (Int i) => SOME (Word.fromInt i)
	   | Const (Word w) => SOME w
	   | Label l => NONE
	   | ImmedUnExp {oper, exp}
	   => (case eval exp
		 of SOME i 
		  => (case oper
			of Negation => SOME (0wx0 - i)
	                 | Complementation 
			 => SOME (notb i))
		  | NONE => NONE)
           | ImmedBinExp {oper, exp1, exp2}
           => (case (eval exp1, eval exp2)
		 of (SOME i1, SOME i2)
		  => (case oper
			of Multiplication => SOME (i1 * i2)
			 | Division => SOME (i1 div i2)
			 | Remainder => SOME (i1 mod i2)
			 | ShiftLeft
			 => SOME (<<(i1, i2))
                         | ShiftRight
			 => SOME (>>(i1, i2))
	                 | BitOr
			 => SOME (orb(i1, i2))
	                 | BitAnd
			 => SOME (andb(i1, i2))
			 | BitXor
			 => SOME (xorb(i1, i2))
	                 | BitOrNot
			 => SOME ((notb o orb)(i1, i2))
			 | Addition => SOME (i1 + i2)
	                 | Subtraction => SOME (i1 - i2))
		  | _ => NONE)
      end

      val const = Const
      val const_char = Const o Char
      val const_int = Const o Int
      val const_word = Const o Word
      val deConst
	= fn Const c => SOME c
	   | _ => NONE
      val label = Label
      val unexp = ImmedUnExp
      val binexp = ImmedBinExp
    end

  structure Scale = 
    struct
      datatype t 
	= One | Two | Four | Eight
	
      val toString 
	= fn One => "1"
           | Two => "2"
	   | Four => "4"
	   | Eight => "8"

      fun eq(s1, s2) = s1 = s2

      val toImmediate
	= fn One => Immediate.const_int 1
           | Two => Immediate.const_int 2
	   | Four => Immediate.const_int 4
	   | Eight => Immediate.const_int 8

      val fromBytes : int -> t
	= fn 1 => One
	   | 2 => Two
	   | 4 => Four
	   | 8 => Eight
	   | _ => Error.bug "fromBytes"
    end
    
  structure Address =
    struct
      datatype t = T of {disp: Immediate.t option,
			 base: Register.t option,
			 index: Register.t option,
			 scale: Scale.t option}
	
      fun toString (T {disp, base, index, scale})
	= concat[case disp
		   of NONE => ""
		    | SOME disp' => Immediate.toString disp',
		 if (base <> NONE orelse index <> NONE)
		   then concat["(",
			       case base
				 of NONE => ""
				  | SOME base' 
				  => Register.toString base',
			       case index
				 of NONE => ""
				  | SOME index' 
				  => "," ^ (Register.toString index'),
			       case scale
				 of NONE => ""
				  | SOME scale' 
				  => "," ^ (Scale.toString scale'),
			       ")"]
		   else ""]

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
      structure Commit =
	struct
	  datatype t = T of {isTemp: bool, onFlush: bool}

	  fun toString (T {isTemp, onFlush})
	    = concat [if isTemp then "T" else "",
		      if onFlush then "F" else ""]

	  fun eq (T commit, T commit') = commit = commit'

	  fun commit {isTemp, onFlush} 
	    = T {isTemp = isTemp, onFlush = onFlush}

	  val isTemp = fn T {isTemp, ...} => isTemp
	  val onFlush = fn T {onFlush, ...} => onFlush
	end

      structure Class =
	struct
	  val class_num : int ref = ref 0
	  val class_names : string list ref = ref []
	  datatype t = T of int

	  fun toString (T i)
	    = let
		val i' = (!class_num) - i - 1
	      in 
		case List.peeki(!class_names, fn (j,_) => i' = j)
		  of SOME (_,class_name) => class_name
		   | NONE => "UNKNOWN"
	      end

	  fun new class_name
	    = let
		val class = T (!class_num)
		val _ = class_num := !class_num + 1
		val _ = class_names := class_name::(!class_names)
	      in
		class
	      end

	  val eq = fn (T i, T j) => i = j
	  val mayAlias = eq

	  val CStack = new "CStack"
	  val Code = new "Code"
	end


      datatype t'
	= Imm of Immediate.t
	| Mem of t
      and t
	= T of {base: t',
		index: t',
		scale: Scale.t,
		size: Size.t,
		class: Class.t,
		commit: Commit.t}

      val rec toString'
	= fn Imm i => Immediate.toString i
	   | Mem m => toString m
      and toString 
	= fn (memloc as T {base, index, 
				 scale, size, 
				 class, commit})
	   => concat["MEM<",
		     Commit.toString commit,
		     ">(",
		     Size.toString size,
		     "){",
		     Class.toString class,
		     "}[",
		     toString' base,
		     "+",
		     "(",
		     toString' index,
		     "*",
		     Scale.toString scale,
		     ")]"]

      val rec eq'
	= fn (Imm i1, Imm i2) => Immediate.eq(i1, i2)
	   | (Mem m1, Mem m2) => eq(m1, m2)
	   | _ => false
      and eq
	= fn (T {base = base1, index = index1, 
		 scale = scale1, size = size1, 
		 commit = commit1, class = class1},
	      T {base = base2, index = index2, 
		 scale = scale2, size = size2, 
		 commit = commit2, class = class2})
	   => Class.eq(class1, class2) andalso
	      eq'(base1, base2) andalso
	      eq'(index1, index2) andalso
	      Scale.eq(scale1, scale2) andalso
	      Size.eq(size1, size2) andalso
	      Commit.eq(commit1, commit2)

      val rec utilized'
	= fn Imm i => []
	   | Mem m => m::(utilized m)
      and utilized
	= fn T {base, index, ...} 
	   => (utilized' base) @ (utilized' index)

      val rec mayAlias'
	= fn (T {base = Imm base1, index = Imm index1, 
		 scale = scale1, size = size1, ...},
	      T {base = Imm base2, index = Imm index2, 
		 scale = scale2, size = size2, ...})
	   => Immediate.eq(base1, base2)
	      andalso
	      let
		val size1 = Size.toBytes size1
		val size2 = Size.toBytes size2
		val scale1 = Scale.toImmediate scale1
		val scale2 = Scale.toImmediate scale2
	      in
		case (Immediate.eval (Immediate.ImmedBinExp 
				      {oper = Immediate.Multiplication, 
				       exp1 = index1, 
				       exp2 = scale1}),
		      Immediate.eval (Immediate.ImmedBinExp 
				      {oper = Immediate.Multiplication, 
				       exp1 = index2, 
				       exp2 = scale2}))
		  of (SOME pos1, SOME pos2)
		   => (let
			 val pos1 = Word.toInt pos1
			 val pos2 = Word.toInt pos2
		       in 
			 if pos1 < pos2 
			   then pos2 < (pos1 + size1) 
			   else pos1 < (pos2 + size2)
		       end
		       handle Overflow => false)
		   | _ => true
	      end
	   | (T {base = Imm base1, index = Mem index1, 
		 scale = scale1, size = size1, ...},
	      T {base = Imm base2, index = Mem index2, 
		 scale = scale2, size = size2, ...})
	   => Immediate.eq(base1, base2)
	   | (T {base = Mem base1, index = Imm index1, 
		 scale = scale1, size = size1, ...},
	      T {base = Mem base2, index = Imm index2, 
		 scale = scale2, size = size2, ...})
	   => not (eq(base1, base2))
	      orelse
	      let
		val size1 = Size.toBytes size1
		val size2 = Size.toBytes size2
		val scale1 = Scale.toImmediate scale1
		val scale2 = Scale.toImmediate scale2
	      in
		case (Immediate.eval (Immediate.ImmedBinExp 
				      {oper = Immediate.Multiplication, 
				       exp1 = index1, 
				       exp2 = scale1}),
		      Immediate.eval (Immediate.ImmedBinExp 
				      {oper = Immediate.Multiplication, 
				       exp1 = index2, 
				       exp2 = scale2}))
		  of (SOME pos1, SOME pos2)
		   => (let
			 val pos1 = Word.toInt pos1
			 val pos2 = Word.toInt pos2
		       in 
			 if pos1 < pos2 
			   then pos2 < (pos1 + size1) 
			   else pos1 < (pos2 + size2)
		       end
		       handle Overflow => false)
		   | _ => true
	      end
	   | _ => true
      and mayAlias 
	= fn (memloc1 as T {class = class1, ...},
	      memloc2 as T {class = class2, ...})
	   => Class.mayAlias(class1, class2) andalso
	      mayAlias'(memloc1, memloc2)

      fun replace' replacer
	= fn Imm i => Imm i
	   | Mem m => Mem (replace replacer m)
      and replace replacer
	= fn memloc as T {base, index, scale, size, commit, class}
	   => let
		val memloc' = replacer memloc
	      in
		if eq(memloc',memloc)
		  then T {base = replace' replacer base,
			  index = replace' replacer index,
			  scale = scale,
			  size = size,
			  commit = commit,
			  class = class}
		  else memloc'
	      end

      val size = fn T {size, ...} => size
      val commit = fn T {commit, ...} => commit
      val isTemp = Commit.isTemp o commit
      val onFlush = Commit.onFlush o commit
      val class = fn T {class, ...} => class

      val imm = fn {base, index,
		    scale, size,
		    commit, class} => T {base = Imm base,
					 index = Imm index,
					 scale = scale,
					 size = size,
					 commit = commit,
					 class = class}
      val basic = fn {base, index,
		      scale, size,
		      commit, class} => T {base = Imm base,
					   index = Mem index, 
					   scale = scale,
					   size = size,
					   commit = commit,
					   class = class}
      val simple = fn {base, index,
		       scale, size,
		       commit, class} => T {base = Mem base,
					    index = Imm index,
					    scale = scale,
					    size = size,
					    commit = commit,
					    class = class}
      val complex = fn {base, index,
			scale, size,
			commit, class} => T {base = Mem base,
					     index = Mem index,
					     scale = scale,
					     size = size,
					     commit = commit,
					     class = class}
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
	   | FltRegister f => SOME Size.EXTD
	   | Immediate i => NONE
	   | Label l => NONE
	   | Address a => NONE
	   | MemLoc m => SOME (MemLoc.size m)

      val toString 
	= fn Register r => Register.toString r
	   | FltRegister f => FltRegister.toString f
           | Immediate i => "$" ^ (Immediate.toString i)
	   | Label l => Label.toString l
	   | Address a => Address.toString a
	   | MemLoc m => MemLoc.toString m

      val eq
	= fn (Register r1,    Register r2)    => Register.eq(r1, r2)
	   | (FltRegister f1, FltRegister f2) => FltRegister.eq(f1, f2)
	   | (Immediate i1,   Immediate i2)   => Immediate.eq(i1, i2)
	   | (Label l1,       Label l2)       => Label.equals(l1, l2)
	   | (Address a1,     Address a2)     => Address.eq(a1, a2)
	   | (MemLoc m1,      MemLoc m2)      => MemLoc.eq(m1, m2)
	   | _                                => false

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
      val immediate_const = immediate o Immediate.const
      val immediate_const_char = immediate o Immediate.const_char
      val immediate_const_int = immediate o Immediate.const_int
      val immediate_const_word = immediate o Immediate.const_word
      val immediate_label = immediate o Immediate.label
      val label = Label
      val address = Address
      val memloc = MemLoc
      val deMemloc 
	= fn MemLoc x => SOME x
           | _ => NONE
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
      val binal_toString 
	= fn ADD => "add"
           | ADC => "adc"
	   | SUB => "sub"
	   | SBB => "sbb"
	   | AND => "and"
	   | OR  => "or"
	   | XOR => "xor"

      (* Integer multiplication and division. *)
      datatype md
	= IMUL (* signed multiplication (one operand form); p. 335 *)
	| MUL  (* unsigned multiplication; p. 488 *)
	| IDIV (* signed division; p. 332 *)
	| DIV  (* unsigned division; p. 188 *)
	| IMOD (* signed modulus; *)
	| MOD  (* unsigned modulus; *)
      val md_toString
	= fn IMUL => "imul"
           | MUL  => "mul"
	   | IDIV => "idiv"
	   | DIV  => "div"
	   | IMOD => "imod"
	   | MOD  => "mod"

      (* Integer unary arithmetic/logic instructions. *)
      datatype unal
	= INC (* increment by 1; p. 341 *)
	| DEC (* decrement by 1; p. 186 *)
	| NEG (* two's complement negation; p. 494 *)
	| NOT (* one's complement negation; p. 497 *)
      val unal_toString 
	= fn INC => "inc"
           | DEC => "dec"
	   | NEG => "neg"
	   | NOT => "not"

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
      val sral_toString 
	= fn SAL => "sal"
           | SHL => "shl"
	   | SAR => "sar"
	   | SHR => "shr"
	   | ROL => "rol"
	   | RCL => "rcl"
	   | ROR => "ror"
	   | RCR => "rcr"

      (* Move with extention instructions. *)
      datatype movx
	= MOVSX (* move with sign extention; p. 481 *)
	| MOVZX (* move with zero extention; p. 486 *)
      val movx_toString
	= fn MOVSX => "movs"
           | MOVZX => "movz"

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
	
      val rec condition_toString
	= fn O  => "o"
           | B  => "b"
	   | AE => "ae"
	   | C  => "c"
	   | E  => "e"
	   | Z  => "z"
	   | BE => "be"
	   | A  => "a"
	   | S  => "s"
	   | P  => "p"
	   | PE => "pe"
	   | PO => "po"
	   | L  => "l"
	   | LE => "le"
	   | G  => "g"
	   | GE => "ge"
	   | c  => "n" ^ (condition_toString (condition_negate c))

      (* Floating-point binary arithmetic instructions. *)
      datatype fbina
	= FADD  (* addition; p. 205 *)
        | FSUB  (* subtraction; p. 297 *)
	| FSUBR (* reversed subtraction; p. 301 *)
	| FMUL  (* multiplication; p. 256 *)
	| FDIV  (* division; p. 229 *)
	| FDIVR (* reversed division; p. 233 *)
      val fbina_toString
	= fn FADD => "fadd"
	   | FSUB => "fsub"
	   | FSUBR => "fsubr"
	   | FMUL => "fmul"
	   | FDIV => "fdiv"
	   | FDIVR => "fdivr"
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
      val funa_toString
	= fn F2XM1 => "f2xm1"
	   | FABS => "fabs"
	   | FCHS => "fchs"
	   | FSQRT => "fsqrt"
	   | FSIN => "fsin"
	   | FCOS => "fcos"
	   | FRNDINT => "frndint"

      (* Floating-point binary arithmetic stack instructions. *)
      datatype fbinas
	= FSCALE (* scale; p. 278 *)
	| FPREM (* partial remainder; p. 263 *)
	| FPREM1 (* IEEE partial remainder; p. 266 *)
      val fbinas_toString
	= fn FSCALE => "fscale"
	   | FPREM=> "fprem"
	   | FPREM1 => "fprem1"

      (* floating point binary arithmetic stack pop instructions. *)
      datatype fbinasp
	= FYL2X (* compute y * log_2 x; p. 327 *)
	| FYL2XP1 (* compute y * log_2 (x + 1.0); p. 329 *)
	| FPATAN (* partial arctangent; p. 261 *)
      val fbinasp_toString
	= fn FYL2X => "fyl2x"
	   | FYL2XP1 => "fyl2xp1"
	   | FPATAN => "fpatan"
	    
      (* Floating-point constants. *)
      datatype fldc
	= ONE  (* +1.0; p. 250 *) 
	| ZERO (* +0.0; p. 250 *)
	| PI   (* pi; p. 250 *)
	| L2E  (* log_2 e; p. 250 *)
	| LN2  (* log_e 2; p. 250 *)
	| L2T  (* log_2 10; p. 250 *)
	| LG2  (* log_10 2; p. 250 *)
      val fldc_toString
	= fn ONE => "fld1"
	   | ZERO => "fldz"
	   | PI => "fldpi"
	   | L2E => "fldl2e"
	   | LN2 => "fldln2"
	   | L2T => "fldl2t"
	   | LG2 => "fldlg2"

      (* x86 Instructions.
       * src operands are not changed by the instruction.
       * dst operands are changed by the instruction.
       *)
      datatype t
	(* Integer binary arithmetic(w/o mult & div)/logic instructions.
	 *)
	= BinAL of {oper: binal,
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

      val toString
	= fn BinAL {oper, src, dst, size}
	   => concat[binal_toString oper, 
		     Size.toString size, " ",
		     Operand.toString src, ",",
		     Operand.toString dst]
	   | pMD {oper, src, dst, size}
	   => concat[md_toString oper, 
		     Size.toString size, " ",
		     Operand.toString src, ",",
		     Operand.toString dst]
	   | MD {oper, src, size}
	   => concat[md_toString oper, 
		     Size.toString size, " ",
		     Operand.toString src]
	   | UnAL {oper, dst, size}
	   => concat[unal_toString oper, 
		     Size.toString size, " ",
		     Operand.toString dst]
	   | SRAL {oper, count, dst, size}
	   => concat[sral_toString oper, 
		     Size.toString size, " ",
		     Operand.toString count, ",",
		     Operand.toString dst]
	   | CMP {src1, src2, size}
	   => concat["cmp", 
		     Size.toString size, " ",
		     Operand.toString src2, ",",
		     Operand.toString src1]
	   | TEST {src1, src2, size}
	   => concat["test", 
		     Size.toString size, " ",
		     Operand.toString src2, ",",
		     Operand.toString src1]
	   | SETcc {condition, dst, size}
	   => concat["set", 
		     condition_toString condition, " ",
		     Operand.toString dst]
	   | JMP {target, absolute}
	   => concat["jmp ", 
		     if absolute then "*" else "", 
		     Operand.toString target]
	   | Jcc {condition, target}
	   => concat["j", 
		     condition_toString condition, " ",
		     Operand.toString target]
	   | CALL {target, absolute}
 	   => concat["call ", 
		     if absolute then "*" else "", 
		     Operand.toString target]
	   | RET {src}
	   => concat["ret", 
		     case src
		       of NONE => ""
			| SOME src => " " ^ (Operand.toString src)]
	   | MOV {src, dst, size}
	   => concat["mov", 
		     Size.toString size, " ",
		     Operand.toString src, ",",
		     Operand.toString dst]
	   | CMOVcc {condition, src, dst, size}
	   => concat["cmov", 
		     condition_toString condition,
		     Size.toString size, " ",
		     Operand.toString src,
		     Operand.toString dst]
	   | XCHG {src, dst, size}
	   => concat["xchg", 
		     Size.toString size, " ",
		     Operand.toString src, ",",
		     Operand.toString dst]
	   | pPUSH {src, base, size}
	   => concat["ppush", 
		     Size.toString size, " [",
		     Operand.toString base, "] ",
		     Operand.toString src]
	   | pPOP {dst, base, size}
	   => concat["ppop", 
		     Size.toString size, " [",
		     Operand.toString base, " ]",
		     Operand.toString dst]
	   | PUSH {src, size}
	   => concat["push", 
		     Size.toString size, " ",
		     Operand.toString src]
	   | POP {dst, size}
	   => concat["pop", 
		     Size.toString size, " ",
		     Operand.toString dst]
	   | CX {size}
	   => (case size
		 of Size.BYTE => "cbtw"
		  | Size.WORD => "cwtd"
		  | Size.LONG => "cltd"
		  | _ => Error.bug "unsupported conversion")
	   | MOVX {oper, src, srcsize, dst, dstsize}
	   => concat[movx_toString oper, 
		     Size.toString srcsize, 
		     Size.toString dstsize, " ",
		     Operand.toString src, ",",
		     Operand.toString dst]
	   | XVOM {src, srcsize, dst, dstsize}
	   => concat["xvom",
		     Size.toString srcsize,
		     Size.toString dstsize, " ",
		     Operand.toString src, ",",
		     Operand.toString dst]
	   | LEA {src, dst, size}
	   => concat["lea", 
		     Size.toString size, " ",
		     Operand.toString src, ",",
		     Operand.toString dst]
	   | pFMOV {src, dst, size}
	   => concat["fmov", 
		     Size.toString size, " ",
		     Operand.toString src, ",",
		     Operand.toString dst]
	   | pFLDC {oper, dst, size}
	   => concat[fldc_toString oper,
		     Size.toString size, " ",
		     Operand.toString dst]
	   | pFMOVFI {src, dst, srcsize, dstsize}
	   => concat["fmovfi", 
		     Size.toString srcsize, " ",
		     Size.toString dstsize, " ",
		     Operand.toString src, ",",
		     Operand.toString dst]
	   | pFMOVTI {src, dst, srcsize, dstsize}
	   => concat["fmovti", 
		     Size.toString srcsize, " ",
		     Size.toString dstsize, " ",
		     Operand.toString src, ",",
		     Operand.toString dst]
	   | pFCOM {src1, src2, size}
	   => concat["fcom",
		     Size.toString size, " ",
		     Operand.toString src1, " ",
		     Operand.toString src2]
	   | pFUCOM {src1, src2, size}
	   => concat["fucom",
		     Size.toString size, " ",
		     Operand.toString src1, " ",
		     Operand.toString src2]
	   | pFBinA {oper, src, dst, size}
	   => concat[fbina_toString oper, 
		     Size.toString size, " ",
		     Operand.toString src, ", ",
		     Operand.toString dst]
	   | pFUnA {oper, dst, size}
	   => concat[funa_toString oper,
		     Size.toString size, " ",
		     Operand.toString dst]
	   | pFPTAN {dst, size}
	   => concat["fptan",
		     Size.toString size, " ",
		     Operand.toString dst]
	   | pFBinAS {oper, src, dst, size}
           => concat[fbinas_toString oper,
		     Size.toString size, " ",
		     Operand.toString src, ", ",
		     Operand.toString dst]
	   | pFBinASP {oper, src, dst, size}
           => concat[fbinasp_toString oper,
		     Size.toString size, " ",
		     Operand.toString src, ", ",
		     Operand.toString dst]
	   | FLD {src, size}
	   => concat["fld", 
		     case src
		       of Operand.FltRegister _ => ""
			| _ => Size.toString size, " ",
		     Operand.toString src]
	   | FST {dst, size, pop}
	   => concat["fst", 
		     if pop then "p" else "", 
		     case dst
		       of Operand.FltRegister _ => ""
			| _ => Size.toString size, " ",
		     Operand.toString dst]
	   | FILD {src, size}
	   => concat["fild", 
		     Size.toString size, " ",
		     Operand.toString src]
	   | FIST {dst, size, pop}
	   => concat["fist", 
		     if pop then "p" else "", 
		     Size.toString size, " ",
		     Operand.toString dst]
	   | FXCH {src}
	   => concat["fxch ",
		     Operand.toString src]
	   | FLDC {oper}
	   => concat[fldc_toString oper]
	   | FLDCW {src}
	   => concat["fldcw ",
		     Operand.toString src]
	   | FSTCW {dst, check}
	   => concat[if check then "fstcw " else "fnstcw ",
		     Operand.toString dst]
	   | FSTSW {dst, check}
	   => concat[if check then "fstsw " else "fnstsw ",
		     Operand.toString dst]
	   | FCOM {src, size, pop, pop'}
	   => concat["fcom",
		     if pop andalso pop' 
		       then "pp"
		       else concat[if pop then "p" else "",
				   case src
				     of Operand.FltRegister _
				      => concat[" ",
						Operand.toString src]
				      | _
				      => concat[Size.toString size, " ",
						Operand.toString src]]]
	   | FUCOM {src, pop, pop'}
	   => concat["fucom",
		     if pop andalso pop' 
		       then "pp"
		       else concat[if pop then "p " else " ",
				   Operand.toString src]]
	   | FBinA {oper, src, dst, size, pop}
	   => concat[fbina_toString oper,
		     case src
		       of Operand.FltRegister _ 
			=> concat[if pop then "p " else " ",
				  Operand.toString src, ", ",
				  Operand.toString dst]
		        | _ 
			=> concat[Size.toString size, " ",
				  Operand.toString src]]
	   | FUnA {oper}
	   => concat[funa_toString oper]
	   | FPTAN 
	   => "fptan"
	   | FBinAS {oper}
	   => concat[fbinas_toString oper]
	   | FBinASP {oper}
	   => concat[fbinasp_toString oper]
			 
      val uses_defs_kills
	= fn BinAL {oper, src, dst, size}
	   => {uses = [src, dst], defs = [dst], kills = []}
	   | pMD {oper, src, dst, size}
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
		       | _ => Error.bug "uses_defs: MD, size"
	      in 
		{uses = [src, Operand.register hi, Operand.register lo],
		 defs = [Operand.register hi, Operand.register lo],
		 kills = []}
	      end
	   | UnAL {oper, dst, size}
	   => {uses = [dst], defs = [dst], kills = []}
	   | SRAL {oper, count, dst, size}
	   => {uses = [count, dst], defs = [dst], kills = []}
	   | CMP {src1, src2, size}
	   => {uses = [src1, src2], defs = [], kills = []}
	   | TEST {src1, src2, size}
	   => {uses = [src1, src2], defs = [], kills = []}
	   | SETcc {condition, dst, size}
	   => {uses = [], defs = [dst], kills = []}
	   | JMP {target, absolute}
	   => {uses = [target], defs = [], kills = []}
	   | Jcc {condition, target}
	   => {uses = [target], defs = [], kills = []}
	   | CALL {target, absolute}
	   => {uses = [target], defs = [], kills = []}
	   | RET {src}
	   => {uses = case src of NONE => [] | SOME src => [src], 
	       defs = [], 
	       kills = []}
	   | MOV {src, dst, size}
	   => {uses = [src], defs = [dst], kills = []}
	   | CMOVcc {condition, src, dst, size}
	   => {uses = [src], defs = [dst], kills = []}
	   | XCHG {src, dst, size}
	   => {uses = [src,dst], defs = [src,dst], kills = []}
	   | pPUSH {src, base, size}
	   => {uses = [src,base], 
	       defs = base::
	              (case base
			 of Operand.MemLoc base
			  => [Operand.MemLoc 
			      (MemLoc.simple {base = base,
					      index = Immediate.const_int 0,
					      size = size,
					      scale = Scale.One,
					      commit = MemLoc.Commit.commit 
					               {isTemp = false,
							onFlush = true},
					      class = MemLoc.Class.CStack})]
			  | _ => []),
	       kills = []}
	   | pPOP {dst, base, size}
	   => {uses = base::
	              (case base
			 of Operand.MemLoc base
			  => [Operand.MemLoc 
			      (MemLoc.simple {base = base,
					      index = Immediate.const_int 0,
					      size = size,
					      scale = Scale.One,
					      commit = MemLoc.Commit.commit
					               {isTemp = false,
							onFlush = true},
					      class = MemLoc.Class.CStack})]
			  | _ => []),
	       defs = [dst,base],
	       kills = []}
	   | PUSH {src, size}
	   => {uses = [src, Operand.register Register.esp],
	       defs = [Operand.register Register.esp,
		       Operand.address (Address.T {disp = NONE,
						   base = SOME Register.esp,
						   index = NONE,
						   scale = NONE})], 
	       kills = []}
	   | POP {dst, size}
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
		       | _ => Error.bug "uses_defs: CX, size"
	      in
		{uses = [Operand.register lo],
		 defs = [Operand.register hi, Operand.register lo], 
		 kills = []}
	      end
	   | MOVX {oper, src, srcsize, dst, dstsize}
	   => {uses = [src], defs = [dst], kills = []}
	   | XVOM {src, srcsize, dst, dstsize}
	   => {uses = [src], defs = [dst], kills = []}
	   | LEA {src, dst, size}
	   => {uses = [src], defs = [dst], kills = []}
	   | pFMOV {src, dst, size}
	   => {uses = [src], defs = [dst], kills = []}
	   | pFLDC {oper, dst, size}
	   => {uses = [], defs = [dst], kills = []}
	   | pFMOVFI {src, dst, srcsize, dstsize}
	   => {uses = [src], defs = [dst], kills = []}
	   | pFMOVTI {src, dst, srcsize, dstsize}
	   => {uses = [src], defs = [dst], kills = []}
	   | pFCOM {src1, src2, size}
	   => {uses = [src1, src2], defs = [], kills = []}
	   | pFUCOM {src1, src2, size}
	   => {uses = [src1, src2], defs = [], kills = []}
	   | pFBinA {oper, src, dst, size}
	   => {uses = [src, dst], defs = [dst], kills = []}
	   | pFUnA {oper, dst, size}
	   => {uses = [dst], defs = [dst], kills = []}
	   | pFPTAN {dst, size}
	   => {uses = [dst], defs = [dst], kills = []}
	   | pFBinAS {oper, src, dst, size}
	   => {uses = [src, dst], defs = [dst], kills = []}
	   | pFBinASP {oper, src, dst, size}
	   => {uses = [src, dst], 
	       defs = [dst], 
	       kills = if Operand.eq(src,dst)
			 then []
			 else [src]}
	   | FLD {src, size}
	   => {uses = [src], 
	       defs = [Operand.fltregister FltRegister.top], 
	       kills = []}
	   | FST {dst, size, pop}
	   => {uses = [Operand.fltregister FltRegister.top], 
	       defs = [dst], 
	       kills = if pop 
			 then [Operand.fltregister FltRegister.top] 
			 else []}
	   | FILD {src, size}
	   => {uses = [src], 
	       defs = [Operand.fltregister FltRegister.top], 
	       kills = []}
	   | FIST {dst, size, pop}
	   => {uses = [Operand.fltregister FltRegister.top], 
	       defs = [dst], 
	       kills = if pop 
			 then [Operand.fltregister FltRegister.top] 
			 else []}
	   | FXCH {src}
	   => {uses = [src, Operand.fltregister FltRegister.top],
	       defs = [src, Operand.fltregister FltRegister.top], 
	       kills = []}
	   | FLDC {oper}
	   => {uses = [], 
	       defs = [Operand.fltregister FltRegister.top], 
	       kills = []}
	   | FLDCW {src}
           => {uses = [src], defs = [], kills = []}
	   | FSTCW {dst, check}
	   => {uses = [], defs = [dst], kills = []}
	   | FSTSW {dst, check}
	   => {uses = [], defs = [dst], kills = []}
	   | FCOM {src, size, pop, pop'}
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
	   | FBinA {oper, src, dst, size, pop}
	   => {uses = [src, dst], 
	       defs = [dst], 
	       kills = if pop then [src] else []}
	   | FUnA {oper}
	   => {uses = [Operand.fltregister FltRegister.top],
	       defs = [Operand.fltregister FltRegister.top], kills = []}
	   | FPTAN
	   => {uses = [Operand.fltregister FltRegister.top],
	       defs = [Operand.fltregister FltRegister.top], kills = []}
	   | FBinAS {oper}
	   => {uses = [Operand.fltregister FltRegister.top,
		       Operand.fltregister FltRegister.one],
	       defs = [Operand.fltregister FltRegister.top,
		       Operand.fltregister FltRegister.one], 
	       kills = []}
	   | FBinASP {oper}
	   => {uses = [Operand.fltregister FltRegister.top,
		       Operand.fltregister FltRegister.one],
	       defs = [Operand.fltregister FltRegister.one],
	       kills = [Operand.fltregister FltRegister.top]}

      val srcs_dsts
	= fn BinAL {oper, src, dst, size}
	   => {srcs = SOME [src, dst], dsts = SOME [dst]}
	   | pMD {oper, src, dst, size}
	   => {srcs = SOME [src, dst], dsts = SOME [dst]}
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
		       | _ => Error.bug "uses_defs: MD, size"
	      in 
		{srcs = SOME [src, Operand.register hi, Operand.register lo],
		 dsts = SOME [Operand.register hi, Operand.register lo]}
	      end
	   | UnAL {oper, dst, size}
	   => {srcs = SOME [dst], dsts = SOME [dst]}
	   | SRAL {oper, count, dst, size}
	   => {srcs = SOME [count, dst], dsts = SOME [dst]}
	   | CMP {src1, src2, size}
	   => {srcs = SOME [src1, src2], dsts = NONE}
	   | TEST {src1, src2, size}
	   => {srcs = SOME [src1, src2], dsts = NONE}
	   | SETcc {condition, dst, size}
	   => {srcs = NONE, dsts = SOME [dst]}
	   | JMP {target, absolute}
	   => {srcs = SOME [target], dsts = NONE}
	   | Jcc {condition, target}
	   => {srcs = SOME [target], dsts = NONE}
	   | CALL {target, absolute}
	   => {srcs = SOME [target], dsts = NONE}
	   | RET {src}
	   => {srcs = case src of NONE => NONE | SOME src => SOME [src], 
	       dsts = NONE} 
	   | MOV {src, dst, size}
	   => {srcs = SOME [src], dsts = SOME [dst]}
	   | CMOVcc {condition, src, dst, size}
	   => {srcs = SOME [src], dsts = SOME [dst]}
	   | XCHG {src, dst, size}
	   => {srcs = SOME [src,dst], dsts = SOME [src,dst]}
	   | pPUSH {src, base, size}
	   => {srcs = SOME [src,base], dsts = SOME [base]}
	   | pPOP {dst, base, size}
	   => {srcs = SOME [base], dsts = SOME [dst,base]}
	   | PUSH {src, size}
	   => {srcs = SOME [src, Operand.register Register.esp],
	       dsts = SOME [Operand.register Register.esp]}
	   | POP {dst, size}
	   => {srcs = SOME [Operand.register Register.esp],
	       dsts = SOME [dst, Operand.register Register.esp]}
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
		       | _ => Error.bug "uses_defs: CX, size"
	      in
		{srcs = SOME [Operand.register lo],
		 dsts = SOME [Operand.register hi, Operand.register lo]}
	      end
	   | MOVX {oper, src, srcsize, dst, dstsize}
	   => {srcs = SOME [src], dsts = SOME [dst]}
	   | XVOM {src, srcsize, dst, dstsize}
	   => {srcs = SOME [src], dsts = SOME [dst]}
	   | LEA {src, dst, size}
	   => {srcs = SOME [src], dsts = SOME [dst]}
	   | pFMOV {src, dst, size}
	   => {srcs = SOME [src], dsts = SOME [dst]}
	   | pFLDC {oper, dst, size}
	   => {srcs = SOME [], dsts = SOME [dst]}
	   | pFMOVFI {src, dst, srcsize, dstsize}
	   => {srcs = SOME [src], dsts = SOME [dst]}
	   | pFMOVTI {src, dst, srcsize, dstsize}
	   => {srcs = SOME [src], dsts = SOME [dst]}
	   | pFCOM {src1, src2, size}
	   => {srcs = SOME [src1, src2], dsts = NONE}
	   | pFUCOM {src1, src2, size}
	   => {srcs = SOME [src1, src2], dsts = NONE}
	   | pFBinA {oper, src, dst, size}
	   => {srcs = SOME [src, dst], dsts = SOME [dst]}
	   | pFUnA {oper, dst, size}
	   => {srcs = SOME [dst], dsts = SOME [dst]}
	   | pFPTAN {dst, size}
	   => {srcs = SOME [dst], dsts = SOME [dst]}
	   | pFBinAS {oper, src, dst, size}
	   => {srcs = SOME [src, dst], dsts = SOME [dst]}
	   | pFBinASP {oper, src, dst, size}
	   => {srcs = SOME [src, dst], 
	       dsts = SOME [dst]}
	   | FLD {src, size}
	   => {srcs = SOME [src], 
	       dsts = SOME [Operand.fltregister FltRegister.top]}
	   | FST {dst, size, pop}
	   => {srcs = SOME [Operand.fltregister FltRegister.top], 
	       dsts = SOME [dst]}
	   | FILD {src, size}
	   => {srcs = SOME [src], 
	       dsts = SOME [Operand.fltregister FltRegister.top]}
	   | FIST {dst, size, pop}
	   => {srcs = SOME [Operand.fltregister FltRegister.top], 
	       dsts = SOME [dst]}
	   | FXCH {src}
	   => {srcs = SOME [src, Operand.fltregister FltRegister.top],
	       dsts = SOME [src, Operand.fltregister FltRegister.top]}
	   | FLDC {oper}
	   => {srcs = NONE, 
	       dsts = SOME [Operand.fltregister FltRegister.top]}
	   | FLDCW {src}
           => {srcs = SOME [src], dsts = NONE}
	   | FSTCW {dst, check}
	   => {srcs = NONE, dsts = SOME [dst]}
	   | FSTSW {dst, check}
	   => {srcs = NONE, dsts = SOME [dst]}
	   | FCOM {src, size, pop, pop'}
	   => {srcs = SOME [src, Operand.fltregister FltRegister.top],
	       dsts = NONE}
	   | FUCOM {src, pop, pop'}
	   => {srcs = SOME [src, Operand.fltregister FltRegister.top],
	       dsts = NONE}
	   | FBinA {oper, src, dst, size, pop}
	   => {srcs = SOME [src, dst], 
	       dsts = SOME [dst]}
	   | FUnA {oper}
	   => {srcs = SOME [Operand.fltregister FltRegister.top],
	       dsts = SOME [Operand.fltregister FltRegister.top]}
	   | FPTAN
	   => {srcs = SOME [Operand.fltregister FltRegister.top],
	       dsts = SOME [Operand.fltregister FltRegister.top]}
	   | FBinAS {oper}
	   => {srcs = SOME [Operand.fltregister FltRegister.top,
		       Operand.fltregister FltRegister.one],
	       dsts = SOME [Operand.fltregister FltRegister.top,
		       Operand.fltregister FltRegister.one]}
	   | FBinASP {oper}
	   => {srcs = SOME [Operand.fltregister FltRegister.top,
		       Operand.fltregister FltRegister.one],
	       dsts = SOME [Operand.fltregister FltRegister.one]}

      fun replace replacer
	= fn BinAL {oper, src, dst, size}
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

      val binal = BinAL
      val pmd = pMD
      val md = MD
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
      datatype t 
	= Reset
	| Reserve of {register: Register.t}
	| Unreserve of {register: Register.t}
	| Cache of {register: Register.t, 
		    memloc: MemLoc.t, 
		    reserve: bool}
	| Assume of {register: Register.t, 
		     memloc: MemLoc.t,
		     weight: int,
		     sync: bool,
		     reserve: bool}
        | Eject of {memlocs: MemLoc.t list}
	| Commit of {memlocs: MemLoc.t list}
	| Flush
	| Clear
        | Return of {memloc: MemLoc.t}
        | FltReturn of {memloc: MemLoc.t}

      val toString
	= fn Reset
	   => concat["Reset"]
	   | Reserve {register}
	   => concat["Reserve: ", Register.toString register]
	   | Unreserve {register}
	   => concat["Unreserve: ", Register.toString register]
	   | Cache {register, memloc, reserve}
	   => concat["Cache: ", MemLoc.toString memloc, 
		     " -> ", Register.toString register,
		     if reserve then " (reserved)" else ""]
	   | Assume {register, memloc, weight, sync, reserve}
	   => concat["Assume: ", MemLoc.toString memloc, 
		     " -> ", Register.toString register,
		     if reserve then " (reserved)" else "",
		     if sync then " (sync)" else ""]
	   | Eject {memlocs}
	   => concat["Eject: ", 
		     List.fold(memlocs,
			       "",
			       fn (memloc,s)
			        => concat[MemLoc.toString memloc, " ", s])]
	   | Commit {memlocs}
	   => concat["Commit: ", 
		     List.fold(memlocs,
			       "",
			       fn (memloc,s)
			        => concat[MemLoc.toString memloc, " ", s])]
	   | Flush
	   => concat["Flush"]
	   | Clear
	   => concat["Clear"]
	   | Return {memloc}
	   => concat["Return: ", MemLoc.toString memloc]
	   | FltReturn {memloc}
	   => concat["FltReturn: ", MemLoc.toString memloc]

      val uses_defs_kills
	= fn Reset => {uses = [], defs = [], kills = []}
	   | Reserve {register} => {uses = [], defs = [], kills = []}
	   | Unreserve {register} => {uses = [], defs = [], kills = []}
	   | Cache {register, memloc, reserve}
	   => {uses = [Operand.memloc memloc],
	       defs = [Operand.register register], 
	       kills = []}
           | Assume {register, memloc, weight, sync, reserve}
           => {uses = [], defs = [], kills = []}
           | Eject {memlocs}
           => {uses = [], defs = [], kills = []}
           | Commit {memlocs}
	   => {uses = List.map(memlocs, Operand.memloc), defs = [], kills = []}
           | Flush 
           => {uses = [], defs = [], kills = []}
           | Clear
           => {uses = [], defs = [], kills = []}
           | Return {memloc}
           => {uses = [], defs = [Operand.memloc memloc], kills = []}
           | FltReturn {memloc}
           => {uses = [], defs = [Operand.memloc memloc], kills = []}

      fun replace replacer
	= fn Reset => Reset
           | Reserve {register} => Reserve {register = register}
           | Unreserve {register} => Unreserve {register = register}
           | Cache {register, memloc, reserve}
           => Cache {register = case replacer {use = false, def = true}
                                              (Operand.register register)
				  of Operand.Register register => register
                                   | _ => Error.bug "Directive.replace",
		     memloc = case replacer {use = true, def = false}
                                            (Operand.memloc memloc)
				of Operand.MemLoc memloc => memloc
				 | _ => Error.bug "Directive.replace",
		     reserve = reserve}
	   | Assume {register, memloc, weight, sync, reserve}
	   => Assume {register = register,
		      memloc = memloc,
		      weight = weight,
		      sync = sync,
		      reserve = reserve}
           | Eject {memlocs}
           => Eject {memlocs = List.map(memlocs,
					fn memloc
					 => case replacer 
                                                 {use = false, def = false}
                                                 (Operand.memloc memloc)
					      of Operand.MemLoc memloc 
					       => memloc
					       | _ 
					       => Error.bug 
                                                  "Directive.replace")}
	   | Commit {memlocs}
           => Commit {memlocs = List.map(memlocs,
					 fn memloc
					  => case replacer 
                                                  {use = true, def = false}
                                                  (Operand.memloc memloc)
					       of Operand.MemLoc memloc 
					        => memloc
					        | _ 
					        => Error.bug 
                                                   "Directive.replace")}
	   | Flush => Flush
	   | Clear => Clear
           | Return {memloc}
           => Return {memloc = case replacer {use = true, def = false}
                                             (Operand.memloc memloc)
			         of Operand.MemLoc memloc => memloc
			          | _ => Error.bug "Directive.replace"}
           | FltReturn {memloc}
           => FltReturn {memloc = case replacer {use = true, def = false}
                                                (Operand.memloc memloc)
			            of Operand.MemLoc memloc => memloc
			             | _ => Error.bug "Directive.replace"}

      val reset = fn () => Reset
      val reserve = Reserve
      val unreserve = Unreserve
      val cache = Cache
      val assume = Assume
      val eject = Eject
      val commit = Commit
      val flush = fn () => Flush
      val clear = fn () => Clear
      val return = Return
      val fltreturn = FltReturn
    end

  structure PseudoOp = 
    struct
      datatype t
	= Data
	| Text
	| Balign of int
	| P2align of int
	| Space of int * Immediate.t
	| Byte of Immediate.t list
	| Word of Immediate.t list
	| Long of Immediate.t list
	| String of string list
        | Global of Label.t
        | Local of Label.t
	| Comm of Label.t * int * int option

      val toString
        = fn Data => ".data"
	   | Text => ".text"
	   | Balign i => ".balign " ^ (Int.toString i)
	   | P2align i => ".p2align " ^ (Int.toString i)
	   | Space (i,f) 
	   => concat [".space ", Int.toString i, ", ", Immediate.toString f]
	   | Byte bs
	   => ".byte " ^ 
	      concat(List.separate(List.map(bs, Immediate.toString), ", "))
	   | Word ws 
	   => ".word " ^ 
	      concat(List.separate(List.map(ws, Immediate.toString), ", "))
	   | Long ls 
	   => ".long " ^ 
	      concat(List.separate(List.map(ls, Immediate.toString), ", "))
	   | String ss 
	   => ".string " ^ 
	      concat(List.separate(List.map(ss,
					    fn s => concat["\"",
							   String_escapeASM s,
							   "\""]), ", "))
	   | Global l => ".global " ^ (Label.toString l)
	   | Local l => ".local " ^ (Label.toString l)
	   | Comm (l, i, a) 
	   => concat [".comm ", Label.toString l, ", ", Int.toString i,
		      case a of NONE => "" | SOME i => ", " ^ (Int.toString i)]

      val data = fn () => Data
      val text = fn () => Text
      val balign = Balign
      val p2align = P2align
      val space = Space
      val byte = Byte
      val word = Word
      val long = Long
      val string = String
      val global = Global
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

      val toString
	= fn Comment s => "# " ^ s
	   | Directive d => "# directive: " ^ (Directive.toString d)
	   | PseudoOp p => (PseudoOp.toString p)
	   | Label l => (Label.toString l) ^ ":"
	   | Instruction i => "\t" ^ (Instruction.toString i) 

      val uses_defs_kills
	= fn Comment s => {uses = [], defs = [], kills = []}
	   | Directive d => Directive.uses_defs_kills d
	   | PseudoOp p => {uses = [], defs = [], kills = []}
	   | Label l => {uses = [], defs = [], kills = []}
	   | Instruction i => Instruction.uses_defs_kills i

      fun replace replacer
	= fn Comment s => Comment s
	   | Directive d => Directive (Directive.replace replacer d)
	   | PseudoOp p => PseudoOp p
	   | Label l => Label l
	   | Instruction i => Instruction (Instruction.replace replacer i)

      val comment = Comment
      val isComment = fn Comment _ => true | _ => false
      val directive = Directive
      val directive_reset = Directive o Directive.reset
      val directive_reserve = Directive o Directive.reserve
      val directive_unreserve = Directive o Directive.unreserve
      val directive_cache = Directive o Directive.cache
      val directive_assume = Directive o Directive.assume
      val directive_eject = Directive o Directive.eject
      val directive_commit = Directive o Directive.commit
      val directive_flush = Directive o Directive.flush
      val directive_clear = Directive o Directive.clear
      val directive_return = Directive o Directive.return
      val directive_fltreturn = Directive o Directive.fltreturn
      val pseudoop = PseudoOp
      val pseudoop_data = PseudoOp o PseudoOp.data
      val pseudoop_text = PseudoOp o PseudoOp.text
      val pseudoop_balign = PseudoOp o PseudoOp.balign
      val pseudoop_p2align = PseudoOp o PseudoOp.p2align
      val pseudoop_space = PseudoOp o PseudoOp.space
      val pseudoop_byte = PseudoOp o PseudoOp.byte
      val pseudoop_word = PseudoOp o PseudoOp.word
      val pseudoop_long = PseudoOp o PseudoOp.long
      val pseudoop_string = PseudoOp o PseudoOp.string
      val pseudoop_global = PseudoOp o PseudoOp.global
      val pseudoop_local = PseudoOp o PseudoOp.locall
      val pseudoop_comm = PseudoOp o PseudoOp.comm
      val label = Label
      val instruction = Instruction
      val instruction_binal = Instruction o Instruction.binal
      val instruction_pmd = Instruction o Instruction.pmd
      val instruction_md = Instruction o Instruction.md
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

  structure ProfileInfo =
    struct
      datatype t = T of {profileLevel: int,
			 profileName: string} list

      val none = T []
      fun add(T profileInfo, {profileLevel,profileName})
	= T ({profileLevel = profileLevel,
	      profileName = profileName}::
	     profileInfo)

      val profileHeader = "MLtonProfile"
      val unique = Counter.new 0
      fun profile_begin_end (T profileInfo)
	= if !Control.profile
	    then let
		   val profileInfo
		     = List.removeDuplicates(profileInfo, op =)
		   val profileInfo
		     = List.insertionSort
		       (profileInfo,
			fn ({profileLevel = profileLevel1,...},
			    {profileLevel = profileLevel2,...})
			    => profileLevel1 < profileLevel2)
		     
		   val profileHeader 
		     = profileHeader ^ (Int.toString (Counter.next unique))

		   val profileString
		     = List.fold
		       (profileInfo,
			profileHeader,
			fn ({profileLevel,profileName},profileString)
			 => concat [profileString,
				    "$$",
				    Int.toString profileLevel,
				    ".",
				    profileName])

		   val profileBegin = profileString ^ "$$Begin"
		   val profileBeginLabel = Label.fromString profileBegin
		   val profileEnd = profileString ^ "$$End"
		   val profileEndLabel = Label.fromString profileEnd
		 in
		   ([Assembly.pseudoop_local profileBeginLabel,
		     Assembly.label profileBeginLabel],
		    (* Don't use end labels. *)
		    if true
		       then []
		    else
		       [Assembly.pseudoop_local profileEndLabel,
			Assembly.label profileEndLabel])
		 end
	    else ([],[])

      fun combine (T profileInfo1, T profileInfo2) 
	= T (profileInfo1 @ profileInfo2)
    end

  structure Transfer =
    struct
      structure Cases =
	struct
	  datatype 'a t
	    = Char of (char * 'a) list
	    | Int of (int * 'a) list
	    | Word of (word * 'a) list

	  val char = Char
	  val int = Int
	  val word = Word

	  fun isEmpty cases
	    = case cases
	       of Char [] => true
		| Int [] => true
		| Word [] => true
		| _ => false 

	  fun isSingle cases
	    = case cases
		of Char [_] => true
		 | Int [_] => true
		 | Word [_] => true
		 | _ => false

	  fun extract(cases,f)
	    = let
		fun doit [(k,target)] = f target
		  | doit _ = Error.bug "Transfer.Cases.extract"
	      in
		case cases
		  of Char cases => doit cases
		   | Int cases => doit cases
		   | Word cases => doit cases
	      end

	  fun extract'(cases,f,cf',if',wf')
	    = let
		fun doit ([(k,target)],f') = (f o f') (k, target)
		  | doit _ = Error.bug "Transfer.Cases.extract"
	      in
		case cases
		  of Char cases => doit(cases,cf')
		   | Int cases => doit(cases,if')
		   | Word cases => doit(cases,wf')
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
		  of Char cases => doit cases
		   | Int cases => doit cases
		   | Word cases => doit cases
	      end

	  fun keepAll(cases, p)
	    = let
		fun doit l = List.keepAll(l, fn (_,target) => p target)
	      in
		case cases
		  of Char cases => Char(doit cases)
		   | Int cases => Int(doit cases)
		   | Word cases => Word(doit cases)
	      end

	  fun keepAll'(cases, p, cp', ip', wp')
	    = let
		fun doit (l, p') = List.keepAll(l, p o p')
	      in
		case cases
		  of Char cases => Char(doit(cases,cp'))
		   | Int cases => Int(doit(cases,ip'))
		   | Word cases => Word(doit(cases,wp'))
	      end

	  fun foreach(cases, f)
	    = let
		fun doit l = List.foreach(l, fn (k, target) => f target)
	      in
		case cases
		  of Char cases => doit cases
		   | Int cases => doit cases
		   | Word cases => doit cases
	      end

	  fun foreach'(cases, f, cf', if', wf')
	    = let
		fun doit(l,f') = List.foreach(l, f o f')
	      in
		case cases
		  of Char cases => doit(cases, cf')
		   | Int cases => doit(cases, if')
		   | Word cases => doit(cases, wf')
	      end

	  fun map(cases, f)
	    = let
		fun doit l = List.map(l, fn (k,target) => (k, f target))
	      in
		case cases
		  of Char cases => Char(doit cases)
		   | Int cases => Int(doit cases)
		   | Word cases => Word(doit cases)
	      end

	  fun map'(cases, f, cf', if', wf')
	    = let
		fun doit(l,f') = List.map(l, f o f')
	      in
		case cases
		  of Char cases => doit(cases, cf')
		   | Int cases => doit(cases, if')
		   | Word cases => doit(cases, wf')
	      end

	  fun fold'(cases, b, f, cf', if', wf')
	    = let
		fun doit(l,f') = List.fold(l, b, f o f')
	      in
		case cases
		  of Char cases => doit(cases, cf')
		   | Int cases => doit(cases, if')
		   | Word cases => doit(cases, wf')
	      end

	  fun zip(cases, l')
	    = let
		fun doit l  = List.map(List.zip(l,l'),
				       fn ((k,a),b) => (k,(a,b)))
	      in
		case cases
		  of Char cases => Char (doit cases)
		   | Int cases => Int (doit cases)
		   | Word cases => Word (doit cases)
	      end
	end

      datatype t
	= Assembly of Assembly.t list
	| Goto of {target: Label.t}
	| Iff of {condition: Instruction.condition,
		  truee: Label.t,
		  falsee: Label.t}
	| Switch of {test: Operand.t,
		     cases: Label.t Cases.t,
		     default: Label.t}

      val toString
	= fn Assembly assembly
	   => "ASSEMBLY " ^
	      (concat (List.map(assembly,
				fn asm => concat[Assembly.toString asm,
						 "; "])))
	   | Goto {target} 
	   => concat["GOTO ",
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
	      (concat o Cases.map')
	      (cases,
	       fn (s, target) => concat[" (",
					s,
					" -> GOTO ",
					Label.toString target,
					")"],
	       fn (c, target) => (Char.toString c, target),
	       fn (i, target) => (Int.toString i, target),
	       fn (w, target) => (Word.toString w, target)) ^
	      (concat[" GOTO ",
		      Label.toString default])

      val uses_defs_kills
	= fn Assembly assembly
	   => let
		val {uses', defs', kills'}
		  = List.fold(assembly,
			      {uses' = [], defs' = [], kills' = []},
			      fn (assembly,{uses',defs',kills'})
			       => let
				    val {uses, defs, kills} 
				      = Assembly.uses_defs_kills assembly
				  in
				    {uses' = uses @ uses',
				     defs' = defs @ defs',
				     kills' = kills @ kills'}
				  end)
	      in
		{uses = List.removeDuplicates(uses', Operand.eq),
		 defs = List.removeDuplicates(defs', Operand.eq),
		 kills = List.removeDuplicates(kills', Operand.eq)}
	      end
	   | Switch {test, cases, default}
	   => {uses = [test], defs = [], kills = []}
	   | _ => {uses = [], defs = [], kills = []}

      fun replace replacer
	= fn Assembly assembly
	   => Assembly (List.map(assembly,
				 Assembly.replace replacer))
	   | Switch {test, cases, default}
	   => Switch {test = replacer {use = true, def = false} test,
		      cases = cases,
		      default = default}
           | transfer => transfer
					   
      val targets
	= fn Assembly _ => []
	   | Goto {target} => [target]
	   | Iff {truee,falsee,...} => [truee,falsee]
	   | Switch {cases,default,...} 
	   => default::(Cases.map'
			(cases,
			 fn target => target,
			 fn (c,target) => target,
			 fn (i,target) => target,
			 fn (w,target) => target))

      val assembly = Assembly
      val goto = Goto
      val iff = Iff
      val switch = Switch
    end

  structure Block =
    struct
      datatype t' = T' of {label: Label.t option,
			   profileInfo: ProfileInfo.t,
			   statements: Assembly.t list,
			   transfer: Transfer.t option}
      datatype t = T of {label: Label.t,
			 profileInfo: ProfileInfo.t,
			 statements: Assembly.t list,
			 transfer: Transfer.t}

      fun print_block (T {label, profileInfo, statements, transfer})
	= (print (Label.toString label);
	   print ":\n";
	   List.foreach
	   (statements,
	    fn asm
	     => (print (Assembly.toString asm);
		 print "\n"));
	   print (Transfer.toString transfer);
	   print "\n")

      fun print_block' (T' {label, profileInfo, statements, transfer})
	= (print (if isSome label
		    then Label.toString (valOf label)
		    else "NONE");
	   print ":\n";
	   List.foreach
	   (statements,
	    fn asm
	     => (print (Assembly.toString asm);
		 print "\n"));
	   print (if isSome transfer
		    then Transfer.toString (valOf transfer)
		    else "NONE");
	   print "\n")

      val rec compress
	= fn [] => []
           | [T' {label = SOME label1,
		  profileInfo = profileInfo1,
		  statements = statements1,
		  transfer = SOME transfer1}]
	   => [T {label = label1,
		  profileInfo = profileInfo1,
		  statements = statements1,
		  transfer = transfer1}]
	   | (T' {label = SOME label1,
		  profileInfo = profileInfo1,
		  statements = statements1,
		  transfer = SOME transfer1})::blocks
	   => (T {label = label1,
		  profileInfo = profileInfo1,
		  statements = statements1,
		  transfer = transfer1})::(compress blocks)
	   | (T' {label = SOME label1, 
		  profileInfo = profileInfo1,
		  statements = statements1, 
		 transfer = NONE})::
	     (T' {label = NONE, 
		  profileInfo = profileInfo2,
		  statements = statements2, 
		  transfer = transfer2})::blocks
           => compress ((T' {label = SOME label1,
			     profileInfo = ProfileInfo.combine(profileInfo1,
							       profileInfo2),
			     statements = statements1 @ statements2,
			     transfer = transfer2})::blocks)
           | _ => Error.bug "Blocks.compress"
    end

  structure Chunk =
    struct
      datatype t = T of {exports: Label.t list,
			 blocks: Block.t list}
    end
end
