(* Copyright (C) 2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

functor RepType (S: REP_TYPE_STRUCTS): REP_TYPE =
struct

open S

structure CFunction = Prim.CFunction

type int = Int.t

structure Type =
   struct
      datatype t = T of {node: node,
			 width: Bits.t}
      and node =
	 Address of t
	| ExnStack
	| GCState
	| Label of Label.t
	| Pointers of PointerTycon.t vector
	| Real of RealSize.t
	| Seq of t vector
	| Word

      local
	 fun make f (T r) = f r
      in
	 val node = make #node
	 val width = make #width
      end

      val rec layout: t -> Layout.t =
	 fn t =>
	 let
	    open Layout
	 in
	    case node t of
	       Address t => seq [str "Address ", layout t]
	     | ExnStack => str "ExnStack"
	     | GCState => str "GCState"
	     | Label l => seq [str "Label ", Label.layout l]
	     | Pointers pts =>
		  seq [str "Pointers ",
		       tuple (Vector.toListMap (pts, PointerTycon.layout))]
	     | Real s => str (concat ["Real", RealSize.toString s])
	     | Seq ts => List.layout layout (Vector.toList ts)
	     | Word => str (concat ["Word", Bits.toString (width t)])
	 end

      val toString = Layout.toString o layout

      val rec equals: t * t -> bool =
	 fn (t, t') =>
	 Bits.equals (width t, width t')
	 andalso
	 (case (node t, node t') of
	     (Address t, Address t') => equals (t, t')
	   | (ExnStack, ExnStack) => true
	   | (GCState, GCState) => true
	   | (Label l, Label l') => Label.equals (l, l')
	   | (Pointers v, Pointers v') =>
		Vector.equals (v, v', PointerTycon.equals)
	   | (Real s, Real s') => RealSize.equals (s, s')
	   | (Seq ts, Seq ts') => Vector.equals (ts, ts', equals)
	   | (Word, Word) => true
	   | _ => false)

      val sameWidth: t * t -> bool =
	 fn (t, t') => Bits.equals (width t, width t')

      val word: Bits.t -> t = fn width => T {node = Word, width = width}

      val add: t * t -> t = #1

      val bogusWord: t -> WordX.t =
	 fn t => WordX.one (WordSize.fromBits (width t))

      val address: t -> t =
	 fn t => T {node = Address t,
		    width = Bits.inPointer}

      val andb: t * t -> t option = SOME o #1

      val arshift: t * t -> t = #1

      val bool: t = word Bits.inWord

      val bytes: t -> Bytes.t = Bits.toBytes o width

      val char: t = word Bits.inByte

      val cPointer: unit -> t = fn () => word Bits.inPointer

      val constant: WordX.t -> t = fn w => word (WordSize.bits (WordX.size w))

      val deLabel: t -> Label.t option =
	 fn t =>
	 case node t of
	    Label l => SOME l
	  | _ => NONE

      val dePointer: t -> PointerTycon.t option =
	 fn t => 
	 case node t of
	    Pointers pts =>
	       if 1 = Vector.length pts
		  then SOME (Vector.sub (pts, 0))
	       else NONE
	  | _ => NONE

      val deReal: t -> RealSize.t option =
	 fn t =>
	 case node t of
	    Real s => SOME s
	  | _ => NONE

      val defaultWord: t = word Bits.inWord

      val exnStack: t = T {node = ExnStack,
			   width = Bits.inPointer}

      val rec isPointer: t -> bool =
	 fn t =>
	 case node t of
	    Pointers _ => true
	  | _ => false

      val real: RealSize.t -> t =
	 fn s => T {node = Real s, width = RealSize.bits s}

      local
	 structure C =
	    struct
	       open CType

	       fun fromBits (b: Bits.t): t =
		  case Bits.toInt b of
		     8 => Word8
		   | 16 => Word16
		   | 32 => Word32
		   | 64 => Word64
		   | _ => Error.bug (concat ["CType.fromBits: ",
					     Bits.toString b])
	    end
	 fun w i = word (Bits.fromInt i)
      in
	 val fromCType: CType.t -> t =
	    fn C.Pointer => w 32
	     | C.Real32 => real RealSize.R32
	     | C.Real64 => real RealSize.R64
	     | C.Word8 => w 8
	     | C.Word16 => w 16
	     | C.Word32 => w 32
	     | C.Word64 => w 64

	 val rec toCType: t -> CType.t =
	    fn t =>
	    if isPointer t
	       then C.Pointer
	    else 
	       case node t of
		  Real s =>
		     (case s of
			 RealSize.R32 => C.Real32
		       | RealSize.R64 => C.Real64)
		| _ => C.fromBits (width t)

	 val name = C.name o toCType

	 val align: t * Bytes.t -> Bytes.t =
	    fn (t, n) => C.align (toCType t, n)
      end

      val gcState: t = T {node = GCState, width = Bits.inPointer}

      val isCPointer: t -> bool =
	 fn t =>
	 case node t of
	    Word => Bits.equals (width t, Bits.inPointer)
	  | _ => false

      val isUnit: t -> bool = fn t => Bits.equals (Bits.zero, width t)

      val isUnit = Trace.trace ("RepType.isUnit", layout, Bool.layout) isUnit
	 
      val isReal: t -> bool = isSome o deReal
 
      val rec isSubtype: t * t -> bool =
	 fn (t, t') =>
	 if not (sameWidth (t, t'))
	    then Error.bug "RepType.isSubtype"
	 else
	    (equals (t, t')
	     orelse
	     case (node t, node t') of
		(Pointers ps, Pointers ps') =>
		   Vector.isSubsequence (ps, ps', PointerTycon.equals)
	      | (Real _, _) => false
	      | (Word, Pointers _) => true
	      | (_, Word) => true
	      | _ => false)

      val isSubtype =
	 Trace.trace2 ("RepType.isSubtype", layout, layout, Bool.layout)
	 isSubtype

      val junk: Bits.t -> t = word

      val label: Label.t -> t =
	 fn l => T {node = Label l, width = Bits.inPointer}

      val lshift: t * t -> t = #1

      val mul: t * t -> t = #1

      val orb: t * t -> t option = SOME o #1

      val pointer: PointerTycon.t -> t =
	 fn pt => T {node = Pointers (Vector.new1 pt),
		     width = Bits.inPointer}

      val stack = pointer PointerTycon.stack

      val thread = pointer PointerTycon.thread

      val wordVector = pointer PointerTycon.wordVector

      val word8Vector = pointer PointerTycon.word8Vector

      val string = word8Vector

      val resize: t * Bits.t -> t = fn (_, b) => word b

      val rshift: t * t -> t = #1

      val unit: t = word Bits.zero

      val seq: t vector -> t =
	 fn ts =>
	 if 0 = Vector.length ts
	    then unit
	 else
	    let
	       fun seqOnto (ts, ac) =
		  Vector.foldr
		  (ts, ac, fn (t, ac) =>
		   case ac of
		      [] => [t]
		    | t' :: ac' =>
			 (case (node t, node t') of
			     (Seq ts, _) => seqOnto (ts, ac)
			   | (Word, Word) =>
				word (Bits.+ (width t, width t')) :: ac'
			   | _ => t :: ac))
	    in
	       case seqOnto (ts, []) of
		  [t] => t
		| ts =>
		     let
			val ts = Vector.fromList ts
		     in
			T {node = Seq ts,
			   width = Vector.fold (ts, Bits.zero, fn (t, ac) =>
						Bits.+ (ac, width t))}
		     end
	    end

      val seq = Trace.trace ("RepType.seq", Vector.layout layout, layout) seq

      val sum: t vector -> t =
	 fn ts =>
	 if 0 = Vector.length ts
	    then Error.bug "empty sum"
	 else
	    let
	       val pts =
		  Vector.concatV
		  (Vector.keepAllMap
		   (ts, fn t =>
		    case node t of
		       Pointers pts => SOME pts
		     | _ => NONE))
	    in
	       if 0 = Vector.length pts
		  then Vector.sub (ts, 0)
	       else
		  T {node = (Pointers
			     (QuickSort.sortVector (pts, PointerTycon.<=))),
		     width = Bits.inPointer}
	    end

      val sum = Trace.trace ("RepType.sum", Vector.layout layout, layout) sum

      val intInf: t =
	 sum (Vector.new2
	      (wordVector,
	       seq (Vector.new2
		    (constant (WordX.fromIntInf
			       (1, WordSize.fromBits (Bits.fromInt 1))),
		     word (Bits.fromInt 31)))))

      val word8: t = word Bits.inByte

      val words: t -> Words.t = Bits.toWords o width

      val zero: Bits.t -> t = word

      fun bytesAndPointers (t: t): Bytes.t * int =
	 case node t of
	    Pointers _ => (Bytes.zero, 1)
	  | Seq ts =>
	       (case Vector.peeki (ts, isPointer o #2) of
		   NONE => (bytes t, 0)
		 | SOME (i, _) =>
		      let
			 val b = bytes (seq (Vector.prefix (ts, i)))
		      in
			 (b, (Bytes.toInt (Bytes.- (bytes t, b))
			      div Bytes.toInt Bytes.inPointer))
		      end)
	  | _ => (bytes t, 0)

      val isZero = fn _ => false
   end

structure ObjectType =
   struct
      structure PointerTycon = PointerTycon
      structure Runtime = Runtime

      type ty = Type.t
	 
      datatype t =
	 Array of Type.t
       | Normal of Type.t
       | Stack
       | Weak of Type.t
       | WeakGone

      fun layout (t: t) =
	 let
	    open Layout
	 in
	    case t of
	       Array t => seq [str "Array ", Type.layout t]
	     | Normal t => seq [str "Normal ", Type.layout t]
	     | Stack => str "Stack"
	     | Weak t => seq [str "Weak ", Type.layout t]
	     | WeakGone => str "WeakGone"
	 end

      fun isOk (t: t): bool =
	 case t of
	    Array t =>
	       let
		  val b = Type.width t
	       in
		  Bits.> (b, Bits.zero)
		  andalso Bits.isByteAligned b
	       end
	  | Normal t =>
	       not (Type.isUnit t) andalso Bits.isWordAligned (Type.width t)
	  | Stack => true
	  | Weak t => Type.isPointer t
	  | WeakGone => true

      val stack = Stack

      val thread =
	 Normal (Type.seq
		 (Vector.new3 (Type.defaultWord,
			       Type.defaultWord,
			       Type.stack)))

      val word8Vector = Array Type.word8

      val wordVector = Array Type.defaultWord

      (* Order in the following vector matters.  The basic pointer tycons must
       * correspond to the constants in gc.h.
       * STACK_TYPE_INDEX,
       * STRING_TYPE_INDEX,
       * THREAD_TYPE_INDEX,
       * WEAK_GONE_TYPE_INDEX,
       * WORD_VECTOR_TYPE_INDEX.
       *)
      val basic =
	 Vector.fromList
	 [(PointerTycon.stack, stack),
	  (PointerTycon.word8Vector, word8Vector),
	  (PointerTycon.thread, thread),
	  (PointerTycon.weakGone, WeakGone),
	  (PointerTycon.wordVector, wordVector)]

      local
	 structure R = Runtime.RObjectType
      in
	 fun toRuntime (t: t): R.t =
	    case t of
	       Array t => let
			     val (b, p) = Type.bytesAndPointers t
			  in
			     R.Array {nonPointer = b,
				      pointers = p}
			  end
	     | Normal t => let
			      val (b, p) = Type.bytesAndPointers t
			   in
			      R.Normal {nonPointer = Bytes.toWords b,
					pointers = p}
			   end
	     | Stack => R.Stack
	     | Weak _ => R.Weak
	     | WeakGone => R.WeakGone
      end
   end

open Type
   
fun pointerHeader p =
   constant (WordX.fromIntInf
	     (1 + 2 * Int.toIntInf (PointerTycon.index p),
	      WordSize.default))

fun arrayOffsetIsOk _ = true

structure GCField = Runtime.GCField
   
fun ofGCField (f: GCField.t): t =
   let
      datatype z = datatype GCField.t
   in
      case f of
	 CanHandle => defaultWord
       | CardMap => cPointer ()
       | CurrentThread => cPointer ()
       | ExnStack => defaultWord
       | Frontier => cPointer ()
       | Limit => cPointer ()
       | LimitPlusSlop => cPointer ()
       | MaxFrameSize => defaultWord
       | SignalIsPending => bool
       | StackBottom => cPointer ()
       | StackLimit => cPointer ()
       | StackTop => cPointer ()
   end

fun castIsOk {from, to, tyconTy = _} =
   Bits.equals (width from, width to)

fun checkPrimApp _ = true

fun offsetIsOk _ = true

structure BuiltInCFunction =
   struct
      open CFunction

      datatype z = datatype Convention.t
	 
      val bug = vanilla {args = Vector.new1 string,
			 name = "MLton_bug",
			 return = unit}

      local
	 open Type
      in
	 val Word32 = word (Bits.fromInt 32)
	 val unit = unit
      end
   
      local
	 fun make b =
	    T {args = let
			 open Type
		      in
			 Vector.new5 (gcState, Word32, bool, cPointer (), Word32)
		      end,
		   bytesNeeded = NONE,
		   convention = Cdecl,
		   ensuresBytesFree = true,
		   mayGC = true,
		   maySwitchThreads = b,
		   modifiesFrontier = true,
		   name = "GC_gc",
		   readsStackTop = true,
		   return = unit,
		   writesStackTop = true}
	 val t = make true
	 val f = make false
      in
	 fun gc {maySwitchThreads = b} = if b then t else f
      end
   end

end
