(* Copyright (C) 2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

functor MachineAtoms (S: MACHINE_ATOMS_STRUCTS): MACHINE_ATOMS =
struct

open S

structure PointerTycon =
   struct
      datatype t = T of {index: int,
			 plist: PropertyList.t}

      local
	 fun make f (T r) = f r
      in
	 val index = make #index
	 val plist = make #plist
      end
   
      fun equals (pt, pt') = PropertyList.equals (plist pt, plist pt')

      val op <= = fn (pt, pt') => index pt <= index pt'

      fun toString (T {index, ...}) =
	 concat ["pt_", Int.toString index]

      val layout = Layout.str o toString

      val c = Counter.new 0

      fun new () =
	 T {index = Counter.next c,
	    plist = PropertyList.new ()}

      (* These basic pointer tycons are hardwired into the runtime and are
       * prefixed to every user program.  See gc.h for the definitions of
       * {STACK,STRING,THREAD,WORD_VECTOR}_TYPE_INDEX.
       *)
      val stack = new ()
      val string = new ()
      val thread = new ()
      val wordVector = new ()
   end

structure TypeAndMemChunk =
   struct
      datatype ty =
	 Char
       | CPointer
       | EnumPointers of {enum: int vector,
			  pointers: PointerTycon.t vector}
       | ExnStack
       | Int
       | IntInf
       | Label of Label.t
       | MemChunk of memChunk
       | Real
       | Word
      and memChunk =
	 T of {components: {mutable: bool,
			    offset: int,
			    ty: ty} vector,
	       size: int}

      fun layoutTy (t: ty) =
	 let
	    open Layout
	 in
	    case t of
	       Char => str "char"
	     | CPointer => str "cpointer"
	     | EnumPointers {enum, pointers} => 
		  if 0 = Vector.length enum
		     andalso 1 = Vector.length pointers
		     then PointerTycon.layout (Vector.sub (pointers, 0))
		  else
		     Vector.layout (fn x => x)
		     (Vector.concat [Vector.map (enum, Int.layout),
				     Vector.map (pointers, PointerTycon.layout)])
	     | ExnStack => str "exnStack"
	     | Int => str "int"
	     | IntInf => str "intInf"
	     | Label l => seq [str "Label ", Label.layout l]
	     | MemChunk m => seq [str "MemChunk ", layoutMemChunk m]
	     | Real => str "real"
	     | Word => str "word"
	 end
      and layoutMemChunk (T {components, size}) =
	 Layout.record
	 [("components",
	   Vector.layout (fn {mutable, offset, ty} =>
			  Layout.record [("mutable", Bool.layout mutable),
					 ("offset", Int.layout offset),
					 ("ty", layoutTy ty)])
	   components),
	  ("size", Int.layout size)]

      fun equalsTy (t, t'): bool =
	 case (t, t') of
	    (Char, Char) => true
	  | (CPointer, CPointer) => true
	  | (EnumPointers {enum = e, pointers = p},
	     EnumPointers {enum = e', pointers = p'}) =>
	       e = e'
	       andalso (MLton.eq (p, p')
			orelse Vector.equals (p, p', PointerTycon.equals))
          | (ExnStack, ExnStack) => true
	  | (Int, Int) => true
	  | (IntInf, IntInf) => true
	  | (Label l, Label l') => Label.equals (l, l')
	  | (MemChunk m, MemChunk m') => equalsMemChunk (m, m')
	  | (Real, Real) => true
	  | (Word, Word) => true
	  | _ => false
      and equalsMemChunk (T {components = cs, size = s},
			  T {components = cs', size = s'}) =
	 s = s'
	 andalso
	 Vector.equals (cs, cs', fn ({mutable = m, offset = i, ty = t},
				     {mutable = m', offset = i', ty = t'}) =>
			m = m' andalso i = i' andalso equalsTy (t, t'))

      local
	 val byte: int = 1
	 val word: int = 4
	 val double: int = 8
      in
	 val size =
	    fn Char => byte
	     | CPointer => word
	     | EnumPointers _ => word
	     | ExnStack => word
	     | Int => word
	     | IntInf => word
	     | Label _ => word
	     | MemChunk _ => word
	     | Real => double
	     | Word => word
      end

      fun isOkTy (t: ty): bool =
	 case t of
	    Char => true
	  | CPointer => true
	  | EnumPointers {enum, pointers} =>
	       Vector.isSorted (enum, op <=)
	       andalso Vector.isSorted (pointers, PointerTycon.<=)
	       andalso (0 = Vector.length pointers
			orelse Vector.forall (enum, Int.isOdd))
	  | ExnStack => true
	  | Int => true
	  | IntInf => true
	  | Label _ => true
	  | MemChunk m => isOkMemChunk m
	  | Real => true
	  | Word => true
      and isOkMemChunk (T {components, size = s}) =
	 let
	    exception No
	    fun no () = raise No
	    fun doit () =
	       Vector.fold
	       (components, (0, false),
		fn ({offset, ty, ...}, (prev, isPointer)) =>
		if prev <= offset
		   andalso isOkTy ty
		   andalso (case ty of
			       (* Can't store pointers to MemChunks in other
				* MemChunks.
				*)
			       MemChunk _ => false
			     | _ => true)
		   then (offset + size ty,
			 let
			    fun nonPointer () =
			       if isPointer
				  then no ()
			       else false
			 in
			    case ty of
			       EnumPointers {pointers, ...} =>
				  if 0 = Vector.length pointers
				     then nonPointer ()
				  else true
			     | IntInf => true
			     | _ => nonPointer ()
			 end)
		else no ())
	 in
	    #1 (doit ()) <= s
	    handle No => false
	 end
   end

type memChunk = TypeAndMemChunk.memChunk
   
structure Type =
   struct
      local
	 open TypeAndMemChunk
      in
	 datatype t = datatype ty

	 val equals = equalsTy
	 val layout = layoutTy
	 val size = size
      end

      val equals =
	 Trace.trace2 ("Machine.Type.equals", layout, layout, Bool.layout)
	 equals

      val toString = Layout.toString o layout

      val bool = EnumPointers {enum = Vector.new2 (0, 1),
			       pointers = Vector.new0 ()}
      val char = Char
      val cpointer = CPointer
      val exnStack = ExnStack
      val int = Int
      val intInf = IntInf
      val label = Label
      val real = Real
      val word = Word

      fun pointer pt =
	 EnumPointers {enum = Vector.new0 (),
		       pointers = Vector.new1 pt}

      val stack = pointer PointerTycon.stack
      val string = pointer PointerTycon.string
      val thread = pointer PointerTycon.thread
      val wordVector = pointer PointerTycon.wordVector

      fun containsPointer (t, pt): bool =
	 case t of
	    EnumPointers {pointers, ...} =>
	       Vector.exists (pointers, fn pt' => PointerTycon.equals (pt, pt'))
	  | _ => false

      val isPointer =
	 fn EnumPointers {pointers, ...} => 0 < Vector.length pointers
	  | IntInf => true
	  | _ => false

      fun split ({enum, pointers}) =
	 {enum = {enum = enum, pointers = Vector.new0 ()},
	  pointers = {enum = Vector.new0 (), pointers = pointers}}

      local
	 structure R = Runtime.Type
      in
	 val fromRuntime: Runtime.Type.t -> t =
	    fn t =>
	    case R.dest t of
	       R.Char => char
	     | R.Double => real
	     | R.Int => int
	     | R.Pointer => cpointer
	     | Uint => word

	 val toRuntime: t -> Runtime.Type.t =
	    fn Char => R.char
	     | CPointer => R.pointer
	     | EnumPointers {enum, pointers} =>
		  if 0 = Vector.length pointers
		     then R.int
		  else R.pointer
	     | ExnStack => R.uint
	     | Int => R.int
	     | IntInf => R.pointer
	     | Label _ => R.uint
	     | MemChunk _ => R.pointer
	     | Real => R.double
	     | Word => R.word

	 val name = R.name o toRuntime

	 fun align (t: t, n: int): int = R.align (toRuntime t, n)
      end

      val equals =
	 Trace.trace2 ("Rtype.equals", layout, layout, Bool.layout) equals

      fun dePointer t =
	 case t of
	    EnumPointers {enum, pointers} =>
	       if 0 = Vector.length enum
		  andalso 1 = Vector.length pointers
		  then SOME (Vector.sub (pointers, 0))
	       else NONE
	  | _ => NONE
   end

structure MemChunk =
   struct
      local
	 open TypeAndMemChunk
      in
	 datatype t = datatype memChunk

	 val isOk = isOkMemChunk
	 val layout = layoutMemChunk
      end

      fun numBytesAndPointers (T {components, size}) =
	 let
	    val offset =
	       case Vector.peek (components, Type.isPointer o #ty) of
		  NONE => size
		| SOME {offset, ...} => offset
	 in
	    (offset, Int.quot (size - offset, Runtime.pointerSize))
	 end

      fun isValidInit (T {components, ...},
		       stores: {offset: int, ty: Type.t} vector): bool =
	 Vector.length stores = Vector.length components
	 andalso
	 Vector.isSorted
	 (stores, fn ({offset = off, ...}, {offset = off', ...}) =>
	  off <= off')
	 andalso
	 Vector.forall2
	 (components, stores, fn ({offset = off, ty, ...},
				  {offset = off', ty = ty'}) =>
	  off = off' andalso Type.equals (ty, ty'))
   end

structure ObjectType =
   struct
      datatype t =
	 Array of MemChunk.t
       | Normal of MemChunk.t
       | Stack

      fun layout (t: t) =
	 let
	    open Layout
	 in
	    case t of
	       Array mc => seq [str "Array ", MemChunk.layout mc]
	     | Normal mc => seq [str "Normal ", MemChunk.layout mc]
	     | Stack => str "Stack"
	 end

      val wordSize = Runtime.wordSize
	 
      val stack = Stack
      val string =
	 Array (MemChunk.T {components = Vector.new1 {mutable = false,
						      offset = 0,
						      ty = Type.char},
			    size = 1})
      val thread =
	 let
	    val components =
	       Vector.new3 ({mutable = true,
			     offset = 0,
			     ty = Type.word},
			    {mutable = true,
			     offset = wordSize,
			     ty = Type.word},
			    {mutable = true,
			     offset = 2 * wordSize,
			     ty = Type.stack})
	 in			     
	    Normal (MemChunk.T {components = components,
				size = 3 * wordSize})
	 end
      val wordVector =
	 Array (MemChunk.T {components = Vector.new1 {mutable = false,
						      offset = 0,
						      ty = Type.word},
			    size = wordSize})
		
      val isOk =
	 fn Array mc => MemChunk.isOk mc
	  | Normal mc => MemChunk.isOk mc
	  | Stack => true

      local
	 structure R = Runtime.ObjectType
      in
	 fun toRuntime t =
	    case t of
	       Array m => let
			     val (b, p) = MemChunk.numBytesAndPointers m
			  in
			     R.Array {numBytesNonPointers = b,
				      numPointers = p}
			  end
	     | Normal m => let
			      val (b, p) = MemChunk.numBytesAndPointers m
			     val w = Int.quot (b, Runtime.wordSize)
			  in
			     R.Normal {numWordsNonPointers = w,
				       numPointers = p}
			  end
	     | Stack => R.Stack
      end

      val basic =
	 Vector.fromList
	 [(PointerTycon.stack, stack),
	  (PointerTycon.string, string),
	  (PointerTycon.thread, thread),
	  (PointerTycon.wordVector, wordVector)]
   end

fun castIsOk {from: Type.t,
	      fromInt: int option,
	      to: Type.t,
	      tyconTy: PointerTycon.t -> ObjectType.t}: bool =
   let
      fun castEnumIsOk ({enum = e, pointers = p},
			{enum = e', pointers = p'}): bool =
	 (* Safe subtyping. *)
	 (Vector.isSubsequence (e, e', op =)
	  andalso Vector.isSubsequence (p, p', PointerTycon.equals))
	 orelse
	 (* Unsafe Vector_fromArray. *)
	 (0 = Vector.length e
	  andalso 0 = Vector.length e'
	  andalso 1 = Vector.length p
	  andalso 1 = Vector.length p'
	  andalso
	  (case (tyconTy (Vector.sub (p, 0)),
		 tyconTy (Vector.sub (p', 0))) of
	      (ObjectType.Array (MemChunk.T {components = cs, size = s}),
	       ObjectType.Array (MemChunk.T {components = cs', size = s'})) =>
	      s = s'
	      andalso
	      Vector.equals
	      (cs, cs', fn ({offset = off, ty, ...},
			    {offset = off', ty = ty', ...}) =>
	       off = off' andalso Type.equals (ty, ty'))
	     | _ => false))
	 orelse
	 (* Unsafe downcast, but we use it in SwitchEnumPointers
	  * and SwitchPointer. It should be made properly type safe
	  * by using dominators or somesuch.
	  *)
	 (if 0 = Vector.length e
	     then (0 = Vector.length e'
		   andalso 1 = Vector.length p'
		   andalso
		   let
		      val pt = Vector.sub (p', 0)
		   in
		      Vector.exists (p, fn pt' =>
				     PointerTycon.equals (pt, pt'))
		   end)
	  else
	     (e = e' andalso 0 = Vector.length p')
	     orelse
	     ((MLton.eq (p, p')
	       orelse Vector.equals (p, p', PointerTycon.equals))
	      andalso 0 = Vector.length e'))
      datatype z = datatype Type.t
   in
      not (Type.equals (from, to))
      andalso
      case from of
	 CPointer =>
	    (case to of
		Int => true
	      | Word => true
	      | _ => false)
       | EnumPointers (ep as {enum, pointers}) =>
	    (case to of
		EnumPointers ep' => castEnumIsOk (ep, ep')
	      | IntInf =>
		   (* IntInf_fromVector *)
		   0 = Vector.length enum
		   andalso 1 = Vector.length pointers
		   andalso PointerTycon.equals (PointerTycon.wordVector,
						Vector.sub (pointers, 0))
	      | Word => true (* necessary for card marking *)
	      | _ => false)
       | Int =>
	    (case to of
		EnumPointers {enum, ...} =>
		   (case fromInt of
		       NONE => false
		     | SOME int => Vector.exists (enum, fn i => i = int))
		   orelse
		   (* MLton_bogus *)
		   (0 = Vector.length enum
		    andalso (case fromInt of
				SOME 1 => true
			      | _ => false))
	      | Word => true (* Word32_fromInt *)
	      | _ => false)
       | IntInf =>
	    (case to of
		EnumPointers {enum, pointers} =>
		   (* IntInf_toVector *)
		   0 = Vector.length enum
		   andalso 1 = Vector.length pointers
		   andalso PointerTycon.equals (PointerTycon.wordVector,
						Vector.sub (pointers, 0))
	      | Word => true  (* IntInf_toWord *)
	      | _ => false)
       | MemChunk _ =>
	    (case to of
		Word => true (* needed for card marking of arrays *)
	      | _ => false)
       | Word =>
	    (case to of
		Int => true (* Word32_toIntX *)
	      | IntInf => true (* IntInf_fromWord *)
	      | _ => false)
       | _ => false
   end

end
