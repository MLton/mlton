(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
(* Has a special case to make sure that true is represented as 1
 * and false is represented as 0.
 *)
functor Representation (S: REPRESENTATION_STRUCTS): REPRESENTATION = 
struct

open S
local open Ssa
in
   structure Con = Con
   structure Datatype = Datatype
   structure Tycon = Tycon
   structure Type = Type
end

structure TyconRep =
   struct
      datatype t =
	 Enum of {numEnum: int}
       | EnumDirect of {numEnum: int}
       | EnumIndirect of {numEnum: int}
       | EnumIndirectTag of {numEnum: int,
			     numTag: int}
       | IndirectTag of {numTag: int}
       | Prim of Mtype.t
       | Void

      val pointer = Prim Mtype.pointer

      val toMtype =
	 fn Enum _ => SOME Mtype.int
	  | EnumDirect _ => SOME Mtype.pointer
	  | EnumIndirect _ => SOME Mtype.pointer
	  | EnumIndirectTag _ => SOME Mtype.pointer
	  | IndirectTag _ => SOME Mtype.pointer
	  | Prim t => SOME t
	  | Void => NONE

      val layout =
	 let
	    open Layout
	 in
	    fn Enum {numEnum} => seq [str "Enum ", Int.layout numEnum]
	     | EnumDirect {numEnum} =>
		  seq [str "EnumDirect ", Int.layout numEnum]
	     | EnumIndirect {numEnum} =>
		  seq [str "EnumIndirect ", Int.layout numEnum]
	     | EnumIndirectTag {numEnum, numTag} =>
		  seq [str "EnumIndirectTag",
		       record [("numEnum", Int.layout numEnum),
			       ("numTag", Int.layout numTag)]]
	     | IndirectTag {numTag} =>
		  seq [str "IndirectTag ", Int.layout numTag]
	     | Prim m => Mtype.layout m
	     | Void => str "Void"
	 end
      
      val equals =
	 fn (Prim t, Prim t') => Mtype.equals (t, t')
	  | (Enum {numEnum = n}, Enum {numEnum = n'}) => n = n'
	  | (EnumDirect {numEnum = n}, EnumDirect {numEnum = n'}) => n = n'
	  | (EnumIndirect {numEnum = n}, EnumIndirect {numEnum = n'}) => n = n'
	  | (EnumIndirectTag {numEnum = n, numTag = t},
	     EnumIndirectTag {numEnum = n', numTag = t'}) =>
	    n = n' andalso t = t'
	   | (IndirectTag {numTag = n}, IndirectTag {numTag = n'}) => n = n'
	   | (Void, Void) => true
	   | _ => false
   end

structure ConRep =
   struct
      datatype t =
	 Void
       | Int of int
       | IntCast of int
       | Transparent of Mtype.t
       | Tuple
       | TagTuple of int

      local
	 open Layout
      in
	 val layout =
	    fn Void => str "Void"
	     | Int n => seq [str "Int ", Int.layout n]
	     | IntCast n => seq [str "IntCast ", Int.layout n]
	     | Transparent t => seq [str "Transparent ", Mtype.layout t]
	     | Tuple => str "Tuple"
	     | TagTuple n => seq [str "TagTuple ", Int.layout n]
      end
   end

(* fixed-point.  Initially assume all datatype tycons are Void
 * Change them if they have more than one variant or contain
 * a useful component
 *)

fun compute (Ssa.Program.T {datatypes, ...}) =
   let 
      val {get = tyconRep, set = setTyconRep, ...} =
	 Property.getSet (Tycon.plist, Property.initRaise ("rep", Tycon.layout))
      val tyconRep =
	 Trace.trace ("tyconRep", Tycon.layout, TyconRep.layout) tyconRep
      val {get = conRep, set = setConRep, ...} =
	 Property.getSetOnce (Con.plist, Property.initRaise ("rep", Con.layout))
      val tyconMtype = TyconRep.toMtype o tyconRep
      fun toMtype t =
	 let
	    datatype z = datatype Type.dest
	 in
	    case Type.dest t of
	       Array _ => SOME Mtype.pointer
	     | Char => SOME Mtype.char
	     | Datatype c => tyconMtype c
	     | Int => SOME Mtype.int
	     | IntInf => SOME Mtype.pointer
	     | Pointer => SOME Mtype.uint
	     | PreThread => SOME Mtype.pointer
	     | Real => SOME Mtype.double
	     | Ref _ => SOME Mtype.pointer
	     | Thread => SOME Mtype.pointer
	     | Tuple ts => if Vector.isEmpty ts
			      then NONE
			   else SOME Mtype.pointer
	     | Vector _ => SOME Mtype.pointer
	     | Word => SOME Mtype.uint
	     | Word8 => SOME Mtype.char
	 end
      (* You can't memoize toMtype here because it depends on tyconMtype, which
       * is in the midst of being computed.
       *)
      (* Split constructors into those that carry values and those that don't. *)
      fun splitCons cons =
	 Vector.fold (cons, ([], []), fn ({con, args}, (no, have)) =>
		      if Vector.forall (args, Option.isNone o toMtype)
			 then (con :: no, have)
		      else (no, {con = con, args = args} :: have))
      (* Compute a least-fixed-point on tycon representations. *)
      val _ =
	 Vector.foreach (datatypes, fn Datatype.T {tycon, ...} =>
			 setTyconRep (tycon, TyconRep.Void))
      val _ =
	 FixedPoint.fix'
	 (fn continue =>
	  Vector.foreach
	  (datatypes, fn Datatype.T {tycon, cons} =>
	   let
	      val (noArgs, haveArgs) = splitCons cons
	      val numEnum = List.length noArgs
	      val numTag = List.length haveArgs
	      val old = tyconRep tycon
	      val new =
		 case (noArgs, haveArgs) of
		    ([],     [])           => TyconRep.Void
		  | ([_],    [])           => TyconRep.Void
		  | (_,      [])           => TyconRep.Enum {numEnum = numEnum}
		  | ([],     [{args, ...}]) =>
		       (case Vector.length args of
			   0 => Error.bug "args should be nonempty"
			 | 1 => (case toMtype (Vector.sub (args, 0)) of
				    NONE => TyconRep.Void
				  | SOME t => TyconRep.Prim t)
			 | _ => TyconRep.pointer)
		  | (_,      [{args, ...}]) =>
		       if (if 1 = Vector.length args
			      then
				 let
				    val a = Vector.sub (args, 0)
				    (* Which types are guaranteed to be represented
				     * as zero mod 4.  You can't use IntInf or
				     * Thread here -- In fact, it's not clear to
				     * me you can use anything, because of bogus
				     * values.
				     *)
				    open Type
				 in case Type.dest a of
				    Array _ => true
				  | Datatype c =>
				       (case tyconRep c of
					   TyconRep.IndirectTag _ => true
					 | _ => false)
				  | Ref _ => true
				  | Tuple _ => true
				  | Vector _ => true
				  | _ => false
				 end
			   else true)
			  then TyconRep.EnumDirect {numEnum = numEnum}
		       else TyconRep.EnumIndirect {numEnum = numEnum}
		  | ([],     _)            =>
		       TyconRep.IndirectTag {numTag = numTag}
		  | _                      =>
		       TyconRep.EnumIndirectTag {numEnum = numEnum,
						 numTag = numTag}
	   in if TyconRep.equals (old, new)
		 then ()
	      else (continue ()
		    ; setTyconRep (tycon, new))
	   end))
      (* Now we can memoize toMtype. *)
      val {get = toMtype, ...} =
	 Property.get (Type.plist, Property.initFun toMtype)
      (* Set constructor representations. *)
      fun direct (con, args, t) =
	 setConRep (con,
		    if 1 = Vector.length args
		       then ConRep.Transparent t
		    else ConRep.Tuple)
      (* Choose tags that are not equal to 0 mod 4. *)
      fun enum noArgs =
	 let
	    fun loop (i, cs) =
	       case cs of
		  [] => ()
		| c :: cs => (setConRep (c, ConRep.IntCast i)
			      ; loop (i + 2, cs))
	 in loop (1, noArgs)
	 end
      fun indirectTag haveArgs = 
	 List.foreachi (haveArgs, fn (i, {con, args}) =>
			setConRep (con, ConRep.TagTuple i))
      val _ =
	 Vector.foreach
	 (datatypes, fn Datatype.T {tycon, cons} =>
	  let
	     val (noArgs, haveArgs) = splitCons cons
	  in
	     case tyconRep tycon of
		TyconRep.Prim t =>
		   (case (noArgs, haveArgs) of
		       ([], []) => ()
		     | ([con], []) => setConRep (con, ConRep.Void)
		     | ([], [{con, args}]) => direct (con, args, t)
		     | _ => Error.bug ("strange TyconRep.Prim for "
				       ^ Layout.toString (Tycon.layout tycon)))
	      | TyconRep.Enum _ =>
		   if Tycon.equals (tycon, Tycon.bool)
		      then (setConRep (Con.falsee, ConRep.Int 0)
			    ; setConRep (Con.truee, ConRep.Int 1))
		   else List.foreachi (noArgs, fn (i, c) =>
				       setConRep (c, ConRep.Int i))
	      | TyconRep.EnumDirect _ =>
		   (enum noArgs
		    ; (case haveArgs of
			  [{con, args}] => direct (con, args, Mtype.pointer)
			| _ => Error.bug "strange haveArgs for EnumDirect"))
	      | TyconRep.EnumIndirect _ =>
		   (enum noArgs
		    ; List.foreach (haveArgs, fn {con, ...} =>
				    setConRep (con, ConRep.Tuple)))
	      | TyconRep.EnumIndirectTag _ => (enum noArgs; indirectTag haveArgs)
	      | TyconRep.IndirectTag _ => indirectTag haveArgs
	      | TyconRep.Void =>
		   (case (noArgs, haveArgs) of
		       ([], []) => ()
		     | ([con], []) => setConRep (con, ConRep.Void)
		     | _ => Error.bug "strange TyconRep.Void")
	  end)
   in
      {tyconRep = tyconRep,
       conRep = conRep,
       toMtype = toMtype}
   end

end
