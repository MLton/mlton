(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
(* Has a special case to make sure that true is represented as 1
 * and false is represented as 0.
 *)
functor Representation (S: REPRESENTATION_STRUCTS): REPRESENTATION = 
struct

open S
local open Cps
in
   structure Con = Con
   structure Tycon = Tycon
   structure Type = Type
end

structure TyconRep =
   struct
      datatype t =
	 Prim of Mtype.t
       | Enum of {numEnum: int}
       | EnumDirect of {numEnum: int}
       | EnumIndirect of {numEnum: int}
       | EnumIndirectTag of {numEnum: int,
			     numTag: int}
       | IndirectTag of {numTag: int}

      val pointer = Prim Mtype.pointer
      val void = Prim Mtype.void

      val toMtype =
	 fn Prim t => t
	  | Enum _ => Mtype.int
	  | EnumDirect _ => Mtype.pointer
	  | EnumIndirect _ => Mtype.pointer
	  | EnumIndirectTag _ => Mtype.pointer
	  | IndirectTag _ => Mtype.pointer

      val layout =
	 let open Layout
	 in fn Prim m => Mtype.layout m
       | Enum {numEnum} => seq [str "Enum ", Int.layout numEnum]
       | EnumDirect {numEnum} => seq [str "EnumDirect ", Int.layout numEnum]
       | EnumIndirect {numEnum} =>
	    seq [str "EnumIndirect ", Int.layout numEnum]
       | EnumIndirectTag {numEnum, numTag} =>
	    seq [str "EnumIndirectTag",
		 record [("numEnum", Int.layout numEnum),
			 ("numTag", Int.layout numTag)]]
       | IndirectTag {numTag} => seq [str "IndirectTag ", Int.layout numTag]
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

fun compute (Cps.Program.T {datatypes, ...}) =
   let 
      val {get = tyconRep, set = setTyconRep} =
	 Property.getSet (Tycon.plist, Property.initRaise ("rep", Tycon.layout))
      val tyconRep =
	 Trace.trace ("tyconRep", Tycon.layout, TyconRep.layout) tyconRep
      val {get = conRep, set = setConRep} =
	 Property.getSetOnce (Con.plist, Property.initRaise ("rep", Con.layout))
      val tyconMtype = TyconRep.toMtype o tyconRep
      fun toMtype t =
	 let datatype z = datatype Type.dest
	 in case Type.dest t of
	    Array _ => Mtype.pointer
	  | Char => Mtype.char
	  | Datatype c => tyconMtype c
	  | Int => Mtype.int
	  | IntInf => Mtype.pointer
	  | Pointer => Mtype.uint
	  | Real => Mtype.double
	  | Ref _ => Mtype.pointer
	  | String => Mtype.pointer
	  | Thread => Mtype.pointer
	  | Tuple ts => if Vector.isEmpty ts
			   then Mtype.void
			else Mtype.pointer
	  | Vector _ => Mtype.pointer
	  | Word => Mtype.uint
	  | Word8 => Mtype.char
	 end
      (* You can't memoize toMtype here because it depends on tyconMtype, which
       * is in the midst of being computed.
       *)
      (* Split constructors into those that carry values and those that don't. *)
      fun splitCons cons =
	 Vector.fold (cons, ([], []), fn ({con, args}, (no, have)) =>
		      if Vector.forall (args, Mtype.isVoid o toMtype)
			 then (con :: no, have)
		      else (no, {con = con, args = args} :: have))
      (* Compute a least-fixed-point on tycon representations. *)
      val _ =
	 Vector.foreach (datatypes, fn {tycon, ...} =>
			setTyconRep (tycon, TyconRep.void))
      val _ =
	 FixedPoint.fix'
	 (fn continue =>
	  Vector.foreach
	  (datatypes, fn {tycon, cons} =>
	   let
	      val (noArgs, haveArgs) = splitCons cons
	      val numEnum = List.length noArgs
	      val numTag = List.length haveArgs
	      val old = tyconRep tycon
	      val new =
		 case (noArgs, haveArgs) of
		    ([],     [])           => TyconRep.void
		  | ([_],    [])           => TyconRep.void
		  | (_,      [])           => TyconRep.Enum {numEnum = numEnum}
		  | ([],     [{args, ...}]) =>
		       (case Vector.length args of
			   0 => Error.bug "args should be nonempty"
			 | 1 => TyconRep.Prim (toMtype (Vector.sub (args, 0)))
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
				  | String => true
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
      val {get = toMtype} = Property.get (Type.plist, Property.initFun toMtype)
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
	 (datatypes, fn {tycon, cons} =>
	  let val (noArgs, haveArgs) = splitCons cons
	  in case tyconRep tycon of
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
	  end)

   in {tyconRep = tyconRep,
       conRep = conRep,
       toMtype = toMtype}
   end

end
