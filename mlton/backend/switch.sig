(* Copyright (C) 2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

type int = Int.t
type word = Word.t
   
signature SWITCH_STRUCTS =
   sig
      include MACHINE_ATOMS

      structure Use: sig
			type t

			val layout: t -> Layout.t
			val ty: t -> Type.t
		     end
   end

signature SWITCH =
   sig
      include SWITCH_STRUCTS

      datatype t =
	 EnumPointers of {enum: Label.t,
			  pointers: Label.t,
			  test: Use.t}
       | Int of {(* Cases are in increasing order of int. *)
		 cases: (IntX.t * Label.t) vector,
		 default: Label.t option,
		 size: IntSize.t,
		 test: Use.t}
       | Pointer of {(* Cases are in increasing order of tycon. *)
		     cases: {dst: Label.t,
			     tag: int,
			     tycon: PointerTycon.t} vector,
		     default: Label.t option,
		     tag: Use.t, (* of type int *)
		     test: Use.t}
       | Word of {(* Cases are in increasing order of word. *)
		  cases: (WordX.t * Label.t) vector,
		  default: Label.t option,
		  size: WordSize.t,
		  test: Use.t}

      val foldLabelUse: t * 'a * {label: Label.t * 'a -> 'a,
				  use: Use.t * 'a -> 'a} -> 'a
      val foreachLabel: t * (Label.t -> unit) -> unit
      val isOk: t * {checkUse: Use.t -> unit,
		     labelIsOk: Label.t -> bool} -> bool
      val layout: t -> Layout.t
   end

