(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor AstMLBs (S: AST_MLBS_STRUCTS): AST_MLBS = 
struct

open S

structure AstPrograms = AstPrograms (S)

open AstPrograms Layout
   
val layouts = List.map
structure Wrap = Region.Wrap
val node = Wrap.node

(*---------------------------------------------------*)
(*                Basdecs and Basexps                *)
(*---------------------------------------------------*)

datatype basexpNode =
   Bas of basdec
 | Var of Basid.t
 | Let of basdec * basexp
and basdecNode =
   Defs of ModIdBind.t
 | Basis of {name: Basid.t,
	     def: basexp} vector
 | Local of basdec * basdec
 | Seq of basdec list
 | Open of Basid.t vector   
 | Prog of File.t * Program.t
 | MLB of File.t * OS.FileSys.file_id option * basdec
 | Prim
 | Ann of (string list * Region.t) list * basdec
withtype basexp = basexpNode Wrap.t
     and basdec = basdecNode Wrap.t

fun layoutBasexp exp =
   case node exp of
      Bas dec => align [str "bas", indent (layoutBasdec dec, 3), str "end"]
    | Var basid => Basid.layout basid
    | Let (dec, exp) => Pretty.lett (layoutBasdec dec, layoutBasexp exp)
and layoutBasdec dec =
   case node dec of
      Defs def => ModIdBind.layout def
    | Basis basbnds =>
	 layoutAndsBind
	 ("basis", "=", basbnds, fn {name, def} =>
	  (case node def of Var _ => OneLine | _ => Split 3,
	   Basid.layout name, layoutBasexp def))
    | Local (dec1, dec2) => Pretty.locall (layoutBasdec dec1, layoutBasdec dec2)
    | Seq decs => align (layoutBasdecs decs)
    | Open bs => seq [str "open ",
		      seq (separate (Vector.toListMap (bs, Basid.layout),
				     " "))]
    | Prog (f,_) => File.layout f
    | MLB (f,_,_) => File.layout f
    | Prim => str "_prim"
    | Ann (anns, dec) =>
	 align [str "ann", 
		indent ((seq o separate)
			(List.map (anns, fn (ann,_) =>
				   (seq o separate) (List.map (ann, str), " ")), 
			 ","),
			3),
		str "in", 
		indent (layoutBasdec dec, 3), str "end"]
and layoutBasdecs decs = layouts (decs, layoutBasdec)

structure Basexp =
   struct
      open Wrap
      type basdec = basdec
      type t = basexp
      datatype node = datatype basexpNode
      type node' = node
      type obj = t

      fun make n = makeRegion (n, Region.bogus)
      val bas = make o Bas
      val lett = make o Let
      val var = make o Var
      val layout = layoutBasexp
   end

structure Basdec =
   struct
      open Wrap
      type t = basdec
      datatype node = datatype basdecNode
      type node' = node
      type obj = t

      fun make n = makeRegion (n, Region.bogus)
      val ann = make o Ann
      val defs = make o Defs
      val basis = make o Basis
      val locall = make o Local
      val seq = make o Seq
      val empty = seq []
      val openn = make o Open
      val prim = make Prim
      val prog = make o Prog
      val mlb = make o MLB
      val layout = layoutBasdec
   end
end
