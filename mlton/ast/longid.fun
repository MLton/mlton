(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor Longid (S: LONGID_STRUCTS): LONGID =
struct

open S

datatype node = T of {strids: Strid.t list,
		      id: Id.t}
type node' = node
structure Wrap = Region.Wrap
open Wrap
type t = node Wrap.t
type obj = t

fun split id =
   let val T {strids, id, ...} = node id
   in (strids, id)
   end

fun prepend (id, strid) =
   let val (T {strids, id}, region) = dest id
   in makeRegion (T {strids = strid :: strids, id = id},
		 region)
   end

fun prepends (id, strids') =
   let val (T {strids, id}, region) = dest id
   in makeRegion (T {strids = strids' @ strids, id = id},
		 region)
   end

fun isLong id =
   let val T {strids, ...} = node id
   in not (List.isEmpty strids)
   end

fun toId id =
   let val T {id, ...} = node id
   in id
   end
   
val equals =
   fn (id, id') =>
   let val T {strids=ss, id=i} = node id
      val T {strids=ss', id=i'} = node id'
   in List.equals (ss, ss', Strid.equals) andalso Id.equals (i, i')
   end
   
fun long (strids, id) = makeRegion (T {strids = strids, id = id},
				    Region.bogus)

fun short id = long ([], id)

fun layout id =
   let
      val T {strids, id} = node id
      open Layout
   in seq [case strids of
	      [] => empty
	    | _ => seq [seq (separate (List.map (strids, Strid.layout), ".")),
			str "."],
	   Id.layout id]
   end

val toString = Layout.toString o layout

fun fromString (longid: string, region: Region.t): t =
   let
      val (strids, id) = List.splitLast (String.split (longid, #"."))
   in
      makeRegion (T {strids = List.map (strids, fn s =>
					Strid.fromString (s, region)),
		     id = Id.fromString (id, region)},
		  region)
   end

val bogus = short Id.bogus

end
