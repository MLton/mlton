(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
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
   
fun long (strids, id) = make (T {strids = strids, id = id})

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
   let val (strids, id) = List.splitLast (String.split (longid, #"."))
   in makeRegion (T {strids = List.map (strids, Strid.fromString),
		     id = Id.fromString id},
		  region)
   end

val className = Id.className

val bogus = short Id.bogus
   
fun unbound (x: t): unit =
   Control.error
   (region x,
    let open Layout
    in seq [str "unbound ", str className, str " ", layout x]
    end)

end
