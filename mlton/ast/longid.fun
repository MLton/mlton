(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
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
   let
      val T {strids, id, ...} = node id
   in
      (strids, id)
   end

fun prepend (id, strid) =
   let
      val (T {strids, id}, region) = dest id
   in
      makeRegion (T {strids = strid :: strids, id = id},
                 region)
   end

fun prepends (id, strids') =
   let
      val (T {strids, id}, region) = dest id
   in
      makeRegion (T {strids = strids' @ strids, id = id},
                 region)
   end

fun isLong id =
   let
      val T {strids, ...} = node id
   in
      not (List.isEmpty strids)
   end

fun toId id =
   let
      val T {id, ...} = node id
   in
      id
   end
   
val equals =
   fn (id, id') =>
   let
      val T {strids=ss, id=i} = node id
      val T {strids=ss', id=i'} = node id'
   in
      List.equals (ss, ss', Strid.equals) andalso Id.equals (i, i')
   end
   
fun long (strids, id) =
   makeRegion (T {strids = strids, id = id},
               case strids of
                  [] => Id.region id
                | s :: _ => Region.append (Strid.region s, Id.region id))

fun short id = long ([], id)

fun layout id =
   let
      val T {strids, id} = node id
      open Layout
   in
      seq [case strids of
              [] => empty
            | _ => seq [seq (separate (List.map (strids, Strid.layout), ".")),
                        str "."],
           Id.layout id]
   end

val toString = Layout.toString o layout

fun fromSymbols (ss: Symbol.t list, region: Region.t): t =
   let
      val (strids, id) = List.splitLast ss
   in
      makeRegion (T {strids = List.map (strids, fn s =>
                                        Strid.fromSymbol (s, region)),
                     id = Id.fromSymbol (id, region)},
                  region)
   end

val bogus = short Id.bogus

end
