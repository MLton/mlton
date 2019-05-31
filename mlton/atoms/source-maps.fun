(* Copyright (C) 2009,2014,2016-2017,2019 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor SourceMaps (S: SOURCE_MAPS_STRUCTS): SOURCE_MAPS =
struct

open S

datatype t =
   T of {profileLabelInfos: {profileLabel: ProfileLabel.t,
                             sourceSeqIndex: int} vector,
         sourceNames: string vector,
         sourceSeqs: {sourceIndex: int} vector vector,
         sources: {sourceNameIndex: int,
                   successorSourceSeqIndex: int} vector}

val empty = T {profileLabelInfos = Vector.new0 (),
               sourceNames = Vector.new0 (),
               sourceSeqs = Vector.new0 (),
               sources = Vector.new0 ()}

fun clear (T {profileLabelInfos, ...}) =
   Vector.foreach (profileLabelInfos, ProfileLabel.clear o #profileLabel)

fun layout (T {profileLabelInfos, sourceNames, sourceSeqs, sources}) =
   Layout.record
   [("profileLabelInfos",
     Vector.layout (fn {profileLabel, sourceSeqIndex} =>
                    Layout.record
                    [("profileLabel", ProfileLabel.layout profileLabel),
                     ("sourceSeqIndex", Int.layout sourceSeqIndex)])
     profileLabelInfos),
    ("sourceNames", Vector.layout String.layout sourceNames),
    ("sourceSeqs",
     Vector.layout (Vector.layout (fn {sourceIndex} =>
                                   Layout.record
                                   [("sourceIndex", Int.layout sourceIndex)]))
     sourceSeqs),
    ("sources",
     Vector.layout (fn {sourceNameIndex, successorSourceSeqIndex} =>
                    Layout.record [("sourceNameIndex", Int.layout sourceNameIndex),
                                   ("successorSourceSeqIndex", Int.layout successorSourceSeqIndex)])
     sources)]

fun layouts (pi, output) = output (layout pi)

fun check (T {profileLabelInfos, sourceNames, sourceSeqs, sources}): bool =
   if !Control.profile = Control.ProfileNone
      then Vector.isEmpty profileLabelInfos
           andalso Vector.isEmpty sourceNames
           andalso Vector.isEmpty sourceSeqs
           andalso Vector.isEmpty sources
      else let
              val sourceNamesLength = Vector.length sourceNames
              val sourceSeqsLength = Vector.length sourceSeqs
              val sourcesLength = Vector.length sources
              val {get = getSeen, destroy = destSeen, ...} =
                 Property.destGet
                 (ProfileLabel.plist, Property.initFun (fn _ => ref false))
           in
              (Vector.forall
               (profileLabelInfos, fn {profileLabel, sourceSeqIndex, ...} =>
                let
                   val seen = getSeen profileLabel
                in
                   not (!seen) before (seen := true)
                   andalso 0 <= sourceSeqIndex andalso sourceSeqIndex < sourceSeqsLength
                end)
               before destSeen ())
              andalso (Vector.forall
                       (sourceSeqs, fn v =>
                        Vector.forall
                        (v, fn {sourceIndex} =>
                         0 <= sourceIndex andalso sourceIndex < sourcesLength)))
              andalso (Vector.forall
                       (sources, fn {sourceNameIndex, successorSourceSeqIndex} =>
                        0 <= sourceNameIndex andalso sourceNameIndex < sourceNamesLength
                        andalso
                        0 <= successorSourceSeqIndex
                        andalso successorSourceSeqIndex < sourceSeqsLength))
           end

fun checkSourceSeqIndex (T {sourceSeqs, ...}, sourceSeqIndex) =
   0 <= sourceSeqIndex andalso sourceSeqIndex < Vector.length sourceSeqs
fun checkProfileLabel (T {profileLabelInfos, ...}) =
   let
      val {get = getSeen, destroy = destSeen, ...} =
         Property.destGet
         (ProfileLabel.plist, Property.initFun (fn _ => ref false))
   in
      (fn profileLabel =>
       let
          val seen = getSeen profileLabel
       in
          not (!seen) before (seen := true)
          andalso Vector.exists (profileLabelInfos, fn {profileLabel = profileLabel', ...} =>
                                 ProfileLabel.equals (profileLabel, profileLabel'))
       end,
       fn () =>
       let
       in
          Vector.forall (profileLabelInfos, ! o getSeen o #profileLabel)
          before destSeen ()
       end)
   end

fun modify (T {profileLabelInfos, sourceNames, sourceSeqs, sources})
   : {newProfileLabel: ProfileLabel.t -> ProfileLabel.t,
      delProfileLabel: ProfileLabel.t -> unit,
      getSourceMaps: unit -> t} =
   let
      val {get: ProfileLabel.t -> int, set, ...} =
         Property.getSet
         (ProfileLabel.plist,
          Property.initRaise ("SourceMaps.extend", ProfileLabel.layout))
      val _ =
         Vector.foreach
         (profileLabelInfos, fn {profileLabel, sourceSeqIndex} =>
          set (profileLabel, sourceSeqIndex))
      val new = ref []
      fun newProfileLabel pl =
         let
            val sourceSeqIndex = get pl
            val pl' = ProfileLabel.new ()
            val _ = set (pl', sourceSeqIndex)
            val _ = List.push (new, {profileLabel = pl',
                                     sourceSeqIndex = sourceSeqIndex})
         in
            pl'
         end
      fun delProfileLabel pl = set (pl, ~1)
      fun getSourceMaps () =
         let
            val profileLabelInfos =
               Vector.concat [profileLabelInfos, Vector.fromList (!new)]
            val profileLabelInfos =
               Vector.keepAll (profileLabelInfos, fn {profileLabel, ...} =>
                               get profileLabel <> ~1)
            val pi = T {profileLabelInfos = profileLabelInfos,
                        sourceNames = sourceNames,
                        sourceSeqs = sourceSeqs,
                        sources = sources}
         in
            Assert.assert ("SourceMaps.getSourceMaps", fn () => check pi);
            pi
         end
   in
      {newProfileLabel = newProfileLabel,
       delProfileLabel = delProfileLabel,
       getSourceMaps = getSourceMaps}
   end
end
