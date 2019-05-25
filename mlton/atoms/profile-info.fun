(* Copyright (C) 2009,2014,2016-2017,2019 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor ProfileInfo (S: PROFILE_INFO_STRUCTS): PROFILE_INFO =
struct

open S

datatype t =
   T of {frameSources: int vector,
         labels: {label: ProfileLabel.t,
                  sourceSeqsIndex: int} vector,
         names: string vector,
         sourceSeqs: int vector vector,
         sources: {nameIndex: int,
                   successorsIndex: int} vector}

val empty = T {frameSources = Vector.new0 (),
               labels = Vector.new0 (),
               names = Vector.new0 (),
               sourceSeqs = Vector.new0 (),
               sources = Vector.new0 ()}

fun clear (T {labels, ...}) =
   Vector.foreach (labels, ProfileLabel.clear o #label)

fun layout (T {frameSources, labels, names, sourceSeqs, sources}) =
   Layout.record
   [("frameSources", Vector.layout Int.layout frameSources),
    ("labels",
     Vector.layout (fn {label, sourceSeqsIndex} =>
                    Layout.record
                    [("label", ProfileLabel.layout label),
                     ("sourceSeqsIndex",
                      Int.layout sourceSeqsIndex)])
     labels),
    ("names", Vector.layout String.layout names),
    ("sourceSeqs", Vector.layout (Vector.layout Int.layout) sourceSeqs),
    ("sources",
     Vector.layout (fn {nameIndex, successorsIndex} =>
                    Layout.record [("nameIndex", Int.layout nameIndex),
                                   ("successorsIndex",
                                    Int.layout successorsIndex)])
     sources)]

fun layouts (pi, output) = output (layout pi)

fun isOK (T {frameSources, labels, names, sourceSeqs, sources}): bool =
   let
      val namesLength = Vector.length names
      val sourceSeqsLength = Vector.length sourceSeqs
      val sourcesLength = Vector.length sources
   in
      !Control.profile = Control.ProfileNone
      orelse
      (Vector.forall (frameSources, fn i =>
                      0 <= i andalso i < sourceSeqsLength)
       andalso (Vector.forall
                (labels, fn {sourceSeqsIndex = i, ...} =>
                 0 <= i andalso i < sourceSeqsLength))
       andalso (Vector.forall
                (sourceSeqs, fn v =>
                 Vector.forall
                 (v, fn i => 0 <= i andalso i < sourcesLength)))
       andalso (Vector.forall
                (sources, fn {nameIndex, successorsIndex} =>
                 0 <= nameIndex
                 andalso nameIndex < namesLength
                 andalso 0 <= successorsIndex
                 andalso successorsIndex < sourceSeqsLength)))
   end

fun modify (T {frameSources, labels, names, sourceSeqs, sources})
   : {newProfileLabel: ProfileLabel.t -> ProfileLabel.t,
      delProfileLabel: ProfileLabel.t -> unit,
      getProfileInfo: unit -> t} =
   let
      val {get: ProfileLabel.t -> int, set, ...} =
         Property.getSet
         (ProfileLabel.plist,
          Property.initRaise ("ProfileInfo.extend", ProfileLabel.layout))
      val _ =
         Vector.foreach
         (labels, fn {label, sourceSeqsIndex} =>
          set (label, sourceSeqsIndex))
      val new = ref []
      fun newProfileLabel l =
         let
            val i = get l
            val l' = ProfileLabel.new ()
            val _ = set (l', i)
            val _ = List.push (new, {label = l', sourceSeqsIndex = i})
         in
            l'
         end
      fun delProfileLabel l = set (l, ~1)
      fun getProfileInfo () =
         let
            val labels = Vector.concat [labels, Vector.fromList (!new)]
            val labels = Vector.keepAll (labels, fn {label, ...} => get label <> ~1)
            val pi = T {frameSources = frameSources,
                        labels = Vector.concat [labels, Vector.fromList (!new)],
                        names = names,
                        sourceSeqs = sourceSeqs,
                        sources = sources}
         in
            Assert.assert ("ProfileInfo.getProfileInfo", fn () => isOK pi);
            pi
         end
   in
      {newProfileLabel = newProfileLabel,
       delProfileLabel = delProfileLabel,
       getProfileInfo = getProfileInfo}
   end
end
