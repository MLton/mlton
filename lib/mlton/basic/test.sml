(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Z =
   struct
      val _ =
         let open Trace.Immediate
         in debug := Out Out.error
            ; flagged()
            ; ["concat"]
            ; ["Uri.fromString", "Uri.resolve", "Uri.relativize",
               "Authority.equals"]
            ; ["Uri.fromString", "Uri.resolve", "Uri.toString",
                  "Uri.relativize", "Uri.checkResolve"]
            ; ["Regexp.match"]
         end
   end
structure Z = TestBase64 (Base64)
structure Z = TestBinarySearch (BinarySearch)
structure Z = TestChar (Char)
structure Z = TestDirectedGraph (DirectedGraph)
structure Z = TestFormat (Format)
structure Z = TestHashSet (HashSet)
structure Z = TestHttp (Http)
structure Z = TestLinkedList (LinkedList)
structure Z = TestList (List)
structure Z = TestProcess (Process)
structure Z = TestQuickSort (QuickSort)
structure Z = TestRegexp (Regexp)
structure Z = TestString (String)
structure Z = TestUri (Uri)
structure Z = TestVector (Vector)
structure Z = struct end
