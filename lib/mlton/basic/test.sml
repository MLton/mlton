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
structure Z = TestRegexp (Regexp)
structure Z = TestBase64 (Base64)
structure Z = TestBinarySearch (BinarySearch)
structure Z = TestChar (Char)
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
structure Z = TestWord32 (Word)
structure Z = struct end
