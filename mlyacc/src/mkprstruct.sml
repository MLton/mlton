(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

functor mkPrintStruct(structure LrTable : LR_TABLE
		      structure ShrinkLrTable : SHRINK_LR_TABLE
		      sharing LrTable = ShrinkLrTable.LrTable):PRINT_STRUCT =
   struct
      open Array List
      infix 9 sub
      structure LrTable = LrTable
      open ShrinkLrTable LrTable
      
     
      (* lineLength = approximately the largest number of characters to allow
	 on a line when printing out an encode string *)
	  
      val lineLength = 72

      (* maxLength = length of a table entry.  All table entries are encoded
	 using two 16-bit integers, one for the terminal number and the other
	 for the entry.  Each integer is printed as two characters (low byte,
	 high byte), using the ML ascii escape sequence.  We need 4
	 characters for each escape sequence and 16 characters for each entry
      *)

      val maxLength =  16

      (* number of entries we can fit on a row *)

      val numEntries = lineLength div maxLength

      (* convert integer between 0 and 255 to the three character ascii
	 decimal escape sequence for it *)

      val chr =
	let val lookup = Array.array(256,"\000")
	    val intToString = fn i =>
		if i>=100 then "\\" ^ (Int.toString i)
		else if i>=10 then "\\0" ^ (Int.toString i)
		else  "\\00" ^ (Int.toString i)
	    fun loop n = if n=256 then ()
			 else (Array.update(lookup,n,intToString n); loop (n+1))
	in loop 0; fn i => lookup sub i
	end

      val makeStruct = fn {table,name,print,verbose} =>
       let
	 val states = numStates table
	 val rules = numRules table
         fun printPairList (prEntry : 'a * 'b -> unit) l =
	       let fun f (EMPTY,_) = ()
                     | f (PAIR(a,b,r),count) =
			    if count >= numEntries then
			       (print "\\\n\\"; prEntry(a,b); f(r,1))
			    else (prEntry(a,b); f(r,(count+1)))
               in f(l,0)
               end
         val printList : ('a -> unit) -> 'a list -> unit =
           fn prEntry => fn l =>
                let fun f (nil,_) = ()
                      | f (a :: r,count) =
			     if count >= numEntries then
				 (print "\\\n\\"; prEntry a; f(r,1))
				else (prEntry a; f(r,count+1))
                in f(l,0)
                end
	 val prEnd = fn _ => print "\\000\\000\\\n\\"
	 fun printPairRow prEntry =
	       let val printEntries = printPairList prEntry
	       in fn l => (printEntries l; prEnd())
	       end
	 fun printPairRowWithDefault (prEntry,prDefault) =
	       let val f = printPairRow prEntry
	       in fn (l,default) => (prDefault default; f l)
	       end
	 fun printTable (printRow,count) =
	       (print "\"\\\n\\";
		let fun f i = if i=count then ()
			       else (printRow i; f (i+1))
		in f 0
		end;
		print"\"\n")
	 val printChar = print o chr

	  (* print an integer between 0 and 2^16-1 as a 2-byte character,
	     with the low byte first *)

	 val printInt = fn i => (printChar (i mod 256);
				  printChar (i div 256))

	 (* encode actions as integers:

		ACCEPT => 0
		ERROR => 1
		SHIFT i => 2 + i
		REDUCE rulenum => numstates+2+rulenum
	 *)

	 val printAction =
	      fn (REDUCE rulenum) => printInt (rulenum+states+2)
		 | (SHIFT (STATE i)) => printInt (i+2)
		 | ACCEPT => printInt 0
		 | ERROR => printInt 1
	
	 val printTermAction = fn (T t,action) =>
		(printInt (t+1); printAction action)

	 val printGoto = fn (NT n,STATE s) => (printInt (n+1); printInt s)

	 val ((rowCount,rowNumbers,actionRows),entries)= 
	           shrinkActionList(table,verbose)
         val getActionRow = let val a = Array.fromList actionRows
	                    in fn i => a sub i
			    end
	 val printGotoRow : int -> unit = 
	       let val f = printPairRow printGoto
                   val g = describeGoto table
               in fn i => f (g (STATE i))
               end
        val printActionRow =
	      let val f = printPairRowWithDefault(printTermAction,printAction)
              in fn i => f (getActionRow i)
              end
	in print "val ";
	   print name;
	   print "=";
	   print "let val actionRows =\n";
	   printTable(printActionRow,rowCount);
	   print "val actionRowNumbers =\n\"";
	   printList (fn i => printInt i) rowNumbers;
	   print "\"\n";
	   print "val gotoT =\n"; 
	   printTable(printGotoRow,states);
	   print "val numstates = ";
	   print (Int.toString states);
	   print "\nval numrules = ";
	   print (Int.toString rules);
	   print "\n\
\val s = ref \"\" and index = ref 0\n\
\val string_to_int = fn () => \n\
\let val i = !index\n\
\in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256\n\
\end\n\
\val string_to_list = fn s' =>\n\
\    let val len = String.size s'\n\
\        fun f () =\n\
\           if !index < len then string_to_int() :: f()\n\
\           else nil\n\
\   in index := 0; s := s'; f ()\n\
\   end\n\
\val string_to_pairlist = fn (conv_key,conv_entry) =>\n\
\     let fun f () =\n\
\         case string_to_int()\n\
\         of 0 => EMPTY\n\
\          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())\n\
\     in f\n\
\     end\n\
\val string_to_pairlist_default = fn (conv_key,conv_entry) =>\n\
\    let val conv_row = string_to_pairlist(conv_key,conv_entry)\n\
\    in fn () =>\n\
\       let val default = conv_entry(string_to_int())\n\
\           val row = conv_row()\n\
\       in (row,default)\n\
\       end\n\
\   end\n\
\val string_to_table = fn (convert_row,s') =>\n\
\    let val len = String.size s'\n\
\        fun f ()=\n\
\           if !index < len then convert_row() :: f()\n\
\           else nil\n\
\     in (s := s'; index := 0; f ())\n\
\     end\n\
\local\n\
\  val memo = Array.array(numstates+numrules,ERROR)\n\
\  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))\n\
\       fun f i =\n\
\            if i=numstates then g i\n\
\            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))\n\
\          in f 0 handle Subscript => ()\n\
\          end\n\
\in\n\
\val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))\n\
\end\n\
\val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))\n\
\val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)\n\
\val actionRowNumbers = string_to_list actionRowNumbers\n\
\val actionT = let val actionRowLookUp=\n\
\let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end\n\
\in Array.fromList(map actionRowLookUp actionRowNumbers)\n\
\end\n\
\in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,\n\
\numStates=numstates,initialState=STATE ";
print (Int.toString ((fn (STATE i) => i) (initialState table)));
print "}\nend\n";
      entries
      end
end;
