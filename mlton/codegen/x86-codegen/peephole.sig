(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
type int = Int.t
type word = Word.t

signature PEEPHOLE_TYPES =
  sig
    type label_type
    type profileInfo_type
    type statement_type
    type transfer_type
    datatype block = T of {label: label_type,
			   profileInfo: profileInfo_type,
			   statements: statement_type list,
			   transfer: transfer_type}
  end

signature PEEPHOLE =
  sig
    include PEEPHOLE_TYPES

    datatype statement_border = Empty
                              | EmptyOrNonEmpty
    type statement_element = (int * int option) * (statement_type -> bool)
    type transfer_element = transfer_type -> bool

    val Zero : (statement_type -> bool) -> statement_element
    val One : (statement_type -> bool) -> statement_element 
    val ZeroOrOne : (statement_type -> bool) -> statement_element
    val All : (statement_type -> bool) -> statement_element
		      
    type template = {start: statement_border,
		     statements: statement_element list,
		     finish: statement_border,
		     transfer: transfer_element}
	
    type match = {label: label_type,
		  profileInfo: profileInfo_type,
		  start: statement_type list,
		  statements: statement_type list list,
		  finish: statement_type list,
		  transfer: transfer_type}
		    
    type rewriter = match -> block option

    type callback = bool -> unit
      
    type optimization = {template: template, 
			 rewriter: rewriter,
			 callback: callback}
      
    val peepholeBlock : {block: block,
			 optimizations: optimization list} ->
                        {block: block,
			 changed: bool}
    val peepholeBlocks : {blocks: block list,
			  optimizations: optimization list} ->
                         {blocks: block list,
			  changed: bool}
  end