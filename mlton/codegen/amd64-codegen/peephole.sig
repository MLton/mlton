(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature PEEPHOLE_TYPES =
  sig
    type entry_type
    type profileLabel_type
    type statement_type
    type transfer_type
    datatype block = T of {entry: entry_type,
                           profileLabel: profileLabel_type,
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

    val One : (statement_type -> bool) -> statement_element 
    val All : (statement_type -> bool) -> statement_element

    type template = {start: statement_border,
                     statements: statement_element list,
                     finish: statement_border,
                     transfer: transfer_element}

    type match = {entry: entry_type,
                  profileLabel: profileLabel_type,
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
