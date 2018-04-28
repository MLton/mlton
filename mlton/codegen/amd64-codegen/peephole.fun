(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Peephole(T : PEEPHOLE_TYPES): PEEPHOLE =
  struct 
    open T

    datatype statement_border = Empty
                              | EmptyOrNonEmpty
    type statement_element = (int * int option) * (statement_type -> bool)
    type transfer_element = transfer_type -> bool

    val One : (statement_type -> bool) -> statement_element 
      = fn p => ((1, SOME 1), p)
    val All : (statement_type -> bool) -> statement_element
      = fn p => ((0, NONE), p)

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

    datatype match_state
      = Start of {block: block}
      | Continue of {remaining: optimization list,
                     match: match}
      | Done of {block: block}

    type find_state = {remaining: optimization list,
                       state: {entry: entry_type,
                               profileLabel: profileLabel_type,
                               start: statement_type list,
                               finish: statement_type list,
                               transfer: transfer_type}}

    fun split (l, p) 
      = case l 
          of [] => ([],[])
           | l as h::t => if p h
                            then let
                                    val (tt,ff) = split (t, p)
                                 in
                                    (h::tt,ff)
                                 end
                            else ([],l)

    val rec matcher' : {template_statement: statement_element,
                        statement: statement_type list,
                        finish: statement_type list} ->
                       {statement: statement_type list,
                        finish: statement_type list} option
      = fn (* Zero *)
           {template_statement = ((0, SOME 0), _),
            statement,
            finish}
         => SOME {statement = List.rev statement,
                  finish = finish}
         | (* ZeroOrOne *)
           {template_statement = ((0, SOME 1), p),
            statement,
            finish}
         => (case finish
               of [] => SOME {statement = List.rev statement,
                              finish = finish}
                | (statement'::finish')
                => if p statement'
                     then SOME {statement = List.rev (statement'::statement),
                                finish = finish'}
                     else SOME {statement = List.rev statement,
                                finish = finish})
         | (* One *)
           {template_statement = ((1, SOME 1), p),
            statement,
            finish}
         => (case finish
               of [] => NONE
                | (statement'::finish')
                => if p statement'
                     then SOME {statement = List.rev (statement'::statement),
                                finish = finish'}
                     else NONE)
         | (* *)
           {template_statement = ((0, SOME i), p),
            statement,
            finish}
         => (case finish
               of [] => SOME {statement = List.rev statement,
                              finish = finish}
                | (statement'::finish')
                => if p statement'
                     then matcher' {template_statement = ((0, SOME (i-1)), p),
                                    statement = statement'::statement,
                                    finish = finish'}
                     else SOME {statement = List.rev statement,
                                finish = finish})
         | (* All *)
           {template_statement = ((0, NONE), p),
            statement,
            finish}
         => let
              val (statement',finish') = split (finish, p)
            in
              SOME {statement = List.fold(statement,
                                          statement',
                                          op ::),
                    finish = finish'}
            end
         | {template_statement = ((min, max), p),
            statement,
            finish = (statement'::finish')}
         => if p statement'
              then matcher' {template_statement 
                             = ((Int.max(min-1,0),
                                 Option.map(max,fn i => i - 1)), p),
                             statement = statement'::statement,
                             finish = finish'}
              else NONE
         | _ => NONE

    val rec matcher : {template_statements: statement_element list,
                       statements: statement_type list list,
                       finish: statement_type list} ->
                      {statements: statement_type list list,
                       finish: statement_type list} option
      = fn {template_statements = [],
            statements,
            finish}
         => SOME {statements = List.rev statements,
                  finish = finish}
         | {template_statements = (template_statement::template_statements),
            statements,
            finish}
         => (case matcher' {template_statement = template_statement,
                            statement = [],
                            finish = finish}
               of NONE => NONE
                | SOME {statement, finish} 
                => matcher {template_statements = template_statements,
                            statements = statement::statements,
                            finish = finish})

    fun peepholeBlock' {optimizations: optimization list,
                        match_state: match_state}
      = let
          fun next {remaining: optimization list,
                    state as {entry, profileLabel, start, finish, transfer}} : 
                   find_state option 
            = (case remaining
                 of [] => NONE
                  | _::nil 
                  => (case finish
                        of [] => NONE
                         | statement::finish
                         => SOME {remaining = optimizations,
                                  state = {entry = entry,
                                           profileLabel = profileLabel,
                                           start = statement::start,
                                           finish = finish,
                                           transfer = transfer}})
                  | _::remaining
                  => SOME {remaining = remaining,
                           state = state})

          fun findMatch' (find_state 
                          as {remaining as {template = {start 
                                                        = template_start,
                                                        statements 
                                                        = template_statements,
                                                        finish 
                                                        = template_finish,
                                                        transfer 
                                                        = template_transfer},
                                            ...}::_,
                              state = {entry,
                                       profileLabel,
                                       start, 
                                       finish, 
                                       transfer}}) : 
                         match_state
            = let
                fun loop ()
                  = (case next find_state
                       of SOME find_state => findMatch' find_state
                        | NONE 
                        => Done {block = T {entry = entry,
                                            profileLabel = profileLabel,
                                            statements = List.fold(start,
                                                                   finish,
                                                                   op ::),
                                            transfer = transfer}})
              in
                if not (template_transfer transfer)
                  then loop ()
                else if template_start = Empty
                        andalso
                        not (List.isEmpty start)
                     then loop ()
                else case matcher {template_statements = template_statements,
                                   statements = [],
                                   finish = finish}
                       of NONE => loop ()
                        | SOME {statements, finish}
                        => if template_finish = Empty
                              andalso
                              not (List.isEmpty finish)
                             then loop ()
                             else Continue {remaining = remaining,
                                            match 
                                            = {entry = entry,
                                               profileLabel = profileLabel,
                                               start = start,
                                               statements = statements,
                                               finish = finish,
                                               transfer = transfer}}
              end
            | findMatch' _ = Error.bug "Peephole.peepholeBlock'.findMatch'"

          fun findMatch (match_state: match_state) : match_state
            = case match_state
                of Start {block = T {entry, profileLabel, 
                                     statements, transfer}}
                 => let
                      val find_state
                        = {remaining = optimizations,
                           state = {entry = entry,
                                    profileLabel = profileLabel,
                                    start = [],
                                    finish = statements,
                                    transfer = transfer}}
                    in
                      findMatch' find_state
                    end
                 | Continue {remaining,
                             match = {entry, 
                                      profileLabel,
                                      start, 
                                      statements, 
                                      finish, 
                                      transfer},
                             ...}
                 => let
                      val finish = List.foldr(statements,
                                              finish,
                                              op @)
                      val find_state
                        = {remaining = remaining,
                           state = {entry = entry,
                                    profileLabel = profileLabel,
                                    start = start,
                                    finish = finish,
                                    transfer = transfer}}
                    in
                      case next find_state
                        of NONE => Done {block 
                                         = T {entry = entry,
                                              profileLabel = profileLabel,
                                              statements = List.fold(start,
                                                                     finish,
                                                                     op ::),
                                              transfer = transfer}}
                         | SOME find_state => findMatch' find_state
                    end
               | Done _ => match_state

          fun peepholeBlock'' {match_state: match_state,
                               changed: bool}
            = case findMatch match_state
                of match_state as Continue {remaining = {rewriter,
                                                         callback,
                                                         ...}::_,
                                            match}
                 => (case rewriter match
                       of SOME block 
                        => (callback true;
                            peepholeBlock'' {match_state 
                                             = Start {block = block},
                                             changed = true})
                        | NONE 
                        => (callback false;
                            peepholeBlock'' {match_state = match_state,
                                             changed = changed}))
                 | Done {block} => {block = block, changed = changed}
                 | _ => Error.bug "Peephole.peepholeBlock''"
        in
          case optimizations
            of [] => (case match_state
                        of Start {block = block} => {block = block,
                                                     changed = false}
                         | _ => Error.bug "Peephole.peepholeBlock'")
             | _ => peepholeBlock'' {match_state = match_state,
                                     changed = false}
        end

    fun peepholeBlock {block: block,
                       optimizations: optimization list}
      = peepholeBlock' {optimizations = optimizations,
                        match_state = Start {block = block}}

    fun peepholeBlocks {blocks: block list,
                        optimizations: optimization list}
      = let
          val {blocks, changed}
            = List.foldr
              (blocks,
               {blocks = [], changed = false},
               fn (block,{blocks,changed})
                => let
                     val {block = block',
                          changed = changed'}
                       = peepholeBlock' {optimizations = optimizations,
                                         match_state = Start {block = block}}
                   in
                     {blocks = block'::blocks,
                      changed = changed orelse changed'}
                   end)
        in
          {blocks = blocks,
           changed = changed}
        end
  end
