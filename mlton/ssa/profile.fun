(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Profile (S: PROFILE_STRUCTS): PROFILE = 
struct

open S

fun addProfileFunction (f: Function.t) =
   let
      val {args, blocks, mayInline, name, raises, returns, start} =
         Function.dest f
      val extraBlocks = ref []
      val siF =
         SourceInfo.function
         {name = [Func.toString name],
          region = Region.bogus}
      val enterF = ProfileExp.Enter siF
      val enterF = fn () => Statement.profile enterF
      val leaveF = ProfileExp.Leave siF
      val leaveF = fn () => Statement.profile leaveF
      val blocks =
         Vector.map
         (blocks, fn Block.T {args, label, statements, transfer} =>
          let
             val (enterFL, enterL, leaveL, leaveLF) =
                if Vector.isEmpty statements
                   then (fn () => Vector.new1 (enterF ()),
                         fn () => Vector.new0 (),
                         fn () => Vector.new0 (),
                         fn () => Vector.new1 (leaveF ()))
                else let
                        val siL =
                           SourceInfo.function
                           {name = [Label.toString label],
                            region = Region.bogus}
                        val enterL = ProfileExp.Enter siL
                        val enterL = fn () => Statement.profile enterL
                        val leaveL = ProfileExp.Leave siL
                        val leaveL = fn () => Statement.profile leaveL
                     in
                        (fn () => Vector.new2 (enterF (), enterL ()),
                         fn () => Vector.new1 (enterL ()),
                         fn () => Vector.new1 (leaveL ()),
                         fn () => Vector.new2 (leaveL (), leaveF ()))
                     end
             val enterStmts =
                if Label.equals (label, start)
                   then enterFL ()
                else enterL ()
             fun doitLF () = (leaveLF (), transfer)
             fun doitL () = (leaveL (), transfer)
             fun doit () = (Vector.new0 (), transfer)
             fun genHandler () =
                case raises of
                   NONE => Handler.Caller
                 | SOME ts => 
                      let
                         val xs = Vector.map (ts, fn _ => Var.newNoname ())
                         val l = Label.newNoname ()
                         val _ =
                            List.push
                            (extraBlocks,
                             Block.T
                             {args = Vector.zip (xs, ts),
                              label = l,
                              statements = Vector.new1 (leaveF ()),
                              transfer = Transfer.Raise xs})
                      in
                         Handler.Handle l
                      end
             val (leaveStmts, transfer) =
                case transfer of
                   Transfer.Call {args, func, return} => 
                      (case return of
                          Return.Dead => doit ()
                        | Return.NonTail {cont, handler} => 
                             (case  handler of
                                 Handler.Dead => doitL ()
                               | Handler.Caller =>
                                    let
                                       val handler = genHandler ()
                                       val return = 
                                          Return.NonTail {cont = cont,
                                                          handler = handler}
                                    in
                                       (leaveL (),
                                        Transfer.Call {args = args,
                                                       func = func,
                                                       return = return})
                                    end
                               | Handler.Handle _ => doitL ())
                        | Return.Tail => doitLF ())
                 | Transfer.Raise _ => doitLF ()
                 | Transfer.Return _ => doitLF ()
                 | _ => doitL ()
             val statements =
                Vector.concat
                [enterStmts, statements, leaveStmts]
          in
             Block.T {args = args,
                      label = label,
                      statements = statements,
                      transfer = transfer}
          end)
      val blocks = Vector.concat [Vector.fromList (!extraBlocks), blocks]
   in
      Function.new {args = args,
                    blocks = blocks,
                    mayInline = mayInline,
                    name = name,
                    raises = raises,
                    returns = returns,
                    start = start}
   end

fun addProfile (Program.T {datatypes, functions, globals, main}) =
   Program.T {datatypes = datatypes,
              functions = List.revMap (functions, addProfileFunction),
              globals = globals,
              main = main}

fun dropProfileFunction f =
   let
      val {args, blocks, mayInline, name, raises, returns, start} =
         Function.dest f
      val blocks =
         Vector.map
         (blocks, fn Block.T {args, label, statements, transfer} =>
          Block.T {args = args,
                   label = label,
                   statements = Vector.keepAll
                                (statements, 
                                 fn Statement.T {exp = Exp.Profile _, ...} => false
                                  | _ => true),
                   transfer = transfer})
   in
      Function.new {args = args,
                    blocks = blocks,
                    mayInline = mayInline,
                    name = name,
                    raises = raises,
                    returns = returns,
                    start = start}
   end

fun dropProfile (Program.T {datatypes, globals, functions, main}) =
   (Control.profile := Control.ProfileNone
    ; Program.T {datatypes = datatypes,
                 globals = globals,
                 functions = List.revMap (functions, dropProfileFunction),
                 main = main})

end
