(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)

type int = Int.t

signature ONCE_STRUCTS = 
   sig
      include SSA_TREE
   end

signature ONCE = 
   sig
      include ONCE_STRUCTS

      (* Returns true for variables that are guaranteed to be bound no
       * more than once during the entire program.
       *)
      val once: Program.t -> Var.t -> bool
   end

signature MULTI_STRUCTS = 
   sig
      include SSA_TREE
   end

signature MULTI = 
   sig
      include MULTI_STRUCTS

      val multi: Program.t -> 
	         {(* Program has an occurence of Thread_switchTo. *)
		  usesThreadsOrConts: bool,
		  (* usesThreadsOrConts == true
		   * and the func directly or indirectly invokes
		   * Thread_copyCurrent. 
		   *)
		  funcDoesThreadCopyCurrent: Func.t -> bool,
		  (* usesThreadsOrConts == true
		   * and the func may be called by two
		   * different threads during some run of the 
		   * program.
		   *)
		  funcIsMultiThreaded: Func.t -> bool,
		  (* The func may be called more than once
		   * during some run of the program.
		   *)
		  funcIsMultiUsed: Func.t -> bool,
		  (* usesThreadsOrConts == true
		   * and the label's block's transfer is
		   * either Runtime {prim, ...}
		   * with prim = Thread_copyCurrent
		   * or Call {func, ...}
		   * with funcDoesThreadCopyCurrent(func) == true.
		   *)
		  labelDoesThreadCopyCurrent: Label.t -> bool,
		  (* usesTheadsOrConts == true
		   * and the label may be executed by two
		   * different threads during some run of the
		   * program.
		   *)
		  labelIsMultiThreaded: Label.t -> bool,
		  (* The label may be executed more than once
		   * during some run of the program.
		   *)
		  labelIsMultiUsed: Label.t -> bool,
		  (* The var may be defined more than once
		   * during some run of the program;
		   * i.e., varIsMultiDefed(x) = 
		   * labelIsMultiUsed(label of x's def)
		   * when x is defined in a block;
		   *)
		  varIsMultiDefed: Var.t -> bool}
   end
