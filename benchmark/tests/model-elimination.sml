(* Benchmark from Joe Hurd <joe.hurd@cl.cam.ac.uk> on 2002-09-24.
 *
 * He writes:
 *
 * FYI: this benchmark attacks a bunch of non-trivial problems using the
 * model elimination first-order proof procedure. I've spent a fairly
 * long time optimizing this at a "high-level" (meaning data-structures
 * and algorithms optimizations, as well as exploiting domain knowledge,
 * but no tricks that speed things up for a particular ML
 * implementation).
 *)
exception Empty

(*#line 0.0 "$HOME/dev/sml/basic/src/PP.sig"*)
(* PP -- pretty-printing -- from the SML/NJ library *)

signature PP =
   sig
      type ppstream
      type ppconsumer = { consumer  : string -> unit,
                         linewidth : int,
                         flush     : unit -> unit }

      datatype break_style = 
         CONSISTENT
       | INCONSISTENT

      val mk_ppstream    : ppconsumer -> ppstream
      val dest_ppstream  : ppstream -> ppconsumer
      val add_break      : ppstream -> int * int -> unit
      val add_newline    : ppstream -> unit
      val add_string     : ppstream -> string -> unit
      val begin_block    : ppstream -> break_style -> int -> unit
      val end_block      : ppstream -> unit
      val clear_ppstream : ppstream -> unit
      val flush_ppstream : ppstream -> unit
      val with_pp        : ppconsumer -> (ppstream -> unit) -> unit
      val pp_to_string   : int -> (ppstream -> 'a -> unit) -> 'a -> string
   end

(* 
   This structure provides tools for creating customized Oppen-style
   pretty-printers, based on the type ppstream.  A ppstream is an
   output stream that contains prettyprinting commands.  The commands
   are placed in the stream by various function calls listed below.

   There following primitives add commands to the stream:
   begin_block, end_block, add_string, add_break, and add_newline.
   All calls to add_string, add_break, and add_newline must happen
   between a pair of calls to begin_block and end_block must be
   properly nested dynamically.  All calls to begin_block and
   end_block must be properly nested (dynamically).

   [ppconsumer] is the type of sinks for pretty-printing.  A value of 
   type ppconsumer is a record 
                 { consumer  : string -> unit,
                   linewidth : int,
                   flush     : unit -> unit }
   of a string consumer, a specified linewidth, and a flush function
   which is called whenever flush_ppstream is called.

   A prettyprinter can be called outright to print a value.  In
   addition, a prettyprinter for a base type or nullary datatype ty
   can be installed in the top-level system.  Then the installed
   prettyprinter will be invoked automatically whenever a value of
   type ty is to be printed.

   [break_style] is the type of line break styles for blocks:

   [CONSISTENT] specifies that if any line break occurs inside the
   block, then all indicated line breaks occur.

   [INCONSISTENT] specifies that breaks will be inserted to only to
   avoid overfull lines.

   [mk_ppstream {consumer, linewidth, flush}] creates a new ppstream
   which invokes the consumer to output text, putting at most
   linewidth characters on each line.

   [dest_ppstream ppstrm] extracts the linewidth, flush function, and
   consumer from a ppstream.

   [add_break ppstrm (size, offset)] notifies the pretty-printer that
   a line break is possible at this point.  
   * When the current block style is CONSISTENT:
      ** if the entire block fits on the remainder of the line, then
         output size spaces; else
      ** increase the current indentation by the block offset;
         further indent every item of the block by offset, and add
         one newline at every add_break in the block.
   * When the current block style is INCONSISTENT:
      ** if the next component of the block fits on the remainder of
         the line, then output size spaces; else
      ** issue a newline and indent to the current indentation level
         plus the block offset plus the offset.

   [add_newline ppstrm] issues a newline.

   [add_string ppstrm str] outputs the string str to the ppstream.

   [begin_block ppstrm style blockoffset] begins a new block and
   level of indentation, with the given style and block offset.

   [end_block ppstrm] closes the current block.  

   [clear_ppstream ppstrm] restarts the stream, without affecting the
   underlying consumer.

   [flush_ppstream ppstrm] executes any remaining commands in the
   ppstream (that is, flushes currently accumulated output to the
   consumer associated with ppstrm); executes the flush function
   associated with the consumer; and calls clear_ppstream.

   [with_pp consumer f] makes a new ppstream from the consumer and
   applies f (which can be thought of as a producer) to that
   ppstream, then flushed the ppstream and returns the value of f.

   [pp_to_string linewidth printit x] constructs a new ppstream
   ppstrm whose consumer accumulates the output in a string s.  Then
   evaluates (printit ppstrm x) and finally returns the string s.

   
   Example 1: A simple prettyprinter for Booleans:

       load "PP";
       fun ppbool pps d = 
           let open PP
           in
               begin_block pps INCONSISTENT 6; 
               add_string pps (if d then "right" else "wrong");
               end_block pps
           end;

   Now one may define a ppstream to print to, and exercise it:

       val ppstrm = PP.mk_ppstream {consumer  = 
                                    fn s => TextIO.output(TextIO.stdOut, s), 
                                    linewidth = 72,
                                    flush     = 
                                     fn () => TextIO.flushOut TextIO.stdOut};

       fun ppb b = (ppbool ppstrm b; PP.flush_ppstream ppstrm);

       - ppb false;
       wrong> val it = () : unit   

   The prettyprinter may also be installed in the toplevel system;
   then it will be used to print all expressions of type bool
   subsequently computed:

       - installPP ppbool;
       > val it = () : unit
       - 1=0;
       > val it = wrong : bool
       - 1=1;
       > val it = right : bool

   See library Meta for a description of installPP.


   Example 2: Prettyprinting simple expressions (examples/pretty/ppexpr.sml):

       datatype expr = 
           Cst of int 
         | Neg of expr
         | Plus of expr * expr

       fun ppexpr pps e0 = 
           let open PP
               fun ppe (Cst i)        = add_string pps (Int.toString i)
                 | ppe (Neg e)        = (add_string pps "~"; ppe e)
                 | ppe (Plus(e1, e2)) = (begin_block pps CONSISTENT 0;
                                         add_string pps "(";
                                         ppe e1; 
                                         add_string pps " + ";
                                         add_break pps (0, 1);
                                         ppe e2; 
                                         add_string pps ")";
                                         end_block pps)
           in
               begin_block pps INCONSISTENT 0; 
               ppe e0;
               end_block pps
           end

       val _ = installPP ppexpr;

       (* Some example values: *)

       val e1 = Cst 1;
       val e2 = Cst 2;
       val e3 = Plus(e1, Neg e2);
       val e4 = Plus(Neg e3, e3);
       val e5 = Plus(Neg e4, e4);
       val e6 = Plus(e5, e5);
       val e7 = Plus(e6, e6);
       val e8 = 
           Plus(e3, Plus(e3, Plus(e3, Plus(e3, Plus(e3, Plus(e3, e7))))));
*)
(*#line 0.0 "$HOME/dev/sml/basic/src/PP.sml"*)
(* PP -- Oppen-style prettyprinters.
 *
 * Modified for Milton ML from SML/NJ Library version 0.2
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories.
 * See file mosml/copyrght/copyrght.att for details.
 *)

(* the functions and data for actually doing printing. *)

structure PP :> PP =
struct

open Array 
infix 9 sub

(* the queue library, formerly in unit Ppqueue *)

datatype Qend = Qback | Qfront

exception QUEUE_FULL
exception QUEUE_EMPTY
exception REQUESTED_QUEUE_SIZE_TOO_SMALL

local 
    fun ++ i n = (i + 1) mod n
    fun -- i n = (i - 1) mod n
in

abstype 'a queue = QUEUE of {elems: 'a array, (* the contents *)
                             front: int ref,
                             back: int ref,
                             size: int}  (* fixed size of element array *)
with

  fun is_empty (QUEUE{front=ref ~1, back=ref ~1,...}) = true
    | is_empty _ = false

  fun mk_queue n init_val =
      if (n < 2)
      then raise REQUESTED_QUEUE_SIZE_TOO_SMALL
      else QUEUE{elems=array(n, init_val), front=ref ~1, back=ref ~1, size=n}

  fun clear_queue (QUEUE{front,back,...}) = (front := ~1; back := ~1)

  fun queue_at Qfront (QUEUE{elems,front,...}) = elems sub !front
    | queue_at Qback (QUEUE{elems,back,...}) = elems sub !back

  fun en_queue Qfront item (Q as QUEUE{elems,front,back,size}) =
        if (is_empty Q)
        then (front := 0; back := 0;
              update(elems,0,item))
        else let val i = --(!front) size
             in  if (i = !back)
                 then raise QUEUE_FULL
                 else (update(elems,i,item); front := i)
             end
    | en_queue Qback item (Q as QUEUE{elems,front,back,size}) =
        if (is_empty Q)
        then (front := 0; back := 0;
              update(elems,0,item))
        else let val i = ++(!back) size
             in  if (i = !front)
                 then raise QUEUE_FULL
                 else (update(elems,i,item); back := i)
             end

  fun de_queue Qfront (Q as QUEUE{front,back,size,...}) =
        if (!front = !back) (* unitary queue *)
        then clear_queue Q
        else front := ++(!front) size
    | de_queue Qback (Q as QUEUE{front,back,size,...}) =
        if (!front = !back)
        then clear_queue Q
        else back := --(!back) size

end (* abstype queue *)
end (* local   *)


val magic: 'a -> 'a = fn x => x

(* exception PP_FAIL of string *)

datatype break_style = CONSISTENT | INCONSISTENT

datatype break_info
  = FITS
  | PACK_ONTO_LINE of int
  | ONE_PER_LINE of int

(* Some global values *)
val INFINITY = 999999

abstype indent_stack = Istack of break_info list ref
with
  fun mk_indent_stack() = Istack (ref([]:break_info list))
  fun clear_indent_stack (Istack stk) = (stk := ([]:break_info list))
  fun top (Istack stk) =
      case !stk
        of nil => raise Fail "PP-error: top: badly formed block"
         | x::_ => x
  fun push (x,(Istack stk)) = stk := x::(!stk)
  fun pop (Istack stk) =
      case !stk
        of nil => raise Fail "PP-error: pop: badly formed block"
         | _::rest => stk := rest
end

(* The delim_stack is used to compute the size of blocks. It is
   a stack of indices into the token buffer. The indices only point to
   BBs, Es, and BRs. We push BBs and Es onto the stack until a BR
   is encountered. Then we compute sizes and pop. When we encounter
   a BR in the middle of a block, we compute the Distance_to_next_break
   of the previous BR in the block, if there was one.

   We need to be able to delete from the bottom of the delim_stack, so
   we use a queue, treated with a stack discipline, i.e., we only add
   items at the head of the queue, but can delete from the front or
   back of the queue.
*)
abstype delim_stack = Dstack of int queue
with
  fun new_delim_stack i = Dstack(mk_queue i ~1)
  fun reset_delim_stack (Dstack q) = clear_queue q

  fun pop_delim_stack (Dstack d) = de_queue Qfront d
  fun pop_bottom_delim_stack (Dstack d) = de_queue Qback d

  fun push_delim_stack(i,Dstack d) = en_queue Qfront i d
  fun top_delim_stack (Dstack d) = queue_at Qfront d
  fun bottom_delim_stack (Dstack d) = queue_at Qback d
  fun delim_stack_is_empty (Dstack d) = is_empty d
end


type block_info = { Block_size : int ref,
                    Block_offset : int,
                    How_to_indent : break_style }


(* Distance_to_next_break includes Number_of_blanks. Break_offset is
   a local offset for the break. BB represents a sequence of contiguous
   Begins. E represents a sequence of contiguous Ends.
*)
datatype pp_token
  = S of  {String : string, Length : int}
  | BB of {Pblocks : block_info list ref,   (* Processed   *)
           Ublocks : block_info list ref}  (* Unprocessed *)
  | E of  {Pend : int ref, Uend : int ref}
  | BR of {Distance_to_next_break : int ref,
           Number_of_blanks : int,
           Break_offset : int}


(* The initial values in the token buffer *)
val initial_token_value = S{String = "", Length = 0}

(* type ppstream = General.ppstream; *)
datatype ppstream_ =
  PPS of
     {consumer : string -> unit,
      linewidth : int,
      flush : unit -> unit,
      the_token_buffer : pp_token array,
      the_delim_stack : delim_stack,
      the_indent_stack : indent_stack,
      ++ : int ref -> unit,    (* increment circular buffer index *)
      space_left : int ref,    (* remaining columns on page *)
      left_index : int ref,    (* insertion index *)
      right_index : int ref,   (* output index *)
      left_sum : int ref,      (* size of strings and spaces inserted *)
      right_sum : int ref}     (* size of strings and spaces printed *)

type ppstream = ppstream_

type ppconsumer = {consumer : string -> unit,
                   linewidth : int,
                   flush : unit -> unit}

fun mk_ppstream {consumer,linewidth,flush} =
    if (linewidth<5)
    then raise Fail "PP-error: linewidth too_small"
    else let val buf_size = 3*linewidth
          in magic(
             PPS{consumer = consumer,
                 linewidth = linewidth,
                 flush = flush,
                 the_token_buffer = array(buf_size, initial_token_value),
                 the_delim_stack = new_delim_stack buf_size,
                 the_indent_stack = mk_indent_stack (),
                 ++ = fn i => i := ((!i + 1) mod buf_size),
                 space_left = ref linewidth,
                 left_index = ref 0, right_index = ref 0,
                 left_sum = ref 0, right_sum = ref 0}
                 ) : ppstream
         end

fun dest_ppstream(pps : ppstream) =
  let val PPS{consumer,linewidth,flush, ...} = magic pps
  in {consumer=consumer,linewidth=linewidth,flush=flush} end

local
  val space = " "
  fun mk_space (0,s) = String.concat s
    | mk_space (n,s) = mk_space((n-1), (space::s))
  val space_table = Vector.tabulate(100, fn i => mk_space(i,[]))
  fun nspaces n = Vector.sub(space_table, n)
      handle General.Subscript =>
        if n < 0
        then ""
        else let val n2 = n div 2
                 val n2_spaces = nspaces n2
                 val extra = if (n = (2*n2)) then "" else space
              in String.concat [n2_spaces, n2_spaces, extra]
             end
in
  fun cr_indent (ofn, i) = ofn ("\n"^(nspaces i))
  fun indent (ofn,i) = ofn (nspaces i)
end


(* Print a the first member of a contiguous sequence of Begins. If there
   are "processed" Begins, then take the first off the list. If there are
   no processed Begins, take the last member off the "unprocessed" list.
   This works because the unprocessed list is treated as a stack, the
   processed list as a FIFO queue. How can an item on the unprocessed list
   be printable? Because of what goes on in add_string. See there for details.
*)

fun print_BB (_,{Pblocks = ref [], Ublocks = ref []}) =
             raise Fail "PP-error: print_BB"
  | print_BB (PPS{the_indent_stack,linewidth,space_left=ref sp_left,...},
             {Pblocks as ref({How_to_indent=CONSISTENT,Block_size,
                              Block_offset}::rst),
              Ublocks=ref[]}) =
       (push ((if (!Block_size > sp_left)
               then ONE_PER_LINE (linewidth - (sp_left - Block_offset))
               else FITS),
              the_indent_stack);
        Pblocks := rst)
  | print_BB(PPS{the_indent_stack,linewidth,space_left=ref sp_left,...},
             {Pblocks as ref({Block_size,Block_offset,...}::rst),Ublocks=ref[]}) =
       (push ((if (!Block_size > sp_left)
               then PACK_ONTO_LINE (linewidth - (sp_left - Block_offset))
               else FITS),
              the_indent_stack);
        Pblocks := rst)
  | print_BB (PPS{the_indent_stack, linewidth, space_left=ref sp_left,...},
              {Ublocks,...}) =
      let fun pr_end_Ublock [{How_to_indent=CONSISTENT,Block_size,Block_offset}] l =
                (push ((if (!Block_size > sp_left)
                        then ONE_PER_LINE (linewidth - (sp_left - Block_offset))
                        else FITS),
                       the_indent_stack);
                 List.rev l)
            | pr_end_Ublock [{Block_size,Block_offset,...}] l =
                (push ((if (!Block_size > sp_left)
                        then PACK_ONTO_LINE (linewidth - (sp_left - Block_offset))
                        else FITS),
                       the_indent_stack);
                 List.rev l)
            | pr_end_Ublock (a::rst) l = pr_end_Ublock rst (a::l)
            | pr_end_Ublock _ _ =
                raise Fail "PP-error: print_BB: internal error"
       in Ublocks := pr_end_Ublock(!Ublocks) []
      end


(* Uend should always be 0 when print_E is called. *)
fun print_E (_,{Pend = ref 0, Uend = ref 0}) =
      raise Fail "PP-error: print_E"
  | print_E (istack,{Pend, ...}) =
      let fun pop_n_times 0 = ()
            | pop_n_times n = (pop istack; pop_n_times(n-1))
       in pop_n_times(!Pend); Pend := 0
      end


(* "cursor" is how many spaces across the page we are. *)

fun print_token(PPS{consumer,space_left,...}, S{String,Length}) =
      (consumer String;
       space_left := (!space_left) - Length)
  | print_token(ppstrm,BB b) = print_BB(ppstrm,b)
  | print_token(PPS{the_indent_stack,...},E e) =
      print_E (the_indent_stack,e)
  | print_token (PPS{the_indent_stack,space_left,consumer,linewidth,...},
                 BR{Distance_to_next_break,Number_of_blanks,Break_offset}) =
     (case (top the_indent_stack)
        of FITS =>
             (space_left := (!space_left) - Number_of_blanks;
              indent (consumer,Number_of_blanks))
         | (ONE_PER_LINE cursor) =>
             let val new_cursor = cursor + Break_offset
              in space_left := linewidth - new_cursor;
                 cr_indent (consumer,new_cursor)
             end
         | (PACK_ONTO_LINE cursor) =>
             if (!Distance_to_next_break > (!space_left))
             then let val new_cursor = cursor + Break_offset
                   in space_left := linewidth - new_cursor;
                      cr_indent(consumer,new_cursor)
                  end
             else (space_left := !space_left - Number_of_blanks;
                   indent (consumer,Number_of_blanks)))


fun clear_ppstream(pps : ppstream) =
    let val PPS{the_token_buffer, the_delim_stack,
                the_indent_stack,left_sum, right_sum,
                left_index, right_index,space_left,linewidth,...}
              = magic pps
        val buf_size = 3*linewidth
        fun set i =
            if (i = buf_size)
            then ()
            else (update(the_token_buffer,i,initial_token_value);
                  set (i+1))
     in set 0;
        clear_indent_stack the_indent_stack;
        reset_delim_stack the_delim_stack;
        left_sum := 0; right_sum := 0;
        left_index := 0; right_index := 0;
        space_left := linewidth
    end


(* Move insertion head to right unless adding a BB and already at a BB,
   or unless adding an E and already at an E.
*)
fun BB_inc_right_index(PPS{the_token_buffer, right_index, ++,...})=
    case (the_token_buffer sub (!right_index))
      of (BB _) => ()
       | _ => ++right_index

fun E_inc_right_index(PPS{the_token_buffer,right_index, ++,...})=
    case (the_token_buffer sub (!right_index))
      of (E _) => ()
       | _ => ++right_index


fun pointers_coincide(PPS{left_index,right_index,the_token_buffer,...}) =
    (!left_index = !right_index) andalso
    (case (the_token_buffer sub (!left_index))
       of (BB {Pblocks = ref [], Ublocks = ref []}) => true
        | (BB _) => false
        | (E {Pend = ref 0, Uend = ref 0}) => true
        | (E _) => false
        | _ => true)

fun advance_left (ppstrm as PPS{consumer,left_index,left_sum,
                                the_token_buffer,++,...},
                  instr) =
    let val NEG = ~1
        val POS = 0
        fun inc_left_sum (BR{Number_of_blanks, ...}) =
                 left_sum := (!left_sum) + Number_of_blanks
          | inc_left_sum (S{Length, ...}) = left_sum := (!left_sum) + Length
          | inc_left_sum _ = ()

        fun last_size [{Block_size, ...}:block_info] = !Block_size
          | last_size (_::rst) = last_size rst
          | last_size _ = raise Fail "PP-error: last_size: internal error"
        fun token_size (S{Length, ...}) = Length
          | token_size (BB b) =
             (case b
                of {Pblocks = ref [], Ublocks = ref []} =>
                     raise Fail "PP-error: BB_size"
                 | {Pblocks as ref(_::_),Ublocks=ref[]} => POS
                 | {Ublocks, ...} => last_size (!Ublocks))
          | token_size (E{Pend = ref 0, Uend = ref 0}) =
              raise Fail "PP-error: token_size.E"
          | token_size (E{Pend = ref 0, ...}) = NEG
          | token_size (E _) = POS
          | token_size (BR {Distance_to_next_break, ...}) = !Distance_to_next_break
        fun loop (instr) =
            if (token_size instr < 0)  (* synchronization point; cannot advance *)
            then ()
            else (print_token(ppstrm,instr);
                  inc_left_sum instr;
                  if (pointers_coincide ppstrm)
                  then ()
                  else (* increment left index *)

    (* When this is evaluated, we know that the left_index has not yet
       caught up to the right_index. If we are at a BB or an E, we can
       increment left_index if there is no work to be done, i.e., all Begins
       or Ends have been dealt with. Also, we should do some housekeeping and
       clear the buffer at left_index, otherwise we can get errors when
       left_index catches up to right_index and we reset the indices to 0.
       (We might find ourselves adding a BB to an "old" BB, with the result
       that the index is not pushed onto the delim_stack. This can lead to
       mangled output.)
    *)
                       (case (the_token_buffer sub (!left_index))
                          of (BB {Pblocks = ref [], Ublocks = ref []}) =>
                               (update(the_token_buffer,!left_index,
                                       initial_token_value);
                                ++left_index)
                           | (BB _) => ()
                           | (E {Pend = ref 0, Uend = ref 0}) =>
                               (update(the_token_buffer,!left_index,
                                       initial_token_value);
                                ++left_index)
                           | (E _) => ()
                           | _ => ++left_index;
                        loop (the_token_buffer sub (!left_index))))
     in loop instr
    end


fun begin_block (pps : ppstream) style offset =
  let val ppstrm = magic pps : ppstream_
      val PPS{the_token_buffer, the_delim_stack,left_index,
              left_sum, right_index, right_sum,...}
            = ppstrm
  in
   (if (delim_stack_is_empty the_delim_stack)
    then (left_index := 0;
          left_sum := 1;
          right_index := 0;
          right_sum := 1)
    else BB_inc_right_index ppstrm;
    case (the_token_buffer sub (!right_index))
      of (BB {Ublocks, ...}) =>
           Ublocks := {Block_size = ref (~(!right_sum)),
                       Block_offset = offset,
                       How_to_indent = style}::(!Ublocks)
       | _ => (update(the_token_buffer, !right_index,
                      BB{Pblocks = ref [],
                         Ublocks = ref [{Block_size = ref (~(!right_sum)),
                                         Block_offset = offset,
                                         How_to_indent = style}]});
               push_delim_stack (!right_index, the_delim_stack)))
  end

fun end_block(pps : ppstream) =
  let val ppstrm = magic pps : ppstream_
      val PPS{the_token_buffer,the_delim_stack,right_index,...}
            = ppstrm
  in
    if (delim_stack_is_empty the_delim_stack)
    then print_token(ppstrm,(E{Pend = ref 1, Uend = ref 0}))
    else (E_inc_right_index ppstrm;
          case (the_token_buffer sub (!right_index))
            of (E{Uend, ...}) => Uend := !Uend + 1
             | _ => (update(the_token_buffer,!right_index,
                            E{Uend = ref 1, Pend = ref 0});
                     push_delim_stack (!right_index, the_delim_stack)))
  end

local
  fun check_delim_stack(PPS{the_token_buffer,the_delim_stack,right_sum,...}) =
      let fun check k =
              if (delim_stack_is_empty the_delim_stack)
              then ()
              else case(the_token_buffer sub (top_delim_stack the_delim_stack))
                     of (BB{Ublocks as ref ((b as {Block_size, ...})::rst),
                            Pblocks}) =>
                           if (k>0)
                           then (Block_size := !right_sum + !Block_size;
                                 Pblocks := b :: (!Pblocks);
                                 Ublocks := rst;
                                 if (List.length rst = 0)
                                 then pop_delim_stack the_delim_stack
                                 else ();
                                 check(k-1))
                           else ()
                      | (E{Pend,Uend}) =>
                           (Pend := (!Pend) + (!Uend);
                            Uend := 0;
                            pop_delim_stack the_delim_stack;
                            check(k + !Pend))
                      | (BR{Distance_to_next_break, ...}) =>
                           (Distance_to_next_break :=
                              !right_sum + !Distance_to_next_break;
                            pop_delim_stack the_delim_stack;
                            if (k>0)
                            then check k
                            else ())
                      | _ => raise Fail "PP-error: check_delim_stack.catchall"
       in check 0
      end
in

  fun add_break (pps : ppstream) (n, break_offset) =
    let val ppstrm = magic pps : ppstream_
        val PPS{the_token_buffer,the_delim_stack,left_index,
                right_index,left_sum,right_sum, ++, ...}
              = ppstrm
    in
      (if (delim_stack_is_empty the_delim_stack)
       then (left_index := 0; right_index := 0;
             left_sum := 1;   right_sum := 1)
       else ++right_index;
       update(the_token_buffer, !right_index,
              BR{Distance_to_next_break = ref (~(!right_sum)),
                 Number_of_blanks = n,
                 Break_offset = break_offset});
       check_delim_stack ppstrm;
       right_sum := (!right_sum) + n;
       push_delim_stack (!right_index,the_delim_stack))
    end

  fun flush_ppstream0(pps : ppstream) =
    let val ppstrm = magic pps : ppstream_
        val PPS{the_delim_stack,the_token_buffer, flush, left_index,...}
              = ppstrm
    in
      (if (delim_stack_is_empty the_delim_stack)
       then ()
       else (check_delim_stack ppstrm;
             advance_left(ppstrm, the_token_buffer sub (!left_index)));
       flush())
    end

end (* local *)


fun flush_ppstream ppstrm =
    (flush_ppstream0 ppstrm;
     clear_ppstream ppstrm)

fun add_string (pps : ppstream) s =
    let val ppstrm = magic pps : ppstream_
        val PPS{the_token_buffer,the_delim_stack,consumer,
                right_index,right_sum,left_sum,
                left_index,space_left,++,...}
              = ppstrm
        fun fnl [{Block_size, ...}:block_info] = Block_size := INFINITY
          | fnl (_::rst) = fnl rst
          | fnl _ = raise Fail "PP-error: fnl: internal error"

        fun set(dstack,BB{Ublocks as ref[{Block_size,...}:block_info],...}) =
              (pop_bottom_delim_stack dstack;
               Block_size := INFINITY)
          | set (_,BB {Ublocks = ref(_::rst), ...}) = fnl rst
          | set (dstack, E{Pend,Uend}) =
              (Pend := (!Pend) + (!Uend);
               Uend := 0;
               pop_bottom_delim_stack dstack)
          | set (dstack,BR{Distance_to_next_break,...}) =
              (pop_bottom_delim_stack dstack;
               Distance_to_next_break := INFINITY)
          | set _ = raise (Fail "PP-error: add_string.set")

        fun check_stream () =
            if ((!right_sum - !left_sum) > !space_left)
            then if (delim_stack_is_empty the_delim_stack)
                 then ()
                 else let val i = bottom_delim_stack the_delim_stack
                       in if (!left_index = i)
                          then set (the_delim_stack, the_token_buffer sub i)
                          else ();
                          advance_left(ppstrm,
                                       the_token_buffer sub (!left_index));
                          if (pointers_coincide ppstrm)
                          then ()
                          else check_stream ()
                      end
            else ()

        val slen = String.size s
        val S_token = S{String = s, Length = slen}

    in if (delim_stack_is_empty the_delim_stack)
       then print_token(ppstrm,S_token)
       else (++right_index;
             update(the_token_buffer, !right_index, S_token);
             right_sum := (!right_sum)+slen;
             check_stream ())
   end


(* Derived form. The +2 is for peace of mind *)
fun add_newline (pps : ppstream) =
  let val PPS{linewidth, ...} = magic pps
  in add_break pps (linewidth+2,0) end

(* Derived form. Builds a ppstream, sends pretty printing commands called in
   f to the ppstream, then flushes ppstream.
*)

fun with_pp ppconsumer ppfn =
   let val ppstrm = mk_ppstream ppconsumer
    in ppfn ppstrm;
       flush_ppstream0 ppstrm
   end
   handle Fail msg =>
     (TextIO.print (">>>> Pretty-printer failure: " ^ msg ^ "\n"))

fun pp_to_string linewidth ppfn ob =
    let val l = ref ([]:string list)
        fun attach s = l := (s::(!l))
     in with_pp {consumer = attach, linewidth=linewidth, flush = fn()=>()}
                (fn ppstrm =>  ppfn ppstrm ob);
        String.concat(List.rev(!l))
    end
end
(*#line 0.0 "$HOME/dev/sml/basic/src/Binarymap.sig"*)
(* Binarymap -- applicative maps as balanced ordered binary trees *)
(* From SML/NJ lib 0.2, copyright 1993 by AT&T Bell Laboratories  *)
(* Original implementation due to Stephen Adams, Southampton, UK  *)

signature Binarymap =
sig

type ('key, 'a) dict

exception NotFound

val mkDict    : ('key * 'key -> order) -> ('key, 'a) dict
val insert    : ('key, 'a) dict * 'key * 'a -> ('key, 'a) dict
val find      : ('key, 'a) dict * 'key -> 'a
val peek      : ('key, 'a) dict * 'key -> 'a option
val remove    : ('key, 'a) dict * 'key -> ('key, 'a) dict * 'a
val numItems  : ('key, 'a) dict -> int
val listItems : ('key, 'a) dict -> ('key * 'a) list
val app       : ('key * 'a -> unit) -> ('key,'a) dict -> unit
val revapp    : ('key * 'a -> unit) -> ('key,'a) dict -> unit
val foldr     : ('key * 'a * 'b -> 'b)-> 'b -> ('key,'a) dict -> 'b
val foldl     : ('key * 'a * 'b -> 'b) -> 'b -> ('key,'a) dict -> 'b
val map       : ('key * 'a -> 'b) -> ('key,'a) dict -> ('key, 'b) dict
val transform : ('a -> 'b) -> ('key,'a) dict -> ('key, 'b) dict

end

(* 
   [('key, 'a) dict] is the type of applicative maps from domain type
   'key to range type 'a, or equivalently, applicative dictionaries
   with keys of type 'key and values of type 'a.  They are implemented
   as ordered balanced binary trees.

   [mkDict ordr] returns a new, empty map whose keys have ordering
   ordr.

   [insert(m, i, v)] extends (or modifies) map m to map i to v.

   [find (m, k)] returns v if m maps k to v; otherwise raises NotFound.
   
   [peek(m, k)] returns SOME v if m maps k to v; otherwise returns NONE.

   [remove(m, k)] removes k from the domain of m and returns the
   modified map and the element v corresponding to k.  Raises NotFound
   if k is not in the domain of m.

   [numItems m] returns the number of entries in m (that is, the size
   of the domain of m).

   [listItems m] returns a list of the entries (k, v) of keys k and
   the corresponding values v in m, in order of increasing key values.

   [app f m] applies function f to the entries (k, v) in m, in
   increasing order of k (according to the ordering ordr used to
   create the map or dictionary).

   [revapp f m] applies function f to the entries (k, v) in m, in
   decreasing order of k.

   [foldl f e m] applies the folding function f to the entries (k, v)
   in m, in increasing order of k.

   [foldr f e m] applies the folding function f to the entries (k, v)
   in m, in decreasing order of k.

   [map f m] returns a new map whose entries have form (k, f(k,v)),
   where (k, v) is an entry in m.

   [transform f m] returns a new map whose entries have form (k, f v),
   where (k, v) is an entry in m.
*)
(*#line 0.0 "$HOME/dev/sml/basic/src/Binarymap.sml"*)
(* Binarymap -- modified for Milton ML 
 * from SML/NJ library v. 0.2 file binary-dict.sml.
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  
 * See file mosml/copyrght/copyrght.att for details.
 *
 * This code was adapted from Stephen Adams' binary tree implementation
 * of applicative integer sets.
 *
 *   Copyright 1992 Stephen Adams.
 *
 *    This software may be used freely provided that:
 *      1. This copyright notice is attached to any copy, derived work,
 *         or work including all or part of this software.
 *      2. Any derived work must contain a prominent notice stating that
 *         it has been altered from the original.
 *
 *
 *   Name(s): Stephen Adams.
 *   Department, Institution: Electronics & Computer Science,
 *      University of Southampton
 *   Address:  Electronics & Computer Science
 *             University of Southampton
 *           Southampton  SO9 5NH
 *           Great Britian
 *   E-mail:   sra@ecs.soton.ac.uk
 *
 *   Comments:
 *
 *     1.  The implementation is based on Binary search trees of Bounded
 *         Balance, similar to Nievergelt & Reingold, SIAM J. Computing
 *         2(1), March 1973.  The main advantage of these trees is that
 *         they keep the size of the tree in the node, giving a constant
 *         time size operation.
 *
 *     2.  The bounded balance criterion is simpler than N&R's alpha.
 *         Simply, one subtree must not have more than `weight' times as
 *         many elements as the opposite subtree.  Rebalancing is
 *         guaranteed to reinstate the criterion for weight>2.23, but
 *         the occasional incorrect behaviour for weight=2 is not
 *         detrimental to performance.
 *
 *)

structure Binarymap :> Binarymap =
struct
 
exception NotFound

fun wt (i : int) = 3 * i

datatype ('key, 'a) dict = 
    DICT of ('key * 'key -> order) * ('key, 'a) tree
and ('key, 'a) tree =
    E 
  | T of {key   : 'key, 
          value : 'a, 
          cnt   : int, 
          left  : ('key, 'a) tree, 
          right : ('key, 'a) tree}

fun treeSize E            = 0
  | treeSize (T{cnt,...}) = cnt

fun numItems (DICT(_, t)) = treeSize t

local
    fun N(k,v,E,E) = T{key=k,value=v,cnt=1,left=E,right=E}
      | N(k,v,E,r as T n) = T{key=k,value=v,cnt=1+(#cnt n),left=E,right=r}
      | N(k,v,l as T n,E) = T{key=k,value=v,cnt=1+(#cnt n),left=l,right=E}
      | N(k,v,l as T n,r as T n') = 
          T{key=k,value=v,cnt=1+(#cnt n)+(#cnt n'),left=l,right=r}

    fun single_L (a,av,x,T{key=b,value=bv,left=y,right=z,...}) = 
          N(b,bv,N(a,av,x,y),z)
      | single_L _ = raise Match
    fun single_R (b,bv,T{key=a,value=av,left=x,right=y,...},z) = 
          N(a,av,x,N(b,bv,y,z))
      | single_R _ = raise Match
    fun double_L (a,av,w,T{key=c,value=cv, 
                           left=T{key=b,value=bv,left=x,right=y,...},
                           right=z,...}) =
          N(b,bv,N(a,av,w,x),N(c,cv,y,z))
      | double_L _ = raise Match
    fun double_R (c,cv,T{key=a,value=av,left=w,
                         right=T{key=b,value=bv,left=x,right=y,...},...},z) = 
          N(b,bv,N(a,av,w,x),N(c,cv,y,z))
      | double_R _ = raise Match

    fun T' (k,v,E,E) = T{key=k,value=v,cnt=1,left=E,right=E}
      | T' (k,v,E,r as T{right=E,left=E,...}) =
          T{key=k,value=v,cnt=2,left=E,right=r}
      | T' (k,v,l as T{right=E,left=E,...},E) =
          T{key=k,value=v,cnt=2,left=l,right=E}

      | T' (p as (_,_,E,T{left=T _,right=E,...})) = double_L p
      | T' (p as (_,_,T{left=E,right=T _,...},E)) = double_R p

        (* these cases almost never happen with small weight*)
      | T' (p as (_,_,E,T{left=T{cnt=ln,...},right=T{cnt=rn,...},...})) =
          if ln < rn then single_L p else double_L p
      | T' (p as (_,_,T{left=T{cnt=ln,...},right=T{cnt=rn,...},...},E)) =
          if ln > rn then single_R p else double_R p

      | T' (p as (_,_,E,T{left=E,...})) = single_L p
      | T' (p as (_,_,T{right=E,...},E)) = single_R p

      | T' (p as (k,v,l as T{cnt=ln,left=ll,right=lr,...},
                      r as T{cnt=rn,left=rl,right=rr,...})) =
          if rn >= wt ln then (*right is too big*)
            let val rln = treeSize rl
                val rrn = treeSize rr
            in
              if rln < rrn then  single_L p  else  double_L p
            end
        
          else if ln >= wt rn then  (*left is too big*)
            let val lln = treeSize ll
                val lrn = treeSize lr
            in
              if lrn < lln then  single_R p  else  double_R p
            end
    
          else T{key=k,value=v,cnt=ln+rn+1,left=l,right=r}

    local
      fun min (T{left=E,key,value,...}) = (key,value)
        | min (T{left,...}) = min left
        | min _ = raise Match
  
      fun delmin (T{left=E,right,...}) = right
        | delmin (T{key,value,left,right,...}) = 
          T'(key,value,delmin left,right)
        | delmin _ = raise Match
    in
      fun delete' (E,r) = r
        | delete' (l,E) = l
        | delete' (l,r) = let val (mink,minv) = min r 
                          in T'(mink,minv,l,delmin r) end
    end
in
    fun mkDict cmpKey = DICT(cmpKey, E)
    
    fun insert (DICT (cmpKey, t),x,v) = 
        let fun ins E = T{key=x,value=v,cnt=1,left=E,right=E}
              | ins (T(set as {key,left,right,value,...})) =
                case cmpKey (key,x) of
                    GREATER => T'(key,value,ins left,right)
                  | LESS    => T'(key,value,left,ins right)
                  | _       => 
                        T{key=x,value=v,left=left,right=right,cnt= #cnt set}
        in DICT(cmpKey, ins t) end

    fun find (DICT(cmpKey, t), x) =
        let fun mem E = raise NotFound
              | mem (T(n as {key,left,right,...})) =
                case cmpKey (x,key) of
                    GREATER => mem right
                  | LESS    => mem left
                  | _       => #value n
        in mem t end

    fun peek arg = (SOME(find arg)) handle NotFound => NONE

    fun remove (DICT(cmpKey, t), x) = 
        let fun rm E = raise NotFound
              | rm (set as T{key,left,right,value,...}) = 
                (case cmpKey (key,x) of
                     GREATER => let val (left', v) = rm left
                                in (T'(key, value, left', right), v) end
                   | LESS    => let val (right', v) = rm right
                                in (T'(key, value, left, right'), v) end
                   | _       => (delete'(left,right),value))
            val (newtree, valrm) = rm t
        in (DICT(cmpKey, newtree), valrm) end

    fun listItems (DICT(_, d)) = 
        let fun d2l E res = res
              | d2l (T{key,value,left,right,...}) res =
                d2l left ((key,value) :: d2l right res)
        in d2l d [] end

    fun revapp f (DICT(_, d)) = let
      fun a E = ()
        | a (T{key,value,left,right,...}) = (a right; f(key,value); a left)
      in a d end

    fun app f (DICT(_, d)) = let
      fun a E = ()
        | a (T{key,value,left,right,...}) = (a left; f(key,value); a right)
      in a d end

    fun foldr f init (DICT(_, d)) = let
      fun a E v = v
        | a (T{key,value,left,right,...}) v = a left (f(key,value,a right v))
      in a d init end

    fun foldl f init (DICT(_, d)) = let
      fun a E v = v
        | a (T{key,value,left,right,...}) v = a right (f(key,value,a left v))
      in a d init end

    fun map f (DICT(cmpKey, d)) = let
      fun a E = E
        | a (T{key,value,left,right,cnt}) = let
            val left' = a left
            val value' = f(key,value)
            in
              T{cnt=cnt, key=key,value=value',left = left', right = a right}
            end
      in DICT(cmpKey, a d) end

    fun transform f (DICT(cmpKey, d)) = 
        let fun a E = E
              | a (T{key,value,left,right,cnt}) = 
                let val left' = a left
                in
                    T{cnt=cnt, key=key, value=f value, left = left', 
                      right = a right}
                end
      in DICT(cmpKey, a d) end
end

end
(*#line 0.0 "$HOME/dev/sml/basic/src/Susp.sig"*)
(* Susp -- support for lazy evaluation *)

signature Susp =
sig

type 'a susp

val delay : (unit -> 'a) -> 'a susp
val force : 'a susp -> 'a

end

(* 
   ['a susp] is the type of lazily evaluated expressions with result
   type 'a.

   [delay (fn () => e)] creates a suspension for the expression e.
   The first time the suspension is forced, the expression e will be
   evaluated, and the result stored in the suspension.  All subsequent
   forcing of the suspension will just return this result, so e is
   evaluated at most once.  If the suspension is never forced, then e
   is never evaluated.

   [force su] forces the suspension su and returns the result of the
   expression e stored in the suspension.
*)
(*#line 0.0 "$HOME/dev/sml/basic/src/Susp.sml"*)
(* Susp -- support for lazy evaluation 1995-05-22 *)

structure Susp :> Susp =
struct

datatype 'a thunk = VAL of 'a | THUNK of unit -> 'a;

type 'a susp = 'a thunk ref;

fun delay (f : unit -> 'a) = ref (THUNK f);

fun force (su : 'a susp) : 'a = 
  case !su of
    VAL v   => v 
  | THUNK f => let val v = f () in su := VAL v; v end

end
(*#line 0.0 "$HOME/dev/sml/basic/src/Milton.sig"*)
(* ========================================================================= *)
(* MLton SPECIFIC FUNCTIONS                                                  *)
(* Created by Joe Hurd, September 2002                                       *)
(* ========================================================================= *)

signature Milton =
sig

(* The ML implementation *)
val ml : string

(* Pointer equality using the run-time system *)

(* Quotations a la Mosml *)
datatype 'a frag = QUOTE of string | ANTIQUOTE of 'a

(* Timing function applications a la Mosml.time *)
val time : ('a -> 'b) -> 'a -> 'b

(* Bring certain declarations to the top-level *)
type ppstream = PP.ppstream

(* Dummy versions of Mosml declarations to stop MLton barfing *)
val quotation : bool ref
val load      : string -> unit
val installPP : (ppstream -> 'a -> unit) -> unit

end
(*#line 0.0 "$HOME/dev/sml/basic/src/Milton.sml"*)
(* ========================================================================= *)
(* MLton SPECIFIC FUNCTIONS                                                  *)
(* Created by Joe Hurd, September 2002                                       *)
(* ========================================================================= *)

structure Milton :> Milton =
struct

(* ------------------------------------------------------------------------- *)
(* The ML implementation.                                                    *)
(* ------------------------------------------------------------------------- *)

val ml = "MLton";

(* ------------------------------------------------------------------------- *)
(* Pointer equality using the run-time system.                               *)
(* ------------------------------------------------------------------------- *)

(* ------------------------------------------------------------------------- *)
(* Quotations a la Mosml.                                                    *)
(* ------------------------------------------------------------------------- *)

datatype 'a frag = QUOTE of string | ANTIQUOTE of 'a;

(* ------------------------------------------------------------------------- *)
(* Timing function applications a la Mosml.time.                             *)
(* ------------------------------------------------------------------------- *)

fun time f x =
  let
    fun p t =
      let
        val s = Time.fmt 3 t
      in
        case size (List.last (String.fields (fn x => x = #".") s)) of 3 => s
        | 2 => s ^ "0"
        | 1 => s ^ "00"
        | _ => raise Fail "Milton.time"
      end
    val c = Timer.startCPUTimer ()
    val r = Timer.startRealTimer ()
    fun pt () =
      let
        val {usr, sys, ...} = Timer.checkCPUTimer c
        val real = Timer.checkRealTimer r
      in
        print
        ("User: " ^ p usr ^ "  System: " ^ p sys ^ "  Real: " ^ p real ^ "\n")
      end
    val y = f x handle e => (pt (); raise e)
    val () = pt ()
  in
    y
  end;

(* ------------------------------------------------------------------------- *)
(* Bring certain declarations to the top-level.                              *)
(* ------------------------------------------------------------------------- *)

type ppstream = PP.ppstream;

(* ------------------------------------------------------------------------- *)
(* Dummy versions of Mosml declarations to stop MLton barfing.               *)
(* ------------------------------------------------------------------------- *)

val quotation = ref false;
val load      = fn (_ : string) => ();
val installPP = fn (_ : ppstream -> 'a -> unit) => ();

end
open Milton;
(*#line 0.0 "basic/Useful.sig"*)
(* ========================================================================= *)
(* ML UTILITY FUNCTIONS                                                      *)
(* Created by Joe Hurd, April 2001                                           *)
(* ========================================================================= *)

signature Useful =
sig

(* Exceptions, profiling and tracing *)
exception ERR_EXN of {origin_function : string, message : string}
exception BUG_EXN of {origin_function : string, message : string}
val ERR      : string -> string -> exn
val BUG      : string -> string -> exn
val assert   : bool -> exn -> unit
val try      : ('a -> 'b) -> 'a -> 'b
val total    : ('a -> 'b) -> 'a -> 'b option
val can      : ('a -> 'b) -> 'a -> bool
val partial  : exn -> ('a -> 'b option) -> 'a -> 'b
val timed    : ('a -> 'b) -> 'a -> real * 'b
val tracing  : int ref
val traces   : {module : string, alignment : int -> int} list ref
val trace    : {module : string, message : string, level : int} -> unit

(* Combinators *)
val C  : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
val I  : 'a -> 'a
val K  : 'a -> 'b -> 'a
val N  : int -> ('a -> 'a) -> 'a -> 'a
val S  : ('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c
val W  : ('a -> 'a -> 'b) -> 'a -> 'b
val oo : ('a -> 'b) * ('c -> 'd -> 'a) -> 'c -> 'd -> 'b
val ## : ('a -> 'b) * ('c -> 'd) -> 'a * 'c -> 'b * 'd

(* Booleans *)
val bool_to_string : bool -> string
val non            : ('a -> bool) -> 'a -> bool

(* Pairs *)
val D       : 'a -> 'a * 'a
val Df      : ('a -> 'b) -> 'a * 'a -> 'b * 'b
val fst     : 'a * 'b -> 'a
val snd     : 'a * 'b -> 'b
val pair    : 'a -> 'b -> 'a * 'b
val curry   : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
val equal   : ''a -> ''a -> bool

(* State transformers *)
val unit   : 'a -> 's -> 'a * 's
val bind   : ('s -> 'a * 's) -> ('a -> 's -> 'b * 's) -> 's -> 'b * 's
val mmap   : ('a -> 'b) -> ('s -> 'a * 's) -> 's -> 'b * 's
val join   : ('s -> ('s -> 'a * 's) * 's) -> 's -> 'a * 's
val mwhile : ('a -> bool) -> ('a -> 's -> 'a * 's) -> 'a -> 's -> 'a * 's

(* Lists: note we count elements from 0 *)
val cons         : 'a -> 'a list -> 'a list
val append       : 'a list -> 'a list -> 'a list
val wrap         : 'a -> 'a list
val unwrap       : 'a list -> 'a
val first        : ('a -> 'b option) -> 'a list -> 'b option
val index        : ('a -> bool) -> 'a list -> int option
val maps         : ('a -> 's -> 'b * 's) -> 'a list -> 's -> 'b list * 's
val partial_maps : ('a -> 's -> 'b option * 's) -> 'a list -> 's -> 'b list * 's
val enumerate    : int -> 'a list -> (int * 'a) list
val cartwith     : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
val zipwith      : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
val zip          : 'a list -> 'b list -> ('a * 'b) list
val unzip        : ('a * 'b) list -> 'a list * 'b list
val split        : 'a list -> int -> 'a list * 'a list      (* Subscript *)
val update_nth   : ('a -> 'a) -> int -> 'a list -> 'a list  (* Subscript *)

(* Lists-as-sets *)
val mem       : ''a -> ''a list -> bool
val insert    : ''a -> ''a list -> ''a list
val delete    : ''a -> ''a list -> ''a list
val union     : ''a list -> ''a list -> ''a list
val intersect : ''a list -> ''a list -> ''a list
val subtract  : ''a list -> ''a list -> ''a list
val setify    : ''a list -> ''a list
val subset    : ''a list -> ''a list -> bool
val distinct  : ''a list -> bool

(* Comparisons *)
val lex_compare : ('a * 'a -> order) -> ('a * 'a) list -> order

(* Sorting and searching *)
val min   : ('a -> 'a -> bool) -> 'a list -> 'a
val merge : ('a -> 'a -> bool) -> 'a list -> 'a list -> 'a list
val sort  : ('a -> 'a -> bool) -> 'a list -> 'a list

(* Integers *)
val int_to_string : int -> string
val string_to_int : string -> int                 (* Overflow, Option *)
val int_to_bits   : int -> bool list
val bits_to_int   : bool list -> int              (* Overflow *)
val interval      : int -> int -> int list
val divides       : int -> int -> bool
val primes        : int -> int list

(* Strings *)
val variant     : string -> string list -> string
val variant_num : string -> string list -> string
val dest_prefix : string -> string -> string
val is_prefix   : string -> string -> bool
val mk_prefix   : string -> string -> string

(* Reals *)
val real_to_string : real -> string;

(* Pretty-printing *)
type 'a pp = ppstream -> 'a -> unit
val LINE_LENGTH  : int ref
val unit_pp      : 'a pp -> 'a -> unit pp
val pp_unit_pp   : unit pp pp
val pp_map       : ('a -> 'b) -> 'b pp -> 'a pp
val pp_bracket   : string * string -> 'a pp -> 'a pp
val pp_sequence  : string -> 'a pp -> 'a list pp
val pp_unop      : string -> 'a pp -> 'a pp
val pp_binop     : string -> 'a pp -> 'b pp -> ('a * 'b) pp
val pp_nothing   : 'a pp
val pp_string    : string pp
val pp_unit      : unit pp
val pp_bool      : bool pp
val pp_int       : int pp
val pp_real      : real pp
val pp_order     : order pp
val pp_list      : 'a pp -> 'a list pp
val pp_pair      : 'a pp -> 'b pp -> ('a * 'b) pp
val pp_triple    : 'a pp -> 'b pp -> 'c pp -> ('a * 'b * 'c) pp
val pp_record    : (string * unit pp) list -> unit pp
val pp_option    : 'a pp -> 'a option pp

(* Sum datatype *)
datatype ('a, 'b) sum = INL of 'a | INR of 'b
val is_inl : ('a, 'b) sum -> bool
val is_inr : ('a, 'b) sum -> bool

(* Maplets *)
datatype ('a, 'b) maplet = |-> of 'a * 'b
val pp_maplet : 'a pp -> 'b pp -> ('a, 'b) maplet pp

(* Trees *)
datatype ('a, 'b) tree = BRANCH of 'a * ('a, 'b) tree list | LEAF of 'b
val tree_size  : ('a, 'b) tree -> int
val tree_foldr : ('a -> 'c list -> 'c) -> ('b -> 'c) -> ('a, 'b) tree -> 'c
val tree_foldl :
  ('a -> 'c -> 'c) -> ('b -> 'c -> 'd) -> 'c -> ('a, 'b) tree -> 'd list
val tree_partial_foldl :
  ('a -> 'c -> 'c option) -> ('b -> 'c -> 'd option) -> 'c -> ('a, 'b) tree ->
  'd list

(* Useful imperative features *)
val lazify_thunk : (unit -> 'a) -> unit -> 'a
val new_int      : unit -> int
val new_ints     : int -> int list
val with_flag    : 'r ref * ('r -> 'r) -> ('a -> 'b) -> 'a -> 'b

(* Information about the environment *)
val host : string
val date : unit -> string

end
(*#line 0.0 "basic/Useful.sml"*)
(* ========================================================================= *)
(* ML UTILITY FUNCTIONS                                                      *)
(* Created by Joe Hurd, April 2001                                           *)
(* ========================================================================= *)

structure Useful :> Useful =
struct

infixr 0 oo ## |->;

(* ------------------------------------------------------------------------- *)
(* Exceptions, profiling and tracing.                                        *)
(* ------------------------------------------------------------------------- *)

exception ERR_EXN of {origin_function : string, message : string};
exception BUG_EXN of {origin_function : string, message : string};

fun ERR f s = ERR_EXN {origin_function = f, message = s};
fun BUG f s = BUG_EXN {origin_function = f, message = s};

fun ERR_to_string (ERR_EXN {origin_function, message}) =
  "\nERR in function " ^ origin_function ^ ":\n" ^ message ^ "\n"
  | ERR_to_string _ = raise BUG "ERR_to_string" "not a ERR_EXN";

fun BUG_to_string (BUG_EXN {origin_function, message}) =
  "\nBUG in function " ^ origin_function ^ ":\n" ^ message ^ "\n"
  | BUG_to_string _ = raise BUG "BUG_to_string" "not a BUG_EXN";

fun assert b e = if b then () else raise e;

fun try f a = f a
  handle h as ERR_EXN _ => (print (ERR_to_string h); raise h)
       | b as BUG_EXN _ => (print (BUG_to_string b); raise b)
       | e => (print "\ntry: strange exception raised\n"; raise e);

fun total f x = SOME (f x) handle ERR_EXN _ => NONE;

fun can f = Option.isSome o total f;

fun partial (e as ERR_EXN _) f x = (case f x of SOME y => y | NONE => raise e)
  | partial _ _ _ = raise BUG "partial" "must take a ERR_EXN";

fun timed f a =
  let
    val tmr = Timer.startCPUTimer ()
    val res = f a
    val {usr, sys, ...} = Timer.checkCPUTimer tmr
  in
    (Time.toReal usr + Time.toReal sys, res)
  end;

val tracing = ref 1;

val traces : {module : string, alignment : int -> int} list ref = ref [];

local
  val MAX = 10;
  val trace_printer = print;
  fun query m l =
    let val t = List.find (fn {module, ...} => module = m) (!traces)
    in case t of NONE => MAX | SOME {alignment, ...} => alignment l
    end;
in
  fun trace {module = m, message = s, level = l} =
    if 0 < !tracing andalso (MAX <= !tracing orelse query m l <= !tracing)
    then trace_printer s
    else ();
end;

(* ------------------------------------------------------------------------- *)
(* Combinators                                                               *)
(* ------------------------------------------------------------------------- *)

fun C f x y = f y x;
fun I x = x;
fun K x y = x;
fun N 0 _ x = x | N n f x = N (n - 1) f (f x);
fun S f g x = f x (g x);
fun W f x = f x x;
fun f oo g = fn x => f o (g x);

(* ------------------------------------------------------------------------- *)
(* Booleans                                                                  *)
(* ------------------------------------------------------------------------- *)

fun bool_to_string true = "true"
  | bool_to_string false = "false";

fun non f = not o f;

(* ------------------------------------------------------------------------- *)
(* Pairs                                                                     *)
(* ------------------------------------------------------------------------- *)

fun op## (f, g) (x, y) = (f x, g y);
fun D x = (x, x);
fun Df f = f ## f;
fun fst (x,_) = x;
fun snd (_,y) = y;
fun pair x y = (x, y)
(* Note: val add_fst = pair and add_snd = C pair; *)
fun curry f x y = f (x, y);
fun uncurry f (x, y) = f x y;
fun equal x y = (x = y);

(* ------------------------------------------------------------------------- *)
(* State transformers.                                                       *)
(* ------------------------------------------------------------------------- *)

val unit : 'a -> 's -> 'a * 's = pair;

fun bind f (g : 'a -> 's -> 'b * 's) = uncurry g o f;

fun mmap f (m : 's -> 'a * 's) = bind m (unit o f);

fun join (f : 's -> ('s -> 'a * 's) * 's) = bind f I;

fun mwhile c b = let fun f a = if c a then bind (b a) f else unit a in f end;

(* ------------------------------------------------------------------------- *)
(* Lists.                                                                    *)
(* ------------------------------------------------------------------------- *)

fun cons x y = x :: y;
fun append xs ys = xs @ ys;
fun wrap a = [a];
fun unwrap [a] = a | unwrap _ = raise ERR "unwrap" "not a singleton";

fun first f [] = NONE
  | first f (x :: xs) = (case f x of NONE => first f xs | s => s);

fun index p =
  let
    fun idx _ [] = NONE
      | idx n (x :: xs) = if p x then SOME n else idx (n + 1) xs
  in
    idx 0
  end;

(* This is the pure version
fun maps (_ : 'a -> 's -> 'b * 's) [] = unit []
  | maps f (x :: xs) =
  bind (f x) (fn y => bind (maps f xs) (fn ys => unit (y :: ys)));
*)

(* This is an optimized version *)
fun maps f =
  let fun g (x, (ys, s)) = let val (y, s) = f x s in (y :: ys, s) end
  in fn l => fn (s : 's) => (rev ## I) (foldl g ([], s) l)
  end;

(* This is the pure version
fun partial_maps (_ : 'a -> 's -> 'b option * 's) [] = unit []
  | partial_maps f (x :: xs) =
  bind (f x)
  (fn yo => bind (partial_maps f xs)
   (fn ys => unit (case yo of NONE => ys | SOME y => y :: ys)));
*)

(* This is an optimized version *)
fun partial_maps f =
  let
    fun g (x, (ys, s)) =
      let val (yo, s) = f x s
      in (case yo of NONE => ys | SOME y => y :: ys, s)
      end
  in
    fn l => fn (s : 's) => (rev ## I) (foldl g ([], s) l)
  end;

fun enumerate n = fst o C (maps (fn x => fn m => ((m, x), m + 1))) n;

fun zipwith f =
  let
    fun z l [] [] = l
      | z l (x :: xs) (y :: ys) = z (f x y :: l) xs ys
      | z _ _ _ = raise ERR "zipwith" "lists different lengths";
  in
    fn xs => fn ys => rev (z [] xs ys)
  end;

fun zip xs ys = zipwith pair xs ys;

fun unzip ab =
  foldl (fn ((x, y), (xs, ys)) => (x :: xs, y :: ys)) ([], []) (rev ab);

fun cartwith f =
  let
    fun aux _ res _ [] = res
      | aux xs_copy res [] (y :: yt) = aux xs_copy res xs_copy yt
      | aux xs_copy res (x :: xt) (ys as y :: _) =
      aux xs_copy (f x y :: res) xt ys
  in
    fn xs => fn ys =>
    let val xs' = rev xs in aux xs' [] xs' (rev ys) end
  end;

local
  fun aux res l 0 = (rev res, l)
    | aux _ [] _ = raise Subscript
    | aux res (h :: t) n = aux (h :: res) t (n - 1);
in
  fun split l n = aux [] l n;
end;

fun update_nth f n l =
  let
    val (a, b) = split l n
  in
    case b of [] => raise Subscript
    | h :: t => a @ (f h :: t)
  end;

(* ------------------------------------------------------------------------- *)
(* Lists-as-sets.                                                            *)
(* ------------------------------------------------------------------------- *)

fun mem x = List.exists (equal x);

fun insert x s = if mem x s then s else x :: s;
fun delete x s = List.filter (not o equal x) s;

(* Removes duplicates *)
fun setify s = foldl (fn (v, x) => if mem v x then x else v :: x) [] s;

(* For all three set operations: if s has duplicates, so may the result. *)
fun union s t = foldl (fn (v, x) => if mem v x then x else v :: x) s t;
fun intersect s t = foldl (fn (v, x) => if mem v t then v :: x else x) [] s;
fun subtract s t = foldl (fn (v, x) => if mem v t then x else v :: x) [] s;

fun subset s t = List.all (fn x => mem x t) s;

fun distinct [] = true
  | distinct (x :: rest) = not (mem x rest) andalso distinct rest;

(* ------------------------------------------------------------------------- *)
(* Comparisons.                                                              *)
(* ------------------------------------------------------------------------- *)

fun lex_compare f =
  let
    fun lex [] = EQUAL
      | lex (x :: l) = case f x of EQUAL => lex l | y => y
  in
    lex
  end;

(* ------------------------------------------------------------------------- *)
(* Finding the minimal element of a list, wrt some order.                    *)
(* ------------------------------------------------------------------------- *)

fun min f =
  let
    fun min_acc best [] = best
      | min_acc best (h :: t) = min_acc (if f best h then best else h) t
  in
    fn [] => raise ERR "min" "empty list"
     | h :: t => min_acc h t
  end;

(* ------------------------------------------------------------------------- *)
(* Merge (for the following merge-sort, but generally useful too).           *)
(* ------------------------------------------------------------------------- *)

fun merge f =
  let
    fun mrg res [] ys = foldl (op ::) ys res
      | mrg res xs [] = foldl (op ::) xs res
      | mrg res (xs as x :: xt) (ys as y :: yt) =
      if f x y then mrg (x :: res) xt ys else mrg (y :: res) xs yt
  in
    mrg []
  end;

(* ------------------------------------------------------------------------- *)
(* Order function here should be <= for a stable sort...                     *)
(* ...and I think < gives a reverse stable sort (but don't quote me).        *)
(* ------------------------------------------------------------------------- *)

fun sort f =
  let
    fun srt [] = []
      | srt (l as [x]) = l
      | srt l =
      let
        val halfway = length l div 2
      in
        merge f (srt (List.take (l, halfway))) (srt (List.drop (l, halfway)))
      end
  in
    srt
  end;

(* ------------------------------------------------------------------------- *)
(* Integers.                                                                 *)
(* ------------------------------------------------------------------------- *)

val int_to_string = Int.toString;
val string_to_int = Option.valOf o Int.fromString;

fun int_to_bits 0 = []
  | int_to_bits n = (n mod 2 <> 0) :: (int_to_bits (n div 2));

fun bits_to_int [] = 0
  | bits_to_int (h :: t) = (if h then curry op+ 1 else I) (2 * bits_to_int t);

fun interval m 0 = []
  | interval m len = m :: interval (m + 1) (len - 1);

fun divides a b = if a = 0 then b = 0 else b mod (Int.abs a) = 0;

local
  fun both f g n = f n andalso g n;
  fun next f = let fun nx x = if f x then x else nx (x + 1) in nx end;

  fun looking res 0 _ _ = rev res
    | looking res n f x =
    let
      val p = next f x
      val res' = p :: res
      val f' = both f (not o divides p)
    in
      looking res' (n - 1) f' (p + 1)
    end
in
  fun primes n = looking [] n (K true) 2
end;

(* ------------------------------------------------------------------------- *)
(* Strings.                                                                  *)
(* ------------------------------------------------------------------------- *)

fun variant x vars = if mem x vars then variant (x ^ "'") vars else x;

fun variant_num x vars =
  let
    fun xn n = x ^ int_to_string n
    fun v n = let val x' = xn n in if mem x' vars then v (n + 1) else x' end
  in
    if mem x vars then v 1 else x
  end;

fun dest_prefix p =
  let
    fun check s = assert (String.isPrefix p s) (ERR "dest_prefix" "")
    val size_p = size p
  in
    fn s => (check s; String.extract (s, size_p, NONE))
  end;

fun is_prefix p = can (dest_prefix p);

fun mk_prefix p s = p ^ s;

(* ------------------------------------------------------------------------- *)
(* Reals.                                                                    *)
(* ------------------------------------------------------------------------- *)

val real_to_string = Real.toString;

(* ------------------------------------------------------------------------- *)
(* Pretty-printing.                                                          *)
(* ------------------------------------------------------------------------- *)

type 'a pp = ppstream -> 'a -> unit;

val LINE_LENGTH = ref 75;

fun unit_pp pp_a a pp () = pp_a pp a;

fun pp_unit_pp pp upp = upp pp ();

fun pp_map f pp_a (ppstrm : ppstream) x : unit = pp_a ppstrm (f x);

fun pp_bracket (l, r) pp_a pp a =
  (PP.begin_block pp PP.INCONSISTENT (size l); PP.add_string pp l; pp_a pp a;
   PP.add_string pp r; PP.end_block pp);

fun pp_sequence sep pp_a =
  let
    fun pp_elt pp x = (PP.add_string pp sep; PP.add_break pp (1, 0); pp_a pp x)
    fun pp_seq pp []       = ()
      | pp_seq pp (h :: t) = (pp_a pp h; app (pp_elt pp) t)
  in
    fn pp => fn l =>
    (PP.begin_block pp PP.INCONSISTENT 0; pp_seq pp l; PP.end_block pp)
  end;

fun pp_unop s pp_a pp a =
  (PP.begin_block pp PP.CONSISTENT 0;
   PP.add_string pp s;
   PP.add_break pp (1, 0);
   pp_a pp a;
   PP.end_block pp);

fun pp_binop s pp_a pp_b pp (a, b) =
  (PP.begin_block pp PP.CONSISTENT 0;
   pp_a pp a;
   PP.add_string pp s;
   PP.add_break pp (1, 0);
   pp_b pp b;
   PP.end_block pp);

fun pp_nothing pp _ = (PP.begin_block pp PP.CONSISTENT 0; PP.end_block pp);

fun pp_string pp s =
  (PP.begin_block pp PP.CONSISTENT 0; PP.add_string pp s; PP.end_block pp);

val pp_unit = fn z => (pp_map (K "()") pp_string) z;

val pp_bool = pp_map bool_to_string pp_string;

val pp_int = pp_map int_to_string pp_string;

val pp_real = pp_map real_to_string pp_string;

val pp_order =
  pp_map (fn LESS => "LESS" | EQUAL => "EQUAL" | GREATER => "GREATER")
  pp_string;

fun pp_list pp_a = pp_bracket ("[", "]") (pp_sequence "," pp_a);

fun pp_pair pp_a pp_b = pp_bracket ("(", ")") (pp_binop "," pp_a pp_b);

fun pp_triple pp_a pp_b pp_c =
  pp_bracket ("(", ")")
  (pp_map (fn (a, b, c) => (a, (b, c)))
   (pp_binop "," pp_a (pp_binop "," pp_b pp_c)));

local
  val pp_l = fn z => (pp_sequence "," (pp_binop " =" pp_string pp_unit_pp)) z;
in
  fun pp_record l = pp_bracket ("{", "}") (unit_pp pp_l l);
end;

fun pp_option pp_a pp NONE = pp_string pp "NONE"
  | pp_option pp_a pp (SOME a) = pp_unop "SOME" pp_a pp a;

(* ------------------------------------------------------------------------- *)
(* Sums.                                                                     *)
(* ------------------------------------------------------------------------- *)

datatype ('a, 'b) sum = INL of 'a | INR of 'b

fun is_inl (INL _) = true | is_inl (INR _) = false;

fun is_inr (INR _) = true | is_inr (INL _) = false;

(* ------------------------------------------------------------------------- *)
(* Maplets.                                                                  *)
(* ------------------------------------------------------------------------- *)

datatype ('a, 'b) maplet = |-> of 'a * 'b;

fun pp_maplet pp_a pp_b =
  pp_map (fn a |-> b => (a, b)) (pp_binop " |->" pp_a pp_b);

(* ------------------------------------------------------------------------- *)
(* Trees.                                                                    *)
(* ------------------------------------------------------------------------- *)

datatype ('a, 'b) tree = BRANCH of 'a * ('a, 'b) tree list | LEAF of 'b;

fun tree_size (LEAF _) = 1
  | tree_size (BRANCH (_, t)) = foldl (op+ o (tree_size ## I)) 1 t;

fun tree_foldr f_b f_l (LEAF l) = f_l l
  | tree_foldr f_b f_l (BRANCH (p, s)) = f_b p (map (tree_foldr f_b f_l) s);

fun tree_foldl f_b f_l =
  let
    fun fold state (LEAF l, res) = f_l l state :: res
      | fold state (BRANCH (p, ts), res) = foldl (fold (f_b p state)) res ts
  in
    fn state => fn t => fold state (t, [])
  end;

fun tree_partial_foldl f_b f_l =
  let
    fun fold state (LEAF l, res) =
      (case f_l l state of NONE => res | SOME x => x :: res)
      | fold state (BRANCH (p, ts), res) =
      (case f_b p state of NONE => res | SOME s => foldl (fold s) res ts)
  in
    fn state => fn t => fold state (t, [])
  end;

(* ------------------------------------------------------------------------- *)
(* Useful imperative features.                                               *)
(* ------------------------------------------------------------------------- *)

fun lazify_thunk f = let val s = Susp.delay f in fn () => Susp.force s end;

local
  val generator = ref 0
in
  fun new_int () = let val n = !generator val () = generator := n + 1 in n end;

  fun new_ints 0 = []
    | new_ints k =
    let val n = !generator val () = generator := n + k in interval n k end;
end;

fun with_flag (r, update) f x =
  let
    val old = !r
    val () = r := update old
    val y = f x handle e => (r := old; raise e)
    val () = r := old
  in
    y
  end;

(* ------------------------------------------------------------------------- *)
(* Information about the environment.                                        *)
(* ------------------------------------------------------------------------- *)

val host = Option.getOpt (OS.Process.getEnv "HOSTNAME", "unknown");

val date = Date.fmt "%H:%M:%S %d/%m/%Y" o Date.fromTimeLocal o Time.now;

end
(*#line 0.0 "basic/Queue.sig"*)
(* ========================================================================= *)
(* A QUEUE DATATYPE FOR ML                                                   *)
(* Created by Joe Hurd, October 2001                                         *)
(* ========================================================================= *)

signature Queue =
sig

type 'a queue

val empty     : 'a queue
val add       : 'a -> 'a queue -> 'a queue
val is_empty  : 'a queue -> bool
val hd        : 'a queue -> 'a               (* raises Empty *)
val tl        : 'a queue -> 'a queue         (* raises Empty *)
val length    : 'a queue -> int
val from_list : 'a list -> 'a queue
val to_list   : 'a queue -> 'a list
val pp_queue  : 'a Useful.pp -> 'a queue Useful.pp

end
(*#line 0.0 "basic/Queue.sml"*)
(* ========================================================================= *)
(* A QUEUE DATATYPE FOR ML                                                   *)
(* Created by Joe Hurd, October 2001                                         *)
(* ========================================================================= *)

structure Queue :> Queue =
struct

type 'a queue = 'a list * 'a list;

val empty : 'a queue = ([], []);

fun norm ([], ys as _ :: _) = (rev ys, [])
  | norm q = q;

fun add z (xs, ys) = norm (xs, z :: ys);

fun is_empty ([], _) = true
  | is_empty (_ :: _, _) = false;

fun hd ([], _) = raise Empty
  | hd (x :: _, _) = x;

fun tl ([], _) = raise Empty
  | tl (_ :: xs, ys) = norm (xs, ys);

val length = fn (xs, ys) => length xs + length ys;

fun from_list l = (rev l, []);

fun to_list (xs, ys) = xs @ rev ys;

local
  open Useful;
in
  fun pp_queue pp_a =
    pp_map to_list (pp_bracket ("Q[", "]") (pp_sequence "," pp_a));
end;

end
(*#line 0.0 "basic/Heap.sig"*)
(* ========================================================================= *)
(* A HEAP DATATYPE FOR ML                                                    *)
(* Created by Joe Hurd, October 2001                                         *)
(* Taken from Purely Functional Data Structures, by Chris Okasaki.           *)
(* ========================================================================= *)

signature Heap =
sig

type 'a heap

val empty    : ('a * 'a -> order) -> 'a heap
val add      : 'a -> 'a heap -> 'a heap
val is_empty : 'a heap -> bool
val top      : 'a heap -> 'a            (* raises Empty *)
val remove   : 'a heap -> 'a * 'a heap  (* raises Empty *)
val size     : 'a heap -> int
val app      : ('a -> unit) -> 'a heap -> unit
val to_list  : 'a heap -> 'a list
val pp_heap  : 'a Useful.pp -> 'a heap Useful.pp

end
(*#line 0.0 "basic/Heap.sml"*)
(* ========================================================================= *)
(* A HEAP DATATYPE FOR ML                                                    *)
(* Created by Joe Hurd, October 2001                                         *)
(* Taken from Purely Functional Data Structures, by Chris Okasaki.           *)
(* ========================================================================= *)

(*
*)
structure Heap :> Heap =
struct

datatype 'a node = E | T of int * 'a * 'a node * 'a node;

datatype 'a heap = Heap of ('a * 'a -> order) * int * 'a node;

fun rank E = 0
  | rank (T (r, _, _, _)) = r;

fun makeT (x, a, b) =
  if rank a >= rank b then T (rank b + 1, x, a, b) else T (rank a + 1, x, b, a);

fun merge f =
  let
    fun mrg (h, E) = h
      | mrg (E, h) = h
      | mrg (h1 as T (_, x, a1, b1), h2 as T (_, y, a2, b2)) =
      (case f (x, y) of GREATER => makeT (y, a2, mrg (h1, b2))
       | _ => makeT (x, a1, mrg (b1, h2)))
  in
    mrg
  end;

fun empty f = Heap (f, 0, E);

fun add x (Heap (f, n, a)) = Heap (f, n + 1, merge f (T (1, x, E, E), a));

fun is_empty (Heap (_, _, E)) = true
  | is_empty (Heap (_, _, T _)) = false;

fun top (Heap (_, _, E)) = raise Empty
  | top (Heap (_, _, T (_, x, _, _))) = x;

fun remove (Heap (_, _, E)) = raise Empty
  | remove (Heap (f, n, T (_, x, a, b))) = (x, Heap (f, n - 1, merge f (a, b)));

fun size (Heap (_, n, _)) = n;

fun app f =
  let
    fun ap [] = ()
      | ap (E :: rest) = ap rest
      | ap (T (_, d, a, b) :: rest) = (f d; ap (a :: b :: rest))
  in
    fn Heap (_, _, a) => ap [a]
  end;

local
  fun to_lst res h =
    if is_empty h then rev res
    else let val (x, h) = remove h in to_lst (x :: res) h end;
in
  fun to_list h = to_lst [] h;
end;

local
  open Useful;
in
  fun pp_heap pp_a =
    pp_map to_list (pp_bracket ("H[", "]") (pp_sequence "," pp_a));
end;

end
(*#line 0.0 "basic/Multiset.sig"*)
(* ========================================================================= *)
(* A MULTISET DATATYPE FOR ML                                                *)
(* Created by Joe Hurd, July 2002                                            *)
(* ========================================================================= *)

signature Multiset =
sig

type 'a mset

val empty    : ('a * 'a -> order) -> 'a mset
val insert   : 'a * int -> 'a mset -> 'a mset
val count    : 'a mset -> 'a -> int
val union    : 'a mset -> 'a mset -> 'a mset
val compl    : 'a mset -> 'a mset
val subtract : 'a mset -> 'a mset -> 'a mset
val subset   : 'a mset -> 'a mset -> bool
val compare  : 'a mset * 'a mset -> order option
val app      : ('a * int -> unit) -> 'a mset -> unit
val to_list  : 'a mset -> ('a * int) list
val pp_mset  : 'a Useful.pp -> 'a mset Useful.pp

end
(*#line 0.0 "basic/Multiset.sml"*)
(* ========================================================================= *)
(* A MULTISET DATATYPE FOR ML                                                *)
(* Created by Joe Hurd, July 2002                                            *)
(* ========================================================================= *)

(*
List.app load ["Binarymap", "Useful"];
*)

(*
*)
structure Multiset :> Multiset =
struct

structure M = Binarymap;

fun Mpurge m k = let val (m, _) = M.remove (m, k) in m end;

fun Mall p =
  let
    exception Cut
    fun f (x, y, ()) = if p (x, y) then () else raise Cut
  in
    fn a => (M.foldl f () a; true) handle Cut => false
  end;

type 'a mset = ('a, int) M.dict;

fun empty ord : 'a mset = M.mkDict ord;

fun insert (_, 0) a = a
  | insert (x, n) a =
  (case M.peek (a, x) of NONE => M.insert (a, x, n)
   | SOME n' =>
   let val n'' = n + n'
   in if n'' = 0 then Mpurge a x else M.insert (a, x, n'')
   end);

fun count m x = case M.peek (m, x) of SOME n => n | NONE => 0;

local fun un a b = M.foldl (fn (x : 'a, n : int, d) => insert (x, n) d) a b;
in fun union a b = if M.numItems a < M.numItems b then un b a else un a b;
end;

fun compl a : 'a mset = M.transform ~ a;

fun subtract a b = union a (compl b);

local
  fun sign a = (Mall (fn (_, n) => 0 <= n) a, Mall (fn (_, n) => n <= 0) a);
in
  fun compare (a, b) =
    (case sign (subtract a b) of (true, true) => SOME EQUAL
     | (true, false) => SOME GREATER
     | (false, true) => SOME LESS
     | (false, false) => NONE);
end;

fun subset a b =
  (case compare (a, b) of SOME LESS => true
   | SOME EQUAL => true
   | _ => false);

fun app f (a : 'a mset) = M.app f a;

fun to_list (a : 'a mset) = M.listItems a;

local
  open Useful;
in
  fun pp_mset pp_a =
    pp_map (map Useful.|-> o to_list)
    (pp_bracket ("M[", "]") (pp_sequence "," (Useful.pp_maplet pp_a pp_int)));
end;

end
(*#line 0.0 "basic/Stream.sig"*)
(* ========================================================================= *)
(* A POSSIBLY-INFINITE STREAM DATATYPE FOR ML                                *)
(* Created by Joe Hurd, April 2001                                           *)
(* ========================================================================= *)

signature Stream =
sig

datatype 'a stream = NIL | CONS of 'a * (unit -> 'a stream)
type 'a Sthk = unit -> 'a stream

(* If you're wondering how to create an infinite stream: *)
(* val stream4 = let fun s4 () = CONS 4 s4 in s4 () end; *)

val cons          : 'a -> (unit -> 'a stream) -> 'a stream
val null          : 'a stream -> bool
val hd            : 'a stream -> 'a                    (* raises Empty *)
val tl            : 'a stream -> 'a stream             (* raises Empty *)
val dest          : 'a stream -> 'a * 'a stream        (* raises Empty *)
val repeat        : 'a -> 'a stream
val count         : int -> int stream
val fold          : ('a -> (unit -> 'b) -> 'b) -> 'b -> 'a stream -> 'b
val map           : ('a -> 'b) -> 'a stream -> 'b stream
val map_thk       : ('a Sthk -> 'a Sthk) -> 'a Sthk -> 'a Sthk
val partial_map   : ('a -> 'b option) -> 'a stream -> 'b stream
val maps          : ('a -> 'c -> 'b * 'c) -> 'c -> 'a stream -> 'b stream
val partial_maps  : ('a -> 'c -> 'b option * 'c) -> 'c -> 'a stream -> 'b stream
val filter        : ('a -> bool) -> 'a stream -> 'a stream
val flatten       : 'a stream stream -> 'a stream
val zipwith       : ('a -> 'b -> 'c) -> 'a stream -> 'b stream -> 'c stream
val zip           : 'a stream -> 'b stream -> ('a * 'b) stream
val take          : int -> 'a stream -> 'a stream      (* raises Subscript *)
val drop          : int -> 'a stream -> 'a stream      (* raises Subscript *)
val to_list       : 'a stream -> 'a list
val from_list     : 'a list -> 'a stream
val from_textfile : string -> string stream            (* lines of the file *)

end
(*#line 0.0 "basic/Stream.sml"*)
(* ========================================================================= *)
(* A POSSIBLY-INFINITE STREAM DATATYPE FOR ML                                *)
(* Created by Joe Hurd, April 2001                                           *)
(* ========================================================================= *)

structure Stream :> Stream =
struct

open Useful;

infixr 0 oo ##;

(* ------------------------------------------------------------------------- *)
(* The datatype declaration encapsulates all the primitive operations.       *)
(* ------------------------------------------------------------------------- *)

datatype 'a stream = NIL | CONS of 'a * (unit -> 'a stream);

type 'a Sthk = unit -> 'a stream;

(* ------------------------------------------------------------------------- *)
(* Useful functions.                                                         *)
(* ------------------------------------------------------------------------- *)

val cons = fn z => curry CONS z;

fun null NIL = true | null (CONS _) = false;

fun hd NIL = raise Empty | hd (CONS (h, _)) = h;

fun tl NIL = raise Empty | tl (CONS (_, t)) = t ();

fun dest s = (hd s, tl s);

fun repeat x = let fun rep () = CONS (x, rep) in rep () end;

fun count n = CONS (n, fn () => count (n + 1));

fun fold b c =
  let fun f NIL = c | f (CONS (x, xs)) = b x (fn () => f (xs ())) in f end;

fun map f =
  let
    fun m NIL = NIL
      | m (CONS (h, t)) = CONS (f h, fn () => m (t ()))
  in
    m
  end;

fun map_thk f =
  let
    fun mt NIL = NIL
      | mt (CONS (h, t)) = CONS (h, mt' t)
    and mt' t = f (fn () => mt (t ()))
  in
    mt'
  end;

fun partial_map f =
  let
    fun mp NIL = NIL
      | mp (CONS (h, t)) =
      case f h of NONE => mp (t ())
      | SOME h' => CONS (h', fn () => mp (t ()))
  in
    mp
  end;

fun maps f =
  let
    fun mm _ NIL = NIL
      | mm s (CONS (x, xs)) =
      let val (y, s') = f x s
      in CONS (y, fn () => mm s' (xs ()))
      end
  in
    mm
  end;

fun partial_maps f =
  let
    fun mm _ NIL = NIL
      | mm s (CONS (x, xs)) =
      let
        val (yo, s') = f x s
        val t = mm s' o xs
      in
        case yo of NONE => t () | SOME y => CONS (y, t)
      end
  in
    mm
  end;

fun filter f = partial_map (fn x => if f x then SOME x else NONE);

fun flatten NIL = NIL
  | flatten (CONS (NIL, ss)) = flatten (ss ())
  | flatten (CONS (CONS (x, xs), ss)) =
  CONS (x, fn () => flatten (CONS (xs (), ss)));

fun zipwith f =
  let
    fun z NIL _ = NIL
      | z _ NIL = NIL
      | z (CONS (x, xs)) (CONS (y, ys)) =
      CONS (f x y, fn () => z (xs ()) (ys ()))
  in
    z
  end;

fun zip s t = zipwith pair s t;

fun take 0 s = NIL
  | take n NIL = raise Subscript
  | take 1 (CONS (x, _)) = CONS (x, K NIL)
  | take n (CONS (x, xs)) = CONS (x, fn () => take (n - 1) (xs ()));

fun drop n s = N n tl s handle Empty => raise Subscript;

local
  fun to_lst res NIL = res
    | to_lst res (CONS (x, xs)) = to_lst (x :: res) (xs ());
in
  val to_list = fn z => (rev o to_lst []) z
end;

fun from_list [] = NIL
  | from_list (x :: xs) = CONS (x, fn () => from_list xs);

fun from_textfile filename =
  let
    open TextIO
    val fh = openIn filename
    fun res () =
      case inputLine fh of NONE => (closeIn fh; NIL)
      | SOME s => CONS (s, lazify_thunk res)
  in
    res ()
  end;

end
(*#line 0.0 "basic/Parser.sig"*)
(* ========================================================================= *)
(* PARSER COMBINATORS                                                        *)
(* Created by Joe Hurd, April 2001                                           *)
(* ========================================================================= *)

signature Parser =
sig

(* Recommended fixities
  infixr 9 >>++;
  infixr 8 ++;
  infixr 7 >>;
  infixr 6 ||;
*)

type 'a pp     = 'a Useful.pp
type 'a stream = 'a Stream.stream

(* Generic *)
exception Noparse
val ++         : ('a -> 'b * 'a) * ('a -> 'c * 'a) -> 'a -> ('b * 'c) * 'a
val >>         : ('a -> 'b * 'a) * ('b -> 'c) -> 'a -> 'c * 'a
val >>++       : ('a -> 'b * 'a) * ('b -> 'a -> 'c * 'a) -> 'a -> 'c * 'a
val ||         : ('a -> 'b * 'a) * ('a -> 'b * 'a) -> 'a -> 'b * 'a
val many       : ('a -> 'b * 'a) -> 'a -> 'b list * 'a
val atleastone : ('a -> 'b * 'a) -> 'a -> 'b list * 'a
val nothing    : 'a -> unit * 'a
val optional   : ('a -> 'b * 'a) -> 'a -> 'b option * 'a

(* Stream-based *)
type ('a, 'b) parser = 'a stream -> 'b * 'a stream
val maybe    : ('a -> 'b option) -> ('a, 'b) parser
val finished : ('a, unit) parser
val some     : ('a -> bool) -> ('a, 'a) parser
val any      : ('a, 'a) parser
val exact    : ''a -> (''a, ''a) parser

(* Parsing and pretty-printing for infix operators *)
type infixities  = {tok : string, prec : int, left_assoc : bool} list
type 'a con      = string * 'a * 'a -> 'a
type 'a des      = 'a -> (string * 'a * 'a) option
type 'a iparser  = (string, 'a) parser
type 'a iprinter = ('a * bool) pp
val optoks            : infixities -> string list
val parse_left_infix  : string list -> 'a con -> 'a iparser -> 'a iparser
val parse_right_infix : string list -> 'a con -> 'a iparser -> 'a iparser
val parse_infixes     : infixities  -> 'a con -> 'a iparser -> 'a iparser
val pp_left_infix     : string list -> 'a des -> 'a iprinter -> 'a iprinter
val pp_right_infix    : string list -> 'a des -> 'a iprinter -> 'a iprinter
val pp_infixes        : infixities  -> 'a des -> 'a iprinter -> 'a iprinter

(* Lexing *)
val space    : char -> bool
val digit    : char -> bool
val lower    : char -> bool
val upper    : char -> bool
val alpha    : char -> bool
val alphanum : char -> bool             (* alpha + digit + _ + ' *)
val symbol   : char -> bool             (* <>=-*+/\?@|!$%&~#^: *)
val punct    : char -> bool             (* ()[]{}.,; *)

(* Quotations *)
type 'a quotation = 'a frag list
val quotation_parser : (string -> 'a) -> 'b pp -> 'b quotation -> 'a

end
(*#line 0.0 "basic/Parser.sml"*)
(* ========================================================================= *)
(* PARSER COMBINATORS                                                        *)
(* Created by Joe Hurd, April 2001                                           *)
(* ========================================================================= *)

(*
app load ["Useful", "Stream"];
*)

(*
*)
structure Parser :> Parser =
struct

open Useful;

structure S = Stream;

infixr 9 >>++;
infixr 8 ++;
infixr 7 >>;
infixr 6 ||;
infix ##;

type 'a stream = 'a Stream.stream;
val omap       = Option.map;

(* ------------------------------------------------------------------------- *)
(* Generic.                                                                  *)
(* ------------------------------------------------------------------------- *)

exception Noparse;

fun op ++ (parser1, parser2) input =
  let
    val (result1, rest1) = parser1 input
    val (result2, rest2) = parser2 rest1
  in
    ((result1, result2), rest2)
  end;

fun op >> (parser, treatment) input =
  let
    val (result, rest) = parser input
  in
    (treatment result, rest)
  end;

fun op >>++ (parser, treatment) input =
  let
    val (result, rest) = parser input
  in
    treatment result rest
  end;

fun op || (parser1, parser2) input = parser1 input
handle Noparse => parser2 input;

fun many parser input =
  let
    val (result, next) = parser input
    val (results, rest) = many parser next
  in
    ((result :: results), rest)
  end
  handle Noparse => ([], input);

fun atleastone p = (p ++ many p) >> op::;

fun nothing input = ((), input);

fun optional p = (p >> SOME) || (nothing >> K NONE);

(* ------------------------------------------------------------------------- *)
(* Stream-based.                                                             *)
(* ------------------------------------------------------------------------- *)

type ('a, 'b) parser = 'a stream -> 'b * 'a stream

fun maybe p S.NIL = raise Noparse
  | maybe p (S.CONS (h, t)) =
  case p h of SOME r => (r, t ()) | NONE => raise Noparse;

fun finished S.NIL = ((), S.NIL)
  | finished (S.CONS _) = raise Noparse;

val finished: ('a, unit) parser = finished

fun some p = maybe (fn x => if p x then SOME x else NONE);

fun any input = some (K true) input;

fun exact tok = some (fn item => item = tok);

(* ------------------------------------------------------------------------- *)
(* Parsing and pretty-printing for infix operators.                          *)
(* ------------------------------------------------------------------------- *)

type infixities = {tok : string, prec : int, left_assoc : bool} list;
type 'a con      = string * 'a * 'a -> 'a;
type 'a des      = 'a -> (string * 'a * 'a) option;
type 'a iparser  = (string, 'a) parser;
type 'a iprinter = ('a * bool) pp;

local
  val sort_ops : infixities -> infixities =
    let
       fun order {prec, tok = _, left_assoc = _}
          {prec = prec', tok = _, left_assoc = _} =
          prec < prec'
    in sort order
    end;
  fun unflatten ({tok, prec, left_assoc}, ([], _)) =
    ([(left_assoc, [tok])], prec)
    | unflatten ({tok, prec, left_assoc}, ((a, l) :: dealt, p)) =
    if p = prec then
      (assert (left_assoc = a) (BUG "infix parser/printer" "mixed assocs");
       ((a, tok :: l) :: dealt, p))
    else
      ((left_assoc, [tok]) :: (a, l) :: dealt, prec);
in
  val layerops = fst o foldl unflatten ([], 0) o sort_ops;
end;

local
  fun chop (#" " :: chs) = (curry op+ 1 ## I) (chop chs) | chop chs = (0, chs);
  fun nspaces n = N n (curry op^ " ") "";
  fun spacify tok =
    let
      val chs = explode tok
      val (r, chs) = chop (rev chs)
      val (l, chs) = chop (rev chs)
    in
      ((l, r), implode chs)
    end;
  fun lrspaces (l, r) =
    (if l = 0 then K () else C PP.add_string (nspaces l),
     if r = 0 then K () else C PP.add_break (r, 0));
in
  val op_spaces = (lrspaces ## I) o spacify;
  val op_clean  = snd             o spacify;
end;

val optoks : infixities -> string list = map (fn {tok, ...} => op_clean tok);

fun parse_gen_infix update sof toks parse inp =
  let
    val (e, rest) = parse inp
    val continue =
      case rest of S.NIL => NONE
      | S.CONS (h, t) => if mem h toks then SOME (h, t) else NONE
  in
    case continue of NONE => (sof e, rest)
    | SOME (h, t) => parse_gen_infix update (update sof h e) toks parse (t ())
  end;

fun parse_left_infix toks con =
  parse_gen_infix (fn f => fn t => fn a => fn b => con (t, f a, b)) I toks;

fun parse_right_infix toks con =
  parse_gen_infix (fn f => fn t => fn a => fn b => f (con (t, a, b))) I toks;

fun parse_infixes ops =
  let
    val layeredops = map (I ## map op_clean) (layerops ops)
    fun iparser (a, t) = (if a then parse_left_infix else parse_right_infix) t
    val iparsers = map iparser layeredops
  in
    fn con => fn subparser => foldl (fn (p, sp) => p con sp) subparser iparsers
  end;

fun pp_gen_infix left toks : 'a des -> 'a iprinter -> 'a iprinter =
  let
    val spc = map op_spaces toks
  in
    fn dest => fn pp_sub =>
    let
      fun dest' tm =
        case dest tm of NONE => NONE
        | SOME (t, a, b) => omap (pair (a, b)) (List.find (equal t o snd) spc)
      open PP
      fun pp_go pp (tmr as (tm, r)) =
        case dest' tm of NONE => pp_sub pp tmr
        | SOME ((a, b), ((lspc, rspc), tok))
          => ((if left then pp_go else pp_sub) pp (a, true);
              lspc pp; add_string pp tok; rspc pp;
              (if left then pp_sub else pp_go) pp (b, r))
    in
      fn pp => fn tmr as (tm, _) =>
      case dest' tm of NONE => pp_sub pp tmr
      | SOME _ => (begin_block pp INCONSISTENT 0; pp_go pp tmr; end_block pp)
    end
  end;

fun pp_left_infix toks = pp_gen_infix true toks;

fun pp_right_infix toks = pp_gen_infix false toks;

fun pp_infixes ops =
  let
    val layeredops = layerops ops
    val toks = List.concat (map (map op_clean o snd) layeredops)
    fun iprinter (a, t) = (if a then pp_left_infix else pp_right_infix) t
    val iprinters = map iprinter layeredops
  in
    fn dest => fn pp_sub =>
    let
      fun printer sub = foldl (fn (ip, p) => ip dest p) sub iprinters
      fun is_op t = case dest t of SOME (x, _, _) => mem x toks | _ => false
      open PP
      fun subpr pp (tmr as (tm, _)) =
        if is_op tm then
          (begin_block pp INCONSISTENT 1; add_string pp "(";
           printer subpr pp (tm, false); add_string pp ")"; end_block pp)
        else pp_sub pp tmr
    in
      fn pp => fn tmr =>
      (begin_block pp INCONSISTENT 0; printer subpr pp tmr; end_block pp)
    end
  end;

(* ------------------------------------------------------------------------- *)
(* Lexing.                                                                   *)
(* ------------------------------------------------------------------------- *)

val space = Char.isSpace;
val digit = Char.isDigit;
val lower = Char.isLower;
val upper = Char.isUpper;
val alpha = Char.isAlpha;
val alphanum = fn c => alpha c orelse digit c orelse c = #"'" orelse c = #"_";
val symbol = Char.contains "<>=-*+/\\?@|!$%&~#^:";
val punct = Char.contains "()[]{}.,;";

(* ------------------------------------------------------------------------- *)
(* Quotations.                                                               *)
(* ------------------------------------------------------------------------- *)

type 'a quotation = 'a frag list;

fun quotation_parser parser pp_a =
  let val f = PP.pp_to_string (!LINE_LENGTH) pp_a
  in parser o foldl (fn (QUOTE q, s) => s ^ q | (ANTIQUOTE a, s) => s ^ f a) ""
  end;

end
(*#line 0.0 "fol/Term1.sig"*)
(* ========================================================================= *)
(* BASIC FIRST-ORDER LOGIC MANIPULATIONS                                     *)
(* Created by Joe Hurd, September 2001                                       *)
(* Partly ported from the CAML-Light code accompanying John Harrison's book  *)
(* ========================================================================= *)

signature Term1 =
sig

type 'a pp           = 'a Useful.pp
type ('a, 'b) maplet = ('a, 'b) Useful.maplet
type 'a quotation    = 'a Parser.quotation
type infixities      = Parser.infixities

(* Datatypes for terms and formulas *)
datatype term =
  Var of string
| Fn  of string * term list

datatype formula =
  True
| False
| Atom   of term
| Not    of formula
| And    of formula * formula
| Or     of formula * formula
| Imp    of formula * formula
| Iff    of formula * formula
| Forall of string * formula
| Exists of string * formula

(* Contructors and destructors *)
val dest_var        : term -> string
val is_var          : term -> bool

val dest_fn         : term -> string * term list
val is_fn           : term -> bool
val fn_name         : term -> string
val fn_args         : term -> term list
val fn_arity        : term -> int
val fn_function     : term -> string * int

val mk_const        : string -> term
val dest_const      : term -> string
val is_const        : term -> bool

val mk_binop        : string -> term * term -> term
val dest_binop      : string -> term -> term * term
val is_binop        : string -> term -> bool

val dest_atom       : formula -> term
val is_atom         : formula -> bool

val list_mk_conj    : formula list -> formula
val strip_conj      : formula -> formula list
val flatten_conj    : formula -> formula list

val list_mk_disj    : formula list -> formula
val strip_disj      : formula -> formula list
val flatten_disj    : formula -> formula list

val list_mk_forall  : string list * formula -> formula
val strip_forall    : formula -> string list * formula

val list_mk_exists  : string list * formula -> formula
val strip_exists    : formula -> string list * formula

(* New variables *)
val new_var  : unit -> term
val new_vars : int -> term list

(* Sizes of terms and formulas *)
val term_size    : term -> int
val formula_size : formula -> int

(* Total comparison functions for terms and formulas *)
val term_compare    : term * term -> order
val formula_compare : formula * formula -> order

(* Operations on literals *)
val mk_literal   : bool * formula -> formula
val dest_literal : formula -> bool * formula
val is_literal   : formula -> bool
val literal_atom : formula -> formula

(* Operations on formula negations *)
val negative : formula -> bool
val positive : formula -> bool
val negate   : formula -> formula

(* Functions and relations in a formula *)
val functions      : formula -> (string * int) list
val function_names : formula -> string list
val relations      : formula -> (string * int) list
val relation_names : formula -> string list

(* The equality relation has a special status *)
val eq_rel          : string * int
val mk_eq           : term * term -> formula
val dest_eq         : formula -> term * term
val is_eq           : formula -> bool
val lhs             : formula -> term
val rhs             : formula -> term
val eq_occurs       : formula -> bool
val relations_no_eq : formula -> (string * int) list

(* Free variables *)
val FVT        : term -> string list
val FV         : formula -> string list
val FVL        : formula list -> string list
val specialize : formula -> formula
val generalize : formula -> formula

(* Subterms *)
val subterm         : int list -> term -> term
val rewrite         : (int list, term) maplet -> term -> term
val literal_subterm : int list -> formula -> term
val literal_rewrite : (int list, term) maplet -> formula -> formula

(* The Knuth-Bendix ordering *)
type Weight = string * int -> int
type Prec   = (string * int) * (string * int) -> order
val kb_weight  : Weight -> term -> int * string Multiset.mset
val kb_compare : Weight -> Prec -> term * term -> order option

(* A datatype to antiquote both terms and formulas *)
datatype thing = Term of term | Formula of formula;

(* Operators parsed and printed infix *)
val infixes : infixities ref

(* Deciding whether a string denotes a variable or constant *)
val var_string : (string -> bool) ref

(* Parsing *)
val string_to_term'    : infixities -> string -> term  (* purely functional *)
val string_to_formula' : infixities -> string -> formula
val parse_term'        : infixities -> thing quotation -> term
val parse_formula'     : infixities -> thing quotation -> formula
val string_to_term     : string -> term                (* using !infixes *)
val string_to_formula  : string -> formula
val parse_term         : thing quotation -> term
val parse_formula      : thing quotation -> formula

(* Pretty-printing *)
val pp_term'           : infixities -> term pp         (* purely functional *)
val pp_formula'        : infixities -> formula pp
val term_to_string'    : infixities -> int -> term -> string
val formula_to_string' : infixities -> int -> formula -> string
val pp_term            : term pp                       (* using !infixes   *)
val pp_formula         : formula pp                    (* and !LINE_LENGTH *)
val term_to_string     : term -> string
val formula_to_string  : formula -> string

end
(*#line 0.0 "fol/Term1.sml"*)
(* ========================================================================= *)
(* BASIC FIRST-ORDER LOGIC MANIPULATIONS                                     *)
(* Created by Joe Hurd, September 2001                                       *)
(* Partly ported from the CAML-Light code accompanying John Harrison's book  *)
(* ========================================================================= *)

(*
app load ["Useful", "Stream", "Parser", "Mosml", "Binarymap"];
*)

(*
*)
structure Term1 :> Term1 =
struct

open Parser Useful;

infixr 8 ++;
infixr 7 >>;
infixr 6 ||;
infixr |-> ::> @> oo ##;

(* ------------------------------------------------------------------------- *)
(* Datatypes for storing first-order terms and formulas.                     *)
(* ------------------------------------------------------------------------- *)

datatype term =
  Var of string
| Fn  of string * term list;

datatype formula =
  True
| False
| Atom   of term
| Not    of formula
| And    of formula * formula
| Or     of formula * formula
| Imp    of formula * formula
| Iff    of formula * formula
| Forall of string * formula
| Exists of string * formula;

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

(* Variables *)

fun dest_var (Var v) = v
  | dest_var (Fn _) = raise ERR "dest_var" "";

val is_var = can dest_var;

(* Functions *)

fun dest_fn (Fn f) = f
  | dest_fn (Var _) = raise ERR "dest_fn" "";

val is_fn = can dest_fn;

val fn_name = fst o dest_fn;

val fn_args = snd o dest_fn;

val fn_arity = length o fn_args;

fun fn_function tm = (fn_name tm, fn_arity tm);

(* Constants *)

fun mk_const c = (Fn (c, []));

fun dest_const (Fn (c, [])) = c
  | dest_const _ = raise ERR "dest_const" "";

val is_const = can dest_const;

(* Binary functions *)

fun mk_binop f (a, b) = Fn (f, [a, b]);

fun dest_binop f (Fn (x, [a, b])) =
  if x = f then (a, b) else raise ERR "dest_binop" "wrong binop"
  | dest_binop _ _ = raise ERR "dest_binop" "not a binop";

fun is_binop f = can (dest_binop f);

(* Atoms *)

fun dest_atom (Atom a) = a
  | dest_atom _ = raise ERR "dest_atom" "";

val is_atom = can dest_atom;

(* Conjunctions *)

fun list_mk_conj l = (case rev l of [] => True | h :: t => foldl And h t);

local
  fun conj cs (And (a, b)) = conj (a :: cs) b
    | conj cs fm = rev (fm :: cs);
in
  fun strip_conj True = []
    | strip_conj fm = conj [] fm;
end;

val flatten_conj =
  let
    fun flat acc []                  = acc
      | flat acc (And (p, q) :: fms) = flat acc (q :: p :: fms)
      | flat acc (True       :: fms) = flat acc fms
      | flat acc (fm         :: fms) = flat (fm :: acc) fms
  in
    fn fm => flat [] [fm]
  end;

(* Disjunctions *)

fun list_mk_disj l = (case rev l of [] => False | h :: t => foldl Or h t);

local
  fun disj cs (Or (a, b)) = disj (a :: cs) b
    | disj cs fm = rev (fm :: cs);
in
  fun strip_disj False = []
    | strip_disj fm = disj [] fm;
end;

val flatten_disj =
  let
    fun flat acc []                 = acc
      | flat acc (Or (p, q) :: fms) = flat acc (q :: p :: fms)
      | flat acc (False     :: fms) = flat acc fms
      | flat acc (fm        :: fms) = flat (fm :: acc) fms
  in
    fn fm => flat [] [fm]
  end;

(* Universal quantifiers *)

fun list_mk_forall ([], body) = body
  | list_mk_forall (v :: vs, body) = Forall (v, list_mk_forall (vs, body));

local
  fun dest vs (Forall (v, b)) = dest (v :: vs) b
    | dest vs tm = (rev vs, tm);
in
  val strip_forall = dest [];
end;

(* Existential quantifiers *)

fun list_mk_exists ([], body) = body
  | list_mk_exists (v :: vs, body) = Exists (v, list_mk_exists (vs, body));

local
  fun dest vs (Exists (v, b)) = dest (v :: vs) b
    | dest vs tm = (rev vs, tm);
in
  val strip_exists = dest [];
end;

(* ------------------------------------------------------------------------- *)
(* A datatype to antiquote both terms and formulas.                          *)
(* ------------------------------------------------------------------------- *)

datatype thing = Term of term | Formula of formula;

(* ------------------------------------------------------------------------- *)
(* Built-in infix operators and reserved symbols.                            *)
(* ------------------------------------------------------------------------- *)

val infixes : infixities ref = ref
  [(* ML style *)
   {tok = " / ",   prec = 7,  left_assoc = true},
   {tok = " div ", prec = 7,  left_assoc = true},
   {tok = " mod ", prec = 7,  left_assoc = true},
   {tok = " * ",   prec = 7,  left_assoc = true},
   {tok = " + ",   prec = 6,  left_assoc = true},
   {tok = " - ",   prec = 6,  left_assoc = true},
   {tok = " ^ ",   prec = 6,  left_assoc = true},
   {tok = " @ ",   prec = 5,  left_assoc = false},
   {tok = " :: ",  prec = 5,  left_assoc = false},
   {tok = " = ",   prec = 4,  left_assoc = true},    (* may be interpreted *)
   {tok = " == ",  prec = 4,  left_assoc = true},    (* won't be interpreted *)
   {tok = " <> ",  prec = 4,  left_assoc = true},
   {tok = " <= ",  prec = 4,  left_assoc = true},
   {tok = " < ",   prec = 4,  left_assoc = true},
   {tok = " >= ",  prec = 4,  left_assoc = true},
   {tok = " > ",   prec = 4,  left_assoc = true},
   {tok = " o ",   prec = 8,  left_assoc = true},    (* ML prec = 3 *)
   (* HOL style *)
   {tok = " % ",   prec = 9,  left_assoc = true},    (* function application *)
   {tok = " -> ",  prec = 2,  left_assoc = false},   (* HOL ty prec = 50 *)
   {tok = " : ",   prec = 1,  left_assoc = false},   (* not in HOL grammars *)
   {tok =  ", ",   prec = 0,  left_assoc = false},   (* HOL tm prec = 50 *)
   (* Convenient alternative symbols *)
   {tok = " ** ",  prec = 7,  left_assoc = true},
   {tok = " ++ ",  prec = 6,  left_assoc = true},
   {tok = " -- ",  prec = 6,  left_assoc = true}];

val connectives =
  [{tok = " /\\ ", prec = ~1, left_assoc = false},
   {tok = " \\/ ", prec = ~2, left_assoc = false},
   {tok = " ==> ", prec = ~3, left_assoc = false},
   {tok = " <=> ", prec = ~4, left_assoc = false}];

val reserved = ["!", "?", "(", ")", ".", "~"];

(* ------------------------------------------------------------------------- *)
(* Deciding whether a string denotes a variable or constant.                 *)
(* ------------------------------------------------------------------------- *)

val var_string =
  ref (C mem [#"_",#"v",#"w",#"x",#"y",#"z"] o Char.toLower o hd o explode);

(* ------------------------------------------------------------------------- *)
(* Pretty-printing.                                                          *)
(* ------------------------------------------------------------------------- *)

(* Purely functional pretty-printing *)

val pp_vname =
  pp_map (fn s => if !var_string s then s else "var->" ^ s ^ "<-var") pp_string;

val pp_cname =
  pp_map (fn s => if !var_string s then "const->" ^ s ^ "<-const" else s)
  pp_string;

val pp_fname =
  pp_map (fn s => if !var_string s then "fn->" ^ s ^ "<-fn" else s) pp_string;

fun pp_term' ops =
  let
    val ops = ops @ connectives
    val iprinter = pp_infixes ops
    val itoks = optoks ops
    fun pp_uninfix pp_s pp s =
      if mem s itoks then PP.add_string pp ("(" ^ s ^ ")") else pp_s pp s
    fun idest (Fn (f, [a, b])) = SOME (f, a, b) | idest _ = NONE
    fun is_op t = case idest t of SOME (f, _, _) => mem f itoks | NONE => false
    fun is_q (Fn ("!", _)) = true | is_q (Fn ("?", _)) = true | is_q _ = false
    fun negs (Fn ("~", [a])) = (curry op+ 1 ## I) (negs a) | negs tm = (0, tm)
    fun binds s (tm as Fn (n, [Var v, b])) =
      if s = n then (cons v ## I) (binds s b) else ([], tm)
      | binds _ tm = ([], tm)
    open PP
    fun basic pp (Var v) = pp_vname pp v
      | basic pp (Fn (c, [])) = pp_uninfix pp_cname pp c
      | basic pp (Fn (f, a)) =
      (pp_uninfix pp_fname pp f;
       app (fn x => (add_break pp (1, 0); argument pp x)) a)
    and argument pp tm =
      if is_var tm orelse is_const tm then basic pp tm else pp_btm pp tm
    and quant pp (tm, r) =
      let
        fun pr pp (Fn (q, [Var v, tm])) =
          let
            val (vs, body) = binds q tm
          in
            add_string pp q;
            pp_vname pp v;
            app (fn a => (add_break pp (1, 0); pp_vname pp a)) vs;
            add_string pp ".";
            add_break pp (1, 0);
            if is_q body then pr pp body else pp_tm pp (body, false)
          end
          | pr pp tm = raise BUG "pp_term" "not a quantifier"
        fun pp_q pp t = (begin_block pp INCONSISTENT 2; pr pp t; end_block pp)
      in
        (if is_q tm then (if r then pp_bracket ("(", ")") else I) pp_q
         else basic) pp tm
      end
    and molecule pp (tm, r) =
      let
        val (n, x) = negs tm
      in
        begin_block pp INCONSISTENT n;
        N n (fn () => add_string pp "~") ();
        if is_op x then pp_btm pp x else quant pp (x, r);
        end_block pp
      end
    and pp_btm pp tm = pp_bracket ("(", ")") pp_tm pp (tm, false)
    and pp_tm pp tmr = iprinter idest molecule pp tmr
  in
    pp_map (C pair false) pp_tm
  end;

local
  fun demote True            = Fn ("T",   []                  )
    | demote False           = Fn ("F",   []                  )
    | demote (Not a)         = Fn ("~",   [demote a]          )
    | demote (And (a, b))    = Fn ("/\\", [demote a, demote b])
    | demote (Or  (a, b))    = Fn ("\\/", [demote a, demote b])
    | demote (Imp (a, b))    = Fn ("==>", [demote a, demote b])
    | demote (Iff (a, b))    = Fn ("<=>", [demote a, demote b])
    | demote (Forall (v, b)) = Fn ("!",   [Var v,    demote b])
    | demote (Exists (v, b)) = Fn ("?",   [Var v,    demote b])
    | demote (Atom t)        = t;
in
  fun pp_formula' ops = pp_map demote (pp_term' ops);
end;

fun term_to_string'    ops len tm = PP.pp_to_string len (pp_term'    ops) tm;
fun formula_to_string' ops len fm = PP.pp_to_string len (pp_formula' ops) fm;

(* Pretty-printing things is needed for parsing thing quotations *)

fun pp_thing ops pp (Term tm)    = pp_term'    ops pp tm
  | pp_thing ops pp (Formula fm) = pp_formula' ops pp fm;

fun pp_bracketed_thing ops pp th =
  (PP.begin_block pp PP.INCONSISTENT 1; PP.add_string pp "(";
   pp_thing ops pp th; PP.add_string pp ")"; PP.end_block pp);

(* Pretty-printing using !infixes and !LINE_LENGTH *)

fun pp_term        pp tm = pp_term'           (!infixes) pp             tm;
fun pp_formula     pp fm = pp_formula'        (!infixes) pp             fm;
fun term_to_string    tm = term_to_string'    (!infixes) (!LINE_LENGTH) tm;
fun formula_to_string fm = formula_to_string' (!infixes) (!LINE_LENGTH) fm;

(* ------------------------------------------------------------------------- *)
(* Parsing.                                                                  *)
(* ------------------------------------------------------------------------- *)

(* Lexing *)

val lexer =
  (fn ((_, (toks, _)), _) => toks) o
  (many (some space) ++
   (many
    ((((atleastone (some alphanum) ||
        (some (fn c => symbol c andalso c <> #"~") ++ many (some symbol)) >>
        op ::) >> implode
       || some (fn c => c = #"~" orelse punct c) >> str) ++
      many (some space)) >> fst)) ++
   finished);

val lex_str = lexer o Stream.from_list o explode;

(* Purely functional parsing *)

val vname_parser =
  some (fn tok => not (mem tok reserved) andalso !var_string tok);

fun term_parser ops =
  let
    val ops            = ops @ connectives
    val iparser        = parse_infixes ops
    val itoks          = optoks ops
    val avoid          = itoks @ reserved
    fun fname tok      = not (mem tok avoid) andalso not (!var_string tok)
    fun uninfix tok    = mem tok itoks
    val uninfix_parser = (exact "(" ++ some uninfix ++ exact ")") >> (fst o snd)
    val fname_parser   = some fname || uninfix_parser
    fun bind s (v, t)  = Fn (s, [Var v, t])
    fun basic inp      =
      ((exact "(" ++ tm_parser ++ exact ")") >> (fn (_, (t, _)) => t) ||
       (exact "!" ++ atleastone vname_parser ++ exact "." ++ tm_parser) >>
       (fn (_, (vs, (_, body))) => foldr (bind "!") body vs) ||
       (exact "?" ++ atleastone vname_parser ++ exact "." ++ tm_parser) >>
       (fn (_, (vs, (_, body))) => foldr (bind "?") body vs) ||
       fname_parser >> (fn f => Fn (f, [])) ||
       vname_parser >> Var) inp
    and molecule inp      =
      ((many (exact "~") ++ ((fname_parser ++ many basic) >> Fn || basic)) >>
       (fn (l, t) => N (length l) (fn x => Fn ("~", [x])) t)) inp
    and tm_parser inp  = iparser (fn (f, a, b) => Fn (f, [a, b])) molecule inp
  in
    tm_parser
  end;

local
  fun promote (Fn ("T",   []        )) = True
    | promote (Fn ("F",   []        )) = False
    | promote (Fn ("~",   [a]       )) = Not (promote a)
    | promote (Fn ("/\\", [a, b]    )) = And (promote a, promote b)
    | promote (Fn ("\\/", [a, b]    )) = Or  (promote a, promote b)
    | promote (Fn ("==>", [a, b]    )) = Imp (promote a, promote b)
    | promote (Fn ("<=>", [a, b]    )) = Iff (promote a, promote b)
    | promote (Fn ("!",   [Var v, b])) = Forall (v, promote b)
    | promote (Fn ("?",   [Var v, b])) = Exists (v, promote b)
    | promote tm                       = Atom tm;
in
  fun formula_parser ops = term_parser ops >> promote;
end;

fun string_to_term' ops =
  fst o ((term_parser ops ++ finished) >> fst) o Stream.from_list o lex_str;

fun string_to_formula' ops =
  fst o ((formula_parser ops ++ finished) >> fst) o Stream.from_list o lex_str;

fun parse_term' ops =
  quotation_parser (string_to_term' ops) (pp_bracketed_thing ops);

fun parse_formula' ops =
  quotation_parser (string_to_formula' ops) (pp_bracketed_thing ops);

(* Parsing using !infixes *)

fun string_to_term    s = string_to_term'    (!infixes) s;
fun string_to_formula s = string_to_formula' (!infixes) s;
fun parse_term        q = parse_term'        (!infixes) q;
fun parse_formula     q = parse_formula'     (!infixes) q;

(* ------------------------------------------------------------------------- *)
(* New variables.                                                            *)
(* ------------------------------------------------------------------------- *)

local
  val prefix  = "_";
  val num_var = Var o mk_prefix prefix o int_to_string;
in
  val new_var  = num_var o new_int;
  val new_vars = map num_var o new_ints;
end;

(* ------------------------------------------------------------------------- *)
(* Sizes of terms and formulas.                                              *)
(* ------------------------------------------------------------------------- *)

local
  fun szt n []                    = n
    | szt n (Var _        :: tms) = szt (n + 1) tms
    | szt n (Fn (_, args) :: tms) = szt (n + 1) (args @ tms);

  fun sz n []                     = n
    | sz n (True          :: fms) = sz (n + 1) fms
    | sz n (False         :: fms) = sz (n + 1) fms
    | sz n (Atom t        :: fms) = sz (szt (n + 1) [t]) fms
    | sz n (Not p         :: fms) = sz (n + 1) (p :: fms)
    | sz n (And (p, q)    :: fms) = sz (n + 1) (p :: q :: fms)
    | sz n (Or  (p, q)    :: fms) = sz (n + 1) (p :: q :: fms)
    | sz n (Imp (p, q)    :: fms) = sz (n + 1) (p :: q :: fms)
    | sz n (Iff (p, q)    :: fms) = sz (n + 1) (p :: q :: fms)
    | sz n (Forall (_, p) :: fms) = sz (n + 1) (p :: fms)
    | sz n (Exists (_, p) :: fms) = sz (n + 1) (p :: fms);
in
  val term_size    = szt 0 o wrap;
  val formula_size = sz  0 o wrap;
end;

(* ------------------------------------------------------------------------- *)
(* Total comparison functions for terms and formulas.                        *)
(* ------------------------------------------------------------------------- *)

local
  fun lex EQUAL f x = f x | lex x _ _ = x;

  fun cmt [] = EQUAL
    | cmt ((Var _, Fn _) :: _) = LESS
    | cmt ((Fn _, Var _) :: _) = GREATER
    | cmt ((Var v, Var w) :: l) = lex (String.compare (v, w)) cmt l
    | cmt ((Fn (f, a), Fn (g, b)) :: l) =
    (case lex (String.compare (f, g)) (Int.compare o Df length) (a, b) of EQUAL
       => cmt (zip a b @ l)
     | x => x);

  fun cm [] = EQUAL
    | cm ((True,          True         ) :: l) = cm l
    | cm ((True,          _            ) :: _) = LESS
    | cm ((_,             True         ) :: _) = GREATER
    | cm ((False,         False        ) :: l) = cm l
    | cm ((False,         _            ) :: _) = LESS
    | cm ((_,             False        ) :: _) = GREATER
    | cm ((Atom t,        Atom u       ) :: l) = lex (cmt [(t, u)]) cm l
    | cm ((Atom _,        _            ) :: _) = LESS
    | cm ((_,             Atom _       ) :: _) = GREATER
    | cm ((Not p,         Not q        ) :: l) = cm ((p, q) :: l)
    | cm ((Not _ ,        _            ) :: _) = LESS
    | cm ((_,             Not _        ) :: _) = GREATER
    | cm ((And (p1, q1),  And (p2, q2) ) :: l) = cm ((p1, p2) :: (q1, q2) :: l)
    | cm ((And _,         _            ) :: _) = LESS
    | cm ((_,             And _        ) :: _) = GREATER
    | cm ((Or (p1, q1),   Or (p2, q2)  ) :: l) = cm ((p1, p2) :: (q1, q2) :: l)
    | cm ((Or _,          _            ) :: _) = LESS
    | cm ((_,             Or _         ) :: _) = GREATER
    | cm ((Imp (p1, q1),  Imp (p2, q2) ) :: l) = cm ((p1, p2) :: (q1, q2) :: l)
    | cm ((Imp _,         _            ) :: _) = LESS
    | cm ((_,             Imp _        ) :: _) = GREATER
    | cm ((Iff (p1, q1),  Iff (p2, q2) ) :: l) = cm ((p1, p2) :: (q1, q2) :: l)
    | cm ((Iff _,         _            ) :: _) = LESS
    | cm ((_,             Iff _        ) :: _) = GREATER
    | cm ((Forall (v, p), Forall (w, q)) :: l) =
    lex (String.compare (v, w)) (cm o cons (p, q)) l
    | cm ((Forall _,      Exists _     ) :: _) = LESS
    | cm ((Exists _,      Forall _     ) :: _) = GREATER
    | cm ((Exists (v, p), Exists (w, q)) :: l) =
    lex (String.compare (v, w)) (cm o cons (p, q)) l;
in
  val term_compare    = cmt o wrap;
  val formula_compare = cm o wrap;
end;
  
(* ------------------------------------------------------------------------- *)
(* Basic operations on literals.                                             *)
(* ------------------------------------------------------------------------- *)

fun mk_literal (true,  a) = a
  | mk_literal (false, a) = Not a;

fun dest_literal (a as Atom _)       = (true,  a)
  | dest_literal (Not (a as Atom _)) = (false, a)
  | dest_literal _                   = raise ERR "dest_literal" "";

val is_literal = can dest_literal;

val literal_atom = snd o dest_literal;

(* ------------------------------------------------------------------------- *)
(* Dealing with formula negations.                                           *)
(* ------------------------------------------------------------------------- *)

fun negative (Not p) = true
  | negative _       = false;

val positive = non negative;

fun negate (Not p) = p
  | negate p       = Not p;

(* ------------------------------------------------------------------------- *)
(* Functions and relations in a formula.                                     *)
(* ------------------------------------------------------------------------- *)

local
  fun fnc fs []                 = fs
    | fnc fs (Var _     :: tms) = fnc fs tms
    | fnc fs (Fn (n, a) :: tms) = fnc (insert (n, length a) fs) (a @ tms);

  fun func fs []                          = fs
    | func fs (True               :: fms) = func fs fms
    | func fs (False              :: fms) = func fs fms
    | func fs (Atom (Var _)       :: fms) = func fs fms
    | func fs (Atom (Fn (_, tms)) :: fms) = func (fnc fs tms) fms
    | func fs (Not p              :: fms) = func fs (p :: fms)
    | func fs (And (p, q)         :: fms) = func fs (p :: q :: fms)
    | func fs (Or  (p, q)         :: fms) = func fs (p :: q :: fms)
    | func fs (Imp (p, q)         :: fms) = func fs (p :: q :: fms)
    | func fs (Iff (p, q)         :: fms) = func fs (p :: q :: fms)
    | func fs (Forall (_, p)      :: fms) = func fs (p :: fms)
    | func fs (Exists (_, p)      :: fms) = func fs (p :: fms);
in
  val functions = func [] o wrap;
end;

val function_names = map fst o functions;

local
  fun rel rs []                        = rs
    | rel rs (True             :: fms) = rel rs fms
    | rel rs (False            :: fms) = rel rs fms
    | rel rs (Atom (Var _)     :: fms) = rel rs fms
    | rel rs (Atom (f as Fn _) :: fms) = rel (insert (fn_function f) rs) fms
    | rel rs (Not p            :: fms) = rel rs (p :: fms)
    | rel rs (And (p, q)       :: fms) = rel rs (p :: q :: fms)
    | rel rs (Or  (p, q)       :: fms) = rel rs (p :: q :: fms)
    | rel rs (Imp (p, q)       :: fms) = rel rs (p :: q :: fms)
    | rel rs (Iff (p, q)       :: fms) = rel rs (p :: q :: fms)
    | rel rs (Forall (_, p)    :: fms) = rel rs (p :: fms)
    | rel rs (Exists (_, p)    :: fms) = rel rs (p :: fms);
in
  val relations = rel [] o wrap;
end;

val relation_names = map fst o relations;

(* ------------------------------------------------------------------------- *)
(* The equality relation has a special status.                               *)
(* ------------------------------------------------------------------------- *)

val eq_rel = ("=", 2);

fun mk_eq (a, b) = Atom (Fn ("=", [a, b]));

fun dest_eq (Atom (Fn ("=", [a, b]))) = (a, b)
  | dest_eq _ = raise ERR "dest_eq" "";

val is_eq = can dest_eq;

val lhs = fst o dest_eq;

val rhs = snd o dest_eq;

val eq_occurs = mem eq_rel o relations;

val relations_no_eq = List.filter (non (equal eq_rel)) o relations;

(* ------------------------------------------------------------------------- *)
(* Free variables in terms and formulas.                                     *)
(* ------------------------------------------------------------------------- *)

local
  fun fvt av =
    let
      val seen =
        if null av then mem else (fn v => fn vs => mem v av orelse mem v vs)
      fun f vs [] = vs
        | f vs (Var v :: tms) = f (if seen v vs then vs else v :: vs) tms
        | f vs (Fn (_, args) :: tms) = f vs (args @ tms)
    in
      f
    end;

  fun fv vs []                           = vs
    | fv vs ((_ , True         ) :: fms) = fv vs fms
    | fv vs ((_ , False        ) :: fms) = fv vs fms
    | fv vs ((av, Atom t       ) :: fms) = fv (fvt av vs [t]) fms
    | fv vs ((av, Not p        ) :: fms) = fv vs ((av, p) :: fms)
    | fv vs ((av, And (p, q)   ) :: fms) = fv vs ((av, p) :: (av, q) :: fms)
    | fv vs ((av, Or  (p, q)   ) :: fms) = fv vs ((av, p) :: (av, q) :: fms)
    | fv vs ((av, Imp (p, q)   ) :: fms) = fv vs ((av, p) :: (av, q) :: fms)
    | fv vs ((av, Iff (p, q)   ) :: fms) = fv vs ((av, p) :: (av, q) :: fms)
    | fv vs ((av, Forall (x, p)) :: fms) = fv vs ((insert x av, p) :: fms)
    | fv vs ((av, Exists (x, p)) :: fms) = fv vs ((insert x av, p) :: fms);
in    
  fun FVT tm  = rev (fvt [] [] [tm]);
  fun FV  fm  = rev (fv  [] [([], fm)]);
  fun FVL fms = rev (fv  [] (map (pair []) fms));
end;

val specialize = snd o strip_forall;

fun generalize fm = list_mk_forall (FV fm, fm);

(* ------------------------------------------------------------------------- *)
(* Subterms.                                                                 *)
(* ------------------------------------------------------------------------- *)

fun subterm [] tm = tm
  | subterm (_ :: _) (Var _) = raise ERR "subterm" "Var"
  | subterm (h :: t) (Fn (_, args)) =
  subterm t (List.nth (args, h))
  handle Subscript => raise ERR "subterm" "bad path";

local
  fun update _ _ [] = raise ERR "rewrite" "bad path"
    | update f n (h :: t) = if n = 0 then f h :: t else h :: update f (n - 1) t;
in
  fun rewrite ([] |-> res) _ = res
    | rewrite _ (Var _) = raise ERR "rewrite" "Var"
    | rewrite ((h :: t) |-> res) (Fn (f, args)) =
    Fn (f, update (rewrite (t |-> res)) h args);
end;

local
  fun atom_rewrite r = Atom o rewrite r o dest_atom;
in
  fun literal_subterm p = subterm p o dest_atom o literal_atom;
  fun literal_rewrite r = mk_literal o (I ## atom_rewrite r) o dest_literal;
end;

(* ------------------------------------------------------------------------- *)
(* The Knuth-Bendix ordering.                                                *)
(* ------------------------------------------------------------------------- *)

type Weight = string * int -> int;
type Prec   = (string * int) * (string * int) -> order;

val no_vars = Multiset.empty String.compare;
fun one_var v = Multiset.insert (v, 1) no_vars;

fun kb_weight w =
  let
    fun weight (Var v) = (0, one_var v)
      | weight (Fn (f, a)) = foldl wght (w (f, length a), no_vars) a
    and wght (t, (n, v)) = (curry op+ n ## Multiset.union v) (weight t)
  in
    weight
  end;

(* The Knuth-Bendix ordering is partial when terms contain variables *)

fun kb_compare w p =
  let
    fun kbo [] = SOME EQUAL
      | kbo (tu :: rest) =
      if op= tu then SOME EQUAL
      else
        let val ((wt, vt), (wu, vu)) = Df (kb_weight w) tu
        in kbo1 (Int.compare (wt, wu)) (Multiset.compare (vt, vu)) tu rest
        end
    and kbo1 _       NONE           _      _    = NONE
      | kbo1 LESS    (SOME LESS)    _      _    = SOME LESS
      | kbo1 GREATER (SOME LESS)    _      _    = NONE
      | kbo1 EQUAL   (SOME LESS)    _      _    = SOME LESS
      | kbo1 LESS    (SOME GREATER) _      _    = NONE
      | kbo1 GREATER (SOME GREATER) _      _    = SOME GREATER
      | kbo1 EQUAL   (SOME GREATER) _      _    = SOME GREATER
      | kbo1 LESS    (SOME EQUAL)   _      _    = SOME LESS
      | kbo1 GREATER (SOME EQUAL)   _      _    = SOME GREATER
      | kbo1 EQUAL   (SOME EQUAL)   (t, u) rest = kbo2 t u rest
    and kbo2 (Fn (f, a)) (Fn (g, b)) rest =
      (case p ((f, length a), (g, length b)) of LESS => SOME LESS
       | GREATER => SOME GREATER
       | EQUAL => kbo (zip a b @ rest))
      | kbo2 _ _ _ = raise BUG "kbo" "variable"
  in
    kbo o wrap
  end;

end
(*#line 0.0 "fol/Subst1.sig"*)
(* ========================================================================= *)
(* SUBSTITUTIONS ON FIRST-ORDER TERMS AND FORMULAS                           *)
(* Created by Joe Hurd, June 2002                                            *)
(* ========================================================================= *)

signature Subst1 =
sig

type 'a pp           = 'a Useful.pp
type ('a, 'b) maplet = ('a, 'b) Useful.maplet
type term            = Term1.term
type formula         = Term1.formula

type subst

val |<>|          : subst
val ::>           : (string, term) maplet * subst -> subst
val @>            : subst * subst -> subst
val null          : subst -> bool
val term_subst    : subst -> term -> term
val formula_subst : subst -> formula -> formula
val find_redex    : string -> subst -> term option
val norm          : subst -> subst       (* Removes identity substitutions *)
val restrict      : string list -> subst -> subst
val refine        : subst -> subst -> subst
val is_renaming   : subst -> bool
val to_maplets    : subst -> (string, term) maplet list
val from_maplets  : (string, term) maplet list -> subst
val foldl         : ((string, term) maplet -> 'a -> 'a) -> 'a -> subst -> 'a
val foldr         : ((string, term) maplet -> 'a -> 'a) -> 'a -> subst -> 'a
val pp_subst      : subst pp

end

(*#line 0.0 "fol/Subst1.sml"*)
(* ========================================================================= *)
(* SUBSTITUTIONS ON FIRST-ORDER TERMS AND FORMULAS                           *)
(* Created by Joe Hurd, June 2002                                            *)
(* ========================================================================= *)

(*
app load ["Binarymap", "Useful", "Term1"];
*)

(*
*)
structure Subst1 :> Subst1 =
struct

open Useful Term1;

infixr 8 ++;
infixr 7 >>;
infixr 6 ||;
infixr |-> ::> @> oo ##;

structure M = Binarymap;

(* ------------------------------------------------------------------------- *)
(* Helper functions.                                                         *)
(* ------------------------------------------------------------------------- *)

fun Mpurge (d, k) = fst (M.remove (d, k)) handle NotFound => d;

(* ------------------------------------------------------------------------- *)
(* The underlying representation.                                            *)
(* ------------------------------------------------------------------------- *)

datatype subst = Subst of (string, term) M.dict;

(* ------------------------------------------------------------------------- *)
(* Operations.                                                               *)
(* ------------------------------------------------------------------------- *)

val |<>| = Subst (M.mkDict String.compare);

fun (a |-> b) ::> (Subst d) = Subst (M.insert (d, a, b));

fun (Subst sub1) @> (Subst sub2) =
  Subst (M.foldl (fn (a, b, d) => M.insert (d, a, b)) sub2 sub1);

fun null (Subst s) = M.numItems s = 0;

fun find_redex r (Subst d) = M.peek (d, r);

fun purge v (Subst d) = Subst (Mpurge (d, v));

local
  exception Unchanged;

  fun always f x = f x handle Unchanged => x;

  fun pair_unchanged f (x, y) =
    let
      val (c, x) = (true, f x) handle Unchanged => (false, x)
      val (c, y) = (true, f y) handle Unchanged => (c, y)
    in
      if c then (x, y) else raise Unchanged
    end;

  fun list_unchanged f =
    let
      fun g (x, (b, l)) = (true, f x :: l) handle Unchanged => (b, x :: l)
      fun h (true, l) = rev l | h (false, _) = raise Unchanged
    in
      h o foldl g (false, [])
    end;

  fun find_unchanged v r =
    case find_redex v r of SOME t => t | NONE => raise Unchanged;

  fun tm_subst r =
    let
      fun f (Var v)     = find_unchanged v r
        | f (Fn (n, a)) = Fn (n, list_unchanged f a)
    in
      f
    end;

  fun fm_subst r =
    let
      fun f False       = raise Unchanged
        | f True        = raise Unchanged
        | f (Atom tm  ) = Atom (tm_subst r tm)
        | f (Not p    ) = Not (f p)
        | f (And pq   ) = And (pair_unchanged f pq)
        | f (Or  pq   ) = Or  (pair_unchanged f pq)
        | f (Imp pq   ) = Imp (pair_unchanged f pq)
        | f (Iff pq   ) = Iff (pair_unchanged f pq)
        | f (Forall vp) = fm_substq r Forall vp
        | f (Exists vp) = fm_substq r Exists vp
    in
      if null r then I else always f
    end
  and fm_substq r Q (v, p) =
    let val v' = variant v (FV (fm_subst (purge v r) p))
    in Q (v', fm_subst ((v |-> Var v') ::> r) p)
    end;
in
  fun term_subst    env tm = if null env then tm else always (tm_subst env) tm;
  fun formula_subst env fm = fm_subst env fm;
end;
  
fun norm (sub as Subst dict) =
  let
    fun check (a, b, (c, d)) =
      if Var a = b then (true, fst (M.remove (d, a))) else (c, d)
    val (removed, dict') = M.foldl check (false, dict) dict
  in
    if removed then Subst dict' else sub
  end;

fun to_maplets (Subst s) = map (op|->) (M.listItems s);

fun from_maplets ms = foldl (op ::>) |<>| (rev ms);

fun restrict vs =
  from_maplets o List.filter (fn (a |-> _) => mem a vs) o to_maplets;

(* Note: this just doesn't work with cyclic substitutions! *)
fun refine (Subst sub1) sub2 =
  let
    fun f ((a, b), s) =
      let val b' = term_subst sub2 b
      in if Var a = b' then s else (a |-> b') ::> s
      end
  in
    foldl f sub2 (M.listItems sub1)
  end;

local
  exception QF
  fun rs (v, Var w, l) = if mem w l then raise QF else w :: l
    | rs (_, Fn _, _) = raise QF
in
  fun is_renaming (Subst sub) = (M.foldl rs [] sub; true) handle QF => false;
end;

fun foldl f b (Subst sub) = M.foldl (fn (s, t, a) => f (s |-> t) a) b sub;

fun foldr f b (Subst sub) = M.foldr (fn (s, t, a) => f (s |-> t) a) b sub;

val pp_subst =
  pp_map to_maplets
  (fn pp =>
   (fn [] => pp_string pp "|<>|"
     | l => pp_list (pp_maplet pp_string pp_term) pp l));

end
(*#line 0.0 "fol/Kernel1.sig"*)
(* ========================================================================= *)
(* A LCF-STYLE LOGICAL KERNEL FOR FIRST-ORDER CLAUSES                        *)
(* Created by Joe Hurd, September 2001                                       *)
(* ========================================================================= *)

signature Kernel1 =
sig

type term    = Term1.term
type formula = Term1.formula
type subst   = Subst1.subst

(* An ABSTRACT type for theorems *)
eqtype thm
datatype inference = Axiom | Refl | Assume | Inst | Factor | Resolve | Equality

(* Destruction of theorems is fine *)
val dest_thm : thm -> formula list * (inference * thm list)

(* But creation is only allowed by the primitive rules of inference *)
val AXIOM    : formula list -> thm
val REFL     : term -> thm
val ASSUME   : formula -> thm
val INST     : subst -> thm -> thm
val FACTOR   : thm -> thm
val RESOLVE  : formula -> thm -> thm -> thm
val EQUALITY : formula -> int list -> term -> bool -> thm -> thm

end
(*#line 0.0 "fol/Kernel1.sml"*)
(* ========================================================================= *)
(* A LCF-STYLE LOGICAL KERNEL FOR FIRST-ORDER CLAUSES                        *)
(* Created by Joe Hurd, September 2001                                       *)
(* ========================================================================= *)

structure Kernel1 :> Kernel1 =
struct

open Useful Term1;

infixr |-> ::> oo;

type subst        = Subst1.subst;
val formula_subst = Subst1.formula_subst;

(* ------------------------------------------------------------------------- *)
(* An ABSTRACT type for theorems.                                            *)
(* ------------------------------------------------------------------------- *)

datatype inference = Axiom | Refl | Assume | Inst | Factor | Resolve | Equality;

datatype thm = Thm of formula list * (inference * thm list);

(* ------------------------------------------------------------------------- *)
(* Destruction of theorems is fine.                                          *)
(* ------------------------------------------------------------------------- *)

fun dest_thm (Thm th) = th;

val clause = fst o dest_thm;

(* ------------------------------------------------------------------------- *)
(* But creation is only allowed by the primitive rules of inference.         *)
(* ------------------------------------------------------------------------- *)

fun AXIOM cl =
  if List.all is_literal cl then Thm (cl, (Axiom, []))
  else raise ERR "AXIOM" "argument not a list of literals";

fun REFL tm = Thm ([mk_eq (tm, tm)], (Refl, []));

fun ASSUME fm =
  if is_literal fm then Thm ([fm, negate fm], (Assume, []))
  else raise ERR "ASSUME" "argument not a literal";

fun INST env (th as Thm (cl, pr)) =
  let
    val cl' = map (formula_subst env) cl
  in
    if cl' = cl then th else
      case pr of (Inst, [th'])
        => if cl' = clause th' then th' else Thm (cl', (Inst, [th']))
      | _ => Thm (cl', (Inst, [th]))
  end;

fun FACTOR th =
  let val cl = rev (setify (clause th))
  in if cl = clause th then th else Thm (cl, (Factor, [th]))
  end;

fun RESOLVE fm th1 th2 =
  let
    val cl1  = clause th1
    val cl1' = List.filter (not o equal fm) cl1
    val cl2  = clause th2
    val cl2' = List.filter (not o equal (negate fm)) cl2
    val ()   =
      assert (cl1 <> cl1' orelse cl2 <> cl2')
      (ERR "RESOLVE" "resolvant does not feature in either clause")
  in
    Thm (cl1' @ cl2', (Resolve, [th1, th2]))
  end;

fun EQUALITY fm p res lr th =
  let
    val eq_lit =
      let
        val red = literal_subterm p fm
      in
        Not (mk_eq (if lr then (red, res) else (res, red)))
      end
    val other_lits =
      let
        val l = clause th
      in
        case index (equal fm) l of NONE
          => raise ERR "EQUALITY" "literal does not occur in clause"
        | SOME n => update_nth (literal_rewrite (p |-> res)) n l
      end
  in
    Thm (eq_lit :: other_lits, (Equality, [th]))
  end;

end
(*#line 0.0 "fol/Match1.sig"*)
(* ========================================================================= *)
(* MATCHING AND UNIFICATION                                                  *)
(* Created by Joe Hurd, September 2001                                       *)
(* ========================================================================= *)

signature Match1 =
sig

type term    = Term1.term
type formula = Term1.formula
type subst   = Subst1.subst

(* Matching *)
val matchl          : subst -> (term * term) list -> subst
val match           : term -> term -> subst
val matchl_literals : subst -> (formula * formula) list -> subst
val match_literals  : formula -> formula -> subst

(* Unification *)
val unifyl          : subst -> (term * term) list -> subst
val unify           : subst -> term -> term -> subst
val unify_and_apply : term -> term -> term
val unifyl_literals : subst -> (formula * formula) list -> subst
val unify_literals  : subst -> formula -> formula -> subst

end
(*#line 0.0 "fol/Match1.sml"*)
(* ========================================================================= *)
(* MATCHING AND UNIFICATION                                                  *)
(* Created by Joe Hurd, September 2001                                       *)
(* ========================================================================= *)

(*
app load ["Useful", "Mosml", "Term1"];
*)

(*
*)
structure Match1 :> Match1 =
struct

open Useful Term1;

infixr |-> ::>;

type subst        = Subst1.subst;
val |<>|          = Subst1.|<>|;
val op ::>        = Subst1.::>;
val term_subst    = Subst1.term_subst;
val formula_subst = Subst1.formula_subst;

(* ------------------------------------------------------------------------- *)
(* Matching.                                                                 *)
(* ------------------------------------------------------------------------- *)

fun raw_match env x tm =
  (case Subst1.find_redex x env of NONE => (x |-> tm) ::> env
   | SOME tm' =>
     if tm = tm' then env
     else raise ERR "match" "one var trying to match two different terms");

fun matchl env [] = env
  | matchl env ((Var x, tm) :: rest) = matchl (raw_match env x tm) rest
  | matchl env ((Fn (f, args), Fn (f', args')) :: rest) =
  if f = f' andalso length args = length args' then
    matchl env (zip args args' @ rest)
  else raise ERR "match" "can't match two different functions"
  | matchl _ _ = raise ERR "match" "different structure";

fun match tm tm' = Subst1.norm (matchl |<>| [(tm, tm')]);

local
  fun conv (Atom t, Atom t') = SOME (t, t')
    | conv (Not p,  Not q)   = conv (p, q)
    | conv (True,   True)    = NONE
    | conv (False,  False)   = NONE
    | conv _                 = raise ERR "match_literals" "incompatible";
in
  fun matchl_literals sub = matchl sub o List.mapPartial conv;
end;

fun match_literals lit lit' = Subst1.norm (matchl_literals |<>| [(lit, lit')]);

(* ------------------------------------------------------------------------- *)
(* Unification.                                                              *)
(* ------------------------------------------------------------------------- *)

local
  fun occurs v tm = mem v (FVT tm);

  fun solve env [] = env
    | solve env ((tm1, tm2) :: rest) =
    solve' env (term_subst env tm1) (term_subst env tm2) rest
  and solve' env (Var x) tm rest =
    if Var x = tm then solve env rest
    else if occurs x tm then raise ERR "unify" "occurs check"
    else
      (case Subst1.find_redex x env of NONE
         => solve (Subst1.refine env ((x |-> tm) ::> |<>|)) rest
       | SOME tm' => solve' env tm' tm rest)
    | solve' env tm (tm' as Var _) rest = solve' env tm' tm rest
    | solve' env (Fn (f, args)) (Fn (f', args')) rest =
    if f = f' andalso length args = length args' then
      solve env (zip args args' @ rest)
    else raise ERR "unify" "different structure";
in
  val unifyl = solve;
end;

fun unify env tm tm' = unifyl env [(tm, tm')];

fun unify_and_apply tm tm' = term_subst (unify |<>| tm tm') tm;

local
  fun conv (Atom t, Atom t') = SOME (t, t')
    | conv (Not p,  Not q)   = conv (p, q)
    | conv (True,   True)    = NONE
    | conv (False,  False)   = NONE
    | conv _                 = raise ERR "unify_literals" "incompatible";
in
  fun unifyl_literals env = unifyl env o List.mapPartial conv;
end;

fun unify_literals env lit lit' = unifyl_literals env [(lit, lit')];

end
(*#line 0.0 "fol/TermNet1.sig"*)
(* ========================================================================= *)
(* MATCHING AND UNIFICATION FOR SETS OF TERMS                                *)
(* Created by Joe Hurd, September 2001                                       *)
(* ========================================================================= *)

signature TermNet1 =
sig

type 'a pp           = 'a Useful.pp
type ('a, 'b) maplet = ('a, 'b) Useful.maplet
type term            = Term1.term

type 'a term_map

val empty        : 'a term_map
val insert       : (term, 'a) maplet -> 'a term_map -> 'a term_map
val match        : 'a term_map -> term -> 'a list
val matched      : 'a term_map -> term -> 'a list
val unify        : 'a term_map -> term -> 'a list
val size         : 'a term_map -> int
val from_maplets : (term, 'a) maplet list -> 'a term_map
val to_list      : 'a term_map -> 'a list
val pp_term_map  : 'a pp -> 'a term_map pp

end
(*#line 0.0 "fol/TermNet1.sml"*)
(* ========================================================================= *)
(* MATCHING AND UNIFICATION FOR SETS OF TERMS                                *)
(* Created by Joe Hurd, September 2001                                       *)
(* ========================================================================= *)

(*
app load ["Useful", "Mosml", "Term1"];
*)

(*
*)
structure TermNet1 :> TermNet1 =
struct

open Useful Term1;

infixr |-> ::> oo;

val flatten = List.concat;

(* ------------------------------------------------------------------------- *)
(* Helper functions.                                                         *)
(* ------------------------------------------------------------------------- *)

local
  fun fifo_order (m, _) (n, _) = m <= n;
in
  fun restore_fifo_order l = map snd (sort fifo_order l);
end;

fun partition_find f l =
  let
    fun pf _ [] = (NONE, l)
      | pf dealt (x :: xs) =
      if f x then (SOME x, List.revAppend (dealt, xs)) else pf (x :: dealt) xs
  in
    pf [] l
  end;

(* ------------------------------------------------------------------------- *)
(* Term discrimination trees are optimized for match queries.                *)
(* ------------------------------------------------------------------------- *)

datatype pattern = VAR | FN of string * int;

type 'a map = (pattern, 'a) tree;

datatype 'a term_map = MAP of int * (int * 'a) map list;

val empty = MAP (0, []);

fun size (MAP (i, _)) = i;

fun to_list (MAP (_, n)) =
  restore_fifo_order (flatten (map (tree_foldr (K flatten) wrap) n));

fun pp_term_map pp_a = pp_map to_list (pp_list pp_a);

local
  fun find_pat x (BRANCH (p, _)) = p = x
    | find_pat _ (LEAF _) = raise BUG "find_pat" "misplaced LEAF";

  fun add a [] l = LEAF a :: l
    | add a (tm :: rest) l =
    let
      val (pat, rest) =
        case tm of Var _ => (VAR, rest)
        | Fn (f, args) => (FN (f, length args), args @ rest)
      val (this, others) = partition_find (find_pat pat) l
      val next =
        case this of NONE => []
        | SOME (BRANCH (_, l)) => l
        | SOME (LEAF _) => raise BUG "add" "misplaced LEAF"
    in
      BRANCH (pat, add a rest next) :: others
    end;
in
  fun insert (tm |-> a) (MAP (i, n)) = MAP (i + 1, add (i, a) [tm] n)
  handle ERR_EXN _ => raise BUG "insert" "should never fail";
end;

fun from_maplets l = foldl (uncurry insert) empty l;

local
  fun mat VAR (_ :: rest) = SOME rest
    | mat (FN (f, n)) (Fn (g, args) :: rest) =
    if f = g andalso n = length args then SOME (args @ rest) else NONE
    | mat (FN _) (Var _ :: _) = NONE
    | mat _ [] = raise BUG "match" "ran out of subterms";

  fun final a [] = SOME a
    | final _ (_ :: _) = raise BUG "match" "too many subterms";
in
  fun match (MAP (_, n)) tm =
    restore_fifo_order (flatten (map (tree_partial_foldl mat final [tm]) n))
  handle ERR_EXN _ => raise BUG "match" "should never fail";
end;

local
  fun more VAR = 0 | more (FN (f, n)) = n;
  fun mat pat (0, Var _ :: rest) = SOME (more pat, rest)
    | mat VAR (0, Fn _ :: _) = NONE
    | mat (FN (f, n)) (0, Fn (g, args) :: rest) =
    if f = g andalso n = length args then SOME (0, args @ rest) else NONE
    | mat _ (0, []) = raise BUG "matched" "ran out of subterms"
    | mat pat (n, rest) = SOME (more pat + n - 1, rest);

  fun final a (0, []) = SOME a
    | final _ (0, _ :: _) = raise BUG "matched" "too many subterms"
    | final _ (n, _) = raise BUG "matched" "still skipping";
in
  fun matched (MAP (_, n)) tm =
    restore_fifo_order (flatten (map (tree_partial_foldl mat final (0,[tm])) n))
  handle ERR_EXN _ => raise BUG "matched" "should never fail";
end;

local
  fun more VAR = 0 | more (FN (f, n)) = n;
  fun mat pat (0, Var _ :: rest) = SOME (more pat, rest)
    | mat VAR (0, Fn _ :: rest) = SOME (0, rest)
    | mat (FN (f, n)) (0, Fn (g, args) :: rest) =
    if f = g andalso n = length args then SOME (0, args @ rest) else NONE
    | mat _ (0, []) = raise BUG "unify" "ran out of subterms"
    | mat pat (n, rest) = SOME (more pat + n - 1, rest);

  fun final a (0, []) = SOME a
    | final _ (0, _ :: _) = raise BUG "unify" "too many subterms"
    | final _ (n, _) = raise BUG "unify" "still skipping";
in
  fun unify (MAP (_, n)) tm =
    restore_fifo_order (flatten (map (tree_partial_foldl mat final (0,[tm])) n))
  handle ERR_EXN _ => raise BUG "unify" "should never fail";
end;

(* ------------------------------------------------------------------------- *)
(* We can overlay the above type with a simple list type.                    *)
(* ------------------------------------------------------------------------- *)
(*
type 'a simple = int * int * term list * 'a list;

type 'a term_map = ('a simple, 'a term_map) sum;

fun check (0, _, t, a) =
  INR (from_maplets (foldl (fn (x, xs) => op|-> x :: xs) [] (zip t a)))
  | check p = INL p;

val empty : 'a term_map = INR empty;

fun new n = check (n, 0, [], []);

val insert = fn m =>
  (fn INL (n, s, ts, xs) =>
      (case m of t |-> x => check (n - 1, s + 1, t :: ts, x :: xs))
    | INR d => INR (insert m d));

val match = fn INL (_, _, _, xs) => K (rev xs) | INR d => match d;

val matched = fn INL (_, _, _, xs) => K (rev xs) | INR d => matched d;

val unify = fn INL (_, _, _, xs) => K (rev xs) | INR d => unify d;

val size = fn INL (_, s, _, _) => s | INR d => size d;

val from_maplets = INR o from_maplets;

val to_list = fn INL (_, _, _, xs) => rev xs | INR d => to_list d;

val pp_term_map =
  fn pp_a => fn pp =>
  (fn INL (_, _, _, xs) => pp_list pp_a pp xs | INR d => pp_term_map pp_a pp d);
*)

end
(*#line 0.0 "fol/LiteralNet1.sig"*)
(* ========================================================================= *)
(* MATCHING AND UNIFICATION FOR SETS OF LITERALS                             *)
(* Created by Joe Hurd, June 2002                                            *)
(* ========================================================================= *)

signature LiteralNet1 =
sig

type 'a pp           = 'a Useful.pp
type formula         = Term1.formula
type ('a, 'b) maplet = ('a, 'b) Term1.maplet

type 'a literal_map

val empty          : 'a literal_map
val insert         : (formula, 'a) maplet -> 'a literal_map -> 'a literal_map
val match          : 'a literal_map -> formula -> 'a list
val matched        : 'a literal_map -> formula -> 'a list
val unify          : 'a literal_map -> formula -> 'a list
val size           : 'a literal_map -> int
val size_profile   : 'a literal_map -> {t : int, f : int, p : int, n : int}
val from_maplets   : (formula, 'a) maplet list -> 'a literal_map
val to_list        : 'a literal_map -> 'a list
val pp_literal_map : 'a pp -> 'a literal_map pp

end
(*#line 0.0 "fol/LiteralNet1.sml"*)
(* ========================================================================= *)
(* MATCHING AND UNIFICATION FOR SETS OF LITERALS                             *)
(* Created by Joe Hurd, June 2002                                            *)
(* ========================================================================= *)

(*
app load ["Useful", "Mosml", "Term1"];
*)

(*
*)
structure LiteralNet1 :> LiteralNet1 =
struct

open Useful Term1;

infixr |-> ::> oo;

structure T = TermNet1;

(* ------------------------------------------------------------------------- *)
(* Literal nets.                                                             *)
(* ------------------------------------------------------------------------- *)

type 'a literal_map =
  ('a T.term_map * 'a T.term_map) * ((int * 'a list) * (int * 'a list));

val empty = ((T.empty, T.empty), ((0, []), (0, [])));

fun insert (Atom a |-> b) ((p, n), tf)       = ((T.insert (a |-> b) p, n), tf)
  | insert (Not (Atom a) |-> b) ((p, n), tf) = ((p, T.insert (a |-> b) n), tf)
  | insert (True |-> b) (pn, ((n, l), f))    = (pn, ((n + 1, b :: l), f))
  | insert (False |-> b) (pn, (t, (n, l)))   = (pn, (t, (n + 1, b :: l)))
  | insert (f |-> _) _ = raise BUG "insert" ("not a lit: "^formula_to_string f);

fun from_maplets l = foldl (uncurry insert) empty l;

fun to_list ((pos, neg), ((_, t), (_, f))) =
  rev t @ rev f @ T.to_list pos @ T.to_list neg;

fun pp_literal_map pp_a = pp_map to_list (pp_list pp_a);

local
  fun pos     ((pos, _  ), _               ) = T.size pos;
  fun neg     ((_,   neg), _               ) = T.size neg;
  fun truth   (_,          ((n, _), _     )) = n;
  fun falsity (_,          (_,      (n, _))) = n;
in
  fun size l         = truth l + falsity l + pos l + neg l;
  fun size_profile l = {t = truth l, f = falsity l, p = pos l, n = neg l};
end;

fun match ((pos, _), _) (Atom a) = T.match pos a
  | match ((_, neg), _) (Not (Atom a)) = T.match neg a
  | match (_, ((_, t), _)) True = rev t
  | match (_, (_, (_, f))) False = rev f
  | match _ _ = raise BUG "match" "not a literal";

fun matched ((pos, _), _) (Atom a) = T.matched pos a
  | matched ((_, neg), _) (Not (Atom a)) = T.matched neg a
  | matched (_, ((_, t), _)) True = rev t
  | matched (_, (_, (_, f))) False = rev f
  | matched _ _ = raise BUG "matched" "not a literal";

fun unify ((pos, _), _) (Atom a) = T.unify pos a
  | unify ((_, neg), _) (Not (Atom a)) = T.unify neg a
  | unify (_, ((_, t), _)) True = rev t
  | unify (_, (_, (_, f))) False = rev f
  | unify _ _ = raise BUG "unify" "not a literal";

end
(*#line 0.0 "fol/Subsume1.sig"*)
(* ========================================================================= *)
(* A TYPE FOR SUBSUMPTION CHECKING                                           *)
(* Created by Joe Hurd, April 2002                                           *)
(* ========================================================================= *)

signature Subsume1 =
sig

type 'a pp           = 'a Useful.pp
type ('a, 'b) maplet = ('a, 'b) Useful.maplet
type formula         = Term1.formula
type subst           = Subst1.subst

type 'a subsume

val empty             : 'a subsume
val add               : (formula list, 'a) maplet -> 'a subsume -> 'a subsume
val subsumed          : 'a subsume -> formula list -> (subst * 'a) list
val strictly_subsumed : 'a subsume -> formula list -> (subst * 'a) list
val info              : 'a subsume -> string
val pp_subsum         : 'a subsume pp

end
(*#line 0.0 "fol/Subsume1.sml"*)
(* ========================================================================= *)
(* A TYPE FOR SUBSUMPTION CHECKING                                           *)
(* Created by Joe Hurd, April 2002                                           *)
(* ========================================================================= *)

(*
app load ["Thm1", "Match1"];
*)

(*
*)
structure Subsume1 :> Subsume1 =
struct

infix |-> ::>;

open Useful Term1 Match1;

structure N = LiteralNet1;

val ofilter       = Option.filter;
type subst        = Subst1.subst;
val |<>|          = Subst1.|<>|;
val op ::>        = Subst1.::>;
val term_subst    = Subst1.term_subst;
val formula_subst = Subst1.formula_subst;

(* ------------------------------------------------------------------------- *)
(* Chatting.                                                                 *)
(* ------------------------------------------------------------------------- *)

val () = traces := {module = "Subsume1", alignment = K 1} :: !traces;

fun chat l m = trace {module = "Subsume1", message = m, level = l};

(* ------------------------------------------------------------------------- *)
(* Helper functions.                                                         *)
(* ------------------------------------------------------------------------- *)

val frozen_prefix = "FROZEN__";

fun mk_frozen v = Fn (frozen_prefix ^ v, []);

local
  val chk = String.isPrefix frozen_prefix;
  val dest =
    let val l = size frozen_prefix in fn s => String.extract (s, l, NONE) end;
in
  fun dest_frozen (Fn (s, [])) =
    (assert (chk s) (ERR "dest_frozen" "not a frozen var"); dest s)
    | dest_frozen _ = raise ERR "dest_frozen" "bad structure";
end;

val is_frozen = can dest_frozen;

fun freeze_vars fms =
  let
    val vars = FV (list_mk_disj fms)
    val sub = foldl (fn (v, s) => (v |-> mk_frozen v) ::> s) |<>| vars
  in
    map (formula_subst sub) fms
  end;

local
  fun f (v |-> a) = (v |-> (if is_frozen a then Var (dest_frozen a) else a));
in
  val defrost_vars = Subst1.from_maplets o map f o Subst1.to_maplets;
end;

val lit_size = formula_size o literal_atom;

val sort_literals_by_size =
  map snd o sort (fn (m, _) => fn (n, _) => m <= n) o
  map (fn lit => (lit_size lit, lit));

(* ------------------------------------------------------------------------- *)
(* The core engine for subsumption checking.                                 *)
(* ------------------------------------------------------------------------- *)

type 'a sinfo = {sub : subst, hd : formula, tl : formula list, fin : 'a};

type 'a subs = 'a sinfo N.literal_map;

fun add_lits (i as {hd, ...}) (net : 'a subs) = N.insert (hd |-> i) net;

local
  fun subsum strict lits =
    let
      val accept =
        (if strict then ofilter (non Subst1.is_renaming) else SOME) o
        defrost_vars
      val impossible =
        let val lit_net = N.from_maplets (map (fn l => (l |-> ())) lits)
        in List.exists (null o N.matched lit_net)
        end
      fun extend sub lits fin net =
        if impossible lits then net
        else
          case sort_literals_by_size lits of [] => raise BUG "extend" "null"
          | hd :: tl => add_lits {sub = sub, hd = hd, tl = tl, fin = fin} net
      fun examine lit ({sub, hd, tl, fin}, s as (net, res)) =
        case total (matchl_literals sub) [(hd, lit)] of NONE => s
        | SOME sub =>
          if null tl then
            case accept sub of SOME sub => (net, (sub, fin) :: res) | NONE => s
          else (extend sub (map (formula_subst sub) tl) fin net, res)
      fun narrow1 net (lit, s) = foldl (examine lit) s (N.match net lit)
      fun narrow (net, res) =
        if N.size net = 0 then res
        else narrow (foldl (narrow1 net) (N.empty, res) lits)
    in
      narrow
    end;
in
  fun subsumes strict net lits =
    subsum strict (freeze_vars lits) (net, [])
    handle ERR_EXN _ => raise BUG "subsumes" "shouldn't fail";
end;

(* ------------------------------------------------------------------------- *)
(* The user interface.                                                       *)
(* ------------------------------------------------------------------------- *)

type 'a subsume = ('a, 'a subs) sum;

val empty : 'a subsume = INR N.empty;

fun add _ (s as INL _) = s
  | add (fms |-> fin) (INR net) =
  case sort_literals_by_size fms of [] => INL fin
  | h :: t => INR (add_lits {sub = |<>|, hd = h, tl = t, fin = fin} net);

fun subsumed (INL fin) _ = [(|<>|, fin)]
  | subsumed (INR _) [] = []
  | subsumed (INR net) lits = subsumes false net lits;

fun strictly_subsumed _ [] = []
  | strictly_subsumed (INL fin) _ = [(|<>|, fin)]
  | strictly_subsumed (INR net) lits = subsumes true net lits;

fun info ((INL _) : 'a subsume) = "*"
  | info (INR net) = int_to_string (N.size net);

val pp_subsum = fn z => pp_map info pp_string z;

(* Quick testing
quotation := true;
installPP pp_formula;
installPP pp_term;
installPP pp_subst;
installPP pp_thm;
freeze_vars (map parse [`x + y <= 0`, `x = __x()`]);
val s = add_subsumer (AXIOM (map parse [`p(x,3)`, `p(2,y)`])) empty_subsum;
subsumed s (map parse [`p(2,3)`]);
*)

end
(*#line 0.0 "fol/Tptp1.sig"*)
(* ========================================================================= *)
(* INTERFACE TO TPTP PROBLEM FILES                                           *)
(* Created by Joe Hurd, December 2001                                        *)
(* ========================================================================= *)

signature Tptp1 =
sig

type term    = Term1.term
type formula = Term1.formula

(* Maintaining different relation and function names in TPTP problems *)
val renaming : {tptp : string, fol : string, arity : int} list ref

(* Parsing: pass in a filename *)
val parse_cnf : string -> formula

end
(*#line 0.0 "fol/Tptp1.sml"*)
(* ========================================================================= *)
(* INTERFACE TO TPTP PROBLEM FILES                                           *)
(* Created by Joe Hurd, December 2001                                        *)
(* ========================================================================= *)

(*
app load ["Stream", "Useful", "Parser", "Term1"];
*)

(*
*)
structure Tptp1 :> Tptp1 =
struct

open Parser Useful Term1;

infixr 9 >>++;
infixr 8 ++;
infixr 7 >>;
infixr 6 ||;
infix |->;

structure S = Stream;

(* ------------------------------------------------------------------------- *)
(* Abbreviating relation and function names in TPTP problems.                *)
(* ------------------------------------------------------------------------- *)

type rename = {tptp : string, fol : string, arity : int};

val renaming : rename list ref = ref [{tptp = "equal", fol = "=", arity = 2}];

(* ------------------------------------------------------------------------- *)
(* Parsing: pass in a filename.                                              *)
(* ------------------------------------------------------------------------- *)

val comment = equal #"%" o hd o explode;

val input_lines = S.filter (non comment) o S.from_textfile;

val input_chars = S.flatten o S.map (S.from_list o explode);

datatype tok_type = Lower | Upper | Symbol | Punct;

val lexer =
  (many (some space) ++
   (((some lower || some digit) ++ many (some alphanum) >>
     (fn (a, b) => (Lower, implode (a :: b)))) ||
    (some upper ++ many (some alphanum) >>
     (fn (a, b) => (Upper, implode (a :: b)))) ||
    (atleastone (some symbol) >> (fn l => (Symbol, implode l))) ||
    (some punct >> (fn c => (Punct, str c))))) >> snd;

val lex = many lexer ++ (many (some space) ++ finished) >> fst;

val input_toks = S.from_list o fst o lex;

fun Var' "T" = Var "T'"
  | Var' "F" = Var "F'"
  | Var' v = Var (if !var_string v then v else "v_" ^ v);

local
  fun verify (f, a) =
    (if !var_string f then (if null a then "c_" else "f_") ^ f else f, a);
  fun mapped (f, a) (m : rename list) =
    let
       fun g {tptp, arity, fol = _} = tptp = f andalso arity = length a
    in case List.find g m of SOME {fol, ...} => (fol, a) | NONE => verify (f, a)
    end;
in
  fun Fn'   A = Fn   (mapped A (!renaming));
end;

fun term_parser input =
  ((some (equal Upper o fst) >> (Var' o snd)) ||
   ((some (equal Lower o fst) >> snd) ++
    (optional
     (exact (Punct, "(") ++ term_parser ++
      many ((exact (Punct, ",") ++ term_parser) >> snd) ++
      exact (Punct, ")")) >>
     (fn SOME (_, (t, (ts, _))) => t :: ts | NONE => [])) >>
    Fn')) input;

val literal_parser =
  ((exact (Symbol, "++") >> K true || exact (Symbol, "--") >> K false) ++
   term_parser) >>
  (fn (s, t) => mk_literal (s, Atom (case t of Var v => Fn (v, []) | _ => t)));

val clause_parser =
  (exact (Lower, "input_clause") ++ exact (Punct, "(") ++ any ++
   exact (Punct, ",") ++ any ++ exact (Punct, ",") ++ exact (Punct, "[") ++
   literal_parser ++ many ((exact (Punct, ",") ++ literal_parser) >> snd) ++
   exact (Punct, "]") ++ exact (Punct, ")") ++ exact (Punct, ".")) >>
  (fn (_, (_, (name, (_, (typ, (_, (_, (l, (ls, _))))))))) =>
   (snd name, snd typ, l :: ls));

val cnf_parser = fst o ((many clause_parser ++ finished) >> fst);

local
  fun cycle _ _ ([], _) = raise BUG "cycle" ""
    | cycle f v (h :: t, avoid) =
    let val h' = f h avoid in (h', (t @ [h], h' :: avoid)) end;
in    
  fun generalize_clause fm =
    let
      open Subst1
      val vars = FV fm
      val nvars = length vars
      val var_fn = if nvars <= 15 then variant else variant_num
      val news =
        if nvars = 6 then ["x", "y", "z", "x'", "y'", "z'"]
        else fst (maps (cycle var_fn) vars (["x", "y", "z", "v", "w"], []))
      val sub = from_maplets (zipwith (fn v => fn x => v |-> Var x) vars news)
    in
      generalize (formula_subst sub fm)
    end;
end;

val input_cnf =
  (fn (a, b) => Imp (a, Imp (b, False))) o
  Df (list_mk_conj o map (generalize_clause o list_mk_disj o #3)) o
  List.partition (not o equal "conjecture" o #2) o cnf_parser;

val parse_cnf = input_cnf o input_toks o input_chars o input_lines;

end
(*#line 0.0 "fol/Thm1.sig"*)
(* ========================================================================= *)
(* INTERFACE TO THE LCF-STYLE LOGICAL KERNEL, PLUS SOME DERIVED RULES        *)
(* Created by Joe Hurd, September 2001                                       *)
(* ========================================================================= *)

signature Thm1 =
sig

type 'a pp = 'a Useful.pp

include Kernel1

(* Annotated primitive inferences *)
datatype inference' =
  Axiom'    of formula list
| Refl'     of term
| Assume'   of formula
| Inst'     of subst * thm
| Factor'   of thm
| Resolve'  of formula * thm * thm
| Equality' of formula * int list * term * bool * thm

val primitive_inference : inference' -> thm

(* User-friendly destructors *)
val clause    : thm -> formula list
val inference : thm -> inference'
val proof     : thm -> (thm * inference') list

(* Pretty-printing of theorems and inferences *)
val pp_thm               : thm pp
val pp_inference         : inference pp
val pp_inference'        : inference' pp
val pp_proof             : (thm * inference') list pp
val thm_to_string'       : int -> thm -> string        (* purely functional *)
val inference_to_string' : int -> inference' -> string
val thm_to_string        : thm -> string               (* using !LINE_LENGTH *)
val inference_to_string  : inference' -> string

(* A total comparison function for theorems *)
val thm_compare : thm * thm -> order

(* Contradictions and unit clauses *)
val is_contradiction : thm -> bool
val dest_unit        : thm -> formula
val is_unit          : thm -> bool

(* Derived rules and theorems *)
val CONTR          : formula -> thm -> thm
val WEAKEN         : formula list -> thm -> thm
val FRESH_VARS     : thm -> thm
val FRESH_VARSL    : thm list -> thm list
val UNIT_SQUASH    : thm -> thm
val REFLEXIVITY    : thm
val SYMMETRY       : thm
val TRANSITIVITY   : thm
val FUN_CONGRUENCE : string * int -> thm
val REL_CONGRUENCE : string * int -> thm

end
(*#line 0.0 "fol/Thm1.sml"*)
(* ========================================================================= *)
(* INTERFACE TO THE LCF-STYLE LOGICAL KERNEL, PLUS SOME DERIVED RULES        *)
(* Created by Joe Hurd, September 2001                                       *)
(* ========================================================================= *)

(*
app load ["Useful", "Term1", "Kernel1", "Match1"];
*)

(*
*)
structure Thm1 :> Thm1 =
struct

open Useful Term1 Kernel1 Match1;

infixr |-> ::> oo ##;

type subst        = Subst1.subst;
val |<>|          = Subst1.|<>|;
val op ::>        = Subst1.::>;
val term_subst    = Subst1.term_subst;
val formula_subst = Subst1.formula_subst;
val pp_subst      = Subst1.pp_subst;

(* ------------------------------------------------------------------------- *)
(* Annotated primitive inferences.                                           *)
(* ------------------------------------------------------------------------- *)

datatype inference' =
  Axiom'    of formula list
| Refl'     of term
| Assume'   of formula
| Inst'     of subst * thm
| Factor'   of thm
| Resolve'  of formula * thm * thm
| Equality' of formula * int list * term * bool * thm

fun primitive_inference (Axiom'    cl              ) = AXIOM cl
  | primitive_inference (Refl'     tm              ) = REFL tm
  | primitive_inference (Assume'   l               ) = ASSUME l
  | primitive_inference (Inst'     (s, th)         ) = INST s th
  | primitive_inference (Factor'   th              ) = FACTOR th
  | primitive_inference (Resolve'  (l, th1, th2)   ) = RESOLVE l th1 th2
  | primitive_inference (Equality' (l, p, t, s, th)) = EQUALITY l p t s th;

val clause = fst o dest_thm;

(* ------------------------------------------------------------------------- *)
(* Pretty-printing of theorems                                               *)
(* ------------------------------------------------------------------------- *)

fun pp_thm pp th =
  (PP.begin_block pp PP.INCONSISTENT 3;
   PP.add_string pp "|- ";
   pp_formula pp (list_mk_disj (clause th));
   PP.end_block pp);

local
  fun inf_to_string Axiom    = "Axiom"
    | inf_to_string Refl     = "Refl"
    | inf_to_string Assume   = "Assume"
    | inf_to_string Inst     = "Inst"
    | inf_to_string Factor   = "Factor"
    | inf_to_string Resolve  = "Resolve"
    | inf_to_string Equality = "Equality";
in
  val pp_inference = pp_map inf_to_string pp_string;
end;

local
  fun pp_inf (Axiom'    a) = (Axiom,   C (pp_list pp_formula)                 a)
    | pp_inf (Refl'     a) = (Refl,    C pp_term                              a)
    | pp_inf (Assume'   a) = (Assume,  C pp_formula                           a)
    | pp_inf (Inst'     a) = (Inst,    C (pp_pair pp_subst pp_thm)            a)
    | pp_inf (Factor'   a) = (Factor,  C pp_thm                               a)
    | pp_inf (Resolve'  a) = (Resolve, C (pp_triple pp_formula pp_thm pp_thm) a)
    | pp_inf (Equality' (lit, p, r, lr, th)) =
    (Equality,
     C (pp_record [("lit",  unit_pp pp_formula lit),
                   ("path", unit_pp (pp_list pp_int) p),
                   ("res",  unit_pp pp_term r),
                   ("lr",   unit_pp pp_bool lr),
                   ("thm",  unit_pp pp_thm th)]) ());
in
  fun pp_inference' pp inf =
  let
    open PP
    val (i, ppf) = pp_inf inf
  in
    (begin_block pp INCONSISTENT 0;
     pp_inference pp i;
     add_break pp (1, 0);
     ppf pp;
     end_block pp)
  end;
end;

val pp_proof = pp_list (pp_pair pp_thm pp_inference');

(* Purely functional pretty-printing *)

fun thm_to_string'       len = PP.pp_to_string len pp_thm;
fun inference_to_string' len = PP.pp_to_string len pp_inference';

(* Pretty-printing using !LINE_LENGTH *)

fun thm_to_string       th  = thm_to_string'       (!LINE_LENGTH) th;
fun inference_to_string inf = inference_to_string' (!LINE_LENGTH) inf;

(* ------------------------------------------------------------------------- *)
(* A total comparison function for theorems.                                 *)
(* ------------------------------------------------------------------------- *)

local
  fun cmp Axiom    Axiom    = EQUAL
    | cmp Axiom    _        = LESS
    | cmp _        Axiom    = GREATER
    | cmp Refl     Refl     = EQUAL
    | cmp Refl     _        = LESS
    | cmp _        Refl     = GREATER
    | cmp Assume   Assume   = EQUAL
    | cmp Assume   _        = LESS
    | cmp _        Assume   = GREATER
    | cmp Inst     Inst     = EQUAL
    | cmp Inst     _        = LESS
    | cmp _        Inst     = GREATER
    | cmp Factor   Factor   = EQUAL
    | cmp Factor   _        = LESS
    | cmp _        Factor   = GREATER
    | cmp Resolve  Resolve  = EQUAL
    | cmp Resolve  Equality = LESS
    | cmp Equality Resolve  = GREATER
    | cmp Equality Equality = EQUAL;

  fun cm [] = EQUAL
    | cm ((th1, th2) :: l) =
    let
      val (l1, (p1, ths1)) = dest_thm th1
      val (l2, (p2, ths2)) = dest_thm th2
    in
      case Int.compare (length l1, length l2) of EQUAL
        => (case lex_compare formula_compare (zip l1 l2) of EQUAL
              => (case cmp p1 p2 of EQUAL
                    => cm (zip ths1 ths2 @ l)
                  | x => x)
            | x => x)
      | x => x
    end
in
  val thm_compare = cm o wrap;
end;

(* ------------------------------------------------------------------------- *)
(* Reconstructing proofs.                                                    *)
(* ------------------------------------------------------------------------- *)

fun reconstruct_resolvant cl1 cl2 (cl1', cl2') =
  case (subtract (setify cl1) cl1', subtract (setify cl2) cl2') of
    (_ :: _ :: _, _) => NONE
  | (_, _ :: _ :: _) => NONE
  | ([l], []) => SOME l
  | ([], [l']) => SOME (negate l')
  | ([l], [l']) => if negate l = l' then SOME l else NONE
  | ([], []) => NONE;

fun reconstruct_equality l r =
  let
    fun recon_fn p (f, args) (f', args') rest =
      recon_tm
      (if f <> f' orelse length args <> length args' then rest
       else map (C cons p ## I) (enumerate 0 (zip args args')) @ rest)
    and recon_tm [] = NONE
      | recon_tm ((p, (tm, tm')) :: rest) =
      if tm = l andalso tm' = r then SOME (rev p)
      else
        case (tm, tm') of (Fn a, Fn a') => recon_fn p a a' rest
        | _ => recon_tm rest

    fun recon_lit (Not a) (Not a') = recon_lit a a'
      | recon_lit (Atom a) (Atom a') =
      if l <> r andalso a = a' then NONE else recon_tm [([], (a, a'))]
      | recon_lit _ _ = NONE
  in
    fn (lit, lit') =>
    case recon_lit lit lit' of SOME p => SOME (lit, p) | NONE => NONE
  end;

fun reconstruct (cl, (Axiom, [])) = Axiom' cl
  | reconstruct ([lit], (Refl, [])) = Refl' (lhs lit)
  | reconstruct ([fm, _], (Assume, [])) = Assume' fm
  | reconstruct (cl, (Inst, [th])) =
  Inst' (matchl_literals |<>| (zip (clause th) cl), th)
  | reconstruct (_, (Factor, [th])) = Factor' th
  | reconstruct (cl, (Resolve, [th1, th2])) =
  let
    val f = reconstruct_resolvant (clause th1) (clause th2)
    val l =
      case f (cl, cl) of SOME l => l
      | NONE =>
        case first f (List.tabulate (length cl, split cl)) of SOME l => l
        | NONE => raise BUG "inference" "couldn't reconstruct resolvant"
  in
    Resolve' (l, th1, th2)
  end
  | reconstruct (Not fm :: cl, (Equality, [th])) =
  let
    val (tm1, tm2) = dest_eq fm
  in
    case first (reconstruct_equality tm1 tm2) (zip (clause th) cl) of
      SOME (l, p) => Equality' (l, p, tm2, true, th)
    | NONE =>
      case first (reconstruct_equality tm2 tm1) (zip (clause th) cl) of
        SOME (l, p) => Equality' (l, p, tm1, false, th)
      | NONE => raise BUG "inference" "couldn't reconstruct equality step"
  end
  | reconstruct _ = raise BUG "inference" "malformed inference";

fun inference th =
  let
    val i = reconstruct (dest_thm th)
    val _ =
      (primitive_inference i = th) orelse
      raise BUG "inference"
      ("failed:\nth = " ^ thm_to_string th ^ "\ninf = " ^ inference_to_string i
       ^ "\ninf_th = " ^ thm_to_string (primitive_inference i))
  in
    i
  end;

local
  val empty = (Binarymap.mkDict thm_compare, []);
  fun contains (m, _) th = Option.isSome (Binarymap.peek (m, th));
  fun add th (m, p) = (Binarymap.insert (m, th, ()), (th, inference th) :: p);
  val finalize = snd;

  fun reduce (th, pf) =
    if contains pf th then pf
    else add th (foldl reduce pf (snd (snd (dest_thm th))));
in
  fun proof th = finalize (reduce (th, empty));
end;

(* ------------------------------------------------------------------------- *)
(* Contradictions and unit clauses.                                          *)
(* ------------------------------------------------------------------------- *)

val is_contradiction = null o clause;

fun dest_unit th =
  case clause th of [lit] => lit | _ => raise ERR "dest_unit" "not a unit";

val is_unit = can dest_unit;

(* ------------------------------------------------------------------------- *)
(* Derived rules                                                             *)
(* ------------------------------------------------------------------------- *)

fun CONTR lit th = RESOLVE (negate lit) (ASSUME lit) th;

fun WEAKEN lits th = foldl (uncurry CONTR) th (rev lits);

fun FRESH_VARSL ths =
  let
    val fvs = FVL (List.concat (map clause ths))
    val vvs = new_vars (length fvs)
    val sub = Subst1.from_maplets (zipwith (curry op |->) fvs vvs)
  in
    map (INST sub) ths
  end;

val FRESH_VARS = unwrap o FRESH_VARSL o wrap;

fun UNIT_SQUASH th =
  let
    fun squash env (x :: (xs as y :: _)) = squash (unify_literals env x y) xs
      | squash env _ = env
  in
    FACTOR (INST (squash |<>| (clause th)) th)
  end;
  
val REFLEXIVITY = REFL (Var "x");

val SYMMETRY =
  EQUALITY (mk_eq (Var "x", Var "x")) [0] (Var "y") true REFLEXIVITY;

val TRANSITIVITY =
  EQUALITY (mk_eq (Var "y", Var "z")) [0] (Var "x") false
  (ASSUME (Not (mk_eq (Var "y", Var "z"))));

fun FUN_CONGRUENCE (function, arity) =
  let
    val xs = List.tabulate (arity, fn i => Var ("x" ^ int_to_string i))
    val ys = List.tabulate (arity, fn i => Var ("y" ^ int_to_string i))
    fun f (i, th) =
      EQUALITY (List.last (clause th)) [1,i] (List.nth (ys, i)) true th
    val refl = INST (("x" |-> Fn (function, xs)) ::> |<>|) REFLEXIVITY
  in
    foldl f refl (rev (interval 0 arity))
  end;

fun REL_CONGRUENCE (relation, arity) =
  let
    val xs = List.tabulate (arity, fn i => Var ("x" ^ int_to_string i))
    val ys = List.tabulate (arity, fn i => Var ("y" ^ int_to_string i))
    fun f (i, th) =
      EQUALITY (List.last (clause th)) [i] (List.nth (ys, i)) true th
    val refl = ASSUME (Not (Atom (Fn (relation, xs))))
  in
    foldl f refl (rev (interval 0 arity))
  end;

end
(*#line 0.0 "fol/Canon1.sig"*)
(* ========================================================================= *)
(* FIRST-ORDER LOGIC CANONICALIZATION                                        *)
(* Created by Joe Hurd, September 2001                                       *)
(* Partly ported from the CAML-Light code accompanying John Harrison's book  *)
(* ========================================================================= *)

signature Canon1 =
sig

type term    = Term1.term
type formula = Term1.formula
type thm     = Thm1.thm

(* Simplification *)
val simplify : formula -> formula

(* Negation normal form *)
val nnf : formula -> formula

(* Prenex normal form *)
val prenex : formula -> formula
val pnf    : formula -> formula

(* Skolemization *)
val skolemize      : formula -> formula
val full_skolemize : formula -> formula

(* A tautology filter for clauses *)
val tautologous : formula list -> bool

(* Conjunctive normal form *)
val purecnf        : formula -> formula list list
val simpcnf        : formula -> formula list list
val clausal        : formula -> formula list list
val cnf            : formula -> formula
val axiomatize     : formula -> thm list
val eq_axiomatize  : formula -> thm list          (* Adds equality axioms *)
val eq_axiomatize' : formula -> thm list          (* Adds if equality occurs *)

end
(*#line 0.0 "fol/Canon1.sml"*)
(* ========================================================================= *)
(* FIRST-ORDER LOGIC CANONICALIZATION                                        *)
(* Created by Joe Hurd, September 2001                                       *)
(* Partly ported from the CAML-Light code accompanying John Harrison's book  *)
(* ========================================================================= *)

(*
app load ["Useful", "Term1"];
*)

structure Canon1 :> Canon1 =
struct

open Useful Term1 Thm1;

infixr |-> ::> oo;

type subst        = Subst1.subst;
val |<>|          = Subst1.|<>|;
val op ::>        = Subst1.::>;
val term_subst    = Subst1.term_subst;
val formula_subst = Subst1.formula_subst;

(* ------------------------------------------------------------------------- *)
(* Simplification.                                                           *)
(* ------------------------------------------------------------------------- *)

fun simplify1 (Not False)           = True
  | simplify1 (Not True)            = False
  | simplify1 (Not (Not fm))        = fm
  | simplify1 (And (False, q))      = False
  | simplify1 (And (p,     False))  = False
  | simplify1 (And (True,  q))      = q
  | simplify1 (And (p,     True))   = p
  | simplify1 (Or  (False, q))      = q
  | simplify1 (Or  (p,     False))  = p
  | simplify1 (Or  (True,  q))      = True
  | simplify1 (Or  (p,     True))   = True
  | simplify1 (Imp (False, q))      = True
  | simplify1 (Imp (True,  q))      = q
  | simplify1 (Imp (p,     True))   = True
  | simplify1 (Imp (Not p, False))  = p
  | simplify1 (Imp (p,     False))  = Not p
  | simplify1 (Iff (True,  q))      = q
  | simplify1 (Iff (p,     True))   = p
  | simplify1 (Iff (False, Not q))  = q
  | simplify1 (Iff (False, q))      = Not q
  | simplify1 (Iff (Not p, False))  = p
  | simplify1 (Iff (p,     False))  = Not p
  | simplify1 (fm as Forall (x, p)) = if mem x (FV p) then fm else p
  | simplify1 (fm as Exists (x, p)) = if mem x (FV p) then fm else p
  | simplify1 fm                    = fm;

fun simplify (Not    p)      = simplify1 (Not (simplify p))
  | simplify (And    (p, q)) = simplify1 (And (simplify p, simplify q))
  | simplify (Or     (p, q)) = simplify1 (Or  (simplify p, simplify q))
  | simplify (Imp    (p, q)) = simplify1 (Imp (simplify p, simplify q))
  | simplify (Iff    (p, q)) = simplify1 (Iff (simplify p, simplify q))
  | simplify (Forall (x, p)) = simplify1 (Forall (x, simplify p))
  | simplify (Exists (x, p)) = simplify1 (Exists (x, simplify p))
  | simplify fm              = fm;

(* ------------------------------------------------------------------------- *)
(* Negation normal form.                                                     *)
(* ------------------------------------------------------------------------- *)

fun nnf (And (p, q))     = And (nnf p, nnf q)
  | nnf (Or (p, q))      = Or (nnf p, nnf q)
  | nnf (Imp (p, q))     = Or (nnf' p, nnf q)
  | nnf (Iff (p, q))     = Or (And (nnf p, nnf q), And (nnf' p, nnf' q))
  | nnf (Forall (x, p))  = Forall (x, nnf p)
  | nnf (Exists (x, p))  = Exists (x, nnf p)
  | nnf (Not x)          = nnf' x
  | nnf fm               = fm
and nnf' True            = False
  | nnf' False           = True
  | nnf' (And (p, q))    = Or (nnf' p, nnf' q)
  | nnf' (Or (p, q))     = And (nnf' p, nnf' q)
  | nnf' (Imp (p, q))    = And (nnf p, nnf' q)
  | nnf' (Iff (p, q))    = Or (And (nnf p, nnf' q), And (nnf' p, nnf q))
  | nnf' (Forall (x, p)) = Exists (x, nnf' p)
  | nnf' (Exists (x, p)) = Forall (x, nnf' p)
  | nnf' (Not x)         = nnf x
  | nnf' fm              = Not fm;

(* ------------------------------------------------------------------------- *)
(* Prenex normal form.                                                       *)
(* ------------------------------------------------------------------------- *)

fun pullquants fm =
  (case fm of
     And (Forall (x, p), Forall (y, q)) => pullquant_2 fm Forall And x y p q
   | Or (Exists (x, p), Exists (y, q)) => pullquant_2 fm Exists Or x y p q
   | And (Forall (x, p), q) => pullquant_l fm Forall And x p q
   | And (p, Forall (x, q)) => pullquant_r fm Forall And x p q
   | Or (Forall (x, p), q) => pullquant_l fm Forall Or x p q
   | Or (p, Forall (x, q)) => pullquant_r fm Forall Or x p q
   | And (Exists (x, p), q) => pullquant_l fm Exists And x p q
   | And (p, Exists (x, q)) => pullquant_r fm Exists And x p q
   | Or (Exists (x, p), q) => pullquant_l fm Exists Or x p q
   | Or (p, Exists (x, q)) => pullquant_r fm Exists Or x p q
   | _ => fm)
and pullquant_l fm Q C x p q =
  let
    val x' = variant x (FV fm)
  in
    Q (x', pullquants (C (formula_subst ((x |-> Var x') ::> |<>|) p, q)))
  end
and pullquant_r fm Q C x p q =
  let
    val x' = variant x (FV fm)
  in
    Q (x', pullquants (C (p, formula_subst ((x |-> Var x') ::> |<>|) q)))
  end
and pullquant_2 fm Q C x y p q =
  let
    val x' = variant x (FV fm)
  in
    Q (x', pullquants(C (formula_subst ((x |-> Var x') ::> |<>|) p,
                         formula_subst ((x |-> Var x') ::> |<>|) q)))
  end;

fun prenex (Forall (x, p)) = Forall (x, prenex p)
  | prenex (Exists (x, p)) = Exists (x, prenex p)
  | prenex (And (p, q)) = pullquants (And (prenex p, prenex q))
  | prenex (Or (p, q)) = pullquants (Or (prenex p, prenex q))
  | prenex fm = fm;

val pnf = prenex o nnf o simplify;

(* ------------------------------------------------------------------------- *)
(* Skolemization function.                                                   *)
(* ------------------------------------------------------------------------- *)

fun skolem avoid (Exists (y, p)) =
  let
    val xs = subtract (FV p) [y]
    val f = variant (if xs = [] then "c_" ^ y else "f_" ^ y) avoid
  in
    skolem avoid (formula_subst ((y |-> Fn (f, map Var xs)) ::> |<>|) p)
  end
  | skolem avoid (Forall (x, p)) = Forall (x, skolem avoid p)
  | skolem avoid (And (p, q)) = skolem2 avoid And p q
  | skolem avoid (Or (p, q)) = skolem2 avoid Or p q
  | skolem _ fm = fm
and skolem2 avoid C p q =
  let
    val p' = skolem avoid p
    val q' = skolem (union avoid (function_names p')) q
  in
    C (p', q')
  end;

fun skolemize fm = skolem (function_names fm) fm;

val full_skolemize = specialize o prenex o skolemize o nnf o simplify;

(* ------------------------------------------------------------------------- *)
(* A tautology filter for clauses.                                           *)
(* ------------------------------------------------------------------------- *)

fun tautologous cls =
  let
    val (pos, neg) = List.partition positive cls
  in
    intersect pos (map negate neg) <> []
  end;

(* ------------------------------------------------------------------------- *)
(* Conjunctive Normal Form.                                                  *)
(* ------------------------------------------------------------------------- *)

fun distrib s1 s2 = cartwith union s1 s2;

fun purecnf (Or (p, q))  = distrib (purecnf p) (purecnf q)
  | purecnf (And (p, q)) = union (purecnf p) (purecnf q)
  | purecnf fm = [[fm]];

fun simpcnf True = []
  | simpcnf False = [[]]
  | simpcnf fm = List.filter (non tautologous) (purecnf fm);

val clausal =
  List.concat o map (simpcnf o specialize o prenex) o flatten_conj o
  skolemize o nnf o simplify

val cnf = list_mk_conj o map list_mk_disj o clausal;

val axiomatize = map AXIOM o clausal;

fun eq_axiomatize fm =
  let
    val eqs = [REFLEXIVITY, SYMMETRY, TRANSITIVITY]
    val rels = map REL_CONGRUENCE (relations_no_eq fm)
    val funs = map FUN_CONGRUENCE (functions fm)
  in
    eqs @ funs @ rels @ axiomatize fm
  end;

fun eq_axiomatize' fm = (if eq_occurs fm then eq_axiomatize else axiomatize) fm;

end
(*#line 0.0 "fol/Units1.sig"*)
(* ========================================================================= *)
(* A STORE IN WHICH TO CACHE UNIT THEOREMS                                   *)
(* Created by Joe Hurd, November 2001                                        *)
(* ========================================================================= *)

signature Units1 =
sig

type 'a pp   = 'a Useful.pp
type formula = Term1.formula
type thm     = Thm1.thm

type units

val empty    : units
val add      : thm -> units -> units
val addl     : thm list -> units -> units
val subsumes : units -> formula -> thm option
val prove    : units -> formula list -> thm list option
val demod    : units -> thm -> thm
val info     : units -> string
val pp_units : units pp

end
(*#line 0.0 "fol/Units1.sml"*)
(* ========================================================================= *)
(* A STORE IN WHICH TO CACHE UNIT THEOREMS                                   *)
(* Created by Joe Hurd, November 2001                                        *)
(* ========================================================================= *)

(*
app load
 ["Useful", "Mosml", "Term1", "Thm1", "Canon1", "Match1"];
*)

(*
*)
structure Units1 :> Units1 =
struct

open Useful Term1 Thm1 Match1;

infix |-> ::> @> oo ##;

structure N = LiteralNet1;

(* ------------------------------------------------------------------------- *)
(* Auxiliary functions.                                                      *)
(* ------------------------------------------------------------------------- *)

fun lift_options f =
  let
    fun g res [] = SOME (rev res)
      | g res (x :: xs) = case f x of SOME y => g (y :: res) xs | NONE => NONE
  in
    g []
  end;

(* ------------------------------------------------------------------------- *)
(* Operations on the raw unit cache.                                         *)
(* ------------------------------------------------------------------------- *)

type uns = thm N.literal_map;

val uempty : uns = N.empty;

fun uadd th uns = N.insert (dest_unit th |-> th) uns;

fun usubsumes uns lit =
  List.find (can (C match_literals lit) o dest_unit)
  (rev (N.match uns lit));

fun uprove uns =
  let
    fun pr lit =
      Option.map (fn th => INST (match_literals (dest_unit th) lit) th)
      (usubsumes uns lit)
  in
    lift_options pr
  end;

fun udemod uns =
  let
    fun demod (lit, th) =
      case uprove uns [negate lit] of NONE => th
      | SOME [dth] => RESOLVE lit th dth
      | SOME _ => raise BUG "unit_demod" "corrupt"
  in
    fn th => foldl demod th (clause th)
  end;

(* ------------------------------------------------------------------------- *)
(* The user interface.                                                       *)
(* ------------------------------------------------------------------------- *)

type units = (thm, uns) sum;

val empty = INR uempty;

fun subsumes (INL th) = K (SOME th)
  | subsumes (INR uns) = usubsumes uns;

fun prove (INL th) = SOME o map (fn False => th | lit => CONTR lit th)
  | prove (INR uns) = uprove uns;

fun demod (INL th) = K th
  | demod (INR uns) = udemod uns;

fun info ((INL _) : units) = "*"
  | info (INR uns) = int_to_string (N.size uns);

val pp_units = pp_map info pp_string;

(* Adding a theorem involves squashing it to a unit, if possible. *)

fun add _ (U as INL _) = U
  | add th (U as INR uns) =
  if List.exists (Option.isSome o usubsumes uns) (clause th) then U
  else
    let
      val th = udemod uns th
    in
      if is_contradiction th then INL th
      else case total UNIT_SQUASH th of NONE => U | SOME th => INR (uadd th uns)
    end;

val addl = C (foldl (uncurry add));

end
(*#line 0.0 "fol/Problem1.sig"*)
(* ========================================================================= *)
(* SOME SAMPLE PROBLEMS TO TEST PROOF PROCEDURES                             *)
(* Created by Joe Hurd, September 2001                                       *)
(* Partly ported from the CAML-Light code accompanying John Harrison's book  *)
(* ========================================================================= *)

signature Problem1 =
sig

type 'a quotation = 'a frag list
type 'a problem   = {name : string, goal : 'a quotation}

(* Accessing individual problems *)
val get : 'a problem list -> string -> 'a quotation

(* The master collections *)
val nonequality : 'a problem list
val equality    : 'a problem list
val tptp        : 'a problem list

(* Some compilations *)
(*val quick : 'a problem list *)

end
(*#line 0.0 "fol/Problem1.sml"*)
(* ========================================================================= *)
(* SOME SAMPLE PROBLEMS TO TEST PROOF PROCEDURES                             *)
(* Created by Joe Hurd, September 2001                                       *)
(* Partly ported from the CAML-Light code accompanying John Harrison's book  *)
(* ========================================================================= *)

structure Problem1 :> Problem1 =
struct

type 'a quotation = 'a frag list;

type 'a problem = {name : string, goal : 'a quotation};

(* ========================================================================= *)
(* Accessing individual problems.                                            *)
(* ========================================================================= *)

fun extract (p : 'a problem list) n =
  Option.valOf (List.find (fn {name, ...} => name = n) p);

fun get p = #goal o extract p;

(* ========================================================================= *)
(* Problems without equality.                                                *)
(* ========================================================================= *)

val nonequality = [

(* ------------------------------------------------------------------------- *)
(* Trivia (some of which demonstrate ex-bugs in provers).                    *)
(* ------------------------------------------------------------------------- *)

{name = "TRUE",
 goal = [
QUOTE "\nT"]},

{name = "P_or_not_P",
 goal = [
QUOTE "\np \\/ ~p"]},

{name = "JH_test",
 goal = [
QUOTE "\n!x y. ?z. p x \\/ p y ==> p z"]},

{name = "CYCLIC",
 goal = [
QUOTE "\n(!x. p (g (c x))) ==> ?z. p (g z)"]},

{name = "MN_bug",
 goal = [
QUOTE "\n(!x. ?y. f y x x) ==> ?z. f z 0 0"]},

{name = "ERIC",
 goal = [
QUOTE "\n!x. ?v w. !y z. p x /\\ q y ==> (p v \\/ r w) /\\ (r z ==> q v)"]},

(* ------------------------------------------------------------------------- *)
(* Propositional Logic.                                                      *)
(* ------------------------------------------------------------------------- *)

{name = "PROP_1",
 goal = [
QUOTE "\np ==> q <=> ~q ==> ~p"]},

{name = "PROP_2",
 goal = [
QUOTE "\n~~p <=> p"]},

{name = "PROP_3",
 goal = [
QUOTE "\n~(p ==> q) ==> q ==> p"]},

{name = "PROP_4",
 goal = [
QUOTE "\n~p ==> q <=> ~q ==> p"]},

{name = "PROP_5",
 goal = [
QUOTE "\n(p \\/ q ==> p \\/ r) ==> p \\/ (q ==> r)"]},

{name = "PROP_6",
 goal = [
QUOTE "\np \\/ ~p"]},

{name = "PROP_7",
 goal = [
QUOTE "\np \\/ ~~~p"]},

{name = "PROP_8",
 goal = [
QUOTE "\n((p ==> q) ==> p) ==> p"]},

{name = "PROP_9",
 goal = [
QUOTE "\n(p \\/ q) /\\ (~p \\/ q) /\\ (p \\/ ~q) ==> ~(~q \\/ ~q)"]},

{name = "PROP_10",
 goal = [
QUOTE "\n(q ==> r) /\\ (r ==> p /\\ q) /\\ (p ==> q /\\ r) ==> (p <=> q)"]},

{name = "PROP_11",
 goal = [
QUOTE "\np <=> p"]},

{name = "PROP_12",
 goal = [
QUOTE "\n((p <=> q) <=> r) <=> p <=> q <=> r"]},

{name = "PROP_13",
 goal = [
QUOTE "\np \\/ q /\\ r <=> (p \\/ q) /\\ (p \\/ r)"]},

{name = "PROP_14",
 goal = [
QUOTE "\n(p <=> q) <=> (q \\/ ~p) /\\ (~q \\/ p)"]},

{name = "PROP_15",
 goal = [
QUOTE "\np ==> q <=> ~p \\/ q"]},

{name = "PROP_16",
 goal = [
QUOTE "\n(p ==> q) \\/ (q ==> p)"]},

{name = "PROP_17",
 goal = [
QUOTE "\np /\\ (q ==> r) ==> s <=> (~p \\/ q \\/ s) /\\ (~p \\/ ~r \\/ s)"]},

{name = "MATHS4_EXAMPLE",
 goal = [
QUOTE "\n(a \\/ ~k ==> g) /\\ (g ==> q) /\\ ~q ==> k"]},

{name = "XOR_ASSOC",
 goal = [
QUOTE "\n~(~(p <=> q) <=> r) <=> ~(p <=> ~(q <=> r))"]},

(* ------------------------------------------------------------------------- *)
(* Monadic Predicate Logic.                                                  *)
(* ------------------------------------------------------------------------- *)

(* The drinker's principle *)
{name = "P18",
 goal = [
QUOTE "\n?very_popular_guy. !whole_pub. drinks very_popular_guy ==> drinks whole_pub"]},

{name = "P19",
 goal = [
QUOTE "\n?x. !y z. (p y ==> q z) ==> p x ==> q x"]},

{name = "P20",
 goal = [
QUOTE "\n(!x y. ?z. !w. p x /\\ q y ==> r z /\\ u w) /\\ (!x y. p x /\\ q y) ==> ?z. r z"]},

{name = "P21",
 goal = [
QUOTE "\n(?x. p ==> q x) /\\ (?x. q x ==> p) ==> ?x. p <=> q x"]},

{name = "P22",
 goal = [
QUOTE "\n(!x. p <=> q x) ==> (p <=> !x. q x)"]},

{name = "P23",
 goal = [
QUOTE "\n(!x. p \\/ q x) <=> p \\/ !x. q x"]},

{name = "P24",
 goal = [

QUOTE "\n~(?x. u x /\\ q x) /\\ (!x. p x ==> q x \\/ r x) /\\ ~(?x. p x ==> ?x. q x) /\\\n(!x. q x /\\ r x ==> u x) ==> ?x. p x /\\ r x"]},

{name = "P25",
 goal = [

QUOTE "\n(?x. p x) /\\ (!x. u x ==> ~g x /\\ r x) /\\ (!x. p x ==> g x /\\ u x) /\\\n((!x. p x ==> q x) \\/ ?x. q x /\\ p x) ==> ?x. q x /\\ p x"]},

{name = "P26",
 goal = [

QUOTE "\n((?x. p x) <=> ?x. q x) /\\ (!x y. p x /\\ q y ==> (r x <=> u y)) ==>\n((!x. p x ==> r x) <=> !x. q x ==> u x)"]},

{name = "P27",
 goal = [

QUOTE "\n(?x. p x /\\ ~q x) /\\ (!x. p x ==> r x) /\\ (!x. u x /\\ s x ==> p x) /\\\n(?x. r x /\\ ~q x) ==> (!x. u x ==> ~r x) ==> !x. u x ==> ~s x"]},

{name = "P28",
 goal = [

QUOTE "\n(!x. p x ==> !x. q x) /\\ ((!x. q x \\/ r x) ==> ?x. q x /\\ r x) /\\\n((?x. r x) ==> !x. l x ==> m x) ==> !x. p x /\\ l x ==> m x"]},

{name = "P29",
 goal = [


QUOTE "\n(?x. p x) /\\ (?x. g x) ==>\n((!x. p x ==> h x) /\\ (!x. g x ==> j x) <=>\n !x y. p x /\\ g y ==> h x /\\ j y)"]},

{name = "P30",
 goal = [

QUOTE "\n(!x. p x \\/ g x ==> ~h x) /\\ (!x. (g x ==> ~u x) ==> p x /\\ h x) ==>\n!x. u x"]},

{name = "P31",
 goal = [

QUOTE "\n~(?x. p x /\\ (g x \\/ h x)) /\\ (?x. q x /\\ p x) /\\ (!x. ~h x ==> j x) ==>\n?x. q x /\\ j x"]},

{name = "P32",
 goal = [

QUOTE "\n(!x. p x /\\ (g x \\/ h x) ==> q x) /\\ (!x. q x /\\ h x ==> j x) /\\\n(!x. r x ==> h x) ==> !x. p x /\\ r x ==> j x"]},

{name = "P33",
 goal = [

QUOTE "\n(!x. p a /\\ (p x ==> p b) ==> p c) <=>\n(!x. p a ==> p x \\/ p c) /\\ (p a ==> p b ==> p c)"]},

(* This gives rise to 5184 clauses when converted to CNF! *)
{name = "P34",
 goal = [

QUOTE "\n((?x. !y. p x <=> p y) <=> (?x. q x) <=> !y. q y) <=>\n(?x. !y. q x <=> q y) <=> (?x. p x) <=> !y. p y"]},

{name = "P35",
 goal = [
QUOTE "\n?x y. p x y ==> !x y. p x y"]},

(* ------------------------------------------------------------------------- *)
(* Full predicate logic (without Identity and Functions)                     *)
(* ------------------------------------------------------------------------- *)

{name = "P36",
 goal = [

QUOTE "\n(!x. ?y. p x y) /\\ (!x. ?y. g x y) /\\\n(!x y. p x y \\/ g x y ==> !z. p y z \\/ g y z ==> h x z) ==> !x. ?y. h x y"]},

{name = "P37",
 goal = [


QUOTE "\n(!z. ?w. !x. ?y. (p x z ==> p y w) /\\ p y z /\\ (p y w ==> ?v. q v w)) /\\\n(!x z. ~p x z ==> ?y. q y z) /\\ ((?x y. q x y) ==> !x. r x x) ==>\n!x. ?y. r x y"]},

{name = "P38",
 goal = [



QUOTE "\n(!x. p a /\\ (p x ==> ?y. p y /\\ r x y) ==> ?z w. p z /\\ r x w /\\ r w z) <=>\n!x.\n  (~p a \\/ p x \\/ ?z w. p z /\\ r x w /\\ r w z) /\\\n  (~p a \\/ ~(?y. p y /\\ r x y) \\/ ?z w. p z /\\ r x w /\\ r w z)"]},

{name = "P39",
 goal = [
QUOTE "\n~?x. !y. p y x <=> ~p y y"]},

{name = "P40",
 goal = [
QUOTE "\n(?y. !x. p x y <=> p x x) ==> ~!x. ?y. !z. p z y <=> ~p z x"]},

{name = "P41",
 goal = [
QUOTE "\n(!z. ?y. !x. p x y <=> p x z /\\ ~p x x) ==> ~?z. !x. p x z"]},

{name = "P42",
 goal = [
QUOTE "\n~?y. !x. p x y <=> ~?z. p x z /\\ p z x"]},

{name = "P43",
 goal = [
QUOTE "\n(!x y. q x y <=> !z. p z x <=> p z y) ==> !x y. q x y <=> q y x"]},

{name = "P44",
 goal = [

QUOTE "\n(!x. p x ==> (?y. g y /\\ h x y) /\\ ?y. g y /\\ ~h x y) /\\\n(?x. j x /\\ !y. g y ==> h x y) ==> ?x. j x /\\ ~p x"]},

{name = "P45",
 goal = [



QUOTE "\n(!x. p x /\\ (!y. g y /\\ h x y ==> j x y) ==> !y. g y /\\ h x y ==> r y) /\\\n~(?y. l y /\\ r y) /\\\n(?x. p x /\\ (!y. h x y ==> l y) /\\ !y. g y /\\ h x y ==> j x y) ==>\n?x. p x /\\ ~?y. g y /\\ h x y"]},

{name = "P46",
 goal = [


QUOTE "\n(!x. p x /\\ (!y. p y /\\ h y x ==> g y) ==> g x) /\\\n((?x. p x /\\ ~g x) ==> ?x. p x /\\ ~g x /\\ !y. p y /\\ ~g y ==> j x y) /\\\n(!x y. p x /\\ p y /\\ h x y ==> ~j y x) ==> !x. p x ==> g x"]},

{name = "P50",
 goal = [
QUOTE "\n(!x. f0 a x \\/ !y. f0 x y) ==> ?x. !y. f0 x y"]},

(* ------------------------------------------------------------------------- *)
(* Example from Manthey and Bry, CADE-9.                                     *)
(* ------------------------------------------------------------------------- *)

{name = "P55",
 goal = [








QUOTE "\nlives agatha /\\ lives butler /\\ lives charles /\\\n(killed agatha agatha \\/ killed butler agatha \\/ killed charles agatha) /\\\n(!x y. killed x y ==> hates x y /\\ ~richer x y) /\\\n(!x. hates agatha x ==> ~hates charles x) /\\\n(hates agatha agatha /\\ hates agatha charles) /\\\n(!x. lives x /\\ ~richer x agatha ==> hates butler x) /\\\n(!x. hates agatha x ==> hates butler x) /\\\n(!x. ~hates x agatha \\/ ~hates x butler \\/ ~hates x charles) ==>\nkilled agatha agatha /\\ ~killed butler agatha /\\ ~killed charles agatha"]},

{name = "P57",
 goal = [

QUOTE "\np (f a b) (f b c) /\\ p (f b c) (f a c) /\\\n(!x y z. p x y /\\ p y z ==> p x z) ==> p (f a b) (f a c)"]},

(* ------------------------------------------------------------------------- *)
(* See info-hol, circa 1500.                                                 *)
(* ------------------------------------------------------------------------- *)

{name = "P58",
 goal = [
QUOTE "\n!x. ?v w. !y z. p x /\\ q y ==> (p v \\/ r w) /\\ (r z ==> q v)"]},

{name = "P59",
 goal = [
QUOTE "\n(!x. p x <=> ~p (f x)) ==> ?x. p x /\\ ~p (f x)"]},

{name = "P60",
 goal = [
QUOTE "\n!x. p x (f x) <=> ?y. (!z. p z y ==> p z (f x)) /\\ p x y"]},

(* ------------------------------------------------------------------------- *)
(* From Gilmore's classic paper.                                             *)
(* ------------------------------------------------------------------------- *)

(*
JRH: Amazingly, this still seems non-trivial... in HOL it works at depth 45!
Joe: Confirmed (depth=45, inferences=263702, time=148s), though if lemmaizing
     is used then a lemma is discovered at depth 29 that allows a much quicker
     proof (depth=30, inferences=10039, time=5.5s). [13 Oct 2001]
*)
{name = "GILMORE_1",
 goal = [


QUOTE "\n?x. !y z.\n  (f y ==> g y <=> f x) /\\ (f y ==> h y <=> g x) /\\\n  ((f y ==> g y) ==> h y <=> h x) ==> f z /\\ g z /\\ h z"]},

(*
JRH: This is not valid, according to Gilmore
{name = "GILMORE_2",
 goal = `
?x y. !z.
  (f x z <=> f z y) /\ (f z y <=> f z z) /\ (f x y <=> f y x) ==>
  (f x y <=> f x z)`},
*)

{name = "GILMORE_3",
 goal = [


QUOTE "\n?x. !y z.\n  ((f y z ==> g y ==> h x) ==> f x x) /\\ ((f z x ==> g x) ==> h z) /\\\n  f x y ==> f z z"]},

{name = "GILMORE_4",
 goal = [
QUOTE "\n?x y. !z. (f x y ==> f y z /\\ f z z) /\\ (f x y /\\ g x y ==> g x z /\\ g z z)"]},

{name = "GILMORE_5",
 goal = [
QUOTE "\n(!x. ?y. f x y \\/ f y x) /\\ (!x y. f y x ==> f y y) ==> ?z. f z z"]},

{name = "GILMORE_6",
 goal = [



QUOTE "\n!x. ?y.\n  (?w. !v. f w x ==> g v w /\\ g w x) ==>\n  (?w. !v. f w y ==> g v w /\\ g w y) \\/\n  !z v. ?w. g v z \\/ h w y z ==> g z w"]},

{name = "GILMORE_7",
 goal = [

QUOTE "\n(!x. k x ==> ?y. l y /\\ (f x y ==> g x y)) /\\\n(?z. k z /\\ !w. l w ==> f z w) ==> ?v w. k v /\\ l w /\\ g v w"]},

{name = "GILMORE_8",
 goal = [


QUOTE "\n?x. !y z.\n  ((f y z ==> g y ==> !w. ?v. h w v x) ==> f x x) /\\\n  ((f z x ==> g x) ==> !w. ?v. h w v z) /\\ f x y ==> f z z"]},

(*
JRH: This is still a very hard goal
Joe: With lemmaizing (in HOL): (depth=18, inferences=15632, time=21s)
     Without: gave up waiting after (depth=25, inferences=2125072, time=3000s)
     [13 Oct 2001]
*)
{name = "GILMORE_9",
 goal = [







QUOTE "\n!x. ?y. !z.\n  ((!w. ?v. f y w v /\\ g y w /\\ ~h y x) ==>\n   (!w. ?v. f x w v /\\ g z u /\\ ~h x z) ==>\n   !w. ?v. f x w v /\\ g y w /\\ ~h x y) /\\\n  ((!w. ?v. f x w v /\\ g y w /\\ ~h x y) ==>\n   ~(!w. ?v. f x w v /\\ g z w /\\ ~h x z) ==>\n   (!w. ?v. f y w v /\\ g y w /\\ ~h y x) /\\\n   !w. ?v. f z w v /\\ g y w /\\ ~h z y)"]},

(* ------------------------------------------------------------------------- *)
(* Translation of Gilmore procedure using separate definitions.              *)
(* ------------------------------------------------------------------------- *)

{name = "GILMORE_9a",
 goal = [


QUOTE "\n(!x y. p x y <=> !w. ?v. f x w v /\\ g y w /\\ ~h x y) ==>\n!x. ?y. !z.\n  (p y x ==> p x z ==> p x y) /\\ (p x y ==> ~p x z ==> p y x /\\ p z y)"]},

(* ------------------------------------------------------------------------- *)
(* Example from Davis-Putnam papers where Gilmore procedure is poor.         *)
(* ------------------------------------------------------------------------- *)

{name = "DAVIS_PUTNAM_EXAMPLE",
 goal = [
QUOTE "\n?x y. !z. (f x y ==> f y z /\\ f z z) /\\ (f x y /\\ g x y ==> g x z /\\ g z z)"]},

(* ------------------------------------------------------------------------- *)
(* The interesting example where connections make the proof longer.          *)
(* ------------------------------------------------------------------------- *)

{name = "BAD_CONNECTIONS",
 goal = [

QUOTE "\n~a /\\ (a \\/ b) /\\ (c \\/ d) /\\ (~b \\/ e \\/ f) /\\ (~c \\/ ~e) /\\ (~c \\/ ~f) /\\\n(~b \\/ g \\/ h) /\\ (~d \\/ ~g) /\\ (~d \\/ ~h) ==> F"]},

(* ------------------------------------------------------------------------- *)
(* The classic Los puzzle. (Clausal version MSC006-1 in the TPTP library.)   *)
(* Note: this is actually in the decidable "AE" subset,  though that doesn't *)
(* yield a very efficient proof.                                             *)
(* ------------------------------------------------------------------------- *)

{name = "LOS",
 goal = [


QUOTE "\n(!x y z. p x y ==> p y z ==> p x z) /\\\n(!x y z. q x y ==> q y z ==> q x z) /\\ (!x y. q x y ==> q y x) /\\\n(!x y. p x y \\/ q x y) ==> (!x y. p x y) \\/ !x y. q x y"]},

(* ------------------------------------------------------------------------- *)
(* The steamroller.                                                          *)
(* ------------------------------------------------------------------------- *)

{name = "STEAM_ROLLER",
 goal = [












QUOTE "\n((!x. p1 x ==> p0 x) /\\ ?x. p1 x) /\\ ((!x. p2 x ==> p0 x) /\\ ?x. p2 x) /\\\n((!x. p3 x ==> p0 x) /\\ ?x. p3 x) /\\ ((!x. p4 x ==> p0 x) /\\ ?x. p4 x) /\\\n((!x. p5 x ==> p0 x) /\\ ?x. p5 x) /\\ ((?x. q1 x) /\\ !x. q1 x ==> q0 x) /\\\n(!x.\n   p0 x ==>\n   (!y. q0 y ==> r x y) \\/\n   !y. p0 y /\\ s0 y x /\\ (?z. q0 z /\\ r y z) ==> r x y) /\\\n(!x y. p3 y /\\ (p5 x \\/ p4 x) ==> s0 x y) /\\\n(!x y. p3 x /\\ p2 y ==> s0 x y) /\\ (!x y. p2 x /\\ p1 y ==> s0 x y) /\\\n(!x y. p1 x /\\ (p2 y \\/ q1 y) ==> ~r x y) /\\\n(!x y. p3 x /\\ p4 y ==> r x y) /\\ (!x y. p3 x /\\ p5 y ==> ~r x y) /\\\n(!x. p4 x \\/ p5 x ==> ?y. q0 y /\\ r x y) ==>\n?x y. p0 x /\\ p0 y /\\ ?z. q1 z /\\ r y z /\\ r x y"]},

(* ------------------------------------------------------------------------- *)
(* An incestuous example used to establish completeness characterization.    *)
(* ------------------------------------------------------------------------- *)

{name = "MODEL_COMPLETENESS",
 goal = [








QUOTE "\n(!w x. sentence x ==> holds w x \\/ holds w (not x)) /\\\n(!w x. ~(holds w x /\\ holds w (not x))) ==>\n((!x.\n    sentence x ==>\n    (!w. models w s ==> holds w x) \\/\n    !w. models w s ==> holds w (not x)) <=>\n !w v.\n   models w s /\\ models v s ==>\n   !x. sentence x ==> (holds w x <=> holds v x))"]}

];

(* ========================================================================= *)
(* Problems with equality.                                                   *)
(* ========================================================================= *)

val equality = [
 
(* ------------------------------------------------------------------------- *)
(* Trivia (some of which demonstrate ex-bugs in the prover).                 *)
(* ------------------------------------------------------------------------- *)

{name = "REFLEXIVITY",
 goal = [
QUOTE "\nc = c"]},

{name = "SYMMETRY",
 goal = [
QUOTE "\n!x y. x = y ==> y = x"]},

{name = "TRANSITIVITY",
 goal = [
QUOTE "\n!x y z. x = y /\\ y = z ==> x = z"]},

{name = "TRANS_SYMM",
 goal = [
QUOTE "\n!x y z. x = y /\\ y = z ==> z = x"]},

{name = "SUBSTITUTIVITY",
 goal = [
QUOTE "\n!x y. f x /\\ x = y ==> f y"]},

{name = "CYCLIC_SUBSTITUTION_BUG",
 goal = [
QUOTE "\n(!x. y = g (c x)) ==> ?z. y = g z"]},

(* ------------------------------------------------------------------------- *)
(* Simple equality problems.                                                 *)
(* ------------------------------------------------------------------------- *)

{name = "P48",
 goal = [
QUOTE "\n(a = b \\/ c = d) /\\ (a = c \\/ b = d) ==> a = d \\/ b = c"]},

{name = "P49",
 goal = [
QUOTE "\n(?x y. !z. z = x \\/ z = y) /\\ p a /\\ p b /\\ ~(a = b) ==> !x. p x"]},

{name = "P51",
 goal = [

QUOTE "\n(?z w. !x y. f0 x y <=> x = z /\\ y = w) ==>\n?z. !x. (?w. !y. f0 x y <=> y = w) <=> x = z"]},

{name = "P52",
 goal = [

QUOTE "\n(?z w. !x y. f0 x y <=> x = z /\\ y = w) ==>\n?w. !y. (?z. !x. f0 x y <=> x = z) <=> y = w"]},

(* ------------------------------------------------------------------------- *)
(* The Melham problem after an inverse skolemization step.                   *)
(* ------------------------------------------------------------------------- *)

{name = "UNSKOLEMIZED_MELHAM",
 goal = [
QUOTE "\n(!x y. g x = g y ==> f x = f y) ==> !y. ?w. !x. y = g x ==> w = f x"]},
 
(* ------------------------------------------------------------------------- *)
(* The example always given for congruence closure.                          *)
(* ------------------------------------------------------------------------- *)

{name = "CONGRUENCE_CLOSURE_EXAMPLE",
 goal = [
QUOTE "\n!x. f (f (f (f (f x)))) = x /\\ f (f (f x)) = x ==> f x = x"]},

(* ------------------------------------------------------------------------- *)
(* A simple example (see EWD1266a and the application to Morley's theorem).  *)
(* ------------------------------------------------------------------------- *)

{name = "EWD",
 goal = [

QUOTE "\n(!x. f x ==> g x) /\\ (?x. f x) /\\ (!x y. g x /\\ g y ==> x = y) ==>\n!y. g y ==> f y"]},

{name = "EWD'",
 goal = [
QUOTE "\n(!x. f (f x) = f x) /\\ (!x. ?y. f y = x) ==> !x. f x = x"]},

(* ------------------------------------------------------------------------- *)
(* Wishnu Prasetya's example.                                                *)
(* ------------------------------------------------------------------------- *)

{name = "WISHNU",
 goal = [

QUOTE "\n(?x. x = f (g x) /\\ !x'. x' = f (g x') ==> x = x') <=>\n?y. y = g (f y) /\\ !y'. y' = g (f y') ==> y = y'"]},

(* ------------------------------------------------------------------------- *)
(* An equality version of the Agatha puzzle.                                 *)
(* ------------------------------------------------------------------------- *)

{name = "AGATHA",
 goal = [









QUOTE "\n(?x. lives x /\\ killed x agatha) /\\\n(lives agatha /\\ lives butler /\\ lives charles) /\\\n(!x. lives x ==> x = agatha \\/ x = butler \\/ x = charles) /\\\n(!x y. killed x y ==> hates x y) /\\ (!x y. killed x y ==> ~richer x y) /\\\n(!x. hates agatha x ==> ~hates charles x) /\\\n(!x. ~(x = butler) ==> hates agatha x) /\\\n(!x. ~richer x agatha ==> hates butler x) /\\\n(!x. hates agatha x ==> hates butler x) /\\ (!x. ?y. ~hates x y) /\\\n~(agatha = butler) ==>\nkilled agatha agatha /\\ ~killed butler agatha /\\ ~killed charles agatha"]},

(* ------------------------------------------------------------------------- *)
(* Group theory examples.                                                    *)
(* ------------------------------------------------------------------------- *)

(* JRH: (Size 18, 61814 seconds.) *)
{name = "GROUP_RIGHT_INVERSE",
 goal = [

QUOTE "\n(!x y z. x * (y * z) = x * y * z) /\\ (!x. e * x = x) /\\\n(!x. i x * x = e) ==> !x. x * i x = e"]},

{name = "GROUP_RIGHT_IDENTITY",
 goal = [

QUOTE "\n(!x y z. x * (y * z) = x * y * z) /\\ (!x. e * x = x) /\\\n(!x. i x * x = e) ==> !x. x * e = x"]},

{name = "KLEIN_GROUP_COMMUTATIVE",
 goal = [

QUOTE "\n(!x y z. x * (y * z) = x * y * z) /\\ (!x. e * x = x) /\\ (!x. x * e = x) /\\\n(!x. x * x = e) ==> !x y. x * y = y * x"]}

];

(* ========================================================================= *)
(* Some sample problems from the TPTP archive.                               *)
(* Note: for brevity some relation/function names have been shortened.       *)
(* ========================================================================= *)

val tptp = [

(* ------------------------------------------------------------------------- *)
(* TPTP problems that have demonstrated bugs in the prover.                  *)
(* ------------------------------------------------------------------------- *)

(* Solved trivially by meson without cache cutting, but not with. *)
{name = "PUZ011-1",
 goal = [











QUOTE "\nocean atlantic /\\ ocean indian /\\ borders atlantic brazil /\\\nborders atlantic uruguay /\\ borders atlantic c_venesuela /\\\nborders atlantic c_zaire /\\ borders atlantic nigeria /\\\nborders atlantic angola /\\ borders indian india /\\\nborders indian pakistan /\\ borders indian iran /\\ borders indian somalia /\\\nborders indian kenya /\\ borders indian tanzania /\\ south_american brazil /\\\nsouth_american uruguay /\\ south_american c_venesuela /\\ african c_zaire /\\\nafrican nigeria /\\ african angola /\\ african somalia /\\ african kenya /\\\nafrican tanzania /\\ asian india /\\ asian pakistan /\\ asian iran ==>\n(!x y z.\n   ~ocean x \\/ ~borders x y \\/ ~african y \\/ ~borders x z \\/ ~asian z) ==>\nF"]},

(* ------------------------------------------------------------------------- *)
(* Problems used by the fol unit test to exercise the TPTP parser.           *)
(* ------------------------------------------------------------------------- *)
 
{name = "PUZ001-1",
 goal = [








QUOTE "\nlives agatha /\\ lives butler /\\ lives charles /\\\n(!x y. ~killed x y \\/ ~richer x y) /\\\n(!x. ~hates agatha x \\/ ~hates charles x) /\\\n(!x. ~hates x agatha \\/ ~hates x butler \\/ ~hates x charles) /\\\nhates agatha agatha /\\ hates agatha charles /\\\n(!x y. ~killed x y \\/ hates x y) /\\\n(!x. ~hates agatha x \\/ hates butler x) /\\\n(!x. ~lives x \\/ richer x agatha \\/ hates butler x) ==>\nkilled butler agatha \\/ killed charles agatha ==> F"]},

{name = "PUZ020-1",
 goal = [
























QUOTE "\n(!x. x = x) /\\ (!x y. ~(x = y) \\/ y = x) /\\\n(!x y z. ~(x = y) \\/ ~(y = z) \\/ x = z) /\\\n(!x y. ~(x = y) \\/ statement_by x = statement_by y) /\\\n(!x. ~person x \\/ knight x \\/ knave x) /\\\n(!x. ~person x \\/ ~knight x \\/ ~knave x) /\\\n(!x y. ~says x y \\/ a_truth y \\/ ~a_truth y) /\\\n(!x y. ~says x y \\/ ~(x = y)) /\\ (!x y. ~says x y \\/ y = statement_by x) /\\\n(!x y. ~person x \\/ ~(x = statement_by y)) /\\\n(!x. ~person x \\/ ~a_truth (statement_by x) \\/ knight x) /\\\n(!x. ~person x \\/ a_truth (statement_by x) \\/ knave x) /\\\n(!x y. ~(x = y) \\/ ~knight x \\/ knight y) /\\\n(!x y. ~(x = y) \\/ ~knave x \\/ knave y) /\\\n(!x y. ~(x = y) \\/ ~person x \\/ person y) /\\\n(!x y z. ~(x = y) \\/ ~says x z \\/ says y z) /\\\n(!x y z. ~(x = y) \\/ ~says z x \\/ says z y) /\\\n(!x y. ~(x = y) \\/ ~a_truth x \\/ a_truth y) /\\\n(!x y. ~knight x \\/ ~says x y \\/ a_truth y) /\\\n(!x y. ~knave x \\/ ~says x y \\/ ~a_truth y) /\\ person husband /\\\nperson c_wife /\\ ~(husband = c_wife) /\\\nsays husband (statement_by husband) /\\\n(~a_truth (statement_by husband) \\/ ~knight husband \\/ knight c_wife) /\\\n(a_truth (statement_by husband) \\/ ~knight husband) /\\\n(a_truth (statement_by husband) \\/ knight c_wife) /\\\n(~knight c_wife \\/ a_truth (statement_by husband)) ==> ~knight husband ==>\nF"]},

{name = "NUM001-1",
 goal = [








QUOTE "\n(!x. x == x) /\\ (!x y z. ~(x == y) \\/ ~(y == z) \\/ x == z) /\\\n(!x y. x + y == y + x) /\\ (!x y z. x + (y + z) == x + y + z) /\\\n(!x y. x + y - y == x) /\\ (!x y. x == x + y - y) /\\\n(!x y z. x - y + z == x + z - y) /\\ (!x y z. x + y - z == x - z + y) /\\\n(!x y z v. ~(x == y) \\/ ~(z == x + v) \\/ z == y + v) /\\\n(!x y z v. ~(x == y) \\/ ~(z == v + x) \\/ z == v + y) /\\\n(!x y z v. ~(x == y) \\/ ~(z == x - v) \\/ z == y - v) /\\\n(!x y z v. ~(x == y) \\/ ~(z == v - x) \\/ z == v - y) ==>\n~(a + b + c == a + (b + c)) ==> F"]},

{name = "ALG005-1",
 goal = [








QUOTE "\n(!x. x = x) /\\ (!x y. ~(x = y) \\/ y = x) /\\\n(!x y z. ~(x = y) \\/ ~(y = z) \\/ x = z) /\\\n(!x y z. ~(x = y) \\/ x + z = y + z) /\\\n(!x y z. ~(x = y) \\/ z + x = z + y) /\\\n(!x y z. ~(x = y) \\/ x * z = y * z) /\\\n(!x y z. ~(x = y) \\/ z * x = z * y) /\\ (!x y. x + (y + x) = x) /\\\n(!x y. x + (x + y) = y + (y + x)) /\\\n(!x y z. x + y + z = x + z + (y + z)) /\\ (!x y. x * y = x + (x + y)) ==>\n~(a * b * c = a * (b * c)) ==> F"]},

{name = "GRP057-1",
 goal = [






QUOTE "\n(!x. x = x) /\\ (!x y. ~(x = y) \\/ y = x) /\\\n(!x y z. ~(x = y) \\/ ~(y = z) \\/ x = z) /\\\n(!x y z v. x * i (i (i y * (i x * z)) * v * i (y * v)) = z) /\\\n(!x y. ~(x = y) \\/ i x = i y) /\\ (!x y z. ~(x = y) \\/ x * z = y * z) /\\\n(!x y z. ~(x = y) \\/ z * x = z * y) ==>\n~(i a1 * a1 = i b1 * b1) \\/ ~(i b2 * b2 * a2 = a2) \\/\n~(a3 * b3 * c3 = a3 * (b3 * c3)) ==> F"]},

{name = "LCL009-1",
 goal = [


QUOTE "\n(!x y. ~p (x - y) \\/ ~p x \\/ p y) /\\\n(!x y z. p (x - y - (z - y - (x - z)))) ==>\n~p (a - b - c - (a - (b - c))) ==> F"]},

(* ------------------------------------------------------------------------- *)
(* Small problems that are tricky to prove.                                  *)
(* ------------------------------------------------------------------------- *)

{name = "COL060-3",
 goal = [





QUOTE "\n(!x. x = x) /\\ (!x y. ~(x = y) \\/ y = x) /\\\n(!x y z. ~(x = y) \\/ ~(y = z) \\/ x = z) /\\\n(!x y z. b % x % y % z = x % (y % z)) /\\ (!x y. t % x % y = y % x) /\\\n(!x y z. ~(x = y) \\/ x % z = y % z) /\\\n(!x y z. ~(x = y) \\/ z % x = z % y) ==>\n~(b % (b % (t % b) % b) % t % c_x % c_y % c_z = c_y % (c_x % c_z)) ==> F"]},

{name = "COL058-2",
 goal = [






QUOTE "\n(!x. x = x) /\\ (!x y. ~(x = y) \\/ y = x) /\\\n(!x y z. ~(x = y) \\/ ~(y = z) \\/ x = z) /\\\n(!x y. r (r 0 x) y = r x (r y y)) /\\ (!x y z. ~(x = y) \\/ r x z = r y z) /\\\n(!x y z. ~(x = y) \\/ r z x = r z y) ==>\n~(r (r (r 0 (r (r 0 (r 0 0)) (r 0 (r 0 0)))) (r 0 (r 0 0)))\n  (r (r 0 (r (r 0 (r 0 0)) (r 0 (r 0 0)))) (r 0 (r 0 0))) =\n  r (r 0 (r (r 0 (r 0 0)) (r 0 (r 0 0)))) (r 0 (r 0 0))) ==> F"]},

{name = "LCL107-1",
 goal = [




QUOTE "\n(!x y. ~p (x - y) \\/ ~p x \\/ p y) /\\\n(!x y z v w x' y'.\n   p\n   (x - y - z - (v - w - (x' - w - (x' - v) - y')) -\n    (z - (y - x - y')))) ==> ~p (a - b - c - (e - b - (a - e - c))) ==> F"]},

{name = "LDA007-3",
 goal = [






QUOTE "\n(!x. x = x) /\\ (!x y. ~(x = y) \\/ y = x) /\\\n(!x y z. ~(x = y) \\/ ~(y = z) \\/ x = z) /\\\n(!x y z. f x (f y z) = f (f x y) (f x z)) /\\\n(!x y z. ~(x = y) \\/ f x z = f y z) /\\\n(!x y z. ~(x = y) \\/ f z x = f z y) /\\ tt = f t t /\\ ts = f t s /\\\ntt_ts = f tt ts /\\ tk = f t k /\\ tsk = f ts k ==>\n~(f t tsk = f tt_ts tk) ==> F"]},

{name = "GRP010-4",
 goal = [




QUOTE "\n(!x. x = x) /\\ (!x y. ~(x = y) \\/ y = x) /\\\n(!x y z. ~(x = y) \\/ ~(y = z) \\/ x = z) /\\ (!x y. ~(x = y) \\/ i x = i y) /\\\n(!x y z. ~(x = y) \\/ x * z = y * z) /\\\n(!x y z. ~(x = y) \\/ z * x = z * y) /\\ (!x y z. x * y * z = x * (y * z)) /\\\n(!x. 1 * x = x) /\\ (!x. i x * x = 1) /\\ c * b = 1 ==> ~(b * c = 1) ==> F"]},

{name = "ALG006-1",
 goal = [





QUOTE "\n(!x. x = x) /\\ (!x y. ~(x = y) \\/ y = x) /\\\n(!x y z. ~(x = y) \\/ ~(y = z) \\/ x = z) /\\\n(!x y z. ~(x = y) \\/ x + z = y + z) /\\\n(!x y z. ~(x = y) \\/ z + x = z + y) /\\ (!x y. x + (y + x) = x) /\\\n(!x y. x + (x + y) = y + (y + x)) /\\\n(!x y z. x + y + z = x + z + (y + z)) ==> ~(a + c + b = a + b + c) ==> F"]},

{name = "BOO021-1",
 goal = [








QUOTE "\n(!x. x = x) /\\ (!x y. ~(x = y) \\/ y = x) /\\\n(!x y z. ~(x = y) \\/ ~(y = z) \\/ x = z) /\\\n(!x y z. ~(x = y) \\/ x + z = y + z) /\\\n(!x y z. ~(x = y) \\/ z + x = z + y) /\\ (!x y. ~(x = y) \\/ i x = i y) /\\\n(!x y z. ~(x = y) \\/ x * z = y * z) /\\\n(!x y z. ~(x = y) \\/ z * x = z * y) /\\ (!x y. (x + y) * y = y) /\\\n(!x y z. x * (y + z) = y * x + z * x) /\\ (!x. x + i x = 1) /\\\n(!x y. x * y + y = y) /\\ (!x y z. x + y * z = (y + x) * (z + x)) /\\\n(!x. x * i x = 0) ==> ~(b * a = a * b) ==> F"]},

{name = "GEO002-4",
 goal = [











QUOTE "\n(!x y z v. ~between x y z \\/ ~between y v z \\/ between x y v) /\\\n(!x y z. ~equidistant x y z z \\/ x == y) /\\\n(!x y z v w.\n   ~between x y z \\/ ~between v z w \\/\n   between x (outer_pasch y x v w z) v) /\\\n(!x y z v w.\n   ~between x y z \\/ ~between v z w \\/\n   between w y (outer_pasch y x v w z)) /\\\n(!x y z v. between x y (extension x y z v)) /\\\n(!x y z v. equidistant x (extension y x z v) z v) /\\\n(!x y z v. ~(x == y) \\/ ~between z v x \\/ between z v y) ==>\n~between a a b ==> F"]},

{name = "GRP057-1",
 goal = [






QUOTE "\n(!x. x = x) /\\ (!x y. ~(x = y) \\/ y = x) /\\\n(!x y z. ~(x = y) \\/ ~(y = z) \\/ x = z) /\\\n(!x y z v. x * i (i (i y * (i x * z)) * v * i (y * v)) = z) /\\\n(!x y. ~(x = y) \\/ i x = i y) /\\ (!x y z. ~(x = y) \\/ x * z = y * z) /\\\n(!x y z. ~(x = y) \\/ z * x = z * y) ==>\n~(i a1 * a1 = i b1 * b1) \\/ ~(i b2 * b2 * a2 = a2) \\/\n~(a3 * b3 * c3 = a3 * (b3 * c3)) ==> F"]},

{name = "HEN006-3",
 goal = [









QUOTE "\n(!x. x = x) /\\ (!x y. ~(x = y) \\/ y = x) /\\\n(!x y z. ~(x = y) \\/ ~(y = z) \\/ x = z) /\\\n(!x y. ~(x <= y) \\/ x / y = 0) /\\ (!x y. ~(x / y = 0) \\/ x <= y) /\\\n(!x y. x / y <= x) /\\ (!x y z. x / y / (z / y) <= x / z / y) /\\\n(!x. 0 <= x) /\\ (!x y. ~(x <= y) \\/ ~(y <= x) \\/ x = y) /\\ (!x. x <= 1) /\\\n(!x y z. ~(x = y) \\/ x / z = y / z) /\\\n(!x y z. ~(x = y) \\/ z / x = z / y) /\\\n(!x y z. ~(x = y) \\/ ~(x <= z) \\/ y <= z) /\\\n(!x y z. ~(x = y) \\/ ~(z <= x) \\/ z <= y) /\\ a / b <= d ==>\n~(a / d <= b) ==> F"]},

{name = "RNG035-7",
 goal = [











QUOTE "\n(!x. x = x) /\\ (!x y. ~(x = y) \\/ y = x) /\\\n(!x y z. ~(x = y) \\/ ~(y = z) \\/ x = z) /\\ (!x. 0 + x = x) /\\\n(!x. x + 0 = x) /\\ (!x. n x + x = 0) /\\ (!x. x + n x = 0) /\\\n(!x y z. x + (y + z) = x + y + z) /\\ (!x y. x + y = y + x) /\\\n(!x y z. x * (y * z) = x * y * z) /\\\n(!x y z. x * (y + z) = x * y + x * z) /\\\n(!x y z. (x + y) * z = x * z + y * z) /\\\n(!x y z. ~(x = y) \\/ x + z = y + z) /\\\n(!x y z. ~(x = y) \\/ z + x = z + y) /\\ (!x y. ~(x = y) \\/ n x = n y) /\\\n(!x y z. ~(x = y) \\/ x * z = y * z) /\\\n(!x y z. ~(x = y) \\/ z * x = z * y) /\\ (!x. x * (x * (x * x)) = x) ==>\na * b = c /\\ ~(b * a = c) ==> F"]},

{name = "ROB001-1",
 goal = [






QUOTE "\n(!x. x = x) /\\ (!x y. ~(x = y) \\/ y = x) /\\\n(!x y z. ~(x = y) \\/ ~(y = z) \\/ x = z) /\\ (!x y. x + y = y + x) /\\\n(!x y z. x + y + z = x + (y + z)) /\\\n(!x y. n (n (x + y) + n (x + n y)) = x) /\\\n(!x y z. ~(x = y) \\/ x + z = y + z) /\\\n(!x y z. ~(x = y) \\/ z + x = z + y) /\\ (!x y. ~(x = y) \\/ n x = n y) ==>\n~(n (a + n b) + n (n a + n b) = b) ==> F"]},

{name = "GRP128-4.003",
 goal = [
















QUOTE "\n(!x y.\n   ~elt x \\/ ~elt y \\/ product e_1 x y \\/ product e_2 x y \\/\n   product e_3 x y) /\\\n(!x y.\n   ~elt x \\/ ~elt y \\/ product x e_1 y \\/ product x e_2 y \\/\n   product x e_3 y) /\\ elt e_1 /\\ elt e_2 /\\ elt e_3 /\\ ~(e_1 == e_2) /\\\n~(e_1 == e_3) /\\ ~(e_2 == e_1) /\\ ~(e_2 == e_3) /\\ ~(e_3 == e_1) /\\\n~(e_3 == e_2) /\\\n(!x y.\n   ~elt x \\/ ~elt y \\/ product x y e_1 \\/ product x y e_2 \\/\n   product x y e_3) /\\\n(!x y z v. ~product x y z \\/ ~product x y v \\/ z == v) /\\\n(!x y z v. ~product x y z \\/ ~product x v z \\/ y == v) /\\\n(!x y z v. ~product x y z \\/ ~product v y z \\/ x == v) ==>\n(!x y z v. product x y z \\/ ~product x z v \\/ ~product z y v) /\\\n(!x y z v. product x y z \\/ ~product v x z \\/ ~product v y x) /\\\n(!x y z v. ~product x y z \\/ ~product z y v \\/ product x z v) ==> F"]},

{name = "NUM014-1",
 goal = [






QUOTE "\n(!x. product x x (square x)) /\\\n(!x y z. ~product x y z \\/ product y x z) /\\\n(!x y z. ~product x y z \\/ divides x z) /\\\n(!x y z v.\n   ~prime x \\/ ~product y z v \\/ ~divides x v \\/ divides x y \\/\n   divides x z) /\\ prime a /\\ product a (square c) (square b) ==>\n~divides a b ==> F"]}

];

(* ========================================================================= *)
(* A FEW SAMPLE THEOREMS TO CHECK LARGE RUNS                                 *)
(* ========================================================================= *)

(* val quick =
 *   [extract nonequality "TRUE",
 *    extract nonequality "P_or_not_P",
 *    extract nonequality "JH_test",
 *    extract nonequality "CYCLIC",
 *    extract nonequality "MN_bug",
 *    extract nonequality "ERIC",
 *    extract nonequality "MATHS4_EXAMPLE",
 *    extract nonequality "P18",
 *    extract nonequality "P39",
 *    extract nonequality "P59",
 *    extract nonequality "DAVIS_PUTNAM_EXAMPLE",
 *    extract nonequality "BAD_CONNECTIONS",
 * 
 *    extract equality "TRANS_SYMM",
 *    extract equality "CYCLIC_SUBSTITUTION_BUG",
 *    extract equality "P48"];
 *)
end
(*#line 0.0 "src/Meter1.sig"*)
(* ========================================================================= *)
(* METERING TIME AND INFERENCES                                              *)
(* Created by Joe Hurd, November 2001                                        *)
(* ========================================================================= *)

signature Meter1 =
sig

type 'a pp = 'a Useful.pp

(* Search limits *)
type limit          = {time : real option, infs : int option}
val unlimited       : limit
val expired         : limit
val limit_to_string : limit -> string

(* Meter readings *)
type meter_reading          = {time : real, infs : int}
val zero_reading            : meter_reading
val add_readings            : meter_reading -> meter_reading -> meter_reading
val pp_meter_reading        : meter_reading pp
val meter_reading_to_string : meter_reading -> string

(* Meters record time and inferences *)
type meter
val new_meter   : limit -> meter
val sub_meter   : meter -> limit -> meter
val record_infs : meter -> int -> unit
val read_meter  : meter -> meter_reading
val check_meter : meter -> bool
val pp_meter    : meter pp

end
(*#line 0.0 "src/Meter1.sml"*)
(* ========================================================================= *)
(* METERING TIME AND INFERENCES                                              *)
(* Created by Joe Hurd, November 2001                                        *)
(* ========================================================================= *)

(*
app load
 ["Useful", "Mosml", "Term1", "Thm1", "Canon1", "Match1"];
*)

(*
*)
structure Meter1 :> Meter1 =
struct

open Useful;

infix |-> ::> @> oo ## ::* ::@;

(* ------------------------------------------------------------------------- *)
(* Search limits                                                             *)
(* ------------------------------------------------------------------------- *)

type limit = {time : real option, infs : int option};

val unlimited = {time = NONE, infs = NONE};

val expired = {time = SOME 0.0, infs = SOME 0};

fun limit_to_string {time, infs} =
  "{time = " ^
   (case time of NONE => "unlimited"
    | SOME r => Real.fmt (StringCvt.FIX (SOME 3)) r ^ "s") ^
   ", infs = " ^
   (case infs of NONE => "unlimited" | SOME i => int_to_string i) ^
   "}";

(* ------------------------------------------------------------------------- *)
(* Meter readings.                                                           *)
(* ------------------------------------------------------------------------- *)

type meter_reading = {time : real, infs : int};

val zero_reading = {time = 0.0, infs = 0};

fun add_readings {time : real, infs} {time = time', infs = infs'} =
  {time = time + time', infs = infs + infs'};

fun pp_meter_reading pp {time, infs} =
  let
    open PP
    val () = begin_block pp INCONSISTENT 1
    val () = add_string pp "{";
    val () = begin_block pp INCONSISTENT 2
    val () = add_string pp "time ="
    val () = add_break pp (1, 0)
    val () = add_string pp (Real.fmt (StringCvt.FIX (SOME 3)) time)
    val () = end_block pp
    val () = add_string pp ","
    val () = add_break pp (1, 0)
    val () = begin_block pp INCONSISTENT 2
    val () = add_string pp "infs ="
    val () = add_break pp (1, 0)
    val () = pp_int pp infs
    val () = end_block pp
    val () = add_string pp "}"
    val () = end_block pp
  in
    ()
  end;

fun meter_reading_to_string r =
  PP.pp_to_string (!LINE_LENGTH) pp_meter_reading r;

(* ------------------------------------------------------------------------- *)
(* Meters record time and inferences.                                        *)
(* ------------------------------------------------------------------------- *)

type meter = {read : unit -> meter_reading, log : (int -> unit), lim : limit};

fun new_time_meter () =
  let
    val tmr = Timer.startCPUTimer ()
    fun read () =
      (fn {usr, sys, ...} => Time.toReal (Time.+ (usr, sys)))
      (Timer.checkCPUTimer tmr)
  in
    read
  end;

fun new_inference_meter () =
  let
    val infs = ref 0
    fun read () = !infs
  in
    (read, fn n => infs := !infs + n)
  end;

fun new_meter lim : meter =
  let
    val tread = new_time_meter ()
    val (iread, ilog) = new_inference_meter ()
  in
    {read = (fn () => {time = tread (), infs = iread ()}),
     log = ilog, lim = lim}
  end;

fun sub_meter {read, log, lim = _} lim =
  let
    val {time = init_time : real, infs = init_infs} = read ()
    fun sub {time, infs} = {time = time - init_time, infs = infs - init_infs}
  in
    {read = sub o read, log = log, lim = lim}
  end;

val read_meter = fn ({read, ...} : meter) => read ();

val check_meter = fn ({read, lim = {time, infs}, ...} : meter) =>
  let
    val {time = t, infs = i} = read ()
  in
    (case time of NONE => true | SOME time => t < time) andalso
    (case infs of NONE => true | SOME infs => i < infs)
  end;

val record_infs = fn ({log, ...} : meter) => log;

val pp_meter = pp_map read_meter pp_meter_reading;

end
(*#line 0.0 "src/Solver1.sig"*)
(* ========================================================================= *)
(* PACKAGING UP SOLVERS TO ALLOW THEM TO COOPERATE UNIFORMLY                 *)
(* Created by Joe Hurd, March 2002                                           *)
(* ========================================================================= *)

signature Solver1 =
sig

type 'a pp         = 'a Useful.pp
type 'a stream     = 'a Stream.stream
type formula       = Term1.formula
type thm           = Thm1.thm
type limit         = Meter1.limit
type meter         = Meter1.meter
type meter_reading = Meter1.meter_reading
type units         = Units1.units

(* The type of a generic solver *)

type solver = formula list -> thm list option stream

val contradiction_solver : thm -> solver

(* Filters to cut off searching or drop subsumed solutions *)

val solved_filter   : solver -> solver
val subsumed_filter : solver -> solver

(* User-friendly interface to generic solvers *)

val solve  : solver -> formula list -> thm list list
val find   : solver -> formula list -> thm list option
val refute : solver -> thm option

(* Solver nodes must construct themselves from the following form. *)

type form =
  {slice : meter ref,                   (* A meter to stop after each slice *)
   units : units ref,                   (* Solvers share a unit cache *)
   thms  : thm list,                    (* Context, assumed consistent *)
   hyps  : thm list}                    (* Hypothesis, or set of support *)

(* Solver nodes also incorporate a name. *)

type node_data = {name : string, solver_con : form -> solver}
type solver_node

val mk_solver_node : node_data -> solver_node
val pp_solver_node : solver_node pp

(* At each step we schedule a time slice to the least cost solver node. *)

val SLICE : limit ref

type cost_fn = meter_reading -> real

val time1 : cost_fn                     (* Time taken (in seconds) *)
val time2 : cost_fn                     (* Time squared *)
val infs1 : cost_fn                     (* Number of inferences made*)
val infs2 : cost_fn                     (* Inferences squared *)

(* This allows us to hierarchically arrange solver nodes. *)

val combine : (cost_fn * solver_node) list -> solver_node

(* Overriding the 'natural' set of support from the problem. *)

val set_of_support : (thm -> bool) -> solver_node -> solver_node
val everything     : thm -> bool
val one_negative   : thm -> bool
val one_positive   : thm -> bool
val all_negative   : thm -> bool        (* This one is used by Metis1.prove *)
val all_positive   : thm -> bool
val nothing        : thm -> bool

(* Initializing a solver node makes it ready for action. *)

type init_data = {limit : limit, thms : thm list, hyps : thm list}

val initialize : solver_node -> init_data -> solver

end
(*#line 0.0 "src/Solver1.sml"*)
(* ========================================================================= *)
(* PACKAGING UP SOLVERS TO ALLOW THEM TO COOPERATE UNIFORMLY                 *)
(* Created by Joe Hurd, March 2002                                           *)
(* ========================================================================= *)

(*
app load
 ["Useful", "Mosml", "Term1", "Thm1", "Canon1", "Match1", "Meter1", "Units1",
  "Solver1"];
*)

(*
*)
structure Solver1 :> Solver1 =
struct

open Useful Term1 Match1 Thm1 Meter1;

infix |-> ::> @> oo ##;

structure S = Stream;
structure U = Units1;

type 'a stream = 'a S.stream;
type units     = U.units;

val |<>|   = Subst1.|<>|;
val op ::> = Subst1.::>;

(* ------------------------------------------------------------------------- *)
(* Chatting.                                                                 *)
(* ------------------------------------------------------------------------- *)

val () = traces := {module = "Solver1", alignment = K 1} :: !traces;

fun chat l m = trace {module = "Solver1", message = m, level = l};

(* ------------------------------------------------------------------------- *)
(* Helper functions.                                                         *)
(* ------------------------------------------------------------------------- *)

fun drop_after f =
  S.fold (fn x => fn xs => S.CONS (x, if f x then K S.NIL else xs)) S.NIL;

fun time_to_string t =
  let val dp = if t < 10.0 then 2 else if t < 1000.0 then 1 else 0
  in Real.fmt (StringCvt.FIX (SOME dp)) t
  end;

fun infs_to_string i =
  if i < 10000 then int_to_string i
  else if i < 10000000 then int_to_string (i div 1000) ^ "K"
  else int_to_string (i div 1000000) ^ "M";

val name_to_string = str o hd o explode;

fun option_case n _ NONE = n
  | option_case _ s (SOME _) = s;

(* ------------------------------------------------------------------------- *)
(* The type of a generic solver.                                             *)
(* ------------------------------------------------------------------------- *)

type solver = formula list -> thm list option stream;

local
  fun contr th [False] = [th]
    | contr th gs = map (C CONTR th) gs;
in
  fun contradiction_solver th =
    (assert (is_contradiction th) (ERR "contradiction_solver" "thm not |- F");
     fn gs => S.CONS (SOME (contr th gs), K S.NIL));
end;

(* ------------------------------------------------------------------------- *)
(* Filters to cut off searching or drop subsumed solutions.                  *)
(* ------------------------------------------------------------------------- *)

local
  fun concl [] = False
    | concl [lit] = lit
    | concl _ = raise BUG "concl" "not a literal";
in
  fun solved_filter solver goals =
    let
      fun solves goals' = can (matchl_literals |<>|) (zip goals' goals)
      fun final NONE = false
        | final (SOME ths) = solves (map (concl o clause) ths)
    in
      drop_after final (solver goals)
    end;
end;

local
  fun munge s n = "MUNGED__" ^ int_to_string n ^ "__" ^ s;
  fun munge_lit (n, Atom (Fn (p, a))) = Atom (Fn (munge p n, a))
    | munge_lit (n, Not (Atom (Fn (p, a)))) = Not (Atom (Fn (munge p n, a)))
    | munge_lit _ = raise BUG "munge_lit" "bad literal";
  fun distinctivize fms = map munge_lit (enumerate 0 fms);    
  fun advance NONE s = (SOME NONE, s)
    | advance (SOME ths) s =
    let
      val fms = distinctivize (List.mapPartial (total dest_unit) ths)
    in
      if non null (Subsume1.subsumed s fms) then (NONE, s)
      else (SOME (SOME ths), Subsume1.add (fms |-> ()) s)
    end
    handle ERR_EXN _ => raise BUG "advance" "shouldn't fail";
in
  fun subsumed_filter s g = S.partial_maps advance Subsume1.empty (s g);
end;

(* ------------------------------------------------------------------------- *)
(* User-friendly interface to generic solvers                                *)
(* ------------------------------------------------------------------------- *)

fun raw_solve s = S.partial_map I o (subsumed_filter (solved_filter s));

fun solve s = S.to_list o (raw_solve s);

fun find s = (fn S.NIL => NONE | S.CONS (x, _) => SOME x) o raw_solve s;

fun refute s = Option.map unwrap (find s [False]);

(* ------------------------------------------------------------------------- *)
(* Solver nodes must construct themselves from the following form.           *)
(* ------------------------------------------------------------------------- *)

type form =
  {slice : meter ref,                   (* A meter to stop after each slice *)
   units : units ref,                   (* Solvers share a unit cache *)
   thms  : thm list,                    (* Context, assumed consistent *)
   hyps  : thm list};                   (* Hypothesis, no assumptions *)

(* ------------------------------------------------------------------------- *)
(* Solver nodes also incorporate a name.                                     *)
(* ------------------------------------------------------------------------- *)

type node_data = {name : string, solver_con : form -> solver};

datatype solver_node =
  Solver_node of {name : string, initial : string, solver_con : form -> solver};

fun mk_solver_node {name, solver_con} =
  Solver_node
  {name = name, initial = (str o hd o explode) name, solver_con = solver_con};

val pp_solver_node = pp_map (fn Solver_node {name, ...} => name) pp_string;

(* ------------------------------------------------------------------------- *)
(* At each step we schedule a time slice to the least cost solver node.      *)
(* ------------------------------------------------------------------------- *)

val SLICE : limit ref = ref {time = SOME (1.0 / 3.0), infs = NONE};

type cost_fn = Meter1.meter_reading -> real;

local
  fun sq x : real = x * x;
in
  val time1 : cost_fn = fn {time, ...} => time;
  val time2 : cost_fn = fn {time, ...} => sq time;
  val infs1 : cost_fn = fn {infs, ...} => Real.fromInt infs;
  val infs2 : cost_fn = fn {infs, ...} => sq (Real.fromInt infs);
end;

(* ------------------------------------------------------------------------- *)
(* This allows us to hierarchically arrange solver nodes.                    *)
(* ------------------------------------------------------------------------- *)

local
  fun name (Solver_node {name, ...}) = name;
  fun initial (Solver_node {initial, ...}) = initial;
  fun seq f [] = ""
    | seq f (h :: t) = foldl (fn (n, s) => s ^ "," ^ f n) (f h) t;
in
  fun combine_names csolvers = "[" ^ seq (name o snd) csolvers ^ "]";
  fun combine_initials csolvers = "[" ^ seq (initial o snd) csolvers ^ "]";
end;

datatype subnode = Subnode of
  {name   : string,
   used   : meter_reading,
   cost   : meter_reading -> real,
   solns  : (unit -> thm list option stream) option};

fun init_subnode (cost, (name, solver : solver)) goal =
  Subnode
  {name = name,
   used = zero_reading,
   cost = cost,
   solns = SOME (fn () => solver goal)};

fun least_cost [] = K NONE
  | least_cost _ =
  (SOME o snd o min (fn (r, _) => fn (s, _) => r <= s) o
   map (fn (n, Subnode {used, cost, ...}) => (cost used, n)))

val choose_subnode =
  W least_cost o
  List.filter (fn (_, Subnode {solns, ...}) => Option.isSome solns) o
  enumerate 0;

fun subnode_info (Subnode {name, used = {time, infs}, solns, ...}) =
  name_to_string name ^ "(" ^ time_to_string time ^ "," ^
  infs_to_string infs ^ ")" ^ (case solns of NONE => "*" | SOME _ => "");

local
  fun seq f [] = ""
    | seq f (h :: t) = foldl (fn (n, s) => s ^ "--" ^ f n) (f h) t;
in
  fun status_info subnodes units =
    "[" ^ seq subnode_info subnodes ^ "]--u=" ^ U.info units ^ "--";
end;

fun schedule check read stat =
  let
    fun sched nodes =
      (chat 2 (stat nodes);
       if not (check ()) then
         (chat 1 "?\n"; S.CONS (NONE, fn () => sched nodes))
       else
         case choose_subnode nodes of NONE => (chat 1 "!\n"; S.NIL)
         | SOME n =>
           let
             val Subnode {name, used, solns, cost} = List.nth (nodes, n)
             val () = chat 1 name
             val seq = (Option.valOf solns) ()
             val r = read ()
             val () = chat 2 ("--t=" ^ time_to_string (#time r) ^ "\n")
             val used = add_readings used r
             val (res, solns) =
               case seq of S.NIL => (NONE, NONE) | S.CONS (a, r) => (a, SOME r)
             val node =
               Subnode {name = name, used = used, cost = cost, solns = solns}
             val nodes = update_nth (K node) n nodes
             val () =
               case res of NONE => ()
               | SOME _ => (chat 2 (stat nodes); chat 1 "$\n")
           in
             S.CONS (res, fn () => sched nodes)
           end)
  in
    sched
  end;

fun combine_solvers (n, i) csolvers {slice, units, thms, hyps} =
  let
    val () = chat 2
      (n ^ "--initializing--#thms=" ^ int_to_string (length thms) ^
       "--#hyps=" ^ int_to_string (length hyps) ^ ".\n")
    val meter = ref (new_meter expired)
    fun f (Solver_node {initial, solver_con, ...}) =
      (initial,
       solver_con {slice = meter, units = units, thms = thms, hyps = hyps})
    val cnodes = map (I ## f) csolvers
    fun check () =
      check_meter (!slice) andalso (meter := sub_meter (!slice) (!SLICE); true)
    fun read () = read_meter (!meter)
    fun stat s = status_info s (!units)
  in
    fn goal => schedule check read stat (map (C init_subnode goal) cnodes)
  end;

fun combine csolvers =
  let
    val n = combine_names csolvers
    val i = combine_initials csolvers
  in
    Solver_node
    {name = n, initial = i, solver_con = combine_solvers (n, i) csolvers}
  end;

(* ------------------------------------------------------------------------- *)
(* Overriding the 'natural' set of support from the problem.                 *)
(* ------------------------------------------------------------------------- *)

fun sos_solver_con filt name solver_con {slice, units, thms, hyps} =
  let
    val () = chat 2
      (name ^ "--initializing--#thms=" ^ int_to_string (length thms) ^
       "--#hyps=" ^ int_to_string (length hyps) ^ ".\n")
    val (hyps', thms') = List.partition filt (thms @ hyps)
  in
    solver_con {slice = slice, units = units, thms = thms', hyps = hyps'}
  end;

fun set_of_support filt (Solver_node {name, initial, solver_con}) =
  let val name' = "!" ^ name
  in
    Solver_node
    {name = name', initial = initial,
     solver_con = sos_solver_con filt name' solver_con}
  end;

val everything : thm -> bool = K true;

val one_negative = (fn x => null x orelse List.exists negative x) o clause;

val one_positive = (fn x => null x orelse List.exists positive x) o clause;

val all_negative = List.all negative o clause;

val all_positive = List.all positive o clause;

val nothing : thm -> bool = K false;

(* ------------------------------------------------------------------------- *)
(* Initializing a solver node makes it ready for action.                     *)
(* ------------------------------------------------------------------------- *)

type init_data = {limit : limit, thms : thm list, hyps : thm list}

fun initialize (Solver_node {solver_con, ...}) {limit, thms, hyps} =
  case List.find is_contradiction (thms @ hyps) of SOME th
    => contradiction_solver th
  | NONE =>
    let
      val meter = ref (new_meter expired)
      val units = ref U.empty
      val solver =
        solver_con {slice = meter, units = units, thms = thms, hyps = hyps}
    in
      fn g =>
      let val () = meter := new_meter limit
      in drop_after (fn _ => not (check_meter (!meter))) (solver g)
      end
    end;

end
(*#line 0.0 "src/Meson1.sig"*)
(* ========================================================================= *)
(* THE MESON PROOF PROCEDURE                                                 *)
(* Created by Joe Hurd, November 2001                                        *)
(* Partly ported from the CAML-Light code accompanying John Harrison's book  *)
(* ========================================================================= *)

signature Meson1 =
sig

type solver_node = Solver1.solver_node

(* Tuning parameters *)
type parameters =
  {ancestor_pruning : bool,
   ancestor_cutting : bool,
   state_simplify   : bool,
   cache_cutting    : bool,
   divide_conquer   : bool,
   unit_lemmaizing  : bool}

val defaults : parameters

(* The meson solver *)
val meson' : parameters -> solver_node
val meson  : solver_node                          (* Uses defaults *)

(* The delta preprocessor as a solver *)
val delta' : parameters -> solver_node
val delta  : solver_node                          (* Uses defaults *)

(* The prolog solver *)
val prolog' : parameters -> solver_node
val prolog  : solver_node                         (* Uses defaults *)

end
(*#line 0.0 "src/Meson1.sml"*)
(* ========================================================================= *)
(* THE MESON PROOF PROCEDURE                                                 *)
(* Created by Joe Hurd, November 2001                                        *)
(* Partly ported from the CAML-Light code accompanying John Harrison's book  *)
(* ========================================================================= *)

(*
app load
 ["Useful", "Stream", "Mosml", "Term1", "Thm1", "Canon1", "Match1",
  "Solver1", "Meter1", "Units1"];
*)

(*
*)
structure Meson1 :> Meson1 =
struct

open Useful Term1 Match1 Thm1 Canon1 Meter1 Solver1;

infix |-> ::> @> oo ##;

structure S = Stream;
structure N = LiteralNet1;
structure U = Units1;

val |<>|          = Subst1.|<>|;
val op ::>        = Subst1.::>;
val formula_subst = Subst1.formula_subst;

(* ------------------------------------------------------------------------- *)
(* Chatting.                                                                 *)
(* ------------------------------------------------------------------------- *)

val () = traces := {module = "Meson1", alignment = K 1} :: !traces;

fun chat l m = trace {module = "Meson1", message = m, level = l};

(* ------------------------------------------------------------------------- *)
(* Tuning parameters.                                                        *)
(* ------------------------------------------------------------------------- *)

type parameters =
  {ancestor_pruning : bool,
   ancestor_cutting : bool,
   state_simplify   : bool,
   cache_cutting    : bool,
   divide_conquer   : bool,
   unit_lemmaizing  : bool};

val defaults = 
  {ancestor_pruning = true,
   ancestor_cutting = true,
   state_simplify   = true,
   cache_cutting    = true,
   divide_conquer   = true,
   unit_lemmaizing  = true};

(* ------------------------------------------------------------------------- *)
(* Helper functions.                                                         *)
(* ------------------------------------------------------------------------- *)

fun halves n = let val n1 = n div 2 in (n1, n - n1) end;

fun splittable [] = false
  | splittable [_] = false
  | splittable _ = true;

(*
fun protect r f x =
  let
    val v = !r
    val y = f x handle e as ERR_EXN _ => (r := v; raise e)
    val () = r := v
  in
    y
  end;

fun until p =
  let
    open Stream
    fun u NIL = NIL
      | u (CONS (x, xs)) = CONS (x, if p x then K NIL else fn () => u (xs ()))
  in
    u
  end;
*)

local
  val prefix = "_m";
in
  val mk_mvar      = mk_prefix prefix o int_to_string;
  fun mk_mvars n i = map (Var o mk_mvar) (interval n i);
  val dest_mvar    = string_to_int o dest_prefix prefix;
end;

datatype 'a choice = CHOICE of unit -> 'a * 'a choice;

fun dest_choice (CHOICE c) = c;

val no_choice = (fn () => raise ERR "no_choice" "always fails");

fun binary_choice f g =
  (fn () =>
   let val (a, c) = f () in (a, CHOICE (binary_choice (dest_choice c) g)) end
   handle ERR_EXN _ => g ());

fun first_choice [] = no_choice
  | first_choice [f] = f
  | first_choice (f :: fs) = binary_choice f (first_choice fs);

fun choice_stream f =
  let val (a, CHOICE c) = f () in S.CONS (a, fn () => choice_stream c) end
  handle ERR_EXN _ => S.NIL;

fun swivel m n l =
  let
    val (l1, l') = split l m
    val (l2, l3) = split l' n
  in
    l2 @ l1 @ l3
  end;

fun thm_proves th False = is_contradiction th
  | thm_proves th goal =
  case clause th of [lit] => lit = goal | [] => true | _ => false;

fun filter_meter meter =
  S.filter (fn a => Option.isSome a orelse not (check_meter (!meter)));

(* ------------------------------------------------------------------------- *)
(* Compiling the rule set used by meson.                                     *)
(* ------------------------------------------------------------------------- *)

type rule = {asms : formula list, c : formula, thm : thm, asmn : int};

datatype rules = Rules of rule N.literal_map;

fun dest_rules (Rules r) = r;
val empty_rules = Rules N.empty;
val num_all_rules = N.size o dest_rules;
val num_initial_rules = #f o N.size_profile o dest_rules;
fun num_rules r = num_all_rules r - num_initial_rules r;
val rules_unify = N.unify o dest_rules;

val pp_rules =
  pp_map dest_rules
  (N.pp_literal_map
   (pp_map (fn {asms, c, ...} => (asms, c))
    (pp_binop " ==>" (pp_list pp_formula) pp_formula)));

fun add_contrapositives chosen sos th (Rules ruls) =
  let
    val th = FRESH_VARS th
    val lits = clause th
    val lits' = map negate lits
    val base = map (fn l => (subtract lits' [negate l], l)) (chosen lits)
    val contrs = if sos then (lits', False) :: base else base
    fun f (a, c) = c |-> {asms = a, c = c, thm = th, asmn = length a}
  in
    Rules (foldl (fn (h, t) => N.insert (f h) t) ruls contrs)
  end;

fun thms_to_rules chosen thms hyps =
  let val f = uncurry o add_contrapositives chosen
  in foldl (f true) (foldl (f false) empty_rules thms) hyps
  end;

val meson_rules = thms_to_rules I;

val prolog_rules = thms_to_rules (wrap o hd);

(* ------------------------------------------------------------------------- *)
(* Creating the delta goals.                                                 *)
(* ------------------------------------------------------------------------- *)

val thms_to_delta_goals =
  List.concat o
  map (fn (f,n) => [Atom (Fn (f,new_vars n)), Not (Atom (Fn (f,new_vars n)))]) o
  foldl (uncurry union) [] o
  map relations o
  List.concat o
  map clause;

(* ------------------------------------------------------------------------- *)
(* The state passed around by meson.                                         *)
(* ------------------------------------------------------------------------- *)

type state = {env : subst, depth : int, proof : thm list, offset : int};

fun update_env f ({env, depth, proof, offset} : state) =
  {env = f env, depth = depth, proof = proof, offset = offset};

fun update_depth f ({env, depth, proof, offset} : state) =
  {env = env, depth = f depth, proof = proof, offset = offset};

fun update_proof f ({env, depth, proof, offset} : state) =
  {env = env, depth = depth, proof = f proof, offset = offset};

fun update_offset f ({env, depth, proof, offset} : state) =
  {env = env, depth = depth, proof = proof, offset = f offset};

(* ------------------------------------------------------------------------- *)
(* Ancestor pruning.                                                         *)
(* ------------------------------------------------------------------------- *)

fun ancestor_prune false _ _ = K false
  | ancestor_prune true env g =
  let
    val g' = formula_subst env g
    fun check a' = a' = g'
  in
    List.exists (check o formula_subst env)
  end;

(* ------------------------------------------------------------------------- *)
(* Ancestor cutting.                                                         *)
(* ------------------------------------------------------------------------- *)

fun ancestor_cut false _ _ = K false
  | ancestor_cut true env g =
  let
    val g' = negate (formula_subst env g)
    fun check a' = a' = g'
  in
    List.exists (check o formula_subst env)
  end;

(* ------------------------------------------------------------------------- *)
(* Cache cutting.                                                            *)
(* ------------------------------------------------------------------------- *)

fun cache_cont c ({offset, ...} : state) =
  let
    fun f v = case total dest_mvar v of NONE => true | SOME n => n < offset
    val listify = Subst1.foldr (fn m as v |-> _ => if f v then cons m else I) []
    val mem = ref []
    fun purify (s as {env, depth = n, ...} : state) =
      let
        val l = listify env
        fun p (n', l') = n <= n' andalso l = l'
      in
        if List.exists p (!mem) then raise ERR "cache_cut" "repetition"
        else (mem := (n, l) :: (!mem); update_env (K (Subst1.from_maplets l)) s)
      end
  in
    c o purify
  end;

fun cache_cut false = I
  | cache_cut true =
  fn f => fn a => fn g => fn c => fn s => f a g (cache_cont c s) s;

(* ------------------------------------------------------------------------- *)
(* Unit clause shortcut.                                                     *)
(* ------------------------------------------------------------------------- *)

fun grab_unit units (s as {proof = th :: _, ...} : state) =
  (units := U.add th (!units); s)
  | grab_unit _ {proof = [], ...} = raise BUG "grab_unit" "no thms";

fun use_unit units g c (s as {env, ...}) =
  let val prove = partial (ERR "use_unit" "NONE") (U.prove (!units))
  in c (update_proof (cons (unwrap (prove [formula_subst env g]))) s)
  end;

fun unit_cut false _ = I
  | unit_cut true units =
  fn f => fn a => fn g => fn c => fn s =>
  use_unit units g c s handle ERR_EXN _ => f a g (c o grab_unit units) s;

(* ------------------------------------------------------------------------- *)
(* The core of meson: ancestor unification or Prolog-style extension.        *)
(* ------------------------------------------------------------------------- *)

fun freshen_rule ({thm, asms, c, ...} : rule) i =
  let
    val fvs = FVL (c :: asms)
    val fvn = length fvs
    val mvs = mk_mvars i fvn
    val sub = Subst1.from_maplets (zipwith (curry op|->) fvs mvs)
  in
    ((INST sub thm, map (formula_subst sub) asms, formula_subst sub c), i + fvn)
  end;

fun reward r = update_depth (fn n => n + r);

fun spend m f c (s as {depth = n, ...} : state) =
  let
    val low = n - m
    val () = assert (0 <= low) (ERR "meson" "impossible divide and conquer")
    fun check (s' as {depth = n', ...} : state) =
      if n' <= low then s' else raise ERR "meson" "divide and conquer"
  in
    f (c o check) s
  end;

local
  fun unify env (th, asms, c) g = (th, asms, unify_literals env c g)

  fun match env (th, asms, c) g =
    let val sub = match_literals c g
    in (INST sub th, map (formula_subst sub) asms, env)
    end;
in
  fun next_state false env r g = unify env r g
    | next_state true env r g = match env r g handle ERR_EXN _ => unify env r g;
end;

local
  fun mp _ th [] p = FACTOR th :: p
    | mp env th (g :: gs) (th1 :: p) =
    mp env (RESOLVE (formula_subst env g) (INST env th1) th) gs p
    | mp _ _ (_ :: _) [] = raise BUG "modus_ponens" "fresh out of thms"
in
  fun modus_ponens th gs (state as {env, ...}) =
    update_proof (mp env (INST env th) (rev gs)) state;
end;

fun swivelp m n = update_proof (swivel m n);

fun meson_expand {parm : parameters, rules, cut, meter, saturated} =
  let
    fun expand ancestors g cont (state as {env, ...}) =
      if not (check_meter (!meter)) then
        (NONE, CHOICE (fn () => expand ancestors g cont state))
      else if ancestor_prune (#ancestor_pruning parm) env g ancestors then
        raise ERR "meson" "ancestor pruning"
      else if ancestor_cut (#ancestor_cutting parm) env g ancestors then
        (record_infs (!meter) 1; cont (update_proof (cons (ASSUME g)) state))
      else
        let
        (*val () = print ("meson: " ^ formula_to_string g ^ ".\n")*)
          fun reduction a () =
            let
              val state = update_env (K (unify_literals env g (negate a))) state
              val state = update_proof (cons (ASSUME g)) state
            in
              (record_infs (!meter) 1; cont state)
            end
          val expansion = expand_rule ancestors g cont state
        in
          first_choice
          (map reduction ancestors @
           map expansion (rules_unify rules (formula_subst env g))) ()
        end
    and expand_rule ancestors g cont {env, depth, proof, offset} r () =
      let
        val depth = depth - #asmn r
        val () =
          if 0 <= depth then ()
          else (saturated := false; raise ERR "meson" "too deep")
        val (r, offset) = freshen_rule r offset
        val (th, asms, env) = next_state (#state_simplify parm) env r g
        val () = record_infs (!meter) 1
      in
        expands (g :: ancestors) asms (cont o modus_ponens th asms)
        {env = env, depth = depth, proof = proof, offset = offset}
      end
    and expands ancestors g c (s as {depth = n, ...}) =
      if #divide_conquer parm andalso splittable g then
        let
          val (l1, l2) = halves (length g)
          val (g1, g2) = split g l1
          val (f1, f2) = Df (expands ancestors) (g1, g2)
          val (n1, n2) = halves n
          val s = update_depth (K n1) s
        in
          binary_choice
          (fn () => f1 (f2 c o reward n2) s)
          (fn () => f2 (spend (n1 + 1) f1 (c o swivelp l1 l2) o reward n2) s) ()
        end
      else foldl (uncurry (cut expand ancestors)) c (rev g) s
  in
    cut expand []
  end;

(* ------------------------------------------------------------------------- *)
(* Full meson procedure.                                                     *)
(* ------------------------------------------------------------------------- *)

fun meson_finally g ({env, proof, ...} : state) =
  let
    val () = assert (length proof = length g) (BUG "meson" "bad final state")
    val g' = map (formula_subst env) g
    val proof' = map (INST env) (rev proof)
  (*val () = (print "meson_finally: "; printVal (g', proof'); print ".\n")*)
    val () =
      assert (List.all (uncurry thm_proves) (zip proof' g'))
      (BUG "meson" "did not prove goal list")
  in
    (SOME (FRESH_VARSL proof'), CHOICE no_choice)
  end;

fun raw_meson system goals depth =
  choice_stream
  (fn () =>
   foldl (uncurry (meson_expand system)) (meson_finally goals) (rev goals)
   {env = |<>|, depth = depth, proof = [], offset = 0});

(* ------------------------------------------------------------------------- *)
(* Raw solvers.                                                              *)
(* ------------------------------------------------------------------------- *)

type 'a system =
  {parm : parameters, rules : rules, meter : meter ref, saturated : bool ref,
   cut :
     (formula list -> formula -> (state -> 'a) -> state -> 'a) ->
      formula list -> formula -> (state -> 'a) -> state -> 'a};

fun mk_system parm units meter rules : 'a system =
  let
    val {cache_cutting = caching, unit_lemmaizing = lemmaizing, ...} = parm
  in
    {parm      = parm,
     rules     = rules,
     meter     = meter,
     saturated = ref false,
     cut       = unit_cut lemmaizing units o cache_cut caching}
  end;

fun meson' parm =
  mk_solver_node
  {name = "meson",
   solver_con =
   fn {slice, units, thms, hyps} =>
   let
     val ruls = meson_rules thms hyps
     val () = chat 2
       ("meson--initializing--#thms=" ^ int_to_string (length thms) ^
        "--#hyps=" ^ int_to_string (length hyps) ^
        "--#rules=" ^ int_to_string (num_rules ruls) ^
        "--#initial_rules=" ^ int_to_string (num_initial_rules ruls) ^ ".\n")
     val system as {saturated = b, ...} = mk_system parm units slice ruls
     fun d n = if !b then S.NIL else (b := true; S.CONS (n, fn () => d (n + 1)))
     fun f q d = (chat 1 ("-" ^ int_to_string d); raw_meson system q d)
     fun unit_check goals NONE = U.prove (!units) goals | unit_check _ s = s
   in
     fn goals =>
     filter_meter slice
     (S.map (unit_check goals) (S.flatten (S.map (f goals) (d 0))))
   end};

val meson = meson' defaults;

fun delta' parm =
  mk_solver_node
  {name = "delta",
   solver_con =
   fn {slice, units, thms, hyps} =>
   let
     val ruls = meson_rules thms hyps
     val dgoals = thms_to_delta_goals hyps
     val () = chat 2
       ("delta--initializing--#thms=" ^ int_to_string (length thms) ^
        "--#hyps=" ^ int_to_string (length hyps) ^
        "--#rules=" ^ int_to_string (num_rules ruls) ^
        "--#delta_goals=" ^ int_to_string (length dgoals) ^ ".\n")
     val system as {saturated = b, ...} = mk_system parm units slice ruls
     val delta_goals = S.from_list dgoals
     fun d n = if !b then S.NIL else (b := true; S.CONS (n, fn () => d (n + 1)))
     fun f d g = (chat 1 "+"; S.map (K NONE) (raw_meson system [g] d))
     fun g d = (chat 1 (int_to_string d); S.flatten (S.map (f d) delta_goals))
     fun h () = S.flatten (S.map g (d 0))
     fun unit_check goals NONE = U.prove (!units) goals | unit_check _ s = s
   in
     case delta_goals of S.NIL => K S.NIL
     | _ => fn goals => filter_meter slice (S.map (unit_check goals) (h ()))
   end};

val delta = delta' defaults;

val prolog_depth = case Int.maxInt of NONE => 1000000 | SOME i => i;

fun prolog' parm =
  mk_solver_node
  {name = "prolog",
   solver_con =
   fn {slice, units, thms, hyps} =>
   let
     val system = mk_system parm units slice (prolog_rules thms hyps)
     fun comment S.NIL = "!\n"
       | comment (S.CONS (NONE, _)) = "-"
       | comment (S.CONS (SOME _, _)) = "$\n"
     fun f t () = let val x = t () in chat 1 (comment x); x end
   in
     fn goals => S.map_thk f (fn () => raw_meson system goals prolog_depth) ()
   end};

val prolog = prolog' defaults;

(* quick testing
load "Problem1";
open Problem1;
val time = Mosml.time;
quotation := true;
installPP pp_term;
installPP pp_formula;
installPP Subst1.pp_subst;
installPP pp_rules;
installPP pp_thm;

val limit : limit ref = ref {infs = NONE, time = SOME 30.0};
fun prolog_solve d q =
  try
  (solve
   (initialize prolog {limit = !limit, thms = d, hyps = []})) q;
fun meson_prove g =
  try (time refute)
  (initialize (set_of_support all_negative meson)
   {limit = !limit, thms = [], hyps = axiomatize (Not (generalize g))});
fun delta_prove g =
  try (time refute)
  (initialize  (set_of_support all_negative delta)
   {limit = !limit, thms = [], hyps = eq_axiomatize (Not (generalize g))});

(* Testing the delta prover *)

val p48 = parse_formula (get equality "P48");
delta_prove p48;

(* Testing the prolog solver *)

val database = (axiomatize o parse_formula)
  [

QUOTE "subset nil nil /\\\n   (!v x y. subset x y ==> subset (v :: x) (v :: y)) /\\\n   (!v x y. subset x y ==> subset x        (v :: y))"];

try (prolog_solve database) [parse_formula [QUOTE "subset x (0 :: 1 :: 2 :: nil)"]];
(* takes ages
try (prolog_solve database) [parse_formula `subset (0 :: 1 :: 2 :: nil) x`];
*)

val database = (axiomatize o parse_formula)
  [


QUOTE "p 0 3 /\\\n   (!x. p x 4) /\\\n   (!x. p x 3 ==> p (s (s (s x))) 3) /\\\n   (!x. p (s x) 3 ==> p x 3)"];

try (prolog_solve database) [parse_formula [QUOTE "p (s 0) 3"]];

(* Testing the meson prover *)

meson_prove True;

val p59 = parse_formula (get nonequality "P59");
val ths = axiomatize (Not (generalize p59));
val rules = meson_rules [] ths;
rules_unify rules (parse_formula [QUOTE "~P 0"]);
meson_prove p59;

val p39 = parse_formula (get nonequality "P39");
clausal (Not (generalize p39));
axiomatize (Not (generalize p39));
meson_prove p39;

val num14 = parse_formula (get tptp "NUM014-1");
meson_prove num14;

val p55 = parse_formula (get nonequality "P55");
meson_prove p55;

val p26 = parse_formula (get nonequality "P26");
clausal (Not (generalize p26));
meson_prove p26;

val los = parse_formula (get nonequality "LOS");
meson_prove los;

val reduced_num284 = parse_formula
  [





QUOTE "fibonacci 0 (s 0) /\\ fibonacci (s 0) (s 0) /\\\n   (!x y z x' y' z'.\n      ~sum x (s (s 0)) z \\/ ~sum y (s 0) z \\/\n      ~fibonacci x x' \\/ ~fibonacci y y' \\/ ~sum x' y' z' \\/\n      fibonacci z z') /\\ (!x. sum x 0 x) /\\\n   (!x y z. ~sum x y z \\/ sum x (s y) (s z)) /\\\n   (!x. ~fibonacci (s (s (s (s (s (s (s (s 0)))))))) x) ==> F"];
meson_prove reduced_num284;

val p29 = parse_formula (get nonequality "P29");
clausal (Not (generalize p29));
meson_prove p29;

val num1 = parse_formula (get tptp "NUM001-1");
meson_prove num1;

val model_completeness = parse_formula (get nonequality "MODEL_COMPLETENESS");
meson_prove model_completeness;
*)

end
(*#line 0.0 "src/Resolvers1.sig"*)
(* ========================================================================= *)
(* A TYPE TO FIND RESOLVANT CLAUSES                                          *)
(* Created by Joe Hurd, April 2002                                           *)
(* ========================================================================= *)

signature Resolvers1 =
sig

type 'a pp   = 'a Useful.pp
type formula = Term1.formula
type subst   = Subst1.subst
type thm     = Thm1.thm

type resolvers
type resolvant = {mate : thm, sub : subst, res : thm}

val empty_resolvers : resolvers
val add_resolver    : thm -> resolvers -> resolvers
val find_resolvants : resolvers -> thm -> resolvant list
val resolvers_info  : resolvers -> string
val pp_resolvers    : resolvers pp

end
(*#line 0.0 "src/Resolvers1.sml"*)
(* ========================================================================= *)
(* A TYPE TO FIND RESOLVANT CLAUSES                                          *)
(* Created by Joe Hurd, April 2002                                           *)
(* ========================================================================= *)

(*
app load ["Thm1", "Match1"];
*)

(*
*)
structure Resolvers1 :> Resolvers1 =
struct

infix |-> ::>;

open Useful Term1 Match1 Thm1 Canon1;

structure N = LiteralNet1;

val |<>|          = Subst1.|<>|;
val op ::>        = Subst1.::>;
val formula_subst = Subst1.formula_subst;

(* ------------------------------------------------------------------------- *)
(* Chatting.                                                                 *)
(* ------------------------------------------------------------------------- *)

val () = traces := {module = "Resolvers1", alignment = K 1} :: !traces;

fun chat l m = trace {module = "Resolvers1", message = m, level = l};

(* ------------------------------------------------------------------------- *)
(* Helper functions.                                                         *)
(* ------------------------------------------------------------------------- *)

fun trich l n =
  case split l n of (_, []) => raise ERR "trich" "no exact"
  | (l, h :: t) => (l, h, t);

(* ------------------------------------------------------------------------- *)
(* The type definition with some simple operations.                          *)
(* ------------------------------------------------------------------------- *)

type resolvers = (int * thm) N.literal_map;

type resolvant = {mate : thm, sub : subst, res : thm};

val empty_resolvers : resolvers = N.empty;

fun add_resolver th =
  let fun add_lit ((n, lit), net) = N.insert (lit |-> (n, th)) net
  in fn net => foldl add_lit net (enumerate 0 (clause th))
  end;

fun resolvers_info (net : resolvers) = int_to_string (N.size net);

val pp_resolvers = pp_map resolvers_info pp_string;

val dest_resolvers : resolvers -> thm list = 
  map snd o List.filter (equal 0 o fst) o N.to_list;

(* ------------------------------------------------------------------------- *)
(* A reference implementation for debugging.                                 *)
(* ------------------------------------------------------------------------- *)

fun canonize lits =
  let
    val nvars = enumerate 0 (FV (list_mk_conj lits))
    val ms = map (fn (n, v) => v |-> Var ("__" ^ (int_to_string n))) nvars
  in
    map (formula_subst (Subst1.from_maplets ms)) lits
  end;

local
  fun subs acc [] = acc
    | subs acc ((prev, []) :: others) = subs (prev :: acc) others
    | subs acc ((prev, h :: t) :: others) =
    subs acc ((h :: prev, t) :: (prev, t) :: others);
in
  fun all_nonempty_subsets l = tl (subs [] [([], l)]);
end;

fun pairs [] = raise ERR "pairs" "empty"
  | pairs [h] = []
  | pairs (h :: (t as h' :: _)) = (h, h') :: pairs t;

fun sanity_resolve_on th th' s s' =
  let
    val sub = unifyl_literals |<>| (pairs (s @ s'))
    val lit = formula_subst sub (hd s)
    val res = FACTOR (RESOLVE lit (INST sub th) (INST sub th'))
  in
    {mate = th', sub = sub, res = res}
  end;

fun sanity_resolve th th' =
  List.mapPartial I
  (cartwith (total o sanity_resolve_on th th')
   (all_nonempty_subsets (clause th))
   (all_nonempty_subsets (map negate (clause th'))));

fun sanity_resolvants net th =
  List.concat (map (sanity_resolve th) (dest_resolvers net));

fun sanity_check net th (res : resolvant list) =
  let
    val () = chat 1 "X"
    val f = PP.pp_to_string (!LINE_LENGTH) (pp_list (pp_map AXIOM pp_thm))
    val fast = map (canonize o clause o #res) res
    val slow = map (canonize o clause o #res) (sanity_resolvants net th)
    val () =
      if subset fast slow then ()
      else
        (print ("\nsanity_check: extra clauses:\nnet = " ^
                f (map clause (dest_resolvers net)) ^ "\nth = " ^
                thm_to_string th ^ "\nfast = " ^ f fast ^ "\nslow = " ^ f slow ^
                "\nextra = " ^ f (subtract fast slow) ^
                "\nmissing = " ^ f (subtract slow fast) ^ "\n");
         raise BUG "find_resolvants" "extra clauses!")
    val () =
      if subset slow fast then ()
      else
        (print ("\nsanity_check: missing clauses:\nnet = " ^
                f (map clause (dest_resolvers net)) ^ "\nth = " ^
                thm_to_string th ^ "\nfast = " ^ f fast ^ "\nslow = " ^ f slow ^
                "\nmissing = " ^ f (subtract slow fast) ^
                "\nextra = " ^ f (subtract fast slow) ^ "\n");
         raise BUG "find_resolvants" "missing clauses")
(*
    val () =
      (print ("\nsanity_check: ok:\nnet = " ^
              f (map clause (dest_resolvers net)) ^ "\nth = " ^
              thm_to_string th ^ "\nres = " ^ f fast ^ "\n"))
*)
  in
    ()
  end;

(* ------------------------------------------------------------------------- *)
(* The core engine for combined factor/resolution steps.                     *)
(* ------------------------------------------------------------------------- *)

fun resolve_on s r th th' =
  SOME (FACTOR (RESOLVE r (INST s th) (INST s th')));

fun resolve acc [] = acc
  | resolve acc ((avoid, sub, res, []) :: others) =
  resolve
  (if mem res (map (formula_subst sub) avoid) then acc
   else (res, sub) :: acc) others
  | resolve acc ((avoid, sub, res, x :: xs) :: others) =
  let
    fun f c = resolve acc (c ((x :: avoid, sub, res, xs) :: others))
  in
    case total (unify_literals sub res) x of NONE => f I
    | SOME sub'
      => f (cons (avoid, Subst1.refine sub sub', formula_subst sub' res, xs))
  end;

fun resolve_from (n, th) (n', th') =
  let
    val (prev, lit, succ) = trich (clause th) n
    val (prev', lit', succ') = trich (map negate (clause th')) n'
    val sub = unify_literals |<>| lit lit'
    val res = formula_subst sub lit
    fun f (r, s) = Option.map (pair s) (resolve_on s r th th')
  in
    List.mapPartial f (resolve [] [(prev @ prev', sub, res, succ @ succ')])
  end;

fun resolvants net th =
  let
    fun g (_, mate) ((sub, res), l) = {mate = mate, sub = sub, res = res} :: l
    fun r m (u, acc) =
      case total (resolve_from (m, th)) u of NONE => acc
      | SOME l => foldl (g u) acc l
    fun f ((m, lit), acc) = foldl (r m) acc (N.unify net (negate lit))
    val res = foldl f [] (enumerate 0 (clause th))
  (*val () = sanity_check net th res*)
  in
    res
  end

fun find_resolvants net th =
  List.filter (non tautologous o clause o #res) (resolvants net th)
  handle ERR_EXN _ => raise BUG "find_resolvants" "should never fail";

(* Quick testing
quotation := true;
installPP pp_formula;
installPP pp_term;
installPP pp_subst;
installPP pp_thm;
val th = AXIOM (map parse [`p(3, x, v)`, `q(x)`, `p(3, x, z)`]);
val th' = AXIOM (map parse [`~p(3, f(y), w)`, `~q(y)`, `~p(3, f(y), 4)`]);
try (resolve_from (0, th)) (0, th');
try (resolve_from (2, th)) (0, th');
try (resolve_from (0, th)) (2, th');
try (resolve_from (2, th)) (2, th');
val r = add_resolver th' empty_resolvers;
map #res (find_resolvants r th);
*)

end
(*#line 0.0 "src/Theap1.sig"*)
(* ========================================================================= *)
(* A TYPE TO STORE CLAUSES WAITING TO BE USED (THEAP = THEOREM HEAP)         *)
(* Created by Joe Hurd, April 2002                                           *)
(* ========================================================================= *)

signature Theap1 =
sig

type 'a subsume = 'a Subsume1.subsume
type thm        = Thm1.thm

(* Tuning parameters *)
type parameters = {fifo_skew : int, cleaning_freq : int}
val defaults : parameters

(* Theorem HEAPs *)
type theap
val new_theap       : parameters -> theap
val empty_theap     : theap                       (* Uses defaults *)
val theap_size      : theap -> int
val theap_add       : thm -> theap -> theap
val theap_addl      : thm list -> theap -> theap
val theap_remove    : theap -> (thm * theap) option
val theap_subsumers : theap -> thm subsume
val theap_info      : theap -> string   (* Outputs "(size,weight)" *)

end
(*#line 0.0 "src/Theap1.sml"*)
(* ========================================================================= *)
(* A TYPE TO STORE CLAUSES WAITING TO BE USED (THEAP = THEOREM HEAP)         *)
(* Created by Joe Hurd, April 2002                                           *)
(* ========================================================================= *)

(*
app load ["Heap", "Queue", "Thm1", "Subsumers1"];
*)

(*
*)
structure Theap1 :> Theap1 =
struct

infix |->;

open Useful Term1 Thm1;

structure Q = Queue;
structure H = Heap;
structure S = Subsume1;

type 'a queue   = 'a Q.queue;
type 'a heap    = 'a H.heap;
type 'a subsume = 'a S.subsume;

(* ------------------------------------------------------------------------- *)
(* Tuning parameters.                                                        *)
(* ------------------------------------------------------------------------- *)

type parameters = {fifo_skew : int, cleaning_freq : int}

val defaults = {fifo_skew = 3, cleaning_freq = 1000};

(* ------------------------------------------------------------------------- *)
(* Theorem HEAPs.                                                            *)
(* ------------------------------------------------------------------------- *)

type theap =
  ((int * int) * (int * int)) * thm queue * (int * (int * thm) heap) *
  thm subsume;

local fun order ((m, _ : thm), (n, _ : thm)) = Int.compare (m, n);
in val empty_theap_heap = H.empty order;
end;

fun new_theap {fifo_skew, cleaning_freq} =
  ((D cleaning_freq, D fifo_skew), Q.empty, (0, empty_theap_heap), S.empty);

val empty_theap: theap = new_theap defaults;

fun theap_size (_, _, (_, h), _) = H.size h;
fun theap_weight (_, _, (w, _), _) = w;

(*
fun clean_theap (((_, C), F), Q, (_, H), _) =
  let
    val hash = Polyhash.mkPolyTable (10000, ERR "cleap_theap" "not found")
    fun mark (v, th) = Polyhash.insert hash (clause th, v)
    val () = H.app mark H
    fun add (v, th) (q, w, h, l) =
      (Q.add th q, w + v, H.add (v, th) h, S.add (clause th |-> th) l)
    fun check q n =
      if Q.is_empty q then n
      else
        let
          val th = Q.hd q
        in
          check (Q.tl q)
          (case total (Polyhash.remove hash) (clause th) of NONE => n
           | SOME v => add (v, th) n)
        end
  in
    (fn (q, w, h, l) => (((C, C), F), q, (w, h), l))
    (check Q (Q.empty, 0, empty_theap_heap, S.empty))
  end;
*)

(*fun theap_add th (h as (((0,_), _), _, _, _)) = theap_add th (clean_theap h)*)
fun theap_add th (((c, cm), f), q, (w, h), l) =
  let
    val cl = clause th
    val v = formula_size (list_mk_disj cl)
    val h' = H.add (v, th) h
  in
    (((c - 1, cm), f), Q.add th q, (w + v, h'), S.add (clause th |-> th) l)
  end;

fun theap_addl ths h = foldl (uncurry theap_add) h ths;

fun theap_remove ((c, (0, f)), q, h, l) =
  if Q.is_empty q then NONE
  else SOME (Q.hd q, ((c, (f, f)), Q.tl q, h, l))
  | theap_remove ((c, (n, f)), q, (w, h), l) =
  if H.is_empty h then NONE
  else
    let val ((v, x), h) = H.remove h
    in SOME (x, ((c, (n - 1, f)), q, (w - v, h), l))
    end;

fun theap_subsumers (_, _, _, l) = l;

fun theap_info thp =
  "(" ^ int_to_string (theap_size thp) ^ "," ^
  int_to_string (theap_weight thp) ^ ")";

end
(*#line 0.0 "src/Resolution1.sig"*)
(* ========================================================================= *)
(* THE RESOLUTION PROOF PROCEDURE                                            *)
(* Created by Joe Hurd, November 2001                                        *)
(* ========================================================================= *)

signature Resolution1 =
sig

type solver_node = Solver1.solver_node

(* Tuning parameters *)
type parameters =
  {subsumption_checking : int,                    (* in the range 0..3 *)
   positive_refinement  : bool,
   theap_parm           : Theap1.parameters}

val defaults : parameters

(* Resolution *)
val resolution' : parameters -> solver_node
val resolution  : solver_node                     (* Uses defaults *)

end
(*#line 0.0 "src/Resolution1.sml"*)
(* ========================================================================= *)
(* THE RESOLUTION PROOF PROCEDURE                                            *)
(* Created by Joe Hurd, November 2001                                        *)
(* ========================================================================= *)

(*
app load
 ["Useful", "Mosml", "Term1", "Thm1", "Canon1", "Theap1",
  "Stream", "Solver1", "Meter1", "Units1", "Resolvers1"];
*)

(*
*)
structure Resolution1 :> Resolution1 =
struct

open Useful Term1 Thm1 Canon1 Meter1 Solver1 Resolvers1 Theap1;

infix |-> ::> @> oo ## ::* ::@;

structure S = Stream;
structure U = Units1;

type 'a subsume = 'a Subsume1.subsume;

(* ------------------------------------------------------------------------- *)
(* Chatting.                                                                 *)
(* ------------------------------------------------------------------------- *)

val () = traces := {module = "Resolution1", alignment = K 1} :: !traces;

fun chat l m = trace {module = "Resolution1", message = m, level = l};

(* ------------------------------------------------------------------------- *)
(* Tuning parameters.                                                        *)
(* ------------------------------------------------------------------------- *)

type parameters =
  {subsumption_checking : int,          (* in the range 0..3 *)
   positive_refinement  : bool,
   theap_parm           : Theap1.parameters}

val defaults =
  {subsumption_checking = 1,
   positive_refinement  = true,
   theap_parm           = Theap1.defaults};

(* ------------------------------------------------------------------------- *)
(* Clause stores.                                                            *)
(* ------------------------------------------------------------------------- *)

type store = thm subsume * resolvers;

val empty_store : store = (Subsume1.empty, empty_resolvers);

fun store_add th (s, r) =
  (Subsume1.add (clause th |-> th) s, add_resolver th r);

fun store_resolvants ((_, r) : store) = find_resolvants r;

fun store_subsumed ((s, _) : store) = Subsume1.subsumed s o clause;

fun store_info (s, r) = "(" ^ Subsume1.info s ^ "," ^ resolvers_info r ^ ")";

(* ------------------------------------------------------------------------- *)
(* Positive refinement.                                                      *)
(* ------------------------------------------------------------------------- *)

local
  val pos_th = List.all positive o clause;

  fun check true = K true
    | check false = fn ({mate, ...} : resolvant) => pos_th mate;
in
  fun positive_check false = K (K true)
    | positive_check true = check o pos_th;
end;

(* ------------------------------------------------------------------------- *)
(* Full resolution procedure.                                                *)
(* ------------------------------------------------------------------------- *)

exception Contradiction of thm;

fun unit_strengthen units th =
  (case first (U.subsumes units) (clause th) of SOME th => th
   | NONE => U.demod units th);

fun subsumption_check store th =
  case store_subsumed store th of [] => SOME th | _ :: _ => NONE;

fun theap_strengthen theap th =
  (case Subsume1.strictly_subsumed (theap_subsumers theap) (clause th) of []
     => th
   | (_, th) :: _ => th);

fun resolve (parm : parameters) =
  let
    fun feedback k r =
      int_to_string k ^ (if k = r then "" else "/" ^ int_to_string r)

    fun L n = n <= #subsumption_checking parm
    val pos_filt = Option.filter o positive_check (#positive_refinement parm)

    fun ftest b f = if b then Option.mapPartial (subsumption_check f) else I
    fun stest b s = if b then subsumption_check s else SOME
    fun wpass b w = if b then theap_strengthen w else I
    fun upass u = unit_strengthen u

    fun next_candidate u f s w =
      case theap_remove w of NONE => NONE
      | SOME (th, w) =>
        (case (ftest (L 1) f o stest (L 1) s o wpass (L 2) w o upass u) th of
           NONE => next_candidate u f s w
         | SOME th => SOME (th, w))

    fun retention_test u f s th =
      List.mapPartial
      (Option.mapPartial (ftest (L 3) f o stest (L 3) s o upass u o #res) o
       pos_filt th)

    fun check_add th =
      if is_contradiction th then raise Contradiction th else U.add th
  in
    fn record => fn (facts, used, unused) => fn units =>
    (case next_candidate units facts used unused of NONE => NONE
     | SOME (th, unused) =>
       let
         val units = check_add th units
         val used = store_add th used
         val th = FRESH_VARS th
         val resolvants =
           store_resolvants facts th @ store_resolvants used th
         val () = record (length resolvants)
         val units = foldl (uncurry check_add) units (map #res resolvants)
         val keep = retention_test units facts used th resolvants
         val () = chat 2 (feedback (length keep) (length resolvants))
         val unused = theap_addl keep unused
       in
         SOME ((facts, used, unused), units)
       end)
       handle ERR_EXN _ => raise BUG "resolve" "shouldn't fail"
  end;

fun raw_resolution parm =
  mk_solver_node
  {name = "resolution",
   solver_con =
   fn {slice, units, thms, hyps} =>
   let
     val resolve' = resolve parm
     fun run wrap state =
       if not (check_meter (!slice)) then S.CONS (NONE, wrap state)
       else
         (chat 1 "+";
          case resolve' (record_infs (!slice)) state (!units) of NONE => S.NIL
          | SOME (state, units') => (units := units'; run wrap state))
     fun wrapper g (a as (_, _, w)) () =
       (chat 2 (theap_info w); run (wrapper g) a)
       handle Contradiction th => contradiction_solver th g
     val facts = foldl (fn (t, l) => store_add t l) empty_store thms
     val used = empty_store
     val unused = theap_addl hyps (new_theap (#theap_parm parm))
     val () = chat 2
       ("resolution--initializing--#thms=" ^ int_to_string (length thms) ^
        "--#hyps=" ^ int_to_string (length hyps) ^
        "--facts=" ^ store_info facts ^
        "--unused=" ^ theap_info unused ^ ".\n")
   in
     fn goals => wrapper goals (facts, used, unused) ()
   end};

fun resolution' parm =
  (if #positive_refinement parm then set_of_support everything else I)
  (raw_resolution parm);

val resolution = resolution' defaults;

(* quick testing
load "Problem1";
open Problem1;
val time = Mosml.time;
quotation := true;
installPP pp_term;
installPP pp_formula;
installPP Subst1.pp_subst;
installPP pp_thm;

(* Testing the resolution prover *)

val limit : limit ref = ref {infs = NONE, time = SOME 30.0};
fun resolution_prove g =
  try (time refute)
  (initialize (set_of_support all_negative resolution)
   {limit = !limit, thms = [], hyps = axiomatize (Not (generalize g))});

axiomatize (Not (generalize True));
resolution_prove True;

val prop13 = parse_formula (get nonequality "PROP_13");
axiomatize (Not (generalize prop13));
resolution_prove prop13;

val p33 = parse_formula (get nonequality "P33");
axiomatize (Not (generalize p33));
resolution_prove p33;

val p59 = parse_formula (get nonequality "P59");
val ths = axiomatize (Not (generalize p59));
resolution_prove p59;

val p39 = parse_formula (get nonequality "P39");
clausal (Not (generalize p39));
axiomatize (Not (generalize p39));
resolution_prove p39;

val num14 = parse_formula (get tptp "NUM014-1");
resolution_prove num14;

val p55 = parse_formula (get nonequality "P55");
resolution_prove p55;

val p26 = parse_formula (get nonequality "P26");
clausal (Not (generalize p26));
resolution_prove p26;

val los = parse_formula (get nonequality "LOS");
resolution_prove los;

val reduced_num284 = parse_formula
  [





QUOTE "fibonacci 0 (s 0) /\\ fibonacci (s 0) (s 0) /\\\n   (!x y z x' y' z'.\n      ~sum x (s (s 0)) z \\/ ~sum y (s 0) z \\/\n      ~fibonacci x x' \\/ ~fibonacci y y' \\/ ~sum x' y' z' \\/\n      fibonacci z z') /\\ (!x. sum x 0 x) /\\\n   (!x y z. ~sum x y z \\/ sum x (s y) (s z)) /\\\n   (!x. ~fibonacci (s (s (s (s (s (s (s (s 0)))))))) x) ==> F"];
resolution_prove reduced_num284;

val p29 = parse_formula (get nonequality "P29");
clausal (Not (generalize p29));
resolution_prove p29;

val num1 = parse_formula (get tptp "NUM001-1");
resolution_prove num1;

val gilmore9 = parse_formula (get nonequality "GILMORE_9");
axiomatize (Not (generalize gilmore9));
resolution_prove gilmore9;

val model_completeness = parse_formula (get nonequality "MODEL_COMPLETENESS");
resolution_prove model_completeness;
*)

end
(*#line 0.0 "src/Metis1.sig"*)
(* ========================================================================= *)
(* THE METIS COMBINATION OF PROOF PROCEDURES FOR FIRST-ORDER LOGIC           *)
(* Created by Joe Hurd, September 2001                                       *)
(* ========================================================================= *)

signature Metis1 =
sig

type formula     = Term1.formula
type thm         = Thm1.thm
type limit       = Meter1.limit
type solver      = Solver1.solver
type solver_node = Solver1.solver_node

(* Tuning parameters *)
type Mparm = Meson1.parameters
type Rparm = Resolution1.parameters
type parameters =
  {meson           : bool,
   delta           : bool,
   resolution      : bool,
   meson_parm      : Mparm,
   resolution_parm : Rparm}

val defaults                    : parameters
val update_parm_meson           : (bool -> bool) -> parameters -> parameters
val update_parm_delta           : (bool -> bool) -> parameters -> parameters
val update_parm_resolution      : (bool -> bool) -> parameters -> parameters
val update_parm_meson_parm      : (Mparm -> Mparm) -> parameters -> parameters
val update_parm_resolution_parm : (Rparm -> Rparm) -> parameters -> parameters

(* The metis combination of solvers *)
val metis' : parameters -> solver_node
val metis  : solver_node                (* Uses defaults *)

(* A user-friendly interface *)
val settings  : parameters ref          (* Starts off as defaults *)
val limit     : limit ref               (* Starts off as 10 seconds *)
val raw_prove : formula -> thm option   (* Expects a ==> b ==> F *)
val prove     : formula -> thm option   (* Adds eq axioms, converts to CNF *)
val query     : formula -> solver       (* Prolog query engine *)

end
(*#line 0.0 "src/Metis1.sml"*)
(* ========================================================================= *)
(* THE METIS COMBINATION OF PROOF PROCEDURES FOR FIRST-ORDER LOGIC           *)
(* Created by Joe Hurd, September 2001                                       *)
(* ========================================================================= *)

(*
app load
 ["Useful", "Mosml", "Term1", "Thm1", "Canon1",
  "Solver1", "Meson1", "Resolution1"];
*)

(*
*)
structure Metis1 :> Metis1 =
struct

open Useful Term1 Thm1 Meter1 Canon1 Solver1 Meson1 Resolution1;

infix |-> ::> @> oo ## ::* ::@;

(* ------------------------------------------------------------------------- *)
(* Tuning parameters.                                                        *)
(* ------------------------------------------------------------------------- *)

type Mparm = Meson1.parameters;
type Rparm = Resolution1.parameters;

type parameters =
  {meson           : bool,
   delta           : bool,
   resolution      : bool,
   meson_parm      : Mparm,
   resolution_parm : Rparm};

val defaults =
  {meson           = true,
   delta           = true,
   resolution      = true,
   meson_parm      = Meson1.defaults,
   resolution_parm = Resolution1.defaults};

fun update_parm_meson f parm =
  let
    val {meson, delta, resolution, meson_parm, resolution_parm} = parm
  in
    {meson = f meson, delta = delta, resolution = resolution,
     meson_parm = meson_parm, resolution_parm = resolution_parm}
  end;

fun update_parm_delta f parm =
  let
    val {meson, delta, resolution, meson_parm, resolution_parm} = parm
  in
    {meson = meson, delta = f delta, resolution = resolution,
     meson_parm = meson_parm, resolution_parm = resolution_parm}
  end;

fun update_parm_resolution f parm =
  let
    val {meson, delta, resolution, meson_parm, resolution_parm} = parm
  in
    {meson = meson, delta = delta, resolution = f resolution,
     meson_parm = meson_parm, resolution_parm = resolution_parm}
  end;

fun update_parm_meson_parm f parm =
  let
    val {meson, delta, resolution, meson_parm, resolution_parm} = parm
  in
    {meson = meson, delta = delta, resolution = resolution,
     meson_parm = f meson_parm, resolution_parm = resolution_parm}
  end;

fun update_parm_resolution_parm f parm =
  let
    val {meson, delta, resolution, meson_parm, resolution_parm} = parm
  in
    {meson = meson, delta = delta, resolution = resolution,
     meson_parm = meson_parm, resolution_parm = f resolution_parm}
  end;

(* ------------------------------------------------------------------------- *)
(* The metis combination of solvers.                                         *)
(* ------------------------------------------------------------------------- *)

fun metis' {meson = m, delta = d, resolution = r, meson_parm, resolution_parm} =
  combine
  ((if m then cons (time1, meson' meson_parm) else I)
   ((if r then cons (time1, resolution' resolution_parm) else I)
    ((if d then cons (time2, delta' meson_parm) else I)
     [])));

val metis = metis' defaults;

(* ------------------------------------------------------------------------- *)
(* A user-friendly interface.                                                *)
(* ------------------------------------------------------------------------- *)

val settings = ref defaults;

val limit : limit ref = ref {time = NONE, infs = NONE};

fun raw_prove (Imp (a, Imp (b, False))) =
  let
    val (thms, hyps) = (axiomatize a, axiomatize b)
    val solv = metis' (!settings)
  in
    refute (initialize solv {limit = !limit, thms = thms, hyps = hyps})
  end
  | raw_prove _ = raise ERR "raw_prove" "formula not of type a ==> b ==> F";

fun prove g =
  let
    val hyps = eq_axiomatize' (Not (generalize g))
    val solv = set_of_support all_negative (metis' (!settings))
  in
    refute (initialize solv {limit = !limit, thms = [], hyps = hyps})
  end;

fun query database =
  initialize prolog {thms = axiomatize database, hyps = [], limit = unlimited};

(* quick testing
val time = Mosml.time;
quotation := true;
installPP pp_term;
installPP pp_formula;
installPP Subst1.pp_subst;
installPP pp_thm;

(* Testing the metis prover *)

prove True;

val p59 = parse_formula [QUOTE "(!x. P(x) <=> ~P(f(x))) ==> (?x. P(x) /\\ ~P(f(x)))"];
val ths = axiomatize (Not (generalize p59));
prove p59;

val p39 = parse_formula [QUOTE "~(?x. !y. P(y,x) <=> ~P(y,y))"];
clausal (Not (generalize p39));
axiomatize (Not (generalize p39));
prove p39;

val num14 = parse_formula
  [





QUOTE "(!X. product(X, X, square(X))) /\\\n   (!Z X Y. ~product(X, Y, Z) \\/ product(Y, X, Z)) /\\\n   (!Z X Y. ~product(X, Y, Z) \\/ divides(X, Z)) /\\\n   (!Y X V Z.\n      ~prime(X) \\/ ~product(Y, Z, V) \\/ ~divides(X, V) \\/ divides(X, Y) \\/\n      divides(X, Z)) /\\ prime(a) /\\\n   product(a, square(c), square(b)) /\\ ~divides(a, b) ==> F"];
prove num14;

val p26 = parse_formula
 [

QUOTE "((?x. P(x)) <=> (?x. Q(x))) /\\\n   (!x y. P(x) /\\ Q(y) ==> (R(x) <=> U(y))) ==>\n   ((!x. P(x) ==> R(x)) <=> (!x. Q(x) ==> U(x)))"];
clausal (Not (generalize p26));
prove p26;

val los = parse_formula
 [

QUOTE "(!x y z. P x y ==> P y z ==> P x z) /\\\n  (!x y z. Q x y ==> Q y z ==> Q x z) /\\ (!x y. Q x y ==> Q y x) /\\\n  (!x y. P x y \\/ Q x y) ==> (!x y. P x y) \\/ !x y. Q x y"];
try prove los;

val puz2 = parse_formula
  [


























QUOTE "(!X. equal(X, X)) /\\ (!Y X. ~equal(X, Y) \\/ equal(Y, X)) /\\\n   (!Z X Y. ~equal(X, Y) \\/ ~equal(Y, Z) \\/ equal(X, Z)) /\\\n   (!B A. ~equal(A, B) \\/ equal(every_one_but(A), every_one_but(B))) /\\\n   (!E C D. ~equal(C, D) \\/ ~hates(C, E) \\/ hates(D, E)) /\\\n   (!H F_avoid G.\n      ~equal(F_avoid, G) \\/ ~hates(H, F_avoid) \\/ hates(H, G)) /\\\n   (!K I J. ~equal(I, J) \\/ ~killed(I, K) \\/ killed(J, K)) /\\\n   (!N L M. ~equal(L, M) \\/ ~killed(N, L) \\/ killed(N, M)) /\\\n   (!P O.\n      ~equal(O, P) \\/ ~lives_at_dreadsbury(O) \\/ lives_at_dreadsbury(P)) /\\\n   (!S Q R. ~equal(Q, R) \\/ ~richer(Q, S) \\/ richer(R, S)) /\\\n   (!V T_avoid U.\n      ~equal(T_avoid, U) \\/ ~richer(V, T_avoid) \\/ richer(V, U)) /\\\n   lives_at_dreadsbury(someone()) /\\ killed(someone(), aunt_agatha()) /\\\n   lives_at_dreadsbury(aunt_agatha()) /\\ lives_at_dreadsbury(butler()) /\\\n   lives_at_dreadsbury(charles()) /\\\n   (!Person.\n      ~lives_at_dreadsbury(Person) \\/ equal(Person, aunt_agatha()) \\/\n      equal(Person, butler()) \\/ equal(Person, charles())) /\\\n   (!Victim Killer. ~killed(Killer, Victim) \\/ hates(Killer, Victim)) /\\\n   (!Victim Killer. ~killed(Killer, Victim) \\/ ~richer(Killer, Victim)) /\\\n   (!Person. ~hates(aunt_agatha(), Person) \\/ ~hates(charles(), Person)) /\\\n   (!Person. equal(Person, butler()) \\/ hates(aunt_agatha(), Person)) /\\\n   (!Person. richer(Person, aunt_agatha()) \\/ hates(butler(), Person)) /\\\n   (!Person. ~hates(aunt_agatha(), Person) \\/ hates(butler(), Person)) /\\\n   (!Person. ~hates(Person, every_one_but(Person))) /\\\n   ~equal(aunt_agatha(), butler()) /\\\n   ~killed(aunt_agatha(), aunt_agatha()) ==> F"];
prove puz2;

val num284 = parse_formula
  [







QUOTE "fibonacci(0, successor(0)) /\\ fibonacci(successor(0), successor(0)) /\\\n   (!N2 N1 N F1 FN F2.\n      ~sum(N1, successor(0), N) \\/ ~sum(N2, successor(successor(0)), N) \\/\n      ~fibonacci(N1, F1) \\/ ~fibonacci(N2, F2) \\/ ~sum(F1, F2, FN) \\/\n      fibonacci(N, FN)) /\\ (!X. sum(X, 0, X)) /\\\n   (!Z X Y. ~sum(X, Y, Z) \\/ sum(X, successor(Y), successor(Z))) /\\\n   (!Result.\n      ~fibonacci(successor(successor(successor(successor(successor(successor(successor(successor(0)))))))),\n                 Result)) ==> F"];
prove num284;

val p29 = parse_formula
  [

QUOTE "(?x. P(x)) /\\ (?x. G(x)) ==>\n   ((!x. P(x) ==> H(x)) /\\ (!x. G(x) ==> J(x)) <=>\n    (!x y. P(x) /\\ G(y) ==> H(x) /\\ J(y)))"];
clausal (Not (generalize p29));
prove p29;

val num27 = parse_formula
  [





















QUOTE "(!A. equalish(add(A, 0), A)) /\\\n   (!A B. equalish(add(A, successor(B)), successor(add(A, B)))) /\\\n   (!A. equalish(multiply(A, 0), 0)) /\\\n   (!A B. equalish(multiply(A, successor(B)), add(multiply(A, B), A))) /\\\n   (!B A. ~equalish(successor(A), successor(B)) \\/ equalish(A, B)) /\\\n   (!B A. ~equalish(A, B) \\/ equalish(successor(A), successor(B))) /\\\n   (!C A B. ~less(A, B) \\/ ~less(C, A) \\/ less(C, B)) /\\\n   (!C B A. ~equalish(add(successor(A), B), C) \\/ less(B, C)) /\\\n   (!B A.\n      ~less(A, B) \\/\n      equalish(add(successor(predecessor_of_1st_minus_2nd(B, A)), A),\n        B)) /\\ (!X. equalish(X, X)) /\\\n   (!Y X. ~equalish(X, Y) \\/ equalish(Y, X)) /\\\n   (!Z X Y. ~equalish(X, Y) \\/ ~equalish(Y, Z) \\/ equalish(X, Z)) /\\\n   (!C A B. ~equalish(A, B) \\/ equalish(multiply(A, C), multiply(B, C))) /\\\n   (!B A. ~less(A, B) \\/ ~equalish(A, B)) /\\\n   (!B A. less(A, B) \\/ equalish(B, A) \\/ less(B, A)) /\\\n   (!A. ~less(A, A)) /\\ (!A. ~equalish(successor(A), 0)) /\\\n   (!C A B.\n      ~less(A, B) \\/ equalish(C, 0) \\/\n      less(multiply(A, C), multiply(B, C))) /\\ ~less(b(), a()) /\\\n   less(multiply(b(), c()), multiply(a(), c())) /\\ ~equalish(c(), 0) ==>\n   F"];
prove num27;

val model_completeness = parse_formula
  [







QUOTE "(!M p. sentence(p) ==> holds(M,p) \\/ holds(M,not(p))) /\\\n   (!M p. ~(holds(M,p) /\\ holds(M,not(p)))) ==>\n   ((!p.\n       sentence(p) ==>\n       (!M. models(M,S) ==> holds(M,p)) \\/\n       (!M. models(M,S) ==> holds(M,not(p)))) <=>\n    (!M M'.\n       models(M,S) /\\ models(M',S) ==>\n       (!p. sentence(p) ==> (holds(M,p) <=> holds(M',p)))))"];
prove model_completeness;

val agatha = parse_formula
  [










QUOTE "lives(agatha()) /\\ lives(butler()) /\\ lives(charles()) /\\\n   (killed(agatha(),agatha()) \\/ killed(butler(),agatha()) \\/\n    killed(charles(),agatha())) /\\\n   (!x y. killed(x,y) ==> hates(x,y) /\\ ~richer(x,y)) /\\\n   (!x. hates(agatha(),x) ==> ~hates(charles(),x)) /\\\n   (hates(agatha(),agatha()) /\\ hates(agatha(),charles())) /\\\n   (!x. lives(x) /\\ ~richer(x,agatha()) ==> hates(butler(),x)) /\\\n   (!x. hates(agatha(),x) ==> hates(butler(),x)) /\\\n   (!x. ~hates(x,agatha()) \\/ ~hates(x,butler()) \\/ ~hates(x,charles()))\n   ==>\n   killed(agatha(),agatha()) /\\ ~killed(butler(),agatha()) /\\\n   ~killed(charles(),agatha())"];
prove agatha;

val boo3 = parse_formula
 [

















































QUOTE "(!X. equal(X, X)) /\\ (!Y X. ~equal(X, Y) \\/ equal(Y, X)) /\\\n  (!Z X Y. ~equal(X, Y) \\/ ~equal(Y, Z) \\/ equal(X, Z)) /\\\n  (!Y X. sum(X, Y, add(X, Y))) /\\ (!Y X. product(X, Y, multiply(X, Y))) /\\\n  (!Z X Y. ~sum(X, Y, Z) \\/ sum(Y, X, Z)) /\\\n  (!Z X Y. ~product(X, Y, Z) \\/ product(Y, X, Z)) /\\\n  (!X. sum(additive_identity(), X, X)) /\\\n  (!X. sum(X, additive_identity(), X)) /\\\n  (!X. product(multiplicative_identity(), X, X)) /\\\n  (!X. product(X, multiplicative_identity(), X)) /\\\n  (!Z X Y V1 V3 V4 V2.\n     ~product(X, Y, V1) \\/ ~product(X, Z, V2) \\/ ~sum(Y, Z, V3) \\/\n     ~product(X, V3, V4) \\/ sum(V1, V2, V4)) /\\\n  (!Z X Y V1 V3 V4 V2.\n     ~product(X, Y, V1) \\/ ~product(X, Z, V2) \\/ ~sum(Y, Z, V3) \\/\n     ~sum(V1, V2, V4) \\/ product(X, V3, V4)) /\\\n  (!Z Y X V1 V3 V4 V2.\n     ~product(Y, X, V1) \\/ ~product(Z, X, V2) \\/ ~sum(Y, Z, V3) \\/\n     ~product(V3, X, V4) \\/ sum(V1, V2, V4)) /\\\n  (!Z Y X V1 V3 V4 V2.\n     ~product(Y, X, V1) \\/ ~product(Z, X, V2) \\/ ~sum(Y, Z, V3) \\/\n     ~sum(V1, V2, V4) \\/ product(V3, X, V4)) /\\\n  (!Z X Y V1 V3 V4 V2.\n     ~sum(X, Y, V1) \\/ ~sum(X, Z, V2) \\/ ~product(Y, Z, V3) \\/\n     ~sum(X, V3, V4) \\/ product(V1, V2, V4)) /\\\n  (!Z X Y V1 V3 V4 V2.\n     ~sum(X, Y, V1) \\/ ~sum(X, Z, V2) \\/ ~product(Y, Z, V3) \\/\n     ~product(V1, V2, V4) \\/ sum(X, V3, V4)) /\\\n  (!Z Y X V1 V3 V4 V2.\n     ~sum(Y, X, V1) \\/ ~sum(Z, X, V2) \\/ ~product(Y, Z, V3) \\/\n     ~sum(V3, X, V4) \\/ product(V1, V2, V4)) /\\\n  (!Z Y X V1 V3 V4 V2.\n     ~sum(Y, X, V1) \\/ ~sum(Z, X, V2) \\/ ~product(Y, Z, V3) \\/\n     ~product(V1, V2, V4) \\/ sum(V3, X, V4)) /\\\n  (!X. sum(inverse(X), X, multiplicative_identity())) /\\\n  (!X. sum(X, inverse(X), multiplicative_identity())) /\\\n  (!X. product(inverse(X), X, additive_identity())) /\\\n  (!X. product(X, inverse(X), additive_identity())) /\\\n  (!V X Y U. ~sum(X, Y, U) \\/ ~sum(X, Y, V) \\/ equal(U, V)) /\\\n  (!V X Y U. ~product(X, Y, U) \\/ ~product(X, Y, V) \\/ equal(U, V)) /\\\n  (!W X Y Z. ~equal(X, Y) \\/ ~sum(X, W, Z) \\/ sum(Y, W, Z)) /\\\n  (!W X Y Z. ~equal(X, Y) \\/ ~sum(W, X, Z) \\/ sum(W, Y, Z)) /\\\n  (!W X Y Z. ~equal(X, Y) \\/ ~sum(W, Z, X) \\/ sum(W, Z, Y)) /\\\n  (!W X Y Z. ~equal(X, Y) \\/ ~product(X, W, Z) \\/ product(Y, W, Z)) /\\\n  (!W X Y Z. ~equal(X, Y) \\/ ~product(W, X, Z) \\/ product(W, Y, Z)) /\\\n  (!W X Y Z. ~equal(X, Y) \\/ ~product(W, Z, X) \\/ product(W, Z, Y)) /\\\n  (!W X Y. ~equal(X, Y) \\/ equal(add(X, W), add(Y, W))) /\\\n  (!W X Y. ~equal(X, Y) \\/ equal(add(W, X), add(W, Y))) /\\\n  (!W X Y. ~equal(X, Y) \\/ equal(multiply(X, W), multiply(Y, W))) /\\\n  (!W X Y. ~equal(X, Y) \\/ equal(multiply(W, X), multiply(W, Y))) /\\\n  (!Y X. ~equal(X, Y) \\/ equal(inverse(X), inverse(Y))) /\\\n  ~product(x(), x(), x()) ==> F"];
prove boo3;

val fld5 = parse_formula
 [



















































QUOTE "(!Y X V W Z U.\n     sum(X, V, W) \\/ ~sum(X, Y, U) \\/ ~sum(Y, Z, V) \\/ ~sum(U, Z, W)) /\\\n  (!X U Z W V Y.\n     sum(U, Z, W) \\/ ~sum(X, Y, U) \\/ ~sum(Y, Z, V) \\/ ~sum(X, V, W)) /\\\n  (!X. sum(additive_identity(), X, X) \\/ ~defined(X)) /\\\n  (!X. sum(additive_inverse(X), X, additive_identity()) \\/ ~defined(X)) /\\\n  (!Z Y X. sum(Y, X, Z) \\/ ~sum(X, Y, Z)) /\\\n  (!Y X V W Z U.\n     product(X, V, W) \\/ ~product(X, Y, U) \\/ ~product(Y, Z, V) \\/\n     ~product(U, Z, W)) /\\\n  (!X U Z W V Y.\n     product(U, Z, W) \\/ ~product(X, Y, U) \\/ ~product(Y, Z, V) \\/\n     ~product(X, V, W)) /\\\n  (!X. product(multiplicative_identity(), X, X) \\/ ~defined(X)) /\\\n  (!X.\n     product(multiplicative_inverse(X), X, multiplicative_identity()) \\/\n     sum(additive_identity(), X, additive_identity()) \\/ ~defined(X)) /\\\n  (!Z Y X. product(Y, X, Z) \\/ ~product(X, Y, Z)) /\\\n  (!X C D B Z A Y.\n     sum(C, D, B) \\/ ~sum(X, Y, A) \\/ ~product(A, Z, B) \\/\n     ~product(X, Z, C) \\/ ~product(Y, Z, D)) /\\\n  (!X A Z B C D Y.\n     product(A, Z, B) \\/ ~sum(X, Y, A) \\/ ~product(X, Z, C) \\/\n     ~product(Y, Z, D) \\/ ~sum(C, D, B)) /\\\n  (!X Y. defined(add(X, Y)) \\/ ~defined(X) \\/ ~defined(Y)) /\\\n  defined(additive_identity()) /\\\n  (!X. defined(additive_inverse(X)) \\/ ~defined(X)) /\\\n  (!X Y. defined(multiply(X, Y)) \\/ ~defined(X) \\/ ~defined(Y)) /\\\n  defined(multiplicative_identity()) /\\\n  (!X.\n     defined(multiplicative_inverse(X)) \\/ ~defined(X) \\/\n     sum(additive_identity(), X, additive_identity())) /\\\n  (!Y X. sum(X, Y, add(X, Y)) \\/ ~defined(X) \\/ ~defined(Y)) /\\\n  (!Y X. product(X, Y, multiply(X, Y)) \\/ ~defined(X) \\/ ~defined(Y)) /\\\n  (!Y X.\n     sum(additive_identity(), X, Y) \\/ ~less_or_equal(X, Y) \\/\n     ~less_or_equal(Y, X)) /\\\n  (!Y X Z.\n     less_or_equal(X, Z) \\/ ~less_or_equal(X, Y) \\/\n     ~less_or_equal(Y, Z)) /\\\n  (!Y X.\n     less_or_equal(X, Y) \\/ less_or_equal(Y, X) \\/ ~defined(X) \\/\n     ~defined(Y)) /\\\n  (!X U V Z Y.\n     less_or_equal(U, V) \\/ ~less_or_equal(X, Y) \\/ ~sum(X, Z, U) \\/\n     ~sum(Y, Z, V)) /\\\n  (!X Z Y.\n     less_or_equal(additive_identity(), Z) \\/\n     ~less_or_equal(additive_identity(), X) \\/\n     ~less_or_equal(additive_identity(), Y) \\/ ~product(X, Y, Z)) /\\\n  ~sum(additive_identity(), additive_identity(),\n     multiplicative_identity()) /\\ defined(a()) /\\ defined(b()) /\\\n  (!X. ~sum(a(), X, b())) ==> F"];
prove fld5;
*)

end
(*#line 0.0 "data/preamble.sml"*)
(* ========================================================================= *)
(* SETTING UP THE ENVIRONMENT IN WHICH WE CAN EXECUTE THE METIS PROVER       *)
(* Created by Joe Hurd, September 2001                                       *)
(* ========================================================================= *)

(* Loading the modules we use *)

structure Main =
struct

fun main _ =
let

val () = app load
  ["CommandLine",
   "Milton",
   "Useful", "Term1", "Canon1", "Tptp1", "Metis1", "Problem1"];

(* Infix operators *)

infixr ## |-> ::> @> oo;

(* Pretty printers *)

val () = installPP Term1.pp_term;
val () = installPP Term1.pp_formula;
val () = installPP Subst1.pp_subst;
val () = installPP Thm1.pp_thm;

(* Parsing quotations *)

val () = quotation := true;

(* Creating nice output *)

local
  fun dup _ 0 l = l | dup x n l = dup x (n - 1) (x :: l);
  fun chs x n = implode (dup x n []);
in
  fun advertize s = print ("==" ^ s ^ chs #"=" (77 - size s) ^ "\n\n");
  fun separator () = print (chs #"-" 79 ^ "\n\n");
end;

fun cutoff max =
  let
    fun cut feas sofa l =
      let val poss = sofa ^ " ... " ^ Useful.int_to_string (length l) ^ " more"
      in cut' (if size poss < max then poss else feas) sofa l
      end
    and cut' _ sofa [] = sofa
      | cut' feas sofa (h :: t) =
      let val sofa' = if sofa = "" then h else sofa ^ " " ^ h
      in if size sofa' < max then cut feas sofa' t else feas
      end
  in
    cut "" ""
  end;

local
  fun b2s true = "on" | b2s false = "off";
  val i2s = Useful.int_to_string;
  val l2s = Meter1.limit_to_string;
in
  fun show (settings : Metis1.parameters) =
    let
      val {meson = Mactive, delta = Dactive, resolution = Ractive,
           meson_parm = Mparm, resolution_parm = Rparm} = settings
    in
      "resolution             = " ^ b2s Ractive ^ "\n" ^
      "meson                  = " ^ b2s Mactive ^ "\n" ^
      "delta                  = " ^ b2s Dactive ^ "\n" ^
      "\n" ^
      "resolution_parm:\n" ^
      "  subsumption_checking = " ^ i2s (#subsumption_checking Rparm) ^ "\n" ^
      "  positive_refinement  = " ^ b2s (#positive_refinement Rparm) ^ "\n" ^
      "  theap_parm:\n" ^
      "    fifo_skew          = " ^ i2s (#fifo_skew (#theap_parm Rparm)) ^ "\n" ^
      "    theap_cleaning     = " ^ i2s (#cleaning_freq (#theap_parm Rparm)) ^"\n"^
      "\n" ^
      "meson_parm:\n" ^
      "  ancestor_pruning     = " ^ b2s (#ancestor_pruning Mparm) ^ "\n" ^
      "  ancestor_cutting     = " ^ b2s (#ancestor_cutting Mparm) ^ "\n" ^
      "  state_simplify       = " ^ b2s (#state_simplify Mparm) ^ "\n" ^
      "  cache_cutting        = " ^ b2s (#cache_cutting Mparm) ^ "\n" ^
      "  divide_conquer       = " ^ b2s (#divide_conquer Mparm) ^ "\n" ^
      "  unit_lemmaizing      = " ^ b2s (#unit_lemmaizing Mparm) ^ "\n" ^
      "\n" ^
      "limit                  = " ^ l2s (!Metis1.limit) ^ "\n\n"
    end;
end;

(* The core proving function *)

val cnf_normalization = ref false;

fun with_cnf b = Useful.with_flag (cnf_normalization, Useful.K b);

fun core_prove fm =
  let
    val prover = if !cnf_normalization then Metis1.prove else Metis1.raw_prove
  in
    case Useful.try prover fm of SOME _
      => print "METIS: SUCCESSFULLY PROVED\nMETIS: "
    | NONE => print "METIS: FAILED TO PROVE\nMETIS: "
  end;

fun process name goal =
  (print ("METIS: Problem " ^ name ^ "\n");
   Milton.time core_prove goal;
   print "\n");

fun process_set (n, s) =
  let
    val () = advertize n
    fun f {name, goal} = process name (Term1.parse_formula goal)
  in
    case s of [] => ()
    | p :: ps => (f p; app (fn x => (separator (); f x)) ps)
  end;

(* Get options from the command line *)

local
  open Useful Metis1;

  fun tlimit "-" = NONE | tlimit s = SOME (Real.fromInt (string_to_int s));

  fun opts [] = 0
    | opts (x :: xs) =
    if x = "-t" orelse x = "--time" then
      case xs of [] => raise Fail "options: last argument -t / --time"
      | y :: ys => (limit := {time = tlimit y, infs = NONE}; opts ys)
    else if x = "-m" orelse x = "--meson" then
      (settings := update_parm_meson not (!settings); opts xs)
    else if x = "-r" orelse x = "--resolution" then
      (settings := update_parm_resolution not (!settings); opts xs)
    else if x = "-d" orelse x = "--delta" then
      (settings := update_parm_delta not (!settings); opts xs)
    else if x = "--" then length xs
    else if hd (explode x) = #"-" then raise Fail ("unknown parameter: " ^ x)
    else 1 + length xs;
in
  fun options () =
    let
      val () = settings := update_parm_resolution (K false) (!settings);
      val () = settings := update_parm_meson      (K false) (!settings);
      val () = settings := update_parm_delta      (K false) (!settings);

      val l = (**CommandLine.arguments ()**) []
      val n = opts l
    in
      split l (length l - n)
    end;
end;

val (opts, work) = if Milton.ml = "MLton" then options () else ([], []);
(*#line 0.0 "data/benchmark.sml"*)
val pure = null ((**CommandLine.arguments ()**) []);

local
  open Useful Metis1;
in
  val () =
    if pure then settings:= update_parm_meson (K true) (!settings) else ();
end;

local
  open Useful Problem1;
  fun extract p n =
     (Option.valOf o List.find (fn {name, goal = _} => name = n)) p;

  val meson_prune =
    if pure then ["P29", "LDA007-3", "GRP010-4", "GEO002-4"] else ["GEO002-4"];
      
  val prune =
    let
      val {meson, resolution, ...} = !Metis1.settings
    in
      (fn f => List.filter (not o f))
      (case (meson, resolution) of (false, false) => K true
       | (false, true) => C mem ["COL060-3"]
       | (true, false) => C mem meson_prune
       | (true, true) => K false)
    end;

  val src0 = ["P26", "P29", "P46", "GILMORE_1", "LOS", "STEAM_ROLLER"];

  val src1 = ["P48", "P49", "AGATHA"];

  val src2 =
    ["LCL009-1", "COL060-3", "COL058-2", "LCL107-1", "LDA007-3",
     "GRP010-4", "BOO021-1", "GEO002-4", "GRP128-4.003"];
in
  val set0 = map (extract nonequality) (prune src0);
  val set1 = map (extract equality)    (prune src1);
  val set2 = map (extract tptp)        (prune src2);
end;

val program = "benchmark" ^ (if pure then "*" else "");

val () = advertize (program ^ "==starting");

val () = advertize "settings";

val () = print (show (!Metis1.settings));

val () = with_cnf true process_set ("nonequality", set0);

val () = with_cnf true process_set ("equality", set1);

val () = with_cnf false process_set ("tptp", set2);

val () = advertize (program ^ "==finishing");

in
  ()
end;

fun doit n =
   if n = 0
      then ()
   else (main (); doit (n - 1))

end
