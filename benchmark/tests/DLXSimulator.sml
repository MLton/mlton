(* Minor tweaks by Stephen Weeks (sweeks@sweeks.com) on 2001-07-17 to turn into a
 * benchmark.
 * Added rand function.
 *)
(*
 * Matthew Thomas Fluet
 * Harvey Mudd College
 * Claremont, CA 91711
 * e-mail: Matthew_Fluet@hmc.edu
 *
 * A DLX Simulator in Standard ML
 *
 * Description:
 * The DLX Simulator is a partial implementation of the RISC instruction
 * set described in Patterson's and Hennessy's _Computer Architecture_.
 * Currently, the DLX Simulator implements the following instructions:
 *   ADD     ADDI
 *   ADDU    ADDUI
 *   SUB     SUBI
 *   SUBU    SUBUI
 *   AND     ANDI
 *   OR      ORI
 *   XOR     XORI
 *
 *   LHI
 *
 *   SLL     SLLI
 *   SRL     SRLI
 *   SRA     SRAI
 *
 *   SEQ     SEQI
 *   SNE     SNEI
 *   SLT     SLTI
 *   SGT     SGTI
 *   SLE     SLEI
 *   SGE     SGEI
 *
 *   LB      LBU     SB
 *   LH      LHU     SH
 *   LW      SW
 *
 *   BEQZ    BNEZ
 *   J       JR
 *   JAL     JALR
 *
 *   TRAP
 *
 *   NOP
 *
 * Currently, the DLX Simulator uses 32 bit words for addressing and
 * the register file and a 65535 word memory.  To augment the memory
 * a cache can be installed in the simulator, with a number of different
 * caching options that can be made.  Caches can also cache other caches,
 * so realistic dual level caches can be simulated.  Input and output
 * is limited to requesting and outputing signed integers.
 *
 * Usage:
 * DLXSimulatorCX.run_file : string -> unit
 * DLXSimulatorCX.run_prog : string list -> unit;
 * The DLXSimualatorCX family of structures represent different caches
 * used on the simulator.  The following table describes the different
 * caches used:
 * C1: a small level 1 cache
 * DLXSimulatorCX.run_file attempts to open and execute the instructions
 * in a file.  DLXSimulatorCX.run_prog runs a set of instructions as
 * a list of strings.  Four programs are included here.
 * Simple : simply outputs the number 42.
 * Twos: performs the twos complement on an inputed number.
 * Abs: performs the absolute value on an imputed number.
 * Fact: performs the factorial on an inputed number.
 * GCD: performs the greatest common divisor on two imputed numbers.
 * After running, the DLX Simulator outputs a set of statistics
 * concerning memory reads and writes, and cache hits and misses.
 *
 * Future Work: 
 * With the implementation of the PACK_REAL structures
 * as documented in the SML'97 Basis Library, the remainder
 * of the DLX instruction set should be implemented.
 * Currently, without an efficient and correct means of 
 * converting a 32 bit word into a 32 bit float, it is
 * difficult to incorporate these instructions.
 * In order to finish following the current development
 * model, a FPALU structure should be implemented as the 
 * floating point arithmetic-logic unit.
 * Another possibility for future work would be to 
 * model a pipelined processor.  Currently, the DLX Simulator
 * uses a simple one cycle per instruction model.
 * It should be possible to break this model and implement
 * a pipeline, but it would mean a major reworking of the
 * DLXSimulatorFun functor.
 *
 * References:
 * Patterson, David A. and John L. Hennessy.  _Computer Architecture: A
 *   Quantitative Approach: Second Edition_.  San Francisco: Morgan 
 *   Kaufmann Publishers, Inc., 1996.
 *
 *)

(* ************************************************************************* *)

(* sweeks added rand *)
local
   open Word
   val seed: word ref = ref 0w13
in
   (* From page 284 of Numerical Recipes in C. *)
   fun rand (): word =
      let
         val res = 0w1664525 * !seed + 0w1013904223
         val _ = seed := res
      in
         res
      end
end

(* 
 * ImmArray.sml
 *
 * The ImmArray structure defines an immutable array implementation.
 * An immarray is stored internally as a list.
 * This results in O(n) sub and update functions, as opposed
 * to O(1) sub and update functions found in Array.  However,
 * immutable arrays are truly immutable, and can be integrated
 * with a functionally programming style easier than mutable
 * arrays.
 *
 * The ImmArray structure mimics the Array structure as much as possible.
 * The most obvious deviation is that unit return types in Array are replaced
 * by 'a immarray return types in ImmArray.  Unlike an 'a array, an 'a immarray
 * is an equality type if and only if 'a is an equality type.  Further immarray
 * equality is structural, rather than the "creation" equality used by Array.
 * Additionally, no vector type is supported, and consequently no copyVec
 * function is supported.  Finally, the functions mapi and map provide
 * similar functionality as modifyi and modify, but relax the constraint that
 * the argument function need be of type 'a -> 'a.
 *
 * Future Work : There are random-access list implementations
 *               that support O(log n) sub and update functions,
 *               which may provide a faster implementation, although
 *               possibly at the expense of space and the ease of 
 *               implementing app, foldl, foldr, modify, and map functions.
 *)

signature IMMARRAY
  = sig
      type 'a immarray;
        
      val maxLen : int;
      val immarray : (int * 'a) -> 'a immarray;
      val fromList : 'a list -> 'a immarray;
      val toList : 'a immarray -> 'a list;
  
      val tabulate : int * (int -> 'a) -> 'a immarray;
      val length : 'a immarray -> int;
        
      val sub : 'a immarray * int -> 'a;
      val update : 'a immarray * int * 'a -> 'a immarray;
      val extract : 'a immarray * int * int option -> 'a immarray;

      val copy : {src : 'a immarray, si : int, len : int option, 
                  dst : 'a immarray, di : int} -> 'a immarray;

      val appi : (int * 'a -> unit) -> ('a immarray * int * int option) 
                 -> unit;
      val app : ('a -> unit) -> 'a immarray -> unit;
      val foldli : ((int * 'a * 'b) -> 'b) -> 'b 
                   -> ('a immarray * int * int option) -> 'b;
      val foldri : ((int * 'a * 'b) -> 'b) -> 'b 
                   -> ('a immarray * int * int option) -> 'b;
      val foldl : (('a * 'b) -> 'b) -> 'b -> 'a immarray -> 'b;
      val foldr : (('a * 'b) -> 'b) -> 'b -> 'a immarray -> 'b;
      val mapi : ((int * 'a) -> 'b) -> ('a immarray * int * int option)
                 ->  'b immarray;
      val map : ('a -> 'b) -> 'a immarray -> 'b immarray;
      val modifyi : ((int * 'a) -> 'a) -> ('a immarray * int * int option) 
                    -> 'a immarray;
      val modify : ('a -> 'a) -> 'a immarray -> 'a immarray;
    end;
    
    
structure ImmArray : IMMARRAY
  = struct
  
      (* datatype 'a immarray
       * An immarray is stored internally as a list.
       * The use of a constructor prevents list functions from
       * treating immarray type as a list.
       *)
      datatype 'a immarray = IA of 'a list;
        
      (* val maxLen : int
       * The maximum length of immarrays supported.
       * Technically, under this implementation, the maximum length
       * of immarrays is the same as the maximum length of a list,
       * but for convience and compatibility, use the Array structure's
       * maximum length.
       *)
      val maxLen = Array.maxLen;

      (* val tabulate : int * (int -> 'a) -> 'a immarray
       * val immarray : int * 'a -> 'a immarray
       * val fromList : 'a list -> 'a immarray
       * val toList : 'a immarray -> 'a list
       * val length : 'a immarray -> int
       * These functions perform basic immarray functions.
       * The tabulate, immarray, and fromList functions create an immarray.
       * The toList function converts an immarray to a list.
       * The length function returns the length of an immarray.
       *)
      fun tabulate (n, initfn) = IA (List.tabulate (n, initfn));
      fun immarray (n, init) = tabulate (n, fn _ => init);
      fun fromList l = IA l;
      fun toList (IA ia) = ia;
      fun length (IA ia) = List.length ia;
        
      (* val sub : 'a immarray * int -> 'a
       * val update : 'a immarray * int * 'a -> 'a immarray
       * These functions sub and update an immarray by index.
       *)
      fun sub (IA ia, i) = List.nth (ia, i);
      fun update (IA ia, i, x) = IA ((List.take (ia, i)) @ 
                                     (x::(List.drop (ia, i + 1))));
        
      (* val extract : 'a immarray * int * int option -> 'a immarray
       * This function extracts an immarray slice from an immarray from
       * one index either through the rest of the immarray (NONE)
       * or for n elements (SOME n), as described in the 
       * Standard ML Basis Library.
       *)
      fun extract (IA ia, i, NONE) = IA (List.drop (ia, i))
        | extract (IA ia, i, SOME n) = IA (List.take (List.drop (ia, i), n));
        
      (* val copy : {src : 'a immarray, si : int, len : int option, 
                     dst : 'a immarray, di : int} -> 'a immarray
       * This function copies an immarray slice from src into dst starting
       * at the di element.
       *)
      fun copy {src, si, len, dst=IA ia, di} 
        = let
            val IA sia = extract (src, si, len);
            val pre = List.take (ia, di);
            val post = case len 
                         of NONE => List.drop (ia, di+(List.length sia))
                          | SOME n => List.drop (ia, di+n);
          in 
            IA (pre @ sia @ post)
          end;
                                           
      (* val appi : ('a * int -> unit) -> ('a immarray * int * int option) 
       *            -> unit
       * val app : ('a -> unit) -> 'a immarray -> unit
       * These functions apply a function to every element
       * of an immarray.  The appi function also provides the
       * index of the element as an argument to the applied function
       * and uses an immarray slice argument.
       *)
      local
        fun appi_aux f i [] = ()
          | appi_aux f i (h::t) = (f(i,h); appi_aux f (i + 1) t);
      in
        fun appi f (IA ia, i, len) = let 
                                       val IA sia = extract (IA ia, i, len);
                                     in 
                                       appi_aux f i sia
                                     end;
      end;
      fun app f immarr = appi (f o #2) (immarr, 0, NONE);

      (* val foldli : (int * 'a * 'b -> 'b) -> 'b 
       *              -> ('a immarray * int * int option) -> 'b;
       * val foldri : (int * 'a * 'b -> 'b) -> 'b
       *              -> ('a immarray * int * int option) -> 'b;
       * val foldl : ('a * 'b -> 'b) -> 'b -> 'a immarray -> 'b
       * val foldr : ('a * 'b -> 'b) -> 'b -> 'a immarray -> 'b
       * These functions fold a function over every element
       * of an immarray.  The foldri and foldli functions also provide
       * the index of the element as an argument to the folded function
       * and uses an immarray slice argument.
       *)
      local 
        fun foldli_aux f b i [] = b
          | foldli_aux f b i (h::t) = foldli_aux f (f(i,h,b)) (i+1) t;
        fun foldri_aux f b i [] = b
          | foldri_aux f b i (h::t) = f(i,h,foldri_aux f b (i+1) t);
      in
        fun foldli f b (IA ia, i, len) 
          = let 
              val IA ia2 = extract (IA ia, i, len);
            in 
              foldli_aux f b i ia2
            end;
        fun foldri f b (IA ia, i, len) 
          = let
              val IA ia2 = extract (IA ia, i, len);
            in 
              foldri_aux f b i ia2
            end;
      end;
      fun foldl f b (IA ia) = foldli (fn (_,i,x) => f(i,x)) b (IA ia, 0, NONE);
      fun foldr f b (IA ia) = foldri (fn (_,i,x) => f(i,x)) b (IA ia, 0, NONE);

      (* val mapi : ('a * int -> 'b) -> 'a immarray -> 'b immarray
       * val map : ('a -> 'b) -> 'a immarray -> 'b immarray
       * These functions map a function over every element 
       * of an immarray.  The mapi function also provides the
       * index of the element as an argument to the mapped function
       * and uses an immarray slice argument.  Although there are
       * similarities between mapi and modifyi, note that when mapi is 
       * used with an immarray slice, the resulting immarray is the
       * same size as the slice.  This is necessary to preserve the
       * type of the resulting immarray.  Thus, mapi with the identity
       * function reduces to the extract function. 
       *)
      local
        fun mapi_aux f i [] = []
          | mapi_aux f i (h::t) = (f (i,h))::(mapi_aux f (i + 1) t);
      in
        fun mapi f (IA ia, i, len) = let 
                                       val IA ia2 = extract (IA ia, i, len);
                                     in 
                                       IA (mapi_aux f i ia2)
                                     end;
      end;
      fun map f (IA ia)= mapi (f o #2) (IA ia, 0, NONE);

      (* val modifyi : (int * 'a -> 'a) -> ('a immarray * int * int option) 
       *               -> 'a immarray
       * val modify : ('a -> 'a) -> 'a immarray -> 'a immarray
       * These functions apply a function to every element of an immarray
       * in left to right order and returns a new immarray where corresponding
       * elements are replaced by their modified values.  The modifyi 
       * function also provides the index of the element as an argument 
       * to the mapped function and uses an immarray slice argument.
       *)
      local
        fun modifyi_aux f i [] = []
          | modifyi_aux f i (h::t) = (f (i,h))::(modifyi_aux f (i + 1) t);
      in
        fun modifyi f (IA ia, i, len)
          = let
              val pre = List.take (ia, i);
              val IA ia2 = extract (IA ia, i, len);
              val post = case len 
                           of NONE => []
                            | SOME n => List.drop (ia, i+n);
            in
              IA (pre @ (modifyi_aux f i ia2) @ post)
            end;
      end;
      fun modify f (IA ia) = modifyi (f o #2) (IA ia, 0, NONE);

    end;

(* ************************************************************************* *)

(*
 * ImmArray2.sml
 * 
 * The ImmArray2 structure defines a two dimensional immutable array 
 * implementation.  An immarray2 is stored internally as an immutable 
 * array of immutable arrays.  As such, the ImmArray2 makes heavy use 
 * of the ImmArray structure.
 *
 * The ImmArray2 structure mimics the Array2 structure as much as possible.
 * The most obvious deviation is that unit return types in Array2 are replaced
 * by 'a immarray2 return types in ImmArray2.  Unlike an 'a array, 
 * an 'a immarray2 is an equality type if and only if 'a is an equality type.
 * Further immarray2 equality is structural, rather than the "creation" 
 * equality used by Array2.  Also, the 'a region type is not included in
 * ImmArray2, but all functions in Array2 that require 'a regions are present
 * with arguments taken in the natural order.  Finally, the functions mapi 
 * and map provide similar functionality as modifyi and modify, but relax 
 * the constraint that the argument function need be of type 'a -> 'a.
 *)

signature IMMARRAY2
  = sig

      type 'a immarray2;

      datatype traversal = RowMajor | ColMajor

      val immarray2 : int * int * 'a -> 'a immarray2;
      val tabulate : traversal -> int * int * ((int * int) -> 'a) 
                     -> 'a immarray2;
      val fromList : 'a list list -> 'a immarray2;
      val dimensions : 'a immarray2 -> int * int;

      val sub : 'a immarray2 * int * int -> 'a;
      val update : 'a immarray2 * int * int * 'a -> 'a immarray2;
      val extract : 'a immarray2 * int * int * int option * int option 
                    -> 'a immarray2;
                    
      val copy : {src : 'a immarray2, si : int, sj : int,
                  ilen : int option, jlen : int option,
                  dst : 'a immarray2, di : int, dj : int} -> 'a immarray2;

      val nRows : 'a immarray2 -> int;
      val nCols : 'a immarray2 -> int;
      val row : 'a immarray2 * int -> 'a ImmArray.immarray;
      val column : 'a immarray2 * int -> 'a ImmArray.immarray;

      val appi : traversal -> (int * int * 'a -> unit) 
                 -> ('a immarray2 * int * int * int option * int option) 
                 -> unit;
      val app : traversal -> ('a -> unit) -> 'a immarray2 -> unit;
      val foldli : traversal -> ((int * int * 'a * 'b) -> 'b) -> 'b 
                   -> ('a immarray2 * int * int * int option * int option) 
                   -> 'b
      val foldri : traversal -> ((int * int * 'a * 'b) -> 'b) -> 'b 
                   -> ('a immarray2 * int * int * int option * int option) 
                   -> 'b
      val foldl : traversal -> (('a * 'b) -> 'b) -> 'b -> 'a immarray2 -> 'b
      val foldr : traversal -> (('a * 'b) -> 'b) -> 'b -> 'a immarray2 -> 'b
      val mapi : traversal -> (int * int * 'a -> 'b) 
                 -> ('a immarray2 * int * int * int option * int option) 
                 -> 'b immarray2;
      val map : traversal -> ('a -> 'b) -> 'a immarray2 -> 'b immarray2;
      val modifyi : traversal -> ((int * int * 'a) -> 'a) 
                    -> ('a immarray2 * int * int * int option * int option) 
                    -> 'a immarray2;
      val modify : traversal -> ('a -> 'a) -> 'a immarray2 -> 'a immarray2;
    end;

structure ImmArray2 : IMMARRAY2
  = struct
  
      (* datatype 'a immarray2
       * An immarray2 is stored internally as an immutable array 
       * of immutable arrays.  The use of a contructor prevents ImmArray
       * functions from treating the immarray2 type as an immarray.
      *)
      datatype 'a immarray2 = IA2 of 'a ImmArray.immarray ImmArray.immarray;
      datatype traversal = RowMajor | ColMajor

      (* val tabulate : traversal -> int * int * (int * int -> 'a) 
       *                -> 'a immarray2
       * val immarray2 : int * int * 'a -> 'a immarray2
       * val fromList : 'a list list -> 'a immarray2
       * val dmensions : 'a immarray2 -> int * int
       * These functions perform basic immarray2 functions.
       * The tabulate and immarray2 functions create an immarray2.
       * The fromList function converts a list of lists into an immarray2.
       * Unlike Array2.fromList, fromList will accept lists of different
       * lengths, allowing one to create an immarray2 in which the
       * rows have different numbers of columns, although it is likely that
       * exceptions will be raised when other ImmArray2 functions are applied
       * to such an immarray2.  Note that dimensions will return the 
       * number of columns in row 0.
       * The dimensions function returns the dimensions of an immarray2.
       *)
      fun tabulate RowMajor (r, c, initfn)
        = let 
            fun initrow r = ImmArray.tabulate (c, fn ic => initfn (r,ic));  
          in
            IA2 (ImmArray.tabulate (r, fn ir => initrow ir))
          end
        | tabulate ColMajor (r, c, initfn)
          = turn (tabulate RowMajor (c,r, fn (c,r) => initfn(r,c)))
      and immarray2 (r, c, init) = tabulate RowMajor (r, c, fn (_, _) => init)
      and fromList l 
        = IA2 (ImmArray.tabulate (length l, 
                                  fn ir => ImmArray.fromList (List.nth(l,ir))))
      and dimensions (IA2 ia2) = (ImmArray.length ia2, 
                                  ImmArray.length (ImmArray.sub (ia2, 0)))

      (* turn : 'a immarray2 -> 'a immarray2
       * This function reverses the rows and columns of an immarray2
       * to allow handling of ColMajor traversals.
       *)
      and turn ia2 = let
                       val (r,c) = dimensions ia2;
                     in 
                       tabulate RowMajor (c,r,fn (cc,rr) => sub (ia2,rr,cc))
                     end

      (* val sub : 'a immarray2 * int * int -> 'a
       * val update : 'a immarray2 * int * int * 'a -> 'a immarray2
       * These functions sub and update an immarray2 by indices.
       *)
      and sub (IA2 ia2, r, c) = ImmArray.sub(ImmArray.sub (ia2, r), c);
      fun update (IA2 ia2, r, c, x)
          = IA2 (ImmArray.update (ia2, r, 
                                  ImmArray.update (ImmArray.sub (ia2, r), 
                                                   c, x)));       

      (* val extract : 'a immarray2 * int * int * 
       *               int option * int option -> 'a immarray2
       * This function extracts a subarray from an immarray2 from
       * one pair of indices either through the rest of the
       * immarray2 (NONE, NONE) or for the specfied number of elements.
       *)
      fun extract (IA2 ia2, i, j, rlen, clen)
          = IA2 (ImmArray.map (fn ia => ImmArray.extract (ia, j, clen))
                              (ImmArray.extract (ia2, i, rlen)));
        
      (* val nRows : 'a immarray2 -> int
       * val nCols : 'a immarray2 -> int
       * These functions return specific dimensions of an immarray2.
       *)
      fun nRows (IA2 ia2) = (#1 o dimensions) (IA2 ia2);
      fun nCols (IA2 ia2) = (#2 o dimensions) (IA2 ia2);
      (* val row : immarray2 * int -> ImmArray.immarray
       * val column : immarray2 * int -> ImmArray.immarray
       * These functions extract an entire row or column from
       * an immarray2 by index, returning the row or column as
       * an ImmArray.immarray.
       *)
      fun row (ia2, r) = let
                           val (c, _) = dimensions ia2;
                         in
                           ImmArray.tabulate (c, fn i => sub (ia2, r, i))
                         end;
      fun column (ia2, c) = let
                              val (_, r) = dimensions ia2;
                            in
                              ImmArray.tabulate (r, fn i => sub (ia2, i, c))
                            end;

      (* val copy : {src : 'a immarray2, si : int, sj : int,
       *             ilen : int option, jlen : int option,
       *             dst : 'a immarray2, di : int, dj : int};
       * This function copies an immarray2 slice from src int dst starting
       * at the di,dj element.
       *)
      fun copy {src, si, sj, ilen, jlen, dst=IA2 ia2, di, dj}
        = let
            val nilen = case ilen 
                          of NONE => SOME ((nRows src) - si)
                           | SOME n => SOME n;
          in 
            IA2 (ImmArray.modifyi (fn (r, ia) 
                                   => ImmArray.copy {src=row (src, si+r-di),
                                                     si=sj, len=jlen, 
                                                     dst=ia, di=dj}) 
                                  (ia2, di, nilen))
          end;

      (* val appi : traversal -> ('a * int * int -> unit) -> 'a immarray2 
       *            -> unit
       * val app : traversal -> ('a -> unit) -> 'a immarray2 -> unit
       * These functions apply a function to every element
       * of an immarray2.  The appi function also provides the
       * indices of the element as an argument to the applied function
       * and uses an immarray2 slice argument.
       *)
      fun appi RowMajor f (IA2 ia2, i, j, rlen, clen)
        = ImmArray.appi (fn (r,ia) => ImmArray.appi (fn (c,x) => f(r,c,x))
                                                    (ia, j, clen))
                        (ia2, i, rlen)
        | appi ColMajor f (ia2, i, j, rlen, clen)
        = appi RowMajor (fn (c,r,x) => f(r,c,x)) (turn ia2, j, i, clen, rlen); 
      fun app tr f (IA2 ia2) = appi tr (f o #3) (IA2 ia2, 0, 0, NONE, NONE);

      (* val foldli : traversal -> ((int * int * 'a * 'b) -> 'b) -> 'b 
       *              -> ('a immarray2 * int * int * int option * int option) 
       *              -> 'b
       * val foldri : traversal -> ((int * int * 'a * 'b) -> 'b) -> 'b 
       *              -> ('a immarray2 * int * int * int option * int option) 
       *              -> 'b
       * val foldl : traversal -> ('a * 'b -> 'b) -> 'b -> 'a immarray2 -> 'b
       * val foldr : traversal -> ('a * 'b -> 'b) -> 'b -> 'a immarray2 -> 'b
       * These functions fold a function over every element
       * of an immarray2.  The foldri and foldli functions also provide
       * the index of the element as an argument to the folded function
       * and uses an immarray2 slice argument.
       *)
      fun foldli RowMajor f b (IA2 ia2, i, j, rlen, clen)
        = ImmArray.foldli (fn (r,ia,b) 
                           => ImmArray.foldli (fn (c,x,b) => f(r,c,x,b))
                                              b
                                              (ia, j, clen))
                          b
                          (ia2, i, rlen)
        | foldli ColMajor f b (ia2, i, j, rlen, clen)
        = foldli RowMajor (fn (c,r,x,b) => f(r,c,x,b)) b 
                 (turn ia2, j, i, clen, rlen);
      fun foldri RowMajor f b (IA2 ia2, i, j, rlen, clen)
        = ImmArray.foldri (fn (r,ia,b) 
                           => ImmArray.foldri (fn (c,x,b) => f(r,c,x,b))
                                              b
                                              (ia, j, clen))
                          b
                          (ia2, i, rlen)
        | foldri ColMajor f b (ia2, i, j, rlen, clen)
        = foldri RowMajor (fn (c,r,x,b) => f(r,c,x,b)) b 
                          (turn ia2, j, i, clen, rlen);
      fun foldl tr f b (IA2 ia2)
        = foldli tr (fn (_,_,x,b) => f(x,b)) b (IA2 ia2, 0, 0, NONE, NONE);
      fun foldr tr f b (IA2 ia2)
        = foldri tr (fn (_,_,x,b) => f(x,b)) b (IA2 ia2, 0, 0, NONE, NONE);

      (* val mapi : traversal -> ('a * int * int -> 'b) -> 'a immarray2 
       *            -> 'b immarray2
       * val map : traversal -> ('a -> 'b) -> 'a immarray2 -> 'b immarray2
       * These functions map a function over every element
       * of an immarray2.  The mapi function also provides the
       * indices of the element as an argument to the mapped function
       * and uses an immarray2 slice argument.  Although there are
       * similarities between mapi and modifyi, note that when mapi is 
       * used with an immarray2 slice, the resulting immarray2 is the
       * same size as the slice.  This is necessary to preserve the
       * type of the resulting immarray2.  Thus, mapi with the identity
       * function reduces to the extract function.
       *)
      fun mapi RowMajor f (IA2 ia2, i, j, rlen, clen)
        = IA2 (ImmArray.mapi (fn (r,ia) => ImmArray.mapi (fn (c,x) => f(r,c,x))
                                                         (ia, j, clen))
                             (ia2, i, rlen))
        | mapi ColMajor f (ia2, i, j, rlen, clen)
        = turn (mapi RowMajor (fn (c,r,x) => f(r,c,x)) 
                     (turn ia2, j, i, clen, rlen))
      fun map tr f (IA2 ia2)
        = mapi tr (f o #3) (IA2 ia2, 0, 0, NONE, NONE);

      (* val modifyi : traversal -> (int * int* 'a -> 'a) 
                       -> ('a immarray2 * int * int * int option * int option) 
       *               -> 'a immarray2
       * val modify : traversal -> ('a -> 'a) -> 'a immarray2 -> 'a immarray2
       * These functions apply a function to every element of an immarray2
       * in row by column order and returns a new immarray2 where corresponding
       * elements are replaced by their modified values.  The modifyi 
       * function also provides the index of the element as an argument 
       * to the mapped function and uses an immarray2 slice argument.
       *)
      fun modifyi RowMajor f (IA2 ia2, i, j, rlen, clen)
        = IA2 (ImmArray.modifyi (fn (r,ia) => ImmArray.modifyi (fn (c,x) 
                                                                => f(r,c,x)) 
                                                               (ia, j, clen)) 
              (ia2, i, rlen))
        | modifyi ColMajor f (ia2, i, j, rlen, clen)
        = turn (modifyi RowMajor (fn (c,r,x) => f(r,c,x)) 
               (turn ia2, j, i, clen, rlen));
      fun modify tr f (IA2 ia2) 
        = modifyi tr (f o #3) (IA2 ia2, 0, 0, NONE, NONE);

    end;

(* ************************************************************************* *)

(*
 * RegisterFile.sig
 *
 * This defines the exported datatype and functions provided by the
 * register file.  The datatype registerfile provides the encapsulation
 * of the register file, InitRegisterFile initializes the registerfile,
 * setting all registers to zero and setting r0, gp, sp, and fp to
 * their appropriate values, LoadRegister takes a registerfile and
 * an integer corresponding to the register, and returns the
 * Word32.word value at that register, and StoreRegister takes a
 * registerfile, an integer corresponding to the register, and a
 * Word32.word and returns the registerfile updated with the word
 * stored in the appropriate register.
 *)

signature REGISTERFILE
  = sig
  
      type registerfile;
        
      val InitRegisterFile : unit  -> registerfile; 

      val LoadRegister : registerfile * int -> Word32.word;

      val StoreRegister : registerfile * int * Word32.word -> registerfile;

    end;

(*****************************************************************************)

(*
 * RegisterFile.sml
 *
 * This defines the RegisterFile structure, which provides the
 * functionality of the register file.  The datatype registerfile
 * provides the encapsulation of the register file, InitRegisterFile
 * initializes the registerfile, setting all registers to zero and
 * setting r0, gp, sp, and fp to their appropriate values,
 * LoadRegister takes a registerfile and an integer corresponding to
 * the register, and returns the Word32.word value at that register, 
 * and StoreRegister takes a registerfile, an integer corresponding to
 * the register, and a Word32.word and returns the registerfile
 * updated with the word stored in the appropriate register.
 *
 * The underlying structure of registerfile is an immutable array of
 * Word32.word.
 *)

structure RegisterFile : REGISTERFILE
  = struct

      type registerfile = Word32.word ImmArray.immarray;
          
      fun InitRegisterFile () 
          = ImmArray.update
            (ImmArray.update
             (ImmArray.update
              (ImmArray.update
               (ImmArray.immarray(32, 0wx00000000 : Word32.word),
                00, 0wx00000000 : Word32.word),
               28, 0wx00000000 : Word32.word),
              29, 0wx00040000 : Word32.word),
             30, 0wx00040000 : Word32.word) : registerfile;
            
      fun LoadRegister (rf, reg) = ImmArray.sub(rf, reg);

      fun StoreRegister (rf, reg, data) = ImmArray.update(rf, reg, data);
        
    end;
    

(*****************************************************************************)

(*
 * ALU.sig
 * 
 * This defines the exported datatype and function provided by the
 * ALU.  The datatype ALUOp provides a means to specify which
 * operation is to be performed by the ALU, and PerformAL performs
 * one of the operations on two thirty-two bit words, returning the
 * result as a thirty-two bit word.
 *)
 
signature ALU
  = sig

      datatype ALUOp = SLL | SRL | SRA |
                       ADD | ADDU | 
                       SUB | SUBU | 
                       AND | OR | XOR | 
                       SEQ | SNE | 
                       SLT | SGT | 
                       SLE | SGE;
  
      val PerformAL : (ALUOp * Word32.word * Word32.word) -> Word32.word;
        
    end;

(*****************************************************************************)

(*
 * ALU.sml
 *
 * This defines the ALU structure, which provides the functionality of
 * an Arithmetic/Logic Unit.  The datatype ALUOp provides a means to 
 * specify which operation is to be performed by the ALU, and
 * PerformAL performs one of the operations on two thirty-two bit
 * words, returning the result as a thirty-two bit word.
 *
 * A note about SML'97 Basis Library implementation of thirty-two bit 
 * numbers: the Word32.word is an unsigned thirty-two bit integer,
 * while Int.int (equivalent to Int.int) is a signed thirty-two
 * bit integer.  In order to perform the signed operations, it is
 * necessary to convert the words to signed form, using the
 * Word32.toIntX function, which performs sign extension,
 * and to convert the result back into unsigned form using the
 * Word32.fromInt function.  In addition, to perform a shift,
 * the second Word32.word needs to be "downsized" to a normal
 * Word.word using the Word.fromWord function.
 *)

structure ALU : ALU 
  = struct

      datatype ALUOp = SLL | SRL | SRA |
                       ADD | ADDU | 
                       SUB | SUBU | 
                       AND | OR | XOR | 
                       SEQ | SNE | 
                       SLT | SGT | 
                       SLE | SGE;

      fun PerformAL (opcode, s1, s2) =
        (case opcode
           of SLL => 
                Word32.<< (s1, Word.fromLarge (Word32.toLarge s2))
            | SRL => 
                Word32.>> (s1, Word.fromLarge (Word32.toLarge s2))
            | SRA => 
                Word32.~>> (s1, Word.fromLarge (Word32.toLarge s2))
            | ADD => 
                Word32.fromInt (Int.+ (Word32.toIntX s1,
                                                 Word32.toIntX s2))
            | ADDU => 
                Word32.+ (s1, s2)
            | SUB =>
                Word32.fromInt (Int.- (Word32.toIntX s1,
                                                 Word32.toIntX s2))
            | SUBU => 
                Word32.- (s1, s2)
            | AND => 
                Word32.andb (s1, s2)
            | OR => 
                Word32.orb (s1, s2)
            | XOR => 
                Word32.xorb (s1, s2)
            | SEQ => 
                if (s1 = s2)
                  then 0wx00000001 : Word32.word
                  else 0wx00000000 : Word32.word
            | SNE => 
                if not (s1 = s2)
                  then 0wx00000001 : Word32.word
                  else 0wx00000000 : Word32.word
            | SLT => 
                if Int.< (Word32.toIntX s1, Word32.toIntX s2)
                  then 0wx00000001 : Word32.word
                  else 0wx00000000 : Word32.word
            | SGT => 
                if Int.> (Word32.toIntX s1, Word32.toIntX s2)
                  then 0wx00000001 : Word32.word
                  else 0wx00000000 : Word32.word
            | SLE => 
                if Int.<= (Word32.toIntX s1, Word32.toIntX s2)
                  then 0wx00000001 : Word32.word
                  else 0wx00000000 : Word32.word
            | SGE => 
                if Int.>= (Word32.toIntX s1, Word32.toIntX s2)
                  then 0wx00000001 : Word32.word
                  else 0wx00000000 : Word32.word)
           (* 
            * This handle will handle all ALU errors, most
            * notably overflow and division by zero, and will
            * print an error message and return 0.                
            *)
           handle _ => 
             (print "Error : ALU returning 0\n";
              0wx00000000 : Word32.word);
             
    end;

(*****************************************************************************)

(*
 * Memory.sig
 *
 * This defines the exported datatype and functions provided by 
 * memory.  The datatype memory provides the encapsulation
 * of memory, InitMemory initializes memory, setting all 
 * addresses to zero, LoadWord takes memory and
 * a Word32.word corresponding to the address, and returns the
 * Word32.word value at that address, StoreWord takes memory, 
 * a Word32.word corresponding to the address, and a
 * Word32.word and returns memory updated with the word
 * stored at the appropriate address.  LoadHWord, LoadHWordU, 
 * LoadByte, and LoadByteU load halfwords, unsigned halfwords,
 * bytes, and unsigned bytes respectively from memory into the
 * lower portion of the returned Word32.word.  StoreHWord and 
 * StoreByte store halfwords and bytes taken from the lower portion
 * of the Word32.word into memory.
 * GetStatistics takes memory and returns the read and write 
 * statistics as a string.
 *)

signature MEMORY
  = sig

      type memory;
        
      val InitMemory : unit -> memory; 

      val LoadWord : memory * Word32.word -> memory * Word32.word;
      val StoreWord : memory * Word32.word * Word32.word -> memory;

      val LoadHWord : memory * Word32.word -> memory * Word32.word;
      val LoadHWordU : memory * Word32.word -> memory * Word32.word;
      val StoreHWord : memory * Word32.word * Word32.word -> memory;

      val LoadByte : memory * Word32.word -> memory * Word32.word;
      val LoadByteU : memory * Word32.word -> memory * Word32.word;
      val StoreByte : memory * Word32.word * Word32.word -> memory;

      val GetStatistics : memory -> string;

    end;





(*****************************************************************************)

(*
 * Memory.sml
 *
 * This defines the Memory structure, which provides the functionality
 * of memory.  The datatype memory provides the encapsulation of
 * memory, InitMemory initializes memory, setting all 
 * addresses to zero, LoadWord takes memory and
 * a Word32.word corresponding to the address, and returns the
 * Word32.word value at that address and the updated memory, 
 * StoreWord takes memory, a Word32.word corresponding to the 
 * address, and a Word32.word and returns memory updated with the word
 * stored at the appropriate address.  LoadHWord, LoadHWordU, 
 * LoadByte, and LoadByteU load halfwords, unsigned halfwords,
 * bytes, and unsigned bytes respectively from memory into the
 * lower portion of the returned Word32.word.  StoreHWord and 
 * StoreByte store halfwords and bytes taken from the lower portion
 * of the Word32.word into memory.
 * GetStatistics takes memory and returns the read and write 
 * statistics as a string.
 *
 * The underlying structure of memory is an immutable array of Word32.word.
 * The array has a length of 0x10000, since every element of the array
 * corresponds to a thirty-two bit integer.
 *
 * Also, the functions AlignWAddress and AlignHWAddress aligns a memory 
 * address to a word and halfword address, respectively.  If LoadWord,
 * StoreWord, LoadHWord, LoadHWordU, or StoreHWord is asked to access an 
 * unaligned address, it writes an error message, and uses the address 
 * rounded down to the aligned address.
 *)

structure Memory : MEMORY
  = struct
        
      type memory = Word32.word ImmArray.immarray * (int * int);
        
      fun InitMemory () =
        (ImmArray.immarray(Word32.toInt(0wx10000 : Word32.word),
                           0wx00000000 : Word32.word), 
         (0, 0)) : memory;

      fun AlignWAddress address 
          = Word32.<< (Word32.>> (address, 0wx0002), 0wx0002);
            
      fun AlignHWAddress address
          = Word32.<< (Word32.>> (address, 0wx0001), 0wx0001);
            
      (* Load and Store provide errorless access to memory.
       * They provide a common interface to memory, while
       * the LoadX and StoreX specifically access words,
       * halfwords and bytes, requiring address to be aligned.
       * In Load and Store, two intermediate values are
       * generated.  The value aligned_address is the aligned
       * version of the given address, and is used to compare with
       * the original address to determine if it was aligned.  The 
       * value use_address is equivalent to aligned_address divided 
       * by four, and it corresponds to the index of the memory 
       * array where the corresponding aligned address can be found.
       *)
          
      fun Load ((mem, (reads, writes)), address)
          = let
              val aligned_address = AlignWAddress address;
              val use_address = Word32.>> (aligned_address, 0wx0002);
            in
              ((mem, (reads + 1, writes)), 
               ImmArray.sub(mem, Word32.toInt(use_address)))
            end;

      fun Store ((mem, (reads, writes)), address, data)
          = let
              val aligned_address = AlignWAddress address;
              val use_address = Word32.>> (aligned_address, 0wx0002);
            in
              (ImmArray.update(mem, Word32.toInt(use_address), data),
               (reads, writes + 1))
            end;


      fun LoadWord (mem, address)
          = let
              val aligned_address 
                  = if address = AlignWAddress address
                      then address
                      else (print "Error LW: Memory using aligned address\n";
                            AlignWAddress address);
            in
              Load(mem, aligned_address)
            end;
            
      fun StoreWord (mem, address, data)
          = let
              val aligned_address
                  = if address = AlignWAddress address
                      then address
                      else (print "Error SW: Memory using aligned address\n";
                            AlignWAddress address);
            in
              Store(mem, aligned_address, data)
            end;

      fun LoadHWord (mem, address)
          = let
              val aligned_address 
                  = if address = AlignHWAddress address
                      then address
                      else (print "Error LH: Memory using aligned address\n";
                            AlignHWAddress address);
              val (nmem,l_word) = Load(mem, aligned_address);
            in
              (nmem,
               case aligned_address 
                 of 0wx00000000 : Word32.word
                   => Word32.~>>(Word32.<<(l_word, 0wx0010), 
                                 0wx0010)
                  | 0wx00000010 : Word32.word
                   => Word32.~>>(Word32.<<(l_word, 0wx0000),
                                 0wx0010)
                  | _ => (print "Error LH: Memory returning 0\n";
                          0wx00000000 : Word32.word))
            end;

      fun LoadHWordU (mem, address)
          = let
              val aligned_address 
                  = if address = AlignHWAddress address
                      then address
                      else (print "Error LHU: Memory using aligned address\n";
                            AlignHWAddress address);
              val (nmem, l_word) = Load(mem, aligned_address);
            in
              (nmem,
               case aligned_address 
                 of 0wx00000000 : Word32.word
                   => Word32.>>(Word32.<<(l_word, 0wx0010), 
                                0wx0010)
                  | 0wx00000010 : Word32.word
                   => Word32.>>(Word32.<<(l_word, 0wx0000),
                                0wx0010)
                  | _ => (print "Error LHU: Memory returning 0\n";
                          0wx00000000 : Word32.word))
            end;
            
      fun StoreHWord (mem, address, data)
          = let
              val aligned_address
                  = if address = AlignHWAddress address
                      then address
                      else (print "Error SH: Memory using aligned address\n";
                            AlignWAddress address);
              val (_, s_word) = Load(mem, aligned_address);
            in
              case aligned_address
                of 0wx00000000 : Word32.word
                  => Store(mem, aligned_address,
                           Word32.orb(Word32.andb(0wxFFFF0000 : Word32.word,
                                                  s_word),
                                      Word32.<<(Word32.andb(0wx0000FFFF : 
                                                            Word32.word,
                                                            data),
                                                0wx0000)))
                 | 0wx00000010 : Word32.word
                  => Store(mem, aligned_address,
                           Word32.orb(Word32.andb(0wx0000FFFF : Word32.word,
                                                  s_word),
                                      Word32.<<(Word32.andb(0wx0000FFFF : 
                                                            Word32.word,
                                                            data),
                                                0wx0010)))
                 | _ => (print "Error SH: Memory unchanged\n";
                         mem)
            end;

      fun LoadByte (mem, address)
          = let
              val aligned_address = address;
              val (nmem, l_word) = Load(mem, aligned_address);
            in
              (nmem,
               case aligned_address 
                 of 0wx00000000 : Word32.word
                   => Word32.~>>(Word32.<<(l_word, 
                                           0wx0018), 
                                 0wx0018)
                  | 0wx00000008 : Word32.word
                   => Word32.~>>(Word32.<<(l_word,
                                           0wx0010),
                                 0wx0018)
                  | 0wx00000010 : Word32.word
                   => Word32.~>>(Word32.<<(l_word,
                                           0wx0008),
                                 0wx0018)
                  | 0wx00000018 : Word32.word
                   => Word32.~>>(Word32.<<(l_word,
                                           0wx0000),
                                 0wx0018)
                  | _ => (print "Error LB: Memory returning 0\n";
                          0wx00000000 : Word32.word))
            end;

      fun LoadByteU (mem, address)
          = let
              val aligned_address = address;
              val (nmem, l_word) = Load(mem, aligned_address);
            in
              (nmem,
               case aligned_address 
                 of 0wx00000000 : Word32.word
                   => Word32.>>(Word32.<<(l_word,
                                          0wx0018),
                                0wx0018)
                  | 0wx00000008 : Word32.word
                   => Word32.>>(Word32.<<(l_word,
                                          0wx0010),
                                0wx0018)
                  | 0wx00000010 : Word32.word
                   => Word32.>>(Word32.<<(l_word,
                                          0wx0008),
                                0wx0018)
                  | 0wx00000018 : Word32.word
                   => Word32.>>(Word32.<<(l_word,
                                          0wx0000),
                                0wx0018)
                  | _ => (print "Error LBU: Memory returning 0\n";
                          0wx00000000 : Word32.word))
            end;
            
      fun StoreByte (mem, address, data)
          = let
              val aligned_address = address;
              val (_, s_word) = Load(mem, aligned_address);
            in
              case aligned_address
                of 0wx00000000 : Word32.word
                  => Store(mem, aligned_address,
                           Word32.orb(Word32.andb(0wxFFFFFF00 : Word32.word,
                                                  s_word),
                                      Word32.<<(Word32.andb(0wx000000FF : 
                                                            Word32.word,
                                                            data),
                                                0wx0000)))
                 | 0wx00000008 : Word32.word
                  => Store(mem, aligned_address,
                           Word32.orb(Word32.andb(0wxFFFF00FF : Word32.word,
                                                  s_word),
                                      Word32.<<(Word32.andb(0wx000000FF : 
                                                            Word32.word,
                                                            data),
                                                0wx0008)))
                 | 0wx00000010 : Word32.word
                  => Store(mem, aligned_address,
                           Word32.orb(Word32.andb(0wxFF00FFFF : Word32.word,
                                                  s_word),
                                      Word32.<<(Word32.andb(0wx000000FF : 
                                                            Word32.word,
                                                            data),
                                                0wx0010)))
                 | 0wx00000018 : Word32.word
                  => Store(mem, aligned_address,
                           Word32.orb(Word32.andb(0wx00FFFFFF : Word32.word,
                                                  s_word),
                                      Word32.<<(Word32.andb(0wx000000FF : 
                                                            Word32.word,
                                                            data),
                                                0wx0018)))
                 | _ => (print "Error SB: Memory unchanged\n";
                         mem)
            end;

      fun GetStatistics (mem, (reads, writes))
          = "Memory :\n" ^
            "Memory Reads : " ^ (Int.toString reads) ^ "\n" ^
            "Memory Writes : " ^ (Int.toString writes) ^ "\n";
            
    end;

(*****************************************************************************)

(*
 * CacheSpec.sig
 *
 * This defines the signature that outlines the specifications to
 * describe a cache.  The two datatypes are given to provide clear
 * means of differentiating between the write hit and write miss
 * options.  CacheName can be any string describing the cache.
 * CacheSize is an integer that represents the total number of words
 * in the cache.  BlockSize is an integer that represents the total
 * number of words in a block.  Associativity is an integer that
 * represents the associativity of the cache.  WriteHit and WriteMiss
 * represent the write hit and write miss options to be implemented by
 * this cache.
 *)

signature CACHESPEC
  = sig
  
      datatype WriteHitOption = Write_Through
                              | Write_Back;

      datatype WriteMissOption = Write_Allocate
                               | Write_No_Allocate;

      val CacheName : string;
      val CacheSize : int;
      val BlockSize : int;
      val Associativity : int;
      val WriteHit : WriteHitOption;
      val WriteMiss : WriteMissOption;

    end;

(*****************************************************************************)

(*
 * CachedMemory.sml
 *
 * This defines the CachedMemory functor, which provides the
 * functionality of a cached memory and which takes two structures,
 * corresponding to the cache specification and the the level of
 * memory which the cache will be caching.  The datatype memory
 * provides the encapsulation of the cache along with the memory
 * system that is being cached, InitMemory initializes the cache and
 * the memory system that is being cached, LoadWord takes memory and a
 * Word32.word corresponding to the address, and returns the
 * Word32.word at that address and the updated cache and memory,
 * StoreWord takes memory, a Word32.word corresponding to the address,
 * and a Word32.word and returns the cache and memory updated with the
 * stored at the appropriate address.  LoadHWord, LoadHWordU, 
 * LoadByte, and LoadByteU load halfwords, unsigned halfwords,
 * bytes, and unsigned bytes respectively from memory into the
 * lower portion of the returned Word32.word.  StoreHWord and 
 * StoreByte store halfwords and bytes taken from the lower portion
 * of the Word32.word into memory.
 * GetStatistics takes memory and returns the read and write 
 * statistics as a string.
 *
 * The underlying structure of cache is a two dimensional array of
 * cache lines, where a cache line consists of a valid bit, dirty bit,
 * a tag and a block of words, as a Word32.word array.
 * The size of the cache, the associativity, and the block size are
 * specified by the cache specification.
 *
 * Also, the functions AlignWAddress and AlignHWAddress aligns a memory 
 * address to a word and halfword address, respectively.  If LoadWord,
 * StoreWord, LoadHWord, LoadHWordU, or StoreHWord is asked to access an 
 * unaligned address, it writes an error message, and uses the address 
 * rounded down to the aligned address.
 *)

functor CachedMemory (structure CS : CACHESPEC;
                      structure MEM : MEMORY;) : MEMORY 
  = struct
  
      type cacheline 
           = bool * bool * Word32.word * Word32.word ImmArray.immarray;

      type cacheset
           = cacheline ImmArray.immarray;

      type cache 
           = cacheset ImmArray.immarray;
            
      type memory = (cache * (int * int * int * int)) * MEM.memory;
            
            
      (* Performs log[base2] on an integer. *)
      fun exp2 0 = 1
        | exp2 n = 2 * (exp2 (n-1))
      fun log2 x = let
                     fun log2_aux n = if exp2 n > x
                                        then (n-1)
                                        else log2_aux (n+1)
                   in 
                     log2_aux 0
                   end

      open CS;

      (*
       * The following values of index size and field bits are
       * calculated from the values in the cache specification
       * structure.
       *)
      val IndexSize = CacheSize div (BlockSize * Associativity);
      val BlockOffsetBits = log2 (BlockSize * 4);
      val IndexBits = log2 IndexSize;
      val TagBits = 32 - BlockOffsetBits - IndexBits;
            

      (*
       * RandEntry returns a random number between 
       * [0, Associativity - 1].  It is used to determine
       * replacement of data in the cache.
       *)
      val RandEntry = let
                        val modulus = Word.fromInt(Associativity - 1)
                      in 
                        fn () => Word.toInt(Word.mod(rand (),
                                                     modulus))
                      end

      (* 
       * The InitCache function initializes the cache to
       * not-valid, not-dirty, 0wx00000000 tag, blocks initialized
       * to 0wx00000000.  
       *)
      fun InitCache ()
          = let
              val cacheline = (false, false, 0wx00000000 : Word32.word,
                               ImmArray.immarray (BlockSize,
                                                  0wx00000000 : Word32.word));
              val cacheset = ImmArray.immarray (Associativity, cacheline);
            in 
              (ImmArray.immarray (IndexSize, cacheset),
               (0, 0, 0, 0))
            end;
         

      (* 
       * The InitMemory function initializes the cache
       * and the memory being cached.
       *)
      fun InitMemory () = (InitCache (), MEM.InitMemory ()) : memory;
            
        
      (* 
       * GetTag returns the Word32.word corresponding to the tag field of
       * address
       *)
      fun GetTag address 
          = Word32.>> (address, 
                       Word.fromInt (IndexBits + BlockOffsetBits));
            

      (* 
       * GetIndex returns the Word32.word corresponding to the index
       * field of address.
       *)
      fun GetIndex address
          = let
              val mask
                  = Word32.notb 
                    (Word32.<< 
                     (Word32.>> (0wxFFFFFFFF : Word32.word,
                                 Word.fromInt (IndexBits + BlockOffsetBits)), 
                      Word.fromInt (IndexBits + BlockOffsetBits)));
            in
              Word32.>> (Word32.andb (address, mask),
                         Word.fromInt (BlockOffsetBits))
            end;
            

      (* 
       * GetBlockOffset returns the Word32.word corresponding to the
       * block offset field of address.
       *)
      fun GetBlockOffset address
          = let
              val mask
                  = Word32.notb 
                    (Word32.<< 
                     (Word32.>> (0wxFFFFFFFF : Word32.word,
                                 Word.fromInt BlockOffsetBits), 
                      Word.fromInt BlockOffsetBits));
            in
              Word32.andb (address, mask)
            end;
            
           
      (* 
       * The InCache* family of functions returns a boolean value
       * that determines if the word specified by address is in the
       * cache at the current time (and that the data is valid).
       *)
      fun InCache_aux_entry ((valid, dirty, tag, block), address)
          = tag = (GetTag address) andalso valid;

      fun InCache_aux_set (set, address)
          = ImmArray.foldr (fn (entry, result) => 
                               (InCache_aux_entry (entry, address)) orelse 
                               result) 
                           false 
                           set;

      fun InCache (cac, address)
          = InCache_aux_set (ImmArray.sub (cac, 
                                           Word32.toInt (GetIndex address)),
                             address);

      (* 
       * The ReadCache* family of functions returns the Word32.word 
       * stored at address in the cache.
       *)
      fun ReadCache_aux_entry ((valid, dirty, tag, block), address) 
          = ImmArray.sub (block, 
                          Word32.toInt (Word32.>> (GetBlockOffset address, 
                                                   0wx0002)));
        
      fun ReadCache_aux_set (set, address) 
          = ImmArray.foldr (fn (entry, result) =>
                               if InCache_aux_entry (entry, address)
                                 then ReadCache_aux_entry (entry, address)
                                 else result)
                           (0wx00000000 : Word32.word)
                           set;
        
      fun ReadCache (cac, address)
          = ReadCache_aux_set (ImmArray.sub (cac, 
                                             Word32.toInt(GetIndex address)),
                               address);


      (* 
       * The WriteCache* family of functions returns the updated
       * cache with data stored at address.
       *)
      fun WriteCache_aux_entry ((valid, dirty, tag, block), address, data)
          = let
              val ndirty = case WriteHit
                             of Write_Through => false
                              | Write_Back => true; 
            in
              (true, ndirty, tag, 
               ImmArray.update (block, 
                                Word32.toInt (Word32.>> 
                                              (GetBlockOffset address, 
                                               0wx0002)), 
                                data))
            end;
            
      fun WriteCache_aux_set (set, address, data) 
          = ImmArray.map (fn entry =>
                             if InCache_aux_entry (entry, address)
                               then WriteCache_aux_entry (entry, address, 
                                                          data)
                               else entry)
                         set;

      fun WriteCache (cac, address, data)
          = let
              val index = Word32.toInt (GetIndex address);
              val nset = WriteCache_aux_set (ImmArray.sub (cac, index),
                                             address, data);
            in
              ImmArray.update (cac, index, nset)
            end;


      (* 
       * The LoadBlock function returns the updated
       * memory and the block containing address loaded from memory.
       *)
      fun LoadBlock (mem, address)
          = ImmArray.foldr (fn (offset, (block, mem)) =>
                               let
                                 val laddress
                                     = Word32.+ (Word32.<<
                                                 (Word32.>> 
                                                  (address,
                                                   Word.fromInt 
                                                   BlockOffsetBits),
                                                  Word.fromInt 
                                                  BlockOffsetBits), 
                                                 Word32.<< (Word32.fromInt 
                                                            offset, 
                                                            0wx0002));
                                 val (nmem, nword) = MEM.LoadWord (mem, 
                                                                   laddress);
                               in
                                 (ImmArray.update (block, offset, nword), nmem)
                               end)
                           (ImmArray.immarray (BlockSize, 
                                               0wx00000000 : Word32.word), mem)
                           (ImmArray.tabulate (BlockSize, fn i => i));

 
      (* 
       * The StoreBlock functionsreturns the updated
       * memory with block stored into the block containing address.
       *)
      fun StoreBlock (block, mem, address)
          = ImmArray.foldr (fn (offset, mem) =>
                               let
                                 val saddress
                                     = Word32.+ (Word32.<<
                                                 (Word32.>> 
                                                  (address,
                                                   Word.fromInt 
                                                   BlockOffsetBits),
                                                  Word.fromInt 
                                                  BlockOffsetBits), 
                                                 Word32.<< (Word32.fromInt 
                                                            offset, 
                                                            0wx0002));
                               in
                                 MEM.StoreWord (mem, saddress,
                                                ImmArray.sub (block, offset))
                               end)
                           mem
                           (ImmArray.tabulate (BlockSize, fn i => i));


      (* 
       * The LoadCache* family of functions returns the updated
       * cache and memory, with the block containing address loaded
       * into the cache at the appropriate cache line, and dirty
       * data written back to memory as needed.
       *)
      fun LoadCache_aux_entry ((valid, dirty, tag, block), mem, address)
          = let
              val saddress 
                  = Word32.orb (Word32.<< (tag,
                                           Word.fromInt TagBits),
                                Word32.<< (GetIndex address,
                                           Word.fromInt IndexBits));
              val nmem = if valid andalso dirty
                           then StoreBlock (block, mem, saddress)
                           else mem;
              val (nblock, nnmem) = LoadBlock (nmem, address);
            in
              ((true, false, GetTag address, nblock), nnmem)
            end;

      fun LoadCache_aux_set (set, mem, address)
          = let
              val entry = RandEntry ();
              val (nentry, nmem) = LoadCache_aux_entry (ImmArray.sub (set, 
                                                                      entry),
                                                        mem, address);
            in
              (ImmArray.update (set, entry, nentry), nmem)
            end;

      fun LoadCache (cac, mem, address) 
          = let
              val index = Word32.toInt (GetIndex address);
              val (nset, nmem) 
                  = LoadCache_aux_set (ImmArray.sub (cac, index), 
                                       mem, address);
            in
              (ImmArray.update (cac, index, nset), nmem)
            end;


      (* 
       * The remainder of the function defined here satisfy the MEMORY
       * signature.  This allows a CachedMemory to act exactly like
       * a normal Memory, and thus caches can be nested to an arbitrary
       * depth.
       *)

      fun AlignWAddress address 
          = Word32.<< (Word32.>> (address, 0wx0002), 0wx0002);
            
      fun AlignHWAddress address
          = Word32.<< (Word32.>> (address, 0wx0001), 0wx0001);
            
      (* Load and Store provide errorless access to memory.
       * They provide a common interface to memory, while
       * the LoadX and StoreX specifically access words,
       * halfwords and bytes, requiring address to be aligned.
       * In Load and Store, two intermediate values are
       * generated.  The value aligned_address is the aligned
       * version of the given address, and is used to compare with
       * the original address to determine if it was aligned.  The 
       * value use_address is equivalent to aligned_address divided 
       * by four, and it corresponds to the index of the memory 
       * array where the corresponding aligned address can be found.
       *)

      fun Load (((cac, (rh, rm, wh, wm)), mem), address)
          = let
              val aligned_address = AlignWAddress address;
            in
              if InCache (cac, aligned_address)
                then (((cac, (rh + 1, rm, wh, wm)), mem), 
                      ReadCache (cac, aligned_address))
                else let
                       val (ncac, nmem) 
                           = LoadCache (cac, mem, aligned_address);
                     in
                       (((ncac, (rh, rm + 1, wh, wm)), nmem),
                        ReadCache (ncac, aligned_address))
                     end
            end;


      fun Store (((cac, (rh, rm, wh, wm)), mem), address, data)
          = let
              val aligned_address = AlignWAddress address;
            in
              if InCache (cac, aligned_address)
                then let
                       val ncac = WriteCache (cac, aligned_address, data);
                     in
                       case WriteHit 
                         of Write_Through => 
                              ((ncac, (rh, rm, wh + 1, wm)),
                               MEM.StoreWord (mem, aligned_address, data))
                          | Write_Back => 
                              ((ncac, (rh, rm, wh + 1, wm)), mem)
                     end
                else case WriteMiss
                       of Write_Allocate =>
                            let
                              val (ncac, nmem) 
                                = LoadCache (cac, mem, aligned_address);
                              val nncac
                                = WriteCache (ncac, aligned_address, data);
                            in
                              case WriteHit
                                of Write_Through => 
                                     ((nncac, (rh, rm, wh, wm + 1)), 
                                      MEM.StoreWord (nmem, aligned_address, 
                                                     data))
                                 | Write_Back =>
                                     ((nncac, (rh, rm, wh, wm + 1)),
                                      nmem)
                            end
                        | Write_No_Allocate =>
                            ((cac, (rh, rm, wh, wm + 1)),
                             MEM.StoreWord (mem, aligned_address, data))
            end;

      fun LoadWord (mem, address)
          = let
              val aligned_address 
                  = if address = AlignWAddress address
                      then address
                      else (print "Error LW: Memory using aligned address\n";
                            AlignWAddress address);
            in
              Load(mem, aligned_address)
            end;
            
      fun StoreWord (mem, address, data)
          = let
              val aligned_address
                  = if address = AlignWAddress address
                      then address
                      else (print "Error SW: Memory using aligned address\n";
                            AlignWAddress address);
            in
              Store(mem, aligned_address, data)
            end;

      fun LoadHWord (mem, address)
          = let
              val aligned_address 
                  = if address = AlignHWAddress address
                      then address
                      else (print "Error LH: Memory using aligned address\n";
                            AlignHWAddress address);
              val (nmem,l_word) = Load(mem, aligned_address);
            in
              (nmem,
               case aligned_address 
                 of 0wx00000000 : Word32.word
                   => Word32.~>>(Word32.<<(l_word, 0wx0010), 
                                 0wx0010)
                  | 0wx00000010 : Word32.word
                   => Word32.~>>(Word32.<<(l_word, 0wx0000),
                                 0wx0010)
                  | _ => (print "Error LH: Memory returning 0\n";
                          0wx00000000 : Word32.word))
            end;

      fun LoadHWordU (mem, address)
          = let
              val aligned_address 
                  = if address = AlignHWAddress address
                      then address
                      else (print "Error LHU: Memory using aligned address\n";
                            AlignHWAddress address);
              val (nmem, l_word) = Load(mem, aligned_address);
            in
              (nmem,
               case aligned_address 
                 of 0wx00000000 : Word32.word
                   => Word32.>>(Word32.<<(l_word, 0wx0010), 
                                0wx0010)
                  | 0wx00000010 : Word32.word
                   => Word32.>>(Word32.<<(l_word, 0wx0000),
                                0wx0010)
                  | _ => (print "Error LHU: Memory returning 0\n";
                          0wx00000000 : Word32.word))
            end;
            
      fun StoreHWord (mem, address, data)
          = let
              val aligned_address
                  = if address = AlignHWAddress address
                      then address
                      else (print "Error SH: Memory using aligned address\n";
                            AlignWAddress address);
              val (_, s_word) = Load(mem, aligned_address);
            in
              case aligned_address
                of 0wx00000000 : Word32.word
                  => Store(mem, aligned_address,
                           Word32.orb(Word32.andb(0wxFFFF0000 : Word32.word,
                                                  s_word),
                                      Word32.<<(Word32.andb(0wx0000FFFF : 
                                                            Word32.word,
                                                            data),
                                                0wx0000)))
                 | 0wx00000010 : Word32.word
                  => Store(mem, aligned_address,
                           Word32.orb(Word32.andb(0wx0000FFFF : Word32.word,
                                                  s_word),
                                      Word32.<<(Word32.andb(0wx0000FFFF : 
                                                            Word32.word,
                                                            data),
                                                0wx0010)))
                 | _ => (print "Error SH: Memory unchanged\n";
                         mem)
            end;

      fun LoadByte (mem, address)
          = let
              val aligned_address = address;
              val (nmem, l_word) = Load(mem, aligned_address);
            in
              (nmem,
               case aligned_address 
                 of 0wx00000000 : Word32.word
                   => Word32.~>>(Word32.<<(l_word, 
                                           0wx0018), 
                                 0wx0018)
                  | 0wx00000008 : Word32.word
                   => Word32.~>>(Word32.<<(l_word,
                                           0wx0010),
                                 0wx0018)
                  | 0wx00000010 : Word32.word
                   => Word32.~>>(Word32.<<(l_word,
                                           0wx0008),
                                 0wx0018)
                  | 0wx00000018 : Word32.word
                   => Word32.~>>(Word32.<<(l_word,
                                           0wx0000),
                                 0wx0018)
                  | _ => (print "Error LB: Memory returning 0\n";
                          0wx00000000 : Word32.word))
            end;

      fun LoadByteU (mem, address)
          = let
              val aligned_address = address;
              val (nmem, l_word) = Load(mem, aligned_address);
            in
              (nmem,
               case aligned_address 
                 of 0wx00000000 : Word32.word
                   => Word32.>>(Word32.<<(l_word,
                                          0wx0018),
                                0wx0018)
                  | 0wx00000008 : Word32.word
                   => Word32.>>(Word32.<<(l_word,
                                          0wx0010),
                                0wx0018)
                  | 0wx00000010 : Word32.word
                   => Word32.>>(Word32.<<(l_word,
                                          0wx0008),
                                0wx0018)
                  | 0wx00000018 : Word32.word
                   => Word32.>>(Word32.<<(l_word,
                                          0wx0000),
                                0wx0018)
                  | _ => (print "Error LBU: Memory returning 0\n";
                          0wx00000000 : Word32.word))
            end;
            
      fun StoreByte (mem, address, data)
          = let
              val aligned_address = address;
              val (_, s_word) = Load(mem, aligned_address);
            in
              case aligned_address
                of 0wx00000000 : Word32.word
                  => Store(mem, aligned_address,
                           Word32.orb(Word32.andb(0wxFFFFFF00 : Word32.word,
                                                  s_word),
                                      Word32.<<(Word32.andb(0wx000000FF : 
                                                            Word32.word,
                                                            data),
                                                0wx0000)))
                 | 0wx00000008 : Word32.word
                  => Store(mem, aligned_address,
                           Word32.orb(Word32.andb(0wxFFFF00FF : Word32.word,
                                                  s_word),
                                      Word32.<<(Word32.andb(0wx000000FF : 
                                                            Word32.word,
                                                            data),
                                                0wx0008)))
                 | 0wx00000010 : Word32.word
                  => Store(mem, aligned_address,
                           Word32.orb(Word32.andb(0wxFF00FFFF : Word32.word,
                                                  s_word),
                                      Word32.<<(Word32.andb(0wx000000FF : 
                                                            Word32.word,
                                                            data),
                                                0wx0010)))
                 | 0wx00000018 : Word32.word
                  => Store(mem, aligned_address,
                           Word32.orb(Word32.andb(0wx00FFFFFF : Word32.word,
                                                  s_word),
                                      Word32.<<(Word32.andb(0wx000000FF : 
                                                            Word32.word,
                                                            data),
                                                0wx0018)))
                 | _ => (print "Error SB: Memory unchanged\n";
                         mem)
            end;

      fun GetStatistics ((cac, (rh, rm, wh, wm)), mem)
          = let

              val th = rh + wh;
                
              val tm = rm + wm;

              val who = case WriteHit
                          of Write_Through => "Write Through"
                           | Write_Back => "Write Back";

              val wmo = case WriteMiss
                          of Write_Allocate => "Write Allocate"
                           | Write_No_Allocate => "Write No Allocate";

            in
              CacheName ^ " :\n" ^
              "CacheSize : " ^ (Int.toString CacheSize) ^ "\n" ^
              "BlockSize : " ^ (Int.toString BlockSize) ^ "\n" ^
              "Associativity : " ^ (Int.toString Associativity) ^ "\n" ^
              "Write Hit : " ^ who ^ "\n" ^
              "Write Miss : " ^ wmo ^ "\n" ^
              "Read hits : " ^ (Int.toString rh) ^ "\n" ^
              "Read misses : " ^ (Int.toString rm) ^ "\n" ^
              "Write hits : " ^ (Int.toString wh) ^ "\n" ^
              "Write misses : " ^ (Int.toString wm) ^ "\n" ^
              "Total hits : " ^ (Int.toString th) ^ "\n" ^
              "Total misses : " ^ (Int.toString tm) ^ "\n" ^
              (MEM.GetStatistics mem)
            end;

    end;

(*****************************************************************************)

(*
 * DLXSimulator.sig
 *
 * This defines the exported function provided by the DLXSimulator.
 * The function run_file takes a string corresponding to the name of the
 * file to be run, and executes it.  The function run_prog takes a
 * list of instructions and executes them.
 *)

signature DLXSIMULATOR
  = sig
  
      val run_file : string -> unit;
      val run_prog : string list -> unit; 
        
    end;

(*****************************************************************************)

(*
 * DLXSimulator.sml
 *
 * This defines the DLXSimulatorFun functor, which takes three
 * structures, corresponding to the register file, the ALU, and memory,
 * and provides the functionality of a DLX processor, able to execute 
 * DLX programs.  The function run_file takes a string corresponding to the
 * name of the file to be executed, and executes it.  The function
 * run_prog takes a list of instructions and executes them.
 *)

functor DLXSimulatorFun (structure RF : REGISTERFILE; 
                         structure ALU : ALU;
                         structure MEM : MEMORY; ) : DLXSIMULATOR
  = struct
                
      (*
       * The datatype Opcode provides a means of differentiating *
       * among the main opcodes.
       *)
      datatype Opcode = 
        (* for R-type opcodes *)
        SPECIAL |
        (* I-type opcodes *)
        BEQZ | BNEZ |
        ADDI | ADDUI | SUBI | SUBUI |
        ANDI | ORI | XORI |
        LHI |
        SLLI | SRLI | SRAI | 
        SEQI | SNEI | SLTI | SGTI | SLEI | SGEI |
        LB | LBU | SB |
        LH | LHU | SH |
        LW | SW |
        (* J-type opcodes *)
        J | JAL | TRAP | JR | JALR |
        (* Unrecognized opcode *)
        NON_OP; 
            
      (*
       * The datatype RRFuncCode provides a means of 
       * differentiating among
       * the register-register function codes.
       *) 
      datatype RRFunctCode = NOP | SLL | SRL | SRA |
                             ADD | ADDU | SUB | SUBU |
                             AND | OR | XOR |
                             SEQ | SNE | SLT | SGT | SLE | SGE |
                             NON_FUNCT;
            
      (*
       * The datatype Instruction provides a means of
       * differentiating among the three different types of
       * instructions, I-type, R-type, and J-type.
       * An I-type is interpreted as (opcode, rs1, rd, immediate).
       * An R-type is interpreted as (opcode, rs1, rs2, rd, shamt, funct).
       * An J-type is interpreted as (opcode, offset).
       * An ILLEGAL causes the simulator to end.
       *)
      datatype Instruction 
        = ITYPE of Opcode * int * int * Word32.word
        | RTYPE of Opcode * int * int * int * int * RRFunctCode 
        | JTYPE of Opcode * Word32.word 
        | ILLEGAL;

      (*
       * The value HALT is set to the DLX instruction TRAP #0,
       * and is used to check for the halt of the program.
       *)
      val HALT = JTYPE (TRAP, 0wx00000000);
  
      (*
       * The function DecodeIType decodes a Word32.word into an
       * I-type instruction.
       *)
      fun DecodeIType instr
          = let
              val opc = Word32.andb (Word32.>> (instr,
                                                0wx001A),
                                     0wx0000003F : Word32.word);
              
              val opcode = case opc
                             of 0wx00000004 : Word32.word => BEQZ
                              | 0wx00000005 : Word32.word => BNEZ
                              | 0wx00000008 : Word32.word => ADDI
                              | 0wx00000009 : Word32.word => ADDUI
                              | 0wx0000000A : Word32.word => SUBI
                              | 0wx0000000B : Word32.word => SUBUI
                              | 0wx0000000C : Word32.word => ANDI
                              | 0wx0000000D : Word32.word => ORI
                              | 0wx0000000E : Word32.word => XORI
                              | 0wx0000000F : Word32.word => LHI
                              | 0wx00000014 : Word32.word => SLLI
                              | 0wx00000016 : Word32.word => SRLI
                              | 0wx00000017 : Word32.word => SRAI
                              | 0wx00000018 : Word32.word => SEQI
                              | 0wx00000019 : Word32.word => SNEI
                              | 0wx0000001A : Word32.word => SLTI
                              | 0wx0000001B : Word32.word => SGTI
                              | 0wx0000001C : Word32.word => SLEI
                              | 0wx0000001D : Word32.word => SGEI
                              | 0wx00000020 : Word32.word => LB
                              | 0wx00000024 : Word32.word => LBU
                              | 0wx00000028 : Word32.word => SB
                              | 0wx00000021 : Word32.word => LH
                              | 0wx00000025 : Word32.word => LHU
                              | 0wx00000029 : Word32.word => SH
                              | 0wx00000023 : Word32.word => LW
                              | 0wx0000002B : Word32.word => SW
                              | _ => (print "Error : Non I-Type opcode\n";
                                      NON_OP);
                             
              val rs1 = Word32.toInt(Word32.andb (Word32.>> (instr, 0wx0015),
                                                  0wx0000001F : Word32.word));
                    
              val rd = Word32.toInt(Word32.andb (Word32.>> (instr, 0wx0010),
                                                 0wx0000001F : Word32.word));
                    
              val immediate = Word32.~>> (Word32.<< (instr, 0wx0010),
                                          0wx0010);

            in
              if opcode = NON_OP
                then ILLEGAL
                else ITYPE (opcode, rs1, rd, immediate)
            end;
            
      (*
       * The function DecodeRType decodes a Word32.word into an
       * R-type instruction.
       *)
      fun DecodeRType instr
          = let
                
              val rs1 = Word32.toInt (Word32.andb (Word32.>> (instr, 0wx0015),
                                                   0wx0000001F : Word32.word));
                
              val rs2 = Word32.toInt (Word32.andb (Word32.>> (instr, 0wx0010),
                                                   0wx0000001F : Word32.word));
                
              val rd = Word32.toInt (Word32.andb (Word32.>> (instr, 0wx000B),
                                                  0wx0000001F : Word32.word));
                
              val shamt 
                  = Word32.toInt (Word32.andb (Word32.>> (instr, 0wx0006),
                                               0wx0000001F : Word32.word));
                    
              val funct = Word32.andb (instr, 0wx0000003F : Word32.word);
                    
              val functcode = case funct
                                of 0wx00000000 : Word32.word => NOP
                                 | 0wx00000004 : Word32.word => SLL
                                 | 0wx00000006 : Word32.word => SRL
                                 | 0wx00000007 : Word32.word => SRA
                                 | 0wx00000020 : Word32.word => ADD
                                 | 0wx00000021 : Word32.word => ADDU
                                 | 0wx00000022 : Word32.word => SUB
                                 | 0wx00000023 : Word32.word => SUBU
                                 | 0wx00000024 : Word32.word => AND
                                 | 0wx00000025 : Word32.word => OR
                                 | 0wx00000026 : Word32.word => XOR
                                 | 0wx00000028 : Word32.word => SEQ
                                 | 0wx00000029 : Word32.word => SNE
                                 | 0wx0000002A : Word32.word => SLT
                                 | 0wx0000002B : Word32.word => SGT
                                 | 0wx0000002C : Word32.word => SLE
                                 | 0wx0000002D : Word32.word => SGE
                                 | _ => (print "Error : Non R-type funct\n";
                                         NON_FUNCT);
                                     
            in
              if functcode = NON_FUNCT
                then ILLEGAL
                else RTYPE (SPECIAL, rs1, rs2, rd, shamt, functcode)
            end;
            
      (*
       * The function DecodeJType decodes a Word32.word into an
       * J-type instruction.
       *)
      fun DecodeJType instr 
          = let

              val opc = Word32.andb (Word32.>> (instr, 0wx1A),
                                     0wx0000003F : Word32.word);
                    
              val opcode = case opc
                             of 0wx00000002 : Word32.word => J
                              | 0wx00000003 : Word32.word => JAL
                              | 0wx00000011 : Word32.word => TRAP
                              | 0wx00000012 : Word32.word => JR
                              | 0wx00000013 : Word32.word => JALR
                              | _ => (print "Error : Non J-type opcode\n";
                                      NON_OP);
                                  
              val offset = Word32.~>> (Word32.<< (instr, 0wx0006),
                                       0wx0006);

            in
                if opcode = NON_OP
                    then ILLEGAL
                    else JTYPE (opcode, offset)
            end;
            
      (*
       * The function DecodeInstr decodes a Word32.word into an
       * instruction.  It first checks the opcode, and then calls 
       * one of DecodeIType, DecodeJType, and DecodeRType to
       * complete the decoding process.
       *)
      fun DecodeInstr instr
          = let

              val opcode = Word32.andb (Word32.>> (instr, 0wx1A),
                                        0wx0000003F : Word32.word);
                
            in
              case opcode
                of 0wx00000000 : Word32.word => DecodeRType instr
                 | 0wx00000002 : Word32.word => DecodeJType instr
                 | 0wx00000003 : Word32.word => DecodeJType instr
                 | 0wx00000004 : Word32.word => DecodeIType instr
                 | 0wx00000005 : Word32.word => DecodeIType instr
                 | 0wx00000008 : Word32.word => DecodeIType instr
                 | 0wx00000009 : Word32.word => DecodeIType instr
                 | 0wx0000000A : Word32.word => DecodeIType instr
                 | 0wx0000000B : Word32.word => DecodeIType instr
                 | 0wx0000000C : Word32.word => DecodeIType instr
                 | 0wx0000000D : Word32.word => DecodeIType instr
                 | 0wx0000000E : Word32.word => DecodeIType instr
                 | 0wx0000000F : Word32.word => DecodeIType instr
                 | 0wx00000011 : Word32.word => DecodeJType instr
                 | 0wx00000012 : Word32.word => DecodeJType instr
                 | 0wx00000013 : Word32.word => DecodeJType instr
                 | 0wx00000016 : Word32.word => DecodeIType instr
                 | 0wx00000017 : Word32.word => DecodeIType instr
                 | 0wx00000018 : Word32.word => DecodeIType instr
                 | 0wx00000019 : Word32.word => DecodeIType instr
                 | 0wx0000001A : Word32.word => DecodeIType instr
                 | 0wx0000001B : Word32.word => DecodeIType instr
                 | 0wx0000001C : Word32.word => DecodeIType instr
                 | 0wx0000001D : Word32.word => DecodeIType instr
                 | 0wx00000020 : Word32.word => DecodeIType instr
                 | 0wx00000024 : Word32.word => DecodeIType instr
                 | 0wx00000028 : Word32.word => DecodeIType instr
                 | 0wx00000021 : Word32.word => DecodeIType instr
                 | 0wx00000025 : Word32.word => DecodeIType instr
                 | 0wx00000029 : Word32.word => DecodeIType instr
                 | 0wx00000023 : Word32.word => DecodeIType instr
                 | 0wx0000002B : Word32.word => DecodeIType instr
                 | _ => (print "Error : Unrecognized opcode\n";
                         ILLEGAL)
            end;
            

      (*
       * The function PerformIType performs one of the I-Type
       * instructions.  A number of the instructions make use of the
       * ALU, and as such, call ALU.PerformAL.
       *)
      fun PerformIType ((BEQZ, rs1, rd, immediate), (PC, rf, mem)) 
        = if (RF.LoadRegister(rf, rs1) = (0wx00000000 : Word32.word))
            then (Word32.fromInt (Int.+ (Word32.toIntX PC,
                                                Word32.toIntX
                                                (Word32.<< (immediate, 
                                                            0wx0002)))),
                  rf, mem)
            else (PC, rf, mem)
              
        | PerformIType ((BNEZ, rs1, rd, immediate), (PC, rf, mem)) 
          = if not (RF.LoadRegister(rf, rs1) = (0wx00000000 : Word32.word))
              then (Word32.fromInt (Int.+ (Word32.toIntX PC,
                                                  Word32.toIntX
                                                  (Word32.<< (immediate,
                                                              0wx0002)))),
                    rf, mem)
              else (PC, rf, mem)
            
        | PerformIType ((ADDI, rs1, rd, immediate), (PC, rf, mem))
          = (PC, 
             RF.StoreRegister(rf, rd, 
                              ALU.PerformAL(ALU.ADD,
                                            RF.LoadRegister(rf, rs1),
                                            immediate)),
             mem)

        | PerformIType ((ADDUI, rs1, rd, immediate), (PC, rf, mem))
          = (PC,
             RF.StoreRegister(rf, rd, 
                              ALU.PerformAL(ALU.ADDU,
                                            RF.LoadRegister(rf, rs1),
                                            immediate)),
             mem)
            
        | PerformIType ((SUBI, rs1, rd, immediate), (PC, rf, mem))
          = (PC,
             RF.StoreRegister(rf, rd, 
                              ALU.PerformAL(ALU.SUB,
                                            RF.LoadRegister(rf, rs1),
                                            immediate)),
             mem)

        | PerformIType ((SUBUI, rs1, rd, immediate), (PC, rf, mem)) 
          = (PC,
             RF.StoreRegister(rf, rd, 
                              ALU.PerformAL(ALU.SUBU,
                                            RF.LoadRegister(rf, rs1),
                                            immediate)),
             mem)
            
        | PerformIType ((ANDI, rs1, rd, immediate), (PC, rf, mem)) 
          = (PC,
             RF.StoreRegister(rf, rd, 
                              ALU.PerformAL(ALU.AND,
                                            RF.LoadRegister(rf, rs1),
                                            immediate)),
             mem)
            
        | PerformIType ((ORI, rs1, rd, immediate), (PC, rf, mem)) 
          = (PC,
             RF.StoreRegister(rf, rd, 
                              ALU.PerformAL(ALU.OR,
                                            RF.LoadRegister(rf, rs1),
                                            immediate)),
             mem)
          
        | PerformIType ((XORI, rs1, rd, immediate), (PC, rf, mem)) 
          = (PC,
             RF.StoreRegister(rf, rd, 
                              ALU.PerformAL(ALU.XOR,
                                            RF.LoadRegister(rf, rs1),
                                            immediate)),
             mem)
                    
        | PerformIType ((LHI, rs1, rd, immediate), (PC, rf, mem)) 
          = (PC, RF.StoreRegister(rf, rd, Word32.<< (immediate, 0wx0010)), mem)

        | PerformIType ((SLLI, rs1, rd, immediate), (PC, rf, mem))
          = (PC, RF.StoreRegister(rf, rd, 
                                  Word32.<< (RF.LoadRegister(rf, rs1),
                                             Word.fromLarge (Word32.toLarge immediate))),
             mem)

        | PerformIType ((SRLI, rs1, rd, immediate), (PC, rf, mem))
          = (PC, RF.StoreRegister(rf, rd, 
                                  Word32.>> (RF.LoadRegister(rf, rs1),
                                             Word.fromLarge (Word32.toLarge immediate))),
             mem)

        | PerformIType ((SRAI, rs1, rd, immediate), (PC, rf, mem))
          = (PC, RF.StoreRegister(rf, rd, 
                                  Word32.~>> (RF.LoadRegister(rf, rs1),
                                              Word.fromLarge (Word32.toLarge immediate))),
             mem)

        | PerformIType ((SEQI, rs1, rd, immediate), (PC, rf, mem))
          = (PC,
             RF.StoreRegister(rf, rd,
                              ALU.PerformAL(ALU.SEQ,
                                            RF.LoadRegister(rf, rs1),
                                            immediate)),
             mem)

        | PerformIType ((SNEI, rs1, rd, immediate), (PC, rf, mem))
          = (PC,
             RF.StoreRegister(rf, rd,
                              ALU.PerformAL(ALU.SNE,
                                            RF.LoadRegister(rf, rs1),
                                            immediate)),
             mem)

        | PerformIType ((SLTI, rs1, rd, immediate), (PC, rf, mem))
          = (PC,
             RF.StoreRegister(rf, rd,
                              ALU.PerformAL(ALU.SLT,
                                            RF.LoadRegister(rf, rs1),
                                            immediate)),
             mem)

        | PerformIType ((SGTI, rs1, rd, immediate), (PC, rf, mem))
          = (PC,
             RF.StoreRegister(rf, rd,
                              ALU.PerformAL(ALU.SGT,
                                            RF.LoadRegister(rf, rs1),
                                            immediate)),
             mem)

        | PerformIType ((SLEI, rs1, rd, immediate), (PC, rf, mem))
          = (PC,
             RF.StoreRegister(rf, rd,
                              ALU.PerformAL(ALU.SLE,
                                            RF.LoadRegister(rf, rs1),
                                            immediate)),
             mem)

        | PerformIType ((SGEI, rs1, rd, immediate), (PC, rf, mem))
          = (PC,
             RF.StoreRegister(rf, rd,
                              ALU.PerformAL(ALU.SGE,
                                            RF.LoadRegister(rf, rs1),
                                            immediate)),
             mem)
          
        | PerformIType ((LB, rs1, rd, immediate), (PC, rf, mem))
          = let
              val (nmem, l_byte)
                  = MEM.LoadByte(mem, Word32.+ (RF.LoadRegister(rf, rs1),
                                                immediate));
            in
              (PC,
               RF.StoreRegister(rf, rd, l_byte),
               nmem)
            end

        | PerformIType ((LBU, rs1, rd, immediate), (PC, rf, mem))
          = let
              val (nmem, l_byte)
                  = MEM.LoadByteU(mem, Word32.+ (RF.LoadRegister(rf, rs1),
                                                 immediate));
            in
              (PC,
               RF.StoreRegister(rf, rd, l_byte),
               nmem)
            end
          
        | PerformIType ((SB, rs1, rd, immediate), (PC, rf, mem))
          = (PC,
             rf,
             MEM.StoreByte(mem, 
                           Word32.+ (RF.LoadRegister(rf, rs1), immediate),
                           Word32.andb(0wx000000FF, RF.LoadRegister(rf, rd))))
            
        | PerformIType ((LH, rs1, rd, immediate), (PC, rf, mem))
          = let
              val (nmem, l_hword)
                  = MEM.LoadHWord(mem, Word32.+ (RF.LoadRegister(rf, rs1),
                                                 immediate));
            in
              (PC,
               RF.StoreRegister(rf, rd, l_hword),
               nmem)
            end

        | PerformIType ((LHU, rs1, rd, immediate), (PC, rf, mem))
          = let
              val (nmem, l_hword)
                  = MEM.LoadHWordU(mem, Word32.+ (RF.LoadRegister(rf, rs1),
                                                  immediate));
            in
              (PC,
               RF.StoreRegister(rf, rd, l_hword),
               nmem)
            end
          
        | PerformIType ((SH, rs1, rd, immediate), (PC, rf, mem))
          = (PC,
             rf,
             MEM.StoreByte(mem, 
                           Word32.+ (RF.LoadRegister(rf, rs1), immediate),
                           Word32.andb(0wx0000FFFF, RF.LoadRegister(rf, rd))))
            

        | PerformIType ((LW, rs1, rd, immediate), (PC, rf, mem)) 
          = let
              val (nmem, l_word) 
                  = MEM.LoadWord(mem, Word32.+ (RF.LoadRegister(rf, rs1),
                                                immediate));
            in
              (PC,
               RF.StoreRegister(rf, rd, l_word),
               nmem)
            end

        | PerformIType ((SW, rs1, rd, immediate), (PC, rf, mem)) 
          = (PC,
             rf,
             MEM.StoreWord(mem,
                           Word32.+ (RF.LoadRegister(rf, rs1), immediate),
                           RF.LoadRegister(rf, rd)))
            
        | PerformIType ((_, rs1, rd, immediate), (PC, rf, mem)) 
          = (print "Error : Non I-Type opcode, performing NOP\n";
             (PC, rf, mem));


      (*
       * The function PerformRType performs one of the R-Type
       * instructions.  All of the instructions make use of the
       * ALU, and as such, call ALU.PerformAL.
       *)
      fun PerformRType ((SPECIA, rs1, rs2, rd, shamt, NOP), (PC, rf, mem))
          = (PC, rf, mem)

        | PerformRType ((SPECIAL, rs1, rs2, rd, shamt, SLL), (PC, rf, mem)) 
          = (PC,
             RF.StoreRegister(rf, rd,
                              ALU.PerformAL(ALU.SLL,
                                            RF.LoadRegister(rf, rs1),
                                            RF.LoadRegister(rf, rs2))),
             mem)

        | PerformRType ((SPECIAL, rs1, rs2, rd, shamt, SRL), (PC, rf, mem)) 
          = (PC,
             RF.StoreRegister(rf, rd,
                              ALU.PerformAL(ALU.SRL,
                                            RF.LoadRegister(rf, rs1),
                                            RF.LoadRegister(rf, rs2))),
             mem)

        | PerformRType ((SPECIAL, rs1, rs2, rd, shamt, SRA), (PC, rf, mem)) 
          = (PC,
             RF.StoreRegister(rf, rd,
                              ALU.PerformAL(ALU.SRA,
                                            RF.LoadRegister(rf, rs1),
                                            RF.LoadRegister(rf, rs2))),
             mem)

        | PerformRType ((SPECIAL, rs1, rs2, rd, shamt, ADD), (PC, rf, mem)) 
          = (PC,
             RF.StoreRegister(rf, rd,
                              ALU.PerformAL(ALU.ADD,
                                            RF.LoadRegister(rf, rs1),
                                            RF.LoadRegister(rf, rs2))),
             mem)

        | PerformRType ((SPECIAL, rs1, rs2, rd, shamt, ADDU), (PC, rf, mem)) 
          = (PC,
             RF.StoreRegister(rf, rd,
                              ALU.PerformAL(ALU.ADDU,
                                            RF.LoadRegister(rf, rs1),
                                            RF.LoadRegister(rf, rs2))),
             mem)
          
        | PerformRType ((SPECIAL, rs1, rs2, rd, shamt, SUB), (PC, rf, mem)) 
          = (PC,
             RF.StoreRegister(rf, rd,
                              ALU.PerformAL(ALU.SUB,
                                            RF.LoadRegister(rf, rs1),
                                            RF.LoadRegister(rf, rs2))),
             mem)

        | PerformRType ((SPECIAL, rs1, rs2, rd, shamt, SUBU), (PC, rf, mem)) 
          = (PC,
             RF.StoreRegister(rf, rd,
                              ALU.PerformAL(ALU.SUBU,
                                            RF.LoadRegister(rf, rs1),
                                            RF.LoadRegister(rf, rs2))),
             mem)
          
        | PerformRType ((SPECIAL, rs1, rs2, rd, shamt, AND), (PC, rf, mem))
          = (PC,
             RF.StoreRegister(rf, rd,
                              ALU.PerformAL(ALU.AND,
                                            RF.LoadRegister(rf, rs1),
                                            RF.LoadRegister(rf, rs2))),
             mem)
          
        | PerformRType ((SPECIAL, rs1, rs2, rd, shamt, OR), (PC, rf, mem)) 
          = (PC,
             RF.StoreRegister(rf, rd,
                              ALU.PerformAL(ALU.OR,
                                            RF.LoadRegister(rf, rs1),
                                            RF.LoadRegister(rf, rs2))),
             mem)
          
        | PerformRType ((SPECIAL, rs1, rs2, rd, shamt, XOR), (PC, rf, mem)) 
          = (PC,
             RF.StoreRegister(rf, rd,
                              ALU.PerformAL(ALU.XOR,
                                            RF.LoadRegister(rf, rs1),
                                            RF.LoadRegister(rf, rs2))),
             mem)
          
        | PerformRType ((SPECIAL, rs1, rs2, rd, shamt, SEQ), (PC, rf, mem)) 
          = (PC,
             RF.StoreRegister(rf, rd,
                              ALU.PerformAL(ALU.SEQ,
                                            RF.LoadRegister(rf, rs1),
                                            RF.LoadRegister(rf, rs2))),
             mem)
          
        | PerformRType ((SPECIAL, rs1, rs2, rd, shamt, SNE), (PC, rf, mem)) 
          = (PC,
             RF.StoreRegister(rf, rd,
                              ALU.PerformAL(ALU.SNE,
                                            RF.LoadRegister(rf, rs1),
                                            RF.LoadRegister(rf, rs2))),
             mem)
          
        | PerformRType ((SPECIAL, rs1, rs2, rd, shamt, SLT), (PC, rf, mem)) 
          = (PC,
             RF.StoreRegister(rf, rd,
                              ALU.PerformAL(ALU.SLT,
                                            RF.LoadRegister(rf, rs1),
                                            RF.LoadRegister(rf, rs2))),
             mem)
          
        | PerformRType ((SPECIAL, rs1, rs2, rd, shamt, SGT), (PC, rf, mem)) 
          = (PC,
             RF.StoreRegister(rf, rd,
                              ALU.PerformAL(ALU.SGT,
                                            RF.LoadRegister(rf, rs1),
                                            RF.LoadRegister(rf, rs2))),
             mem)
          
        | PerformRType ((SPECIAL, rs1, rs2, rd, shamt, SLE), (PC, rf, mem)) 
          = (PC,
             RF.StoreRegister(rf, rd,
                              ALU.PerformAL(ALU.SLE,
                                            RF.LoadRegister(rf, rs1),
                                            RF.LoadRegister(rf, rs2))),
             mem)
          
        | PerformRType ((SPECIAL, rs1, rs2, rd, shamt, SGE), (PC, rf, mem)) 
          = (PC,
             RF.StoreRegister(rf, rd,
                              ALU.PerformAL(ALU.SGE,
                                            RF.LoadRegister(rf, rs1),
                                            RF.LoadRegister(rf, rs2))),
             mem)
          
        | PerformRType ((_, rs1, rs2, rd, shamt, _), (PC, rf, mem)) 
          = (print "Error : Non R-Type opcode, performing NOP\n";
             (PC, rf, mem));

          
      (*
       * The function PerformJType performs one of the J-Type
       * instructions.
       *)
      fun PerformJType ((J, offset), (PC, rf, mem)) 
          = (Word32.fromInt (Int.+ (Word32.toIntX PC,
                                           Word32.toIntX 
                                           (Word32.<< (offset, 0wx0002)))), 
             rf, mem)
          
        | PerformJType ((JR, offset), (PC, rf, mem)) 
          = (RF.LoadRegister(rf,
                             Word32.toInt(Word32.andb (Word32.>> (offset,
                                                                  0wx0015),
                                                       0wx0000001F : 
                                                       Word32.word))),
             rf, mem)
          
        | PerformJType ((JAL, offset), (PC, rf, mem)) 
          = (Word32.fromInt (Int.+ (Word32.toIntX PC,
                                           Word32.toIntX 
                                           (Word32.<< (offset, 0wx0002)))),
             RF.StoreRegister(rf, 31, PC), 
             mem)
          
        | PerformJType ((JALR, offset), (PC, rf, mem)) 
          = (RF.LoadRegister(rf,
                             Word32.toInt (Word32.andb (Word32.>> (offset,
                                                                   0wx0015),
                                                        0wx0000001F : 
                                                        Word32.word))),
             RF.StoreRegister(rf, 31, PC), 
             mem)
          
        | PerformJType ((TRAP, 0wx00000003 : Word32.word), (PC, rf, mem)) 
          = let
              val x = TextIO.print "Value? ";
              val s = "10" (* TextIO.inputLine TextIO.stdIn; *)
              val i = Int.fromString s;
              val input = if isSome i
                            then valOf i
                            else (TextIO.print "Error : Returning 0\n";
                                  Int.fromInt 0);
            in
              (PC,
               RF.StoreRegister(rf, 14, Word32.fromInt input),
               mem)
            end
        
        | PerformJType ((TRAP, 0wx00000004 : Word32.word), (PC, rf, mem)) 
          = let
              val output =  Int.toString (Word32.toIntX 
                                               (RF.LoadRegister(rf, 14)));

            in
              (TextIO.print ("Output: " ^ output ^ "\n");
               (PC, rf, mem))
            end
        
        | PerformJType ((_, offset), (PC, rf, mem)) 
          = (print "Error : Non J-Type opcode, performing NOP\n";
             (PC, rf, mem));
            
            
      (*
       * The function PerformInstr performs an instruction by
       * passing the instruction to the appropriate auxiliary function.
       *)
      fun PerformInstr (ITYPE instr, (PC, rf, mem))
          = PerformIType (instr, (PC, rf, mem))
        | PerformInstr (RTYPE instr, (PC, rf, mem))
          = PerformRType (instr, (PC, rf, mem))
        | PerformInstr (JTYPE instr, (PC, rf, mem))
          = PerformJType (instr, (PC, rf, mem))
        | PerformInstr (ILLEGAL, (PC, rf, mem))
          = (PC, rf, mem);

            
      (*
       * The function CycleLoop represents the basic clock cylce of
       * the DLX processor.  It takes as input the current program
       * counter, the current register file, and the current memory.
       * It loads, decodes, and executes an instruction and increments
       * the program counter.  If the instruction that was loaded is
       * the HALT instruction, the program terminates, otherwise,
       * CycleLoop is recursively called with the result of performing
       * the instruction.
       *)
      fun CycleLoop (PC, rf, mem)
          = let
              val (nmem, instr_word) = MEM.LoadWord (mem, PC);
              val instr = DecodeInstr instr_word;
              val nPC = Word32.+ (PC, 0wx00000004 : Word32.word);
            in
              if instr = HALT orelse instr = ILLEGAL
                then (print "Program halted.\n"; 
                      print (MEM.GetStatistics (nmem));
                      ())
                else CycleLoop (PerformInstr (instr, (nPC, rf, nmem)))
            end

        
      (*
       * The function LoadProgAux is an auxilary function that
       * assists in loading a program into memory.  It recursively
       * calls itself, each time loading an instruction and incrementing
       * the address to which the next instruction is to be loaded.
       *)
      fun LoadProgAux ([], mem, address)
          = mem
        | LoadProgAux (instrs::instr_list, mem, address)
          = let
              val instro = Word32.fromString instrs;
              val instr = if isSome instro
                            then valOf instro
                            else (print ("Error : Invalid " ^ 
                                         "instruction format, " ^
                                         "returning NOP\n");
                                  0wx00000000 : Word32.word);
            in
              LoadProgAux (instr_list,
                           MEM.StoreWord (mem, address, instr),
                           Word32.+ (address, 0wx00000004 : Word32.word))
            end;

      (*
       * The function LoadProg takes a list of instructions and memory, and
       * loads the file into memory, beginning at 0x10000.
       *)
      fun LoadProg (instr_list, mem)
          = LoadProgAux (instr_list, mem, 0wx00010000 : Word32.word);


      (*
       * The function ReadFileToInstr reads the sequence of
       * instructions in a file into a list.
       *)
      fun ReadFileToInstr file
         = (case TextIO.inputLine file of
               NONE => []
             | SOME l => l :: (ReadFileToInstr file));


      (*
       * The function run_prog is exported by DLXSimulator.
       * It takes a list of instructions, then begins
       * execution of the instructions loaded at 0x10000, with an
       * initialized register file, and the loaded program in an
       * initialised memory.
       *)
      fun run_prog instructions
          = CycleLoop (0wx00010000 : Word32.word,
                       RF.InitRegisterFile (),
                       LoadProg (instructions, MEM.InitMemory ()));

      (*
       * The function run_file is exported by DLXSimulator.
       * It takes the name of a file to be run, then begins
       * execution of the loaded program at 0x10000, with an
       * initialized register file, and the loaded program in an
       * initialized memory.
       *)
      fun run_file filename 
          = (run_prog o ReadFileToInstr) (TextIO.openIn filename);

    end;




(* ************************************************************************* *)

(*
 * Cache1.sml
 *
 * This file describes a small simple level 1 cache.
 *)

structure L1CacheSpec1 : CACHESPEC 
  = struct

      datatype WriteHitOption = Write_Through
                              | Write_Back;

      datatype WriteMissOption = Write_Allocate
                               | Write_No_Allocate;
                                 
      val CacheName = "Level 1 Cache";
      val CacheSize = 256;
      val BlockSize = 4;
      val Associativity = 2;
      val WriteHit = Write_Through;
      val WriteMiss = Write_No_Allocate;

    end;


structure L1Cache1 : MEMORY
  = CachedMemory (structure CS = L1CacheSpec1;
                  structure MEM = Memory; );


structure DLXSimulatorC1 : DLXSIMULATOR
  = DLXSimulatorFun (structure RF = RegisterFile;
                     structure ALU = ALU;
                     structure MEM = L1Cache1; );

(* Example programs *)

val Simple = ["200E002F", 
              "44000004", 
              "44000000"];

val Twos = ["44000003",
            "00000000",
            "3D00FFFF",
            "3508FFFF",
            "010E7026",
            "25CE0001",
            "44000004",
            "00000000",
            "44000000",
            "00000000"];


val Abs = ["44000003",
           "00000000",
           "01C0402A",
           "11000002",
           "00000000",
           "000E7022",
           "44000004",
           "00000000",
           "44000000",
           "00000000"]

val Fact = ["0C000002",
            "00000000",
            "44000000",
            "44000003",
            "000E2020",
            "2FBD0020",
            "AFBF0014",
            "AFBE0010",
            "27BE0020",
            "0C000009",
            "00000000",
            "8FBE0010",
            "8FBF0014",
            "27BD0020",
            "00027020",
            "44000004",
            "00001020",
            "4BE00000",
            "00000000",
            "20080001",
            "0088402C",
            "11000004",
            "00000000",
            "20020001",
            "08000016",
            "00000000",
            "2FBD0004",
            "AFA40000",
            "28840001",
            "2FBD0020",
            "AFBF0014",
            "AFBE0010",
            "27BE0020",
            "0FFFFFF1",
            "00000000",
            "8FBE0010",
            "8FBF0014",
            "27BD0020",
            "8FA40000",
            "27BD0004",
            "00004020",
            "10800005",
            "00000000",
            "01024020",
            "28840001",
            "0BFFFFFB",
            "00000000",
            "01001020",
            "4BE00000",
            "00000000"];

val GCD = ["0C000002",
           "00000000",
           "44000000",
           "44000003",
           "00000000",
           "000E2020",
           "0080402A",
           "11000002",
           "00000000",
           "00042022",
           "44000003",
           "00000000",
           "000E2820",
           "00A0402A",
           "11000002",
           "00000000",
           "00052822",
           "2FBD0020",
           "AFBF0014",
           "AFBE0010",
           "27BE0020",
           "0C00000A",
           "00000000",
           "8FBE0010",
           "8FBF0014",
           "27BD0020",
           "00027020",
           "44000004",
           "00000000",
           "00001020",
           "4BE00000",
           "00000000",
           "14A00004",
           "00000000",
           "00801020",
           "08000013",
           "00000000",
           "0085402C",
           "15000006",
           "00000000",
           "00804020",
           "00A02020",
           "01002820",
           "08000002",
           "00000000",
           "00A42822",
           "2FBD0020",
           "AFBF0014",
           "AFBE0010",
           "27BE0020",
           "0FFFFFED",
           "00000000",
           "8FBE0010",
           "8FBF0014",
           "27BD0020",
           "4BE00000",
           "00000000"];

(*
val _ = DLXSimulatorC1.run_prog GCD
*)

structure Main =
   struct
      fun doit () =
         (DLXSimulatorC1.run_prog Simple
          ; DLXSimulatorC1.run_prog Twos
          ; DLXSimulatorC1.run_prog Abs
          ; DLXSimulatorC1.run_prog Fact
          ; DLXSimulatorC1.run_prog GCD
          )

      val doit =
         fn size =>
         let
            fun loop n =
               if n = 0
                  then ()
               else (doit();
                     loop(n-1))
         in loop size
         end
   end
