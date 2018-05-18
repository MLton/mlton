(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(* Taken from
 * Applications of Path Compression on Balanced Trees
 * Robert Endre Tarjan
 * JACM, 26, 4
 * October 1979
 * 690-715
 *)

functor DisjointMax(O: ORDER): DISJOINT_MAX =
struct

structure O = O

datatype t = T of {label: O.t ref,
                   info: info ref}
and info =
   Parent of t
  | Root of {size: int ref,
             child: t option ref}

fun parent (T{info = ref (Parent p), ...}) = p
  | parent _ = Error.bug "DisjointMax.parent"
fun setParent(T{info, ...}, p) = info := Parent p
fun labelRef (T{label, ...}) = label
val (label, setLabel) = Ref.getAndSet labelRef
fun sizeRef (T{info = ref(Root{size, ...}), ...}) = size
  | sizeRef _ = Error.bug "DisjointMax.sizeRef"
val (size, setSize) = Ref.getAndSet sizeRef
fun childRef (T{info = ref(Root{child, ...}), ...}) = child
  | childRef _ = Error.bug "DisjointMax.childRef"
val (childOption, setChildOption) = Ref.getAndSet childRef
val child = Option.projector childOption
fun setChild(r, c) = setChildOption(r, SOME c)

fun subsize r = size r - (case childOption r of
                             NONE => 0
                           | SOME r' => size r')

fun hasParent (T{info = ref (Parent _), ...}) = true
  | hasParent _ = false

fun isRoot (T{info = ref (Root _), ...}) = true
  | isRoot _ = false

fun singleton l = T{label = ref l,
                    info = ref (Root{size = ref 0, child = ref NONE})}

fun update(r, l) =
   if not(isRoot r) then Error.error "DisjointMax.update"
   else if O.<=(l, label r) then ()
   else let
           fun link r =
              case childOption r of
                 NONE => r
               | SOME r' =>
                     if O.<=(l, label r') then r
                     else if subsize r >= subsize r'
                              then (setChildOption(r, childOption r') ;
                                    setParent(r', r) ;
                                    link r)
                          else (setSize(r', size r) ;
                                setParent(r, r') ;
                                link r')
        in (setLabel(r, l) ;
            case childOption r of
               NONE => ()
             | SOME r' => if O.<=(l, label r') then ()
                          else let val r' = link r'
                               in (setChild(r, r') ;
                                   setLabel(r', l))
                               end)
        end

fun link(r, r') =
   if not (isRoot r andalso isRoot r') then Error.error "DisjointMax.link"
   else let val s = size r
            val s' = size r'
            fun move NONE = ()
              | move (SOME r') = let val r'' = childOption r'
                                 in (setParent(r', r) ; move r'')
                                 end
        in (update(r', label r) ;
            setSize(r, s + s') ;
            if s < s' then move (childOption r) else move (SOME r'))
        end

fun compress s = (* Pre: hasParent s *)
   let val p = parent s
   in if hasParent p
      then (compress p ;
            setLabel(s, O.max(label s, label p)) ;
            setParent(s, parent p))
      else ()
   end

fun eval s = if isRoot s then label s
             else (compress s ;
                   O.max(label s, label (parent s)))

end
