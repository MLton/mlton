(* splaytree.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Splay tree structure.
 *
 *)

structure SplayTree: SPLAY_TREE = 
  struct

    datatype 'a splay = 
      SplayObj of {
        value: 'a,
        right: 'a splay,
        left: 'a splay
      }
    | SplayNil

    datatype 'a ans_t = No | Eq of 'a | Lt of 'a | Gt of 'a

    fun splay (compf, root) = let
        fun adj SplayNil = (No, SplayNil, SplayNil)
          | adj (arg as SplayObj{value, left, right}) =
              (case compf value of
                EQUAL => (Eq value, left, right)
              | GREATER =>
                  (case left of
                    SplayNil => (Gt value, SplayNil, right)
                  | SplayObj{value=value', left=left', right=right'} =>
                      (case compf value' of
                        EQUAL => (Eq value', left',
                                    SplayObj{value=value, left=right', right=right})
                      | GREATER =>
                          (case left' of 
                            SplayNil => (Gt value', left', SplayObj{value=value, left=right', right=right})
                          | _ => 
                            let val (V, L, R) = adj left'
                                val rchild = SplayObj{value=value, left=right', right=right}
                            in
                              (V, L, SplayObj{value=value', left=R, right=rchild})
                            end
                          ) (* end case *)
                      | _ =>
                          (case right' of 
                            SplayNil => (Lt value', left', SplayObj{value=value, left=right', right=right})
                          | _ =>
                            let val (V, L, R) = adj right'
                                 val rchild = SplayObj{value=value, left=R, right=right}
                                 val lchild = SplayObj{value=value', left=left', right=L}
                            in
                              (V, lchild, rchild)
                            end
                          ) (* end case *)
                      ) (* end case *)
                  ) (* end case *)
              | _ =>
                 (case right of
                   SplayNil => (Lt value, left, SplayNil)
                 | SplayObj{value=value', left=left', right=right'} =>
                     (case compf value' of
                       EQUAL =>
                         (Eq value', SplayObj{value=value, left=left, right=left'}, right')
                     | LESS =>
                         (case right' of
                           SplayNil => (Lt value', SplayObj{value=value, left=left, right=left'}, right')
                         | _ =>
                           let val (V, L, R) = adj right'
                               val lchild = SplayObj{value=value, left=left, right=left'}
                           in
                             (V, SplayObj{value=value', left=lchild, right=L}, R)
                           end
                         ) (* end case *)
                     | _ =>
                         (case left' of
                           SplayNil => (Gt value', SplayObj{value=value, left=left, right=left'}, right')
                         | _ =>
                           let val (V, L, R) = adj left'
                               val rchild = SplayObj{value=value', left=R, right=right'}
                               val lchild = SplayObj{value=value, left=left, right=L}
                           in
                             (V, lchild, rchild)
                           end
                         ) (* end case *)
                     ) (* end case *)
                 ) (* end case *)
              ) (* end case *)
      in
        case adj root of
          (No, _, _) => (GREATER, SplayNil)
        | (Eq v, l, r) => (EQUAL, SplayObj{value=v, left=l, right=r})
        | (Lt v, l, r) => (LESS, SplayObj{value=v, left=l, right=r})
        | (Gt v, l, r) => (GREATER, SplayObj{value=v, left=l, right=r})
      end

    fun lrotate SplayNil = SplayNil
      | lrotate (arg as SplayObj{value, left, right=SplayNil}) = arg
      | lrotate (SplayObj{value, left, right=SplayObj{value=v, left=l, right=r}}) = 
          lrotate (SplayObj{value=v, left=SplayObj{value=value, left=left, right=l}, right=r})

    fun join (SplayNil, SplayNil) = SplayNil
      | join (SplayNil, t) = t
      | join (t, SplayNil) = t
      | join (l, r) =
          case lrotate l of
            SplayNil => r      (* impossible as l is not SplayNil *)
          | SplayObj{value, left, right} => SplayObj{value=value, left=left, right=r}

  end (* SplayTree *)

