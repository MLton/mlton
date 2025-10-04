structure DfaGen =
struct
  datatype regex =
    CHAR_LITERAL of {char: char, position: int}
  | CONCAT of {l: regex, r: regex, leftMaxState: int, rightMaxState: int}
  | ALTERNATION of {l: regex, r: regex, leftMaxState: int, rightMaxState: int}
  | ZERO_OR_ONE of regex
  | ZERO_OR_MORE of regex
  | ONE_OR_MORE of regex
  | GROUP of regex
  | WILDCARD of int

  structure Set =
  struct
    datatype 'a set = BRANCH of 'a set * int * 'a * 'a set | LEAF

    fun insertOrReplace (newKey, newVal, tree) =
      case tree of
        BRANCH (l, curKey, curVal, r) =>
          if newKey > curKey then
            let val r = insertOrReplace (newKey, newVal, r)
            in BRANCH (l, curKey, curVal, r)
            end
          else if newKey < curKey then
            let val l = insertOrReplace (newKey, newVal, l)
            in BRANCH (l, curKey, curVal, r)
            end
          else
            BRANCH (l, newKey, newVal, r)
      | LEAF => BRANCH (LEAF, newKey, newVal, LEAF)

    fun getOrDefault (findKey, tree, default) =
      case tree of
        BRANCH (l, curKey, curVal, r) =>
          if findKey > curKey then getOrDefault (findKey, r, default)
          else if findKey < curKey then getOrDefault (findKey, l, default)
          else curVal
      | LEAF => default

    fun helpToList (tree, acc) =
      case tree of
        BRANCH (l, curKey, curVal, r) =>
          let
            val acc = helpToList (r, acc)
            val acc = (curKey, curVal) :: acc
          in
            helpToList (l, acc)
          end
      | LEAF => acc

    fun toList tree = helpToList (tree, [])

    fun helpKeysToList (tree, acc) =
      case tree of
        BRANCH (l, curKey, _, r) =>
          let
            val acc = helpKeysToList (r, acc)
            val acc = curKey :: acc
          in
            helpKeysToList (l, acc)
          end
      | LEAF => acc

    fun keysToList tree = helpKeysToList (tree, [])

    fun map (f, tree) =
      case tree of
        BRANCH (l, key, value, r) =>
          let
            val r = map (f, r)
            val l = map (f, l)
            val value = f value
          in
            BRANCH (l, key, value, r)
          end
      | LEAF => LEAF

    fun foldl (f, tree, acc) =
      case tree of
        BRANCH (l, k, v, r) =>
          let
            val acc = foldl (f, l, acc)
            val acc = f (v, acc)
          in
            foldl (f, r, acc)
          end
      | LEAF => acc

    fun helpToCharsAndPositionsList (tree, acc) =
      case tree of
        BRANCH (l, k, v, r) =>
          let
            val acc = helpToCharsAndPositionsList (r, acc)
            val acc = {char = v, position = k} :: acc
          in
            helpToCharsAndPositionsList (l, acc)
          end
      | LEAF => acc

    fun toCharsAndPositionsList tree = helpToCharsAndPositionsList (tree, [])
  end

  structure ParseDfa =
  struct
    (* parsing through precedence climbing algorithm. *)
    val postfixLevel = 1
    val concatLevel = 2
    val altLevel = 3

    local
      fun loop (pos, str, openParens, closeParens) =
        if pos = String.size str then
          NONE
        else
          case String.sub (str, pos) of
            #"(" => loop (pos + 1, str, openParens + 1, closeParens)
          | #")" =>
              if closeParens + 1 = openParens then SOME pos
              else loop (pos + 1, str, openParens, closeParens + 1)
          | _ => loop (pos + 1, str, openParens, closeParens)
    in
      fun getRightParenIdx (pos, str) = loop (pos, str, 1, 0)
    end

    fun computeAtom (pos, str, stateNum) =
      if pos = String.size str then
        NONE
      else
        case String.sub (str, pos) of
          #"(" =>
            (case getRightParenIdx (pos + 1, str) of
               SOME groupEndIdx =>
                 let
                   val substr = String.substring
                     (str, pos + 1, groupEndIdx - pos - 1)
                 in
                   case parse (substr, stateNum) of
                     SOME (rhs, stateNum) =>
                       SOME (groupEndIdx + 1, rhs, stateNum)
                   | NONE => NONE
                 end
             | NONE => NONE)
        | #")" => NONE
        | #"?" => NONE
        | #"*" => NONE
        | #"+" => NONE
        | #"." => SOME (pos + 1, WILDCARD (stateNum + 1), stateNum + 1)
        | chr =>
            let val chr = CHAR_LITERAL {char = chr, position = stateNum + 1}
            in SOME (pos + 1, chr, stateNum + 1)
            end

    and climb (pos, str, lhs, level, stateNum) : (int * regex * int) option =
      if pos = String.size str then
        SOME (pos, lhs, stateNum)
      else
        case String.sub (str, pos) of
          #"|" =>
            if level < altLevel then
              SOME (pos, lhs, stateNum)
            else if pos + 1 < String.size str then
              let
                val chr = String.sub (str, pos + 1)
                val chr = CHAR_LITERAL {char = chr, position = stateNum + 1}
              in
                case climb (pos + 2, str, chr, altLevel, stateNum + 1) of
                  SOME (pos, rhs, rightStateNum) =>
                    let
                      val result = ALTERNATION
                        { l = lhs
                        , r = rhs
                        , leftMaxState = stateNum
                        , rightMaxState = rightStateNum
                        }
                    in
                      SOME (pos, result, rightStateNum)
                    end
                | NONE => NONE
              end
            else
              NONE
        | #"?" =>
            if level < postfixLevel then
              SOME (pos, lhs, stateNum)
            else
              let val lhs = ZERO_OR_ONE lhs
              in climb (pos + 1, str, lhs, postfixLevel, stateNum)
              end
        | #"*" =>
            if level < postfixLevel then
              SOME (pos, lhs, stateNum)
            else
              let val lhs = ZERO_OR_MORE lhs
              in climb (pos + 1, str, lhs, postfixLevel, stateNum)
              end
        | #"+" =>
            if level < postfixLevel then
              SOME (pos, lhs, stateNum)
            else
              let val lhs = ONE_OR_MORE lhs
              in climb (pos + 1, str, lhs, postfixLevel, stateNum)
              end
        | chr =>
            if level < concatLevel then
              SOME (pos, lhs, stateNum)
            else
              case computeAtom (pos, str, stateNum) of
                SOME (nextPos, curAtom, atomStateNum) =>
                  (case climb (nextPos, str, curAtom, concatLevel, atomStateNum) of
                     SOME (pos, rhs, rightStateNum) =>
                       let
                         val result = CONCAT
                           { l = lhs
                           , r = rhs
                           , leftMaxState = stateNum
                           , rightMaxState = rightStateNum
                           }
                       in
                         SOME (pos, result, rightStateNum)
                       end
                   | NONE => NONE)
              | NONE => NONE

    and loop (pos, str, ast, stateNum) =
      if pos = String.size str then
        SOME (ast, stateNum)
      else
        case climb (pos, str, ast, altLevel, stateNum) of
          SOME (pos, ast, stateNum) => loop (pos, str, ast, stateNum)
        | NONE => NONE

    and parse (str, stateNum) =
      if String.size str > 0 then
        case computeAtom (0, str, stateNum) of
          SOME (nextPos, lhs, stateNum) => loop (nextPos, str, lhs, stateNum)
        | NONE => NONE
      else
        NONE
  end

  structure ToDfa =
  struct
    fun isNullable tree =
      case tree of
        CHAR_LITERAL _ => false
      | WILDCARD _ => false

      | CONCAT {l, r, ...} => isNullable l andalso isNullable r
      | ALTERNATION {l, r, ...} => isNullable l orelse isNullable r

      | ZERO_OR_ONE _ => true
      | ZERO_OR_MORE _ => true

      | ONE_OR_MORE regex => isNullable regex
      | GROUP regex => isNullable regex

    fun firstpos (tree, acc) =
      case tree of
        CHAR_LITERAL {position, ...} => position :: acc
      | WILDCARD i => i :: acc

      | CONCAT {l, r, ...} =>
          if isNullable l then
            let val acc = firstpos (l, acc)
            in firstpos (r, acc)
            end
          else
            firstpos (l, acc)
      | ALTERNATION {l, r, ...} =>
          let val acc = firstpos (l, acc)
          in firstpos (r, acc)
          end

      | ZERO_OR_ONE regex => firstpos (regex, acc)
      | ZERO_OR_MORE regex => firstpos (regex, acc)
      | ONE_OR_MORE regex => firstpos (regex, acc)
      | GROUP regex => firstpos (regex, acc)

    fun firstposWithChar (tree, acc) =
      case tree of
        CHAR_LITERAL {position, char} =>
          {position = position, char = Char.ord char} :: acc
      | WILDCARD position => {position = position, char = ~1} :: acc

      | CONCAT {l, r, ...} =>
          if isNullable l then
            let val acc = firstposWithChar (l, acc)
            in firstposWithChar (r, acc)
            end
          else
            firstposWithChar (l, acc)
      | ALTERNATION {l, r, ...} =>
          let val acc = firstposWithChar (l, acc)
          in firstposWithChar (r, acc)
          end
      | ZERO_OR_ONE regex => firstposWithChar (regex, acc)
      | ZERO_OR_MORE regex => firstposWithChar (regex, acc)
      | ONE_OR_MORE regex => firstposWithChar (regex, acc)
      | GROUP regex => firstposWithChar (regex, acc)

    fun lastpos (tree, acc) =
      case tree of
        CHAR_LITERAL {position, ...} => position :: acc
      | WILDCARD i => i :: acc

      | CONCAT {l, r, ...} =>
          if isNullable r then
            let val acc = lastpos (l, acc)
            in lastpos (r, acc)
            end
          else
            lastpos (l, acc)
      | ALTERNATION {l, r, ...} =>
          let val acc = lastpos (l, acc)
          in lastpos (r, acc)
          end

      | ZERO_OR_ONE regex => lastpos (regex, acc)
      | ZERO_OR_MORE regex => lastpos (regex, acc)
      | ONE_OR_MORE regex => lastpos (regex, acc)
      | GROUP regex => lastpos (regex, acc)

    fun followpos (char, regex, acc) =
      case regex of
        CONCAT {r, ...} => firstposWithChar (r, acc)
      | ZERO_OR_MORE r => firstposWithChar (r, acc)
      | ZERO_OR_ONE r => firstposWithChar (r, acc)
      | ONE_OR_MORE r => firstposWithChar (r, acc)
      | _ => acc

    fun insertIntsFromList (lst, acc) =
      case lst of
        {position, char} :: tl =>
          let val acc = Set.insertOrReplace (position, char, acc)
          in insertIntsFromList (tl, acc)
          end
      | [] => acc

    (* for help finding followpos of a particular node.
     * Get list of concat and loop nodes to pos.
     * Direct ancestor is at front of list, and furthest ancestor
     * is at end of list.
     * We are filtering until first concat because the concat node
     * represents the first transition, from which the pos can exit
     * its current state.
     * *)
    fun filterUntilFirstConcat (lst: regex list, acc, char: int) =
      case lst of
        hd :: tl =>
          (case hd of
             CONCAT _ =>
               let val fp = followpos (char, hd, [])
               in insertIntsFromList (fp, acc)
               end
           | ZERO_OR_ONE _ =>
               let
                 val fp = followpos (char, hd, [])
                 val acc = insertIntsFromList (fp, acc)
               in
                 filterUntilFirstConcat (tl, acc, char)
               end
           | ZERO_OR_MORE _ =>
               let
                 val fp = followpos (char, hd, [])
                 val acc = insertIntsFromList (fp, acc)
               in
                 filterUntilFirstConcat (tl, acc, char)
               end
           | ONE_OR_MORE _ =>
               let
                 val fp = followpos (char, hd, [])
                 val acc = insertIntsFromList (fp, acc)
               in
                 filterUntilFirstConcat (tl, acc, char)
               end
           | _ =>
               raise Fail
                 "dfa-gen.sml 310: should only have loops and concats \
                 \in list to filter")
      | [] => acc

    fun getConcatAndLoopsToPos (tree: regex, pos: int, acc: regex list, char) =
      case tree of
        CONCAT {l, r, leftMaxState, rightMaxState} =>
          if pos <= leftMaxState then
            getConcatAndLoopsToPos (l, pos, tree :: acc, char)
          else
            getConcatAndLoopsToPos (r, pos, tree :: acc, char)
      | ZERO_OR_ONE r => getConcatAndLoopsToPos (r, pos, tree :: acc, char)
      | ZERO_OR_MORE r => getConcatAndLoopsToPos (r, pos, tree :: acc, char)
      | ONE_OR_MORE r => getConcatAndLoopsToPos (r, pos, tree :: acc, char)

      | ALTERNATION {l, r, leftMaxState, rightMaxState} =>
          if pos <= leftMaxState then getConcatAndLoopsToPos (l, pos, acc, char)
          else getConcatAndLoopsToPos (r, pos, acc, char)
      | CHAR_LITERAL _ => acc
      | WILDCARD _ => acc
      | GROUP r => getConcatAndLoopsToPos (r, pos, acc, char)

    fun ifStatesInVec (pos, dstates, newStates) =
      if pos = Vector.length dstates then
        false
      else
        let
          val {transitions, marked = _} = Vector.sub (dstates, pos)
        in
          transitions = newStates
          orelse ifStatesInVec (pos + 1, dstates, newStates)
        end

    fun getUnmarkedTransitionsIfExists (pos, dstates) =
      if pos = Vector.length dstates then
        NONE
      else
        let
          val record = Vector.sub (dstates, pos)
        in
          if #marked record then
            getUnmarkedTransitionsIfExists (pos + 1, dstates)
          else
            SOME (pos, #transitions record)
        end

    fun convertLoop (regex, dstates, dtran) =
      case getUnmarkedTransitionsIfExists (0, dstates) of
        SOME (unmarkedIdx, unamarkedTransition) =>
          let
            (* mark transition *)
            val dstates =
              let
                val newMark = {marked = true, transitions = unamarkedTransition}
              in
                Vector.update (dstates, unmarkedIdx, newMark)
              end

            (* get follow transitions *)
            val nodes =
              List.map
                (fn {char, position} =>
                   let
                     val node =
                       getConcatAndLoopsToPos (regex, position, [], char)
                   in
                     {node = node, char = char}
                   end) unamarkedTransition

            val follows =
              List.foldl
                (fn ({node, char}, set) =>
                   let
                     val subset = Set.getOrDefault (char, set, Set.LEAF)
                     val subset = filterUntilFirstConcat (node, subset, char)
                   in
                     Set.insertOrReplace (char, subset, set)
                   end) Set.LEAF nodes

            (* add any new states we find to dstates
             * and add mark this state as the transition *)
            val (dstates, dtran) = Set.foldl
              ( fn (subtree, (dstates, dtran)) =>
                  let
                    val subtreeStates = Set.toCharsAndPositionsList subtree
                  in
                    if ifStatesInVec (0, dstates, subtreeStates) then
                      ( dstates
                      , Vector.update (dtran, unmarkedIdx, subtreeStates)
                      )
                    else
                      let
                        val dtran = Vector.concat
                          [dtran, Vector.fromList [subtreeStates]]

                        val record =
                          {marked = false, transitions = subtreeStates}
                        val dstates = Vector.concat
                          [dstates, Vector.fromList [record]]
                      in
                        (dstates, dtran)
                      end
                  end
              , follows
              , (dstates, dtran)
              )
          in
            convertLoop (regex, dstates, dtran)
          end
      | NONE => (Vector.map #transitions dstates, dtran)

    fun convert regex =
      let
        val first = List.rev (firstposWithChar (regex, []))
        val dstates = Vector.fromList [{transitions = first, marked = false}]
      in
        convertLoop (regex, dstates, Vector.fromList [])
      end
  end

  fun fromString str =
    case ParseDfa.parse (str, 0) of
      SOME (ast, _) => ToDfa.convert ast
    | NONE => (Vector.fromList [], Vector.fromList [])
end

val dfa = DfaGen.fromString "(a|b)*abb#"
