LABEL   entry/L4:      ( /R2 /R3 /R5 /R0 )
LABEL   node0/L323:      ( /R2 /R3 /R5 /R0 )
NOP
ARITHI  new_allocptr/R1 := zero/R0 iadd 257
NOP
NOP
NOP
GETLAB  v/R6 := node119/L0
NOP
NOP
NOP
STORE   M[ allocptr/R1 + 0 ] := v/R6
NOP
STORE   M[ allocptr/R1 + 1 ] := std_cont/R5
NOP
ARITHI  closure/R5 := allocptr/R1 iadd 0
NOP
NOP
NOP
ARITHI  int10/R8 := zero/R0 iadd 10
NOP
NOP
NOP
GETREAL real3.0/R9 := 3.0
NOP
NOP
NOP
ARITHI  new_allocptr/R12 := allocptr/R1 iadd 2
NOP
NOP
NOP
MOVE    new_allocptr/R1 := new_allocptr/R12
NOP
STORE   M[ allocptr/R1 + 0 ] := n/R8
NOP
ARITH   new_allocptr/R12 := allocptr/R1 iadd allocptr/R1
NOP
NOP
NOP
MOVE    new_allocptr/R1 := new_allocptr/R12
NOP
ARITH   arr/R4 := allocptr/R1 isub n/R8
NOP
NOP
NOP
NOP
NOP
NOP
NOP
NOP
BRANCH  IF zero/R0 ige n/R8 GOTO node56/L266   ( /R9 /R8 /R2 /R3 /R4 /R0 /R1 /R5 )
NOP
GETLAB  v/R6 := node107/L1
NOP
NOP
NOP
STORE   M[ allocptr/R1 + 1 ] := v/R6
NOP
STORE   M[ allocptr/R1 + 2 ] := arr/R4
NOP
STORE   M[ allocptr/R1 + 3 ] := v/R5
NOP
ARITHI  closure/R5 := allocptr/R1 iadd 1
NOP
NOP
NOP
STORE   M[ arr/R4 + 1 ] := v/R9
NOP
ARITHI  int1/R6 := zero/R0 iadd 1
NOP
NOP
NOP
ARITHI  new_allocptr/R12 := allocptr/R1 iadd 4
NOP
NOP
NOP
MOVE    new_allocptr/R1 := new_allocptr/R12
NOP
LABEL   node65/L257:      ( /R5 /R0 /R3 /R2 /R1 /R9 /R4 /R8 /R6 )
NOP
NOP
NOP
NOP
NOP
NOP
BRANCH  IF i/R6 ige n/R8 GOTO node101/L221   ( /R4 /R6 /R9 /R8 /R1 /R2 /R3 /R0 /R5 )
NOP
ARITH   x/R7 := arr/R4 iadd i/R6
NOP
NOP
NOP
STORE   M[ x/R7 + 1 ] := v/R9
NOP
ARITHI  i/R17 := i/R6 iadd 1
NOP
NOP
NOP
MOVE    i/R6 := i/R17
NOP
NOP
NOP
NOP
NOP
NOP
BRANCH  IF i/R6 ige n/R8 GOTO node95/L227   ( /R4 /R6 /R9 /R1 /R2 /R3 /R0 /R5 )
NOP
ARITH   x/R7 := arr/R4 iadd i/R6
NOP
NOP
NOP
STORE   M[ x/R7 + 1 ] := v/R9
NOP
ARITHI  i/R17 := i/R6 iadd 1
NOP
NOP
NOP
MOVE    i/R6 := i/R17
NOP
BRANCH  IF zero/R0 ieq zero/R0 GOTO node65/L257   ( /R5 /R0 /R3 /R2 /R1 /R9 /R4 /R8 /R6 )
NOP
LABEL   node95/L227:      ( /R1 /R2 /R3 /R0 /R5 )
NOP
FETCHi  v/R6 := M[ v/R5 + 0 ]
NOP
NOP
NOP
MOVE    std_arg/R4 := zero/R0
NOP
JUMP    v/R6   ( zero/R0 allocptr/R1 exnhandler/R2 std_cont/R5 std_arg/R4 )
NOP
LABEL   node101/L221:      ( /R1 /R2 /R3 /R0 /R5 )
NOP
FETCHi  v/R6 := M[ v/R5 + 0 ]
NOP
NOP
NOP
MOVE    std_arg/R4 := zero/R0
NOP
JUMP    v/R6   ( zero/R0 allocptr/R1 exnhandler/R2 std_cont/R5 std_arg/R4 )
NOP
LABEL   node56/L266:      ( /R2 /R3 /R4 /R0 /R1 /R5 )
NOP
FETCHi  v/R6 := M[ v/R5 + 0 ]
NOP
NOP
NOP
ARITHI  new_allocptr/R12 := allocptr/R1 iadd 1
NOP
NOP
NOP
MOVE    new_allocptr/R1 := new_allocptr/R12
NOP
JUMP    v/R6   ( zero/R0 allocptr/R1 exnhandler/R2 std_cont/R5 std_arg/R4 )
NOP
LABEL   entry/L1:      ( /R1 /R2 /R3 /R0 /R5 )
LABEL   node107/L214:      ( /R1 /R2 /R3 /R0 /R5 )
NOP
FETCHi  arr/R4 := M[ std_cont/R5 + 1 ]
NOP
NOP
NOP
FETCHi  v/R13 := M[ std_cont/R5 + 2 ]
NOP
NOP
NOP
MOVE    v/R5 := v/R13
NOP
FETCHi  v/R6 := M[ v/R5 + 0 ]
NOP
NOP
NOP
JUMP    v/R6   ( zero/R0 allocptr/R1 exnhandler/R2 std_cont/R5 std_arg/R4 )
NOP
LABEL   entry/L0:      ( /R1 /R4 /R3 /R2 /R0 /R5 )
LABEL   node119/L104:      ( /R1 /R4 /R3 /R2 /R0 /R5 )
NOP
GETLAB  v/R6 := node143/L3
NOP
NOP
NOP
STORE   M[ allocptr/R1 + 0 ] := v/R6
NOP
FETCHi  x/R6 := M[ std_cont/R5 + 1 ]
NOP
NOP
NOP
STORE   M[ allocptr/R1 + 1 ] := x/R6
NOP
STORE   M[ allocptr/R1 + 2 ] := std_arg/R4
NOP
ARITHI  closure/R5 := allocptr/R1 iadd 0
NOP
NOP
NOP
ARITHI  int10/R8 := zero/R0 iadd 10
NOP
NOP
NOP
GETREAL real4.0/R9 := 4.0
NOP
NOP
NOP
ARITHI  new_allocptr/R12 := allocptr/R1 iadd 3
NOP
NOP
NOP
MOVE    new_allocptr/R1 := new_allocptr/R12
NOP
STORE   M[ allocptr/R1 + 0 ] := n/R8
NOP
ARITH   new_allocptr/R12 := allocptr/R1 iadd allocptr/R1
NOP
NOP
NOP
MOVE    new_allocptr/R1 := new_allocptr/R12
NOP
ARITH   arr/R4 := allocptr/R1 isub n/R8
NOP
NOP
NOP
NOP
NOP
NOP
NOP
NOP
BRANCH  IF zero/R0 ige n/R8 GOTO node56/L155   ( /R9 /R8 /R2 /R3 /R4 /R0 /R1 /R5 )
NOP
GETLAB  v/R6 := node107/L1
NOP
NOP
NOP
STORE   M[ allocptr/R1 + 1 ] := v/R6
NOP
STORE   M[ allocptr/R1 + 2 ] := arr/R4
NOP
STORE   M[ allocptr/R1 + 3 ] := v/R5
NOP
ARITHI  closure/R5 := allocptr/R1 iadd 1
NOP
NOP
NOP
STORE   M[ arr/R4 + 1 ] := v/R9
NOP
ARITHI  int1/R6 := zero/R0 iadd 1
NOP
NOP
NOP
ARITHI  new_allocptr/R12 := allocptr/R1 iadd 4
NOP
NOP
NOP
MOVE    new_allocptr/R1 := new_allocptr/R12
NOP
LABEL   node65/L146:      ( /R5 /R0 /R3 /R2 /R1 /R9 /R4 /R8 /R6 )
NOP
NOP
NOP
NOP
NOP
NOP
BRANCH  IF i/R6 ige n/R8 GOTO node101/L110   ( /R4 /R6 /R9 /R8 /R1 /R2 /R3 /R0 /R5 )
NOP
ARITH   x/R7 := arr/R4 iadd i/R6
NOP
NOP
NOP
STORE   M[ x/R7 + 1 ] := v/R9
NOP
ARITHI  i/R17 := i/R6 iadd 1
NOP
NOP
NOP
MOVE    i/R6 := i/R17
NOP
NOP
NOP
NOP
NOP
NOP
BRANCH  IF i/R6 ige n/R8 GOTO node95/L116   ( /R4 /R6 /R9 /R1 /R2 /R3 /R0 /R5 )
NOP
ARITH   x/R7 := arr/R4 iadd i/R6
NOP
NOP
NOP
STORE   M[ x/R7 + 1 ] := v/R9
NOP
ARITHI  i/R17 := i/R6 iadd 1
NOP
NOP
NOP
MOVE    i/R6 := i/R17
NOP
BRANCH  IF zero/R0 ieq zero/R0 GOTO node65/L146   ( /R5 /R0 /R3 /R2 /R1 /R9 /R4 /R8 /R6 )
NOP
LABEL   node95/L116:      ( /R1 /R2 /R3 /R0 /R5 )
NOP
FETCHi  v/R6 := M[ v/R5 + 0 ]
NOP
NOP
NOP
MOVE    std_arg/R4 := zero/R0
NOP
JUMP    v/R6   ( zero/R0 allocptr/R1 exnhandler/R2 std_cont/R5 std_arg/R4 )
NOP
LABEL   node101/L110:      ( /R1 /R2 /R3 /R0 /R5 )
NOP
FETCHi  v/R6 := M[ v/R5 + 0 ]
NOP
NOP
NOP
MOVE    std_arg/R4 := zero/R0
NOP
JUMP    v/R6   ( zero/R0 allocptr/R1 exnhandler/R2 std_cont/R5 std_arg/R4 )
NOP
LABEL   node56/L155:      ( /R2 /R3 /R4 /R0 /R1 /R5 )
NOP
FETCHi  v/R6 := M[ v/R5 + 0 ]
NOP
NOP
NOP
ARITHI  new_allocptr/R12 := allocptr/R1 iadd 1
NOP
NOP
NOP
MOVE    new_allocptr/R1 := new_allocptr/R12
NOP
JUMP    v/R6   ( zero/R0 allocptr/R1 exnhandler/R2 std_cont/R5 std_arg/R4 )
NOP
LABEL   entry/L3:      ( /R1 /R0 /R2 /R3 /R4 /R5 )
LABEL   node143/L93:      ( /R1 /R0 /R2 /R3 /R4 /R5 )
NOP
GETLAB  v/R6 := node213/L2
NOP
NOP
NOP
STORE   M[ allocptr/R1 + 0 ] := v/R6
NOP
FETCHi  x/R6 := M[ std_cont/R5 + 1 ]
NOP
NOP
NOP
STORE   M[ allocptr/R1 + 1 ] := x/R6
NOP
ARITHI  closure/R11 := allocptr/R1 iadd 0
NOP
NOP
NOP
GETREAL real0.0/R10 := 0.0
NOP
NOP
NOP
ARITHI  new_allocptr/R12 := allocptr/R1 iadd 2
NOP
NOP
NOP
MOVE    new_allocptr/R1 := new_allocptr/R12
NOP
MOVE    k/R8 := zero/R0
NOP
LABEL   node164/L72:      ( /R11 /R1 /R2 /R3 /R10 /R4 /R5 /R8 /R0 )
NOP
ARITHI  int10/R6 := zero/R0 iadd 10
NOP
NOP
NOP
NOP
NOP
NOP
NOP
NOP
BRANCH  IF k/R8 ige int10/R6 GOTO node206/L30   ( /R8 /R5 /R4 /R10 /R0 /R3 /R2 /R1 /R11 )
NOP
ARITHI  v/R9 := k/R8 iadd 1
NOP
NOP
NOP
FETCHi  Z1/R6 := M[ closure/R5 + 2 ]
NOP
NOP
NOP
ARITH   x/R16 := k/R8 iadd k/R8
NOP
NOP
NOP
MOVE    x/R6 := x/R16
NOP
FETCHm  v/R7 := M[ x/R6 + 1 ]
NOP
NOP
NOP
ARITH   x/R6 := X1/R4 iadd k/R8
NOP
NOP
NOP
FETCHm  v/R15 := M[ x/R6 + 1 ]
NOP
NOP
NOP
MOVE    v/R6 := v/R15
NOP
ARITH   v/R15 := v/R6 fmul v/R6
NOP
NOP
NOP
NOP
NOP
MOVE    v/R6 := v/R15
NOP
ARITH   v/R14 := v/R6 fadd v/R6
NOP
NOP
NOP
MOVE    v/R10 := v/R14
NOP
MOVE    k/R8 := v/R9
NOP
BRANCH  IF zero/R0 ieq zero/R0 GOTO node164/L72   ( /R11 /R1 /R2 /R3 /R10 /R4 /R5 /R8 /R0 )
NOP
LABEL   node206/L30:      ( /R10 /R0 /R3 /R2 /R1 /R11 )
NOP
FETCHi  v/R6 := M[ v/R11 + 0 ]
NOP
NOP
NOP
MOVE    std_cont/R5 := v/R11
NOP
MOVE    std_arg/R4 := Q/R10
NOP
JUMP    v/R6   ( zero/R0 allocptr/R1 exnhandler/R2 std_cont/R5 std_arg/R4 )
NOP
LABEL   entry/L2:      ( /R5 /R2 /R3 /R0 /R1 /R4 )
LABEL   node213/L22:      ( /R5 /R2 /R3 /R0 /R1 /R4 )
NOP
STORE   M[ allocptr/R1 + 0 ] := std_arg/R4
NOP
ARITHI  S/R4 := allocptr/R1 iadd 0
NOP
NOP
NOP
FETCHi  v/R13 := M[ std_cont/R5 + 1 ]
NOP
NOP
NOP
MOVE    v/R5 := v/R13
NOP
FETCHi  v/R6 := M[ v/R5 + 0 ]
NOP
NOP
NOP
ARITHI  new_allocptr/R12 := allocptr/R1 iadd 1
NOP
NOP
NOP
MOVE    new_allocptr/R1 := new_allocptr/R12
NOP
JUMP    v/R6   ( zero/R0 allocptr/R1 exnhandler/R2 std_cont/R5 std_arg/R4 )
NOP
