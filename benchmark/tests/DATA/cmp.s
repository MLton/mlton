LABEL   entry/L4:      ( /R5 /R3 /R2 /R0 )
LABEL   node0/L227:      ( /R5 /R3 /R2 /R0 )
NOP
GETREAL real3.0/R9 := 3.0
ARITHI  int10/R8 := zero/R0 iadd 10
GETLAB  v/R6 := node119/L0
ARITHI  new_allocptr/R1 := zero/R0 iadd 257
NOP
NOP
NOP
STORE   M[ allocptr/R1 + 0 ] := v/R6
ARITHI  new_allocptr/R12 := allocptr/R1 iadd 2
NOP
STORE   M[ allocptr/R1 + 1 ] := std_cont/R5
NOP
GETLAB  v/R6 := node107/L1
ARITHI  closure/R5 := allocptr/R1 iadd 0
NOP
MOVE    new_allocptr/R1 := new_allocptr/R12
NOP
ARITH   new_allocptr/R12 := allocptr/R1 iadd allocptr/R1
STORE   M[ allocptr/R1 + 0 ] := n/R8
NOP
FETCHi  v/R6 := M[ v/R5 + 0 ]
NOP
NOP
MOVE    new_allocptr/R1 := new_allocptr/R12
NOP
ARITHI  new_allocptr/R12 := allocptr/R1 iadd 1
ARITH   arr/R4 := allocptr/R1 isub n/R8
NOP
NOP
NOP
BRANCH  IF zero/R0 ige n/R8 GOTO node56_496'/L199   ( /R1 /R9 /R8 /R2 /R3 /R4 /R5 /R0 /R12 /R6 )
NOP
ARITHI  new_allocptr/R12 := allocptr/R1 iadd 4
NOP
STORE   M[ allocptr/R1 + 1 ] := v/R6
NOP
ARITHI  int1/R6 := zero/R0 iadd 1
STORE   M[ allocptr/R1 + 2 ] := arr/R4
NOP
STORE   M[ allocptr/R1 + 3 ] := v/R5
NOP
STORE   M[ arr/R4 + 1 ] := v/R9
ARITHI  closure/R5 := allocptr/R1 iadd 1
NOP
MOVE    new_allocptr/R1 := new_allocptr/R12
NOP
NOP
NOP
NOP
NOP
ARITHI  i/R17 := i/R6 iadd 1
ARITH   x/R7 := arr/R4 iadd i/R6
NOP
NOP
NOP
STORE   M[ x/R7 + 1 ] := v/R9
NOP
BRANCH  IF i/R6 ige n/R8 GOTO node101/L156   ( /R8 /R3 /R2 /R1 /R0 /R5 /R9 /R4 /R17 )
NOP
MOVE    i/R6 := i/R17
NOP
NOP
NOP
NOP
NOP
NOP
ARITHI  i/R17 := i/R6 iadd 1
ARITH   x/R7 := arr/R4 iadd i/R6
NOP
NOP
NOP
STORE   M[ x/R7 + 1 ] := v/R9
NOP
LABEL   node85_1061'/L195:      ( /R5 /R0 /R1 /R2 /R3 /R9 /R4 /R17 /R8 /R6 )
BRANCH  IF i/R6 ige n/R8 GOTO node95/L169   ( /R8 /R3 /R2 /R1 /R0 /R5 /R9 /R4 /R17 )
NOP
MOVE    i/R6 := i/R17
NOP
NOP
NOP
NOP
NOP
NOP
ARITHI  i/R17 := i/R6 iadd 1
ARITH   x/R7 := arr/R4 iadd i/R6
NOP
NOP
NOP
STORE   M[ x/R7 + 1 ] := v/R9
NOP
BRANCH  IF i/R6 ige n/R8 GOTO node101/L156   ( /R5 /R0 /R1 /R2 /R3 /R8 /R9 /R4 /R17 )
NOP
MOVE    i/R6 := i/R17
NOP
NOP
NOP
NOP
NOP
NOP
ARITHI  i/R17 := i/R6 iadd 1
ARITH   x/R7 := arr/R4 iadd i/R6
NOP
NOP
NOP
STORE   M[ x/R7 + 1 ] := v/R9
NOP
BRANCH  IF zero/R0 ieq zero/R0 GOTO node85_1061'/L195   ( /R5 /R0 /R1 /R2 /R3 /R9 /R4 /R17 /R8 /R6 )
NOP
LABEL   node101/L156:      ( /R3 /R2 /R1 /R0 /R5 )
MOVE    std_arg/R4 := zero/R0
FETCHi  v/R6 := M[ v/R5 + 0 ]
NOP
NOP
NOP
JUMP    v/R6   ( zero/R0 allocptr/R1 exnhandler/R2 std_cont/R5 std_arg/R4 )
NOP
LABEL   node95/L169:      ( /R3 /R2 /R1 /R0 /R5 )
MOVE    std_arg/R4 := zero/R0
FETCHi  v/R6 := M[ v/R5 + 0 ]
NOP
NOP
NOP
JUMP    v/R6   ( zero/R0 allocptr/R1 exnhandler/R2 std_cont/R5 std_arg/R4 )
NOP
LABEL   node56_496'/L199:      ( /R2 /R3 /R4 /R5 /R0 /R12 /R6 )
MOVE    new_allocptr/R1 := new_allocptr/R12
NOP
NOP
JUMP    v/R6   ( zero/R0 allocptr/R1 exnhandler/R2 std_cont/R5 std_arg/R4 )
NOP
LABEL   entry/L1:      ( /R1 /R2 /R3 /R0 /R5 )
LABEL   node107/L146:      ( /R1 /R2 /R3 /R0 /R5 )
NOP
FETCHi  v/R13 := M[ std_cont/R5 + 2 ]
FETCHi  arr/R4 := M[ std_cont/R5 + 1 ]
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
LABEL   entry/L0:      ( /R1 /R3 /R2 /R4 /R5 /R0 )
LABEL   node119/L70:      ( /R1 /R3 /R2 /R4 /R5 /R0 )
NOP
GETREAL real4.0/R9 := 4.0
ARITHI  int10/R8 := zero/R0 iadd 10
GETLAB  v/R6 := node143/L3
NOP
ARITHI  new_allocptr/R12 := allocptr/R1 iadd 3
NOP
NOP
STORE   M[ allocptr/R1 + 0 ] := v/R6
NOP
FETCHi  x/R6 := M[ std_cont/R5 + 1 ]
NOP
ARITHI  closure/R5 := allocptr/R1 iadd 0
NOP
NOP
STORE   M[ allocptr/R1 + 1 ] := x/R6
NOP
GETLAB  v/R6 := node107/L1
STORE   M[ allocptr/R1 + 2 ] := std_arg/R4
NOP
MOVE    new_allocptr/R1 := new_allocptr/R12
NOP
ARITH   new_allocptr/R12 := allocptr/R1 iadd allocptr/R1
STORE   M[ allocptr/R1 + 0 ] := n/R8
NOP
FETCHi  v/R6 := M[ v/R5 + 0 ]
NOP
NOP
MOVE    new_allocptr/R1 := new_allocptr/R12
NOP
ARITHI  new_allocptr/R12 := allocptr/R1 iadd 1
ARITH   arr/R4 := allocptr/R1 isub n/R8
NOP
NOP
NOP
BRANCH  IF zero/R0 ige n/R8 GOTO node56_2587'/L95   ( /R1 /R9 /R8 /R2 /R3 /R4 /R5 /R0 /R12 /R6 )
NOP
ARITHI  new_allocptr/R12 := allocptr/R1 iadd 4
NOP
STORE   M[ allocptr/R1 + 1 ] := v/R6
NOP
ARITHI  int1/R6 := zero/R0 iadd 1
STORE   M[ allocptr/R1 + 2 ] := arr/R4
NOP
STORE   M[ allocptr/R1 + 3 ] := v/R5
NOP
STORE   M[ arr/R4 + 1 ] := v/R9
ARITHI  closure/R5 := allocptr/R1 iadd 1
NOP
MOVE    new_allocptr/R1 := new_allocptr/R12
NOP
NOP
NOP
NOP
NOP
ARITHI  i/R17 := i/R6 iadd 1
ARITH   x/R7 := arr/R4 iadd i/R6
NOP
NOP
NOP
STORE   M[ x/R7 + 1 ] := v/R9
NOP
BRANCH  IF i/R6 ige n/R8 GOTO node101/L73   ( /R8 /R3 /R2 /R1 /R0 /R5 /R9 /R4 /R17 )
NOP
MOVE    i/R6 := i/R17
NOP
NOP
NOP
NOP
NOP
NOP
ARITHI  i/R17 := i/R6 iadd 1
ARITH   x/R7 := arr/R4 iadd i/R6
NOP
NOP
NOP
STORE   M[ x/R7 + 1 ] := v/R9
NOP
LABEL   node85_3038'/L142:      ( /R5 /R0 /R1 /R2 /R3 /R9 /R4 /R17 /R8 /R6 )
BRANCH  IF i/R6 ige n/R8 GOTO node95/L84   ( /R8 /R3 /R2 /R1 /R0 /R5 /R9 /R4 /R17 )
NOP
MOVE    i/R6 := i/R17
NOP
NOP
NOP
NOP
NOP
NOP
ARITHI  i/R17 := i/R6 iadd 1
ARITH   x/R7 := arr/R4 iadd i/R6
NOP
NOP
NOP
STORE   M[ x/R7 + 1 ] := v/R9
NOP
BRANCH  IF i/R6 ige n/R8 GOTO node101/L73   ( /R5 /R0 /R1 /R2 /R3 /R8 /R9 /R4 /R17 )
NOP
MOVE    i/R6 := i/R17
NOP
NOP
NOP
NOP
NOP
NOP
ARITHI  i/R17 := i/R6 iadd 1
ARITH   x/R7 := arr/R4 iadd i/R6
NOP
NOP
NOP
STORE   M[ x/R7 + 1 ] := v/R9
NOP
BRANCH  IF zero/R0 ieq zero/R0 GOTO node85_3038'/L142   ( /R5 /R0 /R1 /R2 /R3 /R9 /R4 /R17 /R8 /R6 )
NOP
LABEL   node101/L73:      ( /R3 /R2 /R1 /R0 /R5 )
MOVE    std_arg/R4 := zero/R0
FETCHi  v/R6 := M[ v/R5 + 0 ]
NOP
NOP
NOP
JUMP    v/R6   ( zero/R0 allocptr/R1 exnhandler/R2 std_cont/R5 std_arg/R4 )
NOP
LABEL   node95/L84:      ( /R3 /R2 /R1 /R0 /R5 )
MOVE    std_arg/R4 := zero/R0
FETCHi  v/R6 := M[ v/R5 + 0 ]
NOP
NOP
NOP
JUMP    v/R6   ( zero/R0 allocptr/R1 exnhandler/R2 std_cont/R5 std_arg/R4 )
NOP
LABEL   node56_2587'/L95:      ( /R2 /R3 /R4 /R5 /R0 /R12 /R6 )
MOVE    new_allocptr/R1 := new_allocptr/R12
NOP
NOP
JUMP    v/R6   ( zero/R0 allocptr/R1 exnhandler/R2 std_cont/R5 std_arg/R4 )
NOP
LABEL   entry/L3:      ( /R0 /R2 /R3 /R4 /R5 /R1 )
LABEL   node143/L67:      ( /R0 /R2 /R3 /R4 /R5 /R1 )
NOP
ARITHI  new_allocptr/R12 := allocptr/R1 iadd 2
GETREAL real0.0/R10 := 0.0
ARITHI  closure/R11 := allocptr/R1 iadd 0
GETLAB  v/R6 := node213/L2
NOP
MOVE    k/R8 := zero/R0
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
MOVE    new_allocptr/R1 := new_allocptr/R12
ARITHI  int10/R6 := zero/R0 iadd 10
NOP
NOP
NOP
NOP
NOP
NOP
NOP
NOP
ARITH   x/R16 := k/R8 iadd k/R8
ARITHI  v/R9 := k/R8 iadd 1
NOP
NOP
BRANCH  IF k/R8 ige int10/R6 GOTO node206/L51   ( /R10 /R0 /R3 /R2 /R1 /R11 /R4 /R8 /R5 )
NOP
LABEL   node174_1546'/L19:      ( /R0 /R3 /R2 /R1 /R11 /R4 /R8 /R5 )
FETCHi  Z1/R6 := M[ closure/R5 + 2 ]
NOP
NOP
NOP
MOVE    x/R6 := x/R16
NOP
FETCHm  v/R7 := M[ x/R6 + 1 ]
NOP
ARITH   x/R6 := X1/R4 iadd k/R8
NOP
MOVE    k/R8 := v/R9
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
ARITHI  int10/R6 := zero/R0 iadd 10
NOP
NOP
MOVE    v/R10 := v/R14
NOP
NOP
NOP
NOP
NOP
NOP
ARITH   x/R16 := k/R8 iadd k/R8
ARITHI  v/R9 := k/R8 iadd 1
NOP
NOP
BRANCH  IF k/R8 ige int10/R6 GOTO node206/L51   ( /R10 /R0 /R3 /R2 /R1 /R11 /R4 /R8 /R5 )
NOP
BRANCH  IF zero/R0 ieq zero/R0 GOTO node174_1546'/L19   ( /R10 /R0 /R3 /R2 /R1 /R11 /R4 /R8 /R5 )
NOP
LABEL   node206/L51:      ( /R0 /R3 /R2 /R1 /R10 /R11 )
MOVE    std_arg/R4 := Q/R10
MOVE    std_cont/R5 := v/R11
FETCHi  v/R6 := M[ v/R11 + 0 ]
NOP
NOP
NOP
JUMP    v/R6   ( zero/R0 allocptr/R1 exnhandler/R2 std_cont/R5 std_arg/R4 )
NOP
LABEL   entry/L2:      ( /R5 /R2 /R3 /R0 /R1 /R4 )
LABEL   node213/L12:      ( /R5 /R2 /R3 /R0 /R1 /R4 )
NOP
ARITHI  new_allocptr/R12 := allocptr/R1 iadd 1
STORE   M[ allocptr/R1 + 0 ] := std_arg/R4
NOP
FETCHi  v/R13 := M[ std_cont/R5 + 1 ]
ARITHI  S/R4 := allocptr/R1 iadd 0
NOP
NOP
MOVE    new_allocptr/R1 := new_allocptr/R12
NOP
MOVE    v/R5 := v/R13
NOP
FETCHi  v/R6 := M[ v/R5 + 0 ]
NOP
NOP
NOP
JUMP    v/R6   ( zero/R0 allocptr/R1 exnhandler/R2 std_cont/R5 std_arg/R4 )
NOP
