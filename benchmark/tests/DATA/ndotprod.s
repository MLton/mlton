ARITHI  new_allocptr/R1 := zero/R0 iadd 257
LABEL   v/L0:      ( zero/R0 allocptr/R1 exnhandler/R2 std_closure/R3 std_arg/R4 std_cont/R5 )
GETLAB  v/R6 := v/L4
STORE   M[ allocptr/R1 + 0 ] := v/R6
STORE   M[ allocptr/R1 + 1 ] := std_cont/R5
ARITHI  closure/R5 := allocptr/R1 iadd 0
ARITHI  int10/R8 := zero/R0 iadd 10
GETREAL real3.0/R9 := 3.0
ARITHI  new_allocptr/R1 := allocptr/R1 iadd 2
BRANCH  IF zero/R0 ieq zero/R0 GOTO array/L1   ( zero/R0 allocptr/R1 exnhandler/R2 n/R8 v/R9 v/R5 )
LABEL   array/L1:      ( zero/R0 allocptr/R1 exnhandler/R2 n/R8 v/R9 v/R5 )
STORE   M[ allocptr/R1 + 0 ] := n/R8
ARITH   new_allocptr/R1 := n/R8 iadd allocptr/R1
ARITH   arr/R4 := allocptr/R1 isub n/R8
BRANCH  IF zero/R0 ige n/R8 GOTO else/L8   ( zero/R0 allocptr/R1 exnhandler/R2 arr/R4 v/R5 )
GETLAB  v/R6 := v/L3
STORE   M[ allocptr/R1 + 1 ] := v/R6
STORE   M[ allocptr/R1 + 2 ] := arr/R4
STORE   M[ allocptr/R1 + 3 ] := v/R5
ARITHI  closure/R5 := allocptr/R1 iadd 1
STORE   M[ arr/R4 + 1 ] := v/R9
ARITHI  int1/R6 := zero/R0 iadd 1
ARITHI  new_allocptr/R1 := allocptr/R1 iadd 4
BRANCH  IF zero/R0 ieq zero/R0 GOTO g/L2   ( zero/R0 allocptr/R1 exnhandler/R2 i/R6 v/R5 arr/R4 n/R8 v/R9 )
LABEL   else/L8:      ( zero/R0 allocptr/R1 exnhandler/R2 arr/R4 v/R5 )
FETCHi  v/R6 := M[ v/R5 + 0 ]
ARITHI  new_allocptr/R1 := allocptr/R1 iadd 1
JUMP    v/R6   ( zero/R0 allocptr/R1 exnhandler/R2 std_cont/R5 std_arg/R4 )
LABEL   g/L2:      ( zero/R0 allocptr/R1 exnhandler/R2 i/R6 v/R5 arr/R4 n/R8 v/R9 )
BRANCH  IF i/R6 ige n/R8 GOTO else/L9   ( zero/R0 allocptr/R1 exnhandler/R2 v/R5 )
ARITH   x/R7 := arr/R4 iadd i/R6
STORE   M[ x/R7 + 1 ] := v/R9
ARITHI  i/R6 := i/R6 iadd 1
BRANCH  IF i/R6 ige n/R8 GOTO else/L10   ( zero/R0 allocptr/R1 exnhandler/R2 v/R5 )
ARITH   x/R7 := arr/R4 iadd i/R6
STORE   M[ x/R7 + 1 ] := v/R9
ARITHI  i/R6 := i/R6 iadd 1
BRANCH  IF zero/R0 ieq zero/R0 GOTO g/L2   ( zero/R0 allocptr/R1 exnhandler/R2 i/R6 v/R5 arr/R4 n/R8 v/R9 )
LABEL   else/L10:      ( zero/R0 allocptr/R1 exnhandler/R2 v/R5 )
FETCHi  v/R6 := M[ v/R5 + 0 ]
MOVE    std_arg/R4 := zero/R0
JUMP    v/R6   ( zero/R0 allocptr/R1 exnhandler/R2 std_cont/R5 std_arg/R4 )
LABEL   else/L9:      ( zero/R0 allocptr/R1 exnhandler/R2 v/R5 )
FETCHi  v/R6 := M[ v/R5 + 0 ]
MOVE    std_arg/R4 := zero/R0
JUMP    v/R6   ( zero/R0 allocptr/R1 exnhandler/R2 std_cont/R5 std_arg/R4 )
LABEL   v/L3:      ( zero/R0 allocptr/R1 exnhandler/R2 std_cont/R5 std_arg/R4 )
FETCHi  arr/R4 := M[ std_cont/R5 + 1 ]
FETCHi  v/R5 := M[ std_cont/R5 + 2 ]
FETCHi  v/R6 := M[ v/R5 + 0 ]
JUMP    v/R6   ( zero/R0 allocptr/R1 exnhandler/R2 std_cont/R5 std_arg/R4 )
LABEL   v/L4:      ( zero/R0 allocptr/R1 exnhandler/R2 std_cont/R5 std_arg/R4 )
GETLAB  v/R6 := v/L5
STORE   M[ allocptr/R1 + 0 ] := v/R6
FETCHi  x/R6 := M[ std_cont/R5 + 1 ]
STORE   M[ allocptr/R1 + 1 ] := x/R6
STORE   M[ allocptr/R1 + 2 ] := std_arg/R4
ARITHI  closure/R5 := allocptr/R1 iadd 0
ARITHI  int10/R8 := zero/R0 iadd 10
GETREAL real4.0/R9 := 4.0
ARITHI  new_allocptr/R1 := allocptr/R1 iadd 3
BRANCH  IF zero/R0 ieq zero/R0 GOTO array/L1   ( zero/R0 allocptr/R1 exnhandler/R2 n/R8 v/R9 v/R5 )
LABEL   v/L5:      ( zero/R0 allocptr/R1 exnhandler/R2 std_cont/R5 std_arg/R4 )
GETLAB  v/R6 := v/L7
STORE   M[ allocptr/R1 + 0 ] := v/R6
FETCHi  x/R6 := M[ std_cont/R5 + 1 ]
STORE   M[ allocptr/R1 + 1 ] := x/R6
ARITHI  closure/R11 := allocptr/R1 iadd 0
GETREAL real0.0/R10 := 0.0
ARITHI  new_allocptr/R1 := allocptr/R1 iadd 2
MOVE    k/R8 := zero/R0
BRANCH  IF zero/R0 ieq zero/R0 GOTO f/L6   ( zero/R0 allocptr/R1 exnhandler/R2 k/R8 Q/R10 v/R11 X1/R4 closure/R5 )
LABEL   f/L6:      ( zero/R0 allocptr/R1 exnhandler/R2 k/R8 Q/R10 v/R11 X1/R4 closure/R5 )
ARITHI  int10/R6 := zero/R0 iadd 10
BRANCH  IF k/R8 ige int10/R6 GOTO else/L11   ( zero/R0 allocptr/R1 exnhandler/R2 v/R11 Q/R10 )
ARITHI  v/R9 := k/R8 iadd 1
FETCHi  Z1/R6 := M[ closure/R5 + 2 ]
ARITH   x/R6 := Z1/R6 iadd k/R8
FETCHm  v/R7 := M[ x/R6 + 1 ]
ARITH   x/R6 := X1/R4 iadd k/R8
FETCHm  v/R6 := M[ x/R6 + 1 ]
ARITH   v/R6 := v/R7 fmul v/R6
ARITH   v/R10 := Q/R10 fadd v/R6
MOVE    k/R8 := v/R9
BRANCH  IF zero/R0 ieq zero/R0 GOTO f/L6   ( zero/R0 allocptr/R1 exnhandler/R2 k/R8 Q/R10 v/R11 X1/R4 closure/R5 )
LABEL   else/L11:      ( zero/R0 allocptr/R1 exnhandler/R2 v/R11 Q/R10 )
FETCHi  v/R6 := M[ v/R11 + 0 ]
MOVE    std_cont/R5 := v/R11
MOVE    std_arg/R4 := Q/R10
JUMP    v/R6   ( zero/R0 allocptr/R1 exnhandler/R2 std_cont/R5 std_arg/R4 )
LABEL   v/L7:      ( zero/R0 allocptr/R1 exnhandler/R2 std_cont/R5 std_arg/R4 )
STORE   M[ allocptr/R1 + 0 ] := std_arg/R4
ARITHI  S/R4 := allocptr/R1 iadd 0
FETCHi  v/R5 := M[ std_cont/R5 + 1 ]
FETCHi  v/R6 := M[ v/R5 + 0 ]
ARITHI  new_allocptr/R1 := allocptr/R1 iadd 1
JUMP    v/R6   ( zero/R0 allocptr/R1 exnhandler/R2 std_cont/R5 std_arg/R4 )
