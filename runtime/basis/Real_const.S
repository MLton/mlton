	.file "Real_const.S"
.data
.globl Real_maxFinite
	.align 8
	.type	 Real_maxFinite,@object
	.size	 Real_maxFinite,8
Real_maxFinite:
	.long 0xFFFFFFFF
	.long 0x7FEFFFFF
.globl Real_minNormalPos
	.align 8
	.type	 Real_minNormalPos,@object
	.size	 Real_minNormalPos,8
Real_minNormalPos:
	.long 0x00000000
	.long 0x00100000
.globl Real_minPos
	.align 8
	.type	 Real_minPos,@object
	.size	 Real_minPos,8
Real_minPos:
	.long 0x00000001
	.long 0x00000000
