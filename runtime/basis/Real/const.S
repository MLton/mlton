	.file "Real_const.S"
#if (defined (__CYGWIN__))
.data

.globl _Real_maxFinite
	.align 8
	.def _Real_maxFinite
		.scl 2 		# external
		.size 8
	.endef
_Real_maxFinite:
	.long 0xFFFFFFFF
	.long 0x7FEFFFFF

.globl _Real_minNormalPos
	.align 8
	.def _Real_minNormalPos
		.scl 2 		# external
		.size 8
	.endef
_Real_minNormalPos:
	.long 0x00000000
	.long 0x00100000

.globl _Real_minPos
	.align 8
	.def _Real_minPos
		.scl 2 		# external
		.size 8
	.endef
_Real_minPos:
	.long 0x00000001
	.long 0x00000000

#elif (defined (__linux__) || defined (__FreeBSD__))
.data

.globl Real_maxFinite
	.align 8
	.type Real_maxFinite,@object
	.size Real_maxFinite, 8
Real_maxFinite:
	.long 0xFFFFFFFF
	.long 0x7FEFFFFF

.globl Real_minNormalPos
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

#elif (defined (__sun__))

// Don't need to do anything, since Real.c defines these constants.

#else

#error Real_ constants not defined

#endif
