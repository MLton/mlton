	.align 4
	.globl bcopy_simd
	.type bcopy_simd,@function
bcopy_simd:
	pushl	%ebp
	movl	%esp,%ebp
	pushl	%edi
        pushl	%esi

	movl	16(%ebp),%ecx	# size
	movl	8(%ebp),%eax	# source
	orl	12(%ebp),%eax	# destination

	cmpl	$31,%ecx		# size < 32
	jle	bcopy4

	movl	8(%ebp),%edx
	movb	(%edx),%eax	# touch first byte of src
	movb	-4(%ecx,%edx),%eax	# touch last byte of src

	andb	$0xe0,%dl	# align src to 32-byte boundary
	xorl	%eax,%eax
bcopy1:
	prefetchnta (%eax,%edx)
	addl	$32,%eax
	cmpl	%ecx,%eax
	jl	bcopy1

	movl	8(%ebp),%esi	# src
	movl	12(%ebp),%edi	# dst

	movl	%edi,%eax
	andl	$15,%eax
	je	bcopy2
	movl	%ecx,%edx
	movl	$16,%ecx
	sub	%eax,%ecx
	sub	%ecx,%edx
	rep ; movsb
	movl	%edx,%ecx
	jmp	bcopy2a
bcopy2:
	movups	(%esi),%xmm0
	movups	16(%esi),%xmm1

	movntps	%xmm0,(%edi)
	movntps	%xmm1,16(%edi)

	addl	$-32,%ecx
	addl	$32,%esi
	addl	$32,%edi
bcopy2a:
	cmpl	$31,%ecx
	jg	bcopy2
bcopy4:
	testl	%ecx,%ecx
	je	bcopy5
	rep ; movsb
	
bcopy5:	
	sfence			# flush the WC buffer

	popl %esi
	popl %edi
	popl %ebp
	ret
