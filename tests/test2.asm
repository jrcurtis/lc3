	.orig x3000
	
labL babl:gobl:	getC
	add r0, r0, #11 ; add R1, R0, R0
R0:	out
	brNZP GOBL

	.FILL x111

balm	.end


add #4, #4 abc