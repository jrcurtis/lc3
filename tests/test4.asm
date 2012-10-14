	.orig x3000
	ADD R0, R0, #15
	ADD R0, R0, #15
	AND R0, R1, #100
	OUT
	.end
	
	
	.orig x3500
	.STRINGZ "abcdefghijklmnop"
	.end