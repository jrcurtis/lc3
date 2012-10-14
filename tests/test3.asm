	.orig x3000
	
a	add R0, R0, #5
	add R0, R0, R1
	OUT
	GETC
ohmy	LDR R0, R1, #0
	.stringz "hey hey hey \n \t hey there"
noway	.blkw 7
oyes	.fill 56
hey lay tray gray	.end
