	.ORIG x3000

;
; Lab 6 - Double reversal
;
; Jacob Curtis (jrcurtis@ucsc.edu)
; Lab section 7 (Will)
;

;
; Imagine that you're programming in Java. This is kind of like
; public static void main(String[] args), you dig?
;
MAIN	LEA R0, PROMPT
	PUTS
	LEA R1, INPUTS
	JSR INPUT
	AND R0, R0, #0
	ADD R0, R0, #10
	OUT
	LEA R2, INPUTS
	JSR OUTPUT
	AND R0, R0, #0
	ADD R0, R0, #10
	OUT
	BRnzp MAIN
PROMPT	.STRINGZ "Words now>> "
INPUTS	.BLKW 41

;
; Preconditions: R1 holds the address of a stack in which to save the user's
;                input.
;
INPUT	ST R0, INPUT_R0
	ST R2, INPUT_R2	; R2 holds intermediate values
	ST R3, INPUT_R3
	ST R7, INPUT_R7
	AND R3, R3, #0	; 0 in R7 indicates no subroutine has been called
	AND R0, R0, #0
	ADD R0, R0, #15
	ADD R0, R0, #15
	ADD R0, R0, #2
	JSR PUSH	; Initial space, which will be at the end so output
INPUT_LOOP		; can detect the last word.
	GETC
	OUT
	ADD R2, R0, #-10
;	ADD R2, R2, #-15
;	ADD R2, R2, #-15
;	ADD R2, R2, #-15
;	ADD R2, R2, #-15
;	ADD R2, R2, #-15
;	ADD R2, R2, #-15
;	ADD R2, R2, #-15
;	ADD R2, R2, #-7
	BRz INPUT_FINISH
	JSR PUSH
	ADD R3, R3, #1
	BRnzp INPUT_LOOP
INPUT_FINISH
	ADD R3, R3, #0
	BRz INPUT_HALT
	LD R0, INPUT_R0
	LD R2, INPUT_R2
	LD R3, INPUT_R3
	LD R7, INPUT_R7
	RET
INPUT_HALT
	LEA R0, INPUT_GOODBYE
	PUTS
	HALT
INPUT_R0	.FILL 0
INPUT_R2	.FILL 0
INPUT_R3	.FILL 0
INPUT_R7	.FILL 0
INPUT_GOODBYE	.STRINGZ "I hope you've enjoyed this program.\n"

;
; Preconditions: R2 holds the stack from which to retrieve input.
;
OUTPUT	ST R1, OUTPUT_R1
	ST R2, OUTPUT_R2
	ST R3, OUTPUT_R3
	ST R4, OUTPUT_R4
	ST R7, OUTPUT_R7
	AND R4, R4, #0	; R4 switches between input and word
	ADD R4, R4, R2
	LEA R1, OUTPUTS	; Temp swap value. Holds word
OUTPUT_LOOP
	AND R2, R2, #0	; Switch in input
	ADD R2, R2, R4
	JSR POP		; Next input char
	ADD R3, R0, #1	; Stack empty
	BRz OUTPUT_FINISH
	ADD R3, R0, #-16
	ADD R3, R3, #-16
	BRz OUTPUT_OUT	; Got space
	JSR PUSH
	BRnzp OUTPUT_LOOP
OUTPUT_OUT
	AND R0, R0, #0
	ADD R0, R0, #15
	ADD R0, R0, #15
	ADD R0, R0, #2
	OUT
	AND R4, R4, #0	; Switch out input
	ADD R4, R2, #0
	AND R2, R2, #0
	ADD R2, R1, #0	; Switch in word
OUTPUT_OUT_LOOP
	JSR POP		; Next word char
	ADD R3, R0, #1
	BRz OUTPUT_LOOP	; End of word
	OUT
	BRnzp OUTPUT_OUT_LOOP
OUTPUT_FINISH
	LD R1, OUTPUT_R1
	LD R2, OUTPUT_R2
	LD R3, OUTPUT_R3
	LD R4, OUTPUT_R4
	LD R7, OUTPUT_R7
	RET
OUTPUT_R1	.FILL 0
OUTPUT_R2	.FILL 0
OUTPUT_R3	.FILL 0
OUTPUT_R4	.FILL 0
OUTPUT_R5	.FILL 0
OUTPUT_R7	.FILL 0
OUTPUTS	.BLKW 41

;
; Preconditions: R0 holds the value to be pushed.
;                R1 holds the address of the stack on which to push.
;
PUSH	ST R2, PUSH_R2
	ST R3, PUSH_R3
	LDR R2, R1, #0	; R2 is top of stack
	AND R3, R3, #0
	ADD R3, R1, R2	; Move to top
	STR R0, R3, #1	; Store with offset to data
	ADD R2, R2, #1	; Move top up
	STR R2, R1, #0
	LD R2, PUSH_R2
	LD R3, PUSH_R3
	RET
PUSH_R2	.FILL 0
PUSH_R3	.FILL 0

;
; Preconditions: R2 holds the address of the stack from which to pop
; Postconditions: R0 holds the value popped.
;
POP	ST R1, POP_R1
	ST R3, POP_R3
	LDR R1, R2, #0	; R1 is top of stack
	BRz POP_EMPTY
	AND R3, R3, #0
	ADD R3, R2, R1	; Move to top
	LDR R0, R3, #0	; Pop that number
	ADD R1, R1, #-1	; Top comes down
	STR R1, R2, #0
	BRnzp POP_FINISH
POP_EMPTY
	AND R0, R0, #0
	ADD R0, R0, #-1
POP_FINISH
	LD R1, POP_R1
	LD R3, POP_R3
	ADD R0, R0, #0
	RET
POP_R1	.FILL 0
POP_R3	.FILL 0

	.END





