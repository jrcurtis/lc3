	.ORIG x3000
;
; Print the prompt for the user.
;
START	LEA R0, PROMPT
	PUTS		; Print prompt
	JSR INPUT	; Get input (into R0)
	AND R1, R1, #0	; Store input in R1
	ADD R1, R1, R0
	LEA R0, OUT_0	; Display output message part 0
	PUTS
	AND R0, R0, #0
	ADD R0, R0, R1	; Put input back in R0
	JSR DISPD	; Display as decimal
	LEA R0, OUT_1	; Display output message part 1
	PUTS
	AND R0, R0, #0
	ADD R0, R0, R1	; Put input back in R0 again
	JSR DISPH	; Display as hexadecimal
	LEA R0, OUT_2	; Display output message part 2
	PUTS
	AND R0, R0, #0
	ADD R0, R0, R1	; Put input back in R0 again
	JSR DSPB2	; Display as binary
	LEA R0, OUT_3	; Display output message part 3
	PUTS
	AND R0, R0, #0
	ADD R0, R0, R1	; Put input back in R0 again
	JSR DISPLAY	; Display as English
	LEA R0, OUT_4	; Display output message part 4
	PUTS
	AND R0, R0, #0
	ADD R0, R0, R1
	JSR DISPD	; The number __n__
	LEA R0, OUT_5
	PUTS		; Is divisible by
	AND R3, R3, #0	; Store input in R3. MOD clobbers R1.
	ADD R3, R3, R1
	AND R2, R2, #0
	ADD R2, R2, #1	; Start testing divisors with 2 (incremented in loop)
	AND R4, R4, #0	; Largest known divisor? None!
	AND R5, R5, #0	; R5 is how many divisors we've found. 0
PRE_MOD_LOOP	; Determine largest divisor so "and" can be printed last.
	AND R1, R1, #0
	ADD R1, R1, R3	; Initial input is dividend
	ADD R2, R2, #1	; And divisor + 1 is divisor
	JSR MOD
	BRnp PRE_NOT_DIVISIBLE
	AND R4, R4, #0	; Here we know divisor is an even divisor of input
	ADD R4, R4, R2	; Store current divisor into largest known divisor
	ADD R5, R5, #1
PRE_NOT_DIVISIBLE
	AND R1, R1, #0
	ADD R1, R1, R3	; Restore original input to R1
	ADD R1, R1, #0
	BRzp PRE_POSITIVE
	NOT R1, R1	; Negate input if it's negative because div test
	ADD R1, R1, #1	; interprets negative results as divisor too big.
PRE_POSITIVE	; Number in R1 (abs of original input) is positive
	JSR DIV		; How many times divisor goes into it?
	ADD R1, R1, #-2	; If two or less, then no more could possibly fit.
	BRn FOUND_LAST_DIVISOR
	BRnzp PRE_MOD_LOOP
FOUND_LAST_DIVISOR
	ADD R4, R4, #0
	BRz NO_DIVISORS	; If there are no divisors (largest is 0)
	ADD R1, R5, #-1	; Is number of divisors == 1?
	BRnp MULTIPLE_DIVISORS
	BRnzp ONE_DIVISOR	; If we only found one, don't print and, etc.
MULTIPLE_DIVISORS
	AND R2, R2, #0
	ADD R2, R2, #1	; Restart divisors at 2
	NOT R4, R4	; We need to check if current divisor == largest divisor
	ADD R4, R4, #1	; Easiest if we can subtract and check for zero
REAL_MOD_LOOP	; This is the real mod loop, which includes printing stuff.
	AND R1, R1, #0
	ADD R1, R1, R3	; Initial input is dividend
	ADD R2, R2, #1	; And divisor + 1 is divisor
	JSR MOD
	BRnp REAL_MOD_LOOP	; This number isn't divisible. Try again.
	ADD R1, R4, R2	; This number is divisible. Is it the last?
	BRnp REGULAR_DIVISOR
	LEA R0, OUT_7	; This is not a regular divisor, it's the last one.
	PUTS		; So print final "and"
	AND R0, R0, #0
	ADD R0, R0, R2
	JSR DISPD
	BRnzp REAL_MOD_LOOP_END
REGULAR_DIVISOR
	AND R0, R0, #0
	ADD R0, R0, R2
	JSR DISPD	; Display divisor
	LEA R0, OUT_6	; ", "
	PUTS
	BRnzp REAL_MOD_LOOP
REAL_MOD_LOOP_END
	LEA R0, OUT_8
	PUTS		; Print out final punctuation (!)
	BRnzp START	; Get new number
ONE_DIVISOR
	AND R0, R0, #0
	ADD R0, R0, R4
	JSR DISPD	; Print single divisor with no commas, ands, etc.
	LEA R0, OUT_8	; And end punctuation.
	PUTS
	BRnzp START
NO_DIVISORS
	LEA R0, OUT_9
	PUTS
	Brnzp START
PROMPT	.STRINGZ "Please input a number from -9999 to 9999\n>>"
OUT_0	.STRINGZ "Your number ("
OUT_1	.STRINGZ ", x"
OUT_2	.STRINGZ ", b"
OUT_3	.STRINGZ ") in English is "
OUT_4	.STRINGZ "!\nThe number "
OUT_5	.STRINGZ " is divisible by "
OUT_6	.STRINGZ ", "
OUT_7	.STRINGZ "and "
OUT_8	.STRINGZ "!\n"
OUT_9	.STRINGZ "nothing.\n"

;
; English prefixes for numbers. First digit in name is the power of ten the
; prefix applies to and the second is the digit. Prefixes not included for
; hundreds or thousands because they match the case for ones.
;
PRE_00	.STRINGZ "zero"
PRE_01	.STRINGZ "one"
PRE_02	.STRINGZ "two"
PRE_03	.STRINGZ "three"
PRE_04	.STRINGZ "four"
PRE_05	.STRINGZ "five"
PRE_06	.STRINGZ "six"
PRE_07	.STRINGZ "seven"
PRE_08	.STRINGZ "eight"
PRE_09	.STRINGZ "nine"
PRE_T0	.STRINGZ "ten"
PRE_T1	.STRINGZ "eleven"	; Teen prefixes
PRE_T2	.STRINGZ "twelve"
PRE_T3	.STRINGZ "thir"
PRE_T4	.STRINGZ "four"
PRE_T5	.STRINGZ "fif"
PRE_T6	.STRINGZ "six"
PRE_T7	.STRINGZ "seven"
PRE_T8	.STRINGZ "eigh"
PRE_T9	.STRINGZ "nine"
PRE_Y2	.STRINGZ "twen"		; Special cases for tens (fiftY etc.) place
PRE_Y4	.STRINGZ "for"		; Rest use teens

;
; English postfixes for words. Ones have no postfix.
;
POS_0	.STRINGZ "teen"	; Special case for 1 in tens place
POS_1	.STRINGZ "ty"
POS_2	.STRINGZ " hundred"
POS_3	.STRINGZ " thousand"

;
; Gets 0-4 characters from the user (plus optional negative sign), converting
; them to a 2's complement integer as they are read. If 0 characters, then exit.
;
; Preconditions: None
; Postconditions: R0 will hold the input number
;
INPUT	ST R1, INPUT_R1	; Save registers
	ST R2, INPUT_R2
	ST R3, INPUT_R3
	ST R4, INPUT_R4
	ST R7, INPUT_R7
	
	AND R1, R1, #0	; R1 holds our number
	AND R2, R2, #0	; R2 holds flag for negative number
	AND R3, R3, #0	; R3 holds number of iterations
	AND R4, R4, #0	; R4 holds intermediates
	
	BRnzp INPUT_LOOP
INPUT_FIRST_CHAR
	LD R4, INPUT_NEG
	ADD R4, R0, R4
	BRz INPUT_SET_NEG	; First character was '-'?
	LD R4, INPUT_TERM
	ADD R4, R0, R4
	Brz INPUT_HALT		; First character was termination?
	BRnzp INPUT_LOOP_PROC
INPUT_SET_NEG
	OUT		; Show '-'
	NOT R2, R2	; Set negative flag
	BRnzp INPUT_LOOP
INPUT_HALT
	HALT
INPUT_LOOP
	GETC
	ADD R3, R3, #0
	BRz INPUT_FIRST_CHAR	; First character has special considerations
INPUT_LOOP_PROC
	LD R4, INPUT_TERM
	ADD R4, R0, R4
	BRz INPUT_LAST	; Termination character?
	LD R4, INPUT_9
	ADD R4, R0, R4
	BRp INPUT_LOOP	; Input above '9'?
	LD R4, INPUT_0
	ADD R4, R0, R4
	BRn INPUT_LOOP	; Input below '0'?
	ST R2, INPUT_R2_LOCAL	; MUL uses R2 as second operand
	AND R2, R2, #0
	ADD R2, R2, #10
	JSR MUL		; Else, valid digit. Multiply last place by ten.
	LD R2, INPUT_R2_LOCAL
	ADD R1, R1, R4	; Add new digit on
	OUT		; Print input char
	ADD R3, R3, #1	; This is one more digit
	AND R4, R4, #0
	ADD R4, R3, #-4
	BRz INPUT_LAST	; Last (4th) character input?
	BRnzp INPUT_LOOP
INPUT_LAST
	AND R0, R0, #0
	ADD R0, R0, #10	; Newline
	OUT
	ADD R2, R2, #0
	BRn INPUT_NEGATE	; Negative flag was set?
	BRnzp INPUT_CLEANUP
INPUT_NEGATE
	NOT R1, R1
	ADD R1, R1, #1
INPUT_CLEANUP
	AND R0, R0, #0
	ADD R0, R0, R1	; Return input number in R1
	LD R1, INPUT_R1	; Restore values
	LD R2, INPUT_R2
	LD R3, INPUT_R3
	LD R4, INPUT_R4
	LD R7, INPUT_R7
	ADD R0, R0, #0	; Set status codes based on input value
	RET
; Storage for INPUT
INPUT_NEG	.FILL #-45	; Negative sign
INPUT_0		.FILL #-48	; '0'
INPUT_9		.FILL #-57	; '9'
;INPUT_TERM	.FILL #-10	; '\n' (doesn't work under unix simulator)
INPUT_TERM	.FILL #-122	; 'z'
INPUT_R2_LOCAL	.FILL 0		; Local R2 saved during multiply routine
INPUT_R0	.FILL 0
INPUT_R1	.FILL 0
INPUT_R2	.FILL 0
INPUT_R3	.FILL 0
INPUT_R4	.FILL 0
INPUT_R7	.FILL 0

;
; The actual string memory blocks are uneven distances away, so we need to make
; contiguous blocks of pointers. Located near display rather than near the
; strings because only display needs them and they were still too far away up
; there.
;
PRE_00_P	.FILL PRE_00
PRE_01_P	.FILL PRE_01
PRE_02_P	.FILL PRE_02
PRE_03_P	.FILL PRE_03
PRE_04_P	.FILL PRE_04
PRE_05_P	.FILL PRE_05
PRE_06_P	.FILL PRE_06
PRE_07_P	.FILL PRE_07
PRE_08_P	.FILL PRE_08
PRE_09_P	.FILL PRE_09
PRE_T0_P	.FILL PRE_T0
PRE_T1_P	.FILL PRE_T1
PRE_T2_P	.FILL PRE_T2
PRE_T3_P	.FILL PRE_T3
PRE_T4_P	.FILL PRE_T4
PRE_T5_P	.FILL PRE_T5
PRE_T6_P	.FILL PRE_T6
PRE_T7_P	.FILL PRE_T7
PRE_T8_P	.FILL PRE_T8
PRE_T9_P	.FILL PRE_T9
PRE_Y2_P	.FILL PRE_Y2
PRE_Y4_P	.FILL PRE_Y4
POS_0_P		.FILL POS_0
POS_1_P		.FILL POS_1
POS_2_P		.FILL POS_2
POS_3_P		.FILL POS_3

;
; Takes a 2's complement integer and displays its English representation.
;
; Preconditions: The value in R0 is taken to be a two's complement integer
;                to be displayed.
; Postconditions: The words associated with the number will have been displayed.
;
DISPLAY ADD R0, R0, #0
	BRnp DISPLAY_NON_ZERO	; 0 is trivial
	ST R7, DISP_R7
	JSR DISPLAY_ZERO	; And it was creating an infinite loop...
	LD R7, DISP_R7
	RET
DISPLAY_NON_ZERO
	ST R0, DISP_R0
	ST R1, DISP_R1
	ST R2, DISP_R2
	ST R3, DISP_R3
	ST R4, DISP_R4
	ST R5, DISP_R5
	ST R7, DISP_R7
	AND R1, R1, #0	; R1 holds current multiple of ten
	ADD R1, R1, #1
	AND R2, R2, #0	; R2 used by multiply and divide routines
	ADD R2, R2, #1
	AND R3, R3, #0	; R3 holds current power of ten
	AND R4, R4, #0	; 
	AND R5, R5, #0	; R5 holds original number from R0
	ADD R5, R5, R0	; (R0 needed for output)
	BRzp DISPLAY_LOOP_ASC	; Number is positive
	NOT R5, R5		; Otherwise, negative
	ADD R5, R5, #1		; Negate to positive
	LEA R0, DISP_NEG_WORD	; Display negative sign.
	PUTS
;
; The purpose of this loop is to find the highest multiple of ten that fits
; into the number. This is because we need to print the larger digits first,
; so we have to find out where to start.
;
; First our input number is in R1 and 1 is in R2. DIV stores its result in R1
; then, if the result isn't zero, we need to multiply that 10 by 10 so R1 is
; overwritten with 10, multiplication happens, and the result is copied to R2.
; R1 is then restored to the original input number at the beginning of the loop.
;
; R3 is incremented everytime the multiple of ten is multiplied by ten. In other
; words it is the log base 10 of that multiple. Used later as an array index
; into the postfix array.
;
DISPLAY_LOOP_ASC
	AND R1, R1, #0
	ADD R1, R1, R5	; Set R1 to number
	JSR DIV		; How many times does ten*x go into number?
	BRz DISPLAY_LOOP_DESC	; If zero, then exit loop
	AND R1, R1, #0		; Otherwise, keep multiplying
	ADD R1, R1, #10
	JSR MUL		; Highest multiple of ten up by one
	ADD R3, R3, #1	; One more power of ten
	AND R2, R2, #0
	ADD R2, R2, R1	; Store in R2 for next loop
	BRnzp DISPLAY_LOOP_ASC
;
; This is the reverse of the previous loop. At each multiple of ten reached,
; the English representation of that number multiplied by that power of ten is
; printed, e.g. prefix 'two', postfix 'hundred' in the hundreds place, but
; prefix 'twen', postfix 'ty' in the tens place.
;
; At the beginning of this loop R2 holds the highest multiple of ten that fits
; into the input number. (Actually one greater than the highest, so the first
; thing we do is divide it by ten.)
;
; At the end of each loop the result of dividing the multiple of ten into the
; input number is multiplied by 10 again and subtracted from the number for the
; next loop, e.g. given 32, the multiple of ten starts at 10. 32 / 10 = 3.
; 32 - 3 * 10 = 2. The next multiple of ten is 1, so the number of ones are left
; alone for the next loop.
;
DISPLAY_LOOP_DESC
	AND R1, R1, #0
	ADD R1, R1, R2	; Here R1 is current multiple of ten we're looking at
DISPLAY_LOOP_DESC_AGAIN
	AND R2, R2, #0
	ADD R2, R2, #10	; And R2 is used by DIV, 10 as we're moving down
	JSR DIV		; Divide our multiple of ten by ten
	ADD R3, R3, #-1	; One less power of ten
	AND R2, R2, #0
	ADD R2, R2, R1	; Now we have a multiple of ten in R2 (divisor)
	AND R1, R1, #0
	ADD R1, R1, R5	; So we get our input number in R1 (dividend)
	JSR DIV		; And see how many times the one fits in the other
; Here is where we actually display something
	JSR DISPLAY_WORD
	JSR MUL		; Multiply power of ten by result of integer division
	NOT R1, R1
	ADD R1, R1, #1	; And negate result
	ADD R5, R5, R1	; And subtract it from input number
	ADD R3, R3, #0	; If power of ten is zero
	BRz DISPLAY_END	; then we've output the last digit
	AND R1, R1, #0
	ADD R1, R1, R2	; Put multiple of ten in R1 for beginning of loop
	BRnzp DISPLAY_LOOP_DESC_AGAIN
DISPLAY_END
	LD R0, DISP_R0
	LD R1, DISP_R1
	LD R2, DISP_R2
	LD R3, DISP_R3
	LD R4, DISP_R4
	LD R5, DISP_R5
	LD R7, DISP_R7
	RET
DISP_NEG_WORD	.STRINGZ "negative "
DISP_NEG	.FILL #45	; Negative sign
DISP_0		.FILL #48	; ASCII 0
DISP_R0		.FILL 0
DISP_R1		.FILL 0
DISP_R2		.FILL 0
DISP_R3		.FILL 0
DISP_R4		.FILL 0
DISP_R5		.FILL 0
DISP_R7		.FILL 0

;
; Simply displays the word "zero". Zero is a simple case and it can mess up
; other algorithms so it gets its own place.
;
DISPLAY_ZERO
	ST R0, ZERO_R0
	ST R7, ZERO_R7
	LD R0, PRE_00_P	; Grab the zero string
	PUTS		; Print it
	AND R0, R0, #0
	ADD R0, R0, #10	; And a newline
	OUT
	LD R0, ZERO_R0
	LD R7, ZERO_R7
	RET
ZERO_R0	.FILL 0
ZERO_R7 .FILL 0

;
; Displays the English word for the given number at the given location.
; This routine is only responsible for one digit of the word.
;
; Preconditions: Assumes state is the same as in DISPLAY.
;                -R1 is the plurality of the current multiple of ten (e.g. given
;                the number 32 and ten to the first, R1 will be 3).
;                -R3 holds the actual power of ten. 1 in the last examle.
; Postconditions: The words for this digit will have been displayed.
;                 No registers are overwritten.
;
; The power of ten is used as an index into the postfix array. The prefix
; index is the digit + 10 * the power of ten.
;
; In the case of teen numbers, it is known that the number lies in the teen
; range when a one in the tens place is seen, but it is not known which teen it
; is until the ones place is seen so a flag is set and control passes back to
; DISPLAY. On the next call, it is seen that this ones place should be
; printed as a teen number.
;
DISPLAY_WORD
	ST R0, WORD_R0
	ST R1, WORD_R1
	ST R2, WORD_R2
	ST R3, WORD_R3
	ST R4, WORD_R4
	ST R5, WORD_R5
	ST R6, WORD_R6
	ST R7, WORD_R7
	ADD R5, R1, #0
	BRz DISPLAY_WORD_CHECK_TEN	; No display if 0, unless it's 10
	BRnzp DISPLAY_WORD_NOT_ZERO
DISPLAY_WORD_CHECK_TEN
	LD R5, WORD_TEEN
	BRzp DISPLAY_WORD_EARLY_EXIT	; If flag non-negative, not ten, no show
DISPLAY_WORD_NOT_ZERO	; Our digit is confirmed non-zero (or ten)
	ADD R5, R3, #0	; Check the power of ten to see how to procede
	Brz DISPLAY_WORD_ONES
	ADD R5, R3, #-1
	BRz DISPLAY_WORD_TENS
	ADD R5, R3, #-2
	BRz DISPLAY_WORD_HUNDREDS
	ADD R5, R3, #-3
	BRz DISPLAY_WORD_THOUSANDS
	BRnzp DISPLAY_WORD_BAD
DISPLAY_WORD_ONES	; This digit is in the ones place
	LD R5, WORD_TEEN	; Check teen flag
	BRn DISPLAY_WORD_TEEN	; And special display if so
	LEA R0, PRE_00_P
	ADD R0, R0, R1
	LDR R0, R0, #0
	PUTS
	BRnzp DISPLAY_WORD_CLEANUP
DISPLAY_WORD_TENS	; This digit is in the tens place
	ADD R5, R1, #-1	; If power is 1 and plurality is 1, then it's a teen
	BRnp DISPLAY_WORD_NON_TEEN
	AND R5, R5, #0
	NOT R5, R5
	ST R5, WORD_TEEN		; Set teen flag
	BRnzp DISPLAY_WORD_EARLY_EXIT	; Handle this next time
DISPLAY_WORD_NON_TEEN	; Tens place > 1
	ADD R5, R1, #-2	; Check if we're dealing with twenty.
	BRnp DISPLAY_WORD_CHECK_FOR
	LEA R0, PRE_Y2_P	; Load special case prefix "twen"
	BRnzp DISPLAY_WORD_TEN_GO
DISPLAY_WORD_TEEN	; We're at ones place, but tens place was 1
	AND R5, R5, #0	; So teen it the hell up!
	ST R5, WORD_TEEN	; Unset negative flag
	LEA R0, PRE_00_P
	ADD R0, R0, #10
	ADD R0, R0, R1	; Our teen prefix
	LDR R0, R0, #0
	PUTS
	ADD R5, R1, #0	; Ten has no postfix
	BRz DISPLAY_WORD_CLEANUP
	ADD R5, R1, #-1	; Eleven has no postfix
	BRz DISPLAY_WORD_CLEANUP
	ADD R5, R1, #-2	; Twelve has no postfix either (I'm done now)
	BRz DISPLAY_WORD_CLEANUP
	LEA R0, POS_0_P	; Our teen postfix
	LDR R0, R0, #0
	PUTS
	BRnzp DISPLAY_WORD_CLEANUP
DISPLAY_WORD_CHECK_FOR	; Check if we're dealing with forty.
	ADD R5, R1, #-4
	BRnp DISPLAY_WORD_TEN_NORMAL
	LEA R0, PRE_Y4_P
	BRnzp DISPLAY_WORD_TEN_GO
DISPLAY_WORD_TEN_NORMAL	; Assume same prefix as teens
	LEA R0, PRE_00_P
	ADD R0, R0, #10
	ADD R0, R0, R1
DISPLAY_WORD_TEN_GO	; R0 holds address where pointer is stored
	LDR R0, R0, #0	; LDR the actual pointed to address (prefix string)
	PUTS		; And print it
	LEA R0, POS_1_P
	LDR R0, R0, #0
	PUTS
	BRnzp DISPLAY_WORD_CLEANUP
DISPLAY_WORD_HUNDREDS	; This digit is in the hundreds place
	LEA R0, PRE_00_P
	ADD R0, R0, R1
	LDR R0, R0, #0
	PUTS
	LEA R0, POS_2_P
	LDR R0, R0, #0
	PUTS
	BRnzp DISPLAY_WORD_CLEANUP
DISPLAY_WORD_THOUSANDS	; This digit is in the thousands place
	LEA R0, PRE_00_P
	ADD R0, R0, R1
	LDR R0, R0, #0
	PUTS
	LEA R0, POS_3_P
	LDR R0, R0, #0
	PUTS
	BRnzp DISPLAY_WORD_CLEANUP
DISPLAY_WORD_BAD	; This is not a displayable number
	LEA R0, WORD_BAD
	PUTS
DISPLAY_WORD_CLEANUP
	LD R0, WORD_SPC	; Print a space after our digit
	OUT
DISPLAY_WORD_EARLY_EXIT	; Don't print a space before exiting
	LD R0, WORD_R0
	LD R1, WORD_R1
	LD R2, WORD_R2
	LD R3, WORD_R3
	LD R4, WORD_R4
	LD R5, WORD_R5
	LD R6, WORD_R6
	LD R7, WORD_R7
	RET
WORD_BAD	.STRINGZ "This is not a valid number.\n"
WORD_SPC	.FILL #32
WORD_TEEN	.FILL 0	; If this is set, then display a teen
WORD_R0		.FILL 0
WORD_R1		.FILL 0
WORD_R2		.FILL 0
WORD_R3		.FILL 0
WORD_R4		.FILL 0
WORD_R5		.FILL 0
WORD_R6		.FILL 0
WORD_R7		.FILL 0

;
; Multiplies two integers.
;
; Preconditions: The number in R1 is multiplied with the number in R2.
; Postconditions: The number in R1 will be the product.
;
MUL	ST R0, MUL_R0
	ST R2, MUL_R2
	ST R3, MUL_R3
	AND R3, R3, #0	; R3 holds flag for negative
	ADD R1, R1, #0
	BRn MUL_NEG_1	; If operand 1 is negative, flip flag
MUL_CHECK_NEG_2
	ADD R2, R2, #0
	BRn MUL_NEG_2	; And if operand 2 is negative
MUL_POST_CHECK_NEG	; Now we know our arguments are positive
	AND R0, R0, #0	; R0 holds original number (absolute value)
	ADD R0, R0, R1
	AND R1, R1, #0	; R1 to 0 so adding R0 R2 times gives correct result
	BRnzp MUL_LOOP
MUL_NEG_1 ; First operand is negative
	NOT R3, R3	; Negative flag is negative when answer is negative
	NOT R1, R1	; Negate operand 1 (both numbers must be positive)
	ADD R1, R1, #1
	BRnzp MUL_CHECK_NEG_2
MUL_NEG_2 ; Second operand is negative
	NOT R3, R3
	NOT R2, R2	; Negate operand 2
	ADD R2, R2, #1
	BRnzp MUL_POST_CHECK_NEG
MUL_LOOP
	ADD R2, R2, #-1
	BRn MUL_POST_LOOP
	ADD R1, R1, R0	; Add R1 to itself (original saved in R0) R2 times
	BRnzp MUL_LOOP
MUL_POST_LOOP
	ADD R3, R3, #0
	BRzp MUL_CLEANUP	; If negative flag not set
	NOT R1, R1		; If it is, negate answer
	ADD R1, R1, #1
MUL_CLEANUP
	LD R0, MUL_R0
	LD R2, MUL_R2
	LD R3, MUL_R3
	RET
MUL_R0	.FILL 0
MUL_R2	.FILL 0
MUL_R3	.FILL 0

;
; Divides two integers.
;
; Preconditions: R1 is the dividen, R2 the divisor.
; Postconditions: R1 holds the quotient.
;
DIV	ST R0, DIV_R0
	ST R2, DIV_R2
	ST R3, DIV_R3
	AND R0, R0, #0	; R0 holds our quotient
	AND R3, R3, #0	; R3 holds negative flag
	ADD R1, R1, #0
	BRn DIV_NEG_1	; If first argument is negative flip flag
DIV_CHECK_NEG_2
	ADD R2, R2, #0
	Brn DIV_NEG_2	; Or the second
DIV_POST_CHECK_NEG
	NOT R2, R2
	ADD R2, R2, #1	; R2 (divisor) negated for repeated subtraction
	BRnzp DIV_LOOP
DIV_NEG_1 ; First operand is negative
	NOT R1, R1	; Negate first argument (both must be positive)
	ADD R1, R1, #1
	NOT R3, R3
	BRnzp DIV_CHECK_NEG_2
DIV_NEG_2 ; Second operand is negative
	NOT R2, R2	; Negate second argument
	ADD R2, R2, #1
	NOT R3, R3
	BRnzp DIV_POST_CHECK_NEG
DIV_LOOP
	ADD R1, R1, R2
	BRn DIV_CLEANUP	; We've subtracted once too many
	ADD R0, R0, #1	; Number of times it fits in goes up
	BRnzp DIV_LOOP
DIV_CLEANUP
	ADD R3, R3, #0
	BRzp DIV_NOT_NEG
	NOT R0, R0	; Here we know the output is negative, so negate it!
	ADD R0, R0, #1
DIV_NOT_NEG
	AND R1, R1, #0
	ADD R1, R1, R0	; Put our result in R1 for return
	LD R0, DIV_R0
	LD R2, DIV_R2
	LD R3, DIV_R3
	ADD R1, R1, #0	; Set condition codes for calling routine
	RET
DIV_R0	.FILL 0
DIV_R2	.FILL 0
DIV_R3	.FILL 0

;
; Divides two unsigned integers.
;
; Preconditions: R1 is the dividen, R2 the divisor.
; Postconditions: R1 holds the quotient.
;
; THIS ROUTINE IS DIRECTLY DERIVED FROM DIV ABOVE. THE ONLY DIFFERENCE IS THE
; LOGIC FOR HANDLING NEGATIVE INPUTS IS REMOVED.
;
UDIV	ST R0, UDIV_R0
	ST R2, UDIV_R2
	AND R0, R0, #0	; R0 holds our quotient
	ADD R1, R1, #0
	BRn UDIV_NEG_1	; If first argument is negative flip flag
UDIV_CHECK_NEG_2
	ADD R2, R2, #0
	Brn UDIV_NEG_2	; Or the second
UDIV_POST_CHECK_NEG
	NOT R2, R2
	ADD R2, R2, #1	; R2 (divisor) negated for repeated subtraction
	BRnzp UDIV_LOOP
UDIV_NEG_1 ; First operand is negative
	NOT R1, R1	; Negate first argument (both must be positive)
	ADD R1, R1, #1
	BRnzp UDIV_CHECK_NEG_2
UDIV_NEG_2 ; Second operand is negative
	NOT R2, R2	; Negate second argument
	ADD R2, R2, #1
	BRnzp UDIV_POST_CHECK_NEG
UDIV_LOOP
	ADD R1, R1, R2
	BRn UDIV_CLEANUP	; We've subtracted once too many
	ADD R0, R0, #1	; Number of times it fits in goes up
	BRnzp UDIV_LOOP
UDIV_CLEANUP
	AND R1, R1, #0
	ADD R1, R1, R0	; Put our result in R1 for return
	LD R0, UDIV_R0
	LD R2, UDIV_R2
	ADD R1, R1, #0	; Set condition codes for calling routine
	RET
UDIV_R0	.FILL 0
UDIV_R2	.FILL 0

;
; Divides two integers, returns remainder.
;
; Preconditions: R1 is the dividen, R2 the divisor.
; Postconditions: R1 holds the remainder.
;
; THIS ROUTINE WAS DIRECTLY DERIVED FROM DIV ABOVE. THE ONLY DIFFERENCE IS
; WHICH NUMBER IS LEFT IN R1 AT THE END OF THE ROUTINE.
;
MOD	ST R0, MOD_R0
	ST R2, MOD_R2
	ST R3, MOD_R3
	AND R0, R0, #0	; R0 holds our quotient
	AND R3, R3, #0	; R3 holds negative flag
	ADD R1, R1, #0
	BRn MOD_NEG_1	; If first argument is negative flip flag
MOD_CHECK_NEG_2
	ADD R2, R2, #0
	Brn MOD_NEG_2	; Or the second
MOD_POST_CHECK_NEG
	NOT R2, R2
	ADD R2, R2, #1	; R2 (divisor) negated for repeated subtraction
	BRnzp MOD_LOOP
MOD_NEG_1 ; First operand is negative
	NOT R1, R1	; Negate first argument (both must be positive)
	ADD R1, R1, #1
	NOT R3, R3
	BRnzp MOD_CHECK_NEG_2
MOD_NEG_2 ; Second operand is negative
	NOT R2, R2	; Negate second argument
	ADD R2, R2, #1
	NOT R3, R3
	BRnzp MOD_POST_CHECK_NEG
MOD_LOOP
	ADD R1, R1, R2
	BRn MOD_CLEANUP	; We've subtracted once too many
	ADD R0, R0, #1	; Number of times it fits in goes up
	BRnzp MOD_LOOP
MOD_CLEANUP
	NOT R2, R2
	ADD R2, R2, #1	; Mod loop does one too many subtractions
	ADD R1, R1, R2	; So do one addition to compensate.
	ADD R3, R3, #0
	BRzp MOD_NOT_NEG
	NOT R0, R0	; Here we know the output is negative, so negate it!
	ADD R0, R0, #1
MOD_NOT_NEG
	LD R0, MOD_R0
	LD R2, MOD_R2
	LD R3, MOD_R3
	ADD R1, R1, #0	; Set condition codes for calling routine
	RET
MOD_R0	.FILL 0
MOD_R2	.FILL 0
MOD_R3	.FILL 0

;
; Takes a 2's complement integer and displays its DECIMAL representation.
;
; THIS FUNCTION IS DIRECTLY DERIVED FROM DISPLAY ABOVE.
; THE ONLY MODIFICATION IS SIMPLIFIED OUTPUT. THE ALGORITHM IS THE SAME.
; BLOCK COMMENTS HAVE BEEN REMOVED FOR REDUNDANCY AND SPACE SAVING.
;
DISPD	ADD R0, R0, #0
	BRnp DISPD_NON_ZERO
	LD R0, DISPD_0
	OUT
	RET
DISPD_NON_ZERO
	ST R0, DISPD_R0
	ST R1, DISPD_R1
	ST R2, DISPD_R2
	ST R3, DISPD_R3
	ST R4, DISPD_R4
	ST R5, DISPD_R5
	ST R7, DISPD_R7
	AND R1, R1, #0	; R1 holds current multiple of ten
	ADD R1, R1, #1
	AND R2, R2, #0	; R2 used by multiply and divide routines
	ADD R2, R2, #1
	AND R3, R3, #0	; R3 holds current power of ten
	AND R4, R4, #0	; R4 holds plurality of current multiple
	AND R5, R5, #0	; R5 holds original number from R0
	ADD R5, R5, R0	; (R0 needed for output)
	BRzp DISPD_LOOP_ASC
	NOT R5, R5
	ADD R5, R5, #1	; Negate to positive
	LD R0, DISPD_NEG	; Input is negative. Display negative sign.
	OUT
DISPD_LOOP_ASC
	AND R1, R1, #0
	ADD R1, R1, R5	; Set R1 to number
	JSR DIV		; How many times does ten*x go into number?
	BRz DISPD_LOOP_DESC	; If zero, then exit loop
	AND R1, R1, #0		; Otherwise, keep multiplying
	ADD R1, R1, #10
	JSR MUL		; Highest multiple of ten up by one
	ADD R3, R3, #1	; One more power of ten
	AND R2, R2, #0
	ADD R2, R2, R1	; Store in R2 for next loop
	BRnzp DISPD_LOOP_ASC
DISPD_LOOP_DESC
	AND R1, R1, #0
	ADD R1, R1, R2	; Here R1 is current multiple of ten we're looking at
	LD R4, DISPD_0
DISPD_LOOP_DESC_AGAIN
	AND R2, R2, #0
	ADD R2, R2, #10	; And R2 is used by DIV, 10 as we're moving down
	JSR DIV		; Divide our multiple of ten by ten
	ADD R3, R3, #-1	; One less power of ten
	AND R2, R2, #0
	ADD R2, R2, R1	; Now we have a multiple of ten in R2 (divisor)
	AND R1, R1, #0
	ADD R1, R1, R5	; So we get our input number in R1 (dividend)
	JSR DIV		; And see how many times the one fits in the other
; Here is where we actually display something
	ADD R0, R1, R4
	OUT
	JSR MUL		; Multiply power of ten by result of integer division
	NOT R1, R1
	ADD R1, R1, #1	; And negate result
	ADD R5, R5, R1	; And subtract it from input number
	ADD R3, R3, #0	; If power of ten is zero
	BRz DISPD_END	; then we've output the last digit
	AND R1, R1, #0
	ADD R1, R1, R2	; Put multiple of ten in R1 for beginning of loop
	BRnzp DISPD_LOOP_DESC_AGAIN
DISPD_END
	LD R0, DISPD_R0
	LD R1, DISPD_R1
	LD R2, DISPD_R2
	LD R3, DISPD_R3
	LD R4, DISPD_R4
	LD R5, DISPD_R5
	LD R7, DISPD_R7
	RET
DISPD_NEG	.FILL #45	; Negative sign
DISPD_0		.FILL #48	; ASCII 0
DISPD_R0	.FILL 0
DISPD_R1	.FILL 0
DISPD_R2	.FILL 0
DISPD_R3	.FILL 0
DISPD_R4	.FILL 0
DISPD_R5	.FILL 0
DISPD_R7	.FILL 0

;
; Takes a 2's complement integer and displays its HEXADECIMAL representation.
;
; THIS FUNCTION IS DIRECTLY DERIVED FROM DISPLAY ABOVE.
; THE ONLY MODIFICATION IS SIMPLIFIED OUTPUT. THE ALGORITHM IS THE SAME.
; BLOCK COMMENTS HAVE BEEN REMOVED FOR REDUNDANCY AND SPACE SAVING.
;
DISPH	ADD R0, R0, #0
	BRnp DISPH_NON_ZERO
	LD R0, DISPH_0
	OUT
	RET
DISPH_NON_ZERO
	ST R0, DISPH_R0
	ST R1, DISPH_R1
	ST R2, DISPH_R2
	ST R3, DISPH_R3
	ST R4, DISPH_R4
	ST R5, DISPH_R5
	ST R7, DISPH_R7
	AND R1, R1, #0	; R1 holds current multiple of sixteen
	ADD R1, R1, #1
	AND R2, R2, #0	; R2 used by multiply and divide routines
	ADD R2, R2, #1
	AND R3, R3, #0	; R3 holds current power of sixteen
	AND R4, R4, #0	; R4 holds plurality of current multiple
	AND R5, R5, #0	; R5 holds original number from R0
	ADD R5, R5, R0	; (R0 needed for output)
	BRzp DISPH_LOOP_ASC
	NOT R5, R5
	ADD R5, R5, #1	; Negate to positive
	LD R0, DISPH_NEG	; Input is negative. Display negative sign.
	OUT
DISPH_LOOP_ASC
	AND R1, R1, #0
	ADD R1, R1, R5	; Set R1 to number
	JSR DIV		; How many times does ten*x go into number?
	ADD R1, R1, #-16	; Can't fit into last digit > 15 times
	BRn DISPH_LOOP_DESC	; If it goes in less, exit loop
	AND R1, R1, #0		; Otherwise, keep multiplying
	ADD R1, R1, #15
	ADD R1, R1, #1	; 16 doesn't fit in immediate
	JSR MUL		; Highest multiple of sixteen up by one
	ADD R3, R3, #1	; One more power of sixteen
	AND R2, R2, #0
	ADD R2, R2, R1	; Store in R2 for next loop
	BRnzp DISPH_LOOP_ASC
DISPH_LOOP_DESC
	AND R1, R1, #0
	ADD R1, R1, R2	; Here R1 is current multiple of ten we're looking at
DISPH_LOOP_DESC_AGAIN
	AND R2, R2, #0
	ADD R2, R2, R1	; Now we have a multiple of 16 in R2 (divisor)
	AND R1, R1, #0
	ADD R1, R1, R5	; So we get our input number in R1 (dividend)
	JSR DIV		; And see how many times the one fits in the other
; Here is where we actually display something
	ADD R4, R1, #-10	; Is digit greater than 9?
	BRn DISPH_NUM
	LD R4, DISPH_A	; Digit is alphabetic, added to 'A'
	BRnzp DISPH_OUT
DISPH_NUM		; Digit is numeric (< 10)
	LD R4, DISPH_0	; Digit is added to 0
DISPH_OUT
	ADD R0, R1, R4
	OUT
	JSR MUL		; Multiply power of 16 by plurality
	NOT R1, R1
	ADD R1, R1, #1	; And negate result
	ADD R5, R5, R1	; And subtract it from input number
	ADD R3, R3, #0	; If power of ten is zero
	BRz DISPH_END	; then we've output the last digit
	AND R1, R1, #0
	ADD R1, R1, R2	; Put multiple of ten in R1 for beginning of loop
	
	AND R2, R2, #0
	ADD R2, R2, #15	; And R2 is used by DIV, 16 as we're moving down
	ADD R2, R2, #1	; 16 doesn't fit in single immediate
	JSR DIV		; Divide our multiple of 16 by 16
	ADD R3, R3, #-1	; One less power of 16
	
	BRnzp DISPH_LOOP_DESC_AGAIN
DISPH_END
	LD R0, DISPH_R0
	LD R1, DISPH_R1
	LD R2, DISPH_R2
	LD R3, DISPH_R3
	LD R4, DISPH_R4
	LD R5, DISPH_R5
	LD R7, DISPH_R7
	RET
DISPH_NEG	.FILL #45	; Negative sign
DISPH_0		.FILL #48	; ASCII 0
DISPH_A		.FILL #55	; ASCII A - 10. That way if digit == 10
DISPH_R0	.FILL 0		; then digit + DISPH_A == A
DISPH_R1	.FILL 0
DISPH_R2	.FILL 0
DISPH_R3	.FILL 0
DISPH_R4	.FILL 0
DISPH_R5	.FILL 0
DISPH_R7	.FILL 0

;
; Takes a 2's complement integer in R0 and displays its bits.
;
; Preconditions: R0 holds the number you wish to display
; Postconditions: None. No registers modified.
;
DSPB2	ST R0, DSPB2_R0
	ST R1, DSPB2_R1
	ST R2, DSPB2_R2
	ST R3, DSPB2_R3
	ST R4, DSPB2_R4
	ST R5, DSPB2_R5
	ST R7, DSPB2_R7
	LD R1, DSPB2_MASK	; R1 holds bitmask
	AND R2, R2, #0
	ADD R2, R2, #2		; R2 holds two, so divide can right-shift
	AND R3, R3, #0
	ADD R3, R3, R0		; R3 holds input because R0 needed for I/O
	LD R4, DSPB2_0		; R4 holds ASCII 0
	AND R5, R5, #0
	ADD R5, R5, R4
	ADD R5, R5, #1		; R5 holds ASCII 1
DSPB2_LOOP
	AND R2, R1, R3
	BRz DSPB2_ZERO
DSPB2_ONE	; We found a 1 bit
	AND R0, R0, #0
	ADD R0, R0, R5
	OUT	; Show 1
	BRnzp DSPB2_END_LOOP
DSPB2_ZERO	; We found a 0 bit
	AND R0, R0, #0
	ADD R0, R0, R4
	OUT	; Show 0
	BRnzp DSPB2_END_LOOP
DSPB2_END_LOOP
	AND R2, R2, #0
	ADD R2, R2, #2
	JSR UDIV	; Divide mask by two (right shift)
	BRz DSPB2_CLEANUP
	BRnzp DSPB2_LOOP
DSPB2_CLEANUP
	LD R0, DSPB2_R0
	LD R1, DSPB2_R1
	LD R2, DSPB2_R2
	LD R3, DSPB2_R3
	LD R4, DSPB2_R4
	LD R5, DSPB2_R5
	LD R7, DSPB2_R7
	RET
DSPB2_MASK	.FILL x8000
DSPB2_0		.FILL #48
DSPB2_R0	.FILL 0
DSPB2_R1	.FILL 0
DSPB2_R2	.FILL 0
DSPB2_R3	.FILL 0
DSPB2_R4	.FILL 0
DSPB2_R5	.FILL 0
DSPB2_R7	.FILL 0

;
; Takes a 2's complement integer and displays its BINARY representation.
;
; THIS FUNCTION IS DIRECTLY DERIVED FROM DISPLAY ABOVE.
; THE ONLY MODIFICATION IS SIMPLIFIED OUTPUT. THE ALGORITHM IS THE SAME.
; BLOCK COMMENTS HAVE BEEN REMOVED FOR REDUNDANCY AND SPACE SAVING.
;
DISPB	ADD R0, R0, #0
	BRnp DISPB_NON_ZERO
	LD R0, DISPB_0
	OUT
	RET
DISPB_NON_ZERO
	ST R0, DISPB_R0
	ST R1, DISPB_R1
	ST R2, DISPB_R2
	ST R3, DISPB_R3
	ST R4, DISPB_R4
	ST R5, DISPB_R5
	ST R7, DISPB_R7
	AND R1, R1, #0	; R1 holds current multiple of ten
	ADD R1, R1, #1
	AND R2, R2, #0	; R2 used by multiply and divide routines
	ADD R2, R2, #1
	AND R3, R3, #0	; R3 holds current power of ten
	AND R4, R4, #0	; R4 holds plurality of current multiple
	AND R5, R5, #0	; R5 holds original number from R0
	ADD R5, R5, R0	; (R0 needed for output)
	BRzp DISPB_LOOP_ASC
	NOT R5, R5
	ADD R5, R5, #1	; Negate to positive
	LD R0, DISPB_NEG	; Input is negative. Display negative sign.
	OUT
DISPB_LOOP_ASC
	AND R1, R1, #0
	ADD R1, R1, R5	; Set R1 to number
	JSR DIV		; How many times does ten*x go into number?
	BRz DISPB_LOOP_DESC	; If zero, then exit loop
	AND R1, R1, #0		; Otherwise, keep multiplying
	ADD R1, R1, #2
	JSR MUL		; Highest multiple of ten up by one
	ADD R3, R3, #1	; One more power of ten
	AND R2, R2, #0
	ADD R2, R2, R1	; Store in R2 for next loop
	BRnzp DISPB_LOOP_ASC
DISPB_LOOP_DESC
	AND R1, R1, #0
	ADD R1, R1, R2	; Here R1 is current multiple of ten we're looking at
	LD R4, DISPH_0
DISPB_LOOP_DESC_AGAIN
	AND R2, R2, #0
	ADD R2, R2, #2	; And R2 is used by DIV, 10 as we're moving down
	JSR DIV		; Divide our multiple of ten by ten
	ADD R3, R3, #-1	; One less power of ten
	AND R2, R2, #0
	ADD R2, R2, R1	; Now we have a multiple of ten in R2 (divisor)
	AND R1, R1, #0
	ADD R1, R1, R5	; So we get our input number in R1 (dividend)
	JSR DIV		; And see how many times the one fits in the other
; Here is where we actually display something
	ADD R0, R1, R4
	OUT
	JSR MUL		; Multiply power of ten by result of integer division
	NOT R1, R1
	ADD R1, R1, #1	; And negate result
	ADD R5, R5, R1	; And subtract it from input number
	ADD R3, R3, #0	; If power of ten is zero
	BRz DISPB_END	; then we've output the last digit
	AND R1, R1, #0
	ADD R1, R1, R2	; Put multiple of ten in R1 for beginning of loop
	BRnzp DISPB_LOOP_DESC_AGAIN
DISPB_END
	LD R0, DISPB_R0
	LD R1, DISPB_R1
	LD R2, DISPB_R2
	LD R3, DISPB_R3
	LD R4, DISPB_R4
	LD R5, DISPB_R5
	LD R7, DISPB_R7
	RET
DISPB_NEG	.FILL #45	; Negative sign
DISPB_0		.FILL #48	; ASCII 0
DISPB_R0	.FILL 0
DISPB_R1	.FILL 0
DISPB_R2	.FILL 0
DISPB_R3	.FILL 0
DISPB_R4	.FILL 0
DISPB_R5	.FILL 0
DISPB_R7	.FILL 0

	.END
