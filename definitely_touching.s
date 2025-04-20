; ALCATRAZ AND SPREADPOINT
; DEFINITELY TOUCHING (AMIGA, PAL, OCS, 68000, 512 KB)
; (C) 2025 DEPECHE AND VIRGILL

; Build with vasm
; vasmm68k_mot -kick1hunks -Fhunkexe -o definitely_touching -nosym definitely_touching.s

; See
; https://www.youtube.com/watch?v=0ZONMNUKTfU
; https://www.baeldung.com/cs/marching-squares

AbsExecBase	equ	4
OldOpenLibrary	equ	-408
CloseLibrary	equ	-414
Write		equ	-48
Output		equ	-60
AvailMem	equ	-216
AllocMem	equ	-198
FreeMem		equ	-210
TypeOfMem	equ	-534
WaitTOF		equ	-270
Forbid		equ	-132
Permit		equ	-138
LoadView	equ	-222

custom		equ	$dff000

pwidth		equ	40

nummetaballs	equ	3 ; must not be changed
maxglitches	equ	8

pheight		equ	256 ; px

psize		equ	pheight*pwidth
invisheight	equ	24 ; height of clipping zone top and bottom (and middle)
invissize	equ	invisheight*pwidth

; profiling
numbers		equ	0

availablemem	equ	0 ; only in combination with numbers
animlen		equ	0 ; only in combination with numbers
activeglitches	equ	0 ; only in combination with numbers
timing		equ	0 ; does not work correctly

; DMACON
SET		equ	1<<15		; 0=clear, 1=set bits that are set to 1 below
BLTPRI		equ	1<<10		; Blitter DMA priority (over CPU) "blitter nasty"
DMAEN		equ	1<<9		; Enable all DMA below
BPLEN		equ	1<<8		; Bitplane DMA
COPEN		equ	1<<7		; Copper DMA
BLTEN		equ	1<<6		; Blitter DMA
SPREN		equ	1<<5		; Sprite DMA


*------	MACROS ----------------------------------------------------------------*

; actor bits
actor_draw_screen	equ	0
actor_effect		equ	1

	macro CHECKACTOR
	btst	#\1,v_actors+1(a5)
	endm

	macro STARTACTOR
	bset	#\1,v_actors+1(a5)
	endm

	macro STOPACTOR
	bclr	#\1,v_actors+1(a5)
	endm


*------	ENTRY -----------------------------------------------------------------*

base	movem.l	d0-a6,-(a7)		;
	bsr	allocandinit		;
	bne	.exit			; out of memory error?

	if availablemem
	move.l	AbsExecBase.w,a6	;
	move.l	#MEMF_CHIP,d1		;
	jsr	AvailMem(a6)		;
	move.l	b_vars(pc),a5		;
	move.l	d0,v_number(a5)		; free (available) chip memory
	endif

	move.l	AbsExecBase.w,a6	;
	lea	.gfx(pc),a1		;
	jsr	OldOpenLibrary(a6)	; open gfx library
	move.l	d0,a6			;
	beq	.exit			; could not open gfx library
	move.l 	34(a6),-(a7)		; view
	move.l	d0,-(a7)		; gfx base
	move.l 	38(a6),-(a7)		; copper list 1
;	move.l 	50(a6),-(a7)		; copper list 2
	sub.l	a1,a1			;
	jsr	LoadView(a6)		;
	jsr	WaitTOF(a6)		;
	jsr	WaitTOF(a6)		;
	move.l	AbsExecBase.w,a6	;
	jsr	Forbid(a6)		;
	
	lea	custom,a6		;

	move.w	$02(a6),-(a7)		; store DMA control
	move.w	$1c(a6),-(a7)		; store interrupt enable bits
	move.l	$6c.w,-(a7)		; store irq3

	bsr	halt			;

	moveq	#0,d0			; color
	moveq	#32-1,d7		;
	lea	$180(a6),a0		;
.black	move.w	d0,(a0)+		;
	dbf	d7,.black		;		

	clr.w	-(a7)			; store LED state
	btst	#1,$bfe001		;
	beq	.ledstate		;
	not.w	(a7)			;
.ledstate
	bset	#1,$bfe001		; LED dark

*------	START -----------------------------------------------------------------*

	lea	irq3(pc),a0		;
	move.l	a0,$6c.w		;

	move.w	#SET+DMAEN+BPLEN+BLTEN+COPEN+SPREN,$96(a6) ;
	move.l	b_clist1(pc),$80(a6)	;
	tst.w	$88(a6)			;

	move.l	b_animation(pc),v_animp(a5) ;

	move.w	#numframes-1,d7		;
.gen	move.l	d7,-(a7)		;

	move.l	b_clist1(pc),a0		; update Buddha progress bar
	add.w	#clist1wait-clist1,a0	;
	moveq	#-1,d0			;
	move.w	d7,d6			;
	asr.w	#1,d6			;
	sub.b	d6,d0			;
	move.b	d0,(a0)			;

	bsr	marchingsquares		;

	move.l	(a7)+,d7		;
	dbf	d7,.gen			;

	move.l	v_screenplane(a5),a0	; clear text screen
	move.w	#psize/4-1,d7		;
.cls	clr.l	(a0)+			;
	dbf	d7,.cls			;

	move.l	v_animp(a5),a4		;
	move.l	a4,v_animend(a5)	;
	sub.l	b_animation(pc),a4	;
	move.l	a4,v_animlen(a5)	;

	move.l	b_animation(pc),v_animp(a5) ;

	sub.l	a2,a2			; VBR 68000 (improve for > 68000)
	bsr	LSP_MusicDriver_CIA_Start ;

	move.l	v_clistgen(a5),a0	; semi kosher
	moveq	#-2,d0			;
	move.l	d0,(a0)			;
	move.l	a0,$80(a6)		;

	move.w	#$e020,$9a(a6)		; enable vertb and CIA interrupt

*------	IDLE LOOP -------------------------------------------------------------*

	bsr	backgroundtasks		;
	
*------	RESTORE STATE AND EXIT ------------------------------------------------*
	
	tst.w	(a7)+			; restore state
	bne	.leddark		;
	bclr	#1,$bfe001		; LED bright
.leddark
	bsr	halt			;
	move.b	#$7f,$bfdd00		; stop LSP

	move.l	(a7)+,$6c.w		; 
	move.w	(a7)+,d0		;
	or.w	#$c000,d0		;
	move.w	d0,$9a(a6)		;
	move.w	(a7)+,d0		;
	or.w	#$8000,d0		;
	move.w	d0,$96(a6)		;

;	move.l	(a7)+,$84(a6)		; copper list 2
	move.l	(a7)+,$80(a6)		; copper list 1
	move.l	(a7)+,a6		; gfx base
	move.l	(a7)+,a1		; view
	jsr	LoadView(a6)		;
	jsr	WaitTOF(a6)		;
	jsr	WaitTOF(a6)		;
	move.l	a6,a1			; parameter for CloseLibrary
	move.l	AbsExecBase.w,a6	;
	jsr	CloseLibrary(a6)	; close gfx library
	jsr	Permit(a6)		;

	bsr	dealloc			;
.exit	movem.l	(a7)+,d0-a6		;
	moveq	#0,d0			;
	rts				;

.gfx	dc.b	"graphics.library",0
	even


*------	HALT ------------------------------------------------------------------*

halt	bsr	waitblitter		;
	bsr	waitraster		; avoid flickering (?) and sprite DMA "bug"

	move.w	#$7fff,d0		;
	move.w	d0,$9a(a6)		;
	move.w	d0,$9c(a6)		; delete all interrupt requests
	move.w	d0,$96(a6)		; disable all DMAs

	moveq	#0,d0			; volume to zero
	move.w	d0,$a8(a6)		;
	move.w	d0,$b8(a6)		;
	move.w	d0,$c8(a6)		;
	move.w	d0,$d8(a6)		;
	rts				;


*------	VARS ------------------------------------------------------------------*

; # = do not change order
	rsreset
v_doquit	rs.b	1	; signal quit
v_wait		rs.b	1

v_metaballs	rs.b	nummetaballs*sizeofmetaball

v_buffer	rs.l	1

v_animend	rs.l	1	; end address of animation data
v_animp		rs.l	1	; animation data pointer
v_animlen	rs.l	1	; lenght of animation

v_actors	rs.w	1
v_cmdspointer	rs.l	1

v_clistgen	rs.l	1

v_screenplane	rs.l	1
v_screenid	rs.w	1
v_db1a2a	rs.l	1	; #
v_db1b2b	rs.l	1	; #

v_frame		rs.w	1	; frame counter

	if numbers
v_number	rs.l	1	; test value
v_waitcount	rs.w	1
	endif

v_zero		rs.l	10	; 10 registers with value 0 for cpu cls
v_currentwait	rs.w	1
v_ntscpalset	rs.w	1
v_glitches	rs.b	maxglitches*sizeofglitch
sizeofvars	rs.b	0

	rsreset
metaball_a	rs.w	1	; #
metaball_b	rs.w	1	; #
metaball_da	rs.w	1	; #
metaball_db	rs.w	1	; #
sizeofmetaball	rs.w	0

	rsreset
glitch_effect	rs.w	1
glitch_alive	rs.w	1
glitch_line	rs.w	1
glitch_time	rs.w	1
glitch_ttl	rs.w	1
glitch_picked	rs.w	1
glitch_data1	rs.w	1	; custom data
glitch_data2	rs.w	1	; custom data
glitch_data3	rs.w	1	; custom data

; neon glitch data
glitch_tlindex	rs.w	1	; #
glitch_wait	rs.w	1	; #
glitch_neoncol	rs.l	1	; special data for neon glitch

sizeofglitch	rs.w	0


*------ PRINT NUMBER ----------------------------------------------------------*

; d0.l: number, d1.w: pos
	if numbers
printnumber
	move.l	v_db1a2a(a5),d6		;
	beq	.stop			;
	move.l	d6,a0			;
	add.w	d1,a0			;
 	moveq	#8-1,d7			; 8 digits
.loop	move.w	d0,d1			; number
	and.w	#$000f,d1		; mask digit out
	asl.w	#3,d1			; offset to font data
	lea	.digits(pc,d1.w),a1	;
	move.l	a0,a2			;
	rept 5
	move.b	(a1)+,(a2)		; print digit
	add.w	#pwidth,a2		; next line
	endr
	asr.l	#4,d0			; next digit
	subq.w	#1,a0			; next x position
	dbf	d7,.loop		;
.stop	rts				;

.digits	dc.b	%11111000	; 0
	dc.b	%10001000
	dc.b	%10001000
	dc.b	%10001000
	dc.b	%11111000
	ds.b	3

	dc.b	%00100000	; 1
	dc.b	%00100000
	dc.b	%00100000
	dc.b	%00100000
	dc.b	%00100000
	ds.b	3

	dc.b	%11111000	; 2
	dc.b	%00001000
	dc.b	%11111000
	dc.b	%10000000
	dc.b	%11111000
	ds.b	3

	dc.b	%11111000	; 3
	dc.b	%00001000
	dc.b	%11111000
	dc.b	%00001000
	dc.b	%11111000
	ds.b	3

	dc.b	%10001000	; 4
	dc.b	%10001000
	dc.b	%11111000
	dc.b	%00001000
	dc.b	%00001000
	ds.b	3

	dc.b	%11111000	; 5
	dc.b	%10000000
	dc.b	%11111000
	dc.b	%00001000
	dc.b	%11111000
	ds.b	3

	dc.b	%11111000	; 6
	dc.b	%10000000
	dc.b	%11111000
	dc.b	%10001000
	dc.b	%11111000
	ds.b	3

	dc.b	%11111000	; 7
	dc.b	%00001000
	dc.b	%00010000
	dc.b	%00100000
	dc.b	%00100000
	ds.b	3

	dc.b	%11111000	; 8
	dc.b	%10001000
	dc.b	%11111000
	dc.b	%10001000
	dc.b	%11111000
	ds.b	3

	dc.b	%11111000	; 9
	dc.b	%10001000
	dc.b	%11111000
	dc.b	%00001000
	dc.b	%11111000
	ds.b	3

	dc.b	%11111000	; A
	dc.b	%10001000
	dc.b	%11111000
	dc.b	%10001000
	dc.b	%10001000
	ds.b	3

	dc.b	%11110000	; B
	dc.b	%10001000
	dc.b	%11110000
	dc.b	%10001000
	dc.b	%11110000
	ds.b	3

	dc.b	%11111000	; C
	dc.b	%10000000
	dc.b	%10000000
	dc.b	%10000000
	dc.b	%11111000
	ds.b	3

	dc.b	%11110000	; D
	dc.b	%10001000
	dc.b	%10001000
	dc.b	%10001000
	dc.b	%11110000
	ds.b	3

	dc.b	%11111000	; E
	dc.b	%10000000
	dc.b	%11111000
	dc.b	%10000000
	dc.b	%11111000
	ds.b	3

	dc.b	%11111000	; F
	dc.b	%10000000
	dc.b	%11111000
	dc.b	%10000000
	dc.b	%10000000
	ds.b	3

	endif


*------	WAIT BLITTER ----------------------------------------------------------*

waitblitter
	move.w	#SET+DMAEN+BLTPRI,$96(a6)	;
	btst	#14-8,$02(a6)			; DMAB_BLTDONE = 14
.wait	btst	#14-8,$02(a6)			;
	bne	.wait				;
	move.w	#BLTPRI,$96(a6)			;
	rts					;


*------	WAIT RASTER -----------------------------------------------------------*

waitraster
.wait	move.l	$04(a6),d0			;
	and.l	#$0001ff00,d0			;
	cmp.l	#312<<8,d0			; line to wait for
	bne	.wait				;
	rts					;


*------	BACKGROUND TASKS ------------------------------------------------------*

backgroundtasks
.idle	
	CHECKACTOR actor_draw_screen
	beq	.done				;

	tst.w	v_screenid(a5)			;
	bne	.1				;
	lea	screen0(pc),a1			;
	bra	.draw				;
.1	cmp.w	#1,v_screenid(a5)		;
	bne	.2				;
	lea	screen1(pc),a1			;
	moveq	#s1,d7				; scalar
	bra	.draw				;
.2	cmp.w	#2,v_screenid(a5)		;
	bne	.3				;
	lea	screen2(pc),a1			;
	moveq	#s2,d7				; scalar
	bra	.draw				;
.3	cmp.w	#3,v_screenid(a5)		;
	bne	.4				;
	lea	screen3(pc),a1			;
	moveq	#s3,d7				; scalar
	bra	.draw				;
.4	lea	screen4(pc),a1			;
	moveq	#s4,d7				;
.draw	bsr	drawscreen			;
	STOPACTOR actor_draw_screen

.done	tst.b	v_doquit(a5)			;
	beq	.idle				;
	rts					;


*------	IRQ3 ------------------------------------------------------------------*

irq3	movem.l	d0-a6,-(a7)			;
	move.l	b_vars(pc),a5			;
	lea	custom,a6			;

	lea	clist2bpl+2(pc),a1		;
	move.l	v_db1a2a(a5),d0			; active bitplane
	move.w	d0,4(a1)			;
	swap	d0				;
	move.w	d0,(a1)				;

	move.l	v_clistgen(a5),a4		; a4 must not be scratched
	lea	clist2(pc),a0			; clist head
	moveq	#clist2size/4-1,d7		;
.copy	move.l	(a0)+,(a4)+			;
	dbf	d7,.copy			;

	bsr	glitchmachine			;
	moveq	#-2,d0				; end of clist
	move.l	d0,(a4)				;

	bsr	player				;
	bsr	effect				;

	movem.l	v_db1a2a(a5),d0-d1		; double buffering
	exg	d0,d1				; v_db1a2a <-> v_db1b2b
	movem.l	d0-d1,v_db1a2a(a5)		;

	if timing|numbers
	bsr	waitblitter			;
	endif

	if timing
	move.w	#$0440,$180(a6)			; dark yellow color indicates numbers consumption
	endif

	if numbers
	moveq	#0,d0				; first line
	move.w	v_frame(a5),d0			;
	asl.l	#8,d0				;
	move.b	v_waitcount(a5),d0		;
	asl.l	#8,d0				;
	move.b	v_wait(a5),d0			;
	moveq	#8-1,d1				; pos
	bsr	printnumber			;

	moveq	#-1,d0				; default value for second line

	if animlen
	move.l	v_animlen(a5),d0		;
	endif

	if activeglitches
	moveq	#0,d0				; num active glitches
	lea	v_glitches(a5),a1		;
	moveq	#maxglitches-1,d7		;
.search	tst.w	glitch_alive(a1)		;
	beq	.next				;
	addq.l	#1,d0				;
.next	add.w	#sizeofglitch,a1		;
	dbf	d7,.search			;
	endif

	if availablemem
	move.l	v_number(a5),d0			;
	endif

	move.w	#10*pwidth+8-1,d1		; second line
	bsr	printnumber			;

	endif

	addq.w	#1,v_frame(a5)			; advance frame number

	btst	#6,$bfe001			; left mouse button pressed?
	bne	.noquit				; (do NOT use seq)
	st	v_doquit(a5)			;
.noquit
	if timing
	move.w	#$0030,$180(a6)			; dark green color indicates free capacity
	endif

	moveq	#$0020,d0			; delete vertb request
	move.w	d0,$9c(a6)			; 3 times? https://amycoders.org/tutorials/frametime.html
	move.w	d0,$9c(a6)			;
	move.w	d0,$9c(a6)			;

	movem.l	(a7)+,d0-a6			;
	rte					;


*------	GLITCHES --------------------------------------------------------------*

glitch_color	equ	0<<1
glitch_shift	equ	1<<1
glitch_neon	equ	2<<1
glitch_colorbg	equ	3<<1

glitches
	dc.w	glitchcolor-glitches
	dc.w	glitchshift-glitches
	dc.w	glitchneon-glitches
	dc.w	glitchcolorbg-glitches

glitchmachine
	lea	v_glitches(a5),a1		;
	moveq	#maxglitches-1,d7		;
.init	clr.w	glitch_picked(a1)		;
	add.w	#sizeofglitch,a1		;
	dbf	d7,.init			;

	clr.w	v_currentwait(a5)		; reset
	clr.w	v_ntscpalset(a5)		; reset

.loop	moveq	#0,d2				;
	moveq	#-1,d0				; find "lowest" rasterline
	lea	v_glitches(a5),a1		;
	moveq	#maxglitches-1,d7		;
.pickloop
	tst.w	glitch_alive(a1)		;
	beq	.nextpick			;

	tst.w	glitch_picked(a1)		;
	bne	.nextpick			;

	cmp.w	glitch_line(a1),d0		; read from left to right
	bls	.nextpick			; branch if glitch_line is higher than d0 
	move.l	a1,d2				; remember this glitch (struct)

	move.w	glitch_line(a1),d0		; new lowest line
.nextpick
	add.w	#sizeofglitch,a1		;
	dbf	d7,.pickloop			;

	tst.l	d2				; no glitch to execute found
	beq	.done				;

	move.l	d2,a1				;
	st	glitch_picked(a1)		; mark as picked/executed

	move.w	glitch_line(a1),d0		;
	cmp.w	v_currentwait(a5),d0		;
	bls	.linealreadythru		;

	move.w	glitch_effect(a1),d0		;
	lea	glitches(pc),a2			;
	add.w	(a2,d0.w),a2			;
	jsr	(a2)				; execute glitch
.linealreadythru
	move.w	glitch_ttl(a1),d0		; still alive?
	cmp.w	glitch_time(a1),d0		;
	bne	.alive				;
	clr.w	glitch_alive(a1)		;
.alive	addq.w	#1,glitch_time(a1)		;
	bra	.loop				;
.done	rts					;

glitchcolor
	move.w	glitch_line(a1),d0		;
	bsr	emitglitchline			;
	move.w	#$0192,(a4)+			;
	move.w	glitch_data1(a1),d1		;
	move.w	d1,(a4)+			;
	moveq	#0,d2				;
	tst.w	d1				; black color? change playfield priority in order to hide black text
	beq	.black				;
	moveq	#$0040,d2			;
.black	move.w	#$0104,(a4)+			;
	move.w	d2,(a4)+			;
;	move.l	#$018000f0,(a4)+		;
;	move.l	#$01920000,(a4)+		;
;	move.l	#$018000f0,(a4)+		;
	rts					;

glitchcolorbg
	move.w	glitch_line(a1),d0		;
	bsr	emitglitchline			;
	move.w	#$0180,(a4)+			;
	move.w	glitch_data2(a1),(a4)+		;

	move.w	#$0102,(a4)+			;
	move.w	glitch_data3(a1),(a4)+		;

	move.w	glitch_data1(a1),d1		;
	add.w	d1,d0				;
	bsr	emitglitchline			;

	move.l	#$01800000,(a4)+		;
	move.l	#$01020000,(a4)+		;
	rts					;

glitchshift
	move.w	glitch_line(a1),d0		;
	bsr	emitglitchline			; WAIT 1
	move.w	glitch_data1(a1),d1		;
	move.w	#$0108,(a4)+			;
	move.w	d1,(a4)+			;
	move.w	#$010a,(a4)+			;
	move.w	d1,(a4)+			;

;	move.l	#$01800ff0,(a4)+

	move.w	glitch_data2(a1),d3		;
	beq	.done				; open end

	addq.w	#1,d0				;
	bsr	emitglitchline			; WAIT 2
	move.l	#$01080000,(a4)+		;
	move.l	#$010a0000,(a4)+		;

	add.w	d3,d0				; hint: actually d2+1 see addq.b above
	bsr	emitglitchline			; WAIT 3
	neg.w	d1				;
	move.w	#$0108,(a4)+			;
	move.w	d1,(a4)+			;
	move.w	#$010a,(a4)+			;
	move.w	d1,(a4)+			;

	addq.w	#1,d0				;
	bsr	emitglitchline			; WAIT 4
	move.l	#$01080000,(a4)+		;
	move.l	#$010a0000,(a4)+		;

;	move.l	#$01800000,(a4)+

	move.w	glitch_data3(a1),d2		;
	beq	.done				;
	add.w	d2,glitch_line(a1)		;
.done	rts					;

; example data
; cmd_glitch, glitch_neon, $01, -1, 0,colorbank1-colorbanks, 0,timeline1-timelines, 0,0
glitchneon
	clr.w	glitch_time(a1)			; neon glitch lives until c_done

	tst.w	glitch_wait(a1)			;
	beq	.donotwait			;
	subq.w	#1,glitch_wait(a1)		;
.col	move.w	glitch_line(a1),d0		;
	bsr	emitglitchline			;

	move.l	glitch_neoncol(a1),d2		;
	beq	.notset				; might not be initalized yet
	move.l	d2,(a4)+			; eg $0192cccc
.notset	moveq	#0,d2				;
	tst.w	glitch_neoncol+2(a1)		; black color? change playfield priority in order to hide black text
	beq	.black				;
	moveq	#$0040,d2			;
.black	move.w	#$0104,(a4)+			;
	move.w	d2,(a4)+			;
;	move.l	#$018000f0,(a4)+
	rts					;
.donotwait
	lea	timelines(pc),a2		;
	add.w	glitch_data2(a1),a2		; select timeline
	add.w	glitch_tlindex(a1),a2		;
	addq.w	#1,glitch_tlindex(a1)		;
	moveq	#0,d1				;
	move.b	(a2),d1				;
	bmi	.setwait			; negative? -> set wait

	cmp.w	#c_done,d1			;
	bne	.notdone			;
	clr.w	glitch_alive(a1)		;
	rts					;
	
.notdone
	lea	colorbanks(pc),a2		;
	add.w	glitch_data1(a1),a2		; select colorbank
	move.w	(a2,d1.w),d1			; get color value (bright, dark, darker)
	move.w	#$0192,glitch_neoncol(a1)	; $0192....
	move.w	d1,glitch_neoncol+2(a1)		; $....nnnn
	bra	.col				;
	
.setwait
	neg.b	d1				;
	move.w	d1,glitch_wait(a1)		;
	bra	.col				;


bgcolor	equ	$0000

colorbanks
colorbank1
	dc.w	bgcolor,$05bf,$039d,$017b
colorbank2
	dc.w	bgcolor,$028f,$006d,$004b
colorbankwe
	dc.w	bgcolor,$0059,$0037,$0015
colorbankopen
	dc.w	bgcolor,$017b,$0059,$0037
colorbankthe
	dc.w	bgcolor,$039d,$017b,$0059
colorbankcompo
	dc.w	bgcolor,$05bf,$039d,$017b
	
c_off	equ	0<<1	; off
c_b	equ	1<<1	; bright
c_d	equ	2<<1	; dark
c_dd	equ	3<<1	; darkest
c_done	equ	4<<1

timelines
timeline1
	dc.b	c_b
	dc.b	-80
	dc.b	-80

	dc.b	c_d
	dc.b	-2
	dc.b	c_dd
	dc.b	-2
	dc.b	c_b
	dc.b	-1
	dc.b	c_d
	dc.b	-2
	dc.b	c_off
	dc.b	-10
	dc.b	c_b
	dc.b	-1
	dc.b	c_d
	dc.b	-3
	dc.b	c_dd
	dc.b	-2
	dc.b	c_b
	dc.b	-5
	dc.b	c_b
	dc.b	-1
	dc.b	c_d
	dc.b	-1
	dc.b	c_dd
	dc.b	-1
	dc.b	c_off
	dc.b	c_done

timeline2
	dc.b	c_b
	dc.b	-100
	dc.b	-100

	dc.b	-4
	dc.b	c_b
	dc.b	-1
	dc.b	c_d
	dc.b	-3
	dc.b	c_dd
	dc.b	-2
	dc.b	c_off
	dc.b	-2
	dc.b	c_b
	dc.b	-1
	dc.b	c_off
	dc.b	-3
	dc.b	c_b
	dc.b	-2
	dc.b	c_d
	dc.b	-2
	dc.b	c_dd
	dc.b	-1
	dc.b	c_off
	dc.b	c_done

timelinewe
	dc.b	-1

	dc.b	c_b
	dc.b	-1
	dc.b	c_d
	dc.b	-2
	dc.b	c_dd
	dc.b	-2
	dc.b	c_b
	dc.b	-1
	dc.b	c_d
	dc.b	-2
	dc.b	c_off
	dc.b	-10
	dc.b	c_b
	dc.b	-1
	dc.b	c_d
	dc.b	-3
	dc.b	c_dd
	dc.b	-2
	dc.b	c_b
	dc.b	-5
	dc.b	c_b
	dc.b	-1
	dc.b	c_d
	dc.b	-1
	dc.b	c_dd
	dc.b	-1
	dc.b	c_off

	dc.b	-3
	dc.b	c_d
	dc.b	-2
	dc.b	c_dd
	dc.b	-4
	dc.b	c_b
	dc.b	-2
	dc.b	c_off
	dc.b	-1
	dc.b	c_d
	dc.b	-1
	dc.b	c_dd
	dc.b	-1
	dc.b	c_b

	dc.b	-100
	dc.b	-50
	
	dc.b	c_d
	dc.b	-2
	dc.b	c_dd
	dc.b	-1
	dc.b	c_off
	dc.b	-4
	dc.b	c_b
	dc.b	-1
	dc.b	c_off
	dc.b	-5
	dc.b	c_d
	dc.b	-2
	dc.b	c_b
	dc.b	-4
	dc.b	c_dd
	dc.b	-1
	dc.b	c_off
	dc.b	-100 ; wait for other glitches
	dc.b	c_done

timelineopen
	dc.b	-20

	dc.b	c_b
	dc.b	-1
	dc.b	c_d
	dc.b	-2
	dc.b	c_dd
	dc.b	-2
	dc.b	c_b
	dc.b	-1
	dc.b	c_d
	dc.b	-2
	dc.b	c_off
	dc.b	-10
	dc.b	c_b
	dc.b	-1
	dc.b	c_d
	dc.b	-3
	dc.b	c_dd
	dc.b	-2
	dc.b	c_b
	dc.b	-5
	dc.b	c_b
	dc.b	-1
	dc.b	c_d
	dc.b	-1
	dc.b	c_dd
	dc.b	-1
	dc.b	c_off
	
	dc.b	-3
	dc.b	c_d
	dc.b	-2
	dc.b	c_dd
	dc.b	-4
	dc.b	c_b
	dc.b	-2
	dc.b	c_off
	dc.b	-1
	dc.b	c_d
	dc.b	-1
	dc.b	c_dd
	dc.b	-1
	dc.b	c_b

	dc.b	-100
	dc.b	-30

	dc.b	c_d
	dc.b	-2
	dc.b	c_dd
	dc.b	-1
	dc.b	c_off
	dc.b	-4
	dc.b	c_b
	dc.b	-1
	dc.b	c_off
	dc.b	-5
	dc.b	c_d
	dc.b	-2
	dc.b	c_b
	dc.b	-4
	dc.b	c_dd
	dc.b	-1
	dc.b	c_off
	
	dc.b	-100 ; wait for other glitches
	dc.b	c_done

timelinethe
	dc.b	-30

	dc.b	c_b
	dc.b	-1
	dc.b	c_d
	dc.b	-2
	dc.b	c_dd
	dc.b	-2
	dc.b	c_b
	dc.b	-1
	dc.b	c_d
	dc.b	-2
	dc.b	c_off
	dc.b	-10
	dc.b	c_b
	dc.b	-1
	dc.b	c_d
	dc.b	-3
	dc.b	c_dd
	dc.b	-2
	dc.b	c_b
	dc.b	-5
	dc.b	c_b
	dc.b	-1
	dc.b	c_d
	dc.b	-1
	dc.b	c_dd
	dc.b	-1
	dc.b	c_off
	
	dc.b	-3
	dc.b	c_d
	dc.b	-2
	dc.b	c_dd
	dc.b	-4
	dc.b	c_b
	dc.b	-2
	dc.b	c_off
	dc.b	-1
	dc.b	c_d
	dc.b	-1
	dc.b	c_dd
	dc.b	-1
	dc.b	c_b

	dc.b	-100
	dc.b	-40

	dc.b	c_d
	dc.b	-2
	dc.b	c_dd
	dc.b	-1
	dc.b	c_off
	dc.b	-4
	dc.b	c_b
	dc.b	-1
	dc.b	c_off
	dc.b	-5
	dc.b	c_d
	dc.b	-2
	dc.b	c_b
	dc.b	-4
	dc.b	c_dd
	dc.b	-1
	dc.b	c_off
	
	dc.b	-100 ; wait for other glitches
	dc.b	c_done

timelinecompo
	dc.b	-40

	dc.b	c_b
	dc.b	-1
	dc.b	c_d
	dc.b	-2
	dc.b	c_dd
	dc.b	-2
	dc.b	c_b
	dc.b	-1
	dc.b	c_d
	dc.b	-2
	dc.b	c_off
	dc.b	-10
	dc.b	c_b
	dc.b	-1
	dc.b	c_d
	dc.b	-3
	dc.b	c_dd
	dc.b	-2
	dc.b	c_b
	dc.b	-5
	dc.b	c_b
	dc.b	-1
	dc.b	c_d
	dc.b	-1
	dc.b	c_dd
	dc.b	-1
	dc.b	c_off
	
	dc.b	-3
	dc.b	c_d
	dc.b	-2
	dc.b	c_dd
	dc.b	-4
	dc.b	c_b
	dc.b	-2
	dc.b	c_off
	dc.b	-1
	dc.b	c_d
	dc.b	-1
	dc.b	c_dd
	dc.b	-1
	dc.b	c_b

	dc.b	-100
	dc.b	-50

	dc.b	c_d
	dc.b	-2
	dc.b	c_dd
	dc.b	-1
	dc.b	c_off
	dc.b	-4
	dc.b	c_b
	dc.b	-1
	dc.b	c_off
	dc.b	-4
	dc.b	c_dd
	dc.b	-1
	dc.b	c_d
	dc.b	-2
	dc.b	c_b
	dc.b	-1
	dc.b	c_off
	dc.b	-100 ; wait for other glitches
	dc.b	c_done

timelinealca
timelinespread

	dc.b	c_b
	dc.b	-120

	dc.b	-3
	dc.b	c_d
	dc.b	-2
	dc.b	c_dd
	dc.b	-4
	dc.b	c_b
	dc.b	-2
	dc.b	c_off
	dc.b	-1
	dc.b	c_d
	dc.b	-1
	dc.b	c_dd
	dc.b	-1
	dc.b	c_b

	dc.b	-40
	
	dc.b	c_d
	dc.b	-2
	dc.b	c_dd
	dc.b	-1
	dc.b	c_off
	dc.b	-4
	dc.b	c_b
	dc.b	-1
	dc.b	c_off
	dc.b	-1
	dc.b	c_dd
	dc.b	-2
	dc.b	c_b
	dc.b	-3
	dc.b	c_d
	dc.b	-1
	dc.b	c_dd
	dc.b	-1
	dc.b	c_off

	dc.b	-50
	dc.b	c_done

timelinetraz
	dc.b	c_b
	dc.b	-120

	dc.b	-3
	dc.b	c_d
	dc.b	-2
	dc.b	c_dd
	dc.b	-4
	dc.b	c_b
	dc.b	-2
	dc.b	c_off
	dc.b	-1
	dc.b	c_d
	dc.b	-1
	dc.b	c_dd
	dc.b	-1
	dc.b	c_b

	dc.b	-40

	dc.b	c_d
	dc.b	-2
	dc.b	c_dd
	dc.b	-1
	dc.b	c_off
	dc.b	-4
	dc.b	c_b
	dc.b	-1
	dc.b	c_off
	dc.b	-1
	dc.b	c_d
	dc.b	-2
	dc.b	c_b
	dc.b	-3
	dc.b	c_dd
	dc.b	-1
	dc.b	c_off
	
	dc.b	-50
	dc.b	c_done

timelinepoint
	dc.b	c_b
	dc.b	-120

	dc.b	-3
	dc.b	c_d
	dc.b	-2
	dc.b	c_dd
	dc.b	-4
	dc.b	c_b
	dc.b	-2
	dc.b	c_off
	dc.b	-1
	dc.b	c_d
	dc.b	-1
	dc.b	c_dd
	dc.b	-1
	dc.b	c_b
	dc.b	-2
	dc.b	c_dd
	dc.b	-1
	dc.b	c_off
	dc.b	-4
	dc.b	c_b
	dc.b	-1
	dc.b	c_off
	dc.b	-1
	dc.b	c_b

	dc.b	-1
	dc.b	c_off
	dc.b	-1
	dc.b	c_d
	dc.b	-2
	dc.b	c_b
	dc.b	-3
	dc.b	c_dd
	dc.b	-1
	dc.b	c_d
	dc.b	-2
	dc.b	c_b
	dc.b	-26

	dc.b	c_d
	dc.b	-2
	dc.b	c_b
	dc.b	-3
	dc.b	c_dd
	dc.b	-1
	dc.b	c_off
	
	dc.b	-50
	dc.b	c_done

	even


emitglitchline
	cmp.w	v_currentwait(a5),d0		;
	bls	.done				; TODO was beq (which is wrong?)

	moveq	#$2c,d2				;
	add.w	d0,d2				;
	cmp.w	#$00ff,d2			;
	bls	.toolow				;

	tst.w	v_ntscpalset(a5)		;
	bne	.set				;
	move.l	#$ffdffffe,(a4)+		; ntsc/pal border
	st	v_ntscpalset(a5)		;
.toolow
.set	move.b	d2,(a4)+			;
	move.b	#$07,(a4)+			;
	move.w	#$fffe,(a4)+			;

	move.w	d0,v_currentwait(a5)		;
.done	rts					;


*------	MARCHING SQURES -------------------------------------------------------*

; https://www.baeldung.com/cs/marching-squares

isovalue	equ	32

marchingsquares
	bsr	drawmetaballs			; draw metaballs in buffer

	lea	xoffsets(pc),a0			;
	move.l	v_animp(a5),a4			;
	moveq	#%1111<<1,d4			; const

	moveq	#isovalue,d2			;
	moveq	#0,d3				; contours index/offset
	move.l	d3,a2				; previous position
	move.l	d3,a3				; position

	move.l	v_buffer(a5),a1			;
	moveq	#canvasw-1-1,d7			; x
loopx	moveq	#canvash-1-1,d6			; 
loopy	moveq	#0,d0				; code
	cmp.b	(a1),d2				; a
	bhi	code1				;     MSB  (branch on d2 >= (a1))
	moveq	#8<<1,d0			;      a----b     Hint: Comment -> filled Nice somehow
code1	cmp.b	1(a1),d2			; b    |    |
	bhi	code2				;      |    |
	addq.w	#4<<1,d0			;      d----c
code2	cmp.b	bufferw+1(a1),d2		; c   LSB
	bhi	code3				;
	addq.w	#2<<1,d0			;
code3	cmp.b	bufferw(a1),d2			; d
	bhi	code4				;
	addq.w	#1<<1,d0			;
code4						; d0 = code/offset
	cmp.w	d4,d0				; filter out %1111
	bne	code5				;
	moveq	#0,d0				;
code5	tst.w	d0				; void?
	beq	notset				;

	move.w	a2,d5				; d5 = temp
	move.l	a3,a2				; new previous
	move.w	a3,d1				; d1 = temp
	sub.w	d5,d1				;

	move.w	d1,d5				; d5 = temp
	asr.w	#8,d5				;
	move.b	d5,(a4)+			; pos (upper word)
	move.b	d1,(a4)+			; pos (lower word)
	
	add.b	d3,d0				; add contour bank
	move.b	d0,(a4)+			; draw contour cmd
notset	add.w	#bufferw,a1			; next y pixel in buffer
	add.w	#3*pwidth,a3			; next y in bitplane
	dbf	d6,loopy			;

	sub.w	#(canvash-1)*3*pwidth,a3	; next y in bitplane (fully compensate)
	sub.w	#(canvash-1)*bufferw-1,a1	; Note: The 2nd -1 is advance x in buffer (addq.w #1,a1)

	add.w	(a0)+,a3			; next byte?
	add.w	#onecontour-contours,d3		; next contour (x)
	cmp.w	#contoursend-contours,d3	;
	bne	inrange				;
	moveq	#0,d3				;
	lea	xoffsets(pc),a0			;
inrange	dbf	d7,loopx			;

	clr.b	(a4)+				; void pos
	clr.b	(a4)+				; void pos
	clr.b	(a4)+				; eof cmd (offset = 0)

	move.l	a4,v_animp(a5)			;
eof	rts					; note: eof just needs a rts

drawframe
	move.l	v_db1b2b(a5),a3			;
	lea	contours(pc),a1			; const
	moveq	#0,d3				; upper word must be zero all the time

back	move.b	(a4)+,-(a7)			; pos (upper word), shift up by 8 (stack trick)
	move.w	(a7)+,d0			; (stack pointer is always word aligned)
	move.b	(a4)+,d0			; pos (lower word)
	add.w	d0,a3				; add pos difference

	move.b	(a4)+,d3			;
	move.l	a1,a2				;
	add.w	(a2,d3.w),a2			;
	jmp	(a2)				; execute cmd (will return to "back" or eof)

contours
	dc.w	eof-contours	; end of frame
	dc.w	ca1-contours
	dc.w	ca2-contours
	dc.w	ca3-contours
	dc.w	ca4-contours
	dc.w	ca5-contours
	dc.w	ca6-contours
	dc.w	ca7-contours
	dc.w	ca7-contours	; 8 = 7
	dc.w	ca6-contours	; 9 = 6
	dc.w	ca5-contours	; 10 = 5
	dc.w	ca4-contours	; 11 = 4
	dc.w	ca3-contours	; 12 = 3
	dc.w	ca2-contours	; 13 = 2
	dc.w	ca1-contours	; 14 = 1
	dc.w	0		; not used
onecontour

	dc.w	eof-contours
	dc.w	cb1-contours
	dc.w	cb2-contours
	dc.w	cb3-contours
	dc.w	cb4-contours
	dc.w	cb5-contours
	dc.w	cb6-contours
	dc.w	cb7-contours
	dc.w	cb7-contours
	dc.w	cb6-contours
	dc.w	cb5-contours
	dc.w	cb4-contours
	dc.w	cb3-contours
	dc.w	cb2-contours
	dc.w	cb1-contours
	dc.w	0

	dc.w	eof-contours
	dc.w	cc1-contours
	dc.w	cc2-contours
	dc.w	cc3-contours
	dc.w	cc4-contours
	dc.w	cc5-contours
	dc.w	cc6-contours
	dc.w	cc7-contours
	dc.w	cc7-contours
	dc.w	cc6-contours
	dc.w	cc5-contours
	dc.w	cc4-contours
	dc.w	cc3-contours
	dc.w	cc2-contours
	dc.w	cc1-contours
	dc.w	0

	dc.w	eof-contours
	dc.w	cd1-contours
	dc.w	cd2-contours
	dc.w	cd3-contours
	dc.w	cd4-contours
	dc.w	cd5-contours
	dc.w	cd6-contours
	dc.w	cd7-contours
	dc.w	cd7-contours
	dc.w	cd6-contours
	dc.w	cd5-contours
	dc.w	cd4-contours
	dc.w	cd3-contours
	dc.w	cd2-contours
	dc.w	cd1-contours
	dc.w	0

	dc.w	eof-contours
	dc.w	ce1-contours
	dc.w	ce2-contours
	dc.w	ce3-contours
	dc.w	ce4-contours
	dc.w	ce5-contours
	dc.w	ce6-contours
	dc.w	ce7-contours
	dc.w	ce7-contours
	dc.w	ce6-contours
	dc.w	ce5-contours
	dc.w	ce4-contours
	dc.w	ce3-contours
	dc.w	ce2-contours
	dc.w	ce1-contours
	dc.w	0

	dc.w	eof-contours
	dc.w	cf1-contours
	dc.w	cf2-contours
	dc.w	cf3-contours
	dc.w	cf4-contours
	dc.w	cf5-contours
	dc.w	cf6-contours
	dc.w	cf7-contours
	dc.w	cf7-contours
	dc.w	cf6-contours
	dc.w	cf5-contours
	dc.w	cf4-contours
	dc.w	cf3-contours
	dc.w	cf2-contours
	dc.w	cf1-contours
	dc.w	0

	dc.w	eof-contours
	dc.w	cg1-contours
	dc.w	cg2-contours
	dc.w	cg3-contours
	dc.w	cg4-contours
	dc.w	cg5-contours
	dc.w	cg6-contours
	dc.w	cg7-contours
	dc.w	cg7-contours
	dc.w	cg6-contours
	dc.w	cg5-contours
	dc.w	cg4-contours
	dc.w	cg3-contours
	dc.w	cg2-contours
	dc.w	cg1-contours
	dc.w	0

	dc.w	eof-contours
	dc.w	ch1-contours
	dc.w	ch2-contours
	dc.w	ch3-contours
	dc.w	ch4-contours
	dc.w	ch5-contours
	dc.w	ch6-contours
	dc.w	ch7-contours
	dc.w	ch7-contours
	dc.w	ch6-contours
	dc.w	ch5-contours
	dc.w	ch4-contours
	dc.w	ch3-contours
	dc.w	ch2-contours
	dc.w	ch1-contours
	dc.w	0
contoursend

xoffsets
	dc.w	0,0,1,0,0,1,0,1
xoffsetsend

;     byte      xoffset
; ca +0		0
; cb +0		0
; cc +0		1
; cd +1		0
; ce +1		0
; cf +1		1
; cg +2		0
; ch +2		1


; bits: 765
; 0--0  000
; |\ |  100
; 1--0  010
ca1	;         765
	or.b	#%10000000,pwidth(a3)
	or.b	#%01000000,2*pwidth(a3)
	bra	back

; 0--0  000
; | /|  001
; 0--1  010
ca2	or.b	#%00100000,pwidth(a3)
	or.b	#%01000000,2*pwidth(a3)
	bra	back

; 0--0  000
; |--|  111
; 1--1  000
ca3	or.b	#%11100000,pwidth(a3)
	bra	back

; 0--1  010
; | \|  001
; 0--0  000
ca4	or.b	#%01000000,(a3)
	or.b	#%00100000,pwidth(a3)
	bra	back

; 0--1  010
; |\\|  101
; 1--0  010
ca5	or.b	#%01000000,(a3)
	or.b	#%10100000,pwidth(a3)
	or.b	#%01000000,2*pwidth(a3)
	bra	back

; 0--1  010
; | ||  010
; 0--1  010
ca6	or.b	#%01000000,(a3)
	or.b	#%01000000,pwidth(a3)
	or.b	#%01000000,2*pwidth(a3)
	bra	back

; 0--1  010
; |/ |  100
; 1--1  000
ca7	or.b	#%01000000,(a3)
	or.b	#%10000000,pwidth(a3)
	bra	back


; bits: 432
; 0--0  000
; |\ |  100
; 1--0  010
cb1	;            432
	or.b	#%00010000,pwidth(a3)
	or.b	#%00001000,2*pwidth(a3)
	bra	back

; 0--0  000
; | /|  001
; 0--1  010
cb2	or.b	#%00000100,pwidth(a3)
	or.b	#%00001000,2*pwidth(a3)
	bra	back

; 0--0  000
; |--|  111
; 1--1  000
cb3	or.b	#%00011100,pwidth(a3)
	bra	back

; 0--1  010
; | \|  001
; 0--0  000
cb4	or.b	#%00001000,(a3)
	or.b	#%00000100,pwidth(a3)
	bra	back

; 0--1  010
; |\\|  101
; 1--0  010
cb5	or.b	#%00001000,(a3)
	or.b	#%00010100,pwidth(a3)
	or.b	#%00001000,2*pwidth(a3)
	bra	back

; 0--1  010
; | ||  010
; 0--1  010
cb6	or.b	#%00001000,(a3)
	or.b	#%00001000,pwidth(a3)
	or.b	#%00001000,2*pwidth(a3)
	bra	back

; 0--1  010
; |/ |  100
; 1--1  000
cb7	or.b	#%00001000,(a3)
	or.b	#%00010000,pwidth(a3)
	bra	back


; bits: 107
; 0--0  000
; |\ |  100
; 1--0  010
cc1	;               107
	or.b	#%00000010,pwidth(a3)
	or.b	#%00000001,2*pwidth(a3)
	bra	back

; 0--0  000
; | /|  001
; 0--1  010
cc2	or.b	#%10000000,pwidth+1(a3)
	or.b	#%00000001,2*pwidth(a3)
	bra	back

; 0--0  000
; |--|  111
; 1--1  000
cc3	or.b	#%00000011,pwidth(a3)
	or.b	#%10000000,pwidth+1(a3)
	bra	back

; 0--1  010
; | \|  001
; 0--0  000
cc4	or.b	#%00000001,(a3)
	or.b	#%10000000,pwidth+1(a3)
	bra	back

; 0--1  010
; |\\|  101
; 1--0  010
cc5	or.b	#%00000001,(a3)
	or.b	#%00000010,pwidth(a3)
	or.b	#%10000000,pwidth+1(a3)
	or.b	#%00000001,2*pwidth(a3)
	bra	back

; 0--1  010
; | ||  010
; 0--1  010
cc6	or.b	#%00000001,(a3)
	or.b	#%00000001,pwidth(a3)
	or.b	#%00000001,2*pwidth(a3)
	bra	back

; 0--1  010
; |/ |  100
; 1--1  000
cc7	or.b	#%00000001,(a3)
	or.b	#%00000010,pwidth(a3)
	bra	back


; bits: 654
; 0--0  000
; |\ |  100
; 1--0  010
cd1	;          654
	or.b	#%01000000,pwidth(a3)
	or.b	#%00100000,2*pwidth(a3)
	bra	back

; 0--0  000
; | /|  001
; 0--1  010
cd2	or.b	#%00010000,pwidth(a3)
	or.b	#%00100000,2*pwidth(a3)
	bra	back

; 0--0  000
; |--|  111
; 1--1  000
cd3	or.b	#%01110000,pwidth(a3)
	bra	back

; 0--1  010
; | \|  001
; 0--0  000
cd4	or.b	#%00100000,(a3)
	or.b	#%00010000,pwidth(a3)
	bra	back

; 0--1  010
; |\\|  101
; 1--0  010
cd5	or.b	#%00100000,(a3)
	or.b	#%01010000,pwidth(a3)
	or.b	#%00100000,2*pwidth(a3)
	bra	back

; 0--1  010
; | ||  010
; 0--1  010
cd6	or.b	#%00100000,(a3)
	or.b	#%00100000,pwidth(a3)
	or.b	#%00100000,2*pwidth(a3)
	bra	back

; 0--1  010
; |/ |  100
; 1--1  000
cd7	or.b	#%00100000,(a3)
	or.b	#%01000000,pwidth(a3)
	bra	back


; bits: 321
; 0--0  000
; |\ |  100
; 1--0  010
ce1	;             321
	or.b	#%00001000,pwidth(a3)
	or.b	#%00000100,2*pwidth(a3)
	bra	back

; 0--0  000
; | /|  001
; 0--1  010
ce2	or.b	#%00000010,pwidth(a3)
	or.b	#%00000100,2*pwidth(a3)
	bra	back

; 0--0  000
; |--|  111
; 1--1  000
ce3	or.b	#%00001110,pwidth(a3)
	bra	back

; 0--1  010
; | \|  001
; 0--0  000
ce4	or.b	#%00000100,(a3)
	or.b	#%00000010,pwidth(a3)
	bra	back

; 0--1  010
; |\\|  101
; 1--0  010
ce5	or.b	#%00000100,(a3)
	or.b	#%00001010,pwidth(a3)
	or.b	#%00000100,2*pwidth(a3)
	bra	back

; 0--1  010
; | ||  010
; 0--1  010
ce6	or.b	#%00000100,(a3)
	or.b	#%00000100,pwidth(a3)
	or.b	#%00000100,2*pwidth(a3)
	bra	back

; 0--1  010
; |/ |  100
; 1--1  000
ce7	or.b	#%00000100,(a3)
	or.b	#%00001000,pwidth(a3)
	bra	back


; bits: 076
; 0--0  000
; |\ |  100
; 1--0  010
cf1	;                076
	or.b	#%00000001,pwidth(a3)
	or.b	#%10000000,2*pwidth+1(a3)
	bra	back

; 0--0  000
; | /|  001
; 0--1  010
cf2	or.b	#%01000000,pwidth+1(a3)
	or.b	#%10000000,2*pwidth+1(a3)
	bra	back

; 0--0  000
; |--|  111
; 1--1  000
cf3	or.b	#%00000001,pwidth(a3)
	or.b	#%11000000,pwidth+1(a3)
	bra	back

; 0--1  010
; | \|  001
; 0--0  000
cf4	or.b	#%10000000,1(a3)
	or.b	#%01000000,pwidth+1(a3)
	bra	back

; 0--1  010
; |\\|  101
; 1--0  010
cf5	or.b	#%10000000,1(a3)
	or.b	#%00000001,pwidth(a3)
	or.b	#%01000000,pwidth+1(a3)
	or.b	#%10000000,2*pwidth+1(a3)
	bra	back

; 0--1  010
; | ||  010
; 0--1  010
cf6	or.b	#%10000000,1(a3)
	or.b	#%10000000,pwidth+1(a3)
	or.b	#%10000000,2*pwidth+1(a3)
	bra	back

; 0--1  010
; |/ |  100
; 1--1  000
cf7	or.b	#%10000000,1(a3)
	or.b	#%00000001,pwidth(a3)
	bra	back


; bits: 543
; 0--0  000
; |\ |  100
; 1--0  010
cg1	;           543
	or.b	#%00100000,pwidth(a3)
	or.b	#%00010000,2*pwidth(a3)
	bra	back

; 0--0  000
; | /|  001
; 0--1  010
cg2	or.b	#%00001000,pwidth(a3)
	or.b	#%00010000,2*pwidth(a3)
	bra	back

; 0--0  000
; |--|  111
; 1--1  000
cg3	or.b	#%00111000,pwidth(a3)
	bra	back

; 0--1  010
; | \|  001
; 0--0  000
cg4	or.b	#%00010000,(a3)
	or.b	#%00001000,pwidth(a3)
	bra	back

; 0--1  010
; |\\|  101
; 1--0  010
cg5	or.b	#%00010000,(a3)
	or.b	#%00101000,pwidth(a3)
	or.b	#%00010000,2*pwidth(a3)
	bra	back

; 0--1  010
; | ||  010
; 0--1  010
cg6	or.b	#%00010000,(a3)
	or.b	#%00010000,pwidth(a3)
	or.b	#%00010000,2*pwidth(a3)
	bra	back

; 0--1  010
; |/ |  100
; 1--1  000
cg7	or.b	#%00010000,(a3)
	or.b	#%00100000,pwidth(a3)
	bra	back


; bits: 210
; 0--0  000
; |\ |  100
; 1--0  010
ch1	;              210
	or.b	#%00000100,pwidth(a3)
	or.b	#%00000010,2*pwidth(a3)
	bra	back

; 0--0  000
; | /|  001
; 0--1  010
ch2	or.b	#%00000001,pwidth(a3)
	or.b	#%00000010,2*pwidth(a3)
	bra	back

; 0--0  000
; |--|  111
; 1--1  000
ch3	or.b	#%00000111,pwidth(a3)
	bra	back

; 0--1  010
; | \|  001
; 0--0  000
ch4	or.b	#%00000010,(a3)
	or.b	#%00000001,pwidth(a3)
	bra	back

; 0--1  010
; |\\|  101
; 1--0  010
ch5	or.b	#%00000010,(a3)
	or.b	#%00000101,pwidth(a3)
	or.b	#%00000010,2*pwidth(a3)
	bra	back

; 0--1  010
; | ||  010
; 0--1  010
ch6	or.b	#%00000010,(a3)
	or.b	#%00000010,pwidth(a3)
	or.b	#%00000010,2*pwidth(a3)
	bra	back

; 0--1  010
; |/ |  100
; 1--1  000
ch7	or.b	#%00000010,(a3)
	or.b	#%00000100,pwidth(a3)
	bra	back


*------	METABALLS EFFECT ------------------------------------------------------*

; don't change these values - precalculated sintabs depend on them
bufferw		equ	256	; 256*3=768  Improve: maybe 128 would be enough
bufferh		equ	86	; 86*3=258 (256 required)
canvasw		equ	108	; 108*3=324 (320 required)
canvash		equ	bufferh

; generated data size: $45e4f
; precalc on Amiga 500 OCS 512 KB: 1m16s

numframes	equ	360
buffersafezoneh	equ	30
buffersafezone	equ	buffersafezoneh*bufferw
ballw		equ	59

effect	bsr 	cls				;

	if 0
	move.l	v_db1b2b(a5),a3			; const bitplane
	lea	255*pwidth(a3),a4		;
	moveq	#-1,d0				; pattern
	moveq	#40/4-1,d7			;
.pattern
	move.l	d0,(a3)+			;
	move.l	d0,(a4)+			;
	dbf	d7,.pattern			;
	endif

	CHECKACTOR actor_effect
	beq	.skip				;
	move.l	v_animp(a5),a4			;
	bsr	drawframe			;
	move.l	a4,v_animp(a5)			;
	cmp.l	v_animend(a5),a4		;
	bne	.noteod				;
	move.l	b_animation(pc),v_animp(a5)	;
.noteod	
.skip	rts					;

drawmetaballs
	move.l	v_buffer(a5),a1			; hint: no need to help out with cpu - plenty of time left
	bsr	waitblitter			;
	move.w	#bufferw-canvasw,$66(a6)	; modulo D
	move.l	a1,$54(a6) 			; destination D
	move.l	#$01000000,$40(a6)		; bltcon0 / bltcon1
	move.w	#(bufferh<<6)+(canvasw>>1),$58(a6) ; bltsize and start
	bsr	waitblitter			;

	lea	sintabx(pc),a3			;
	lea	sintaby(pc),a4			;

	lea	v_metaballs(a5),a2		;
	moveq	#nummetaballs-1,d7		;
.loop	move.w	#360<<1,d5			; const
	move.l	v_buffer(a5),a1			;
	movem.w	metaball_a(a2),d0-d3		; a(lpha) b(eta) da db

	add.w	(a3,d0.w),a1			; x positioning
	add.w	d2,d0				;
	cmp.w	d5,d0				;
	bcs	.cx				;
	sub.w	d5,d0				;
.cx	move.w	d0,metaball_a(a2)		;	

	add.w	(a4,d1.w),a1			; y positioning (y is *bufferw)
	add.w	d3,d1				;
	cmp.w	d5,d1				;
	bcs	.cy				;
	sub.w	d5,d1				;
.cy	move.w	d1,metaball_b(a2)		;

	move.l	d7,-(a7)			;
	movem.w	constdata(pc),d0-d7		; saves some bytes in drawmetaballs.s
	include	"drawmetaball.s"		;   but doesn't make it faster
	move.l	(a7)+,d7			;
	addq.w	#sizeofmetaball,a2		; next metaball
	dbf	d7,.loop			;
	rts					;

constdata
	dc.w	bufferw-ballw,10,11,12,13,9,14,20

	include "sintabs.s"


*------ SCREENS ---------------------------------------------------------------*

screen0	dc.w	-1 ; empty screen

; GUTEN ABEND
s1	equ	13

s1x1	equ	(320-24*s1)/2
s1y1	equ	5*s1-28 ; 28px up
s1x2	equ	s1x1
s1y2	equ	s1y1+6*s1

screen1	dc.w	s1x1,s1y1 ; position x y
	dc.w	char_g-chars

	dc.w	s1x1+5*s1,s1y1
	dc.w	char_u-chars

	dc.w	s1x1+10*s1,s1y1
	dc.w	char_t-chars

	dc.w	s1x1+15*s1,s1y1
	dc.w	char_e-chars

	dc.w	s1x1+20*s1,s1y1
	dc.w	char_n-chars

	dc.w	s1x1,s1y2
	dc.w	char_a-chars

	dc.w	s1x1+5*s1,s1y2
	dc.w	char_b-chars

	dc.w	s1x1+10*s1,s1y2
	dc.w	char_e-chars

	dc.w	s1x1+15*s1,s1y2
	dc.w	char_n-chars

	dc.w	s1x1+20*s1,s1y2
	dc.w	char_d-chars

	dc.w	-1 ; done


; WE OPEN THE COMPO
s2	equ	10

	if 0
; centered
s2x1	equ	(320-9*s2)/2
s2y1	equ	(256-23*s2)/2
s2x2	equ	(320-19*s2)/2
s2y2	equ	s2y1+6*s2
s2x3	equ	(320-14*s2)/2
s2y3	equ	s2y2+6*s2
s2x4	equ	(320-24*s2)/2
s2y4	equ	s2y3+6*s2
	else
; left
s2x1	equ	(320-24*s2)/2
s2y1	equ	(256-23*s2)/2
s2x2	equ	s2x1
s2y2	equ	s2y1+6*s2
s2x3	equ	s2x1
s2y3	equ	s2y2+6*s2
s2x4	equ	s2x1
s2y4	equ	s2y3+6*s2
	endif

screen2	dc.w	s2x1,s2y1 ; position x y
	dc.w	char_w-chars
	dc.w	s2x1+5*s2,s2y1
	dc.w	char_e-chars

	dc.w	s2x2,s2y2
	dc.w	char_o-chars
	dc.w	s2x2+5*s2,s2y2
	dc.w	char_p-chars
	dc.w	s2x2+10*s2,s2y2
	dc.w	char_e-chars
	dc.w	s2x2+15*s2,s2y2
	dc.w	char_n-chars

	dc.w	s2x3,s2y3
	dc.w	char_t-chars
	dc.w	s2x3+5*s2,s2y3
	dc.w	char_h-chars
	dc.w	s2x3+10*s2,s2y3
	dc.w	char_e-chars

	dc.w	s2x4,s2y4
	dc.w	char_c-chars
	dc.w	s2x4+5*s2,s2y4
	dc.w	char_o-chars
	dc.w	s2x4+10*s2,s2y4
	dc.w	char_m-chars
	dc.w	s2x4+15*s2,s2y4
	dc.w	char_p-chars
	dc.w	s2x4+20*s2,s2y4
	dc.w	char_o-chars

	dc.w	-1


; ALCATRAZ
s3	equ	13

s3x1	equ	(320-(3*5+4)*s3)/2 ; 3*5+4 (3 chars "ALC" with width 5, 1 char "A" width 4)
s3y1	equ	5*s3-28 ;24px up
s3x2	equ	s3x1
s3y2	equ	s3y1+6*s3

screen3	dc.w	s3x1,s3y1 ; position x y
	dc.w	char_a-chars
	dc.w	s3x1+5*s3,s3y1
	dc.w	char_l-chars
	dc.w	s3x1+10*s3,s3y1
	dc.w	char_c-chars
	dc.w	s3x1+15*s3,s3y1
	dc.w	char_a-chars

	dc.w	s3x2,s3y2
	dc.w	char_t-chars
	dc.w	s3x2+5*s3,s3y2
	dc.w	char_r-chars
	dc.w	s3x2+10*s3,s3y2
	dc.w	char_a-chars
	dc.w	s3x2+15*s3,s3y2
	dc.w	char_z-chars

	dc.w	-1 ; done


; SPREADPOINT
s4	equ	11

s4x1	equ	(320-(5*5+4)*s4)/2 ; 5*5+4 (5 chars "SPREA" with width 5, 1 char "D" width 4)
s4y1	equ	5*s4-10 ; move 10px up
s4x2	equ	s4x1+4*s4
s4y2	equ	s4y1+6*s4

screen4	dc.w	s4x1,s4y1 ; position x y
	dc.w	char_s-chars
	dc.w	s4x1+5*s4,s4y1
	dc.w	char_p-chars
	dc.w	s4x1+10*s4,s4y1
	dc.w	char_r-chars
	dc.w	s4x1+15*s4,s4y1
	dc.w	char_e-chars
	dc.w	s4x1+20*s4,s4y1
	dc.w	char_a-chars
	dc.w	s4x1+25*s4,s4y1
	dc.w	char_d-chars

	dc.w	s4x2,s4y2
	dc.w	char_p-chars
	dc.w	s4x2+5*s4,s4y2
	dc.w	char_o-chars
	dc.w	s4x2+10*s4,s4y2
	dc.w	char_i-chars
	dc.w	s4x2+12*s4,s4y2
	dc.w	char_n-chars
	dc.w	s4x2+17*s4,s4y2
	dc.w	char_t-chars

	dc.w	-1 ; done

chars
char_a	dc.b	12-1 ; num lines
	dc.b	0,0, 4,0, 4,5, 3,5, 3,3, 1,3, 1,5, 0,5
	dc.b	0,2, 3,2, 3,1, 0,1
	dc.b	0,0 ; close loop

char_b	dc.b	14-1
	dc.b	0,0, 3,0, 3,2, 4,2, 4,5, 0,5, 0,4, 3,4
	dc.b	3,3, 0,3, 0,2, 2,2, 2,1, 0,1
	dc.b	0,0

char_c	dc.b	8-1
	dc.b	0,0, 4,0, 4,1, 1,1, 1,4, 4,4, 4,5, 0,5
	dc.b	0,0

char_d	dc.b	10-1
	dc.b	0,0, 4,0, 4,5, 0,5, 0,2, 1,2, 1,4, 3,4
	dc.b	3,1, 0,1
	dc.b	0,0

char_e	dc.b	12-1
	dc.b	0,0, 4,0, 4,1, 1,1, 1,2, 4,2, 4,3, 1,3
	dc.b	1,4, 4,4, 4,5, 0,5
	dc.b	0,0

char_g	dc.b	13-1
	dc.b	0,0, 4,0, 4,1, 1,1, 1,4, 1,4, 3,4, 3,3
	dc.b	2,3, 2,2, 4,2, 4,5, 0,5
	dc.b	0,0

char_h	dc.b	12-1
	dc.b	0,0, 1,0, 1,2, 3,2, 3,0, 4,0, 4,5, 3,5
	dc.b	3,3, 1,3, 1,5, 0,5
	dc.b	0,0

char_i	dc.b	4-1
	dc.b	0,0, 1,0, 1,5, 0,5
	dc.b	0,0

char_l	dc.b	6-1
	dc.b	0,0, 1,0, 1,4, 4,4, 4,5, 0,5
	dc.b	0,0

char_m	dc.b	10-1
	dc.b	0,0, 4,0, 4,5, 3,5, 3,1, 2,1, 2,2, 1,2
	dc.b	1,5, 0,5
	dc.b	0,0

char_n	dc.b	8-1
	dc.b	0,0, 4,0, 4,5, 3,5, 3,1, 1,1, 1,5, 0,5
	dc.b	0,0

char_o	dc.b	10-1
	dc.b	0,0, 4,0, 4,5, 0,5, 0,4, 3,4, 3,1, 1,1
	dc.b	1,3, 0,3
	dc.b	0,0

char_p	dc.b	10-1
	dc.b	0,0, 4,0, 4,3, 1,3, 1,5, 0,5, 0,2, 3,2
	dc.b	3,1, 0,1
	dc.b	0,0

char_r	dc.b	12-1
	dc.b	0,0, 4,0, 4,3, 3,3, 3,5, 2,5, 2,2, 3,2
	dc.b	3,1, 1,1, 1,5, 0,5
	dc.b	0,0

char_s	dc.b	12-1
	dc.b	0,0, 4,0, 4,1, 1,1, 1,2, 4,2, 4,5, 0,5
	dc.b	0,4, 3,4, 3,3, 0,3
	dc.b	0,0

char_t	dc.b	6-1
	dc.b	0,0, 4,0, 4,5, 3,5, 3,1, 0,1
	dc.b	0,0

char_u	dc.b	8-1
	dc.b	0,0, 1,0, 1,4, 3,4, 3,0, 4,0, 4,5, 0,5
	dc.b	0,0

char_w	dc.b	10-1
	dc.b	0,0, 1,0, 1,3, 2,3, 2,4, 3,4, 3,0, 4,0
	dc.b	4,5, 0,5
	dc.b	0,0

char_z	dc.b	12-1
	dc.b	0,0, 4,0, 4,3, 1,3, 1,4, 4,4, 4,5, 0,5
	dc.b	0,2, 3,2, 3,1, 0,1
	dc.b	0,0

	even


*------	DRAW SCREEN -----------------------------------------------------------*

; a1=chardata d7=scalar
drawscreen
	lea	$52(a6),a6			;

	move.l	v_screenplane(a5),a0		;
	move.w	#psize/4-1,d6			;
.cls	clr.l	(a0)+				;
	dbf	d6,.cls				;

	btst	#14-8,$02-$52(a6)		;
.waitblitter
	btst	#14-8,$02-$52(a6)		;
	bne	.waitblitter			;
	move.w	#pwidth,$60-$52(a6)		;
	move.w	#pwidth,$66-$52(a6)		;
	move.l	#$ffff8000,$72-$52(a6)		; texture data/index
	move.w	#$8000,$44-$52(a6)		; first word mask

	move.l	v_screenplane(a5),a0		;
.chars	move.w	(a1)+,d4			; origin x or done flag
	bmi	.done				;

	move.w	(a1)+,d5			; origin y
	
	lea	chars(pc),a2			;
	add.w	(a1)+,a2			; char

	moveq	#0,d6				;
	move.b	(a2)+,d6			; num lines
.lines	moveq	#0,d0				;
	move.b	(a2)+,d0			; x1
	mulu	d7,d0				; never mind because not time critial
	add.w	d4,d0				; translate

	moveq	#0,d1				;
	move.b	(a2)+,d1			; y1
	mulu	d7,d1				; never mind because not time critial
	add.w	d5,d1				; translate

	moveq	#0,d2				;
	move.b	(a2)+,d2			; x2
	mulu	d7,d2				; never mind because not time critial
	add.w	d4,d2				; translate

	moveq	#0,d3				;
	move.b	(a2)+,d3			; y2
	mulu	d7,d3				; never mind because not time critial
	add.w	d5,d3				; translate

	movem.l	d4-d7,-(a7)			;
	bsr	.drawline			;
	movem.l	(a7)+,d4-d7			;
	
	subq.w	#2,a2				; p2 becomes p1
	dbf	d6,.lines			;
	bra	.chars				;

.done	lea	-$52(a6),a6			; a6 becomes $dff000 again
	rts					;

.drawline
	moveq	#4,d7				; note: moveq clears d7's upper word
	move.l	d7,a4				; octant code
	move.w	d0,d7				; x1
	and.w	#$000f,d7			;
	ror.w	#4,d7				; startbit of line
	or.w	#$0bca,d7			;
	swap	d7				;
	sub.w	d0,d2				; x2-x1
	bpl	.rightwards			;
	addq.w	#1,a4				;
	neg.w	d2				;
.rightwards
	sub.w	d1,d3				; y2-y1
	bpl	.upwards			;
	addq.w	#2,a4				; bset #1
	neg.w	d3				; d3=y
.upwards
	move.w	d3,d6				;
	sub.w	d2,d6				; d6=y-x
	bmi	.nsteep				; steepness <1
	exg	d2,d3				; swap x and y
	subq.w	#4,a4				; bclr #2
	neg.w	d6				;
.nsteep	lsl.w	#6,d2				; 64 = 1<<6
	add.w	#64+2,d2			; +2 required (width)
	move.w	d6,d4				; d2=y-x
	add.w	d3,d4				; d2=2y-x
	bpl	.nosign				;
	addq.w	#8,a4				; sign of 2y-x (in oct code)
.nosign	lsl.w	#2,d3				; d3=4y
	lsl.w	#2,d6				; d6=4y-4x
	swap	d3				;
	move.w	d6,d3				;
	clr.w	d7				;
	move.b	.octs(pc,a4.w),d7		; octant

	asl.w	#3,d1				; = mulu #40,d1 (*pwidth)
	move.w	d1,d6				;
	add.w	d6,d6				;
	add.w	d6,d6				;
	add.w	d6,d1				;

	lea	(a0,d1.w),a4			;
	asr.w	#3,d0				;
	add.w	d0,a4				;

	move.l	a6,a3				;
	btst	#14-8,$02-$52(a6)		;
.waitblitter2
	btst	#14-8,$02-$52(a6)		;
	bne	.waitblitter2			;
	move.l	d3,$62-$52(a6)			; 4y, 4y-4x	BLTB/AMOD
	move.l	d7,$40-$52(a6)			;		BLTCON 0,1
	move.l	a4,$48-$52(a6)			;
	move.w	d4,(a3)+			; 2y-x		BLTAPTL
	move.l	a4,(a3)+			; set starting address
	move.w	d2,(a3)				; start
	rts					;

.octs	dc.b	0*4+1,2*4+1,1*4+1,3*4+1
	dc.b	4*4+1,5*4+1,6*4+1,7*4+1
	dc.b	0*4+65,2*4+65,1*4+65,3*4+65
	dc.b	4*4+65,5*4+65,6*4+65,7*4+65


*------ SET SPRITE POINTERS ---------------------------------------------------*

; a0: clist d0: sprite data
setspritepointers
	moveq	#8-1,d7				; 8 sprite pointers
.loop	swap	d0				; sprite0
	move.w	d0,(a0)				;
	swap	d0				;
	move.w	d0,4(a0)			;
	addq.w	#8,a0				; next pointer
	dbf	d7,.loop			;
	rts					;


*------ SPRITE DATA -----------------------------------------------------------*

spritedata
	dc.w	$1905,$1a00	; 1px high sprite in the top-leftmost valid position
	dc.w	$0000,$0000	; blank pixel data
	dc.w	$0000,$0000	; end of sprite
spritedataend


*------	PLAYER ----------------------------------------------------------------*

cmd_wait	equ 	0<<1
cmd_actor_start	equ	1<<1
cmd_actor_stop	equ	2<<1
cmd_glitch	equ	3<<1
cmd_quit	equ	4<<1
cmd_drawscreen	equ	5<<1
cmd_colormb	equ	6<<1
cmd_colorbg	equ	7<<1

cmds	dc.w	0 ; cmd_wait (must be zero)
	dc.w	cmdactorstart-cmds
	dc.w	cmdactorstop-cmds
	dc.w	cmdglitch-cmds
	dc.w	cmdquit-cmds
	dc.w	cmddrawscreen-cmds
	dc.w	cmdcolormb-cmds
	dc.w	cmdcolorbg-cmds

player	tst.b	v_doquit(a5)		;
	bne	.quit			; don't mess up with precalc
	tst.b	v_wait(a5)		;
	beq	.donotwait		;
	subq.b	#1,v_wait(a5)		;
.quit	rts				;
.donotwait
	move.l	v_cmdspointer(a5),a0	;
loop	moveq	#0,d0			;
	move.b	(a0)+,d0		;
	bne	.notcmdwait		;
	move.b	(a0)+,v_wait(a5)	; cmd_wait duration
	move.l	a0,v_cmdspointer(a5)	;
	if numbers
	addq.b	#1,v_waitcount(a5)	; sync helper
	endif
	rts				;

.notcmdwait
	lea	cmds(pc),a1		;
	add.w	(a1,d0.w),a1		;
	jsr	(a1)			; execute cmd
	bra	loop			;
	
cmdactorstart
	moveq	#0,d1			;
	move.b	(a0)+,d1		; actor
	move.w	v_actors(a5),d2		;
	bset	d1,d2			;
	bra	set			;

cmdactorstop
	moveq	#0,d1			;
	move.b	(a0)+,d1		; actor
	move.w	v_actors(a5),d2		;
	bclr	d1,d2			;
set	move.w	d2,v_actors(a5)		;
	rts				;

cmdglitch
	lea	v_glitches(a5),a1	;
	moveq	#maxglitches-1,d7	;
.serach	tst.w	glitch_alive(a1)	;
	beq	.slotfound		;
	add.w	#sizeofglitch,a1	;
	dbf	d7,.serach		;
.warn	move.w	#$0f00,$180(a6)		; could be deleted
	bra	.warn			;

.slotfound				; a1 points to glitch struct
	st	glitch_alive(a1)	; set alive

	moveq	#0,d1			;
	move.b	(a0)+,d1		; effect
	move.w	d1,glitch_effect(a1)	;

	move.b	(a0)+,d1		; line
	move.w	d1,glitch_line(a1)	;

	clr.w	glitch_time(a1)		; reset time

	move.b	(a0)+,d1		; ttl
	move.w	d1,glitch_ttl(a1)	;

	move.b	(a0)+,glitch_data1(a1)	; data 1
	move.b	(a0)+,glitch_data1+1(a1) ;

	move.b	(a0)+,glitch_data2(a1)	; data 2
	move.b	(a0)+,glitch_data2+1(a1) ;

	move.b	(a0)+,glitch_data3(a1)	; data 3
	move.b	(a0)+,glitch_data3+1(a1) ;

	clr.l	glitch_tlindex(a1)	; reset tlindex AND wait
	rts				;

cmddrawscreen
	move.b	(a0)+,v_screenid+1(a5)	;
	STARTACTOR actor_draw_screen
	rts				;

cmdquit	st	v_doquit(a5)		;
	rts				;

cmdcolormb
	lea	colormetaballs+2(pc),a1	;
	move.b	(a0)+,(a1)+		;
	move.b	(a0)+,(a1)		;
	rts				;

cmdcolorbg
	lea	colorbackground+2(pc),a1 ;
	move.b	(a0)+,(a1)+		;
	move.b	(a0)+,(a1)		;
	rts				;


playcmds
	dc.b	cmd_drawscreen,1 ; GUTEN ABEND

	dc.b	cmd_wait,5

	; GUTEN
	dc.b	cmd_glitch, glitch_neon, $01, -1, 0,colorbank1-colorbanks, 0,timeline1-timelines, 0,0
	; ABEND
	dc.b	cmd_glitch, glitch_neon, $6e, -1, 0,colorbank2-colorbanks, 0,timeline2-timelines, 0,0

	dc.b	cmd_wait,80

	dc.b	cmd_glitch, glitch_color, $b6, 255, $00,$00, 0,0, 0,0
	dc.b	cmd_wait,1

	dc.b	cmd_glitch, glitch_shift, $51, 4, $ff,$fe, 0,0, 0,0

	dc.b	cmd_glitch, glitch_shift, $71, 4, $00,$02, 0,0, 0,0

	dc.b	cmd_wait,20

	dc.b	cmd_glitch, glitch_shift, $21, 4, $ff,$fe, 0,40, 0,0
	dc.b	cmd_wait,50

	dc.b	cmd_glitch, glitch_shift, $71, 4, $ff,$fe, 0,40, 0,0
	dc.b	cmd_wait,20

	dc.b	cmd_glitch, glitch_shift, $21, 4, $ff,$fe, 0,40, 0,0
	dc.b	cmd_wait,20

	dc.b	cmd_wait,50

	dc.b	cmd_drawscreen,0 ; empty for flash effect

	dc.b	cmd_wait,50-25+11
	dc.b	cmd_actor_start,actor_effect

	dc.b	cmd_colorbg,$0f,$ff, cmd_colormb,$0f,$ff, cmd_wait,0
	dc.b	cmd_colorbg,$0e,$ee, cmd_colormb,$0f,$ed, cmd_wait,0
	dc.b	cmd_colorbg,$0d,$dd, cmd_colormb,$0f,$ea, cmd_wait,0
	dc.b	cmd_colorbg,$0c,$cc, cmd_colormb,$0f,$d8, cmd_wait,0
	dc.b	cmd_colorbg,$0b,$bb, cmd_colormb,$0f,$d5, cmd_wait,0
	dc.b	cmd_colorbg,$0a,$aa, cmd_colormb,$0f,$c3, cmd_wait,0
	dc.b	cmd_colorbg,$09,$99, cmd_wait,0
	dc.b	cmd_colorbg,$08,$88, cmd_wait,0
	dc.b	cmd_colorbg,$07,$77, cmd_wait,0
	dc.b	cmd_colorbg,$06,$66, cmd_wait,0
	dc.b	cmd_colorbg,$05,$55, cmd_wait,0
	dc.b	cmd_colorbg,$04,$44, cmd_wait,0
	dc.b	cmd_colorbg,$03,$33, cmd_wait,0
	dc.b	cmd_colorbg,$02,$22, cmd_wait,0
	dc.b	cmd_colorbg,$01,$11, cmd_wait,0
	dc.b	cmd_colorbg,$00,$00, cmd_wait,0

	dc.b	cmd_drawscreen,2 ; WE OPEN THE COMPO (prepare do not show)

	dc.b	cmd_wait,200-11
	dc.b	cmd_glitch, glitch_shift, $41, 5, $ff,$fe, 0,0, 0,0
	dc.b	cmd_glitch, glitch_shift, $c1, 5, $00,$00, 0,0, 0,0

	dc.b	cmd_wait,100-80
	dc.b	cmd_glitch, glitch_shift, $41, 5, $ff,$fe, 0,0, 0,0
	dc.b	cmd_glitch, glitch_shift, $c1, 5, $00,$00, 0,0, 0,0
	dc.b	cmd_wait,79
	
	dc.b	cmd_glitch, glitch_shift, $20, 50, $ff,$fe, $00,$08, $00,$03

	dc.b	cmd_glitch, glitch_neon, s2y1, -1, 0,colorbankwe-colorbanks, 0,timelinewe-timelines, 0,0
	dc.b	cmd_glitch, glitch_neon, s2y2, -1, 0,colorbankopen-colorbanks, 0,timelineopen-timelines, 0,0
	dc.b	cmd_glitch, glitch_neon, s2y3, -1, 0,colorbankthe-colorbanks, 0,timelinethe-timelines, 0,0
	dc.b	cmd_glitch, glitch_neon, s2y4, -1, 0,colorbankcompo-colorbanks, 0,timelinecompo-timelines, 0,0

	dc.b	cmd_wait,10

	dc.b	cmd_glitch, glitch_shift, $41, 15, $ff,$fe, 0,0, 0,0
	dc.b	cmd_glitch, glitch_shift, $c1, 15, $00,$00, 0,0, 0,0
	dc.b	cmd_wait,40

	dc.b	cmd_glitch, glitch_shift, $21, 5, $ff,$fe, 0,0, 0,0
	dc.b	cmd_glitch, glitch_shift, $a1, 5, $00,$00, 0,0, 0,0
	dc.b	cmd_wait,20

	dc.b	cmd_glitch, glitch_shift, $51, 5, $ff,$fe, 0,0, 0,0
	dc.b	cmd_glitch, glitch_shift, $d1, 5, $00,$00, 0,0, 0,0
	dc.b	cmd_wait,40

	dc.b	cmd_glitch, glitch_shift, $f0-$2c, 50, $ff,$fe, $00,$08, $00,$01
	dc.b	cmd_wait,100

	dc.b	cmd_glitch, glitch_shift, $20, 50, $ff,$fe, $00,$08, $00,$02
	dc.b	cmd_wait,150

	dc.b	cmd_drawscreen,0 ; empty for colorbg glitch
	dc.b	cmd_wait,2

	dc.b	cmd_glitch, glitch_colorbg, $60, 2, 0,15, $0d,$a1, $00,$44
	dc.b	cmd_wait,3
	dc.b	cmd_glitch, glitch_colorbg, $64, 3, 0,15, $0b,$80, $00,$bb
	dc.b	cmd_wait,5

	dc.b	cmd_glitch, glitch_colorbg, $c0, 2, 0,18, $0d,$a1, $00,$66
	dc.b	cmd_wait,3
	dc.b	cmd_glitch, glitch_colorbg, $e0, 4, 0,28, $0b,$80, $00,$dd
	dc.b	cmd_wait,7

	dc.b	cmd_glitch, glitch_colorbg, $70, 2, 0,15, $0d,$a1, $00,$88
	dc.b	cmd_wait,4
	dc.b	cmd_glitch, glitch_colorbg, $74, 4, 0,25, $0b,$80, $00,$ff
	dc.b	cmd_wait,34
	
	dc.b	cmd_glitch, glitch_shift, $41, 5, $ff,$fe, 0,0, 0,0
	dc.b	cmd_glitch, glitch_shift, $c1, 5, $00,$00, 0,0, 0,0

	; wanderer
	dc.b	cmd_glitch, glitch_shift, $44, 50, $ff,$fe, $00,$08, $00,$01

	dc.b	cmd_glitch, glitch_shift, $41, 5, $ff,$fe, 0,0, 0,0
	dc.b	cmd_glitch, glitch_shift, $c1, 5, $00,$00, 0,0, 0,0
	dc.b	cmd_wait,79

	dc.b	cmd_glitch, glitch_shift, $20, 50, $00,$02, $00,$12, $00,$01

	dc.b	cmd_wait,30
	dc.b	cmd_glitch, glitch_shift, $40, 20, $ff,$fe, 0,0, 0,0

	dc.b	cmd_wait,150	
	
	dc.b	cmd_glitch, glitch_shift, $54, 40, $ff,$fe, $00,$06, $00,$01

	dc.b	cmd_glitch, glitch_shift, $41, 10, $ff,$fe, 0,0, 0,0
	dc.b	cmd_glitch, glitch_shift, $c1, 10, $00,$00, 0,0, 0,0

	dc.b	cmd_wait,5

	dc.b	cmd_glitch, glitch_colorbg, $b0, 2, 0,15, $0d,$a1, $00,$44
	dc.b	cmd_wait,3
	dc.b	cmd_glitch, glitch_colorbg, $b4, 3, 0,15, $0b,$80, $00,$bb
	dc.b	cmd_wait,5

	dc.b	cmd_glitch, glitch_shift, $84, 40, $ff,$fe, $00,$04, $00,$01

	dc.b	cmd_wait,10
	dc.b	cmd_glitch, glitch_colorbg, $40, 2, 0,15, $0d,$a1, $00,$44
	dc.b	cmd_wait,3
	dc.b	cmd_glitch, glitch_colorbg, $44, 3, 0,15, $0b,$80, $00,$bb
	dc.b	cmd_wait,3+5

	dc.b	cmd_glitch, glitch_colorbg, $80, 2, 0,15, $0d,$a1, $00,$44
	dc.b	cmd_wait,3
	dc.b	cmd_glitch, glitch_colorbg, $84, 3, 0,15, $0b,$80, $00,$bb
	dc.b	cmd_wait,3+5

	dc.b	cmd_glitch, glitch_colorbg, $70, 2, 0,15, $0d,$a1, $00,$44
	dc.b	cmd_wait,3
	dc.b	cmd_glitch, glitch_colorbg, $74, 3, 0,15, $0b,$80, $00,$bb

	dc.b	cmd_wait,83

	dc.b	cmd_drawscreen,3 ; ALCATRAZ
	dc.b	cmd_wait,2

	dc.b	cmd_glitch, glitch_neon, s3y1, -1, 0,colorbank1-colorbanks
	dc.b	(timelinealca-timelines)>>8,(timelinealca-timelines)&$ff, 0,0
	dc.b	cmd_glitch, glitch_neon, s3y2, -1, 0,colorbank2-colorbanks
	dc.b	(timelinetraz-timelines)>>8,(timelinetraz-timelines)&$ff, 0,0

	dc.b	cmd_wait,191

	dc.b	cmd_colormb,$0f,$c3, cmd_wait,0 ; fade out metaballs
	dc.b	cmd_colormb,$0d,$b3, cmd_wait,0
	dc.b	cmd_colormb,$0b,$a3, cmd_wait,0
	dc.b	cmd_colormb,$09,$92, cmd_wait,0
	dc.b	cmd_colormb,$07,$72, cmd_wait,0
	dc.b	cmd_colormb,$05,$52, cmd_wait,0
	dc.b	cmd_colormb,$03,$31, cmd_wait,0
	dc.b	cmd_colormb,$01,$11, cmd_wait,0
	
	dc.b	cmd_colormb,$00,$00
	dc.b	cmd_actor_stop,actor_effect

	dc.b	cmd_wait,82+6

	dc.b	cmd_drawscreen,4 ; SPREADPOINT
	dc.b	cmd_wait,2

	dc.b	cmd_glitch, glitch_neon, s4y1, -1, 0,colorbank1-colorbanks
	dc.b	(timelinespread-timelines)>>8,(timelinespread-timelines)&$ff, 0,0
	dc.b	cmd_glitch, glitch_neon, s4y2, -1, 0,colorbank2-colorbanks
	dc.b	(timelinepoint-timelines)>>8,(timelinepoint-timelines)&$ff, 0,0
	dc.b	cmd_wait,248-6

	dc.b	cmd_drawscreen,0 ; empty for fading to white
	dc.b	cmd_wait,30

	dc.b	cmd_colorbg,$01,$11, cmd_wait,0
	dc.b	cmd_colorbg,$03,$33, cmd_wait,0
	dc.b	cmd_colorbg,$05,$55, cmd_wait,0
	dc.b	cmd_colorbg,$07,$77, cmd_wait,0
	dc.b	cmd_colorbg,$09,$99, cmd_wait,0
	dc.b	cmd_colorbg,$0b,$bb, cmd_wait,0
	dc.b	cmd_colorbg,$0d,$dd, cmd_wait,0
	dc.b	cmd_colorbg,$0f,$ff, cmd_wait,0

	dc.b	cmd_wait,20+30+20

	dc.b	cmd_quit
	dc.b	cmd_wait,255 ; you never know ;-) stranger things happen

	even


*------	COPPER INSTRUCTION LIST 1 BUDDHA'S PRECALC BAR ------------------------*

clist1	
spritepointersclist1
	dc.w	$0120,0,$0122,0
	dc.w	$0124,0,$0126,0
	dc.w	$0128,0,$012a,0
	dc.w	$012c,0,$012e,0
	dc.w	$0130,0,$0132,0
	dc.w	$0134,0,$0136,0
	dc.w	$0138,0,$013a,0
	dc.w	$013c,0,$013e,0

	dc.w	$008e,$2c81
	dc.w	$0090,$2cc1
	dc.w	$0092,$0038
	dc.w	$0094,$00d0

	dc.w	$0100,$0200
	dc.w	$0102,$0000

	dc.w	$0108,$0000
	dc.w	$010a,$0000

clist1wait
	dc.w	$5d07,$fffe ; $ff-160-1-1
	dc.w	$0180,$0222
	
	dc.w	$ff07,$fffe
	dc.w	$0180,$0000

	dc.w	$ffff,$fffe
clist1end


*------	COPPER INSTRUCTION LIST 2 ---------------------------------------------*

clist2	dc.w	$1507,$fffe ; chance for player to alter clist in time
clist2bpl
	dc.w	$00e0,0,$00e2,0
clist2bpl2
	dc.w	$00e4,0,$00e6,0

spritepointersclist2
	dc.w	$0120,0,$0122,0
	dc.w	$0124,0,$0126,0
	dc.w	$0128,0,$012a,0
	dc.w	$012c,0,$012e,0
	dc.w	$0130,0,$0132,0
	dc.w	$0134,0,$0136,0
	dc.w	$0138,0,$013a,0
	dc.w	$013c,0,$013e,0

	dc.w	$008e,$2c81
	dc.w	$0090,$2cc1
	dc.w	$0092,$0038
	dc.w	$0094,$00d0

	dc.w	$0108,$0000
	dc.w	$010a,$0000

	dc.w	$0100,$2600
	dc.w	$0102,$0000
bplcon2	dc.w	$0104,$0000

colorbackground
	dc.w	$0180,0
colormetaballs
	dc.w	$0182,0
	if numbers
	dc.w	$0182,$0fc3	; color of numbers
	endif

	dc.w	$0190,$0000
colortext
	dc.w	$0192,0
clist2end


*------	CLEAR SCREEN ----------------------------------------------------------*

cpuclslines	equ	130
cpuclschunk	equ	10	; 10 * 4 (longword) = 40 (=visible pwidth)

cls	move.l	v_db1b2b(a5),a4			;
	bsr	waitblitter			;

	move.w	#$0000,$66(a6)			; modulo D
	move.l	#$01000000,$40(a6)		; bltcon0 bltcon1
	move.l	a4,$54(a6)			; destination D
	move.w	#(pheight-cpuclslines)<<6+pwidth>>1,$58(a6) ; bltsize and start
	add.w	#psize,a4			; end of bitplane
	moveq	#(cpuclslines/cpuclschunk)-1,d7	; 14 * 10 lines
	movem.l	v_zero(a5),d0-d6/a0-a2		; 10 registers * 4 bytes = 40 (1 visible line)
.cpucls	
	rept cpuclschunk
	movem.l	d0-d6/a0-a2,-(a4)		; clears 1 line (pwidth)
	endr
	dbf	d7,.cpucls			;

	if timing
	move.w	#$0fff,$180(a6)			; white should be as small as possible
	endif

	bsr	waitblitter			;

	if timing
	move.w	#$0000,$180(a6)			;
	endif

	rts					;


*------	MEMORY MANAGEMENT -----------------------------------------------------*

BESTMEMORY	equ	0
MEMF_CHIP	equ	1<<1
MEMF_CLEAR	equ	1<<16

clist1size	equ	clist1end-clist1
clist2size	equ	clist2end-clist2
lspbanksize	equ	lspbankend-lspbank
spritedatasize	equ	spritedataend-spritedata

; note: MEMF_CLEAR for extra safety
memtable
b_clist1	dc.l	0,MEMF_CHIP+MEMF_CLEAR,clist1size
b_lspbank	dc.l	0,MEMF_CHIP+MEMF_CLEAR,lspbanksize
b_spritedata	dc.l	0,MEMF_CHIP+MEMF_CLEAR,spritedatasize

memtable2
b_vars		dc.l	0,MEMF_CHIP+MEMF_CLEAR,sizeofvars
; clist2 and bitplanes will reuse b_buffer
b_buffer	dc.l	0,MEMF_CHIP+MEMF_CLEAR,buffersafezone+(bufferh*bufferw)+buffersafezone
b_animation	dc.l	0,BESTMEMORY+MEMF_CLEAR,$45e4f ; see profiling animlen flag
memtableend

entrysize 	equ	3*4 ; one entry in the memtable is 12 bytes large (3 longwords)
entries		equ	(memtableend-memtable)/entrysize
entrieschip	equ	(memtable2-memtable)/entrysize

allocandinit
	lea	base(pc),a1			;
	move.l	AbsExecBase.w,a6		;
	jsr	TypeOfMem(a6)			;
	btst	#1,d0				; chipmem?
	beq	.notchipmem			;

	lea	clist1(pc),a0			;
	lea	b_clist1(pc),a1			;
	move.l	a0,(a1)				;

	lea	base(pc),a0			;
	add.l	#lspbank-base,a0		;
	lea	b_lspbank(pc),a1		;
	move.l	a0,(a1)				;

	lea	spritedata(pc),a0		;
	lea	b_spritedata(pc),a1		;
	move.l	a0,(a1)				;
.notchipmem
	lea	memtable(pc),a5			;
	moveq	#entries-1,d7			;
.loop	tst.l	(a5)				; not to be allocated?
	bne	.noalloc			;
	move.l	8(a5),d0			; bytesize
	move.l	4(a5),d1			; requirements
	move.l	AbsExecBase.w,a6		;
	jsr	AllocMem(a6)			;
	move.l	d0,(a5)				;
	beq	.printerrorandfreemem		; out of memory
.noalloc	
	add.w	#entrysize,a5			; next entry
	dbf	d7,.loop			;
	bsr	init				;
	moveq	#0,d0				; ok, all entries allocated
	rts					;

.printerrorandfreemem
	bsr	printoutofmemory		;
dealloc	lea	base(pc),a1			;
	move.l	AbsExecBase.w,a6		;
	jsr	TypeOfMem(a6)			;
	lea	memtable(pc),a5			;
	moveq	#entries-1,d7			;
	btst	#1,d0				; chipmem?
	beq	.loop				; we are not in chipmem so free all entries
	lea	memtable2(pc),a5		;
	moveq	#entries-entrieschip-1,d7	;
.loop	tst.l	(a5)				; end of memtable?
	beq	.done				;
	move.l	(a5),a1				; address of memory block
	move.l	8(a5),d0			; bytesize
	move.l	AbsExecBase.w,a6		;
	jsr	FreeMem(a6)			;
	add.w	#entrysize,a5			;
	dbf	d7,.loop			;
.done	moveq	#-1,d0				; alloc error
	rts					;

init	move.l	b_vars(pc),a5			;

	lea	base(pc),a0			; copy lspbank to chip memory
	add.l	#lspbank-base,a0		;
	move.l	b_lspbank(pc),a1		;
	move.l	#lspbanksize,d0			;
.copylspbank
	move.b	(a0)+,(a1)+			;
	subq.l	#1,d0				;
	bne	.copylspbank			;

	move.l	b_buffer(pc),a0			;
	add.w	#buffersafezone,a0		;
	move.l	a0,v_buffer(a5)			;

;	move.l	b_bitplanes(pc),a0		;
	move.l	b_buffer(pc),a0			;
	add.w	#invissize,a0			;

	lea	v_db1a2a(a5),a1			;
	move.l	a0,(a1)+			; v_db1a2a
	add.w	#invissize+psize,a0		;
	move.l	a0,(a1)+			; v_db1b2b

	add.w	#invissize+psize,a0		;
	move.l	a0,v_screenplane(a5)		;
	lea	clist2bpl2+2(pc),a1		;
	move.l	a0,d0				;
	move.w	d0,4(a1)			;
	swap	d0				;
	move.w	d0,(a1)				;

	add.w	#psize,a0			;
	move.l	a0,v_clistgen(a5)		;

	lea	clist1(pc),a0			; copy clist 1 to chip memory
	move.l	b_clist1(pc),a1			;
	move.w	#clist1size-1,d7		;
.copyc4	move.b	(a0)+,(a1)+			;
	dbf	d7,.copyc4			;

	lea	spritedata(pc),a0		; copy sprite data to chip memory
	move.l	b_spritedata(pc),a1		;
	move.l	a1,d0				; used in setspritepointers
	move.w	#spritedatasize-1,d7		;
.copysd	move.b	(a0)+,(a1)+			;
	dbf	d7,.copysd			;

	move.l	b_clist1(pc),a0			; sprites clist 1
	add.w	#spritepointersclist1+2-clist1,a0 ;
	bsr	setspritepointers		;

	lea	clist2(pc),a0			; sprites clist 2
	add.w	#spritepointersclist2+2-clist2,a0 ;
	bsr	setspritepointers		;

	lea	playcmds(pc),a0			; init and start player
	move.l	a0,v_cmdspointer(a5)		;

	lea	metaballsinitdata(pc),a0	;
	lea	v_metaballs(a5),a1		;
	moveq	#(metaballsinitdataend-metaballsinitdata)/2-1,d7 ;
.imb	move.w	(a0)+,(a1)+			;
	dbf	d7,.imb				;
	rts					;

metaballsinitdata
	dc.w	110,30,1<<1,3<<1
	dc.w	0,0,4<<1,2<<1
	dc.w	64,32,3<<1,2<<1
metaballsinitdataend


*------	PRINT OUT OF MEMORY ---------------------------------------------------*

printoutofmemory
	lea	.dos(pc),a1			;
	move.l	AbsExecBase.w,a6		;
	jsr	OldOpenLibrary(a6)		;
	move.l	d0,a6				;
	beq	.error				;
	jsr	Output(a6)			;
	move.l	d0,d1				;
	beq	.error				;
	moveq	#.textend-.text,d3		; length
	lea	.text(pc),a1			;
	move.l	a1,d2				;
	jsr	Write(a6)			;
	tst.l	d0				;
	beq	.error				;
	move.l	a6,a1				;
	move.l	AbsExecBase.w,a6		;
	jsr	CloseLibrary(a6)		;
.error	moveq	#0,d0				;
	rts					;

.dos	dc.b	"dos.library",0
.text	dc.b	"Error: Could not allocate enough memory",10
.textend
	even


;*****************************************************************
;
;	Light Speed Player v1.13 (modified)
;	Fastest Amiga MOD player ever :)
;	Written By Arnaud Carré (aka Leonard / OXYGENE)
;	https://github.com/arnaud-carre/LSPlayer
;	X: @leonard_coder
;
;	--------How to use--------- 
;
;	bsr LSP_MusicDriver_CIA_Start : Init LSP player code and install CIA interrupt
;		a2: VBR (CPU Vector Base Register) (use 0 if 68000)
;*****************************************************************

LSP_MusicDriver_CIA_Start
	lea	.irqVector(pc),a3
	lea	$78(a2),a2
	move.l	a2,(a3)
	lea	.LSPDmaCon+1(pc),a2		; DMACON byte patch address
	bsr	LSP_MusicInit			; init the LSP player ( whatever fast or insane version )

	lea	.pMusicBPM(pc),a2
	move.l	a0,(a2)				; store music BPM pointer
	move.w	(a0),d0				; start BPM
	lea	.curBpm(pc),a2
	move.w	d0,(a2)

	lea	.LSP_MainIrq(pc),a0
	move.l	.irqVector(pc),a4
	move.l	a0,(a4)

	lea	$bfd000,a0
	move.b 	#$7f,$d00(a0)
	move.b 	#$10,$e00(a0)
	move.b 	#$10,$f00(a0)
	move.l	#1773447,d1			; PAL clock

	lea	.ciaClock(pc),a4
	move.l	d1,(a4)
	divu.w	d0,d1
	move.b	d1,$400(a0)
	lsr.w 	#8,d1
	move.b	d1,$500(a0)
	move.b	#$83,$d00(a0)
	move.b	#$11,$e00(a0)
			
	move.b	#496&255,$600(a0)		; set timer B to 496 (to set DMACON)
	move.b	#496>>8,$700(a0)
	rts

.LSPDmaCon	dc.w	$8000
.irqVector	dc.l	0
.ciaClock	dc.l	0
.curBpm		dc.w	0
.pMusicBPM	dc.l	0

.LSP_MainIrq
	btst.b	#0,$bfdd00
	beq	.skipa

	movem.l	d0-a6,-(a7)
	lea	custom,a6
	bsr	LSP_MusicPlayTick		; LSP main music driver tick

	; check if BMP changed in the middle of the music
.mute	move.l	.pMusicBPM(pc),a0
	move.w	(a0),d0				; current music BPM
	cmp.w	.curBpm(pc),d0
	beq	.noChg
	lea	.curBpm(pc),a2			
	move.w	d0,(a2)				; current BPM
	move.l	.ciaClock(pc),d1
	divu.w	d0,d1
	move.b	d1,$bfd400
	lsr.w 	#8,d1
	move.b	d1,$bfd500			

.noChg	lea	.LSP_DmaconIrq(pc),a0
	move.l	.irqVector(pc),a1
	move.l	a0,(a1)
	move.b	#$19,$bfdf00			; start timer B, one shot

	movem.l	(a7)+,d0-a6
.skipa	move.w	#$2000,$dff09c
	nop
	rte

.LSP_DmaconIrq
	btst.b	#1,$bfdd00
	beq	.skipb
	move.w	.LSPDmaCon(pc),$dff096
	pea	(a0)
	move.l	.irqVector(pc),a0
	pea	.LSP_MainIrq(pc)
	move.l	(a7)+,(a0)
	move.l	(a7)+,a0
.skipb	move.w	#$2000,$dff09c
	nop
	rte


;------------------------------------------------------------------
;
;	LSP_MusicInit
;
;		In:	a0: LSP music data (any memory)
;			a1: LSP sound bank (chip memory)
;			a2: DMAcon patch
;		Out:a0: music BPM pointer (16bits)
;			d0: music len in tick count
;
;------------------------------------------------------------------
LSP_MusicInit
	lea	base(pc),a0			; a0: music data (any mem) + 10
	add.l	#lspmusic-base,a0		;
	move.l	b_lspbank(pc),d1		; a1: sound bank data (chip mem)

	lea	LSP_State(pc),a3
	move.l	a2,m_dmaconPatch(a3)
	
	move.l	a0,a4				; relocation flag ad
	addq.w	#2,a0				; skip relocation flag
	move.w	(a0)+,m_currentBpm(a3)		; default BPM
	move.w	(a0)+,m_escCodeRewind(a3)
	move.w	(a0)+,m_escCodeSetBpm(a3)
	move.w	(a0)+,m_escCodeGetPos(a3)
	move.l	(a0)+,-(a7)			; music len in frame ticks
	move.w	(a0)+,d0			; instrument count
	lea	-12(a0),a2			; LSP data has -12 offset on instrument tab (win 2 cycles in insane player)
	move.l	a2,m_lspInstruments(a3)		; instrument tab addr (minus 4)
	subq.w	#1,d0
	move.l	a0,a1				; keep relocated flag
.relocLoop
	tst.b	(a4)				; relocation guard
	bne	.relocated
	add.l	d1,(a0)
	add.l	d1,6(a0)
.relocated
	lea	12(a0),a0
	dbf	d0,.relocLoop
	move.w	(a0)+,d0			; codes table size
	move.l	a0,m_codeTableAddr(a3)		; code table
	add.w	d0,d0
	add.w	d0,a0

	; read sequence timing infos (if any)
	move.w	(a0)+,m_seqCount(a3)
	beq	.noSeq
	move.l	a0,m_seqTable(a3)
	clr.w	m_currentSeq(a3)
	move.w	m_seqCount(a3),d0
	moveq	#0,d1
	move.w	d0,d1
	lsl.w	#3,d1				; 8 bytes per entry
	add.w	#12,d1				; add 3 last 32bits (word stream size, byte stream loop, word stream loop)
	add.l	a0,d1				; word stream data address
	subq.w	#1,d0
.seqRel	tst.b	(a4)
	bne	.skipRel
	add.l	d1,(a0)
	add.l	d1,4(a0)
.skipRel
	addq.w	#8,a0
	dbf	d0,.seqRel

.noSeq	movem.l	(a0)+,d0-d2			; word stream size, byte stream loop point, word stream loop point
	st	(a4)				; mark this music score as "relocated"
	move.l	a0,m_wordStream(a3)
	lea	(a0,d0.l),a1			; byte stream
	move.l	a1,m_byteStream(a3)
	add.l	d2,a0
	add.l	d1,a1
	move.l	a0,m_wordStreamLoop(a3)
	move.l	a1,m_byteStreamLoop(a3)
	lea	m_currentBpm(a3),a0
	move.l	(a7)+,d0			; music len in frame ticks
	rts


;------------------------------------------------------------------
;
;	LSP_MusicPlayTick
;
;		In:	a6: must be $dff000
;			Scratched regs: d0/d1/d2/a0/a1/a2/a3/a4/a5
;		Out:None
;
;------------------------------------------------------------------
LSP_MusicPlayTick
	lea	LSP_State(pc),a1
	move.l	m_byteStream(a1),a0
	move.l	m_codeTableAddr(a1),a2
.process
	moveq	#0,d0
.cloop	move.b	(a0)+,d0
	beq	.cextended
	add.w	d0,d0
	move.w	(a2,d0.w),d0			; code
	beq	.noInst
.cmdExec
	add.b	d0,d0
	bcc	.noVd
	move.b	(a0)+,$d9(a6)
.noVd	add.b	d0,d0
	bcc	.noVc
	move.b	(a0)+,$c9(a6)
.noVc	add.b	d0,d0
	bcc	.noVb
	move.b	(a0)+,$b9(a6)
.noVb	add.b	d0,d0
	bcc	.noVa
	move.b	(a0)+,$a9(a6)
.noVa	move.l	a0,(a1)+			; store byte stream ptr
	move.l	(a1),a0				; word stream
	tst.b	d0
	beq	.noPa
	add.b	d0,d0
	bcc	.noPd
	move.w	(a0)+,$d6(a6)
.noPd	add.b	d0,d0
	bcc	.noPc
	move.w	(a0)+,$c6(a6)
.noPc	add.b	d0,d0
	bcc	.noPb
	move.w	(a0)+,$b6(a6)
.noPb	add.b	d0,d0
	bcc	.noPa
	move.w	(a0)+,$a6(a6)
.noPa	tst.w	d0
	beq	.noInst

	moveq	#0,d1
	move.l	m_lspInstruments-4(a1),a2	; instrument table
	lea	.resetv+12(pc),a4
	lea	$d0(a6),a5
	moveq	#4-1,d2
.vloop	add.w	d0,d0
	bcs	.setIns
	add.w	d0,d0
	bcc	.skip
	move.l	(a4),a3
	move.l	(a3)+,(a5)
	move.w	(a3)+,4(a5)
	bra	.skip
.setIns	add.w	(a0)+,a2
	add.w	d0,d0
	bcc	.noReset
	bset	d2,d1
	move.w	d1,$96(a6)
.noReset
	move.l	(a2)+,(a5)
	move.w	(a2)+,4(a5)
	move.l	a2,(a4)
.skip	subq.w	#4,a4
	sub.w	#$10,a5
	dbf	d2,.vloop

	move.l	m_dmaconPatch-4(a1),a3	; set dmacon value
	move.b	d1,(a3)

.noInst	move.l	a0,(a1)			; store word stream (or byte stream if coming from early out)
	rts

.cextended
	add.w	#$100,d0
	move.b	(a0)+,d0
	beq	.cextended
	add.w	d0,d0
	move.w	(a2,d0.w),d0		; code
	cmp.w	m_escCodeRewind(a1),d0
	beq	.r_rewind
	cmp.w	m_escCodeSetBpm(a1),d0
	beq	.r_chgbpm
	cmp.w	m_escCodeGetPos(a1),d0
	bne	.cmdExec
.r_setPos
	move.b	(a0)+,(m_currentSeq+1)(a1)
	bra	.process

.r_rewind	
	move.l	m_byteStreamLoop(a1),a0
	move.l	m_wordStreamLoop(a1),m_wordStream(a1)
	bra	.process

.r_chgbpm
	move.b	(a0)+,(m_currentBpm+1)(a1)	; BPM
	bra	.process

.resetv	dc.l	0,0,0,0

	rsreset	
m_byteStream		rs.l	1	;  0 byte stream
m_wordStream		rs.l	1	;  4 word stream
m_dmaconPatch		rs.l	1	;  8 dmacon
m_codeTableAddr		rs.l	1	; 12 code table addr
m_escCodeRewind		rs.w	1	; 16 rewind special escape code
m_escCodeSetBpm		rs.w	1	; 18 set BPM escape code
m_lspInstruments	rs.l	1	; 20 LSP instruments table addr
m_relocDone		rs.w	1	; 24 reloc done flag
m_currentBpm		rs.w	1	; 26 current BPM
m_byteStreamLoop	rs.l	1	; 28 byte stream loop point
m_wordStreamLoop	rs.l	1	; 32 word stream loop point
m_seqCount		rs.w	1
m_seqTable		rs.l	1
m_currentSeq		rs.w	1
m_escCodeGetPos		rs.w	1
sizeof_LSPVars		rs.w	0

LSP_State	ds.b	sizeof_LSPVars
	even


*------	MUSIC -----------------------------------------------------------------*

lspbank	incbin	"virgill-epic.lsbank"
lspbankend
	even	; very important

lspmusic
	incbin	"virgill-epic.lsmusic",10 ; skip header (10 bytes)
	even
