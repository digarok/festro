**************************************************
* FESTRO
* @author Dagen Brock <dagenbrock@gmail.com>
* @date   2012-11-11
*
* Style Guide:
*  ALLCAPS     Apple II ROM "Constants"
*  CamelCase   function names
*  ]loops      loops (duh)
*  GSomething  Global application constants
*  _myVar      local variables
**************************************************
	org $2000
	typ #$ff
	xc off	; 65c02
	xc
	lst off

**************************************************
* Global Addresses / Pointers
**************************************************
srcPtr	equz $00
dstPtr	equz $02


**************************************************
* Main Demo Controller
**************************************************
DemoMain
:mainLoop
	jsr KeyHandler
	lda GDemoState
	asl
	tax
	jmp (DemoSubroutineTable,x)
	bra :mainLoop

DemoSubroutineTable
	dw HandleProdrop
	dw HandleDigawrite
	dw HandleProdrop
	dw HandleSwipeWrite
	dw HandleProdrop
	dw P8Quit



HandleSwipeWrite
]initSwipe
	ldx #$23	; reset line indexes
:clearLoop	stz _swipeLinesX,x	;
	dex
	bne :clearLoop
	stz _swipeActive	; reset overall index

* @todo make parameters ******************************
	lda #FireTextHeight
	sta _swipeMaxHeight	; set max height
	lda #FireTextWidth
	sta _swipeMaxWidth	; set max width
	lda #5
	sta _swipeXOffset	; set x position
	lda #6
	sta _swipeYOffset	; set y position

** DA LOOP!!! does one full pass of all lines.. this is the OUTERMOST LOOP
:swipeLoop
	lda #1
	sta _swipeLinesDone	; set true... flips false every time we find a char
	lda _swipeMaxHeight	; increment active set - once per loop, up to max height
	cmp _swipeActive
	beq :swipeMaxHit
	inc _swipeActive	
:swipeMaxHit
	stz _swipeCurrLine	; start with line 0

:swipeLineLoop
	lda _swipeCurrLine
	ldx _swipeMaxWidth	
	jsr Multiply8x8	; multiply bufferwidth * linenumber

	sta srcPtr
	stx srcPtr+1		; store our multiplication offset
	clc		
	adc #FireText		; and add the buffer location
	sta srcPtr
	lda srcPtr+1
	adc #>FireText
	sta srcPtr+1		; srcPtr points to first character of that line in buffer

	lda _swipeCurrLine
	clc
	adc _swipeYOffset	; linenumber + y-offset
	asl		; x2 for pointer
              tax	
	lda LoLineTable,x           ; now convert to ZP pointer
	clc
	adc _swipeXOffset	; but also add our screen x-offset
	sta dstPtr
	bcc :noAdd
	lda LoLineTable+1,x	; rolled our low-byte so add 1 to high-byte
	inc 
	bra :skip
:noAdd	lda LoLineTable+1,x	
:skip	sta dstPtr+1		; destptr points to first character of where this line is positioned on screen

	ldx _swipeCurrLine
	lda _swipeLinesX,x	
	tay		; get current character index
	lda (srcPtr),y	; get char from buffer
	beq :lineIsDone	; was it #$00?
	sta (dstPtr),y	; store char to screen
	inc _swipeLinesX,x	; next char
	stz _swipeLinesDone	; found a char so we always set this false
	

:lineIsDone	
	inc _swipeCurrLine
	lda _swipeCurrLine
	cmp _swipeActive
	bne :swipeLineLoop
	
	lda #20
	tax
	tay
	jsr SimpleWait

	lda _swipeLinesDone
	beq :swipeLoop
	inc GDemoState
	jmp DemoMain
* for (line =0 ; line < active	; line ++ ) {
*  sourceptr = bufferwidth * line
*  destPtr = ^lores[line+yoffset] + xoffset
*  ldy arrayX[line]
*  lda (sourceptr),y
*  if (a != 0)
*   sta (destptr),y
*   inc arrayX[line]
*  else
*   linesdone ++
* }
* if (linesdone == maxlines) return
* else swipepassloop
_swipeLinesDone	db 0 ; how many lines are fully complete
_swipeCurrLine	db 0	; drawing loop line index
_swipeActive	db 0	; number of lines to be updating
_swipeLinesX	ds 24	; array with x position of each line
_swipeMaxHeight	db #$0	; set # lines in the source buffer
_swipeMaxWidth	db #0	; set # characters per line in the source buffer
_swipeXOffset	db #$0	; screen offset for placement
_swipeYOffset	db #$0	; screen offset for placement


FireTextHeight equ #12	; buffer height
FireTextWidth	equ #34	; buffer width

	ds \
FireText	asc "    _______              ______  ",00
	asc "   / ____(_)_______     /  _/ /_ ",00
	asc "  / /_  / / ___/ _ \    / // __/ ",00
	asc " / __/ / / /  /  __/  _/ // /_   ",00
	asc "/_/   /_/_/   \___/  /___/\__/   ",00
	asc "                                 ",00
	asc "         __  __      __          ",00
	asc "        / / / /___  / /          ",00
	asc "       / / / / __ \/ /           ",00
	asc "      / /_/ / /_/ /_/            ",00
	asc "      \____/ .___(_)             ",00
	asc "          /_/                    ",00




**************************************************
* Demo-Part Controllers
**************************************************
HandleDigawrite
	lda #40
	tax
	tay
	jsr SimpleWait

	ldx #0
]writeLoop
	lda _digawriteString,x
	beq ]writeDone
	sta Lo11+17,x
	inx
	phx
	lda #20
	tax
	tay
	jsr SimpleWait
	plx
	bra ]writeLoop
]writeDone
	lda #$30
	tax
	tay
	jsr SimpleWait

	inc GDemoState
	jmp DemoMain
_digawriteString	asc "DiGAROK",00
** Dropper routine - not specific to ProDrop per se
** - uses DSEG0
HandleProdrop
	lda _prodropState
	beq :prodropScan
	jmp :prodropUpdate

:prodropScan
	lda #$10
	tax
	tay
	jsr SimpleWait	; we actually pause a bit just for dramatic effect
	lda #0	; start scan at line 0 every time
	sta _prodropScanLine

	lda #DSEG0	; initialize destination pointer
	sta dstPtr
	lda #>DSEG0
	sta dstPtr+1

]scanLineLoop
	lda _prodropScanLine
	rol	; (line * 2) for table index
	tax
	lda LoLineTable,x
	sta srcPtr
	lda LoLineTable+1,x
	sta srcPtr+1

	ldy #0
]scanCharLoop
	lda (srcPtr),y
	cmp #" "
	beq ]nextChar
	and #%01111111	; clear high bit to indicate non-animated state

	phy	; +1
	phy	; +2
	ldy #0
	sta (dstPtr),y
	iny
	pla	; +1
	sta (dstPtr),y
	lda _prodropScanLine
	iny
	sta (dstPtr),y
	iny
	jsr GetRand
	and #%0111111	; decided to limit it to make the values a little
	; closer together.  #$00 - #$7f
	; this particularly helps with gaps when nothing is
	; falling because you started with few characters
	sta (dstPtr),y
	ply	; 0


	lda dstPtr	; now add 4
	clc
	adc #$04
	sta dstPtr
	bcc ]nextChar
	inc dstPtr+1	; increment page (high byte)
]nextChar
	iny
	cpy #40
	bne ]scanCharLoop

	inc _prodropScanLine
	lda _prodropScanLine
	cmp #24
	bne ]scanLineLoop

	; we're done scanning
	lda #$FF
	sta (dstPtr)	; set terminator byte

	inc _prodropState

	jmp DemoMain
	rts	;; @todo: add rts support to jmp table stuff?

]prodropAnimDone
	lda #0
	sta _prodropState	; uhg..reset state before exit so can be called again
	inc GDemoState	; ugh again... not great..  using globals
	jmp DemoMain

:prodropUpdate
	lda #0	; finished = false
	sta _prodropAnimDone


:prodropUpdateLoop
	lda #20
	tax
	tay
	jsr SimpleWait


	lda _prodropAnimDone
	bne ]prodropAnimDone

	lda #1	; set finished = true before we start our char
	sta _prodropAnimDone	; loop, any time we find active char set false

	lda #DSEG0	; initialize source pointer (our character buffer)
	sta srcPtr
	lda #>DSEG0
	sta srcPtr+1

:prodropAnimLoop
]isChar
	lda (srcPtr)
	beq ]nextAnimChar
	cmp #$FF	; array termination byte
	beq :prodropUpdateLoop	; reset src pointer and do it all over
	stz _prodropAnimDone	; hate to continually set this false, but we have time

	cmp #$80	; check for high bit
	bcs ]dropIt	; not set? then we're animating it
	ldy #3	; check random wait state
	lda (srcPtr),y
	beq ]setCharAnim	; is 0 so set it to animate on next pass
	dec	; otherwise decrement wait state
	sta (srcPtr),y
	bra ]nextAnimChar
]dropIt
	ldy #2
	lda (srcPtr),y	; get Y value
	asl
	tax
	lda LoLineTable,x	; convert to ZP pointer
	sta dstPtr
	lda LoLineTable+1,x
	sta dstPtr+1
	ldy #1
	lda (srcPtr),y	; get our X value
	tay
	lda #" "
	sta (dstPtr),y
* breath... holy crap all that just to draw a space at X,Y
	ldy #2
	lda (srcPtr),y	; get Y value
	inc	; move it down
	cmp #24
	beq ]charFinished
	sta (srcPtr),y
	asl
	tax
	lda LoLineTable,x	; convert to ZP pointer
	sta dstPtr
	lda LoLineTable+1,x
	sta dstPtr+1
	ldy #1	; get X value
	lda (srcPtr),y
	tay
	lda (srcPtr)	; get real character
	sta (dstPtr),y	; draw that f***er!
	bra ]nextAnimChar

]charFinished
	lda #0
	sta (srcPtr)
	bra ]nextAnimChar

]setCharAnim
	lda (srcPtr)
	ora #%10000000	; set high bit
	sta (srcPtr)


]nextAnimChar
	lda srcPtr
	clc
	adc #4	; struct size
	sta srcPtr
	bcc :prodropAnimLoop
	inc srcPtr+1	; next page
	bra :prodropAnimLoop

* done = true
* foreach DSEG0-blahblah
*  if 00
*   skip
*  else
*   done = false
*
*  if highbit
*   if random 0
*    then strip high bit and continue (beep?)
*   else dec random
*  else
*   store space at x,y
*   inc y
*   if y = 25
*    stz over char
*   else
*    store char at x,y


_prodropAnimDone	db 1	; any time we find a character it flips this false
_prodropScanLine	db 0	; starts scanning at line 0
_prodropState	db 0	; starts with 0, which is scan mode




**************************************************
* Called by DemoMain
**************************************************
KeyHandler
	lda KEY 
	cmp #$80
	blt ]noKey
	sta STROBE
	sta GKeyEvent
	cmp #"q"
	beq P8Quit
]noKey	rts


P8Quit	jsr $bf00
	dfb $65
	da qparms
	rts
qparms	dfb 4
	dfb 0
	da $0000
	dfb 0
	da $0000

**************************************************
* Awesome PRNG thx to White Flame (aka David Holz)
**************************************************
GetRand
	lda _randomByte
	beq :doEor
	asl
	bcc :noEor
:doEor	eor #$1d
:noEor	sta _randomByte
	rts

_randomByte	db 0

**************************************************
* SafeWait
* -silly triple loop, preserves AXY
**************************************************
SimpleWait
	sta _waitA
	stx _waitX
	sty _waitY

	lda _waitA
:parentWait
	ldx _waitX
:secondWait
	ldy _waitY
:innerWait
	dey
	bne :innerWait
	dex
	bne :secondWait
	dec
	bne :parentWait
:waitDone
	lda _waitA
	ldx _waitX
	ldy _waitY
	rts
_waitA	db 0
_waitX	db 0
_waitY	db 0



**************************************************
* Multiply8x8
*  params: a,x = 8bit multipliers
*  result: stored in _multiplyResult and returned in a,x 
**************************************************
Multiply8x8 
	stz _multiplyResult
	stz _multiplyResult+1
	cmp #0		; short circuit on A = 0
	beq :multiplyDone
	cpx #0		; short circuit on X = 0
	beq :multiplyDone
	tay
:loop	tya		; sorry, i suck at asm maths routines
	clc 
	adc _multiplyResult
	sta _multiplyResult
	bcc :next
	inc _multiplyResult+1
:next	dex
	bne :loop

:multiplyDone lda _multiplyResult
	ldx _multiplyResult+1
	rts
_multiplyResult dw 0000


**************************************************
* Apple Standard Memory Locations
* @todo: finalize as include?
**************************************************
CLRLORES	EQU $F832
LORES	EQU $C050
TXTSET	EQU $C051
MIXCLR	EQU $C052
MIXSET	EQU $C053
KEY	EQU $C000
STROBE	EQU $C010
SPEAKER	EQU $C030
VBL	EQU $C02E

**************************************************
* Lores/Text lines
* @todo: finalize as include?
**************************************************
Lo01	equ $400
Lo02	equ $480
Lo03	equ $500
Lo04	equ $580
Lo05	equ $600
Lo06	equ $680
Lo07	equ $700
Lo08	equ $780
Lo09	equ $428
Lo10	equ $4a8
Lo11	equ $528
Lo12	equ $5a8
Lo13	equ $628
Lo14	equ $6a8
Lo15	equ $728
Lo16	equ $7a8
Lo17	equ $450
Lo18	equ $4d0
Lo19	equ $550
Lo20	equ $5d0
* the "plus four" lines
Lo21	equ $650
Lo22	equ $6d0
Lo23	equ $750
Lo24	equ $7d0

LoLineTable	da Lo01,Lo02,Lo03,Lo04,Lo05,Lo06
	da Lo07,Lo08,Lo09,Lo10,Lo11,Lo12
	da Lo13,Lo14,Lo15,Lo16,Lo17,Lo18
	da Lo19,Lo20,Lo21,Lo22,Lo23,Lo24



**************************************************
* Data Segments
**************************************************
	ds \
DSEG0     ds 1024	; General 1K Data Store


**************************************************
* Global Variables
**************************************************

GKeyEvent	db 0	; keypress - allow subroutines to access
GDemoState	db 0	; current demo state


**************************************************
* State 'Enumerators'
**************************************************

DemoStateProdrop	equ #0	; P8 screen shows and letters drop off
DemoStateDigawrite	equ #1	; writes 'DiGAROK' and pauses
DemoStateDigadrop	equ #2	; Really same as DemoStateProdrop
DemoStateFire	equ #3	; Fire has a lot going on so has own substates
DemoStateAppleText	equ #4	; Draws ascii apple + greets/shouts
DemoStateGRSprite	equ #5	; GR mode sprite anim plus bottom text writer
DemoStateExit	equ #6	; Not sure... draw info text? just quit?

ProdropStateScan	equ #0	; Scans all characters into our data structure
ProdropStateUpdate	equ #1	; Does one round of character updates, buffer&screen
ProdropStateDone	equ #2	; Really just to let the callee(s) know it's all done



FireStatePuff	equ #0	; flame code with custom heat (puff) generator
FireStateFire1 equ #1	; lower rndhot?  or not...
FireStateLetterK	equ #2	; draws K, waits, poof
FireStateLetterF	equ #3	; draws F, waits, poof
FireStateLetterE	equ #4	; draws E, waits, poof
FireStateLetterS	equ #5	; draws S, waits, poof
FireStateLetterT	equ #6	; draws T, waits, poof
FireStateKFEST	equ #7	; draws logo, waits, poof
FireStateFlareOut	equ #8	; maybe do a flame up or fizzle out



	lst on
	sav /code/festro.sys
