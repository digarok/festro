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
** alternate mapping for fire averaging routine
srcPtrL	equz $02
srcPtrR	equz $04
srcPtrD	equz $06


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
	dw HandleLoResInit
	dw HandleFireState1
	dw HandleFireStateK
	dw HandleFireState1
	dw HandleFireStateK
	dw HandleFireState1
	dw P8Quit

HandleLoResInit
	sta LORES
	jsr ClearLoRes
	inc GDemoState
	jmp DemoMain

HandleFireState1
	ldx #30
:loop	jsr FirePass
	dex
	bne :loop
	inc GDemoState
	jmp DemoMain

HandleFireStateK
	ldx #$20
:loop	jsr FirePass2
	dex
	bne :loop
	inc GDemoState
	jmp DemoMain


FirePass	pha
	phx
	jsr MakeHeat
	jsr Scroll8
	jsr Average8
	jsr DrawBufFullScreen
	plx
	pla
	rts
FirePass2	pha
	phx
	jsr MakeHeat
	jsr Scroll8
	jsr DrawKMask
	jsr Average8
	jsr DrawBufFullScreen
	plx
	pla
	rts


**************************************************
* Color look up table.
* Fire values 0 through F get translated to these
**************************************************
	ds \
*ColorIdx	ds 16	; we copy the palettes below to here
ColorIdx		; fake it for now
	; bottom / top color
ColorIdxColor	dfb #$00	; BLK / BLK
	dfb #$22	; D.BLU / D.BLU
	dfb #$55	; D.GRY / D.GRY
	dfb #$55	; D.GRY / D.GRY
	dfb #$12	; RED / D.BLUE
	dfb #$18	; RED / BROWN
	dfb #$98	; ORNG / BROWN
	dfb #$99	; ORNG / ORNG
	dfb #$b9	; PINK / ORNG
	dfb #$Db	; YELO / PINK
	dfb #$DD	; YELO / YELO
	dfb #$DD	; YELO / YELO
	dfb #$FD	; WHITE / YELO
	dfb #$FF	; WHITE / WHITE
	dfb #$FF	; WHITE / WHITE
	dfb #$FF	; WHITE / WHITE

ColorIdxMono
	dfb #$00	; BLK / BLK
	dfb #$11	; D.BLU / D.BLU
	dfb #$08	; D.GRY / D.GRY
	dfb #$42	; D.GRY / D.GRY
	dfb #$43	; RED / D.BLUE
	dfb #$53	; RED / BROWN
	dfb #$55	; ORNG / BROWN
	dfb #$65	; ORNG / ORNG
	dfb #$67	; PINK / ORNG
	dfb #$97	; YELO / PINK
	dfb #$F7	; YELO / YELO
	dfb #$FD	; YELO / YELO
	dfb #$FF	; WHITE / YELO
	dfb #$FF	; WHITE / WHITE
	dfb #$FF	; WHITE / WHITE
	dfb #$FF	; WHITE / WHITE

FBufWidth	equ #40
FBufHeight	equ #24
	ds \
FBUF	ds #FBufWidth*#FBufHeight+#FBufWidth	;extra line gets coals
FBufLen	equ #FBufWidth*#FBufHeight
FBufLastLine	equ #FBufWidth*#FBufHeight-#FBufWidth+FBUF


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
	sta Lo11+10,x
	inx
	phx
	lda #18
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
_digawriteString	asc "--==>> DiGAROK <<==--",00
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
	lda #16
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
	lda #3
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


FireTextHeight equ #18	; buffer height
FireTextWidth	equ #34	; buffer width

	ds \
FireText	asc "    __         __                ",00
	asc "   / /   ___  / /______          ",00
	asc "  / /   / _ \/ __/ ___/          ",00
	asc " / /___/  __/ /_(__  )           ",00
	asc "/_____/\___/\__/____/            ",00
              asc "                                 ",00
	asc "     _______              ______ ",00
	asc "    / ____(_)_______     /  _/ /_",00
	asc "   / /_  / / ___/ _ \    / // __/",00
	asc "  / __/ / / /  /  __/  _/ // /_  ",00
	asc " /_/   /_/_/   \___/  /___/\__/  ",00
	asc "                                 ",00
	asc "         __  __      __          ",00
	asc "        / / / /___  / /          ",00
	asc "       / / / / __ \/ /           ",00
	asc "      / /_/ / /_/ /_/            ",00
	asc "      \____/ .___(_)             ",00
	asc "          /_/                    ",00

*********************
* DEMO ONLY!
* x=10,y=10
*********************
_kOffset	equ #11

DrawKMask	ldx #0
:loop	lda _kWidth*0+_spriteK,x
	beq :skip1
	cmp #1
	bne :notRand1
	jsr GetRandLow
:notRand1	sta FBufWidth*6+FBUF+_kOffset,x
:skip1	lda _kWidth*1+_spriteK,x
	beq :skip2
	cmp #1
	bne :notRand2
	jsr GetRandLow
:notRand2	sta FBufWidth*7+FBUF+_kOffset,x
:skip2	lda _kWidth*2+_spriteK,x
	beq :skip3
	sta FBufWidth*8+FBUF+_kOffset,x
:skip3	lda _kWidth*3+_spriteK,x
	beq :skip4
	cmp #1
	bne :notRand4
	jsr GetRandLow
:notRand4	sta FBufWidth*9+FBUF+_kOffset,x
:skip4	lda _kWidth*4+_spriteK,x
	beq :skip5
	sta FBufWidth*10+FBUF+_kOffset,x
:skip5	lda _kWidth*5+_spriteK,x
	beq :skip6
	sta FBufWidth*11+FBUF+_kOffset,x
:skip6	lda _kWidth*6+_spriteK,x
	beq :skip7
	sta FBufWidth*12+FBUF+_kOffset,x
:skip7	lda _kWidth*7+_spriteK,x
	beq :skip8
	sta FBufWidth*13+FBUF+_kOffset,x
:skip8	lda _kWidth*8+_spriteK,x
	beq :skip9
	sta FBufWidth*14+FBUF+_kOffset,x
:skip9	lda _kWidth*9+_spriteK,x
	beq :skip10
	sta FBufWidth*15+FBUF+_kOffset,x
:skip10	lda _kWidth*10+_spriteK,x
	beq :skip11
	sta FBufWidth*16+FBUF+_kOffset,x
:skip11	lda _kWidth*11+_spriteK,x
	beq :skip12
	sta FBufWidth*17+FBUF+_kOffset,x
:skip12	lda _kWidth*12+_spriteK,x
	beq :skip13
	sta FBufWidth*18+FBUF+_kOffset,x
:skip13	lda _kWidth*13+_spriteK,x
	beq :skip14
	sta FBufWidth*19+FBUF+_kOffset,x
:skip14	lda _kWidth*14+_spriteK,x
	beq :skip15
	sta FBufWidth*20+FBUF+_kOffset,x
:skip15	lda _kWidth*15+_spriteK,x
	beq :skip16
	sta FBufWidth*21+FBUF+_kOffset,x
:skip16

	inx
	cpx #_kWidth
	beq :done
	jmp :loop
:done
	rts
_kWidth	equ #18
_kHeight	equ #16
_spriteK	db $01,$01,$01,$01,$01,$01,$01,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01
	db $01,$0F,$0F,$0F,$0F,$0F,$01,$00,$00,$00,$00,$01,$0F,$0F,$0F,$0F,$0F,$01
	db $01,$0F,$0F,$0F,$0F,$0F,$01,$00,$00,$00,$00,$01,$0F,$0F,$0F,$0F,$0F,$01
	db $01,$0F,$0F,$0F,$0F,$0F,$01,$00,$00,$00,$01,$0F,$0F,$0F,$0F,$0F,$0F,$01
	db $01,$0F,$0F,$0F,$0F,$0F,$01,$00,$00,$01,$0F,$0F,$0F,$0F,$0F,$01,$01,$00
	db $01,$0F,$0F,$0F,$0F,$0F,$01,$00,$01,$0F,$0F,$0F,$0F,$0F,$01,$00,$00,$00
	db $01,$0F,$0F,$0F,$0F,$0F,$0F,$01,$0F,$0F,$0F,$0F,$0F,$01,$00,$00,$00,$00
	db $01,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$01,$00,$00,$00,$00,$00
	db $01,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$01,$00,$00,$00,$00,$00
	db $01,$0F,$0F,$0F,$0F,$0F,$0F,$01,$0F,$0F,$0F,$0F,$0F,$01,$00,$00,$00,$00
	db $01,$0F,$0F,$0F,$0F,$0F,$01,$00,$01,$0F,$0F,$0F,$0F,$0F,$01,$00,$00,$00
	db $01,$01,$0F,$0F,$0F,$0F,$01,$00,$00,$01,$0F,$0F,$0F,$0F,$0F,$01,$01,$00
	db $01,$0F,$0F,$0F,$0F,$0F,$01,$00,$00,$00,$01,$0F,$0F,$0F,$0F,$0F,$0F,$01
	db $01,$0F,$0F,$0F,$0F,$0F,$01,$00,$00,$00,$00,$01,$0F,$0F,$0F,$0F,$0F,$01
	db $01,$0F,$0F,$0F,$0F,$0F,$01,$00,$00,$00,$00,$01,$0F,$0F,$0F,$0F,$0F,$01
	db $01,$01,$01,$01,$01,$01,$01,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01

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

GetRandLow
	lda _randomByte2
	beq :doEor
	asl
	bcc :noEor
:doEor	eor #$1d
:noEor	sta _randomByte2
	cmp #$80
	bcs :hot
	lda #$0
	rts
:hot	lda #$04
	rts

_randomByte2	db 0


**************************************************
* Fire stoker.. returns 0 or F
**************************************************
GetRandHot
	lda _rndHot
	beq :doEor
	asl
	bcc :noEor
:doEor	eor #$1d
:noEor	sta _rndHot
	cmp #$90	; FIRE RATIO
	bcs :hot
:not	lda #$0f
	rts
:hot	lda #$00
	rts
_rndHot	db 0

**************************************************
* Very simple routine to lay down a line where
* all values are either 0 (cold) or F (hot)
**************************************************
MakeHeat
	lda #0
	sta FBufLastLine	;FORCE CORNERS BLACK
	sta FBufLastLine+#39
	;EXTRA LINE
	sta FBufLastLine+#FBufWidth	;FORCE CORNERS BLACK
	sta FBufLastLine+#FBufWidth+#39

	ldx #FBufWidth-2
:mloop	jsr GetRandHot
	sta FBufLastLine,x
	jsr GetRandHot
	sta FBufLastLine+#FBufWidth,x
	dex
	bne :mloop

	rts

**************************************************
* Scrolls Fire buffer up one line
**************************************************
Scroll8   
*set source
	lda #FBUF+FBufWidth
	sta srcPtr
	lda #>FBUF+FBufWidth
	sta srcPtr+1

*set destination
	lda #FBUF
	sta dstPtr
	lda #>FBUF
	sta dstPtr+1

:movfwd	ldy #0
	ldx #0
	cpx #>FBufLen-FBufWidth
	beq :frag
:page	lda (srcPtr),y
	sta (dstPtr),y
	iny
	bne :page
	inc srcPtr+1
	inc dstPtr+1
	inx
	cpx #>FBufLen-FBufWidth
	bne :page
:frag	cpy #FBufLen-FBufWidth
	beq :doneCopy
	lda (srcPtr),y
	sta (dstPtr),y
	iny
	bne :frag
:doneCopy	rts


**************************************************
* Averaage Fire buffer - this is where the magic happens
**************************************************
Average8  
	lda #FBUF	; pointer to pixel
	sta srcPtr
	lda #>FBUF
	sta srcPtr+1

	lda #FBUF-#1	; pointer to pixel - 1
	sta srcPtrL
	lda #>FBUF-#1
	sta srcPtrL+1

	lda #FBUF+#1	; pointer to pixel + 1
	sta srcPtrR
	lda #>FBUF+#1
	sta srcPtrR+1

	lda #FBUF+#FBufWidth
	sta srcPtrD
	lda #>FBUF+#FBufWidth
	sta srcPtrD+1

	ldx #0	; lines

:avgLine	ldy #FBufWidth-1
:lineLoop
	clc
	lda (srcPtr),y	;0
	adc (srcPtrL),y	;-1
	adc (srcPtrR),y	;+1
	adc (srcPtrD),y	;1+width
	beq :skipDec	; all zeros then skip everything
	dec	; makes fire dissipate faster
	lsr
	lsr
:skipDec	sta (srcPtr),y	;0
	dey
	bne :lineLoop
	cpx #FBufHeight
	beq :doneLines
	inx	;next line

	;shift pointers up a "line"
	lda srcPtrD+1
	sta srcPtr+1
	sta srcPtrL+1	;\_ also copy this for math below.. not sure
	sta srcPtrR+1	;/    if it would just be better to add all pointers
	;     or go back to two pointers with iny/dey
	lda srcPtrD
	sta srcPtr

	;left pixel
	cmp #0
	bne :noPage	;if A != 0 we aren't crossing pages
	brk
	dec
	sta srcPtrL
	dec srcPtrL+1
	bra :rightPixel
:noPage	dec
	sta srcPtrL

:rightPixel	lda srcPtr
	inc
	beq :zeroFlip	;0
	sta srcPtrR
	bra :bottomPixel
:zeroFlip	sta srcPtrR
	inc srcPtrR+1

:bottomPixel	;add to bottom line pointer
	lda srcPtrD
	clc
	adc #FBufWidth
	sta srcPtrD
	lda srcPtrD+1
	adc #0
	sta srcPtrD+1
	bra :avgLine

:doneLines    rts


ClearLoRes	ldx #40
:loop	dex
	stz Lo01,x
	stz Lo02,x
	stz Lo03,x
	stz Lo04,x
	stz Lo05,x
	stz Lo06,x
	stz Lo07,x
	stz Lo08,x
	stz Lo09,x
	stz Lo10,x
	stz Lo11,x
	stz Lo12,x
	stz Lo13,x
	stz Lo14,x
	stz Lo15,x
	stz Lo16,x
	stz Lo17,x
	stz Lo18,x
	stz Lo19,x
	stz Lo20,x
	stz Lo21,x
	stz Lo22,x
	stz Lo23,x
	stz Lo24,x
	bne :loop
	rts 

**************************************************
* Draw entire buffer on screen
**************************************************
DrawBufFullScreen
	ldx #$0
:loop0    ldy FBUF,x
	lda ColorIdx,y
	sta Lo01,x
	inx
	cpx #FBufWidth
	bne :loop0

	ldx #$0
:loop1    ldy FBufWidth*1+FBUF,x
	lda ColorIdx,y
	sta Lo02,x
	inx
	cpx #FBufWidth
	bne :loop1

	ldx #$0
:loop2    ldy FBufWidth*2+FBUF,x
	lda ColorIdx,y
	sta Lo03,x
	inx
	cpx #FBufWidth
	bne :loop2

	ldx #$0
:loop3    ldy FBufWidth*3+FBUF,x
	lda ColorIdx,y
	sta Lo04,x
	inx
	cpx #FBufWidth
	bne :loop3

	ldx #$0
:loop4    ldy FBufWidth*4+FBUF,x
	lda ColorIdx,y
	sta Lo05,x
	inx
	cpx #FBufWidth
	bne :loop4

	ldx #$0
:loop5    ldy FBufWidth*5+FBUF,x
	lda ColorIdx,y
	sta Lo06,x
	inx
	cpx #FBufWidth
	bne :loop5

	ldx #$0
:loop6    ldy FBufWidth*6+FBUF,x
	lda ColorIdx,y
	sta Lo07,x
	inx
	cpx #FBufWidth
	bne :loop6

	ldx #$0
:loop7    ldy FBufWidth*7+FBUF,x
	lda ColorIdx,y
	sta Lo08,x
	inx
	cpx #FBufWidth
	bne :loop7

	ldx #$0
:loop8    ldy FBufWidth*8+FBUF,x
	lda ColorIdx,y
	sta Lo09,x
	inx
	cpx #FBufWidth
	bne :loop8

	ldx #$0
:loop9    ldy FBufWidth*9+FBUF,x
	lda ColorIdx,y
	sta Lo10,x
	inx
	cpx #FBufWidth
	bne :loop9

	ldx #$0
:loop10   ldy FBufWidth*#10+FBUF,x
	lda ColorIdx,y
	sta Lo11,x
	inx
	cpx #FBufWidth
	bne :loop10

	ldx #$0
:loop11   ldy FBufWidth*#11+FBUF,x
	lda ColorIdx,y
	sta Lo12,x
	inx
	cpx #FBufWidth
	bne :loop11

	ldx #$0
:loop12   ldy FBufWidth*#12+FBUF,x
	lda ColorIdx,y
	sta Lo13,x
	inx
	cpx #FBufWidth
	bne :loop12

	ldx #$0
:loop13   ldy FBufWidth*#13+FBUF,x
	lda ColorIdx,y
	sta Lo14,x
	inx
	cpx #FBufWidth
	bne :loop13

	ldx #$0
:loop14   ldy FBufWidth*#14+FBUF,x
	lda ColorIdx,y
	sta Lo15,x
	inx
	cpx #FBufWidth
	bne :loop14

	ldx #$0
:loop15   ldy FBufWidth*#15+FBUF,x
	lda ColorIdx,y
	sta Lo16,x
	inx
	cpx #FBufWidth
	bne :loop15

	ldx #$0
:loop16   ldy FBufWidth*#16+FBUF,x
	lda ColorIdx,y
	sta Lo17,x
	inx
	cpx #FBufWidth
	bne :loop16

	ldx #$0
:loop17   ldy FBufWidth*#17+FBUF,x
	lda ColorIdx,y
	sta Lo18,x
	inx
	cpx #FBufWidth
	bne :loop17

	ldx #$0
:loop18   ldy FBufWidth*#18+FBUF,x
	lda ColorIdx,y
	sta Lo19,x
	inx
	cpx #FBufWidth
	bne :loop18

	ldx #$0
:loop19   ldy FBufWidth*#19+FBUF,x
	lda ColorIdx,y
	sta Lo20,x
	inx
	cpx #FBufWidth
	bne :loop19

	ldx #$0
:loop20   ldy FBufWidth*#20+FBUF,x
	lda ColorIdx,y
	sta Lo21,x
	inx
	cpx #FBufWidth
	bne :loop20

	ldx #$0
:loop21   ldy FBufWidth*#21+FBUF,x
	lda ColorIdx,y
	sta Lo22,x
	inx
	cpx #FBufWidth
	bne :loop21

	ldx #$0
:loop22   ldy FBufWidth*#22+FBUF,x
	lda ColorIdx,y
	sta Lo23,x
	inx
	cpx #FBufWidth
	bne :loop22

	ldx #$0
:loop23   ldy FBufWidth*#23+FBUF,x
	lda ColorIdx,y
	sta Lo24,x
	inx
	cpx #FBufWidth
	bne :loop23
	rts


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
