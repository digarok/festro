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
* Demo Machine initialization
**************************************************
	jsr DetectIIgs
	jsr InitState


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
	dw HandleScan01
	dw HandleTextClear
	dw HandleShortWait
	dw HandleStarScroll
	dw HandleScan02
	dw HandleTextClear
	dw HandleMapScroll
	dw HandleScan03

	dw HandleLoResInit
	dw SetFireRatio20
	dw HandleFireState1
	dw SetFireRatio90
	dw HandleFireState1
	dw HandleFireStateK
	dw HandleFireState1
	dw HandleFireStateF
	dw HandleFireState1
	dw HandleFireStateE
	dw HandleFireState1
	dw HandleFireStateS
	dw HandleFireState1
	dw HandleFireStateT
	dw HandleFireState1
	dw HandleFireState1
	dw HandleFireStateYear
	dw HandleFireState1
	dw SetFireRatio20
	dw HandleFireState1
	dw HandleFireState1
	dw SetFireRatio01
	dw HandleFireState1
	dw HandleKfestLogo
	dw HandleMedWait
	dw HandleShortWait
	dw HandleSplitSlide

	dw HandleAppleDraw
	dw SetProdropGr
	dw HandleProdrop
	dw SetProdropText
	dw HandleShortWait
	dw HandleTextClear

	dw HandleSwipeWrite
	dw HandleShortWait
	dw HandleGreetScroll
	dw HandleShortWait
	dw HandleProdrop
	dw HandleTextClear
	dw HandleFinalScreen
	dw P8Quit



HandleFinalScreen
	lda #FinalText
	sta srcPtr
	lda #>FinalText
	sta srcPtr+1

	lda #0
:lineLoop	pha
	asl
	tax
	lda LoLineTable,x
	sta dstPtr
	lda LoLineTable+1,x
	sta dstPtr+1
	ldy #0
:copyLoop	lda (srcPtr),y
	beq :nextLine
	cmp #$ff
	beq :done
	sta (dstPtr),y
	iny
	lda #2
	jsr SimplerWait
	bra :copyLoop
:nextLine	iny
	tya
	clc
	adc srcPtr
	sta srcPtr
	bcc :noCarry
	inc srcPtr+1
:noCarry	pla
	inc
	bra :lineLoop

:done	pla	;woops

* FALLTHROUGH to menu

	stz DSEG0
:noKey
	lda #$6
	jsr SimplerWait
	lda DSEG0
	beq :blink
	dec DSEG0
	lda #"?"
	bra :store
:blink	inc DSEG0
	lda #" "
:store	sta Lo21+23
	bra :chkKey
:chkKey	lda KEY 
	cmp #$80
	blt :noKey
	sta STROBE
	cmp #"f"
	beq :goFire
	cmp #"F"
	beq :goFire
	inc GDemoState
	jmp DemoMain
:goFire	bra HiddenFire

HiddenFire 
              lda #$90
	sta GFireRatio
	sta LORES
	jsr ClearLoRes
:noKey	jsr FirePass
	lda KEY
	cmp #$80
	blt :noKey
	sta STROBE
              sta TXTSET
              jsr ClearText
	jmp DemoMain
	

HandleGreetScroll
	lda #_cwoz
	sta srcPtr
	lda #>_cwoz
	sta srcPtr+1
	ldx #23
	ldy #21
	lda #$30
	jsr DrawStringXYWait
	lda #$30
	jsr SimplerWait

:loop	jsr ScrollRightUp
	inc _creditScrollCounter
	lda #26
	jsr SimplerWait

	lda _creditScrollTick
	cmp #01
	beq :next
	inc _creditScrollTick
	lda #$13
	jsr SimplerWait
	bra :loop
:next	stz _creditScrollTick
	lda _creditStringIdx
	asl 
	tax
	lda _creditStringsTable+1,x
	beq :noStrings
	sta srcPtr+1
	lda _creditStringsTable,x
	sta srcPtr

	inc _creditStringIdx	;++

	ldx #23
	ldy #23
	lda #$10
	jsr DrawStringXYWait
	bra :skipWait
:noStrings
	lda #$08
	jsr SimplerWait
:skipWait	lda _creditScrollCounter
	cmp #80
	beq :done
	bra :loop

:done	inc GDemoState
	jmp DemoMain



_creditScrollTick db #$00
_creditScrollCounter db #$00
_creditStringIdx db #$00


HandleMapScroll
	ldx #WorldMapWidth-40
	jsr DrawMapOffset
	lda #$25
	jsr SimplerWait
	ldx #WorldMapWidth-40
:scrollLoop	phx
	jsr DrawMapOffset
	lda #$20
	jsr SimplerWait
	plx 
	dex
	bne :scrollLoop
	ldx #5
:blinkenLoop	phx
	lda #$13
	jsr SimplerWait
	lda #" "
	sta Lo07+16
	lda #$20
	jsr SimplerWait
	lda #"*"
	sta Lo07+16
	plx
	dex
	bne :blinkenLoop


	lda #$03
:flashenLoop	pha
	lda #_scanStr09b	;Scanning BLANK
	sta srcPtr
	lda #>_scanStr09b
	sta srcPtr+1
	ldx #15
	ldy #08
	lda #$10
	jsr DrawStringXYWait
	lda #$10
	jsr SimplerWait

	lda #_scanStr09	;Scanning
	sta srcPtr
	lda #>_scanStr09
	sta srcPtr+1
	ldx #15
	ldy #08
	lda #$18
	jsr DrawStringXYWait
	lda #$20
	jsr SimplerWait
	pla
	dec
	bne :flashenLoop
	lda #$30
	jsr SimplerWait

	inc GDemoState
	jmp DemoMain

DrawMapOffset
	ldy #0

:drawLoop	lda WorldMap,x
	sta Lo01,y
	lda WorldMapWidth*1+WorldMap,x
	sta Lo02,y
	lda WorldMapWidth*2+WorldMap,x
	sta Lo03,y
	lda WorldMapWidth*3+WorldMap,x
	sta Lo04,y
	lda WorldMapWidth*4+WorldMap,x
	sta Lo05,y
	lda WorldMapWidth*5+WorldMap,x
	sta Lo06,y
	lda WorldMapWidth*6+WorldMap,x
	sta Lo07,y
	lda WorldMapWidth*7+WorldMap,x
	sta Lo08,y
	lda WorldMapWidth*8+WorldMap,x
	sta Lo09,y
	lda WorldMapWidth*9+WorldMap,x
	sta Lo10,y
	lda WorldMapWidth*10+WorldMap,x
	sta Lo11,y
	lda WorldMapWidth*11+WorldMap,x
	sta Lo12,y
	lda WorldMapWidth*12+WorldMap,x
	sta Lo13,y
	lda WorldMapWidth*13+WorldMap,x
	sta Lo14,y
	lda WorldMapWidth*14+WorldMap,x
	sta Lo15,y
	lda WorldMapWidth*15+WorldMap,x
	sta Lo16,y
	lda WorldMapWidth*16+WorldMap,x
	sta Lo17,y
	lda WorldMapWidth*17+WorldMap,x
	sta Lo18,y
	lda WorldMapWidth*18+WorldMap,x
	sta Lo19,y
	lda WorldMapWidth*19+WorldMap,x
	sta Lo20,y
	lda WorldMapWidth*20+WorldMap,x
	sta Lo21,y
	lda WorldMapWidth*21+WorldMap,x
	sta Lo22,y
	lda WorldMapWidth*22+WorldMap,x
	sta Lo23,y
	lda WorldMapWidth*23+WorldMap,x
	sta Lo24,y
	inx
	iny
	cpy #40
	beq :done
	jmp :drawLoop
:done	rts 

HandleStarScroll
_defaultStarSpeed equ #$10
	lda #$1e
:speedUp	ldx #5
	pha
	ldy #0	; nosong
	jsr StarScrollAuto
	pla 
	dec
	cmp #_defaultStarSpeed
	bne :speedUp
* full speed
	ldx #50
	lda #_defaultStarSpeed
	ldy #0	; nosong
	jsr StarScrollAuto

* --- diga
	ldx #$0
:loop	phx
	lda #_defaultStarSpeed	; waitfirst
	jsr SimplerWait
	jsr VBlank
	jsr ScrollLeft
	jsr GenStarRight
	plx 
	lda _digawriteString,x
	beq :digawriteDone	; done (zero terminated string)
	sta Lo12+39
	inx 
	bra :loop

* slow down?
:digawriteDone
	lda #_defaultStarSpeed
:slowDown	ldx #1
	pha
	ldy #0	; nosong
	jsr StarScrollAuto
	pla 
	inc
	inc
	inc
	inc
	cmp #8*4+#_defaultStarSpeed 
	bne :slowDown
	lda #$30
	jsr SimplerWait



* speed up again

	lda #_defaultStarSpeed+5
:speedUpAgain	ldx #2
	pha
	jsr StarScrollAuto
	pla 
	dec
	cmp #_defaultStarSpeed
	bne :speedUpAgain ;) 

	ldx #220
	lda #_defaultStarSpeed
	ldy #1	;SONG
	jsr StarScrollAuto

* second loop inserts planet
	ldx #EarthTextWidth
:loop2	phx
	jsr VBlank
	jsr ScrollLeft
	jsr GenStarRight
	lda _earthOffset
	jsr DrawEarthLine
	inc _earthOffset
	lda #_defaultStarSpeed

	pha
	phx
	phy
	ldx #$30
	jsr PlaySong01Note
	ply
	plx
	pla
	;jsr SimplerWait
	plx
	dex
	bne :loop2
* third loop scrolls onto screen more
	lda #_defaultStarSpeed
:slowDownAgain	ldx #1
	pha
	ldy #1
	jsr StarScrollAuto
	pla 
	jsr SimplerWait	; extra wait uhg
	inc
	inc
	cmp #$28 
	bne :slowDownAgain	; i take my varibls srs
	lda #$25
	jsr SimplerWait

	inc GDemoState
	jmp DemoMain
_earthOffset	db #$00

* A = wait , X = reps, y=snd
StarScrollAuto
	sty _starScrollSound
	sta _starScrollAutoWait
:loop	phx
	jsr VBlank
	jsr ScrollLeft
	jsr GenStarRight
	ldy _starScrollSound
	beq :noSong
	ldx #$0F
	jsr PlaySong01Note
	bra :skipWait
:noSong
	lda _starScrollAutoWait
	jsr SimplerWait
:skipWait	plx
	dex
	bne :loop
	rts
_starScrollAutoWait db 0
_starScrollSound db 0



HandleLoResInit
	sta LORES
	jsr ClearLoRes
	jmp DemoNext

HandleTextClear
	sta TXTSET
	jsr ClearText
	jmp DemoNext

SetFireRatio01
	lda #$01
	sta GFireRatio
	jmp DemoNext

SetFireRatio20
	lda #$20
	sta GFireRatio
	jmp DemoNext

SetFireRatio90
	lda #$90
	sta GFireRatio
	jmp DemoNext

SetFireRatioC0
	lda #$C0
	sta GFireRatio
	jmp DemoNext

DemoNext	inc GDemoState
	jmp DemoMain

HandleFireState1
	ldx #20
:loop	jsr FirePass
	dex
	bne :loop
	inc GDemoState
	jmp DemoMain

HandleFireStateK
	lda #$20
:loop	ldx #_sprData_K	
	ldy #>_sprData_K
	jsr FirePass2	; preserves A,X,Y
	dec
	bne :loop
	inc GDemoState
	jmp DemoMain

HandleFireStateF
	lda #$20
:loop	ldx #_sprData_F	
	ldy #>_sprData_F
	jsr FirePass2	; preserves A,X,Y
	dec
	bne :loop
	inc GDemoState
	jmp DemoMain

HandleFireStateE
	lda #$20
:loop	ldx #_sprData_E	
	ldy #>_sprData_E
	jsr FirePass2	; preserves A,X,Y
	dec
	bne :loop
	inc GDemoState
	jmp DemoMain


HandleFireStateS
	lda #$20
:loop	ldx #_sprData_S	
	ldy #>_sprData_S
	jsr FirePass2	; preserves A,X,Y
	dec
	bne :loop
	inc GDemoState
	jmp DemoMain

HandleFireStateT
	lda #$20
:loop	ldx #_sprData_T	
	ldy #>_sprData_T
	jsr FirePass2	; preserves A,X,Y
	dec
	bne :loop
	inc GDemoState
	jmp DemoMain

FirePass	pha
	phx
	jsr MakeHeat
	jsr Scroll8
	jsr Average8
	jsr VBlank
	jsr DrawBufFullScreen
	jsr RandPop
	plx
	pla
	rts

HandleFireStateYear
	lda #$20
:loop	ldx #_sprData_YEAR	
	ldy #>_sprData_YEAR
	jsr FirePass3	; preserves A,X,Y
	dec
	bne :loop
	jmp DemoNext



RandPop	jsr GetRandHot
	bne :noPop
	sta SPEAKER
:noPop	rts


* A = count X=lowbyte  Y=hibyte
FirePass2	
	pha
	phx
	phy
	jsr MakeHeat
	jsr Scroll8
	jsr RandPop
	ply
	plx
	jsr DrawSpriteMask
	jsr Average8
	jsr DrawBufFullScreen
	pla
	rts

* A = count X=lowbyte  Y=hibyte
FirePass3	
	pha
	phx
	phy
	jsr MakeHeat
	jsr Scroll8
	jsr RandPop
	ply
	plx
	jsr DrawSpriteMaskBig
	jsr Average8
	jsr DrawBufFullScreen
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
HandleKfestLogo
	ldx #0
:logoLoop	lda KfestLogo,x
	sta Lo01,x
	lda KfestLogoWidth*1+KfestLogo,x
	sta Lo02,x
	lda KfestLogoWidth*2+KfestLogo,x
	sta Lo03,x
	lda KfestLogoWidth*3+KfestLogo,x
	sta Lo04,x
	lda KfestLogoWidth*4+KfestLogo,x
	sta Lo05,x
	lda KfestLogoWidth*5+KfestLogo,x
	sta Lo06,x
	lda KfestLogoWidth*6+KfestLogo,x
	sta Lo07,x
	lda KfestLogoWidth*7+KfestLogo,x
	sta Lo08,x
	lda KfestLogoWidth*8+KfestLogo,x
	sta Lo09,x
	lda KfestLogoWidth*9+KfestLogo,x
	sta Lo10,x
	lda KfestLogoWidth*10+KfestLogo,x
	sta Lo11,x
	lda KfestLogoWidth*11+KfestLogo,x
	sta Lo12,x
	lda KfestLogoWidth*12+KfestLogo,x
	bra :skip
:loop	bra :logoLoop		; i hate hate hate this
:skip	sta Lo13,x
	lda KfestLogoWidth*13+KfestLogo,x
	sta Lo14,x
	lda KfestLogoWidth*14+KfestLogo,x
	sta Lo15,x
	lda KfestLogoWidth*15+KfestLogo,x
	sta Lo16,x
	lda KfestLogoWidth*16+KfestLogo,x
	sta Lo17,x
	lda KfestLogoWidth*17+KfestLogo,x
	sta Lo18,x
	lda KfestLogoWidth*18+KfestLogo,x
	sta Lo19,x
	lda KfestLogoWidth*19+KfestLogo,x
	sta Lo20,x
	lda KfestLogoWidth*20+KfestLogo,x
	sta Lo21,x
	lda KfestLogoWidth*21+KfestLogo,x
	sta Lo22,x
	lda KfestLogoWidth*22+KfestLogo,x
	sta Lo23,x
	lda KfestLogoWidth*23+KfestLogo,x
	sta Lo24,x
	inx
	cpx #KfestLogoWidth
	bne :loop
	
	inc GDemoState
	jmp DemoMain

HandleSplitSlide
	ldy #$12
:passStart	ldx #0

:loop	lda Lo02,x
	sta Lo01,x
	lda Lo03,x
	sta Lo02,x
	lda Lo04,x
	sta Lo03,x
	lda Lo05,x
	sta Lo04,x
	lda Lo06,x
	sta Lo05,x
	lda Lo07,x
	sta Lo06,x
	lda Lo08,x
	sta Lo07,x
	lda Lo09,x
	sta Lo08,x
	lda Lo10,x
	sta Lo09,x
	lda Lo11,x
	sta Lo10,x
	lda Lo12,x
	sta Lo11,x
	lda Lo13,x
	sta Lo12,x

	lda Lo23,x
	sta Lo24,x
	lda Lo22,x
	sta Lo23,x
	lda Lo21,x
	sta Lo22,x
	lda Lo20,x
	sta Lo21,x
	lda Lo19,x
	sta Lo20,x
	lda Lo18,x
	sta Lo19,x
	lda Lo17,x
	sta Lo18,x
	lda Lo16,x
	sta Lo17,x
	lda Lo15,x
	sta Lo16,x
	lda Lo14,x
	sta Lo15,x
	lda Lo13,x
	sta Lo14,x
	stz Lo13,x
	inx 
	cpx #40
	beq :passComplete
	jmp :loop
:passComplete	dey
	beq :done
	phy
	lda #$16
	jsr SimplerWait
	ply
	jmp :passStart

:done	inc GDemoState
	jmp DemoMain



* set zero page ptr 00 to source of null terminated string
* x = x
* y = y
DrawStringXY
	pha
	phx
	phy
	tya
	asl	; get pointer to line
	tay
	lda LoLineTable,y
	sta dstPtr
	lda LoLineTable+1,y
	sta dstPtr+1
	txa
	clc
	adc dstPtr
	sta dstPtr
	bcc :noCarry
	inc dstPtr+1
:noCarry	ldy #0
:loop	lda (srcPtr),y
	beq :done
	sta (dstPtr),y
	iny 
	bra :loop
:done	ply
	plx
	pla
	rts

* set zero page ptr 00 to source of null terminated string
* A = wait
* x = x
* y = y
DrawStringXYWait
	pha
	phx
	phy
	sta _drawWait
	tya
	asl	; get pointer to line
	tay
	lda LoLineTable,y
	sta dstPtr
	lda LoLineTable+1,y
	sta dstPtr+1
	txa
	clc
	adc dstPtr
	sta dstPtr
	bcc :noCarry
	inc dstPtr+1
:noCarry	ldy #0
:loop	lda (srcPtr),y
	beq :done
	sta (dstPtr),y
	iny 
	pha
	phx
	phy
	lda #6
	ldx #1
	ldy #24
	jsr tone
	ply
	plx
	pla

	lda _drawWait
	jsr SimplerWait
	jsr VBlank
	bra :loop
:done	ply
	plx
	pla
	rts
_drawWait	db 0

HandleScan01
	lda #40
	jsr SimplerWait
* first draw box
	jsr DrawBoxAnim

	lda #$30
	jsr SimplerWait
	lda #_scanStr01	;STATUS
	sta srcPtr
	lda #>_scanStr01
	sta srcPtr+1
	ldx #_boxX+2
	ldy #_boxY+2
	jsr DrawStringXY

	lda #$30
	jsr SimplerWait

	lda #$05
:flashenLoop	pha
	lda #_scanStr08	;Scanning
	sta srcPtr
	lda #>_scanStr08
	sta srcPtr+1
	ldx #_boxX+10
	ldy #_boxY+2
	lda #$10
	jsr DrawStringXYWait
	lda #$20
	jsr SimplerWait

	lda #_scanStr08b	;Scanning BLANK
	sta srcPtr
	lda #>_scanStr08b
	sta srcPtr+1
	ldx #_boxX+10
	ldy #_boxY+2
	lda #$10
	jsr DrawStringXYWait
	lda #$15
	jsr SimplerWait
	pla
	dec
	bne :flashenLoop

	inc GDemoState
	jmp DemoMain

	
HandleScan02
	lda #40
	jsr SimplerWait
* first draw box
	jsr DrawBoxAnim
	
	lda #$30
	jsr SimplerWait
	lda #_scanStr01	;STATUS
	sta srcPtr
	lda #>_scanStr01
	sta srcPtr+1
	ldx #_boxX+2
	ldy #_boxY+2
	jsr DrawStringXY

	lda #$30
	jsr SimplerWait

	lda #_scanStr02	;Located
	sta srcPtr
	lda #>_scanStr02
	sta srcPtr+1
	ldx #_boxX+10
	ldy #_boxY+2
	lda #$10
	jsr DrawStringXYWait

	lda #_scanStr03	; virgo
	sta srcPtr
	lda #>_scanStr03
	sta srcPtr+1
	ldx #_boxX+5
	ldy #_boxY+3
	lda #$10
	jsr DrawStringXYWait

	lda #$30
	jsr SimplerWait

	lda #_scanStr04	; Local
	sta srcPtr
	lda #>_scanStr04
	sta srcPtr+1
	ldx #_boxX+5
	ldy #_boxY+4
	lda #$10
	jsr DrawStringXYWait

	lda #$30
	jsr SimplerWait

	lda #_scanStr05	; Milky
	sta srcPtr
	lda #>_scanStr05
	sta srcPtr+1
	ldx #_boxX+5
	ldy #_boxY+5
	lda #$10
	jsr DrawStringXYWait

	lda #$30
	jsr SimplerWait

	lda #_scanStr06	; Earth
	sta srcPtr
	lda #>_scanStr06
	sta srcPtr+1
	ldx #_boxX+5
	ldy #_boxY+6
	lda #$10
	jsr DrawStringXYWait

	lda #$30	
	jsr SimplerWait

	inc GDemoState
	jmp DemoMain

HandleScan03
	lda #40
	jsr SimplerWait
* first draw box
	jsr DrawBoxAnim

	lda #$30
	jsr SimplerWait
	lda #_scanStr01	;STATUS
	sta srcPtr
	lda #>_scanStr01
	sta srcPtr+1
	ldx #_boxX+2
	ldy #_boxY+2
	jsr DrawStringXY

	lda #$30
	jsr SimplerWait

	lda #_scanStr19	;awesome
	sta srcPtr
	lda #>_scanStr19
	sta srcPtr+1
	ldx #_boxX+2
	ldy #_boxY+4
	lda #$10
	jsr DrawStringXYWait
	lda #$15
	jsr SimplerWait

	lda #$03
:flashenLoop	pha
	lda #_scanStr20	;Thermal
	sta srcPtr
	lda #>_scanStr20
	sta srcPtr+1
	ldx #_boxX+2
	ldy #_boxY+6
	lda #$10
	jsr DrawStringXYWait
	lda #$20
	jsr SimplerWait

	lda #_scanStr20b	;Thermal BLANK
	sta srcPtr
	lda #>_scanStr20b
	sta srcPtr+1
	ldx #_boxX+2
	ldy #_boxY+6
	lda #$08
	jsr DrawStringXYWait
	lda #$10
	jsr SimplerWait
	pla
	dec
	bne :flashenLoop

	inc GDemoState
	jmp DemoMain

DrawBoxAnim
_boxX	equ #13
_boxY	equ #14
	stz _gapCounter	;gap counter
:boxExpandLoop	
	lda #_boxStrTop
	sta srcPtr
	lda #>_boxStrTop
	sta srcPtr+1

	ldy #_boxY	;y position
	ldx #_boxX	;x position
	jsr DrawStringXY ; 0zp, x, y
	
	lda _gapCounter
	beq :noLines
	lda #_boxStrMid
	sta srcPtr
	lda #>_boxStrMid
	sta srcPtr+1
	lda _gapCounter
:middleLoop	iny
	jsr DrawStringXY
	dec
	bne :middleLoop
:noLines	iny 
	lda #_boxStrBot
	sta srcPtr
	lda #>_boxStrBot
	sta srcPtr+1
	jsr DrawStringXY

	inc _gapCounter
	lda _gapCounter
	cmp #$08
	beq :doneBox
	lda #$10
	jsr SimplerWait
	lda _boxNote	; sound code
	ldx #10
	jsr SENoteAX
	lda #$10
	sec
	sbc _boxNote
	sta _boxNote
	jsr VBlank
	bra :boxExpandLoop
:doneBox	stz _boxNote	;reset
	rts
_boxNote	db 0
_gapCounter	db 0


HandleShortWait
	lda #$30
	tax
	tay
	jsr SimpleWait	
	inc GDemoState
	jmp DemoMain

HandleMedWait
	lda #$50
	tax
	tay
	jsr SimpleWait	
	inc GDemoState
	jmp DemoMain



** Dropper routine - not specific to ProDrop per se
** - uses DSEG0
HandleProdrop
	lda _prodropState
	beq :prodropScan
	jmp :prodropUpdate

:prodropScan
	jsr soundDownReset
	lda #0	; start scan at line 0 every time
	sta _prodropScanLine

	lda #DSEG0	; initialize destination pointer
	sta dstPtr
	lda #>DSEG0
	sta dstPtr+1

:scanLineLoop
	lda _prodropScanLine
	rol	; (line * 2) for table index
	tax
	lda LoLineTable,x
	sta srcPtr
	lda LoLineTable+1,x
	sta srcPtr+1

	ldy #0
:scanCharLoop
	lda (srcPtr),y
	cmp #" "	; SPACE
]dropCharCompare equ *-1	; we change this for GRaphics mode
	beq :nextChar

	phy	; +1
	phy	; +2
	ldy #0
	sta (dstPtr),y
	
	iny
	pla	; +1
	sta (dstPtr),y
	lda _prodropScanLine
	ora #%10000000	; set high bit to indicate non-animated state
	
	iny
	sta (dstPtr),y

	iny
	jsr GetRand
	and #%01111111	; decided to limit it to make the values a little
	; closer together.  #$00 - #$7f
	; this particularly helps with gaps when nothing is
	; falling because you started with few characters
	sta (dstPtr),y
	ply	; 0


	lda dstPtr	; now add 4
	clc
	adc #$04
	sta dstPtr
	bcc :nextChar
	inc dstPtr+1	; increment page (high byte)
:nextChar
	iny
	cpy #40
	bne :scanCharLoop

	inc _prodropScanLine
	lda _prodropScanLine
	cmp #24
	bne :scanLineLoop

	; we're done scanning
	lda #$FF
	sta (dstPtr)	; set terminator byte

	inc _prodropState

	jmp DemoMain
	rts	;; @todo: add rts support to jmp table stuff?

]prodropAnimDone	;; might rework this.. is there any point in returning to 
		;; the main demo loop between scans and such?  
	lda #0
	sta _prodropState	; uhg..reset state before exit so can be called again
	inc GDemoState	; ugh again... not great..  using globals
	jmp DemoMain


* This is the animation loop
:prodropUpdate
	lda #0	; finished = false
	stz _prodropAnimDone

:prodropUpdateLoop
	lda _prodropSound
	bne :noSound
	lda #4
	jsr soundDown ;SErandBlip
:noSound	stz _prodropSound ; if this flag gets set we call our sound routine
	lda #16
	tax
	tay
	jsr SimpleWait
	jsr VBlank


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

	ldy #2
	lda (srcPtr),y ; now it's in our y value 
	cmp #$80	; check for high bit
	blt ]dropIt	; not set? then we're animating it
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
]dropCharWrite equ *-1	; we set this differently for GRaphics mode
	sta (dstPtr),y
* breath... holy crap all that just to draw a space at X,Y
* now we draw the character one line down
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
	lda #1		; SET SOUND ANY TIME NEW CHAR DROPS
	sta _prodropSound
	lda #0
	sta (srcPtr)
	bra ]nextAnimChar

]setCharAnim
	ldy #2
	lda (srcPtr),y
	and #%01111111	; clear high bit
	sta (srcPtr),y



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
_prodropSound db 0	; determine if we should call sound engine

soundDownReset stz _sndFreq
	stz _sndFreq+1
	rts

_sndFreq	dw 0
_sndInt	db 0
soundDown	pha	;interval
	ldy _sndFreq
	ldx _sndFreq+1
	lda #$8
	jsr tone
	pla
	cmp _sndInt
	beq :intMatch
	inc _sndInt
	rts
:intMatch	stz _sndInt
	inc _sndFreq
	bcs :inc
	rts
:inc	inc _sndFreq+1
	rts



HandleSwipeWrite
]initSwipe
	ldx #23	; reset line indexes
:clearLoop	stz _swipeLinesX,x	;
	dex
	bne :clearLoop
	stz _swipeActive	; reset overall index

* @todo make parameters ******************************
	lda #FireTextHeight
	sta _swipeMaxHeight	; set max height
	lda #FireTextWidth
	sta _swipeMaxWidth	; set max width
	lda #0
	sta _swipeXOffset	; set x position
	lda #2
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



HandleAppleDraw
:mainLoop
	lda #1
	sta _appleDone

	lda #AppleLogo
	sta srcPtr
	lda #>AppleLogo
	sta srcPtr+1

	
	lda #_appleOffsetY
	sta _currentY

:lineStart	asl 
	tax
	lda LoLineTable,x
	sta dstPtr
	lda LoLineTable+1,x
	sta dstPtr+1

	lda dstPtr	; add offset
	clc
	adc #_appleOffsetX
	sta dstPtr
	bcc :noCarry2
	inc dstPtr+1
:noCarry2
		; src = buffer
		; dst = screen


	ldy #$00
:drawLoop	lda (srcPtr),y
*	bra :noMagic
	cmp (dstPtr),y
	beq :nextChar
	stz _appleDone
	jsr GetRand
	cmp #$10	; occasional black pixel @todo
	bge :noBlack
	lda #0
	bra :noMagic
:noBlack	cmp #$FA	; had to add in a way to keep it from
	blt :noMagic	; getting stuck so now it occasionally puts
	lda (srcPtr),y	; the right pixel even when it wasn't 
:noMagic	sta (dstPtr),y              ; found by getrand.  (because it's only pseudorandom)

:nextChar	iny
	cpy #AppleLogoWidth
	bne :drawLoop
:nextLine     lda srcPtr
	clc
	adc #AppleLogoWidth
	sta srcPtr
	bcc :noCarry1
	inc srcPtr+1
:noCarry1	
	inc _currentY	; handle screen offset
	lda _currentY
	cmp #AppleLogoHeight+_appleOffsetY
	bne :lineStart
		
:donePass	lda #$5
	jsr SimplerWait
	ldy #$4
	jsr SErandStaticBit
	jsr VBlank
	lda _appleDone
	beq :mainLoop
	lda #$33
	jsr SimplerWait
	inc GDemoState
	jmp DemoMain
	
_currentY	db #$00
_appleDone	db #$00
_appleOffsetX equ 10
_appleOffsetY equ 8


	


* done = true
* while !done
* for x = 0 to width
*  for y = 0 to height
*   load buffer x,y
*   cmp screen x,y (plus offsets)
*   if (screen = buffer) continue
*   done = false
*   get rand
*   store at screen x,y
* waitvbl

















*********************
* DEMO ONLY!
* x=10,y=10
*********************
_fbufOffsetX	equ #11
_fbufOffsetY	equ #6

**************
* pass sprite ptr  - fixed width,height,x,y
* 18x16
**************

_spriteWidth equ #18
_spriteHeight equ #16
_spriteDrawRow db 0
DrawSpriteMask 
	stx srcPtr
	sty srcPtr+1	; points to first char of sprite
	lda #_spriteHeight
	sta _spriteDrawRow
	lda #FBufWidth*_fbufOffsetY+FBUF+_fbufOffsetX
	sta dstPtr
	lda #>FBufWidth*_fbufOffsetY+FBUF+_fbufOffsetX
	sta dstPtr+1	; points to first char of buffer with offsets

:lineLoop	ldy #0
:loop1	lda (srcPtr),y
	beq :skip1
	cmp #1
	bne :notRand1
	jsr GetRandLow
:notRand1	sta (dstPtr),y
:skip1	iny
	cpy #_spriteWidth
	bne :loop1
	lda srcPtr
	clc 
	adc #_spriteWidth
	sta srcPtr
	bcc :noFlip1
	inc srcPtr+1
:noFlip1	lda dstPtr
	clc
	adc #FBufWidth
	sta dstPtr
	bcc :noFlip2
	inc dstPtr+1
:noFlip2
	dec _spriteDrawRow
	lda _spriteDrawRow
	bne :lineLoop
	rts	

_spriteBigWidth equ #39
_spriteBigHeight equ #12
_spriteBigDrawRow db 0
DrawSpriteMaskBig
	stx srcPtr
	sty srcPtr+1	; points to first char of sprite
	lda #_spriteBigHeight
	sta _spriteBigDrawRow
	lda #FBufWidth*_fbufOffsetY+FBUF+1
	sta dstPtr
	lda #>FBufWidth*_fbufOffsetY+FBUF+1
	sta dstPtr+1	; points to first char of buffer with offsets

:lineLoop	ldy #0
:loop1	lda (srcPtr),y
	beq :skip1
	cmp #1
	bne :notRand1
	jsr GetRandLow
:notRand1	sta (dstPtr),y
:skip1	iny
	cpy #_spriteBigWidth
	bne :loop1
	lda srcPtr
	clc 
	adc #_spriteBigWidth
	sta srcPtr
	bcc :noFlip1
	inc srcPtr+1
:noFlip1	lda dstPtr
	clc
	adc #FBufWidth
	sta dstPtr
	bcc :noFlip2
	inc dstPtr+1
:noFlip2
	dec _spriteBigDrawRow
	lda _spriteBigDrawRow
	bne :lineLoop
	rts	


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
	cmp #"Q"
	beq P8Quit
	cmp #K_ESC
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
	cmp GFireRatio	; FIRE RATIO
	bcs :hot
:not	lda #$0f
	rts
:hot	lda #$00
	rts
_rndHot	db 0
GFireRatio	db #$90

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

ClearText     ldx #40
	lda #" "
:loop	dex
	sta Lo01,x
	sta Lo02,x
	sta Lo03,x
	sta Lo04,x
	sta Lo05,x
	sta Lo06,x
	sta Lo07,x
	sta Lo08,x
	sta Lo09,x
	sta Lo10,x
	sta Lo11,x
	sta Lo12,x
	sta Lo13,x
	sta Lo14,x
	sta Lo15,x
	sta Lo16,x
	sta Lo17,x
	sta Lo18,x
	sta Lo19,x
	sta Lo20,x
	sta Lo21,x
	sta Lo22,x
	sta Lo23,x
	sta Lo24,x
	bne :loop
	rts 

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
* Draw entire buffer to lores screen using color map
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
* Draws a line of earth sprite on the right
**************************************************
_drawEarthLineXOffset equ #39
DrawEarthLine tax
	lda EarthTextWidth*0+EarthText,x
	sta Lo07+_drawEarthLineXOffset
	lda EarthTextWidth*1+EarthText,x
	sta Lo08+_drawEarthLineXOffset
	lda EarthTextWidth*2+EarthText,x
	sta Lo09+_drawEarthLineXOffset
	lda EarthTextWidth*3+EarthText,x
	sta Lo10+_drawEarthLineXOffset
	lda EarthTextWidth*4+EarthText,x
	sta Lo11+_drawEarthLineXOffset
	lda EarthTextWidth*5+EarthText,x
	sta Lo12+_drawEarthLineXOffset
	lda EarthTextWidth*6+EarthText,x
	sta Lo13+_drawEarthLineXOffset
	lda EarthTextWidth*7+EarthText,x
	sta Lo14+_drawEarthLineXOffset
	lda EarthTextWidth*8+EarthText,x
	sta Lo15+_drawEarthLineXOffset
	lda EarthTextWidth*9+EarthText,x
	sta Lo16+_drawEarthLineXOffset
	rts

**************************************************
* Plot single dot on right column of screen
**************************************************
_maxStarHeight	equ #24	
GenStarRight
:loop	jsr GetRand
	cmp #_maxStarHeight
	bge :loop
	tay
	ldx #39	; right column
	lda #"."
	jsr PlotXY
	rts
:gotMinSize	rol	; *2 for table lookup
	tax
	lda LoLineTable,x
	sta dstPtr
	lda LoLineTable+1,x
	sta dstPtr+1
	ldy #39
	lda #"."
	sta (dstPtr),y
	rts

**************************************************
* Plot single char (in accumulator) to X,Y
**************************************************
PlotXY	pha
	phx
	tya
	asl
	tax
	lda LoLineTable,x
	sta dstPtr
	lda LoLineTable+1,x
	sta dstPtr+1
	ply
	pla
	sta (dstPtr),y
	rts

ScrollLeft	
	pha
	phx
	ldx #0
:loop	lda Lo01+1,x
	sta Lo01,x
	lda Lo02+1,x
	sta Lo02,x
	lda Lo03+1,x
	sta Lo03,x
	lda Lo04+1,x
	sta Lo04,x
	lda Lo05+1,x
	sta Lo05,x
	lda Lo06+1,x
	sta Lo06,x
	lda Lo07+1,x
	sta Lo07,x
	lda Lo08+1,x
	sta Lo08,x
	lda Lo09+1,x
	sta Lo09,x
	lda Lo10+1,x
	sta Lo10,x
	lda Lo11+1,x
	sta Lo11,x
	lda Lo12+1,x
	sta Lo12,x
	lda Lo13+1,x
	sta Lo13,x
	lda Lo14+1,x
	sta Lo14,x
	lda Lo15+1,x
	sta Lo15,x
	lda Lo16+1,x
	sta Lo16,x
	lda Lo17+1,x
	sta Lo17,x
	lda Lo18+1,x
	sta Lo18,x
	lda Lo19+1,x
	sta Lo19,x
	lda Lo20+1,x
	sta Lo20,x
	lda Lo21+1,x
	sta Lo21,x
	lda Lo22+1,x
	sta Lo22,x
	lda Lo23+1,x
	sta Lo23,x
	lda Lo24+1,x
	sta Lo24,x
	inx 
	cpx #39 
	beq :done
	
	jmp :loop
:done	lda #" "
	sta Lo01,x
	sta Lo02,x
	sta Lo03,x
	sta Lo04,x
	sta Lo05,x
	sta Lo06,x
	sta Lo07,x
	sta Lo08,x
	sta Lo09,x
	sta Lo10,x
	sta Lo11,x
	sta Lo12,x
	sta Lo13,x
	sta Lo14,x
	sta Lo15,x
	sta Lo16,x
	sta Lo17,x
	sta Lo18,x
	sta Lo19,x
	sta Lo20,x
	sta Lo21,x
	sta Lo22,x
	sta Lo23,x
	sta Lo24,x
	plx
	pla
	rts
	

ScrollRightUp 
	ldx #22	, x start

:loop	lda Lo02,x
	sta Lo01,x
	lda Lo03,x
	sta Lo02,x
	lda Lo04,x
	sta Lo03,x
	lda Lo05,x
	sta Lo04,x
	lda Lo06,x
	sta Lo05,x
	lda Lo07,x
	sta Lo06,x
	lda Lo08,x
	sta Lo07,x
	lda Lo09,x
	sta Lo08,x
	lda Lo10,x
	sta Lo09,x
	lda Lo11,x
	sta Lo10,x
	lda Lo12,x
	sta Lo11,x
	lda Lo13,x
	sta Lo12,x
	lda Lo14,x
	sta Lo13,x
	lda Lo15,x
	sta Lo14,x
	lda Lo16,x
	sta Lo15,x
	lda Lo17,x
	sta Lo16,x
	lda Lo18,x
	sta Lo17,x
	lda Lo19,x
	sta Lo18,x
	lda Lo20,x
	sta Lo19,x
	lda Lo21,x
	sta Lo20,x
	lda Lo22,x
	sta Lo21,x
	lda Lo23,x
	sta Lo22,x
	lda Lo24,x
	sta Lo23,x
	lda #" "
	sta Lo24,x
	inx 
	cpx #40
	beq :done
	jmp :loop
:done	rts

**************************************************
* SafeWait
* -silly triple loop, preserves AXY
**************************************************
SimplerWait	phx
	phy
	tax
	tay
	jsr SimpleWait
	ply
	plx
	rts

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
* SetProdropGr / SetProdropText
* - set the operating mode of the prodrop effect
**************************************************
SetProdropGr
	lda #$00
	bra SetProdropChar
SetProdropText
	lda #" "
SetProdropChar	
	sta ]dropCharCompare
	sta ]dropCharWrite
	inc GDemoState
	jmp DemoMain


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
* See if we're running on a IIgs
* From Apple II Technote: 
*   Miscellaneous #7
*   Apple II Family Identification
**************************************************
DetectIIgs	
	sec        ;Set carry bit (flag)
	jsr $FE1F    ;Call to the monitor
	bcs :oldmachine    ;If carry is still set, then old machine
*	bcc :newmachine    ;If carry is clear, then new machine
:newmachine   lda #1
	sta GMachineIIgs
	rts
:oldmachine	stz GMachineIIgs
	rts

InitState
	lda GMachineIIgs
	beq :IIe
	rts
:IIe	rts	

VBlank	lda _vblType
	bne :IIc
	jsr VBlankNormal
	rts
:IIc	rts

_vblType	db 0	; 0 - normal, 1 - IIc

**************************************************
* Wait for vertical blanking interval - IIe/IIgs
**************************************************
VBlankNormal
:loop1	lda RDVBLBAR
	bpl :loop1 ; not VBL
:loop	lda $c019
	bmi :loop ;wait for beginning of VBL interval
	rts




**************************************************
* Keyboard equates
**************************************************
K_CTRL	equ $60
K_ESC	equ $9b
K_DELETE	equ $7f
K_SHIFT	equ $20

K_CTRL_S	equ "s"-K_CTRL
K_CTRL_P	equ "n"-K_CTRL



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
DSEG0	ds 1024	; General 1K Data Store
DSEG1	ds 1024	; Secondary (overflow region?)
DSEG2	ds 4096	; Secondary (overflow region?)

**************************************************
* Global Variables
**************************************************

GKeyEvent	db 0	; keypress - allow subroutines to access
GDemoState	db 0	; current demo state
GMachineIIgs	db 0	; machine identification flag


**************************************************
* State 'Enumerators'
**************************************************
ProdropStateScan	equ #0	; Scans all characters into our data structure
ProdropStateUpdate	equ #1	; Does one round of character updates, buffer&screen
ProdropStateDone	equ #2	; Really just to let the callee(s) know it's all done

	use festrodata	; all the sprites and such
	use soundengine	; bleep, boops and clicks
	use applerom		; softswitch locations, etc
	lst on
	sav /code/festro.sys
