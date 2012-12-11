**************************************************
* SNDPLAY
* @author Dagen Brock <dagenbrock@gmail.com>
* @date   2012-12-10
**************************************************

	org $2000
	typ #$ff
	xc off	; 65c02
	xc
	lst off

:noKey	lda KEY
	cmp #$80
	blt :noKey
	bit STROBE
	cmp #K_ESC
	beq P8Quit
	cmp #"+"
	beq :slideUp
	cmp #"-"
	beq :slideDown
	cmp #"_"
	beq :slideDown
	cmp #"]"
	beq :play
	cmp #"/"
	bne :noChangeDur
	lda _noteDur
	clc
	adc #$20
	sta _noteDur
	jsr HexPrint
	bra :noKey
:slideUp	inc _slideNote
	lda _slideNote
	bra :printBeep
:slideDown	dec _slideNote
	lda _slideNote
	bra :printBeep
:play	ldx _noteDur
	lda _slideNote
	bra :printBeep
:noChangeDur
	


	sec 
	sbc #"a"
	tax 
	lda _SE_tones,x
:printBeep	jsr HexPrint
	ldx _noteDur
	jsr SENoteAX
	bra :noKey
_noteDur	db #$20
_slideNote	db 0

P8Quit        jsr $bf00
              dfb $65
              da qparms
              rts
qparms        dfb 4
              dfb 0
              da $0000
              dfb 0
              da $0000

* print accumulator
HexPrint	pha
	pha
	lsr
	lsr
	lsr
	lsr
	tax
	lda _chars,x
	sta $400
	pla
	and #%00001111
	tax
	lda _chars,x
	sta $401
	pla
	rts

_chars	asc "0123456789ABCDEF"


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

K_ESC         equ $9b

	use soundengine	; bleep, boops and clicks
	use applerom
	lst on
	sav /code/sndplay.sys
