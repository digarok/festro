**************************************************
* play random short note 
**************************************************
SErandBlip
	jsr GetRand
	sta _SECURRNOTE
	lda #5
	sta _SECURRNOTE+1
	jsr SEplayNote
	rts

**************************************************
* wrapper for SEplayNote 
* a = freq  ...  x = dur
**************************************************
SENoteAX
	sta _SECURRNOTE
	stx _SECURRNOTE+1
	jsr SEplayNote
	rts

**************************************************
* 
**************************************************
SEplayNote
:loop	lda SPEAKER
:whyWut	dey
	bne :thar
	dec _SECURRNOTE+1
	beq :doneThat
:thar	dex
	bne :whyWut
	ldx _SECURRNOTE
	jmp :loop
:doneThat	rts


_SECURRNOTE	db 0,0	; current note being played (frequency/duration)


* COSMOS THEME

PlaySong01Note
:start	lda Song01
]song01Ptr	equ *-2
	cmp #$02
	bne :noLoop
	lda #Song01
	sta ]song01Ptr
	lda #>Song01
	sta ]song01Ptr+1
	bra :start
:noLoop
	;ldx #$20	; x passed as wait value.. omg the hacks
	jsr SENoteAX

	lda ]frameCount
	cmp #_frameRepeat
	beq :nextNote
	inc ]frameCount
	rts
:nextNote	stz ]frameCount
	inc ]song01Ptr 
	beq :inc
	rts
:inc	inc ]song01Ptr+1
	rts
]frameCount db 0
_frameRepeat equ 2
	


Song01	hex 66,99,a3,66,99,a3,66,99,66,99,a3,66,99,a3,66,99
	hex 72,99,a3,72,99,a3,72,99,72,99,a3,72,99,a3,72,99
	hex 79,a3,b5,79,a3,b5,79,a3,79,a3,b5,79,a3,b5,79,a3
	hex 6c,99,a3,6c,99,a3,6c,99,6c,99,a3,6c,99,a3,6c,5b
	hex 66,99,a3,66,99,a3,66,99,66,99,a3,66,99,a3,66,99
	hex 02 ;end

_SE_tones	db NoteG0,NoteGsharp0,NoteA0,NoteBflat0,NoteB0
	db NoteC1,NoteCsharp1,NoteD1,NoteDsharp1,NoteE1
	db NoteF1,NoteFsharp1,NoteG1,NoteGsharp1,NoteA1
	db NoteBflat1,NoteB1,NoteC2,NoteCsharp2,NoteD2
	db NoteDsharp2,NoteE2,NoteF2
NoteRest	equ $01	;\_ these are inaudible anyway
NoteEnd	equ $02	;/
NoteG0        equ $00       ; because it loops
NoteGsharp0   equ $f0
NoteA0        equ $e6
NoteBflat0    equ $d5
NoteB0        equ $cb       ; speculating here on up
NoteC1        equ $c0
NoteCsharp1   equ $b5
NoteD1        equ $ac
NoteDsharp1   equ $a3
NoteE1        equ $99
NoteF1        equ $90
NoteFsharp1   equ $89
NoteG1        equ $80
NoteGsharp1   equ $79
NoteA1        equ $72
NoteBflat1    equ $6c
NoteB1        equ $66
NoteC2        equ $60
NoteCsharp2   equ $5b
NoteD2        equ $56
NoteDsharp2   equ $51
NoteE2        equ $4c
NoteF2        equ $48
                            ; starts to suck here anyway


SErandStatic
	ldy #$ff
:loop	lda SPEAKER
	jsr GetRand
*	lsr
	beq :next
:wait	dec
	bne :wait
:next	dey
	bne :loop
	rts

* y = length
SErandStaticBit
:loop	lda SPEAKER
	jsr GetRand
	lsr
	beq :next
:wait	dec
	bne :wait
:next	dey
	bne :loop
	rts




*-------------------------------
*
*  T O N E
*
*  In: y-x = pitch lo-hi
*      a = duration
*
*-------------------------------
tone
 sty :pitch
 stx :pitch+1

:outloop bit SPEAKER

 ldx #0
:midloop ldy #0

:inloop iny
 cpy :pitch
 bcc :inloop

 inx
 cpx :pitch+1
 bcc :midloop

 sec
 sbc #1
 bne :outloop

 rts

:pitch ds 2


	


