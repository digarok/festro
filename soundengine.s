* COSMOS THEME

Song01	

_SE_tonesBAD	db 255,241,227,214,202,191,180,170,161,152,143,135
	db 128,121,114,108,102,096,091,085,081,076,072,068
	db 064,060,057,054,051,048,045,043,040,038,036,034

_SE_tones	db NoteG0,NoteGsharp0,NoteA0,NoteBflat0,NoteB0
	db NoteC1,NoteCsharp1,NoteD1,NoteDsharp1,NoteE1
	db NoteF1,NoteFsharp1,NoteG1,NoteGsharp1,NoteA1
	db NoteBflat1,NoteB1,NoteC2,NoteCsharp2,NoteD2
	db NoteDsharp2,NoteE2,NoteF2

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




SErandBlip
	jsr GetRand
	sta _SECURRNOTE
	lda #5
	sta _SECURRNOTE+1
	jsr SEplayNote
	rts

* a = freq  ...  x = dur
SENoteAX	
	sta _SECURRNOTE
	stx _SECURRNOTE+1
	jsr SEplayNote
	;lda _SECURRNOTE+1
	;ldy _SECURRNOTE
	;ldx #$0
	;jsr tone
	rts

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


	


