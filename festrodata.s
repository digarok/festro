FinalText
 asc "",00
 asc " KFest is the world's only annual",00
 asc " convention dedicated to the Apple II",00
 asc " and it's the largest gathering of ",00
 asc " active Apple II users you'll ever see!",00
 asc "",00
 asc " The next KFest is July 23-28, 2013.",00
 asc "",00
 asc " You can get more info on their site *",00
 asc "             www.kansasfest.org",00
 asc "",00
 asc "",00
 asc "* I'm not affiliated with KansasFest.",00
 asc "  I was just looking for an excuse to",00
 asc "  write some code.",00
 asc "",00
 asc "",00
 asc "",00
 asc "                The End?",00
 asc "",00
 asc "",00
 asc "              Press A Key",00
 hex ff,ff




FireTextHeight equ #20 ; buffer height
FireTextWidth equ #23 ; buffer width (INCLUDE 00 BYTE!!)
 ds \
FireText 
 asc "             -/+.     ",00
 asc "            /+++      ",00              
 asc "           :+++.      ",00        
 asc "           ++:`       ",00        
 asc "   .=++++/--.:/+++=-  ",00        
 asc "  -++++++++++++++++++`",00        
 asc " .::::::::::::::::::` ",00        
 asc " ::::::::::::::::::`  ",00        
 asc "`/////////////////:   ",00        
 asc "`+++++++++++++++++:   ",00        
 asc "`++++++++++++++++++`  ",00        
 asc " osssssssssssssssss+` ",00        
 asc " -sssssssssssssssssso/",00        
 asc "  ossssssssssssssssss/",00        
 asc "  `ssssssssssssssssso ",00        
 asc "   .ossssssssssssss+` ",00        
 asc "    `+ssss+//+ssss/   ",00       
 asc "      `-.`    `.-`    ",00       
 asc "                      ",00
 asc "    GREETZ & THANKS ->",00


EarthTextWidth equ #19
EarthTextHeight equ #10
EarthText
	asc "        ___        "
	asc "      .' ':'.      "
	asc "    .'':: .: '.    "
	asc "   /   ::::'   \   "
	asc "  ;.   ': `     ;  "
	asc "  |     '..     |  "
	asc "  ; '   ::::.   ;  "
	asc "   \    '::::  /   "
	asc "    '.   ::: .'    "
	asc "      '._'_.'      "
EarthText2
	asc "        _____        "
	asc "      .'.  ':'.      "
	asc "    .''::: .:  '.    "
	asc "   /   :::::'    \   "
	asc "  ;.    ':' `     ;  "
	asc "  |       '..     |  "
	asc "  ; '     ::::.   ;  "
	asc "   \      '::::  /   "
	asc "    '.     ::: .'    "
	asc "      '.___'_.'      "
EarthTextOrig
	asc "         _____         "
	asc "      .-'.  ':'-.      "
	asc "    .''::: .:    '.    "
	asc "   /   :::::'      \   "
	asc "  ;.    ':' `       ;  "
	asc "  |       '..       |  "
	asc "  ; '      ::::.    ;  "
	asc "   \       '::::   /   "
	asc "    '.      :::  .'    "
	asc "      '-.___'_.-'      "



HandleWorldScroll

* mapOffset = WorldMapWidth - 40 
* for (i = mapOffset; i >= 0; i--) {
*  draw map from buffer+i,y to screen
*  delay
* }

WorldMapWidth equ #72
WorldMapHeight equ #24
WorldMap 
 asc " +90N-+-----+-----+-----+-----+----+-----+-----+-----+-----+-----+-----+"
 asc " |          . _..::__:  ,-^-^._       |7       ,     _,.__             |"
 asc " |  _.___ _ _<_>`!(._`.`-.    /        _._     `_ ,_/  '  '-._.---.-.__|"
 asc " |.{     ^ ` `-==,',._\{  \  / {)     / _ ^>_,-' `                mt-2_|"
 asc " + \_.:--.       `._ )`^-. ^'      , [_/(                       __,/-' +"
 asc " |'^'     \         ^    _L       oD_,--'                )     /. (|   |"
 asc " |         |           ,'         _)_.\\._<> 6              _,' /  '   |"
 asc " |         `.         /          [_/_'` `^(                <'}  )      |"
 asc " +30N       \\    .-. )          /   `-'^..' `:._          _)  '       +"
 asc " |   `        \  (  `(          /         `:\  > \  ,-^.  /' '         |"
 asc " |             `._,   ^`        |           \`'   \|   ?_)  {\         |"
 asc " |                `=.---.       `._._       ,'     ^`  |' ,- '.        |"
 asc " +000               |    `-._        |     /          `:`<_|h--._      +"
 asc " |                  (        >       .     | ,          `=.__.`-'\     |"
 asc " |                   `.     /        |     |{|              ,-.,\     .|"
 asc " |                    |   ,'          \   / `'            ,'     \     |"
 asc " +30S                 |  /             |_'                |  __  /     +"
 asc " |                    | |                                 '-'  `-'   \.|"
 asc " |                    |/                                        '    / |"
 asc " |                    \.                                            '  |"
 asc " +60S                                                                  +"
 asc " |                     ,/           ______._.--._ _..---.---------._   |"
 asc " |    ,-----^-..?----_/ )      _,-'^             ^                  (  |"
 asc " +90S-+-----+-----+-----+-----+----+-----+-----+-----+-----+-----+-----+"

_sprWidth_K equ #18
_sprHeight_K equ #16
_sprData_K db $01,$01,$01,$01,$01,$01,$01,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01
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
_sprWidth_F equ #18
_sprHeight_F equ #16
_sprData_F db $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
 db $01,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$01
 db $01,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$01
 db $01,$0F,$0F,$0F,$0F,$01,$01,$01,$01,$01,$01,$01,$01,$01,$0F,$0F,$0F,$01
 db $01,$0F,$0F,$0F,$0F,$01,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01
 db $01,$0F,$0F,$0F,$0F,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 db $01,$0F,$0F,$0F,$0F,$0F,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00
 db $01,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$01,$00,$00
 db $01,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$01,$00,$00
 db $01,$0F,$0F,$0F,$0F,$0F,$0F,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00
 db $01,$0F,$0F,$0F,$0F,$0F,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 db $01,$0F,$0F,$0F,$0F,$0F,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 db $01,$0F,$0F,$0F,$0F,$0F,$0F,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 db $01,$0F,$0F,$0F,$0F,$0F,$0F,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 db $01,$0F,$0F,$0F,$0F,$0F,$0F,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 db $01,$01,$01,$01,$01,$01,$01,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
_sprWidth_E equ #18
_sprHeight_E equ #16
_sprData_E db $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
 db $01,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$01
 db $01,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$01
 db $01,$0F,$0F,$0F,$0F,$01,$01,$01,$01,$01,$01,$01,$01,$01,$0F,$0F,$0F,$01
 db $01,$0F,$0F,$0F,$0F,$01,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01
 db $01,$0F,$0F,$0F,$0F,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 db $01,$0F,$0F,$0F,$0F,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00,$00
 db $01,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$01,$00,$00,$00
 db $01,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$01,$00,$00,$00
 db $01,$0F,$0F,$0F,$0F,$0F,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00,$00
 db $01,$0F,$0F,$0F,$0F,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 db $01,$0F,$0F,$0F,$0F,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 db $01,$0F,$0F,$0F,$0F,$0F,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
 db $01,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$01
 db $01,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$01
 db $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
_sprWidth_S equ #18
_sprHeight_S equ #16
_sprData_S	db	$00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00
	db	$00,$01,$01,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$01
	db	$01,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$01
	db	$01,$0F,$0F,$0F,$0F,$0F,$01,$01,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01
	db	$01,$0F,$0F,$0F,$0F,$0F,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	db	$01,$0F,$0F,$0F,$0F,$0F,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	db	$00,$00,$0F,$0F,$0F,$0F,$01,$01,$01,$01,$00,$00,$00,$00,$00,$00,$00,$00
	db	$00,$00,$01,$01,$0F,$0F,$0F,$0F,$0F,$0F,$01,$01,$01,$01,$00,$00,$00,$00
	db	$00,$00,$00,$00,$01,$01,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$01,$01,$00,$00
	db	$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$0F,$0F,$0F,$0F,$00,$00
	db	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$0F,$0F,$0F,$0F,$0F,$01
	db	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$0F,$0F,$0F,$0F,$0F,$01
	db	$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$0F,$0F,$0F,$0F,$0F,$0F,$01
	db	$01,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$01
	db	$01,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$01,$01,$00
	db	$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00

_sprWidth_T equ #18
_sprHeight_T equ #16
_sprData_T db $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
 db $01,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$01
 db $01,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$01
 db $01,$0F,$0F,$0F,$01,$01,$0F,$0F,$0F,$0F,$0F,$0F,$01,$01,$0F,$0F,$0F,$01
 db $01,$01,$01,$01,$00,$00,$01,$0F,$0F,$0F,$0F,$01,$00,$00,$01,$01,$01,$01
 db $00,$00,$00,$00,$00,$00,$01,$0F,$0F,$0F,$0F,$01,$00,$00,$00,$00,$00,$00
 db $00,$00,$00,$00,$00,$00,$01,$0F,$0F,$0F,$0F,$01,$00,$00,$00,$00,$00,$00
 db $00,$00,$00,$00,$00,$00,$01,$0F,$0F,$0F,$0F,$01,$00,$00,$00,$00,$00,$00
 db $00,$00,$00,$00,$00,$00,$01,$0F,$0F,$0F,$0F,$01,$00,$00,$00,$00,$00,$00
 db $00,$00,$00,$00,$00,$00,$01,$0F,$0F,$0F,$0F,$01,$00,$00,$00,$00,$00,$00
 db $00,$00,$00,$00,$00,$00,$01,$0F,$0F,$0F,$0F,$01,$00,$00,$00,$00,$00,$00
 db $00,$00,$00,$00,$00,$00,$01,$0F,$0F,$0F,$0F,$01,$00,$00,$00,$00,$00,$00
 db $00,$00,$00,$00,$01,$01,$0F,$0F,$0F,$0F,$0F,$0F,$01,$01,$00,$00,$00,$00
 db $00,$00,$00,$00,$01,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$01,$00,$00,$00,$00
 db $00,$00,$00,$00,$01,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$01,$00,$00,$00,$00
 db $00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00,$00,$00

_sprWidth_YEAR	equ #39
_sprHeight_YEAR	equ #12
_sprData_YEAR	db	$00,$00,$01,$01,$01,$01,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$00,$00,$00,$00,$00
	db	$00,$01,$01,$01,$01,$00,$00,$00,$00,$00,$01,$01,$01,$01,$00,$00,$00
	db	$00,$01,$0F,$0F,$0F,$0F,$01,$00,$00,$00,$00,$00,$01,$0F,$0F,$0F,$0F,$01,$00,$00,$00,$00
	db	$01,$0F,$0F,$0F,$0F,$01,$00,$00,$00,$01,$0F,$0F,$0F,$0F,$01,$00,$00
	db	$01,$0F,$0F,$0F,$0F,$0F,$0F,$01,$00,$00,$00,$01,$0F,$0F,$0F,$0F,$0F,$0F,$01,$00,$00,$01
	db	$0F,$0F,$0F,$0F,$0F,$01,$00,$00,$01,$0F,$0F,$0F,$0F,$0F,$0F,$01,$00
	db	$00,$00,$00,$00,$00,$01,$0F,$0F,$01,$00,$00,$01,$0F,$0F,$00,$00,$0F,$0F,$01,$00,$00,$01
	db	$0F,$01,$01,$0F,$0F,$01,$00,$00,$00,$00,$00,$00,$00,$01,$0F,$0F,$01
	db	$00,$00,$00,$00,$00,$01,$0F,$0F,$01,$00,$01,$0F,$0F,$00,$00,$00,$00,$0F,$0F,$01,$00,$00
	db	$00,$00,$01,$0F,$0F,$01,$00,$00,$00,$00,$00,$00,$00,$01,$0F,$0F,$01
	db	$00,$00,$00,$00,$00,$01,$0F,$0F,$01,$00,$01,$0F,$0F,$00,$00,$00,$00,$0F,$0F,$01,$00,$00
	db	$00,$00,$01,$0F,$0F,$01,$00,$00,$00,$00,$00,$01,$01,$0F,$0F,$01,$00
	db	$00,$00,$00,$00,$01,$0F,$0F,$00,$00,$00,$01,$0F,$0F,$00,$00,$00,$00,$0F,$0F,$01,$00,$00
	db	$00,$00,$01,$0F,$0F,$01,$00,$00,$00,$00,$01,$0F,$0F,$0F,$01,$01,$00
	db	$00,$00,$00,$01,$0F,$0F,$00,$00,$00,$00,$01,$0F,$0F,$01,$00,$00,$01,$0F,$0F,$01,$00,$00
	db	$00,$00,$01,$0F,$0F,$01,$00,$00,$00,$00,$00,$00,$01,$0F,$0F,$0F,$01
	db	$00,$00,$01,$0F,$0F,$00,$00,$00,$00,$00,$00,$01,$0F,$0F,$00,$00,$0F,$0F,$01,$00,$00,$00
	db	$00,$00,$01,$0F,$0F,$01,$00,$00,$00,$00,$00,$00,$00,$01,$0F,$0F,$01
	db	$00,$01,$0F,$0F,$01,$01,$01,$01,$00,$00,$00,$01,$0F,$0F,$01,$01,$0F,$0F,$01,$00,$00,$00
	db	$00,$00,$01,$0F,$0F,$01,$00,$00,$00,$01,$01,$01,$01,$01,$0F,$0F,$01
	db	$01,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$01,$00,$00,$00,$01,$0F,$0F,$0F,$0F,$01,$00,$00,$00,$00
	db	$00,$01,$0F,$0F,$0F,$0F,$01,$00,$01,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$01
	db	$01,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$01,$00,$00,$00,$01,$0F,$0F,$01,$00,$00,$00,$00,$00
	db	$01,$0F,$0F,$0F,$0F,$0F,$0F,$01,$00,$01,$0F,$0F,$0F,$0F,$0F,$01,$00

KfestLogoWidth equ #40
KfestLogoHeight equ #24
KfestLogo
	db $5a,$aa,$5a,$aa,$5a,$aa,$5a,$aa,$5a,$aa,$5a,$aa,$5a,$aa,$5a,$aa,$5a,$aa,$5a,$aa
	db $5a,$aa,$5a,$aa,$5a,$aa,$5a,$aa,$5a,$aa,$5a,$aa,$5a,$aa,$5a,$aa,$5a,$aa,$5a,$aa
	db $5a,$55,$5a,$55,$5a,$55,$5a,$55,$5a,$55,$5a,$55,$5a,$55,$5a,$55,$5a,$55,$5a,$55
	db $5a,$55,$5a,$55,$5a,$55,$5a,$55,$5a,$55,$5a,$55,$5a,$55,$5a,$55,$5a,$55,$5a,$55
	db $55,$5a,$55,$5a,$55,$5a,$55,$5a,$55,$5a,$55,$5a,$55,$5a,$55,$5a,$55,$5a,$55,$5a
	db $55,$5a,$55,$5a,$55,$5a,$55,$5a,$55,$5a,$55,$5a,$55,$5a,$55,$5a,$55,$5a,$55,$5a
	db $50,$55,$50,$55,$50,$55,$50,$55,$50,$55,$50,$55,$50,$55,$50,$55,$50,$55,$50,$55
	db $50,$55,$50,$55,$50,$55,$50,$55,$50,$55,$50,$55,$50,$55,$50,$55,$50,$55,$50,$55
	db $05,$50,$05,$50,$05,$50,$05,$50,$05,$50,$05,$50,$05,$50,$05,$50,$05,$50,$05,$50
	db $05,$50,$05,$50,$05,$50,$05,$50,$05,$50,$05,$50,$05,$50,$05,$50,$05,$50,$05,$50
	db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	db $00,$00,$00,$00,$00,$70,$70,$00,$00,$00,$70,$70,$60,$70,$70,$70,$70,$70,$60,$70
	db $70,$70,$70,$70,$60,$60,$70,$70,$70,$70,$60,$70,$70,$70,$70,$70,$70,$60,$e0,$e0
	db $00,$00,$00,$00,$00,$77,$77,$00,$70,$77,$07,$00,$00,$77,$77,$00,$00,$00,$00,$77
	db $77,$00,$00,$00,$00,$77,$77,$00,$00,$07,$00,$00,$00,$77,$77,$00,$00,$00,$00,$00
	db $00,$00,$00,$00,$00,$77,$77,$77,$07,$00,$00,$00,$00,$77,$77,$70,$70,$00,$00,$77
	db $77,$70,$70,$00,$00,$07,$77,$77,$70,$00,$00,$00,$00,$77,$77,$00,$00,$00,$00,$00
	db $00,$00,$00,$00,$00,$77,$77,$77,$70,$00,$00,$00,$00,$77,$77,$07,$07,$00,$00,$77
	db $77,$07,$07,$00,$00,$00,$07,$77,$77,$70,$00,$00,$00,$77,$77,$00,$00,$00,$00,$00
	db $00,$00,$00,$00,$00,$77,$77,$00,$07,$77,$70,$00,$00,$77,$77,$00,$00,$00,$00,$77
	db $77,$00,$00,$00,$00,$70,$00,$00,$77,$77,$00,$00,$00,$77,$77,$00,$00,$00,$00,$00
	db $0e,$0e,$0e,$0e,$06,$07,$07,$00,$00,$00,$07,$07,$02,$07,$07,$00,$00,$00,$00,$07
	db $07,$07,$07,$07,$02,$07,$07,$07,$07,$00,$00,$00,$00,$07,$07,$00,$00,$00,$00,$00
	db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	db $01,$01,$01,$01,$01,$01,$90,$99,$09,$09,$99,$90,$01,$01,$90,$99,$09,$99,$90,$01
	db $01,$01,$90,$99,$99,$00,$01,$00,$09,$09,$09,$99,$99,$09,$00,$01,$01,$01,$01,$01
	db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$99,$99,$00,$00,$99,$99,$00,$99,$99,$00
	db $00,$00,$00,$99,$99,$00,$00,$00,$00,$90,$99,$99,$90,$00,$00,$00,$00,$00,$00,$00
	db $00,$00,$00,$00,$00,$00,$00,$00,$90,$99,$09,$00,$00,$00,$99,$99,$00,$99,$99,$00
	db $00,$00,$00,$99,$99,$00,$00,$00,$00,$00,$00,$00,$99,$99,$00,$00,$00,$00,$00,$00
	db $00,$00,$00,$00,$00,$00,$00,$90,$99,$09,$00,$00,$00,$00,$99,$99,$00,$99,$99,$00
	db $00,$00,$00,$99,$99,$00,$00,$00,$00,$00,$00,$00,$99,$99,$00,$00,$00,$00,$00,$00
	db $00,$00,$00,$00,$00,$00,$99,$99,$99,$90,$90,$90,$00,$00,$09,$99,$90,$99,$09,$00
	db $00,$00,$00,$99,$99,$00,$00,$00,$09,$90,$90,$90,$99,$09,$00,$00,$00,$00,$00,$00
	db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	db $05,$50,$05,$50,$05,$50,$05,$50,$05,$50,$05,$50,$05,$50,$05,$50,$05,$50,$05,$50
	db $05,$50,$05,$50,$05,$50,$05,$50,$05,$50,$05,$50,$05,$50,$05,$50,$05,$50,$05,$50
	db $55,$05,$55,$05,$55,$05,$55,$05,$55,$05,$55,$05,$55,$05,$55,$05,$55,$05,$55,$05
	db $55,$05,$55,$05,$55,$05,$55,$05,$55,$05,$55,$05,$55,$05,$55,$05,$55,$05,$55,$05
	db $a5,$55,$a5,$55,$a5,$55,$a5,$55,$a5,$55,$a5,$55,$a5,$55,$a5,$55,$a5,$55,$a5,$55
	db $a5,$55,$a5,$55,$a5,$55,$a5,$55,$a5,$55,$a5,$55,$a5,$55,$a5,$55,$a5,$55,$a5,$55
	db $55,$a5,$55,$a5,$55,$a5,$55,$a5,$55,$a5,$55,$a5,$55,$a5,$55,$a5,$55,$a5,$55,$a5
	db $55,$a5,$55,$a5,$55,$a5,$55,$a5,$55,$a5,$55,$a5,$55,$a5,$55,$a5,$55,$a5,$55,$a5
	db $aa,$a5,$aa,$a5,$aa,$a5,$aa,$a5,$aa,$a5,$aa,$a5,$aa,$a5,$aa,$a5,$aa,$a5,$aa,$a5
	db $aa,$a5,$aa,$a5,$aa,$a5,$aa,$a5,$aa,$a5,$aa,$a5,$aa,$a5,$aa,$a5,$aa,$a5,$aa,$a5

