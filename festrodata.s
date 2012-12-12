_boxStrTop	asc " _______________________ ",00
_boxStrMid	asc "|                       |",00
_boxStrBot	asc "|_______________________|",00
_scanStr01	asc "STATUS:",00
_scanStr08	asc "SCANNING",00
_scanStr08b	asc "        ",00
_scanStr09	asc "KCMO",00
_scanStr09b	asc "  .-",00	;lol
_scanStr02	asc "LOCATED",00
_scanStr03	asc "Virgo Supergroup,",00
_scanStr04	asc "Local Group,",00
_scanStr05	asc "Milky Way,",00
_scanStr06	asc "Earth",00
_scanStr07	asc "Scan Surface",00
_scanStr19	asc "Apple // Event Located",00
_scanStr20	asc "BEGIN THERMAL SCAN!",00
_scanStr20b	asc "                   ",00
_scanningString	asc "Status: Scanning surface.",00
_digawriteString	asc " an intro by DiGAROK ",00


_cwoz	asc "Woz",00
_c2	asc "Brutal Deluxe",00
_c3	asc "Belgo",00
_c4	asc "BLuRry",00
_c5	asc "krUe",00
_c6	asc "Ninjaforce",00
_c7	asc "FTA",00
_c8	asc "RedHot ;)",00
_c9	asc "ECC",00
_c10	asc "antoine",00
_c11	asc "MJM",00
_c12	asc "Gamebits/JuicedGS",00
_c13	asc "R&D Automation",00
_c14	asc "KFest Organizers",00
_c15	asc "      Presenters",00
_c16	asc "      Attendees",00
_c17	asc "   AND  YOU!",00
_c18	asc "THANKS",00
_c19          asc "      FOR",00
_c20          asc "         WATCHING",00
_cblank	asc "",00

_creditStringsTable
	da _c2,_c3,_c4,_c5,_c6,_c7,_c8,_c9,_c10
	da _c11,_c12,_cblank,_c13,_c14,_c15,_c16,_cblank
	da _c17,_cblank,_cblank,_c18,_c19,_c20
	da _cblank,_cblank,_cblank,_cblank,_cblank
	dw 0000

FinalText	asc "",00
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
	asc " * I'm not affiliated with KansasFest.",00
	asc "   I was just looking for an excuse to",00
	asc "   write and release some code.",00
	asc "",00
	asc " Download the demo source online!",00
	asc " -> github.com/festro",00
	asc "",00
	asc "",00
	asc "                The End?",00
	asc "",00
	asc "",00
	asc "              Press A Key",00
	hex ff,ff


FireTextHeight equ #20 ; buffer height
FireTextWidth equ #23 ; buffer width (INCLUDE 00 BYTE!!)
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


WorldMapWidth	equ #72
WorldMapHeight	equ #24
WorldMap 	asc " +90N-+-----+-----+-----+-----+----+-----+-----+-----+-----+-----+-----+"
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

_sprWidth_K	equ #18
_sprHeight_K	equ #16
_sprData_K	db $01,$01,$01,$01,$01,$01,$01,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01
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

_sprWidth_F	equ #18
_sprHeight_F	equ #16
_sprData_F	db $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
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

_sprWidth_E	equ #18
_sprHeight_E	equ #16
_sprData_E	db $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
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

_sprWidth_S	equ #18
_sprHeight_S	equ #16
_sprData_S	db $00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00
	db $00,$01,$01,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$01
	db $01,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$01
	db $01,$0F,$0F,$0F,$0F,$0F,$01,$01,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01
	db $01,$0F,$0F,$0F,$0F,$0F,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	db $01,$0F,$0F,$0F,$0F,$0F,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	db $00,$00,$0F,$0F,$0F,$0F,$01,$01,$01,$01,$00,$00,$00,$00,$00,$00,$00,$00
	db $00,$00,$01,$01,$0F,$0F,$0F,$0F,$0F,$0F,$01,$01,$01,$01,$00,$00,$00,$00
	db $00,$00,$00,$00,$01,$01,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$01,$01,$00,$00
	db $00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$0F,$0F,$0F,$0F,$00,$00
	db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$0F,$0F,$0F,$0F,$0F,$01
	db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$0F,$0F,$0F,$0F,$0F,$01
	db $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$0F,$0F,$0F,$0F,$0F,$0F,$01
	db $01,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$01
	db $01,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$01,$01,$00
	db $00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00,$00

_sprWidth_T	equ #18
_sprHeight_T	equ #16
_sprData_T	db $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
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
_sprData_YEAR	db $00,$00,$01,$01,$01,$01,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$00
	db $00,$00,$00,$00,$00,$01,$01,$01,$01,$00,$00,$00,$00,$00,$01,$01,$01,$01
	db $00,$00,$00,$00,$01,$0F,$0F,$0F,$0F,$01,$00,$00,$00,$00,$00,$01,$0F,$0F
	db $0F,$0F,$01,$00,$00,$00,$00,$01,$0F,$0F,$0F,$0F,$01,$00,$00,$00,$01,$0F
	db $0F,$0F,$0F,$01,$00,$00,$01,$0F,$0F,$0F,$0F,$0F,$0F,$01,$00,$00,$00,$01
	db $0F,$0F,$0F,$0F,$0F,$0F,$01,$00,$00,$01,$0F,$0F,$0F,$0F,$0F,$01,$00,$00
	db $01,$0F,$0F,$0F,$0F,$0F,$0F,$01,$00,$00,$00,$00,$00,$00,$01,$0F,$0F,$01
	db $00,$00,$01,$0F,$0F,$00,$00,$0F,$0F,$01,$00,$00,$01,$0F,$01,$01,$0F,$0F
	db $01,$00,$00,$00,$00,$00,$00,$00,$01,$0F,$0F,$01,$00,$00,$00,$00,$00,$01
	db $0F,$0F,$01,$00,$01,$0F,$0F,$00,$00,$00,$00,$0F,$0F,$01,$00,$00,$00,$00
	db $01,$0F,$0F,$01,$00,$00,$00,$00,$00,$00,$00,$01,$0F,$0F,$01,$00,$00,$00
	db $00,$00,$01,$0F,$0F,$01,$00,$01,$0F,$0F,$00,$00,$00,$00,$0F,$0F,$01,$00
	db $00,$00,$00,$01,$0F,$0F,$01,$00,$00,$00,$00,$00,$01,$01,$0F,$0F,$01,$00
	db $00,$00,$00,$00,$01,$0F,$0F,$00,$00,$00,$01,$0F,$0F,$00,$00,$00,$00,$0F
	db $0F,$01,$00,$00,$00,$00,$01,$0F,$0F,$01,$00,$00,$00,$00,$01,$0F,$0F,$0F
	db $01,$01,$00,$00,$00,$00,$01,$0F,$0F,$00,$00,$00,$00,$01,$0F,$0F,$01,$00
	db $00,$01,$0F,$0F,$01,$00,$00,$00,$00,$01,$0F,$0F,$01,$00,$00,$00,$00,$00
	db $00,$01,$0F,$0F,$0F,$01,$00,$00,$01,$0F,$0F,$00,$00,$00,$00,$00,$00,$01
	db $0F,$0F,$00,$00,$0F,$0F,$01,$00,$00,$00,$00,$00,$01,$0F,$0F,$01,$00,$00
	db $00,$00,$00,$00,$00,$01,$0F,$0F,$01,$00,$01,$0F,$0F,$01,$01,$01,$01,$00
	db $00,$00,$01,$0F,$0F,$01,$01,$0F,$0F,$01,$00,$00,$00,$00,$00,$01,$0F,$0F
	db $01,$00,$00,$00,$01,$01,$01,$01,$01,$0F,$0F,$01,$01,$0F,$0F,$0F,$0F,$0F
	db $0F,$0F,$01,$00,$00,$00,$01,$0F,$0F,$0F,$0F,$01,$00,$00,$00,$00,$00,$01
	db $0F,$0F,$0F,$0F,$01,$00,$01,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$01,$01,$0F,$0F
	db $0F,$0F,$0F,$0F,$0F,$0F,$01,$00,$00,$00,$01,$0F,$0F,$01,$00,$00,$00,$00
	db $00,$01,$0F,$0F,$0F,$0F,$0F,$0F,$01,$00,$01,$0F,$0F,$0F,$0F,$0F,$01,$00

KfestLogoWidth equ #40
KfestLogoHeight equ #24
KfestLogo	db $5a,$aa,$5a,$aa,$5a,$aa,$5a,$aa,$5a,$aa,$5a,$aa,$5a,$aa,$5a,$aa,$5a,$aa,$5a,$aa
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

AppleLogoWidth equ #19
AppleLogoHeight equ #13
AppleLogo	db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$40,$c0,$00,$00,$00,$00,$00
	db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$c0,$cc,$cc,$cc,$00,$00,$00,$00,$00
	db $00,$00,$00,$00,$00,$00,$00,$00,$00,$c4,$cc,$cc,$0c,$00,$00,$00,$00,$00,$00
	db $00,$00,$00,$c0,$c4,$c4,$c4,$c0,$c0,$0c,$0c,$c0,$c0,$c4,$c4,$c0,$c0,$00,$00
	db $00,$c0,$cc,$cc,$cc,$cc,$cc,$cc,$cc,$cc,$cc,$cc,$cc,$cc,$cc,$cc,$cc,$cc,$40
	db $d0,$dd,$dd,$dd,$dd,$dd,$dd,$dd,$dd,$dd,$dd,$dd,$dd,$dd,$dd,$dd,$0d,$00,$00
	db $9d,$9d,$9d,$9d,$9d,$9d,$9d,$9d,$9d,$9d,$9d,$9d,$9d,$9d,$9d,$9d,$00,$00,$00
	db $99,$99,$99,$99,$99,$99,$99,$99,$99,$99,$99,$99,$99,$99,$99,$99,$00,$00,$00
	db $01,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$10,$00
	db $00,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31,$31
	db $00,$00,$33,$33,$33,$33,$33,$33,$33,$33,$33,$33,$33,$33,$33,$33,$33,$33,$00
	db $00,$00,$00,$07,$77,$77,$77,$77,$77,$77,$77,$77,$77,$77,$77,$77,$07,$00,$00
	db $00,$00,$00,$00,$00,$07,$07,$07,$00,$00,$00,$00,$07,$07,$07,$00,$00,$00,$00


;db #$25, #$16		; 38 x 45 (*2) 
DLogoWidth	equ #$26
DLogo	db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$55,$55,$55,$55,$55,$55,$55,$55
	db $50,$50,$50,$50,$00,$50,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$50,$55,$55,$55,$05,$00,$05,$05
	db $55,$55,$55,$00,$55,$50,$05,$50,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	db $00,$00,$00,$00,$00,$00,$00,$00,$50,$05,$55,$00,$55,$55,$55,$50,$55,$55,$85
	db $50,$55,$55,$55,$55,$00,$55,$00,$50,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	db $00,$00,$00,$00,$00,$00,$00,$55,$55,$55,$55,$85,$a5,$a5,$a8,$ba,$f8,$aa,$ba
	db $fa,$b8,$a5,$85,$55,$55,$55,$55,$50,$50,$00,$00,$00,$00,$00,$00,$00,$00,$00
	db $00,$00,$00,$00,$00,$00,$00,$00,$55,$55,$a5,$aa,$fb,$bf,$ff,$ff,$ff,$ff,$ff
	db $ff,$ff,$ff,$fb,$fa,$85,$55,$50,$00,$05,$50,$00,$00,$00,$00,$00,$00,$00,$00
	db $00,$00,$00,$00,$00,$00,$00,$05,$55,$aa,$aa,$ab,$ff,$af,$ff,$ff,$ff,$ff,$ff
	db $ff,$ff,$fb,$ff,$fb,$fa,$85,$a8,$55,$55,$50,$00,$00,$00,$00,$00,$00,$00,$00
	db $00,$00,$00,$00,$00,$00,$00,$05,$a5,$aa,$ab,$bf,$ff,$bf,$ff,$fb,$ff,$ff,$ff
	db $ff,$ff,$ff,$fa,$bf,$ff,$fa,$ba,$58,$55,$05,$50,$00,$00,$00,$00,$00,$00,$00
	db $00,$00,$00,$00,$00,$00,$00,$05,$55,$58,$ba,$af,$bf,$ff,$af,$ff,$ff,$ff,$ff
	db $ff,$ff,$ff,$ff,$ff,$af,$fb,$aa,$88,$55,$55,$00,$00,$00,$00,$00,$00,$00,$00
	db $00,$00,$00,$00,$00,$00,$00,$05,$55,$58,$ba,$8a,$aa,$f8,$f8,$ab,$fa,$ff,$ff
	db $ff,$ff,$ab,$f8,$aa,$fb,$a8,$ab,$8a,$55,$50,$00,$00,$00,$00,$00,$00,$00,$00
	db $00,$00,$00,$00,$00,$00,$00,$50,$55,$88,$aa,$ba,$af,$ab,$af,$fb,$bf,$ab,$ff
	db $ff,$ab,$ff,$bb,$af,$ab,$af,$bb,$a5,$55,$50,$00,$00,$00,$00,$00,$00,$00,$00
	db $00,$00,$00,$00,$00,$00,$55,$fb,$ba,$aa,$ba,$ff,$ab,$fa,$fb,$ff,$ba,$ab,$bf
	db $fb,$ab,$bf,$fa,$fa,$ab,$bb,$fa,$ba,$a8,$fb,$85,$00,$00,$00,$00,$00,$00,$00
	db $00,$00,$00,$00,$00,$00,$8a,$ff,$aa,$ab,$fa,$bf,$ff,$fb,$ff,$fb,$af,$bb,$ff
	db $fa,$ab,$bf,$ff,$fb,$ff,$bf,$fa,$bb,$aa,$bb,$aa,$00,$00,$00,$00,$00,$00,$00
	db $00,$00,$00,$00,$00,$00,$55,$ff,$aa,$ab,$fa,$bf,$ff,$fb,$ff,$ff,$fb,$ba,$ff
	db $fb,$bb,$ff,$ff,$fb,$bf,$ff,$fa,$ab,$8a,$ff,$58,$00,$00,$00,$00,$00,$00,$00
	db $00,$00,$00,$00,$00,$00,$05,$fb,$aa,$ab,$fa,$bf,$ff,$fb,$ff,$ff,$fb,$af,$ff
	db $bf,$fa,$bf,$ff,$ff,$ff,$ff,$fb,$ba,$ab,$fb,$55,$00,$00,$00,$00,$00,$00,$00
	db $00,$00,$00,$00,$00,$00,$00,$0a,$8f,$ab,$aa,$bf,$ff,$bf,$ff,$ff,$bb,$ab,$ff
	db $fa,$bb,$af,$bf,$ff,$fb,$bf,$fa,$aa,$8b,$5f,$05,$00,$00,$00,$00,$00,$00,$00
	db $00,$00,$00,$00,$00,$00,$00,$00,$00,$85,$ab,$ff,$bf,$ff,$af,$fb,$aa,$ff,$bf
	db $ff,$ba,$ff,$ab,$ff,$fa,$bf,$ff,$a8,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	db $00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$ba,$ff,$bf,$ff,$ff,$ab,$bb,$ab,$bb
	db $ab,$bb,$ba,$bb,$ff,$fb,$ff,$aa,$58,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	db $9d,$9d,$0d,$0d,$9d,$90,$00,$00,$00,$00,$5a,$ba,$af,$bf,$ff,$fb,$ff,$af,$ff
	db $bf,$ff,$fa,$ff,$af,$fb,$bf,$aa,$50,$00,$00,$00,$00,$9d,$9d,$0d,$0d,$9d,$90
	db $dd,$dd,$00,$00,$dd,$dd,$00,$00,$00,$00,$55,$ba,$af,$bf,$af,$fb,$ff,$bf,$ff
	db $af,$ff,$fb,$af,$fb,$af,$ba,$a8,$50,$00,$00,$00,$00,$dd,$dd,$00,$00,$dd,$dd
	db $dd,$dd,$00,$00,$dd,$dd,$00,$00,$00,$00,$55,$aa,$ba,$af,$ff,$ba,$ff,$af,$ff
	db $ff,$ff,$ba,$ff,$ab,$aa,$fb,$aa,$f5,$50,$00,$00,$00,$dd,$dd,$00,$0d,$dd,$d0
	db $dd,$dd,$00,$00,$dd,$dd,$00,$00,$00,$f5,$fa,$ab,$ff,$fa,$ab,$af,$af,$af,$ff
	db $fa,$af,$ff,$aa,$ab,$ff,$fb,$ba,$af,$fa,$50,$00,$00,$dd,$dd,$00,$00,$dd,$dd
	db $0d,$0d,$0d,$0d,$0d,$00,$00,$50,$fa,$ff,$ff,$ba,$ff,$fb,$ff,$fb,$ba,$ff,$ab
	db $fa,$ab,$fa,$fb,$ff,$ff,$fb,$fa,$85,$aa,$fa,$50,$50,$0d,$0d,$0d,$0d,$0d,$00
	db $00,$00,$00,$00,$a0,$f5,$fa,$ff,$ff,$ff,$fa,$ba,$ff,$bf,$ff,$ff,$ff,$ff,$ff
	db $ff,$ff,$ff,$fb,$ff,$ff,$fb,$ba,$aa,$ff,$ff,$ff,$fa,$f5,$a5,$50,$00,$00,$00
