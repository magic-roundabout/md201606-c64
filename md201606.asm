;
; MD201606 :: JSL POOP
;

; Code and wiring by T.M.R/Cosine
; Character set by Sega (from the Master System)
; Original picture "CSDb Shit" by JSL
; Music by aNdy/Cosine


; Select an output filename
		!to "md201606.prg",cbm


; Scroll speed - $01 is every frame, $02 every second and so on
scroll_speed	=$02


; Yank in binary data
		* = $0800
char_data	!binary "data/sms_font.chr"

		* = $0a00
		!binary "data/scroll_sprites.raw"

		* = $1000
music		!binary "data/prawf_1.prg",,2

		* = $4000
bmp_data

; Some padding after the picture so it doesn't wrap immediately
!set repeat_cnt=$000
!do {
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!set repeat_cnt=repeat_cnt+$01
} until repeat_cnt=$140

		!binary "data/jsl_picture.raw"

bmp_data_end


		* = $ca00
		!binary "data/scroll_sprites.raw"


; Constants
rstr1p		= $00
rstr2p		= $2c

; Labels
rn		= $50
sync		= $51

scroll_y	= $52
sprite_y	= $53
shift_y_tmr	= $54

char_cnt	= $57
char_buffer	= $58		; 24 bytes used

buffer_1	= $2000
buffer_2	= $e000


; Entry point at $c000
		* = $c000

; Stop interrupts, disable the ROMS and set up NMI and IRQ interrupt pointers
entry		sei

		lda #$35
		sta $01

		lda #<nmi
		sta $fffa
		lda #>nmi
		sta $fffb

		lda #<int
		sta $fffe
		lda #>int
		sta $ffff

		lda #$7f
		sta $dc0d
		sta $dd0d

		lda $dc0d
		lda $dd0d

		lda #rstr1p
		sta $d012

		lda #$0b
		sta $d011
		lda #$01
		sta $d019
		sta $d01a

; Configure colour RAM for the two bitmaps
		ldx #$00
		lda #$bf
colour_init	sta $0c00,x
		sta $0d00,x
		sta $0e00,x
		sta $0ee8,x

		sta $cc00,x
		sta $cd00,x
		sta $ce00,x
		sta $cee8,x

		inx
		bne colour_init

; Clear the label space
		ldx #$50
		lda #$00
nuke_zp		sta $00,x
		inx
		bne nuke_zp

; Reset the various data readers
		jsr bmp_read_reset
		jsr reset

; Set up some labels
		lda #$01
		sta rn

		lda #$10
		sta sprite_y

; Initialise the music
		lda #$00
		jsr music+$00

		cli


; Main loop
main_loop

; Copy buffer 1 to buffer 2 with offset
		ldx #$00
copy_1

!set block_cnt=$0000
!do {
		lda buffer_1+$0140+block_cnt,x
		sta buffer_2+$0000+block_cnt,x

		!set block_cnt=block_cnt+$100
} until block_cnt=$1e00

		inx
		beq *+$05
		jmp copy_1

; Fetch a line of bitmap data for the bottom of buffer 2
		ldx #$00
fetch_1a	jsr bmp_read
		sta buffer_2+$1e00,x
		inx
		bne fetch_1a

fetch_1b	jsr bmp_read
		sta buffer_2+$1f00,x
		inx
		cpx #$40
		bne fetch_1b

		jsr bmp_end_check


; Wait until the second buffer copy is needed
wait_1		lda scroll_y
		cmp #$08
		bcc wait_1


; Copy buffer 1 to buffer 2 with offset
		ldx #$00
copy_2

!set block_cnt=$0000
!do {
		lda buffer_2+$0140+block_cnt,x
		sta buffer_1+$0000+block_cnt,x

		!set block_cnt=block_cnt+$100
} until block_cnt=$1e00

		inx
		beq *+$05
		jmp copy_2

; Fetch a line of bitmap data for the bottom of buffer 2
		ldx #$00
fetch_2a	jsr bmp_read
		sta buffer_1+$1e00,x
		inx
		bne fetch_2a

fetch_2b	jsr bmp_read
		sta buffer_1+$1f00,x
		inx
		cpx #$40
		bne fetch_2b

		jsr bmp_end_check


; Wait until the first buffer copy is needed
wait_2		lda scroll_y
		cmp #$08
		bcs wait_2

		jmp main_loop


; Subroutine to read bitmap data
bmp_read	lda bmp_data
		inc bmp_read+$01
		bne *+$05
		inc bmp_read+$02
		rts

; Check for the end of data and reset if reached
bmp_end_check	lda bmp_read+$01
		cmp #<bmp_data_end
		bne bec_out

		lda bmp_read+$02
		cmp #>bmp_data_end
		bne bec_out

bmp_read_reset	lda #<bmp_data
		sta bmp_read+$01
		lda #>bmp_data
		sta bmp_read+$02
bec_out		rts


; IRQ interrupt
int		pha
		txa
		pha
		tya
		pha

		lda $d019
		and #$01
		sta $d019
		bne ya
		jmp ea31

ya		lda rn
		cmp #$02
		bne *+$05
		jmp rout2


; Raster split 1
rout1		lda #$02
		sta rn
		lda #rstr2p
		sta $d012

		lda #$0b
		sta $d020
		sta $d021

		lda #$38
		sta $d018

; Update the scroll register and move the sprites
		ldx shift_y_tmr
		inx
		cpx #scroll_speed
		bne syt_xb

		lda scroll_y
		clc
		adc #$01
		and #$0f
		sta scroll_y

		ldx sprite_y
		dex
		cpx #$e1
		bne *+$04
		ldx #$e2
		stx sprite_y

		ldx #$00
syt_xb		stx shift_y_tmr

; Set up video registers for the scrolling
		lda scroll_y
		and #$07
		eor #$37
		sta $d011

		ldy #$c7
		lda scroll_y
		and #$08
		beq *+$04
		ldy #$c4
		sty $dd00

; Set up the hardware sprites
		ldx #$00
		ldy #$00
sprite_pos_set	lda sprite_x,x
		sta $d000,y
		lda sprite_y
		sta $d001,y
		iny
		iny
		inx
		cpx #$08
		bne sprite_pos_set

		ldx #$00
sprite_dp_set	lda sprite_dp,x
		sta $0ff8,x
		sta $cff8,x

		lda sprite_col,x
		sta $d027,x

		inx
		cpx #$08
		bne sprite_dp_set

		lda #$ff
		sta $d015
		lda #$e0
		sta $d01d
		sta $d01b

		jmp ea31


; Raster split 2
rout2		lda #$01
		sta rn
		lda #rstr1p
		sta $d012

; Update the scrolling message in bank 0
		ldx #$00

mover		asl char_buffer,x

		rol $0b07,x
		rol $0b06,x

		rol $0ac8,x
		rol $0ac7,x
		rol $0ac6,x

		rol $0a88,x
		rol $0a87,x
		rol $0a86,x

		rol $0a48,x
		rol $0a47,x
		rol $0a46,x

		rol $0a08,x
		rol $0a07,x

		inx
		inx
		inx
		cpx #$18
		bne mover

		ldx char_cnt
		inx
		cpx #$08
		bne cc_xb

mread		lda scroll_text
		bne okay
		jsr reset
		jmp mread

okay		sta def_copy+$01
		lda #$00

		asl def_copy+$01
		rol
		asl def_copy+$01
		rol
		asl def_copy+$01
		rol

		clc
		adc #>char_data
		sta def_copy+$02

		ldx #$00
		ldy #$00
def_copy	lda char_data+$08,x
		sta char_buffer,y
		iny
		iny
		iny
		inx
		cpx #$08
		bne def_copy

		inc mread+$01
		bne *+$05
		inc mread+$02

		ldx #$00
cc_xb		stx char_cnt

; Clone the scrolling message to sprites in bank 3
		ldx #$00
sprite_clone	lda $0a06,x
		sta $ca06,x
		lda $0a46,x
		sta $ca46,x
		lda $0a86,x
		sta $ca86,x
		lda $0ac6,x
		sta $cac6,x
		lda $0b06,x
		sta $cb06,x
		inx
		cpx #$18
		bne sprite_clone

; Play the music
		jsr music+$03

; Exit the interrupt
ea31		pla
		tay
		pla
		tax
		pla
nmi		rti


; Scroller self mod resets (start and restart points respectively)
reset		lda #<scroll_text
		sta mread+$01
		lda #>scroll_text
		sta mread+$02
		rts


; Sprite positions, data pointers and colours
sprite_x	!byte $28,$40,$58,$70,$88,$28,$58,$88
sprite_dp	!byte $28,$29,$2a,$2b,$2c,$2d,$2e,$2f
sprite_col	!byte $01,$01,$01,$01,$01,$0c,$0c,$0c


		* = $b000

; Scrolling message text
scroll_text	!scr "it's that time again, so here comes..."
		!scr "   "

		!scr "*** md201606 - jsl poop ***"
		!scr "   "

		!scr "...coded and wired with love by t.m.r, "
		!scr "accompanied by andy on the sid, "
		!scr "a character set lifted wholesale from the sega "
		!scr "master system's rom and this two and a bit screen "
		!scr "high bitmap which was originally drawn by jsl and "
		!scr "is titled ",$22,"csdb shit",$22,"..."
		!scr "     "

		!scr "having to amuse myself because euro2016 has screwed "
		!scr "up the telly schedules aside, this [ahem] release "
		!scr "exists mainly because i didn't have anything planned "
		!scr "for the monthly demo and writing one of these bitmap-"
		!scr "based scrollers completely from scratch seemed like "
		!scr "a ",$22,"fun idea",$22," at the time - carrion wired "
		!scr "this picture as well but i'd already churned out my "
		!scr "own version, reworked it to tidy up after the "
		!scr "conversion process a bit and written half of the code "
		!scr "by that point!"
		!scr "   "

		!scr "originally this was scrolling at a speedy one pixel "
		!scr "each frame, but i felt that dropping things back "
		!scr "to every second frame looked better and seemed to "
		!scr "work with andy's music more; the source code is "
		!scr "available via github if anyone wants to change the "
		!scr "constant near the top which governs the speed "
		!scr "before assembling it for their own amusement."
		!scr "   "

		!scr "the question was raised in the csdb comments about "
		!scr "whether the original picture counts as a c64 scene "
		!scr "release; disk covers or paper-based publications "
		!scr "count so, since it references and indeed insults a "
		!scr "couple of sceners directly, i don't see a problem "
		!scr "personally...?"
		!scr "   "

		!scr "but remember kids, if you don't want something "
		!scr "archived online or wired to the c64, it really is "
		!scr "best not to upload it!"
		!scr "     "

		!scr "and with the ",$22,"scene commentary",$22," done "
		!scr "i have nothing to really say and might as well get "
		!scr "on with some poop-laden greetings to...  "

		!scr "abyss connection -*- "
		!scr "arkanix labs -*- "
		!scr "artstate -*- "
		!scr "ate bit -*- "
		!scr "atlantis and f4cg -*- "
		!scr "booze design -*- "
		!scr "camelot -*- "
		!scr "censor design -*- "
		!scr "chorus -*- "
		!scr "chrome -*- "
		!scr "cncd -*- "
		!scr "cpu -*- "
		!scr "crescent -*- "
		!scr "crest -*- "
		!scr "covert bitops -*- "
		!scr "defence force -*- "
		!scr "dekadence -*- "
		!scr "desire -*- "
		!scr "dac -*- "
		!scr "dmagic -*- "
		!scr "dualcrew -*- "
		!scr "exclusive on -*- "
		!scr "fairlight -*- "
		!scr "fire -*- "
		!scr "focus -*- "
		!scr "french touch -*- "
		!scr "funkscientist productions -*- "
		!scr "genesis project -*- "
		!scr "gheymaid inc. -*- "
		!scr "hitmen -*- "
		!scr "hokuto force -*- "
		!scr "level64 -*- "
		!scr "maniacs of noise -*- "
		!scr "mayday -*- "
		!scr "meanteam -*- "
		!scr "metalvotze -*- "
		!scr "noname -*- "
		!scr "nostalgia -*- "
		!scr "nuance -*- "
		!scr "offence -*- "
		!scr "onslaught -*- "
		!scr "orb -*- "
		!scr "oxyron -*- "
		!scr "padua -*- "
		!scr "plush -*- "
		!scr "psytronik -*- "
		!scr "reptilia -*- "
		!scr "resource -*- "
		!scr "rgcd -*- "
		!scr "secure -*- "
		!scr "shape -*- "
		!scr "side b -*- "
		!scr "singular -*- "
		!scr "slash -*- "
		!scr "slipstream -*- "
		!scr "success and trc -*- "
		!scr "style -*- "
		!scr "suicyco industries -*- "
		!scr "taquart -*- "
		!scr "tempest -*- "
		!scr "tek -*- "
		!scr "triad -*- "
		!scr "trsi -*- "
		!scr "viruz -*- "
		!scr "vision -*- "
		!scr "wow -*- "
		!scr "wrath -*- "
		!scr "xenon -*-"
		!scr "all the groups who should've told me that they're "
		!scr "meant to be on the list but haven't!"
		!scr "     "

		!scr "as always, once the greetings are out of the way "
		!scr "there's very little left to be said...  but how about "
		!scr "wandering over to the cosine website at "
		!scr "http://cosine.org.uk/ for more things a bit like this, "
		!scr "or visiting the c64 scene database at http://csdb.dk/ "
		!scr "for your regular dose of scene dramas!"
		!scr "   "

		!scr "released with love on 2016/06/15... .. .  ."
		!scr "     "

		!byte $00
