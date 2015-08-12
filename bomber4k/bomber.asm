	.inesmir 1
	.inesmap 0      
	.ineschr 0
	.inesprg 1

	.bank 1
	.org $FFFA  ; vector table
	.dw NMI
	.dw Start
	.dw INT

	.bank 0
	.org $0000
tile1:		.db 0
tile2:		.db 0
tile3:		.db 0
tile4:		.db 0
temp1:		.db 0
temp2:		.db 0
offset1:	.db 0
offset2:	.db 0
offset3:	.db 0
offset4:	.db 0
thirtytwo:	.db 0
highbyte:	.db 0
levlow:		.db 0
levhigh:	.db 0
lev:		.db 0
activebomb:	.db 0
start_button:	.db 0
select_button:	.db 0
a_button:	.db 0
four:		.db 0
eight:		.db 0
sixteen:	.db 0
twentyfour:	.db 0
vidlow:		.db 0
vidhigh:	.db 0
nt_inc:		.db 0
gameactive:	.db 0
score1:		.db 0
score2:		.db 0
score3:		.db 0
checkscore:	.db 0

	.org $0200
player1y:	.db 0
player1t:	.db 0
player1a:	.db 0
player1x:	.db 0
player2y:	.db 0
player2t:	.db 0
player2a:	.db 0
player2x:	.db 0
player3y:	.db 0
player3t:	.db 0
player3a:	.db 0
player3x:	.db 0
player4y:	.db 0
player4t:	.db 0
player4a:	.db 0
player4x:	.db 0
bomby:		.db 0
bombt:		.db 0
bomba:		.db 0
bombx:		.db 0
scorenum1y:	.db 0
scorenum1t:	.db 0
scorenum1a:	.db 0
scorenum1x:	.db 0
scorenum2y:	.db 0
scorenum2t:	.db 0
scorenum2a:	.db 0
scorenum2x:	.db 0
scorenum3y:	.db 0
scorenum3t:	.db 0
scorenum3a:	.db 0
scorenum3x:	.db 0

	.org $8000
t_screen: 	.incbin "title.dat"
lev1: 		.incbin "lev1.dat"
lev2: 		.incbin "lev2.dat"
lev3: 		.incbin "lev3.dat"
g_over:	 	.incbin "gameover.dat"
tiledata: 	.incbin "metatiledata.dat"
tilepal: 	.incbin "bomber.pal" ; include the pallete
titleattr:	.incbin "title_attribs.dat"
levelattr:	.incbin "lev_attribs.dat"
finishtext:	.incbin "finishtext.dat"

;***************
; interrupt - just so it points somewhere
;***************
INT:
	rti
;***************

;***************
; non maskable interrupt
;***************
NMI:
	jsr check_keys;get key state
	lda gameactive
	cmp #0
	bne skiptitle
	jsr title_screen
	jmp ret_nmi
skiptitle:
	cmp #1;
	beq do_lev
	cmp #2
	beq do_lev
	cmp #3
	bne game_over
do_lev:
	lda checkscore
	cmp #216
	beq do_nextlev
	jsr playgame
	jmp ret_nmi
do_nextlev:
	inc gameactive
	lda #0
	sta checkscore
	lda #0
	jsr store20001	
	ldy gameactive
	jsr load_level

	jsr setupsprites
	lda gameactive
	cmp #4
	bne skipscoremove
	jsr endscore_pos	
	jsr display_score
skipscoremove:
	jsr turn_screen_on
	jmp ret_nmi
game_over:
	lda gameactive
	cmp #4
	bne ret_nmi
	jsr game__over
ret_nmi:
	lda #2
	sta $4014
	lda #0
	sta $2005
	sta $2005
	lda #$20
	sta $2006
	lda #$00
	sta $2006
	rti
;***********


Start:
	cld
	sei
	lda #0
	tax
	jsr store20001
	sta gameactive
	sta activebomb

	lda #4
	sta four
	lda #8
	sta eight
	lda #16
	sta sixteen
	lda #32
	sta thirtytwo
	sta nt_inc
	lda #24
	sta twentyfour
	
	jsr load_to_ppu;load graphics to ppu mem
	lda #$20  ; set the destination address in PPU memory
  	sta $2006  ; should be $2000
  	stx $2006	
	lda #low(t_screen)	
	sta levlow
	lda #high(t_screen)
	sta levhigh

;**********************

	jsr load_title
	lda #$3F
	sta $2006
	lda #$00    ; point $2006 to the pallete
	sta $2006
	ldx #$00
palload:
	lda tilepal, X     ; use a simple load of 32 bytes.
	inx
	sta $2007
	cpx #28
	bne palload
	jsr blank_sprs	
	jsr turn_screen_on  ; call subroutine to turn on / setup the PPU.

infin:              ; our infinite loop
	jmp infin

;end of game
;***********************************
;***********************************
;***********************************
; sub routines
;***********************************
;***********************************
;***********************************

;********************
;turn screen on
;********************
turn_screen_on:
	; Setup the PPU
	lda #%10000000
	sta $2000
	lda #%00011110
	sta $2001
	rts
;*******************

;*******************
;load sprite/tile data into video at $0000, load 1280 bytes even though their only
;1167 byte of actual data
;*******************
load_to_ppu:
	lda #0  ; set the destination address in PPU memory
  	sta $2006  ; should be $0000
  	sta $2006
  	lda #low(ppumem)   ; put the high and low bytes of the address ppumem
  	sta vidlow        ; into the variables so we can use indirect addressing.
  	lda #high(ppumem)
  	sta vidhigh

	ldx #5  ; number of 256-byte chunks to load
  	ldy #0
ppuloop:
  	lda [vidlow],y
  	sta $2007     ; load 256 bytes
  	iny
  	bne ppuloop
	
  	inc vidhigh  ; increment high byte of address vidhigh to next 256 byte chunk
  	dex        ; one chunk done so X = X - 1.
  	bne ppuloop   ; if X isn't zero, do again
	rts
;********************

;*******************
;load title screen
;*******************
load_title:
	ldy #0
	jsr load_level;		load & expand into nametable
	lda #$23
	sta $2006
	lda #$c0
	sta $2006
	ldy #0
loadtattr:
	lda titleattr,y
	iny
	sta $2007
	cpy #64
	bne loadtattr
	rts
;*******************

;*******************
;load level attributes
;*******************
load_levatribs:
	lda #$23
	sta $2006
	lda #$c0
	sta $2006
	ldy #0
loadlattr:
	lda levelattr,y
	iny
	sta $2007
	cpy #64
	bne loadlattr
	rts
;*******************
;*******************
;load level
;*******************	
load_level:
	lda #low(t_screen)	
	sta levlow
	lda #high(t_screen)
	sta levhigh
	tya;		}
	clc;		}
	adc levhigh;}
	sta levhigh;}-add y to lev high address to get total offset from title screen start address
	ldy #0;start of level data counter
loadlev:
	tya;			}
	and #192;		}
	cmp #0;			}
	bne try21;		}
	lda #$20;		}
	sta highbyte;		}
	jmp finishedhighbyte;	}
try21:;				}
	cmp #64;		}
	bne try22;		}
	lda #$21;		}
	sta highbyte;		}
	jmp finishedhighbyte;	}
try22:;				}
	cmp #128;		}
	bne try23;		}
	lda #$22;		}
	sta highbyte;		}
	jmp finishedhighbyte;	}
try23:;				}
	lda #$23;		}
	sta highbyte;		}-get nametable high byte
finishedhighbyte:
	lda [levlow],y; get current level byte
	tax
	tya;		}
	and #$30;	}
	sta temp2;	}
	asl a;		}
	asl a;		}
	sta temp1;	}-temp1=(y & $30) x 4 - the y nametable offset
	tya;		}
	and #$3f;	}
	sec;		}
	sbc temp2;	}
	asl a;		}
	sta temp2;	}-temp2=((y & $3f) - (y & $30)) x 2 - the x nametable offset
	
	lda temp1;	}
	clc;		}
	adc temp2;	}
	sta offset1;	}
	sta offset2;	}
	inc offset2;	}
	lda offset1;	}
	clc;		}
	adc thirtytwo;	}
	sta offset3;	}
	sta offset4;	}
	inc offset4;	}-calc offsets of nametable low byte
	
	sty temp1;	save y reg
	txa;	get tile data
	asl a;	x 2
	asl a;	x 2
	tay;	store in y (tile data x 4)
	lda tiledata,y;	}
	sta tile1;	}-get 1st tile
	iny;	move to next tile
	lda tiledata,y;	}
	sta tile2;	}-get 2nd tile
	iny;	move to next tile
	lda tiledata,y;	}
	sta tile3;	}-get 3rd tile
	iny;	move to next tile
	lda tiledata,y;	}
	sta tile4;	}-get 4th tile
	ldy temp1;	restore y reg

	lda highbyte;	}
	sta $2006;	}
	lda offset1;	}
	sta $2006;	}
	lda tile1;	}
	sta $2007;	}
	lda highbyte;	}
	sta $2006;	}
	lda offset2;	}
	sta $2006;	}
	lda tile2;	}
	sta $2007;	}
	lda highbyte;	}
	sta $2006;	}
	lda offset3;	}
	sta $2006;	}
	lda tile3;	}
	sta $2007;	}
	lda highbyte;	}
	sta $2006;	}
	lda offset4;	}
	sta $2006;	}
	lda tile4;	}
	sta $2007;	}-write 2x2 tile

	iny
	cpy #0
	beq endlevload
	jmp loadlev
endlevload:
	rts
;********************

;********************
init_player_sprs:
	lda #18
	sta player1t
	lda #0
	sta player1a
	lda #$10
	sta player1y
	lda #$10
	sta player1x

	lda #16
	sta player2t
	lda #0
	sta player2a
	lda #$10
	sta player2y
	lda #8
	sta player2x

	lda #19
	sta player3t
	lda #0
	sta player3a
	lda #$18
	sta player3y
	lda #$10
	sta player3x

	lda #17
	sta player4t
	lda #0
	sta player4a
	lda #$18
	sta player4y
	lda #8
	sta player4x
	rts
;**************************
;********************
;blank sprite area
;********************
blank_sprs:
	ldy #0
spr_loop_blank:
	lda #240
	sta $200,y
	iny
	bne spr_loop_blank
	rts
;********************

;********************
;set up sprites
;********************
setupsprites:
	jsr blank_sprs
	jsr init_player_sprs
	
	lda #255
	sta bomby
	lda #1
	sta bombt
	lda #2
	sta bomba
	lda #128
	sta bombx
	jsr setup_score
	rts

;********************
;setup score
;********************
setup_score:
	lda #197
	sta scorenum1y
	lda #$20
	sta scorenum1t
	lda #1
	sta scorenum1a
	lda #$90
	sta scorenum1x
	lda #197
	sta scorenum2y
	lda #$20
	sta scorenum2t
	lda #1
	sta scorenum2a
	lda #$88
	sta scorenum2x
	lda #197
	sta scorenum3y
	lda #$20
	sta scorenum3t
	lda #1
	sta scorenum3a
	lda #$80
	sta scorenum3x
	rts
;********************

;********************
;end game score position
;********************
endscore_pos:
	lda #149
	sta scorenum1y
	lda #$A0
	sta scorenum1x
	lda #149
	sta scorenum2y
	lda #$98
	sta scorenum2x
	lda #149
	sta scorenum3y
	lda #$90
	sta scorenum3x
;********************

;***********
;title screen
;***********
title_screen:
	lda start_button
	cmp #1
	bne start_notpressed
	lda #1
	sta gameactive
	lda #0
	jsr store20001
	jsr setupsprites
	lda #0
	sta checkscore
	ldy #1
	jsr load_level
	jsr load_levatribs
	jsr blank_score
	jsr turn_screen_on
start_notpressed:
	rts

;***********
;game over
;***********
game__over:
	lda select_button
	cmp #1
	bne select_notpressed
	lda #0
	sta gameactive
	jsr store20001
	jsr load_title
	jsr blank_sprs
	jsr turn_screen_on
select_notpressed:
	rts

;***********
;blank score
;***********
blank_score
	lda #32
	sta score1
	sta score2
	sta score3
	rts
;***********

;***********
;display score
;***********
display_score:
	lda score1
	sta scorenum1t
	lda score2
	sta scorenum2t
	lda score3
	sta scorenum3t
	rts
;***********

;***********
;increase score
;***********
inc_score:
	inc checkscore;check for num of building destroyed
	inc score1
	lda score1
	cmp #42
	bne noscorechange
	lda #32
	sta score1
	inc score2
	lda score2
	cmp #42
	bne noscorechange
	lda #32
	sta score2
	inc score3
	lda score3
	cmp #42
	bne noscorechange
	lda #32
	sta score3
noscorechange:
	rts
;***********

;***********
;play game
;***********
playgame:
	inc player1x
	inc player2x
	inc player3x
	inc player4x
	
	lda activebomb;check that bomb active
	beq bomb_do_nothing
	 
	inc bomby;}
	inc bomby;}-move bomb down 2
	lda bomby
	cmp #152
	bmi bomb_collision
	
	lda #255
	sta bomby
	lda #0
	sta activebomb
	jmp bomb_do_nothing
bomb_collision:
	ldy #0;				}
	jsr check_building_collision;	}-check bomb collisions
bomb_do_nothing:
	lda player3x
	cmp #0
	bne no_lower_plane
	jsr inc_playery
	jsr inc_playery
	jsr inc_playery	
	
no_lower_plane:
	ldy #1;				}
	jsr check_building_collision;	}-check player collisions
	jsr display_score
	
	rts
;*****************

;*****************
;move player sprite down one
;*****************
inc_playery:
	inc player1y
	inc player2y
	inc player3y
	inc player4y
	rts
;*****************

;*****************
;check for key presses
;*****************
check_keys:
	lda #$01    ; |
	sta $4016   ;  \
	lda #$00    ;   - Setup Pad for reading.
	sta $4016   ; _/

	lda $4016  ; read for A key.
	and #1
	sta a_button

	lda $4016  ; read for B key.
	lda $4016  ; read for SELECT
	and #1
	sta select_button
	lda $4016  ; read START status
	and #1
	sta start_button

	lda start_button
	beq do_a
	;lda #9
	;sta bombt
do_a:
	ldx activebomb
	bne skip_abutton
	lda a_button
	beq NothingDown
	lda player3y
	sta bomby
	lda player3x
	and #$f8
	sta bombx

	lda #1
	sta activebomb
skip_abutton:
NothingDown:
	; Nothing was down
	rts
;********************

;***************
;parameter in y - 0=check against bomb, 1=check against player
;***************
check_building_collision:
;***
	cpy #0
	bne do_player_label0
;***
	lda bomby
	clc
	adc eight
;***
do_player_label0:
	cpy #1
	bne skip_label0
	lda player3y
	clc
	adc four
skip_label0:
;***
	and #$f8
	lsr a
	lsr a
	lsr a	
	pha
	and #24
	cmp #16
	bne try8
	ldx #$22
	stx vidhigh
try8:
	cmp #8
	bne try0
	ldx #$21
	stx vidhigh
try0:
	cmp #0
	bne finish_find_ntsection
	ldx #$20
	stx vidhigh
finish_find_ntsection:
	pla
	and #7
	asl a
	asl a
	asl a
	asl a
	asl a
	sta temp1
;***
	cpy #0
	bne do_player_label1
;***
	lda bombx
;***
do_player_label1:
	cpy #1
	bne skip_label1
	lda player3x
skip_label1:
;***
	and #$f8
	lsr a
	lsr a
	lsr a
	clc	
	adc temp1
	sta vidlow
	
	jsr setvidadr
	ldx $2007
	ldx $2007
	cpx #$45
	bne nochange
;***
	cpy #0
	bne do_player_label2
;***
	jsr setvidadr
	lda #$47
	sta $2007
	lda #0
	sta activebomb;deactivate bomb	
	lda #255
	sta bomby;hide bomb sprite
	jsr inc_score
;***
do_player_label2:
	cpy #1
	bne skip_label2
;if players crashes in building
	inc gameactive
	
	jsr do_gamefinish
	lda #$21
	sta $2006
	lda #$ac
	sta $2006
	ldy #0
textloop:
	lda finishtext,y
	sta $2007
	iny
	cpy #9
	bne textloop
	;jsr display_score
	jsr turn_screen_on
skip_label2:
;***
nochange:
	rts

;****************
;set video address from vidlow / vidhigh
;****************
setvidadr:
	lda vidhigh
	sta $2006;set upper vid address
	lda vidlow
	sta $2006;set lower vid address
	rts
;****************

;****************
;game finish
;****************
do_gamefinish:
	lda #0
	jsr store20001
	jsr blank_sprs
	jsr setup_score
	jsr endscore_pos
	ldy #4
	sty gameactive
	jsr load_level
	rts
;****************

;****************
;store $2000/2001
;****************
store20001:
	sta $2000
	sta $2001
	rts
;****************

ppumem:		.incbin "bomber.chr"  ; include the picture data in the background part
