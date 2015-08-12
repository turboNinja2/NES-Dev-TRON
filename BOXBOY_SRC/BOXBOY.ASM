;Assembled with DASM V2.12 (C)Copyright 1987,1988 Matthew Dillon
;---------------------------

 PROCESSOR   6502
 SEG.U zpage
 ORG $0
;----------------------

PLATFORM1 = 0
PLATFORM2 = 1
PLATFORM3 = 2
PLATFORM4 = 3

PLATFORM1_Y = $1d        ;1=topmost
PLATFORM2_Y = $4d
PLATFORM3_Y = $7d
PLATFORM4_Y = $ad

PLATFORM1_MIN_X = 63
PLATFORM2_MIN_X = 47
PLATFORM3_MIN_X = 63
PLATFORM4_MIN_X = 47
PLATFORM1_MAX_X = 210
PLATFORM2_MAX_X = 178
PLATFORM3_MAX_X = 194
PLATFORM4_MAX_X = 178

dont_use        dc.b
temp0           dc.b
temp1           dc.b
temp2           dc.b
temp3           dc.b
temp4           dc.b
temp5           dc.b
temp6           dc.b
temp7           dc.b

joyRAW          dc.b
joyHIT          dc.b
joyEOR          dc.b
joyRIGHT    = $80
joyLEFT     = $40
joyDOWN     = $20
joyUP       = $10
joySTART    = 8
joySELECT   = 4
joyB        = 2
joyA        = 1

OAMstart        dc.b
OAMptr          dc.b
OAMinc = $28
OAM     = $200
OAMpage = $2

lfsr            ds.b 2
randmax         dc.b   ;# values random() can return

;----

boxX    = $300
boxATTRIB  = $400  ;bits 0,1=platform# 2=falling 3=tossing 6,7=color
boxY    = $500
boxETC  = $600
tempX   = $3FF
tempATTRIB = $4FF
tempY   = $5FF
tempETC = $6FF

boxes           dc.b
_16ticks        dc.b    ;box tick countdown
next_box_time   dc.b    ;countdown
new_box_rate    dc.b    ;in seconds
box_movespeed   dc.b
box_tick        dc.b

;- - - - -player data

player_x        ds.b 4
player_anim     ds.b 4
player_facing   ds.b 4   ;0=right
player_active   dc.b
player_animwait dc.b

boxesdone       dc.b
score           ds.b 6
hiscore         ds.b 6

;- - - - - -color cycling

conveyer_pal dc.b
rollers_pal dc.b

;- - - - - -color set data

;$77 thru $7F in box* areas hold colorset data

COLORSET = $77         ;offset into box*
colorsetblink dc.b      ;blink toggler
colorsetindex ds.b 3    ;next color to fill for each platform
colorcount ds.b 4       ;how many of each color are in colorsets. keep next to colorsneed for easier clearing (see initcolorsets)
colorsneed ds.b 4       ;# of each color already produced (for balancing random generation)

;- - - - - - - - - - -

sfx_time ds.b 8      ;#frames til loading next sound reg data
sfx_ptr  ds.b 8         ;self explanatory

;- - - - - -miscellaneous

frame_done      dc.b ;nonzero when frame processing is finished (reset by NMI)
runmode     dc.b        ;0=normal $80=attract 8=paused
titlescreentimer dc.b
currentlevel    dc.b
BOXES_TIL_NEXT_LEVEL = 10
LEVELS = 6
_15frames dc.b
showwavetimeout dc.b
;----------------------
 SEG rom
 ORG $bff0

        ;iNES header
        dc.b "NES",$1a
        dc.b 1       ;prgsize*16k
        dc.b 0       ;chrsize*8k
        dc.b $20     ;mapper 2

;---------------------------- TILE DEFS -----------------------------------
 ORG $f000
chrdata
        INCBIN tileset.chr
monofont
        INCBIN mono.chr

BOX_TILE = 24
PLAYER_IDLE = 32
PLAYER_RUN1 = 36
PLAYER_RUN2 = 40
PLAYER_LIFT = 44
TINYBOX = 12

_0 = 48
_1 = 49
_2 = 50
_3 = 51
_4 = 52
_A = 58
_C = 59
_D = 60
_E = 61
_G = 62
_H = 63
_I = 64
_K = 65
_L = 66
_M = 67
_N = 68
_O = 69
_P = 70
_R = 71
_S = 72
_T = 73
_U = 74
_V = 75
_W = 76
_X = 77
_Y = 78
_EXP = 79
_RP = 13
_LP = 21
_FACE = 20
;--------------------------------------------------------------------------
reset subroutine
                        ;load CHR        
        ldy #0
        sty $2000
        sty $2001
        jsr vblankwait
        jsr vblankwait

        sty $2006
        sty $2006

        ldx #3                  ;2-bit tileset ($300 bytes)
        sty temp0
        lda #>chrdata
        sta temp1
.0
        lda (temp0),y
        sta $2007
        iny
        bne .0
        inc temp1
        dex
        bne .0
.6                              ;1-bit tileset ($100 bytes)
        sty temp2
        jsr .4
        ldy temp2
        jsr .4
        tya
        bne .6

        lda #$0f
        sta $4015               ;enable sound

        lda #0          ;ZP clear
        tax
.1      sta 0,x
        inx
        bne .1

        lda #$22                ;init RNG
        sta lfsr
        sta lfsr+1
        sta randmax

        lda $2002
        ldx #$a0        ;NMI enable, 8x16 sprites
        stx $2000

      ;--- --- main loop ---- ---
.loop
        jsr do_titlescreen
        jsr restartgame
.2
        jsr do_frame
        lda boxesdone
        bmi .loop                    ;game over? (out of boxes)

        cmp #BOXES_TIL_NEXT_LEVEL    ;new level every ? colorset completions
        bcc .2

        ldy #NEXTLEVEL_SFX
        jsr start_sfx

        ldy #p3500
        jsr addpoints

        ldx currentlevel
        cpx #LEVELS
        beq .3
        inx
        jsr new_level
        jmp .2
.3                              ;win game
        jsr cls
        lda #<wingame
        ldx #>wingame
        jsr vramcopy
        jsr win2
        jmp .loop
     ;--- --- --- --- ---

;---          ;(1bit CHR transfer)
.4
        ldx #8
.5      lda (temp0),y
        sta $2007
        iny
        dex     ;0-7
        bne .5
        rts
;--------------------------
do_titlescreen subroutine
        lda #0
        sta frame_done

        jsr cls
        jsr drawtitlescreenboxes
        lda #<titlescreen
        ldx #>titlescreen
        jsr vramcopy

        ldx #1
        jsr showscore
win2
        lda #0          ;reset score
        ldx #5
.2      sta score,x
        dex
        bpl .2

        lda #ALL_PAL
        jsr setpalette

        jsr vblankwait
        lda #%00001010  ;(no sprites)
        jsr screen_on           ;show titlescreen

        ldx #$40
        ldy #2
.0
        lda #joySTART
        bit joyHIT
        bne .9

        jsr vblankwait          ;wait ? seconds for start button
        dex
        bne .0
        dey
        bne .0

        lda #32
        sta titlescreentimer

        lda #$80
        sta runmode
        jsr new_level
.1
        jsr do_frame
        lda joyHIT
        bne do_titlescreen
        lda _16ticks
        bne .1
        dec titlescreentimer
        bne .1
        jmp do_titlescreen
.9
        rts
;--------------------------
restartgame subroutine        

        sta player_active ;=0

        ldx #3
        lda #$80        ;reset X for all men
.2      sta player_x,x
        dex
        bpl .2

        ldx #0
        stx runmode
        inx
;--------------------------
new_level subroutine
        stx currentlevel
        jsr cls
        lda #<playfield
        ldx #>playfield
        jsr vramcopy

        sta boxes       ;reset box count

        lda currentlevel
        tax
        beq .0
        ora #_0
        sta $2007
        lda #120
.0      sta showwavetimeout

        lda leveldefs_startingboxes,x
        sta boxesdone

        lda leveldefs_boxrate,x            ;box production rate
        sta new_box_rate
        sta next_box_time
        
        lda leveldefs_boxspeed,x           ;box move speed
        sta box_movespeed

        lda leveldefs_colors,x             ;# unique colors
        sta randmax

        jsr initcolorsets

        lda #ALL_PAL
        jsr setpalette
restartmusic
        ldy #MUSIC_NOISE_SFX ;game music
        jsr start_sfx
        ldy #MUSIC_TRI_SFX
        jsr start_sfx
        ldy #MUSIC_SQ_SFX
        jmp start_sfx

;rate,speed,#colors
                                ;1  2  3  4  5  6 
leveldefs_boxrate       dc.b  3, 5, 5, 5, 3, 4, 5
leveldefs_boxspeed      dc.b  0, 2, 1, 0, 2, 1, 0
leveldefs_colors        dc.b  4, 3, 3, 2, 4, 4, 3
leveldefs_startingboxes dc.b  2, 5, 4, 3, 3, 3, 3
;--------------------------
do_frame subroutine
        lda frame_done
        bne do_frame            ;wait for nmi to finish before starting new frame

        lda showwavetimeout     ;clear "WAVE ?" display
        bmi .1
        dec showwavetimeout
        bpl .1
        lda #<nowave
        ldx #>nowave
        jsr vramcopy
        jsr clear_scroll
.1      
        jsr clearOAM

        clc                     ;OAM cycling
        lda OAMstart
        adc #OAMinc
        sta OAMstart
        sta OAMptr

        lda runmode
        bmi .attract

        lda #joySTART           ;check start button (pause)
        and joyHIT
        tax
        eor runmode
        sta runmode
        dex
        bmi .0
        ldy #PAUSE_SFX
        jsr start_sfx
.0
        lda runmode
        bne .paused
        
        jsr update_player
        jsr draw_players
.attract
        jsr update_objects
.paused
        jsr drawcolorsets

        inc frame_done

        rts
;---------------------------
update_player subroutine
        ;check button press
        ;----
        lda joyHIT
        and #joyA+joyB
        beq .nohit
                        ;button pushed
        ldx player_active
        lda player_x,x
        sec
        sbc #8
        sta temp0               ;temp0=player_x-8
        adc #15
        sta temp1               ;temp1=x+8

        txa                     ;find nearest box
        jsr locateplatform
        bcc .updown             ;no box.. continue
        sty temp2
.grabbox
        lda boxX,x
        cmp temp0
        bcc .nextbox
        cmp temp1
        bcs .nextbox
                                ;X=box to pick up
        lda joyHIT
        lsr                     ;A: toss box
        bcc .Bbutton                    ;toss box
        ldy player_active
        beq .updown                     ;can't toss from top platform

        lda #4
        sta player_anim,y               ;show 'lift' frame

        lda boxATTRIB,x
        ora #8
        sta boxATTRIB,x
        lda #11                         ;12 frames of 'toss' animation (see toss_y)
        sta boxETC,x
        jsr move_to_end                 ;move to end of box list

        ldy #TOSS_SFX                    ;do 'toss' sound
        jsr start_sfx
        rts
.Bbutton                        ;B: drop box
        jsr drop_box

        ldy #DROP_SFX                    ;do 'drop' sound
        jsr start_sfx
        rts
.nextbox
        inx
        cpx temp2
        bcc .grabbox
        bcs .updown             ;not near any box
.nohit                  ;A/B button not hit.  still being held down?
        lda joyRAW
        and #joyA+joyB
        beq .updown
        rts                     ;yep.
.updown         ;check up/down push
        ldx player_active
        lda #joyDOWN    ;down?
        bit joyHIT
        beq .up_check
        cpx #3
        beq .move
        inx                     ;move to lower platform
.up_check
        lda #joyUP      ;up?
        bit joyHIT
        beq .move
        dex                     ;move to higher platform
        bpl .move
        inx
.move                   ;check left/right push
        stx player_active
        lda player_x,x
        clc
        bit joyRAW
        bmi .moveright
        bvs .moveleft
        bvc .animate
.moveright
        ldy #0
        sty player_facing,x
        adc #2          ;(run speed)
        cmp max_x,x
        bcc .2
        lda max_x,x
.2      
        jmp .animate
.moveleft
        ldy #$40
        sty player_facing,x
        sbc #1          ;(run speed)
        cmp min_x,x
        bcs .3
        lda min_x,x
.3
.animate
        ldx player_active
        cmp player_x,x
        sta player_x,x
        beq .5          ;moved?
        ldy player_anim,x
        lda player_animwait
        eor #$80
        sta player_animwait     ;every other frame
        beq .4
        iny                     ;inc frame#
        cpy #4
        bcc .4
.5      ldy #0                  ;loop every 4th frame
.4      sty player_anim,x
        rts

draw_players subroutine
        ldx #0
.next
                        ;plot sprite
        ldy OAMptr

        lda platform_y,x
        sta OAM,y
        sta OAM+4,y

        lda player_x,x
        sta OAM+3,y
        clc
        adc #8
        sta OAM+7,y

        lda #PLAYER_IDLE
        cpx player_active
        bne .1
        ldy player_anim,x
        lda run_anim,y          ;get tile
.1      ldy player_facing,x
        beq .6
        eor #2                  ;swap tiles if facing left
.6      ldy OAMptr
        sta OAM+1,y
        eor #2
        sta OAM+5,y

        lda player_facing,x     ;H-flip both OBJs if facing left
        cpx player_active
        bne .7
        ora #1                  ;active player is different color
.7      sta OAM+2,y
        sta OAM+6,y

        clc             ;next OAM pos
        tya
        adc #OAMinc
        sta OAMptr
.x
        inx             ;next
        cpx #4
        bcc .next

        rts

platform_y
        dc.b PLATFORM1_Y,PLATFORM2_Y,PLATFORM3_Y,PLATFORM4_Y
min_x
        dc.b PLATFORM1_MIN_X,PLATFORM2_MIN_X,PLATFORM3_MIN_X,PLATFORM4_MIN_X
max_x
        dc.b PLATFORM1_MAX_X-24,PLATFORM2_MAX_X,PLATFORM3_MAX_X,PLATFORM4_MAX_X
run_anim
        dc.b PLAYER_IDLE,PLAYER_RUN1,PLAYER_RUN2,PLAYER_RUN1,PLAYER_LIFT
;---------------------------

update_objects subroutine
        ldy #0
        lda #joySELECT
        bit joyRAW
        bne .1
  
        dec box_tick            ;move all boxes when tick=0
        bpl .0
        ldy box_movespeed
.1      sty box_tick
                                ;music is synced to game speed
        lda runmode
        bne .mus1                       ;play music only when game's active
        ldx #6
.mus0   jsr do_sfx
        bne .mus0
.mus1                           ;do palette cycling
        lda _16ticks
        lsr
        bcc .5

        dec rollers_pal
        bpl .4
        lda #1
        sta rollers_pal
.4      dec conveyer_pal
        bpl .5
        lda #2
        sta conveyer_pal
.5                              ;new box every ? ticks
        dec _16ticks
        bpl .0
        lda #15
        sta _16ticks
        dec next_box_time
        bne .0
        lda new_box_rate
        sta next_box_time

        lda boxes               ;limit # of boxes onscreen
        cmp #18
        bcs .0

        lda #PLATFORM1_MAX_X   ;new box vars
        sta tempX
        lda #PLATFORM1_Y
        sta tempY
        jsr random_check
        ror
        ror
        ror
        and #$c0
        sta tempATTRIB
        jsr .insert
.0
                  ;- - - -now loop thru all boxes
        ldx #0
        lda boxes
        bne .next
        rts
.next
        stx temp4               ;temp4=current box#

        lda box_tick            ;move all boxes every ? frames
        bne .3

        lda boxATTRIB,x
        and #$0f
        cmp #PLATFORM1
        beq .moveleft
        cmp #PLATFORM3
        beq .moveleft
        cmp #PLATFORM2
        beq .moveright
        cmp #PLATFORM4
        beq .moveright
        and #8                          ;rising or falling?
        bne .toss
.falling
        clc
        lda boxY,x
        ldy boxETC,x
        adc falling_y,y
        sta boxY,x
        dec boxETC,x
.3      bpl .plot
        jsr plotbox

        jsr move_to_temp        ;copy to temp & delete
                                ;re-add?
        lda tempATTRIB                  ;(boxATTRIB) C=1 from move_to_temp
        sbc #3                          ;clear bit 2 and increment PF#
        sta tempATTRIB
        and #3                          ;dropped off last platform?
        beq .f0
        jsr .insert                             ;no:
        jmp .next3
.f0                                             ;yes:
        dec boxesdone
        ldy #LOSTBOX_SFX
        jsr start_sfx
        ldx temp4
        jmp .next2
.moveleft
        sec
        lda boxX,x
        sbc #1
        sta boxX,x

        cmp #PLATFORM1_MIN_X-14
        bcc .drop
        jmp .plot
.moveright
        clc
        lda boxX,x
        adc #1
        sta boxX,x

        cmp #PLATFORM2_MAX_X+14
        bcs .drop
.plot
        jsr plotbox
.next3
        ldx temp4
        inx             ;next box
.next2  cpx boxes
        bcs .done
        jmp .next
.done
        rts
.toss
        sec
        lda boxY,x
        ldy boxETC,x
        sbc toss_y,y
        sta boxY,x
        dec boxETC,x
        bpl .plot
        jsr plotbox

        jsr move_to_temp                ;(C=1)
        lda tempATTRIB
        sbc #$09                        ;clear bit 3 and decrement PF#
        sta tempATTRIB
        jsr .insert
        jmp .next3
.drop                   ;box reached end of platform, what to do..?
        lda boxATTRIB,x
        sta temp0
        and #3
        beq .dr3                        ;platform0: drop box (no colorset)
        tay                             ;Y=platform#
        clc
        lda colorsetindex-1,y
        adc #$fd                        ;0-2 becomes fd-ff
        sta temp1
        adc colorsetboxlookup-1,y
        tax                             ;x=index to box we want
        lda temp0
        and #$c0                        ;A=color of this box
        cmp boxATTRIB,x                 ;colors match?
        beq .dr1
        lda #2                          ;colors didn't match
        sta colorsetindex-1,y                   ;reset colorset count
        sty temp2
.dr0    inx
        inc temp1
        beq .dr3
        lda boxATTRIB,x
        and #$c0                                ;clear blinking
        pha
        ora temp2                               ;get platform#
        ora #4                                  ;'falling' attrib
        sta boxATTRIB,x
        jsr copy_to_end
        pla
        sta boxATTRIB,x
        jmp .dr0
.dr3
        ldx temp4
        jsr drop_box
        jmp .next               ;do next box
.dr1                            ;colors match:
        lda colorsetindex-1,y           ;last in colorset?
        beq .dr2                              ;nope.
        inc boxATTRIB,x                            ;make blink
        sbc #1                                     ;(c=1)
        sta colorsetindex-1,y                      ;update fill count
        ldy #BOX_SFX
        jsr start_sfx
.dr9
        ldx temp4
        jsr deletebox                              ;delete box
        jmp .next
.dr2                                          ;yep.
        lda #2
        sta colorsetindex-1,y                      ;reset colorset fill count
        dec boxATTRIB+1,x                          ;reset blink state
        dec boxATTRIB+2,x

        ldy #p325
        jsr addpoints

        ldy #COLORSET_SFX
        jsr start_sfx

        inc boxesdone

        jmp .dr9                                ;delete box
plotbox                ;plot box X
        ldy OAMptr

        lda boxY,x
        sta OAM,y
        sta OAM+4,y

        lda boxX,x
        sta OAM+3,y
        clc
        adc #8
        sta OAM+7,y

        lda #BOX_TILE
        sta OAM+1,y
        sta OAM+5,y

        lda boxATTRIB,x
        rol
        rol
        rol
        and #3
        ora #$20        ;priority (behind)
        sta OAM+2,y
        ora #$C0        ;x,y flip
        sta OAM+6,y

        clc             ;next OAM pos
        tya
        adc #OAMinc
        sta OAMptr
        rts

.insert ;from temp.  call with attrib in A
        jsr locateplatform
        sty temp6               ;x,temp7=platform start  y,temp6=platform end
        bcc .i2                 ;first box on this platform..
.i8                     ;boxes are sorted by X for each platform
        lda tempX               ;a=new x
.i9     cmp boxX,x
        bcc .i2
        inx
        cpx temp6
        bne .i9
.i2                     ;insert new box at X
        stx temp5
        cpx boxes
        beq .i4
        ldy boxes               ;shift up from X to <boxes>
        ldx boxes
.i3     dex
        jsr x_to_y
        dey
        cpy temp5
        bne .i3
.i4                             ;copy to (temp5)
        ldx #$ff
        ldy temp5
        jsr x_to_y
        inc boxes
                                ;fixup X (no overlapping)
        ;temp7=start, temp6=end
        lda tempATTRIB
        lsr
        bcs .shiftleft
.shiftright
        ldx temp7
        bpl .sr1        ;(always +)
.sr2    clc
        lda boxX,x
        adc #16
        cmp boxX+1,x
        bcc .sr0
        beq .sr0
        sta boxX+1,x
.sr0    inx
.sr1    cpx temp6
        bne .sr2
        rts
.shiftleft
        ldx temp6
        bpl .sl1        ;(always +)
.sl2    sec
        lda boxX,x
        sbc #16
        cmp boxX-1,x
        bcs .sl0
        sta boxX-1,x
.sl0    dex
.sl1    cpx temp7
        bne .sl2
        rts

move_to_temp
        ldy #$ff
        jsr x_to_y
        jmp deletebox          ;delete
copy_to_end
        ldy boxes
        inc boxes
x_to_y
        lda boxX,x
        sta boxX,y
        lda boxATTRIB,x
        sta boxATTRIB,y
        lda boxY,x
        sta boxY,y
        lda boxETC,x
        sta boxETC,y
        rts
drop_box
        lda boxATTRIB,x            ;box reached end of platform
        ora #4
        sta boxATTRIB,x
        lda #11                         ;12 frames of 'drop' animation (see falling_y)
        sta boxETC,x
move_to_end             ;X=box to move
        jsr copy_to_end
deletebox subroutine    ;delete box at X. C=1 on exit
        txa
        tay
        inx
.d0        
        jsr x_to_y
        inx
        iny
        cpx boxes
        bcc .d0
        dec boxes
        ldx temp4       ;restore update_objects current box
        rts

locateplatform subroutine       ;A=platform#
        and #$03
        sta temp5
        ldx #0
        beq .l5
.l0
        lda boxATTRIB,x    ;scan for target platform
        and #$0f
        cmp temp5
        beq .l1                 ;found one
        bcc .l2                 ;past it
        inx
.l5     cpx boxes
        bne .l0
        clc
.l2     stx temp7
        ldy temp7
        rts             ;c=0: x,y,temp7=z?end:next platform
.l1                     ;scan for next platform
        stx temp7
        ldy temp7
.l7     lda boxATTRIB,y            ;find end
        and #$0f
        cmp temp5
        bne .l8
        iny
        cpy boxes
        bne .l7
.l8
        sec             ;c=1: x,temp7=platform start,  y=next platform or end
        rts

falling_y dc.b 6,6,5,5,4,4,4,4,4,3,2,1,$ff,$ff,$fe,$fe,$fd,$fc,$fb,0,0,0,0,0
toss_y dc.b $fe,$ff,0,1,1,2,3,4,6,8,10,16 ;used by .toss
colorsetboxlookup dc.b COLORSET+3,COLORSET+6,COLORSET+9
;--------------------------------
initcolorsets subroutine
                        ;reset colorcounts
        lda #0
        ldx #7
.2      sta colorcount,x
        dex
        bpl .2

        lda #2          ;reset fill counts
        sta colorsetindex
        sta colorsetindex+1
        sta colorsetindex+2
                        ;reset colorset data (boxes x,y,etc)
        ldy #8
.1      sty temp0

          lda colorset_y,y              ;Y
          sta boxY+COLORSET,y
          lda colorset_etc,y            ;ETC
          sta boxETC+COLORSET,y
          lda colorset_x,y              ;X
          sta boxX+COLORSET,y
          jsr random      ;(kills all regs)
          tax                             ;x,a=color
          inc colorcount,x
          clc
          ror
          ror
          ror
          ldy temp0
          sta boxATTRIB+COLORSET,y

        dey
        bpl .1

        rts

colorset_etc dc.b 0,23,21,0,23,21,0,23,21
colorset_x dc.b PLATFORM2_MAX_X+46,PLATFORM2_MAX_X+30,PLATFORM2_MAX_X+14,PLATFORM3_MIN_X-47,PLATFORM3_MIN_X-31,PLATFORM3_MIN_X-15,PLATFORM4_MAX_X+46,PLATFORM4_MAX_X+30,PLATFORM4_MAX_X+14
colorset_y dc.b PLATFORM2_Y+18,PLATFORM2_Y+18,PLATFORM2_Y+18,PLATFORM3_Y+18,PLATFORM3_Y+18,PLATFORM3_Y+18,PLATFORM4_Y+18,PLATFORM4_Y+18,PLATFORM4_Y+18
;--------------------------------
drawcolorsets subroutine
        lda _15frames
        bne .2
        inc colorsetblink
.2
        ldx #COLORSET
.0      lda boxATTRIB,x
        and colorsetblink
        lsr
        bcs .1
        jsr plotbox
.1
        inx
        bpl .0
        rts
;-----------------------------------------------
addpoints subroutine ;y=points offset
        ldx #0
.1
        clc
        lda points,y
        bpl .2
        dey
        lda #0
.2
        adc score,x
        cmp #10
        bcc .0
        sbc #10
        inc score+1,x
.0
        sta score,x
        iny
        inx
        cpx #6
        bcc .1

        jsr checkhiscore

        rts
points
p325 = . - points
        dc.b 5,2,3,$ff
p3500 = . - points
        dc.b 0,0,5,3,$ff
;------------------------
showscore subroutine      ;score display: x=0/1 for score/hiscore
        lda scorepos,x
        sta $2006
        lda scorepos+2,x
        sta $2006
        txa
        lsr
        ldy #5
.1      lda score,y
        bcc .2
        lda hiscore,y
.2      ora #_0
        sta $2007
        dey
        bpl .1
        rts

scorepos dc.b $20,$23,$42,$8e
;-------------------------
checkhiscore            ;copy score to hiscore if hiscore<score
        ldy #5
.6
        lda score,y
        cmp hiscore,y
        beq .7
        bcs .8
        rts
.7      dey
        bpl .6

.8      lda score,y
        sta hiscore,y
        dey
        bpl .8
        rts
;--------------------------read joypad state into joyRAW, joyHIT
readjoypad subroutine ;R L D U St Sl B A
        ldy joyRAW
        ldx #7
        stx $4016
        inx
        stx $4016
.0
        lda $4016
        lsr
        ror joyRAW
        dex
        bne .0

        tya
        eor joyRAW
        sta joyEOR
        and joyRAW
        sta joyHIT
        rts
;--------------------------
nmi subroutine
      pha
      txa
      pha
      tya
      pha

      jsr readjoypad

      jsr random

      jsr play_sfx

      dec _15frames
      bpl .4
      lda #14
      sta _15frames
.4
      lda frame_done
      beq .0
                        ;score display
        ldx #0
        jsr showscore
                        ;boxesdone display
        ldx #$20
        stx $2006
        lda #$22
        sta $2006
        ldx boxesdone
        beq .3
        lda #TINYBOX
.2      sta $2007
        dex
        bne .2
.3      stx $2007

        clc             ;do palette cycling
        lda conveyer_pal
        adc #CONVEYER1
        jsr setpalette
        clc
        lda rollers_pal
        adc #ROLLER1
        jsr setpalette

        ldx #2          ;OAM DMA
        stx $4014

        lda #%00011110
        jsr screen_on
        
        stx frame_done  ;show that we're done
.0
      pla
      tay
      pla
      tax
      pla
      rti
screen_on ;------------------
        sta $2001
clear_scroll
        ldx #0          ;no scroll
        stx $2005
        stx $2005       ;(X=0 on exit)
        stx $2006
        stx $2006
        rts
vblankwait ;-----------------
        lda $2002
.vbl    lda $2002
        bpl .vbl
        rts
;----------------------------
clearOAM subroutine
        lda #$f8
        ldx #0
.0      sta OAM,x
        inx
        inx
        inx
        inx
        bne .0
        rts
;----------------------------
random subroutine                ;return random color
        jsr lfsr_clock
        jsr lfsr_clock
        jsr lfsr_clock
        lda lfsr
        and #3
        cmp randmax
        bcs random
        rts
lfsr_clock
        lda lfsr+1
        asl
        eor lfsr+1
        asl
        asl
        rol lfsr
        rol lfsr+1
        rts

random_check            ;get random color, check against colors in colorsets
        jsr random      ;for better balance
        tax
.4      lda colorsneed,x
        bne .2

        lda colorsneed
        ora colorsneed+1
        ora colorsneed+2
        ora colorsneed+3
        bne random_check

        ldy #3
.3      lda colorcount,y
        sta colorsneed,y
        dey
        bpl .3
        bmi .4
.2
        dec colorsneed,x
        txa
        rts
;----------------------------
cls subroutine
        jsr vblankwait

        lda #0
        sta $2001       ;screen off

        lda #$20
        sta $2006
        lda #0
        sta $2006
        tay
        ldx #4
.0      sta $2007
        iny
        bne .0
        dex
        bne .0
        rts
;----------------------------
vramcopy subroutine ;(A=0 on exit)
        ldy #0
        sta temp0
        stx temp1
.top
        lda (temp0),y
        beq fin
        sta $2006
        iny
        lda (temp0),y
        sta $2006
        iny

        ldx #$a0        ;+1 vram increment
        lda (temp0),y
        iny
        lsr
        bcs .prep       ;fill
.nofill
        lsr
        bcc .prep
.vert
        ldx #$a4        ;+32 vram increment
        clc
.prep
        stx $2000
        tax
.copy
        lda (temp0),y
        iny
        sta $2007
        lda (temp0),y
        iny
        sta $2007
        bcc .3
        dey
        dey
.3      dex
        bne .copy
        bcc .top
        iny
        iny
        bne .top
fin
        rts
;-----------------------
drawtitlescreenboxes subroutine
        ldx #$a4        ;+32 vram increment
        stx $2000
        ldx #0
.1      sec
        ldy #BOX_TILE
        lda boxlist,x
        beq fin
        sta $2006
        lda boxlist+1,x
.2      sta $2006
        sty $2007
        iny
        sty $2007
        iny
        bcc .3
        lda boxlist,x
        sta $2006
        lda boxlist+1,x
        adc #0
        bcc .2
.3      inx
        inx
        bcc .1

boxlist dc.b $20,$a3, $20,$a5, $20,$c6, $20,$e3, $21,$23, $21,$63, $21,$05, $21,$26, $21,$66, $21,$65
        dc.b $20,$aa, $20,$ac, $20,$e9, $20,$ed, $21,$29, $21,$2d, $21,$6a, $21,$6c
        dc.b $20,$b0, $20,$d1, $20,$f2, $21,$76, $21,$55, $21,$34, $21,$13, $21,$8f, $21,$70, $21,$51, $20,$97, $20,$b6, $20,$d5
        dc.b $22,$09, $22,$0b, $22,$2c, $22,$49, $22,$89, $22,$c9, $22,$6b, $22,$8c, $22,$cc, $22,$cb
        dc.b $22,$10, $22,$12, $22,$4f, $22,$8f, $22,$53, $22,$93, $22,$d0, $22,$d2
        dc.b $22,$16, $22,$37, $21,$dc, $22,$1b, $22,$5a, $22,$99, $22,$d8, $22,$58
        dc.b 0

titlescreen
        ;addr_hi, addr_lo,
        ;(fill) count<<1+1
        ;(copy) count<<2+(2 for vert. copy)
        ;data
        dc.b $23,$c8, 23
        dc.b $ff,$ff
        dc.b $23,$de, 19
        dc.b $ff,$ff
        dc.b $23,$2b, 20
        dc.b _P,_U,_S,_H,0,_S,_T,_A,_R,_T
        dc.b $20,$47, 40
        dc.b _2,_0,_0,_3,0,_M,_I,_N,_I,_G,_A,_M,_E,0,_C,_O,_M,_P,_O,0
        dc.b $23,$8b, 4
        dc.b _H,_I
        dc.b 0

playfield
        ;addr_hi, addr_lo,
        ;(fill) count<<1+1
        ;(copy) count<<2+(2 for vert. copy)
        ;data

        dc.b $20,$a8, 19        ;top belt
        dc.b 2,2
        dc.b $20,$5a, 10        ;pipe
        dc.b 0,14,22,30
        dc.b $20,$5b, 10
        dc.b 0,15,23,31
        dc.b $20,$5c, 5         ;boxes around pipe
        dc.b 24,26
        dc.b $20,$7c, 5
        dc.b 25,27
        dc.b $20,$9c, 5
        dc.b 24,26
        dc.b $20,$bc, 5
        dc.b 25,27
        dc.b $20,$da, 7
        dc.b 24,26
        dc.b $20,$fa, 7
        dc.b 25,27
        dc.b $21,$08, 19        ;bottom belt
        dc.b 4,4
        dc.b $20,$c7, 6         ;left belt
        dc.b 7,7
        dc.b $20,$c8, 19        ;middle1
        dc.b 10,11 
        dc.b $20,$e8, 19        ;middle2
        dc.b 18,19

        dc.b $21,$66, 19        ;top belt
        dc.b 1,1
        dc.b $21,$c6, 19        ;bottom belt
        dc.b 3,3
        dc.b $21,$85, 6         ;left belt
        dc.b 6,6
        dc.b $21,$86, 19        ;middle1
        dc.b 10,11 
        dc.b $21,$a6, 19        ;middle2
        dc.b 18,19
        dc.b $21,$98, 7         ;boxes(top)
        dc.b 8,9
        dc.b $21,$b8, 7         ;boxes(bottom)
        dc.b 16,17

        dc.b $22,$28, 19        ;top belt
        dc.b 2,2
        dc.b $22,$88, 19        ;bottom belt
        dc.b 4,4
        dc.b $22,$5a, 6         ;right belt
        dc.b 5,5
        dc.b $22,$48, 19        ;middle1
        dc.b 10,11 
        dc.b $22,$68, 19        ;middle2
        dc.b 18,19
        dc.b $22,$42, 7         ;boxes(top)
        dc.b 8,9
        dc.b $22,$62, 7         ;boxes(bottom)
        dc.b 16,17

        dc.b $22,$e6, 19        ;top belt
        dc.b 1,1
        dc.b $23,$46, 19        ;bottom belt
        dc.b 3,3
        dc.b $23,$05, 6         ;left belt
        dc.b 6,6
        dc.b $23,$06, 19        ;middle1
        dc.b 10,11 
        dc.b $23,$26, 19        ;middle2
        dc.b 18,19
        dc.b $23,$18, 7         ;boxes(top)
        dc.b 8,9
        dc.b $23,$38, 7         ;boxes(bottom)
        dc.b 16,17

        dc.b $23,$c0, 28*4      ;BG attrib data
        dc.b $0f,$0f,$0f,$0f,$0f,$0f,$0f,$f0
        dc.b $00,$40,$a5,$a5,$a5,$a5,$e1,$ff
        dc.b $00,$55,$55,$55,$55,$55,$55,$00
        dc.b $00,$49,$5a,$5a,$5a,$5a,$00,$00
        dc.b $00,$00,$a5,$a5,$a5,$a5,$61,$00
        dc.b $00,$40,$55,$55,$55,$55,$01,$00
        dc.b $00,$49,$5a,$5a,$5a,$5a,$00,$00

        dc.b $20,$4d, 12
        dc.b _L,_E,_V,_E,_L,0    ;level X
        dc.b 0
nowave
        dc.b $20,$4d, 9
        dc.b 0,0                ;erase wave text
        dc.b 0
wingame
        dc.b $21,$0c, 16
        dc.b _Y,_O,_U,0,_W,_I,_N,_EXP
        dc.b $21,$4c, 20
        dc.b _C,_O,_N,_G,_R,_A,_T,_S,_EXP,0
        dc.b $22,$c6, 40
        dc.b _LP,_W,_H,_A,_T,0,_D,_I,_D,0,_Y,_O,_U,0,_E,_X,_P,_E,_C,_T
        dc.b $23,$02, 56
        dc.b _I,0,_O,_N,_L,_Y,0,_H,_A,_V,_E,0,_4,_K,0,_T,_O,0,_W,_O,_R,_K,0,_W,_I,_T,_H,_RP
        dc.b $23,$4f, 4
        dc.b _FACE,0
        dc.b 0
;----------------------------
setpalette subroutine     ;a=palette# (palettes offset/3)
        sta temp0
        asl
        adc temp0
        tax                     ;x=a*3
        lda #$3f
        sta $2006
        lda palettes,x
        sta $2006
        ldy palettes+2,x
        lda palettes+1,x
        tax
.0      lda colordata,x
        sta $2007
        inx
        dey
        bne .0
        rts

colordata
        dc.b $0d,$1d,$2d,$10 ;B/W
        dc.b $0d,$0d,$0d,$0d ;conveyer
        dc.b $0d,$1c,$1c,$0d ;rollers
        dc.b $0d,$2a,$1b,$0c ;green (little boxes)
        dc.b $0d,$21,$11,$01 ;blue sprite
        dc.b $0d,$27,$16,$07 ;brown sprite
        dc.b $0d,$2a,$1b,$0c ;green sprite
        dc.b $0d,$23,$14,$04 ;purple sprite
        dc.b $08,$18,$28,$08,$18 ;conveyer colorcycle

palettes ;color#, colordata offset, #colors

ALL_PAL = 0
        dc.b 0,0,32
CONVEYER1 = 1
        dc.b 5,32,3
CONVEYER2 = 2
        dc.b 5,33,3
CONVEYER3 = 3
        dc.b 5,34,3
ROLLER1 = 4
        dc.b 9,8,3
ROLLER2 = 5
        dc.b 9,9,3

;----------------------------
;play_music subroutine   ;do channels 1,2,3
;        ldx #6
;.0      jsr do_sfx
;        bne .0
;        rts
play_sfx                ;do channel 0
        ldx #0
do_sfx                   ;call every frame. x=channel*2, x-=2 on exit
        dec sfx_time,x
        bne .1                ;do next note when time=0
        lda (sfx_ptr,x)       
        beq .1                ;stop sfx when next time=0
        tay
        dey
        beq .2                ;time=1 signals end of music
        jsr next_note
.1
        dex
        dex
        rts
.2
        jsr restartmusic        ;loop music
        ldx #0
        rts
;-----------------------
start_sfx subroutine     ;y=sfxlist offset
        ldx sfxlist,y   ;x=channel*2
        lda sfxlist+1,y
        sta sfx_ptr,x
        lda sfxlist+2,y
        sta sfx_ptr+1,x
next_note               ;x=channel*2 (don't change x)
        txa
        asl
        tay             ;y=channel*4

        jsr .1
        jsr .0
        jsr .0
        jsr .0
        jsr .0
        sta sfx_time,x
        rts
.0
        sta $4000,y
        iny
.1      lda (sfx_ptr,x)
        inc sfx_ptr,x
        bne .2
        inc sfx_ptr+1,x
.2      rts

sfxlist
PAUSE_SFX = . - sfxlist
        dc.b 0                  ;channel*2
        dc.w sfx_pause          ;ptr
TOSS_SFX = . - sfxlist
        dc.b 0
        dc.w sfx_toss
DROP_SFX = . - sfxlist
        dc.b 0
        dc.w sfx_drop
LOSTBOX_SFX = . - sfxlist
        dc.b 0
        dc.w sfx_lostbox
BOX_SFX = . - sfxlist
        dc.b 0
        dc.w sfx_box
COLORSET_SFX = . - sfxlist
        dc.b 0
        dc.w sfx_colorset
NEXTLEVEL_SFX = . - sfxlist
        dc.b 0
        dc.w sfx_nextlevel
MUSIC_NOISE_SFX = . - sfxlist
        dc.b 6
        dc.w sfx_music_noise
MUSIC_TRI_SFX = . - sfxlist
        dc.b 4
        dc.w sfx_music_tri
MUSIC_SQ_SFX = . - sfxlist
        dc.b 2
        dc.w sfx_music_sq
              ;reg0,reg1,reg2,reg3,time, reg0,..., 0
SP = 2 ;music speed

sfx_pause
        dc.b  $1f,$00,$40,$39,6, $1f,$00,$00,$4a,8, $1f,$00,$40,$39,6, $1f,$00,$00,$4a,2, 0
sfx_toss
        dc.b  $85,$9d,$00,$24,2, 0
sfx_drop
        dc.b  $85,$95,$80,$22,2, 0
sfx_lostbox
        dc.b  $2f,$c2,$80,$02,2, 0
sfx_box
        dc.b  $9a,$00,$00,$3b,6, $9a,$00,$00,$3b,2, 0
sfx_colorset
        dc.b  $9a,$00,$00,$3b,6, $9a,$00,$00,$3b,6, $9a,$00,$00,$3a,2, 0
sfx_nextlevel
        dc.b  $9a,$00,$00,$3b,6, $9a,$00,$60,$3a,6, $9a,$00,$00,$3a,6, $9a,$00,$80,$39,2, 0
sfx_music_noise
        dc.b  $10,$00,$00,$00,4*SP+2, $1a,$00,$04,$48,3*SP, $1a,$00,$04,$48,13*SP, $1a,$00,$04,$48,3*SP, $1a,$00,$04,$48,5*SP, $1a,$00,$04,$48,8*SP, $1a,$00,$04,$48,3*SP, $1a,$00,$04,$48,12*SP, $1a,$00,$04,$48,1*SP
        dc.b  $1a,$00,$04,$48,3*SP, $1a,$00,$04,$48,13*SP, $1a,$00,$04,$48,8*SP, $1a,$00,$04,$48,7*SP, $1a,$00,$04,$48,1*SP, $1a,$00,$04,$48,8*SP, $1a,$00,$04,$48,8*SP, $1a,$00,$04,$48,8*SP, $1a,$00,$04,$48,3*SP
        dc.b  $1a,$00,$04,$48,5*SP, $1a,$00,$04,$48,8*SP, $1a,$00,$04,$48,4*SP-2, 0
sfx_music_tri
        dc.b  $80,0,0,0,2, $3f,$00,$53,$09,8*SP, $3f,$00,$0d,$09,4*SP, $3f,$00,$e3,$08,4*SP, $3f,$00,$53,$09,7*SP, $3f,$00,$53,$09,1*SP, $3f,$00,$0d,$09,4*SP, $3f,$00,$e3,$08,4*SP, $3f,$00,$5d,$0a,8*SP
        dc.b  $3f,$00,$fd,$09,4*SP, $3f,$00,$94,$09,4*SP, $3f,$00,$5d,$0a,8*SP, $3f,$00,$c5,$09,4*SP, $3f,$00,$03,$0a,4*SP-2, 0
sfx_music_sq
        dc.b  $10,0,0,0,32*SP+2, $10,0,0,0,32*SP, $1f,$08,$a7,$5a,7*SP, $1f,$08,$a7,$5a,1*SP, $1f,$08,$1b,$5a,4*SP, $1f,$08,$c5,$59,4*SP, $1f,$08,$a7,$5a,4*SP, $1f,$08,$a7,$5a,4*SP, $1f,$08,$1b,$5a,4*SP, $1f,$08,$c5,$59,4*SP
        dc.b  $1f,$08,$b9,$5c,7*SP, $1f,$08,$b9,$5c,1*SP, $1f,$08,$4e,$5e,7*SP, $1f,$08,$4e,$5e,1*SP, $1f,$08,$b9,$5c,4*SP, $1f,$08,$4e,$5d,4*SP, $1f,$08,$9e,$5d,4*SP, $1f,$08,$14,$5f,4*SP-2, 1
;----------------------------
 SEG vectors
        ORG   $FFFA,0
        dc.w  nmi
        dc.w  reset
        dc.w  reset ;(IRQ)
