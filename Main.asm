   ;;--- CODE START ---;;
    .inesprg 1
    .inesmap 0
    .inesmir 1
    .ineschr 1

    .bank 1
    .org $FFFA
    .dw 0        ; no VBlank
    .dw Start    ; address to execute on reset
    .dw 0        ; no whatever
    .bank 0
    .org $0000


;; What follows will be useful to make sure the cycle follows the grid


    .org $0300 ; OAM Copy location $0300
;; THE FIRST TWO SPRITES REPRESENT THE MOTORBIKES
Sprite1_Y:     .db  0   ; sprite #1's Y value
Sprite1_T:     .db  0   ; sprite #1's Tile Number
Sprite1_S:     .db  0   ; sprite #1's special byte
Sprite1_X:     .db  0   ; sprite #1's X value
Sprite2_Y:     .db  0   ; same thing, same order for sprite #2
Sprite2_T:     .db  0   ; note that I numbered 1 2 ...
Sprite2_S:     .db  0   ; some people may actually prefer starting
Sprite2_X:     .db  0   ; the count at 0, but it doesn't really matter.
;; THESE SPRITES WILL REPRESENT THE SCORE OF EACH PLAYER
;; BETWEEN 0 AND 9
Sprite3_Y:     .db  0   ; sprite #1's Y value
Sprite3_T:     .db  0   ; sprite #1's Tile Number
Sprite3_S:     .db  0   ; sprite #1's special byte
Sprite3_X:     .db  0   ; sprite #1's X value
Sprite4_Y:     .db  0   ; same thing, same order for sprite #2
Sprite4_T:     .db  0   ; note that I numbered 1 2 ...
Sprite4_S:     .db  0   ; some people may actually prefer starting
Sprite4_X:     .db  0   ; the count at 0, but it doesn't really matter.

    .org $0500
;; This part allows variables declarations (or definition)
speed1_Up:   .db 0
speed1_Left:   .db 0
speed1_Down:   .db 0
speed1_Right:   .db 0
speed2_Up:   .db 0
speed2_Down:   .db 0
speed2_Left:   .db 0
speed2_Right:   .db 0

speed1_Up_To_Be:   .db 0
speed1_Down_To_Be:   .db 0
speed1_Left_To_Be:   .db 0
speed1_Right_To_Be:   .db 0
speed2_Up_To_Be:   .db 0
speed2_Down_To_Be:   .db 0
speed2_Left_To_Be:   .db 0
speed2_Right_To_Be:   .db 0
loop_count: .db 0


    .org $8000  ; code starts at $8000 or $C000
Start:
    lda #1;
    sta speed1_Up_To_Be;
    sta speed2_Up;
    sta Sprite2_T
    sta Sprite1_S;
    lda #0;
    sta Sprite2_S;
    sta Sprite1_T;
    sta speed1_Down;    
    sta speed1_Right;    
    sta speed1_Left;
    sta speed2_Down;    
    sta speed2_Right;    
    sta speed2_Left;
    ;lda #0
    
    lda #80;
    sta Sprite1_X;
    sta Sprite2_X
    
    lda #80
    sta Sprite1_Y
    lda #120
    sta Sprite2_Y

    lda #%00001000  ;
    sta $2000       ;
    lda #%00011110  ; Our typical PPU Setup code.
    sta $2001       ;

    ldx #$00    ; clear X            ;; start of palette loading code
    lda #$3F    ; have $2006 tell
    sta $2006   ; $2007 to start
    lda #$00    ; at $3F00 (palette).
    sta $2006
    
loadpal:                ; this is a freaky loop
    lda palette, x  ; that gives 32 numbers
    sta $2007       ; to $2007, ending when
    inx             ; X is 32, meaning we
    cpx #32         ; are done.
    bne loadpal     ; if X isn't =32, goto "loadpal:" line.
;; end of palette loading code
 ;;--- CODE START ---;;

    
    jsr dispBackground  ; calls the routine to display the background


;; THE MAIN LOOP

infinite:  ; a label to start our infinite loop
    waitblank:
        bit $2002  ; these 3 lines wait for VBlank, this loop will actually miss VBlank
        bpl waitblank ; alot, in a later Day, I'll give a better way.

    jsr disp_Sprites_DMA
    jsr move_charac1    
    jsr move_charac2    
    jsr strobe_keypad1
    jsr strobe_keypad2
    jsr update_keypad1   
    
    ldx loop_count
    inx
    stx loop_count
    txa
    cmp #8
    beq modulo
    jmp infinite

modulo:
    jsr update_speed1
    ldx #0
    stx loop_count
    lda #0;
    jmp infinite

 update_keypad1:
    lda $4016  ; load Abutton Status ; note that whatever we ain't interested   
    lda $4016  ; load Bbutton Status ; in we just load so it'll go to the next one.
    lda $4016  ; load Select button status
    lda $4016  ; load Start button status
    lda $4016  ; load UP button status
    and #1      ; AND status with #1
    bne UPKEYdown  ; for some reason (not gonna reveal yet), need to use NotEquals
    ;with ANDs. So it'll jump (branch) if key was down.
    lda $4016  ; load DOWN button status
    and #1     ; AND status with #1
    bne DOWNKEYdown
    lda $4016  ; load LEFT button status
    and #1     ; AND status with #1
    bne LEFTKEYdown
    lda $4016  ; load RIGHT button status
    and #1     ; AND status with #1
    bne RIGHTKEYdown
    ;ldy $0
    jmp NOTHINGdown2  ; if nothing was down, we just jump (no check for conditions)
    ; down past the rest of everything.
    UPKEYdown:
        lda speed1_Down;_To_Be
        and #1
        bne NOTHINGdown2
        lda speed1_Down_To_Be
        and #1
        bne NOTHINGdown2
        lda #1
        sta speed1_Up_To_Be
        lda #0
        sta speed1_Left_To_Be
        sta speed1_Down_To_Be
        sta speed1_Right_To_Be
        sta Sprite1_T
        jmp NOTHINGdown2  ; jump over the rest of the handling code
    DOWNKEYdown:
        lda speed1_Up;_To_Be
        and #1
        bne NOTHINGdown2
        lda speed1_Up_To_Be
        and #1
        bne NOTHINGdown2
        lda #1
        sta speed1_Down_To_Be
        lda #2
        sta Sprite1_T
        lda #0
        sta speed1_Left_To_Be
        sta speed1_Right_To_Be
        sta speed1_Up_To_Be
        jmp NOTHINGdown ; jump over the rest of handling code.
    NOTHINGdown2:
        jmp NOTHINGdown
    LEFTKEYdown:
        lda speed1_Right;_To_Be
        and #1
        bne NOTHINGdown
        lda speed1_Right_To_Be
        and #1
        bne NOTHINGdown
        lda #1
        sta speed1_Left_To_Be
        lda #0
        sta speed1_Up_To_Be
        sta speed1_Down_To_Be
        sta speed1_Right_To_Be
        lda #3
        sta Sprite1_T
        jmp NOTHINGdown
    RIGHTKEYdown:
        lda speed1_Left;_To_Be
        and #1
        bne NOTHINGdown
        lda speed1_Left_To_Be
        and #1
        bne NOTHINGdown
        lda #1
        sta speed1_Right_To_Be
        sta Sprite1_T
        lda #0
        sta speed1_Up_To_Be
        sta speed1_Down_To_Be
        sta speed1_Left_To_Be
    NOTHINGdown:
        rts

;; Kind of "function" in assembly style, used to relieve the main loop
move_charac1:
    lda Sprite1_Y ; load A with Y position
    sbc speed1_Up  ; subtract 1 from A. Only can do math on A register. SBC (Subtract with Borrow)
    sta Sprite1_Y; store back to memory
    lda Sprite1_Y
    adc speed1_Down  ; add 1 to A. ADC (Add with Carry)((to A register))
    sta Sprite1_Y
    lda Sprite1_X
    sbc speed1_Left
    sta Sprite1_X
    lda Sprite1_X
    adc speed1_Right
    sta Sprite1_X
    rts
    
move_charac2:
    lda Sprite2_Y ; load A with Y position
    sbc speed2_Up  ; subtract 1 from A. Only can do math on A register. SBC (Subtract with Borrow)
    sta Sprite2_Y; store back to memory
    lda Sprite2_Y
    adc speed2_Down  ; add 1 to A. ADC (Add with Carry)((to A register))
    sta Sprite2_Y
    lda Sprite2_X
    sbc speed2_Left
    sta Sprite2_X
    lda Sprite2_X
    adc speed2_Right
    sta Sprite2_X
    rts

update_speed1:
    ldx speed1_Up_To_Be
    stx speed1_Up
    ldx speed1_Down_To_Be
    stx speed1_Down
    ldx speed1_Left_To_Be
    stx speed1_Left
    ldx speed1_Right_To_Be
    stx speed1_Right
    ldx #0;
    ldy #0;
    rts

disp_Sprites_DMA:
    lda #3
    sta $4014
    lda #0;
    rts
;; ----------------------------------------------------

strobe_keypad1:
    lda #$01   ; these
    sta $4016  ; lines
    lda #$00   ; setup/strobe the
    sta $4016  ; keypad.
    lda #0;
    rts

strobe_keypad2:
    lda #$01   ; these
    sta $4017  ; lines
    lda #$00   ; setup/strobe the
    sta $4017  ; keypad.$
    lda #0;
    rts


palette: 
    ; this upper line is used for the background palette
    .db $0F,$2D,$20,$20,$0F,$3D,$3D,$3D,$0F,$3D,$3D,$3D,$0F,$3D,$3D,$3D
    ; this lower line is used for the sprites palette
    .db $0F,$08,$07,$06,$0F,$01,$03,$02,$0F,$3A,$3A,$3A,$0F,$3A,$3A,$3A


;; -------------------------------------------------------------------------
;; THE FOLLOWING LOOPS ENABLE TO DISPLAY THE BACKGROUND 
;; -------------------------------------------------------------------------
dispBackground: 
    lda #$20
    sta $2006 ; give $2006 both parts of address $2020.
    lda #$00
    sta $2006

    ;; This displays the background using ourMap, defined ways below
        ldx #$00
    loadNames1:
        lda ourMap1, X ; load A with a byte from address (ourMap + X)
        inx
        sta $2007
        cpx #128 ; map in previous section 64 bytes long
        bne loadNames1; if not all 64 done, loop and do some more
        
        ldx #$00
    loadNames2:
        lda ourMap2, X ; load A with a byte from address (ourMap + X)
        inx
        sta $2007
        cpx #128 ; map in previous section 64 bytes long
        bne loadNames2; if not all 64 done, loop and do some more
        ldx #$00
        
    loadNames3:
        lda ourMap2, X ; load A with a byte from address (ourMap + X)
        inx
        sta $2007
        cpx #128 ; map in previous section 64 bytes long
        bne loadNames3; if not all 64 done, loop and do some more
        
        ldx #$00
    loadNames4:
        lda ourMap2, X ; load A with a byte from address (ourMap + X)
        inx
        sta $2007
        cpx #128 ; map in previous section 64 bytes long
        bne loadNames4; if not all 64 done, loop and do some more
        
        ldx #$00
    loadNames5:
        lda ourMap2, X ; load A with a byte from address (ourMap + X)
        inx
        sta $2007
        cpx #128 ; map in previous section 64 bytes long
        bne loadNames5; if not all 64 done, loop and do some more

        ldx #$00
    loadNames6:
        lda ourMap2, X ; load A with a byte from address (ourMap + X)
        inx
        sta $2007
        cpx #128 ; map in previous section 64 bytes long
        bne loadNames6; if not all 64 done, loop and do some more

        ldx #$00
    loadNames7:
        lda ourMap3, X ; load A with a byte from address (ourMap + X)
        inx
        sta $2007
        cpx #128 ; map in previous section 64 bytes long
        bne loadNames7; if not all 64 done, loop and do some more
        
    lda #%00011110 ; enable sprites, enable background
    sta $2001
        
    rts
;; -------------------------------------------------------------------------


ourMap1: 
    .db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    .db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    .db 30,26,15,39,19,32,0,6,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,30,26,15,39,19,32,0,7,0
    .db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
ourMap2: 
    .db 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2
    .db 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2
    .db 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2
    .db 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2
ourMap3: 
    .db 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2
    .db 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2
    .db 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2
    .db 3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4


    .bank 2
    .org $0000
    .incbin "bkg.nes"
    .incbin "sprites.nes"
;;--- END OF CODE FILE ---;