 ;;--- CODE START ---;;
    .inesprg 1
    .inesmap 0
    .inesmir 1
    .ineschr 0

    .bank 1

    .org $FFFA
    .dw 0        ; no VBlank
    .dw Start    ; address to execute on reset
    .dw 0        ; no whatever
    .bank 0
    .org $0000
    
reset:
    sei          ; disable IRQs
    cld          ; disable decimal mode
    ldx #$40
    stx $4017    ; disable APU frame IRQ
    ldx #$FF
    txs          ; Set up stack
    inx          ; now X = 0
    stx $2000    ; disable NMI
    stx $2001    ; disable rendering
    stx $4010    ; disable DMC IRQs

nmi:
    lda        #$00
    sta        $2003        ; Set the low byte (00) of the ram address
    lda        #$02
    sta        $4014        ; Set the high byte (02) of the RAM address,
    
; actually, these variables could be stored in only one byte
; maybe a little bit less...

PPUMASK = $2001
PPUADDR = $2006
PPUDATA = $2007

speed1_Up:   .db 0
speed1_Left:   .db 0
speed1_Down:   .db 0
speed1_Right:   .db 0
speed2_Up:   .db 0
speed2_Down:   .db 0
speed2_Left:   .db 0
speed2_Right:   .db 0
; actually, these variables could be stored in only one byte
speed1_Up_To_Be:   .db 0
speed1_Down_To_Be:   .db 0
speed1_Left_To_Be:   .db 0
speed1_Right_To_Be:   .db 0
speed2_Up_To_Be:   .db 0
speed2_Down_To_Be:   .db 0
speed2_Left_To_Be:   .db 0
speed2_Right_To_Be:   .db 0

Sprite1_T_To_Be:     .db  0   ; sprite #1's Tile Number
Sprite2_T_To_Be:     .db  0   ; sprite #1's Tile Number

edge_horiz1_pos:     .db 0
edge_vert1_pos:     .db 0
edge_horiz2_pos:    .db 0
edge_vert2_pos:     .db 0

loop_count: .db 0

addrLO: .db 0  ; make "variable"s for our indirect addressing
addrHI: .db 0

vidlow: .db 0
vidhigh: .db 0

src: .db 0

    .org $0200 ; OAM Copy location $0300
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
Sprite3_Y:     .db  0   ; 
Sprite3_T:     .db  0   ; This will be the score of P1
Sprite3_S:     .db  0   ; 
Sprite3_X:     .db  0   ; 
Sprite4_Y:     .db  0   ;
Sprite4_T:     .db  0   ; This will be the score of P2
Sprite4_S:     .db  0   ;
Sprite4_X:     .db  0   ;

    .org $8000  ; code starts at $8000 or $C000
Start:
    cld ; supposedly useless but, let's keep it for now

    ldx #$00    ; clear X            ;; start of palette loading code
    lda #$3F    ; have $2006 tell
    sta $2006   ; $2007 to start
    lda #$00    ; at $3F00 (palette).
    sta $2006    
    
    jsr init_pal

    jsr load_to_ppu
    jsr turn_on_PPU
    
    
    jsr init_background  ; calls the routine to load the backgrounds
    jsr init_players_positions
    jsr init_scores
    jsr init_sound
    jsr title_screen ; loads the title_screen
    
    
;; THE MAIN LOOP
infinite:  ; a label to start our infinite loop

    jsr disp_Sprites_DMA
    jsr strobe_keypad1
    jsr update_keypad1   
    jsr move_charac1

    inc loop_count ; this variable will enable to distribute operations over different steps of the loop_count

    lda loop_count
    cmp #2
    beq modulo2

    lda loop_count
    cmp #4
    beq modulo4

    lda loop_count
    cmp #8
    beq modulo8
    
    jmp infinite

modulo8:
    clc ;; THIS THING IS FUCKING MANDATORY AFTER A CMP
    ;; OTHERWISE, ODD EFFECTS WILL APPEAR
    jsr update_speed1
    ldx #0
    stx loop_count
    jmp infinite
    
modulo4:
    clc
    ldy Sprite1_Y
    cpy #24
    bne south_test
    jsr player1_loose
south_test:
    clc
    cpy #224
    bne west_test
    jsr player1_loose
west_test:
    clc
    ldy Sprite1_X
    cpy #248
    bne modulo4rts
    jsr player1_loose
modulo4rts:
    clc
    jmp infinite
    
modulo2:
    clc
    ldy Sprite1_X
    cpy #2
    bne modulo2rts
    jsr player1_loose
modulo2rts:
    clc
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
        sta Sprite1_T_To_Be
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
        sta Sprite1_T_To_Be
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
        sta Sprite1_T_To_Be
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
        sta Sprite1_T_To_Be
        lda #0
        sta speed1_Up_To_Be
        sta speed1_Down_To_Be
        sta speed1_Left_To_Be
    NOTHINGdown:
        rts

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

player1_loose:  ;; the following could (and will) be improved, a loop would be cleaner !
    jsr make_sound
    ldx #16
    stx Sprite1_T
    ldy #16    
loopAnim:
    jsr disp_Sprites_DMA
    inc Sprite1_T
    jsr disp_Sprites_DMA
    nop
    dey
    bne loopAnim  
    jsr init_players_positions
    inc Sprite4_T ;Recall that Sprite4_T is also the score of P2
    ldy #0
    sty loop_count
    rts
    
player2_loose:
    jsr init_players_positions
    inc Sprite3_T
    ldy #0
    sty loop_count
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
    ldx Sprite1_T_To_Be
    stx Sprite1_T
    rts
    
update_speed2:
    ldx speed2_Up_To_Be
    stx speed2_Up
    ldx speed2_Down_To_Be
    stx speed2_Down
    ldx speed2_Left_To_Be
    stx speed2_Left
    ldx speed2_Right_To_Be
    stx speed2_Right
    ldx Sprite2_T_To_Be
    stx Sprite2_T
    rts

disp_Sprites_DMA: ;; Displays all the sprites using Direct Memory Access. So cool
    waitblank:
        bit $2002  ; these 3 lines wait for VBlank, this loop will actually miss VBlank
        bpl waitblank ; alot, in a later Day, I'll give a better way.
    lda #2
    sta $4014
    lda #0;
    rts

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

init_pal:                ; this is a freaky loop
    lda palette, x  ; that gives 32 numbers
    sta $2007       ; to $2007, ending when
    inx             ; X is 32, meaning we
    cpx #32         ; are done.
    bne init_pal     ; if X isn't =32, goto "loadpal:" line.
    rts

palette: 
    ; this upper line is used for the background palette
    .db $0F,$2D,$16,$2C,$0F,$2D,$16,$2C,$0F,$2D,$16,$2C,$0F,$2D,$16,$2C
    ; this lower line is used for the sprites palette
    .db $0F,$08,$07,$06,$0F,$01,$03,$02,$0F,$3A,$3A,$3A,$0F,$3A,$3A,$3A


title_screen:
    jsr turn_off_PPU
    lda #255 ; These lines allow to scroll the background
    sta $2005 ; X POS first
    lda #0 ; Y POS 
    sta $2005 ; store it at the same address
    jsr turn_on_PPU
title_screen_loop:    
    jsr strobe_keypad1
    lda $4016  ; load Abutton Status ; note that whatever we ain't interested   
    lda $4016  ; load Bbutton Status ; in we just load so it'll go to the next one.
    lda $4016  ; load Select button status
    lda $4016  ; load Start button status
    and #1      ; AND status with #1
    bne jump_to_rts_title_screen; for some reason (not gonna reveal yet), need to use NotEquals
    jmp title_screen_loop
    
jump_to_rts_title_screen:
    clc
    jsr turn_off_PPU
    lda #0
    sta $2005 ; This address regards scrolling
    lda #0
    sta $2005 
    jsr turn_on_PPU
    rts
    
;; THE SOUND PART
init_sound:
    lda #$FF   ; typical
    sta $4000  ; write
    lda #%11011011  ; % means binary number, remember the '#' for immediate values.
    sta $4001  ; immediate means "not an address, just a number".
    rts
;; THIS COULD BE SOMEHOW IMPROVED, BUT IT IS NOT THE PRIORITY
make_sound: 
    lda #$A5
    sta $4002
    lda #$AB
    sta $4003
    lda #%00000001
    sta $4015
    rts

    
init_players_positions:
    clc ; once again, removing it would cause nasty bugs
    lda #0
    sta loop_count
    sta Sprite2_S;
    sta Sprite1_T;
    sta Sprite1_T_To_Be
    lda #1;
    sta Sprite2_T
    sta Sprite1_S;
    lda #124;
    sta Sprite1_X;
    sta Sprite2_X
    lda #164
    sta Sprite1_Y
    lda #124
    sta Sprite2_Y
    lda #0
    sta speed1_Down;    
    sta speed1_Right;    
    sta speed1_Left;
    sta speed1_Up;
    sta speed2_Down;    
    sta speed2_Right;    
    sta speed2_Left;
    sta speed2_Up;
    sta speed1_Down_To_Be
    sta speed1_Right_To_Be
    sta speed1_Left_To_Be
    sta speed2_Up_To_Be
    sta speed2_Right_To_Be
    sta speed2_Left_To_Be
    lda #1
    sta speed1_Up_To_Be
    sta speed2_Down_To_Be
    rts 
    
init_scores:
    lda #6
    sta Sprite3_T
    sta Sprite4_T
    lda #0;
    sta Sprite3_S
    sta Sprite4_S
    lda #15
    sta Sprite3_Y
    sta Sprite4_Y
    lda #72
    sta Sprite3_X
    lda #238
    sta Sprite4_X
    rts
    
init_background: ; uses indirect addressing
    ldx #0
    lda #$20  ; set the destination address in PPU memory
    sta $2006  ; should be $2000
    stx $2006
    
    lda #low(background)   ; put the high and low bytes of the address "backg"
    sta addrLO        ; into the variables so we can use indirect addressing.
    lda #high(background)
    sta addrHI
    ldx #16  ; number of 256-byte chunks to load
    ldy #0
loopIndirectAddressing:
    lda [addrLO],y
    sta $2007     ; load 256 bytes
    iny
    bne loopIndirectAddressing
;--------------------
    inc addrHI  ; increment high byte of address backg to next 256 byte chunk
    dex        ; one chunk done so X = X - 1.
    bne loopIndirectAddressing   ; if X isn't zero, do again
    
    rts

;; OKAY, this is a little bit scarying, but it is ways better than using the strange
;; programs proposed to create the .bin files..

load_to_ppu:
    ldy #0       ;; starting index into the first page
    sty PPUMASK  ;; turn off rendering just in case
    sty PPUADDR  ;; load the destination address into the PPU
    sty PPUADDR

   lda #0  ; set the destination address in PPU memory
     sta $2006  ; should be $0000
     sta $2006
     lda #low(ppumem)   ; put the high and low bytes of the address ppumem
     sta vidlow        ; into the variables so we can use indirect addressing.
     lda #high(ppumem)
     sta vidhigh

   ldx #32  ; number of 256-byte chunks to load
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

turn_on_PPU:
    lda #%00001000  ;
    sta $2000       ;
    lda #%00011110  ; Our typical PPU Setup code.
    sta $2001       ;
    rts

turn_off_PPU:
    lda #%00000000  ;
    sta $2000       ;
    lda #%00000000  ; Our typical PPU Setup code.
    sta $2001       ;
    rts

title:
    .db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    .db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    .db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    .db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    .db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    .db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    .db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    .db 0,0,0,0,0,0,0,0,0,0,0,42,43,44,47,48,55,56,57,64,65,66,0,0,0,0,0,0,0,0,0,0
    .db 0,0,0,0,0,0,0,0,0,0,0,0,45,0,49,50,63,0,62,67,68,69,0,0,0,0,0,0,0,0,0,0
    .db 0,0,0,0,0,0,0,0,0,0,0,0,45,0,51,54,58,0,62,70,71,72,0,0,0,0,0,0,0,0,0,0
    .db 0,0,0,0,0,0,0,0,0,0,0,0,46,0,52,53,59,60,61,73,74,75,0,0,0,0,0,0,0,0,0,0
    .db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    .db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    .db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    .db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    .db 0,0,0,0,0,0,76,77,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,83,84,0,0,0,0,0
    .db 0,0,0,0,0,0,78,79,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,85,86,0,0,0,0,0
    .db 0,0,0,0,0,0,80,81,82,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,87,88,89,0,0,0,0,0
    .db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    .db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    .db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    .db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    .db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    .db 0,0,0,0,0,0,0,0,0,30,32,19,33,33,0,33,34,15,32,34,0,0,0,0,0,0,0,0,0,0,0,0
    .db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    .db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    .db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    .db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    
background:    
    .db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    .db 0,16,39,0,24,35,26,23,19,28,0,32,19,36,19,26,26,19,0,0,0,0,0,0,0,7,5,6,8,0,0,0,0
    .db 0,0,0,0,0,0,0,00,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    .db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    .db 30,26,15,39,19,32,0,6,0,0,0,0,0,0,0,0,0,0,0,0,0,30,26,15,39,19,32,0,7,0,0,0
    .db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    .db 0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2
    .db 0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2
    .db 0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2
    .db 0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2
    .db 0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2
    .db 0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2
    .db 0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2
    .db 0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2
    .db 0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2
    .db 0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2
    .db 0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2
    .db 0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2
    .db 0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2
    .db 0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2    
    .db 0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2
    .db 0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2
    .db 0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2
    .db 0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2
    .db 0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2
    .db 0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2    
    .db 0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2
    .db 0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2
    .db 0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2
    .db 0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2
    .db 0,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4
    .db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    .db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

ppumem: .incbin "bkg.nes"
my_sprites: .incbin "sprites.nes"
