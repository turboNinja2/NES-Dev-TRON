                                ;iNES header
                                
      4E 45 53 1A               .db "NES",$1a       ;iNES identifier
                                
      01                        .db $01         ;number of PRG-ROM blocks
                                
      01                        .db $01         ;number of CHR-ROM blocks
                                
      00 00                     .db $00, $00      ;ROM control bytes: Horizontal mirroring, no SRAM or trainer, Mapper #0
                                
      00 00 00 00 00 00 00 00   .db $00,$00,$00,$00,$00,$00,$00,$00   ;filler
                                
                                ;PRG-ROM
                                
                                .include "main.asm"
                                    .org $0300 ; OAM Copy location $0300
00300                           ;; THE FIRST TWO SPRITES REPRESENT THE MOTORBIKES
00300 00                        Sprite1_Y:     .db  0   ; sprite #1's Y value
00301 00                        Sprite1_T:     .db  0   ; sprite #1's Tile Number
00302 00                        Sprite1_S:     .db  0   ; sprite #1's special byte
00303 00                        Sprite1_X:     .db  0   ; sprite #1's X value
00304 00                        Sprite2_Y:     .db  0   ; same thing, same order for sprite #2
00305 00                        Sprite2_T:     .db  0   ; note that I numbered 1 2 ...
00306 00                        Sprite2_S:     .db  0   ; some people may actually prefer starting
00307 00                        Sprite2_X:     .db  0   ; the count at 0, but it doesn't really matter.
00308                           ;; THESE SPRITES WILL REPRESENT THE SCORE OF EACH PLAYER
00308                           ;; BETWEEN 0 AND 9
00308 00                        Sprite3_Y:     .db  0   ; sprite #1's Y value
00309 00                        Sprite3_T:     .db  0   ; sprite #1's Tile Number
0030A 00                        Sprite3_S:     .db  0   ; sprite #1's special byte
0030B 00                        Sprite3_X:     .db  0   ; sprite #1's X value
0030C 00                        Sprite4_Y:     .db  0   ; same thing, same order for sprite #2
0030D 00                        Sprite4_T:     .db  0   ; note that I numbered 1 2 ...
0030E 00                        Sprite4_S:     .db  0   ; some people may actually prefer starting
0030F 00                        Sprite4_X:     .db  0   ; the count at 0, but it doesn't really matter.
00310                           
00310 00 00 00 00 00 00 00 00..     .org $0500
00500                           ;; This part allows variables declarations (or definition)
00500 00                        speed1_Up:   .db 0
00501 00                        speed1_Left:   .db 0
00502 00                        speed1_Down:   .db 0
00503 00                        speed1_Right:   .db 0
00504 00                        speed2_Up:   .db 0
00505 00                        speed2_Down:   .db 0
00506 00                        speed2_Left:   .db 0
00507 00                        speed2_Right:   .db 0
00508                           
00508 00                        speed1_Up_To_Be:   .db 0
00509 00                        speed1_Down_To_Be:   .db 0
0050A 00                        speed1_Left_To_Be:   .db 0
0050B 00                        speed1_Right_To_Be:   .db 0
0050C 00                        speed2_Up_To_Be:   .db 0
0050D 00                        speed2_Down_To_Be:   .db 0
0050E 00                        speed2_Left_To_Be:   .db 0
0050F 00                        speed2_Right_To_Be:   .db 0
00510 00                        loop_count: .db 0
00511                           
00511                           
00511 00 00 00 00 00 00 00 00..     .org $8000  ; code starts at $8000 or $C000
08000                           Start:
08000 A9 01                         lda #1;
08002 8D 08 05                      sta speed1_Up_To_Be;
08005 8D 04 05                      sta speed2_Up;
08008 8D 05 03                      sta Sprite2_T
0800B 8D 02 03                      sta Sprite1_S;
0800E A9 00                         lda #0;
08010 8D 06 03                      sta Sprite2_S;
08013 8D 01 03                      sta Sprite1_T;
08016 8D 02 05                      sta speed1_Down;    
08019 8D 03 05                      sta speed1_Right;    
0801C 8D 01 05                      sta speed1_Left;
0801F 8D 05 05                      sta speed2_Down;    
08022 8D 07 05                      sta speed2_Right;    
08025 8D 06 05                      sta speed2_Left;
08028                               ;lda #0
08028                               
08028 A9 50                         lda #80;
0802A 8D 03 03                      sta Sprite1_X;
0802D 8D 07 03                      sta Sprite2_X
08030                               
08030 A9 50                         lda #80
08032 8D 00 03                      sta Sprite1_Y
08035 A9 78                         lda #120
08037 8D 04 03                      sta Sprite2_Y
0803A                           
0803A A9 08                         lda #%00001000  ;
0803C 8D 00 20                      sta $2000       ;
0803F A9 1E                         lda #%00011110  ; Our typical PPU Setup code.
08041 8D 01 20                      sta $2001       ;
08044                           
08044 A2 00                         ldx #$00    ; clear X            ;; start of palette loading code
08046 A9 3F                         lda #$3F    ; have $2006 tell
08048 8D 06 20                      sta $2006   ; $2007 to start
0804B A9 00                         lda #$00    ; at $3F00 (palette).
0804D 8D 06 20                      sta $2006
08050                               
08050                           loadpal:                ; this is a freaky loop
08050 BD DA 81                      lda palette, x  ; that gives 32 numbers
08053 8D 07 20                      sta $2007       ; to $2007, ending when
08056 E8                            inx             ; X is 32, meaning we
08057 E0 20                         cpx #32         ; are done.
08059 D0 F5                         bne loadpal     ; if X isn't =32, goto "loadpal:" line.
0805B                           ;; end of palette loading code
0805B                            ;;--- CODE START ---;;
0805B                           
0805B                               
0805B 20 FA 81                      jsr dispBackground  ; calls the routine to display the background
0805E                           
0805E                           
0805E                           ;; THE MAIN LOOP
0805E                           
0805E                           infinite:  ; a label to start our infinite loop
0805E                               waitblank:
0805E 2C 02 20                          bit $2002  ; these 3 lines wait for VBlank, this loop will actually miss VBlank
08061 10 FB                             bpl waitblank ; alot, in a later Day, I'll give a better way.
08063                           
08063 20 B8 81                      jsr disp_Sprites_DMA
08066 20 51 81                      jsr move_charac1    
08069 20 76 81                      jsr move_charac2    
0806C 20 C0 81                      jsr strobe_keypad1
0806F 20 CD 81                      jsr strobe_keypad2
08072 20 91 80                      jsr update_keypad1   
08075                               
08075 AE 10 05                      ldx loop_count
08078 E8                            inx
08079 8E 10 05                      stx loop_count
0807C 8A                            txa
0807D C9 08                         cmp #8
0807F F0 03                         beq modulo
08081 4C 5E 80                      jmp infinite
08084                           
08084                           modulo:
08084 20 9B 81                      jsr update_speed1
08087 A2 00                         ldx #0
08089 8E 10 05                      stx loop_count
0808C A9 00                         lda #0;
0808E 4C 5E 80                      jmp infinite
08091                           
08091                            update_keypad1:
08091 AD 16 40                      lda $4016  ; load Abutton Status ; note that whatever we ain't interested   
08094 AD 16 40                      lda $4016  ; load Bbutton Status ; in we just load so it'll go to the next one.
08097 AD 16 40                      lda $4016  ; load Select button status
0809A AD 16 40                      lda $4016  ; load Start button status
0809D AD 16 40                      lda $4016  ; load UP button status
080A0 29 01                         and #1      ; AND status with #1
080A2 D0 18                         bne UPKEYdown  ; for some reason (not gonna reveal yet), need to use NotEquals
080A4                               ;with ANDs. So it'll jump (branch) if key was down.
080A4 AD 16 40                      lda $4016  ; load DOWN button status
080A7 29 01                         and #1     ; AND status with #1
080A9 D0 35                         bne DOWNKEYdown
080AB AD 16 40                      lda $4016  ; load LEFT button status
080AE 29 01                         and #1     ; AND status with #1
080B0 D0 57                         bne LEFTKEYdown
080B2 AD 16 40                      lda $4016  ; load RIGHT button status
080B5 29 01                         and #1     ; AND status with #1
080B7 D0 76                         bne RIGHTKEYdown
080B9                               ;ldy $0
080B9 4C 06 81                      jmp NOTHINGdown2  ; if nothing was down, we just jump (no check for conditions)
080BC                               ; down past the rest of everything.
080BC                               UPKEYdown:
080BC AD 02 05                          lda speed1_Down;_To_Be
080BF 29 01                             and #1
080C1 D0 43                             bne NOTHINGdown2
080C3 AD 09 05                          lda speed1_Down_To_Be
080C6 29 01                             and #1
080C8 D0 3C                             bne NOTHINGdown2
080CA A9 01                             lda #1
080CC 8D 08 05                          sta speed1_Up_To_Be
080CF A9 00                             lda #0
080D1 8D 0A 05                          sta speed1_Left_To_Be
080D4 8D 09 05                          sta speed1_Down_To_Be
080D7 8D 0B 05                          sta speed1_Right_To_Be
080DA 8D 01 03                          sta Sprite1_T
080DD 4C 06 81                          jmp NOTHINGdown2  ; jump over the rest of the handling code
080E0                               DOWNKEYdown:
080E0 AD 00 05                          lda speed1_Up;_To_Be
080E3 29 01                             and #1
080E5 D0 1F                             bne NOTHINGdown2
080E7 AD 08 05                          lda speed1_Up_To_Be
080EA 29 01                             and #1
080EC D0 18                             bne NOTHINGdown2
080EE A9 01                             lda #1
080F0 8D 09 05                          sta speed1_Down_To_Be
080F3 A9 02                             lda #2
080F5 8D 01 03                          sta Sprite1_T
080F8 A9 00                             lda #0
080FA 8D 0A 05                          sta speed1_Left_To_Be
080FD 8D 0B 05                          sta speed1_Right_To_Be
08100 8D 08 05                          sta speed1_Up_To_Be
08103 4C 50 81                          jmp NOTHINGdown ; jump over the rest of handling code.
08106                               NOTHINGdown2:
08106 4C 50 81                          jmp NOTHINGdown
08109                               LEFTKEYdown:
08109 AD 03 05                          lda speed1_Right;_To_Be
0810C 29 01                             and #1
0810E D0 40                             bne NOTHINGdown
08110 AD 0B 05                          lda speed1_Right_To_Be
08113 29 01                             and #1
08115 D0 39                             bne NOTHINGdown
08117 A9 01                             lda #1
08119 8D 0A 05                          sta speed1_Left_To_Be
0811C A9 00                             lda #0
0811E 8D 08 05                          sta speed1_Up_To_Be
08121 8D 09 05                          sta speed1_Down_To_Be
08124 8D 0B 05                          sta speed1_Right_To_Be
08127 A9 03                             lda #3
08129 8D 01 03                          sta Sprite1_T
0812C 4C 50 81                          jmp NOTHINGdown
0812F                               RIGHTKEYdown:
0812F AD 01 05                          lda speed1_Left;_To_Be
08132 29 01                             and #1
08134 D0 1A                             bne NOTHINGdown
08136 AD 0A 05                          lda speed1_Left_To_Be
08139 29 01                             and #1
0813B D0 13                             bne NOTHINGdown
0813D A9 01                             lda #1
0813F 8D 0B 05                          sta speed1_Right_To_Be
08142 8D 01 03                          sta Sprite1_T
08145 A9 00                             lda #0
08147 8D 08 05                          sta speed1_Up_To_Be
0814A 8D 09 05                          sta speed1_Down_To_Be
0814D 8D 0A 05                          sta speed1_Left_To_Be
08150                               NOTHINGdown:
08150 60                                rts
08151                           
08151                           ;; Kind of "function" in assembly style, used to relieve the main loop
08151                           move_charac1:
08151 AD 00 03                      lda Sprite1_Y ; load A with Y position
08154 ED 00 05                      sbc speed1_Up  ; subtract 1 from A. Only can do math on A register. SBC (Subtract with Borrow)
08157 8D 00 03                      sta Sprite1_Y; store back to memory
0815A AD 00 03                      lda Sprite1_Y
0815D 6D 02 05                      adc speed1_Down  ; add 1 to A. ADC (Add with Carry)((to A register))
08160 8D 00 03                      sta Sprite1_Y
08163 AD 03 03                      lda Sprite1_X
08166 ED 01 05                      sbc speed1_Left
08169 8D 03 03                      sta Sprite1_X
0816C AD 03 03                      lda Sprite1_X
0816F 6D 03 05                      adc speed1_Right
08172 8D 03 03                      sta Sprite1_X
08175 60                            rts
08176                               
08176                           move_charac2:
08176 AD 04 03                      lda Sprite2_Y ; load A with Y position
08179 ED 04 05                      sbc speed2_Up  ; subtract 1 from A. Only can do math on A register. SBC (Subtract with Borrow)
0817C 8D 04 03                      sta Sprite2_Y; store back to memory
0817F AD 04 03                      lda Sprite2_Y
08182 6D 05 05                      adc speed2_Down  ; add 1 to A. ADC (Add with Carry)((to A register))
08185 8D 04 03                      sta Sprite2_Y
08188 AD 07 03                      lda Sprite2_X
0818B ED 06 05                      sbc speed2_Left
0818E 8D 07 03                      sta Sprite2_X
08191 AD 07 03                      lda Sprite2_X
08194 6D 07 05                      adc speed2_Right
08197 8D 07 03                      sta Sprite2_X
0819A 60                            rts
0819B                           
0819B                           update_speed1:
0819B AE 08 05                      ldx speed1_Up_To_Be
0819E 8E 00 05                      stx speed1_Up
081A1 AE 09 05                      ldx speed1_Down_To_Be
081A4 8E 02 05                      stx speed1_Down
081A7 AE 0A 05                      ldx speed1_Left_To_Be
081AA 8E 01 05                      stx speed1_Left
081AD AE 0B 05                      ldx speed1_Right_To_Be
081B0 8E 03 05                      stx speed1_Right
081B3 A2 00                         ldx #0;
081B5 A0 00                         ldy #0;
081B7 60                            rts
081B8                           
081B8                           disp_Sprites_DMA:
081B8 A9 03                         lda #3
081BA 8D 14 40                      sta $4014
081BD A9 00                         lda #0;
081BF 60                            rts
081C0                           ;; ----------------------------------------------------
081C0                           
081C0                           strobe_keypad1:
081C0 A9 01                         lda #$01   ; these
081C2 8D 16 40                      sta $4016  ; lines
081C5 A9 00                         lda #$00   ; setup/strobe the
081C7 8D 16 40                      sta $4016  ; keypad.
081CA A9 00                         lda #0;
081CC 60                            rts
081CD                           
081CD                           strobe_keypad2:
081CD A9 01                         lda #$01   ; these
081CF 8D 17 40                      sta $4017  ; lines
081D2 A9 00                         lda #$00   ; setup/strobe the
081D4 8D 17 40                      sta $4017  ; keypad.$
081D7 A9 00                         lda #0;
081D9 60                            rts
081DA                           
081DA                           
081DA                           palette: 
081DA                               ; this upper line is used for the background palette
081DA 0F 2D 20 20 0F 3D 3D 3D..     .db $0F,$2D,$20,$20,$0F,$3D,$3D,$3D,$0F,$3D,$3D,$3D,$0F,$3D,$3D,$3D
081EA                               ; this lower line is used for the sprites palette
081EA 0F 08 07 06 0F 01 03 02..     .db $0F,$08,$07,$06,$0F,$01,$03,$02,$0F,$3A,$3A,$3A,$0F,$3A,$3A,$3A
081FA                           
081FA                           
081FA                           ;; -------------------------------------------------------------------------
081FA                           ;; THE FOLLOWING LOOPS ENABLE TO DISPLAY THE BACKGROUND 
081FA                           ;; -------------------------------------------------------------------------
081FA                           dispBackground: 
081FA A9 20                         lda #$20
081FC 8D 06 20                      sta $2006 ; give $2006 both parts of address $2020.
081FF A9 00                         lda #$00
08201 8D 06 20                      sta $2006
08204                           
08204                               ;; This displays the background using ourMap, defined ways below
08204 A2 00                             ldx #$00
08206                               loadNames1:
08206 BD 65 82                          lda ourMap1, X ; load A with a byte from address (ourMap + X)
08209 E8                                inx
0820A 8D 07 20                          sta $2007
0820D E0 80                             cpx #128 ; map in previous section 64 bytes long
0820F D0 F5                             bne loadNames1; if not all 64 done, loop and do some more
08211                                   
08211 A2 00                             ldx #$00
08213                               loadNames2:
08213 BD E5 82                          lda ourMap2, X ; load A with a byte from address (ourMap + X)
08216 E8                                inx
08217 8D 07 20                          sta $2007
0821A E0 80                             cpx #128 ; map in previous section 64 bytes long
0821C D0 F5                             bne loadNames2; if not all 64 done, loop and do some more
0821E A2 00                             ldx #$00
08220                                   
08220                               loadNames3:
08220 BD E5 82                          lda ourMap2, X ; load A with a byte from address (ourMap + X)
08223 E8                                inx
08224 8D 07 20                          sta $2007
08227 E0 80                             cpx #128 ; map in previous section 64 bytes long
08229 D0 F5                             bne loadNames3; if not all 64 done, loop and do some more
0822B                                   
0822B A2 00                             ldx #$00
0822D                               loadNames4:
0822D BD E5 82                          lda ourMap2, X ; load A with a byte from address (ourMap + X)
08230 E8                                inx
08231 8D 07 20                          sta $2007
08234 E0 80                             cpx #128 ; map in previous section 64 bytes long
08236 D0 F5                             bne loadNames4; if not all 64 done, loop and do some more
08238                                   
08238 A2 00                             ldx #$00
0823A                               loadNames5:
0823A BD E5 82                          lda ourMap2, X ; load A with a byte from address (ourMap + X)
0823D E8                                inx
0823E 8D 07 20                          sta $2007
08241 E0 80                             cpx #128 ; map in previous section 64 bytes long
08243 D0 F5                             bne loadNames5; if not all 64 done, loop and do some more
08245                           
08245 A2 00                             ldx #$00
08247                               loadNames6:
08247 BD E5 82                          lda ourMap2, X ; load A with a byte from address (ourMap + X)
0824A E8                                inx
0824B 8D 07 20                          sta $2007
0824E E0 80                             cpx #128 ; map in previous section 64 bytes long
08250 D0 F5                             bne loadNames6; if not all 64 done, loop and do some more
08252                           
08252 A2 00                             ldx #$00
08254                               loadNames7:
08254 BD 65 83                          lda ourMap3, X ; load A with a byte from address (ourMap + X)
08257 E8                                inx
08258 8D 07 20                          sta $2007
0825B E0 80                             cpx #128 ; map in previous section 64 bytes long
0825D D0 F5                             bne loadNames7; if not all 64 done, loop and do some more
0825F                                   
0825F A9 1E                         lda #%00011110 ; enable sprites, enable background
08261 8D 01 20                      sta $2001
08264                                   
08264 60                            rts
08265                           ;; -------------------------------------------------------------------------
08265                           
08265                           
08265                           ourMap1: 
08265 00 00 00 00 00 00 00 00..     .db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
08285 00 00 00 00 00 00 00 00..     .db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
082A5 1E 1A 0F 27 13 20 00 06..     .db 30,26,15,39,19,32,0,6,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,30,26,15,39,19,32,0,7,0
082C5 00 00 00 00 00 00 00 00..     .db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
082E5                           ourMap2: 
082E5 01 01 01 01 01 01 01 01..     .db 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2
08305 01 01 01 01 01 01 01 01..     .db 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2
08325 01 01 01 01 01 01 01 01..     .db 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2
08345 01 01 01 01 01 01 01 01..     .db 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2
08365                           ourMap3: 
08365 01 01 01 01 01 01 01 01..     .db 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2
08385 01 01 01 01 01 01 01 01..     .db 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2
083A5 01 01 01 01 01 01 01 01..     .db 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2
083C5 03 03 03 03 03 03 03 03..     .db 3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4
083E5                           
083E5                           
083E5                           ;;--- END OF CODE FILE ---;083E5                           ;.include "famitone.asm"   ;FamiTone audio driver by Shiru
083E5                           ;.include "music.asm"   ;sample music data
083E5                           
083E5 00 00 00 00 00 00 00 00.. .pad $FFFA      ;fill any remaining space with zeroes
0FFFA                           ;.dw vblank,reset,irq   ;set interrupt addresses (defined in main.asm)
0FFFA                           
0FFFA                           ;CHR-ROM
0FFFA                           
0FFFA 00 00 00 00 00 00 00 00.. .incbin "bkg.nes"
10FFA 18 18 18 18 18 18 18 18.. .incbin "sprites.nes"