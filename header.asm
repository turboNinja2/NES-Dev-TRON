;iNES header

.db "NES",$1a       ;iNES identifier

.db $01         ;number of PRG-ROM blocks

.db $01         ;number of CHR-ROM blocks

.db $00, $00      ;ROM control bytes: Horizontal mirroring, no SRAM or trainer, Mapper #0

.db $00,$00,$00,$00,$00,$00,$00,$00   ;filler

;PRG-ROM

.include "main.asm"
;.include "famitone.asm"   ;FamiTone audio driver by Shiru
;.include "music.asm"   ;sample music data

.pad $FFFA      ;fill any remaining space with zeroes
;.dw vblank,reset,irq   ;set interrupt addresses (defined in main.asm)

;CHR-ROM

.incbin "bkg.nes"
.incbin "sprites.nes"