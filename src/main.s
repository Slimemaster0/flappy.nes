; vim: set syntax=asm_ca65:
; vim:fileencoding=utf-8:foldmethod=marker

.segment "HEADER"
    .byte $4E, $45, $53, $1A
    .byte 2
    .byte 1
    .byte $01, $00

.export Main
.segment "CODE"

.proc Main
    rti
.endproc

.segment "STARTUP"

POSITION = 	$00
SUBPOSITION = 	$01
SPEEDHI = 	$02
SPEEDLOW = 	$03
JUSTFLAPPED = 	$04

RESET: ; {{{
    SEI ; Disable Interupts
    CLD ; Turn off decimal as its not supported on the NES


    ldx #%1000000 ; Disable sound IRQ
    stx $4017
    ldx #$00
    stx $4010     ; Disable PCM

    ; Initialize the stack
    ldx #$FF
    txs

    ; Clear PPU registers
    ldx #$00
    stx $2000
    stx $2001

    ; Wait for VBlank
:
    bit $2002
    bpl :-

    ; Clear the memory
    txa
CLEARMEMORY: ; Clear the memory from $0000 to $07ff
    sta $0000, x
    sta $0100, x
    sta $0300, x
    sta $0400, x
    sta $0500, x
    sta $0600, x
    sta $0700, x
    lda #$FF
    sta $0200, x
    lda #$00
    inx
    cpx #$00
    bne CLEARMEMORY

    ; Prepair PPU for writting palette data.
    lda #$3f
    sta $2006
    lda #$00
    sta $2006

    ldx #$00
LOADPALETTES:
    lda PALETTEDATA, x
    sta $2007
    inx
    cpx #$20
    bne LOADPALETTES

LOADBACKGROUND:
    lda $2002 ; Read PPU status to reset high/low latch
    lda #$23
    sta $2006
    lda #$80
    sta $2006
    ldx #$00
LOADBACKGROUND1:
    txa
    and #$01
    tay
    lda ROW1, y
    sta $2007
    inx
    cpx #$20
    bne LOADBACKGROUND1
    ldx #$00
LOADBACKGROUND2:
    txa
    and #$01
    tay
    lda ROW2, y
    sta $2007
    inx
    cpx #$20
    bne LOADBACKGROUND2

    lda $2002 ; Read PPU status to reset high/low latch
    lda #$23
    sta $2006
    lda #$f8
    sta $2006
    ldx #$00
    lda #%00000101
LOADBACKGROUNDCOLOR:
    sta $2007
    inx
    cpx #$08
    bne LOADBACKGROUNDCOLOR
    

    ; Reset scroll
    lda #$00
    sta $2005
    sta $2005

    ; Enable Interupts
    cli

    lda #%10010000
    sta $2000 		; When VBlank occurs call NMI

    lda #%00011110 	; Show sprites and background
    sta $2001

    ; Initialize valubles
    lda #$00
    sta POSITION
    sta SUBPOSITION
    sta SPEEDHI
    sta SPEEDLOW
    sta JUSTFLAPPED

    INFLOOP: 
	jmp INFLOOP
; }}}

NMI:

    lda #$02 	; Load sprite range
    sta $4014

GAMELOOP:

    clc
    lda POSITION
    cmp #$10
    bcc MAXHEIGHT

    ; Read input
    lda #$01
    ; While the strobe bit is set, buttons will be continuously reloaded.
    ; This means that reading from JOYPAD1 will only return the state of the
    ; first button: button A.
    sta $4016
    lsr a        ; now A is 0
    ; By storing 0 into $4016, the strobe bit is cleared and the reloading stops.
    ; This allows all 8 buttons (newly reloaded) to be read from $4016.
    sta $4016
    lda $4016

    clc
    cmp #$41
    bcs FLAP

    lda #$00
    sta JUSTFLAPPED

CALCULATESPEED:
    clc
    ; Calculate gravity
    ; low byte
    lda SPEEDLOW
    adc #$10
    sta SPEEDLOW
    ; high byte
    lda SPEEDHI
    adc #$00
    sta SPEEDHI
    tax ; store low byte in X for quick acces
    
    cmp #128
    bcs NEGATIVESPEED

APPLYSPEED:
    ; Apply low speed
    lda SUBPOSITION
    adc SPEEDLOW
    sta SUBPOSITION
    ; Apply high speed
    lda POSITION
    adc SPEEDHI
    sta POSITION

PLACEPLAYERINIT:
    ldx #$00
PLACEPLAYER:
    txa
    and #%00000011
    cmp #$01
    lda PLAYERSPRITE, x
    bcc APPLYOFFSET
POSTOFFSET:
    sta $0200, x
    inx
    cpx #$10
    bne PLACEPLAYER

   jmp INFLOOP 


APPLYOFFSET: ; {{{
    clc
    adc POSITION
    jmp POSTOFFSET

    ; }}}

FLAP: ; {{{
    lda JUSTFLAPPED
    cmp #$01
    bcs CALCULATESPEED

    lda #$01
    sta JUSTFLAPPED

    
    lda #$00
    sta SPEEDLOW
    lda #$fe
    sta SPEEDHI

    jmp APPLYSPEED

; }}}

NEGATIVESPEED: ; {{{
    clc

    ; Low byte
    lda SUBPOSITION
    adc SPEEDLOW
    STA SUBPOSITION

    ; High byte
    txa
    adc POSITION
    sta POSITION
    
    jmp PLACEPLAYERINIT

    ; }}}

MAXHEIGHT: ; {{{
    lda #$11
    sta POSITION
    sta SUBPOSITION
    lda #$00
    sta SPEEDLOW
    sta SPEEDHI

    jmp PLACEPLAYERINIT

; }}}


; Sprite data
PLAYERSPRITE:
    .byte $00, $00, $00, $40
    .byte $00, $01, $00, $48
    .byte $08, $10, $00, $40
    .byte $08, $11, %00000001, $48

PALETTEDATA:
    .byte $31, $0a, $1a, $3a, 	$31, $07, $17, $19, 	$31, $0d, $28, $27, 	$00, $34, $24, $14 	; background palettes
    .byte $31, $0d, $28, $20, 	$31, $16, $28, $20, 	$00, $0F, $30, $27, 	$00, $3C, $2C, $1C 	; sprite palettes

ROW1:
    .byte $32, $33
ROW2:
    .byte $42, $43
    
.segment "VECTORS"
    .word NMI
    .word RESET

.segment "CHARS"
.incbin "../gfx/sprites.chr"
.incbin "../gfx/background.chr"
