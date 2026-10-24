include 'ti84pceg.inc'
include 'vars.inc'

section .text

; a panel command and its parameters, as data after a call (spi_inline)
macro spi cmd, params&
	local count
	count = 0
	match any, params
		iterate param, any
			count = count + 1
		end iterate
	end match
	call	spi_inline
	db	count, cmd
	match any, params
		db	any
	end match
end macro

;;   END DEFINES

spiParam: 
	ld b,1 
	jr spiCmd.entry 
spiCmd: 
	ld b,0
.entry:
	ld	hl, ti.mpSpiStatus + 1
	ld	a, ((ti.bmSpiTxFifoBytes shr 8) and $FF) - 1
.waitNotFull:
	cp	a, (hl)
	jr	c, .waitNotFull
	ld	l, ti.spiData + 1
	ld	(hl), b
	dec	hl
	ld	(hl), c
	ret

; the spi macro's command: its data follows the call. Destroys af, bc
spi_inline:
	ex (sp),hl
	call spi_send
	ex (sp),hl
	ret

; sends a panel command: hl = the number of parameters, the command, the
; parameters. out: hl = after them. Destroys af, bc
spi_send:
	push de
	ld e,(hl)
	inc e
	ld d,0			; the command, then parameters (1)
.byte:
	inc hl
	ld c,(hl)
	ld b,d
	push hl
	call spiCmd.entry
	pop hl
	ld d,1
	dec e
	jr nz,.byte
	inc hl
	pop de
	ret
	
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

public spiSetup
public spiInitVSync
public spiInitVSyncWide
public spiEndVSync

public spi_game_window
public panel_put
public panel_put.rows

public spiLock 
public spiUnlock
public _spiLock
public _spiUnlock
public spiUnlock.locked
public spiScroll

spiSetup:
	;https://github.com/CE-Programming/toolchain/blob/master/src/lcddrvce/lcddrvce.asm
	; Always fully initialize on the first call per program invocation
	ld	hl, .fullinit
	srl	(hl)
	jr	c, .checkPython
	; Additionally, fully initialize if an APD reset the SPI state to something other than LCD
	ld	a, (ti.mpSpiCtrl0)
	cp	a, ti.bmSpiMasterMono or ti.bmSpiClkPhase or ti.bmSpiClkPolarity
	jr	z, .fastinit
.checkPython:
	; Check certificate for Python model
	ld	de, $0330
	call	ti.FindFirstCertField
	jr	nz, .notPython
	call	ti.GetFieldSizeFromType
	ld	de, $0430
	call	ti.FindField
	jr	nz, .notPython
	; Reinitializes Python hardware, probably (routine available on rev M+ boot code)
	; Without this, LCD SPI transfers start failing a short time after init
	call	$000654
	; Magic SPI initialization sequence to work on Python models
	ld	de, ti.spiSpiFrFmt or ti.bmSpiFlash or ti.bmSpiFsPolarity or ti.bmSpiMasterMono
.loop:
	ld	(ti.mpSpiCtrl0), de
	ld	hl, ti.bmSpiTxClr or ti.bmSpiRxClr
	ld	(ti.mpSpiCtrl2), hl
.fullinit:
	; Becomes a nop after being shifted
	db	1
	ld	hl, ti.bmSpiChipReset
	ld	(ti.mpSpiCtrl2), hl
	call	ti.Delay10ms
	bit	ti.bSpiClkPolarity, e
	ld	e, ti.bmSpiFsPolarity or ti.bmSpiMasterMono or ti.bmSpiClkPhase or ti.bmSpiClkPolarity
	jr	z, .loop
	ld	hl, $21
	ld	(ti.mpSpiIntCtrl), hl
.notPython:
	ld	a, ti.bmSpiMasterMono or ti.bmSpiClkPhase or ti.bmSpiClkPolarity
	ld	(ti.mpSpiCtrl0), a
.fastinit:
	ld	hl, ((9-1) shl 16) or (2-1)
	ld	(ti.mpSpiCtrl1), hl
	ld	hl, ti.bmSpiTxEn or ti.bmSpiTxClr or ti.bmSpiRxClr or ti.bmSpiChipEn
	ld	(ti.mpSpiCtrl2), hl
	ret


; changes refresh method to VSYNC timing to eliminate tearing
; makes display a 256x224 window (with only 240 pixels displayed)
spiInitVSync: 
	spi $C6,$08						; set scan speed ( around 1814.4 cycles per scanline )
	spi $B2,120,0,0					; set back porch ( (120+32)*1814.4 ~= 275000 cycles window for DMA ) 
	call spi_game_window.normal		; columns 40-295 (256 pixels), lines 8-231 (224)
	; shown: the picture (columns 40-279: the frame buffer's 8-247, spiScroll),
	; and with the FPS counter the band left of it (ui.asm fps_panel_setup)
	ld a,(_show_fps)
	or a,a
	jr nz,.band
	spi $30,0,40,$01,$17			; set partial area to middle 240 pixels 
	jr .partial
.band:
	spi $30,0,0,$01,$17				; partial area: columns 0-279
.partial:
	spi $B5,$80						; non-display is ($B5,$00 for white ; $B5,$80 for black)
	spi $B0,$12						; enable VSync Interface
	spi $12							; enable partial mode
	; hardware scrolling across the game's 256 columns (40-295): the panel
	; shows frame buffer column x + S at column x (spiScroll sets S). Columns
	; 0-39 are fixed: the LCD never writes them, so they keep the FPS counter.
	spi $33,0,40,$01,$00,0,24		; fixed 40, scrolling 256, fixed 24
	xor a,a
	jp spiScroll.send
	
; the same for the wide picture: a 320x224 window; columns 10-309 shown
; (the NES's 8-247, like the normal picture)
spiInitVSyncWide:
	spi $C6,$08
	spi $B2,120,0,0
	call spi_game_window.wide		; columns 0-319, lines 8-231
	spi $30,0,10,$01,$35			; partial area: columns 10-309
	spi $B5,$80
	spi $B0,$12						; VSync interface
	spi $12							; partial mode
	spi $33,0,0,$01,$40,0,0			; scrolling area: all 320 columns
	xor a,a
	jp spiScroll.send

; The panel's scroll for the frame the LCD sends next: a = S (the rotation
; it was drawn with, pacing.asm). The panel takes it at its next frame
; start, the LCD's next transfer. Sent only when it changes.
; Destroys af, bc, hl
spiScroll:
	ld hl,spi_scroll_sent
	cp a,(hl)
	ret z
.send:
	ld (spi_scroll_sent),a
	or a,a
	sbc hl,hl
	ld l,a
	ld a,(_wide_buffer+2)
	or a,a
	jr nz,.wide
	; normal picture: the scrolling area starts at 40, and its first 8
	; columns are hidden, so the panel shows column x + S + 8 there
	ld a,l
	add a,8
	ld l,a
	ld bc,40
	add hl,bc
.wide:
	push hl
	ld c,$37
	call spiCmd
	pop hl
	push hl
	ld c,h
	call spiParam
	pop hl
	ld c,l
	jp spiParam

; Writes w x .rows pixels (8 bpp, w bytes per line) into the panel's memory
; at x, y over SPI, then sets the LCD's window again. Only while the panel
; isn't taking the LCD's data (spiLock): the window is shared.
; in: hl = pixels, de = x, b = y, c = w. destroys af, bc, de, hl, ix
panel_put:
	push hl
	pop ix
	ld a,b
	ld (panel_window+9),a
	ld hl,.rows
	add a,(hl)
	dec a
	ld (panel_window+11),a
	ld a,d
	ld (panel_window+2),a
	ld a,e
	ld (panel_window+3),a
	ex de,hl
	ld de,0
	ld e,c
	add hl,de
	dec hl
	ld a,h
	ld (panel_window+4),a
	ld a,l
	ld (panel_window+5),a
	ex de,hl
	ld a,(.rows)
	ld h,a
	mlt hl
	push hl			; w * .rows pixels
	ld hl,panel_window
	call spi_send
	call spi_send
	call spi_send
	pop hl
.pixel:
	; its LCD palette entry as 18 bits in 3 bytes (6 bits each, at the top;
	; the panel's default format over SPI): bits 0-4, 5-9 with bit 15 as the
	; lowest (like the LCD sends it), 10-14
	push hl
	ld a,(ix+0)
	inc ix
	or a,a
	sbc hl,hl
	ld l,a
	add hl,hl
	ld de,ti.mpLcdPalette
	add hl,de
	ld e,(hl)
	inc hl
	ld d,(hl)
	ld a,e
	add a,a
	add a,a
	add a,a
	ld c,a
	call spiParam
	ex de,hl
	push hl
	add hl,hl
	add hl,hl
	add hl,hl
	ld a,h
	add a,a
	add a,a
	add a,a
	pop de
	bit 7,d
	jr z,$+4
	or a,4
	ld c,a
	call spiParam
	ld a,d
	add a,a
	and a,$F8
	ld c,a
	call spiParam
	pop hl
	dec hl
	ld a,h
	or a,l
	jr nz,.pixel
	jr spi_game_window
.rows: db 8

; the window the LCD writes (spiInitVSync, spiInitVSyncWide), after panel_put
; destroys af, bc
spi_game_window:
	ld a,(_wide_buffer+2)
	or a,a
	jr nz,.wide
.normal:
	spi $2A,0,40,$01,$27
	jr .rows
.wide:
	spi $2A,0,0,$01,$3F
.rows:
	spi $2B,0,8,0,231
	ret

; return SPI to RGB interface
spiEndVSync:
	spi $33,0,0,$01,$40,0,0			; no hardware scrolling
	spi $37,0,0
	ld a,$FF
	ld (spi_scroll_sent),a			; unknown: sent again next time
	spi $13							; disable partial mode
	spi $C6,$0F
	spi $B2,$0C,$0C,0				; set back porch ( (120+32)*1814.4 ~= 275000 cycles window for DMA ) 
	spi $2A,0,0,$01,$3F				; reset memory access window
	spi $2B,0,0,0,239
	spi $30,0,0,$01,$3F				; reset partial area
	spi $B0,$11						; enable RGB interface 
	ret 
	
; disables sending data to SPI via LCD Controller(lock screen)
_spiLock:
spiLock: 
	ld a,1 
	ld (spiUnlock.locked),a 
	spi $B0,$02 					; disable RGB interface 
	ret 
	
; reenables LCD controller sending 
; Run during Front Porch to avoid visual artifacts
_spiUnlock:
spiUnlock:
	ld a,0 
.locked := $ - 1
	or a,a 
	ret z
	xor a,a 
	ld (.locked),a
	push bc 
	push hl  
	spi $B0,$12 
	; the frame in the frame buffer goes at the next transfer: its scroll too
	ld a,(scroll_want)
	call spiScroll
	; from now on, the LCD's "transfer done" flag means this frame was sent
	ld hl,ti.mpLcdIcr
	set 3,(hl)
	pop hl 
	pop bc
	ret 

	
section .bss
public spi_scroll_sent
public _spi_scroll_sent
public scroll_want
_spi_scroll_sent:
spi_scroll_sent: rb 1	; the scroll last sent to the panel

section .data
private panel_window
; panel_put's commands: columns, rows (big endian), memory write
panel_window:
	db 4,$2A,0,0,0,0
	db 4,$2B,0,0,0,0
	db 0,$2C
scroll_want: rb 1	; the scroll of the frame in the frame buffer (pacing.asm)

extern _wide_buffer
extern _show_fps
