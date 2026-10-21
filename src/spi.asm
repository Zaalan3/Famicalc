include 'ti84pceg.inc'

section .text

macro spi cmd, params&
	ld	c, cmd
	call	spiCmd
	match any, params
		iterate param, any
			ld	c, param
			call	spiParam
		end iterate
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
	
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

public spiSetup
public spiInitVSync
public spiInitVSyncWide
public spiEndVSync

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
	spi $2A,0,32,$01,$1F			; sets x memory access to range [32,287] (256 pixels)
	spi $2B,0,8,0,231				; sets y memory access to range [8,231] (224 pixels)
	spi $30,0,40,$01,$17			; set partial area to middle 240 pixels 
	spi $B5,$80						; non-display is ($B5,$00 for white ; $B5,$80 for black)
	spi $B0,$12						; enable VSync Interface
	spi $12							; enable partial mode
	; hardware scrolling across the game's 256 columns (32-287): the panel
	; shows frame buffer column x + S at column x (spiScroll sets S)
	spi $33,0,32,$01,$00,0,32		; fixed 32, scrolling 256, fixed 32
	xor a,a
	jp spiScroll.send
	
; the same for the wide picture: a 320x224 window; columns 10-309 shown
; (the NES's 8-247, like the normal picture)
spiInitVSyncWide:
	spi $C6,$08
	spi $B2,120,0,0
	spi $2A,0,0,$01,$3F				; x range [0,319]
	spi $2B,0,8,0,231				; y range [8,231]
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
	ld bc,32		; the scrolling area starts at 32 (normal picture)
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
public scroll_want
spi_scroll_sent: rb 1	; the scroll last sent to the panel
scroll_want: rb 1	; the scroll of the frame in the frame buffer (pacing.asm)

extern _wide_buffer
