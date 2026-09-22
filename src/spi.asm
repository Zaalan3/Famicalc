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
public spiEndVSync

public spiLock 
public spiUnlock

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
	ret 
	
; return SPI to RGB interface
spiEndVSync:
	spi $2A,0,0,$01,$3F				; reset memory access window
	spi $2B,0,0,0,239
	spi $30,0,0,$01,$3F				; reset partial area
	spi $13							; disable partial mode
	spi $B0,$11						; enable RGB interface 
	ret 
	
; disables sending data to SPI via LCD Controller(lock screen)
spiLock: 
	ld a,1 
	ld (spiUnlock.locked),a 
	spi $B0,$02 					; disable RGB interface 
	ret 
	
; reenables LCD controller sending 
; Run during Front Porch to avoid visual artifacts
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
	pop hl 
	pop bc
	ret 

	