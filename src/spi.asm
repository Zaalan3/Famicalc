include 'ti84pceg.inc'

section .text

;   DEFINES

pSpiRange    = 0D000h
mpSpiRange   = 0F80000h
spiValid     = 8
pSpiValid    = pSpiRange + spiValid
mpSpiValid   = mpSpiRange + spiValid
spiStatus    = 12
pSpiStatus   = pSpiRange + spiStatus
mpSpiStatus  = mpSpiRange + spiStatus
spiData      = 24
pSpiData     = pSpiRange + spiData
mpSpiData    = mpSpiRange + spiData

lcd          = ti.vRam + 8
lcd.width    = ti.lcdWidth
lcd.height   = ti.lcdHeight
lcd.size     = lcd.width * lcd.height

macro spi cmd, params&
	ld	a, cmd
	call	spiCmd
	match any, params
		iterate param, any
			ld	a, param
			call	spiParam
		end iterate
	end match
end macro

;;   END DEFINES

spiParam:
	scf
	virtual
		jr	nc, $
		load .jr_nc : byte from $$
	end virtual
	db	.jr_nc
	
spiCmd:
	or	a, a
	ld	hl, mpSpiData or spiValid shl 8
	ld	b, 3
.loop:	rla
	rla
	rla
	ld	(hl), a
	djnz	.loop
	ld	l, h
	ld	(hl), 1
.wait:	ld	l, spiStatus + 1
.wait1:	ld	a, (hl)
	and	a, $f0
	jr	nz, .wait1
	dec	l
.wait2:	bit	2, (hl)
	jr	nz, .wait2
	ld	l, h
	ld	(hl), a
	ret
	
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

public spiInitVSync
public spiEndVSync

public spiLock 
public spiUnlock

public spiHalfRes 
public spiFullRes

; changes refresh method to VSYNC timing to eliminate tearing
; makes display a 256x224 window (with only 240 pixels displayed)
spiInitVSync: 
	spi $C6,$08						; set scan speed ( around 1814.4 cycles per scanline )
	spi $B2,98,1,0					; set back porch ( (98+32)*1814.4 cycles before DMA needs to finish ) 
	spi $2A,0,32,$01,$1F			; sets x memory access to range [32,287] (256 pixels)
	spi $2B,0,8,0,231				; sets y memory access to range [8,231] (224 pixels)
	spi $30,0,40,$01,$17			; set partial area to middle 240 pixels 
	spi $B5,$80						; non-display is black
	spi $12							; enable partial mode
	spi $B0,$12						; enable VSync Interface
	ret 
	
; return SPI to RGB interface
spiEndVSync:
	spi $2A,0,0,$01,$3F			
	spi $2B,0,0,0,239
	spi $13
	spi $B0,$11
	ret 
	
; disables sending data to SPI via LCD Controller(lock screen)
spiLock: 					
	spi $B0,$02 					; disable RGB interface 
	ret 
	
; reenables LCD controller sending 
; Run during Front Porch to avoid visual artifacts
spiUnlock:
	spi $B0,$12 
	ret 

	