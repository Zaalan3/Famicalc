include 'ti84pceg.inc' 

section .text 

public _asm_init
public _asm_cleanup

public _lcd_lock
public _lcd_unlock

_asm_init: 
	di 
	or a,a 
	sbc hl,hl 
	ld (ti.mpIntMask),hl 	; disable all interrupts
	xor a,a 
	ld (ti.usbInited),a		; clear usb stack
	
	ld hl,ti.mpLcdCtrl
	ld (hl),$27		; 8bpp 
	inc hl 
	ld (hl),$39 	; vcomp interrupt on front porch 
	
	; set LCD timing 
	ld hl,ti.mpLcdTiming0 
	ld de,lcdTimingBackup 
	ld bc,12
	ldir
	
	ld hl,lcdTiming 
	ld de,ti.mpLcdTiming0 
	ld c,12
	ldir
	
	call spiInitVSync

	ret

_asm_cleanup:
	ld a,$2D
	ld (ti.mpLcdCtrl),a 
	ld de,ti.mpLcdTiming0 
	ld hl,lcdTimingBackup 
	ld bc,12
	ldir
	call spiEndVSync
	ret 

; disables updating screen
_lcd_lock:
	call lcd_wait
	jp spiLock

; enables updating screen
_lcd_unlock: 
	call lcd_wait 
	jp spiUnlock

lcd_wait: 
	ld hl,ti.mpLcdRis	; wait until front porch reached
.wait: 
	bit ti.bLcdIntVcomp,(hl)
	jr z,.wait 
	ld l,ti.lcdIcr		; acknowledge interrupt 
	ld (hl),ti.lcdIntVcomp
	ret
	
; Timing = (HSW+1+HFP+1+HBP+1 + 16*(PPL+1)) * (LPP+1+VSW+1+VFP+VBP) * (PCD+2) * 2
; 256x244 -> 1024x56
; 801060 cc or ~59.92 FPS 
; ~235000 cc spent sending data
lcdTiming: 
	db	63 shl 2 	; PPL 
	db	0 			; HSW
	db	0 			; HFP 
	db	0 			; HBP 
	dw	55 			; LPP & VSW(0) 
	db	138			; VFP
	db	0 			; VBP
	dw  $7800		; PCD = 0
	dw	1023		; CPL
	
section .data
private lcdTimingBackup
lcdTimingBackup: 
	rb 12

extern spiInitVSync
extern spiEndVSync
extern spiLock
extern spiUnlock
