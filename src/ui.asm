include 'ti84pceg.inc' 

section .text 

public _ui_init
public _ui_cleanup
public _ui_printString
	
_ui_init: 
	call render_init
	; set color 1 to white
	ld hl,ti.mpLcdPalette + 2 
	ld (hl),$FF 
	inc hl 
	ld (hl),$FF 
	ret 
	
_ui_cleanup: 
	call render_cleanup
	; clear pixelShadow to prevent trash on homescreen
	ld hl,ti.pixelShadow
	ld de,ti.pixelShadow+1 
	ld bc,8400*2
	ld (hl),0
	ldir
	ret

;ui_printString(uint8_t x,uint8_t y,char* string);
_ui_printString: 
	ld iy,3 
	add iy,sp 
	ld de,$D40000
	ld a,(iy+3) 	; load x and y 
	add a,$20
	ret c 
	ld d,a
	ld a,(iy+0)
	; wrap to start x if out of room on this line
	ld (.smc_wrap),a 
	ld e,a
	push de
	ld de,(iy+6) 	; de = string ptr
	pop iy			; iy = screen ptr
.loop: 
	ld a,(de) 
	or a,a 
	ret z 
	inc de
; fetch character 
	ld b,8 
	ld c,a 
	mlt bc 
	ld hl,font 
	add hl,bc 
	ld c,8
.outer: 
	ld a,(hl)
	inc hl
	ld b,8
.inner: 	
	rla 
	jr c,.one 
	ld (iy),0
	inc iyl
	djnz .inner 
.rep:
	lea iy,iy-8
	inc iyh
	dec c 
	jr nz,.outer
	ld a,iyl
	cp a,$F0 
	jr c,.nowrap 
.wrap:	
	ld iyl,8 	; Line wrap
.smc_wrap := $-1
	jr .loop 
.nowrap: 
	ld a,iyh 
	sub a,8 
	ld iyh,a 
	lea iy,iy+8
	jr .loop
.one: 
	ld (iy),1
.smc_color := $-1
	inc iyl
	djnz .inner 
	jr .rep

	
section .data

private font 

font: 
	; curtesy of Graphx
	db	$00,$00,$00,$00,$00,$00,$00,$00 ;  
	db	$7E,$81,$A5,$81,$BD,$BD,$81,$7E ; ☺
	db	$7E,$FF,$DB,$FF,$C3,$C3,$FF,$7E ; ☻
	db	$6C,$FE,$FE,$FE,$7C,$38,$10,$00 ; ♥
	db	$10,$38,$7C,$FE,$7C,$38,$10,$00 ; ♦
	db	$38,$7C,$38,$FE,$FE,$10,$10,$7C ; ♣
	db	$00,$18,$3C,$7E,$FF,$7E,$18,$7E ; ♠
	db	$00,$00,$18,$3C,$3C,$18,$00,$00 ; •
	db	$FF,$FF,$E7,$C3,$C3,$E7,$FF,$FF ; ◘
	db	$00,$3C,$66,$42,$42,$66,$3C,$00 ; ○
	db	$FF,$C3,$99,$BD,$BD,$99,$C3,$FF ; ◙
	db	$0F,$07,$0F,$7D,$CC,$CC,$CC,$78 ; ♂
	db	$3C,$66,$66,$66,$3C,$18,$7E,$18 ; ♀
	db	$3F,$33,$3F,$30,$30,$70,$F0,$E0 ; ♪
	db	$7F,$63,$7F,$63,$63,$67,$E6,$C0 ; ♫
	db	$99,$5A,$3C,$E7,$E7,$3C,$5A,$99 ; *
	db	$80,$E0,$F8,$FE,$F8,$E0,$80,$00 ; ►
	db	$02,$0E,$3E,$FE,$3E,$0E,$02,$00 ; ◄
	db	$18,$3C,$7E,$18,$18,$7E,$3C,$18 ; ↕
	db	$66,$66,$66,$66,$66,$00,$66,$00 ; ‼
	db	$7F,$DB,$DB,$7B,$1B,$1B,$1B,$00 ; ¶
	db	$3F,$60,$7C,$66,$66,$3E,$06,$FC ; §
	db	$00,$00,$00,$00,$7E,$7E,$7E,$00 ; ▬
	db	$18,$3C,$7E,$18,$7E,$3C,$18,$FF ; ↨
	db	$18,$3C,$7E,$18,$18,$18,$18,$00 ; ↑
	db	$18,$18,$18,$18,$7E,$3C,$18,$00 ; ↓
	db	$00,$18,$0C,$FE,$0C,$18,$00,$00 ; →
	db	$00,$30,$60,$FE,$60,$30,$00,$00 ; ←
	db	$00,$00,$C0,$C0,$C0,$FE,$00,$00 ; └
	db	$00,$24,$66,$FF,$66,$24,$00,$00 ; ↔
	db	$00,$18,$3C,$7E,$FF,$FF,$00,$00 ; ▲
	db	$00,$FF,$FF,$7E,$3C,$18,$00,$00 ; ▼
	db	$00,$00,$00,$00,$00,$00,$00,$00 ;
	db	$C0,$C0,$C0,$C0,$C0,$00,$C0,$00 ; !
	db	$D8,$D8,$D8,$00,$00,$00,$00,$00 ; "
	db	$6C,$6C,$FE,$6C,$FE,$6C,$6C,$00 ; #
	db	$18,$7E,$C0,$7C,$06,$FC,$18,$00 ; $
	db	$00,$C6,$CC,$18,$30,$66,$C6,$00 ; %
	db	$38,$6C,$38,$76,$DC,$CC,$76,$00 ; &
	db	$30,$30,$60,$00,$00,$00,$00,$00 ; '
	db	$30,$60,$C0,$C0,$C0,$60,$30,$00 ; (
	db	$C0,$60,$30,$30,$30,$60,$C0,$00 ; )
	db	$00,$66,$3C,$FF,$3C,$66,$00,$00 ; *
	db	$00,$30,$30,$FC,$FC,$30,$30,$00 ; +
	db	$00,$00,$00,$00,$00,$60,$60,$C0 ; ,
	db	$00,$00,$00,$FC,$00,$00,$00,$00 ; -
	db	$00,$00,$00,$00,$00,$C0,$C0,$00 ; .
	db	$06,$0C,$18,$30,$60,$C0,$80,$00 ; /
	db	$7C,$CE,$DE,$F6,$E6,$C6,$7C,$00 ; 0
	db	$30,$70,$30,$30,$30,$30,$FC,$00 ; 1
	db	$7C,$C6,$06,$7C,$C0,$C0,$FE,$00 ; 2
	db	$FC,$06,$06,$3C,$06,$06,$FC,$00 ; 3
	db	$0C,$CC,$CC,$CC,$FE,$0C,$0C,$00 ; 4
	db	$FE,$C0,$FC,$06,$06,$C6,$7C,$00 ; 5
	db	$7C,$C0,$C0,$FC,$C6,$C6,$7C,$00 ; 6
	db	$FE,$06,$06,$0C,$18,$30,$30,$00 ; 7
	db	$7C,$C6,$C6,$7C,$C6,$C6,$7C,$00 ; 8
	db	$7C,$C6,$C6,$7E,$06,$06,$7C,$00 ; 9
	db	$00,$C0,$C0,$00,$00,$C0,$C0,$00 ; :
	db	$00,$60,$60,$00,$00,$60,$60,$C0 ; ;
	db	$18,$30,$60,$C0,$60,$30,$18,$00 ; <
	db	$00,$00,$FC,$00,$FC,$00,$00,$00 ; =
	db	$C0,$60,$30,$18,$30,$60,$C0,$00 ; >
	db	$78,$CC,$18,$30,$30,$00,$30,$00 ; ?
	db	$7C,$C6,$DE,$DE,$DE,$C0,$7E,$00 ; @
	db	$38,$6C,$C6,$C6,$FE,$C6,$C6,$00 ; A
	db	$FC,$C6,$C6,$FC,$C6,$C6,$FC,$00 ; B
	db	$7C,$C6,$C0,$C0,$C0,$C6,$7C,$00 ; C
	db	$F8,$CC,$C6,$C6,$C6,$CC,$F8,$00 ; D
	db	$FE,$C0,$C0,$F8,$C0,$C0,$FE,$00 ; E
	db	$FE,$C0,$C0,$F8,$C0,$C0,$C0,$00 ; F
	db	$7C,$C6,$C0,$C0,$CE,$C6,$7C,$00 ; G
	db	$C6,$C6,$C6,$FE,$C6,$C6,$C6,$00 ; H
	db	$7E,$18,$18,$18,$18,$18,$7E,$00 ; I
	db	$06,$06,$06,$06,$06,$C6,$7C,$00 ; J
	db	$C6,$CC,$D8,$F0,$D8,$CC,$C6,$00 ; K
	db	$C0,$C0,$C0,$C0,$C0,$C0,$FE,$00 ; L
	db	$C6,$EE,$FE,$FE,$D6,$C6,$C6,$00 ; M
	db	$C6,$E6,$F6,$DE,$CE,$C6,$C6,$00 ; N
	db	$7C,$C6,$C6,$C6,$C6,$C6,$7C,$00 ; O
	db	$FC,$C6,$C6,$FC,$C0,$C0,$C0,$00 ; P
	db	$7C,$C6,$C6,$C6,$D6,$DE,$7C,$06 ; Q
	db	$FC,$C6,$C6,$FC,$D8,$CC,$C6,$00 ; R
	db	$7C,$C6,$C0,$7C,$06,$C6,$7C,$00 ; S
	db	$FF,$18,$18,$18,$18,$18,$18,$00 ; T
	db	$C6,$C6,$C6,$C6,$C6,$C6,$FE,$00 ; U
	db	$C6,$C6,$C6,$C6,$C6,$7C,$38,$00 ; V
	db	$C6,$C6,$C6,$C6,$D6,$FE,$6C,$00 ; W
	db	$C6,$C6,$6C,$38,$6C,$C6,$C6,$00 ; X
	db	$C6,$C6,$C6,$7C,$18,$30,$E0,$00 ; Y
	db	$FE,$06,$0C,$18,$30,$60,$FE,$00 ; Z
	db	$F0,$C0,$C0,$C0,$C0,$C0,$F0,$00 ; [
	db	$C0,$60,$30,$18,$0C,$06,$02,$00 ; \
	db	$F0,$30,$30,$30,$30,$30,$F0,$00 ; ]
	db	$10,$38,$6C,$C6,$00,$00,$00,$00 ; ^
	db	$00,$00,$00,$00,$00,$00,$00,$FF ; _
	db	$C0,$C0,$60,$00,$00,$00,$00,$00 ; `
	db	$00,$00,$7C,$06,$7E,$C6,$7E,$00 ; a
	db	$C0,$C0,$C0,$FC,$C6,$C6,$FC,$00 ; b
	db	$00,$00,$7C,$C6,$C0,$C6,$7C,$00 ; c
	db	$06,$06,$06,$7E,$C6,$C6,$7E,$00 ; d
	db	$00,$00,$7C,$C6,$FE,$C0,$7C,$00 ; e
	db	$1C,$36,$30,$78,$30,$30,$78,$00 ; f
	db	$00,$00,$7E,$C6,$C6,$7E,$06,$FC ; g
	db	$C0,$C0,$FC,$C6,$C6,$C6,$C6,$00 ; h
	db	$60,$00,$E0,$60,$60,$60,$F0,$00 ; i
	db	$06,$00,$06,$06,$06,$06,$C6,$7C ; j
	db	$C0,$C0,$CC,$D8,$F8,$CC,$C6,$00 ; k
	db	$E0,$60,$60,$60,$60,$60,$F0,$00 ; l
	db	$00,$00,$CC,$FE,$FE,$D6,$D6,$00 ; m
	db	$00,$00,$FC,$C6,$C6,$C6,$C6,$00 ; n
	db	$00,$00,$7C,$C6,$C6,$C6,$7C,$00 ; o
	db	$00,$00,$FC,$C6,$C6,$FC,$C0,$C0 ; p
	db	$00,$00,$7E,$C6,$C6,$7E,$06,$06 ; q
	db	$00,$00,$FC,$C6,$C0,$C0,$C0,$00 ; r
	db	$00,$00,$7E,$C0,$7C,$06,$FC,$00 ; s
	db	$30,$30,$FC,$30,$30,$30,$1C,$00 ; t
	db	$00,$00,$C6,$C6,$C6,$C6,$7E,$00 ; u
	db	$00,$00,$C6,$C6,$C6,$7C,$38,$00 ; v
	db	$00,$00,$C6,$C6,$D6,$FE,$6C,$00 ; w
	db	$00,$00,$C6,$6C,$38,$6C,$C6,$00 ; x
	db	$00,$00,$C6,$C6,$C6,$7E,$06,$FC ; y
	db	$00,$00,$FE,$0C,$38,$60,$FE,$00 ; z
	db	$1C,$30,$30,$E0,$30,$30,$1C,$00 ; {
	db	$C0,$C0,$C0,$00,$C0,$C0,$C0,$00 ; |
	db	$E0,$30,$30,$1C,$30,$30,$E0,$00 ; }
	db	$76,$DC,$00,$00,$00,$00,$00,$00 ; ~
	db	$00,$10,$38,$6C,$C6,$C6,$FE,$00 ; △

	
extern render_init 
extern render_cleanup