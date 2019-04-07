;*************************************************************
;*                     gelly vectors                         *
;*     coded somewhre in 1992, revised on 7 april 2019       *
;*************************************************************

; MIT License

; Copyright (c) 2019 Krzysztof Kielak

; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:

; The above copyright notice and this permission notice shall be included in all
; copies or substantial portions of the Software.

; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.

planesize=40*256

start:
	move.l	#begin,$80.w
	trap	#0
	move.l	#0,d0
	rts

;*********************************************************
;*                   startup code                        *
;*********************************************************
begin:
	lea	$dff000,a6
	move.w	$1c(a6),wart1
	move.w	$1e(a6),wart2
	move.w	$02(a6),wart3
	or.w	#$c000,wart1
	or.w	#$8000,wart2
	or.w	#$8000,wart3
	move.w	#$7fff,$9a(a6)
	move.w	#$7fff,$9c(a6)
	move.w	#$7fff,$96(a6)

	bsr.w	screen_swap
	bsr.w	init

	move.l	#copper,$80(a6)
	tst.w	$88(a6)
	move.l	$6c.w,old
	move.l	#inter,$6c.w
	move.w	#$c020,$9a(a6)
	move.w	#%1000001111000000,$96(a6)

	bsr.w	main_loop
	
	bsr.w	wblit
	move.w	#$7fff,$9a(a6)
	move.w	#$7fff,$9c(a6)
	move.w	#$7fff,$96(a6)
	move.l	old(pc),$6c.w
	move.l	4.w,a6
	move.l	#name,a1
	clr.l	d0
	jsr	-408(a6)
	lea	$dff000,a6
	move.l	d0,a5
	move.l	38(a5),$80(a6)
	tst.w	$88(a6)
	move.w	wart1(pc),$9a(a6)
	move.w	wart2(pc),$9c(a6)
	move.w	wart3(pc),$96(a6)
	rte

;*********************************************************
;*                   interrupt routine                   *
;*********************************************************
inter:
	movem.l	d0-d7/a0-a6,-(sp)
	lea	$dff000,a6
	and.w	#$20,$1e(a6)
	beq.s	out
	move.w	#$20,$9c(a6)

out:
	movem.l	(sp)+,d0-d7/a0-a6
	rte

;*********************************************************
;*                      main loop                        *
;*********************************************************
main_loop:
	stop	#$2000

	bsr.w	cls
	bsr.w	rotate
	bsr.w	show
	bsr.w	fill
	bsr.w	screen_swap
	move.w	#0,$dff180
	
	move.l	wsk_sin1(pc),a0
	cmp.w	#-1,(a0)
	bne.s	.dal
	lea	sin1(pc),a0
.dal	move.w	(a0)+,d0
	move.l	a0,wsk_sin1
	move.w	d0,zm1
	move.w	d0,zm3
	neg.w	d0
	move.w	d0,zm2
	move.w	d0,zm4

	move.l	wsk_sin2(pc),a0
	cmp.w	#-1,(a0)
	bne.s	.dal2
	lea	sin2(pc),a0
.dal2	move.w	(a0)+,d0
	move.l	a0,wsk_sin2
	neg.w	d0
	move.w	d0,ch1+2
	move.w	d0,ch3+2
	move.w	d0,ch2+2
	move.w	d0,ch4+2

	move.l	wsk_sin3(pc),a0
	cmp.w	#-1,(a0)
	bne.s	.dal3
	lea	sin3(pc),a0
.dal3	move.w	(a0)+,d0
	move.l	a0,wsk_sin3
	add.w	d0,d0
	move.w	d0,beta

	btst	#6,$bfe001
	bne.w	main_loop
	rts

;*********************************************************
;*                        screen swap                    *
;*********************************************************
screen_swap:
	move.l	clsadr(pc),d0
	move.l	planeadr(pc),clsadr
	move.l	d0,planeadr
	lea	planes(pc),a0
	moveq	#3,d1
loop:
	move.w	d0,6(a0)
	swap	d0
	move.w	d0,2(a0)
	swap	d0
	add.l	#8,a0
	add.l	#40,d0
	dbf	d1,loop
	rts

;*********************************************************
;*              wait for blitter ready                   *
;*********************************************************
wblit:
	btst	#6,2(a6)
	bne.s	wblit
	rts

;*********************************************************
;*                   plot a point                        *
;*********************************************************
;d0 - x
;d1 - y
plot:
	movem.l	d0-d1/a0,-(sp)
	move.l	planeadr(pc),a0
	mulu	#160,d1
	lea	(a0,d1.w),a0
	move.w	d0,d1
	and.w	#$f,d0
	lsr.w	#3,d1
	lea	(a0,d1.w),a0
	not.w	d0
	bset	d0,(a0)
	movem.l	(sp)+,d0-d1/a0
	rts

;*********************************************************
;*             init bezier curve table                   *
;*********************************************************
make_bezjer:
	movem.l	d0-d5,-(sp)
	addq.w	#1,licz
	cmp.w	#4,licz
	bne.s	.dal1
	move.w	d4,(a1)+
	move.w	d5,(a1)+
	bra.s	.exit
.dal1:	movem.l	d0-d5,-(sp)
	add.w	d2,d4		; first half
	add.w	d3,d5
	add.w	d0,d2
	add.w	d1,d3
	lsr.w	#1,d2
	lsr.w	#1,d3
	lsr.w	#1,d4
	lsr.w	#1,d5
	add.w	d2,d4
	add.w	d3,d5
	lsr.w	#1,d4
	lsr.w	#1,d5
	bsr.w	make_bezjer
	movem.l	(sp)+,d0-d5
	add.w	d2,d0		;second half
	add.w	d3,d1
	add.w	d4,d2
	add.w	d5,d3
	lsr.w	#1,d2
	lsr.w	#1,d3
	lsr.w	#1,d0
	lsr.w	#1,d1
	add.w	d2,d0
	add.w	d3,d1
	lsr.w	#1,d0
	lsr.w	#1,d1
	bsr.w	make_bezjer
.exit:
	subq.w	#1,licz
	movem.l	(sp)+,d0-d5
	rts

;*********************************************************
;*                   draw bezier curve                   *
;*********************************************************
draw_bezier_curve:
	movem.l	a0-a1,-(sp)
	lea	tablica(pc),a1
	move.w	#0,licz
	move.w	d0,(a1)+
	move.w	d1,(a1)+
	bsr.w	make_bezjer
	move.w	d4,(a1)+
	move.w	d5,(a1)+
	lea	tablica(pc),a0
	movem.w	(a0)+,d0-d1
	movem.w	(a0),d2-d3
	bsr.w	draw	
	movem.w	(a0)+,d0-d1
	movem.w	(a0),d2-d3
	bsr.w	draw	
	movem.w	(a0)+,d0-d1
	movem.w	(a0),d2-d3
	bsr.w	draw	
	movem.w	(a0)+,d0-d1
	movem.w	(a0),d2-d3
	bsr.w	draw	
	movem.w	(a0)+,d0-d1
	movem.w	(a0),d2-d3
	bsr.w	draw	
	movem.w	(a0)+,d0-d1
	movem.w	(a0),d2-d3
	bsr.w	draw	
	movem.w	(a0)+,d0-d1
	movem.w	(a0),d2-d3
	bsr.w	draw	
	movem.w	(a0)+,d0-d1
	movem.w	(a0),d2-d3
	bsr.w	draw	
	movem.w	(a0)+,d0-d1
	movem.w	(a0),d2-d3
	bsr.w	draw	
	movem.l	(sp)+,a0-a1
	rts

;*********************************************************
;*	 	              draw object                        *
;*********************************************************
show:
	lea	$dff000,a6
	lea	pomoc1(pc),a4
	lea	$dff000,a6
	bsr.w	wblit
	move.l	#-1,$44(a6)
	move.l	#$ffff8000,$72(a6)
	move.w	#120,$60(a6)
	move.w	#120,$66(a6)
	lea	line(pc),a0
	lea	punkty2d(pc),a1
sh_1:
	move.w	(a0)+,modula
	cmp.w	#$7fff,modula
	beq.w	exit_p

	movem.w	(a0)+,d0-d2
	movem.w	(a1,d2.w),d4-d5
	movem.w	(a1,d1.w),d2-d3
	movem.w	(a1,d0.w),d0-d1
	sub.w	d2,d4
	sub.w	d3,d5
	sub.w	d0,d2
	sub.w	d1,d3
	muls	d2,d5
	muls	d3,d4
	sub.l	d5,d4
;	ble.s	visible
visible:	
	move.w	(a0)+,d0
	tst.w	d0
	bne.s	b_b
	movem.w	(a0)+,d0-d1
	movem.w	(a1,d1.w),d2-d3
	movem.w	(a1,d0.w),d0-d1
	bsr.w	draw
	bra.s	next1
b_b:	movem.w	(a0)+,d0-d2
	movem.w	(a1,d2.w),d4-d5
	movem.w	(a1,d1.w),d2-d3
	movem.w	(a1,d0.w),d0-d1
	bsr.w	draw_bezier_curve
next1:
	move.w	(a0)+,d0
	tst.w	d0
	bne.s	b_b2
	movem.w	(a0)+,d0-d1
	movem.w	(a1,d1.w),d2-d3
	movem.w	(a1,d0.w),d0-d1
	bsr.w	draw
	bra.s	next2
b_b2:	movem.w	(a0)+,d0-d2
	movem.w	(a1,d2.w),d4-d5
	movem.w	(a1,d1.w),d2-d3
	movem.w	(a1,d0.w),d0-d1
	bsr.w	draw_bezier_curve
next2:
	move.w	(a0)+,d0
	tst.w	d0
	bne.s	b_b3
	movem.w	(a0)+,d0-d1
	movem.w	(a1,d1.w),d2-d3
	movem.w	(a1,d0.w),d0-d1
	bsr.w	draw
	bra.s	next3
b_b3:	movem.w	(a0)+,d0-d2
	movem.w	(a1,d2.w),d4-d5
	movem.w	(a1,d1.w),d2-d3
	movem.w	(a1,d0.w),d0-d1
	bsr.w	draw_bezier_curve
next3:
	move.w	(a0)+,d0
	tst.w	d0
	bne.s	b_b4
	movem.w	(a0)+,d0-d1
	movem.w	(a1,d1.w),d2-d3
	movem.w	(a1,d0.w),d0-d1
	bsr.w	draw
	bra.s	next4
b_b4:	movem.w	(a0)+,d0-d2
	movem.w	(a1,d2.w),d4-d5
	movem.w	(a1,d1.w),d2-d3
	movem.w	(a1,d0.w),d0-d1
	bsr.w	draw_bezier_curve
next4:
	bra.w	sh_1
no_vis:
	addq.l	#6,a0
	bra.w	sh_1

;*********************************************************
;*	 	              draw a line                        *
;*********************************************************
;d0,d1-x,y poczatku linii
;d2,d3-x,y konca linii
draw:
	move.l	clsadr(pc),a2
	add.w	modula(pc),a2
	moveq	#15,d4
	cmp.w	d1,d3
	beq.w	exit
	ble.s	moon
	exg	d0,d2
	exg	d1,d3
moon:	and.w	d2,d4
	move.w	d4,d5
	not.b	d5
	sub.w	d3,d1
	ext.l	d3
	add.w	d3,d3
	move.w	(a4,d3.w),d3
	sub.w	d2,d0
	blt.s	tron
	cmp.w	d0,d1
	bge.s	prince
	moveq	#$11,d7
	bra.s	speedy
prince:	moveq	#1,d7
	exg	d1,d0
	bra.s	speedy
tron:	neg.w	d0
	cmp.w	d0,d1
	bge.s	only
	moveq	#$15,d7
	bra.s	speedy
only:	moveq	#$9,d7
	exg	d1,d0
speedy: add.w   d1,d1
	lsr.w	#3,d2
	ext.l	d2
	add.w	d2,d3
	add.w	d3,a2
	move.w	d1,d2
	swap	d4
	lsr.l	#4,d4
	or.w	#$0b6a,d4
	swap	d7
	move.w	d4,d7
	swap	d7
	move.w	d0,d6
	addq.w	#1,d6
	lsl.w	#6,d6
	addq.w	#2,d6
	sub.w	d0,d2
	or.w	#2,d7
	bge.s	wblit2
	ori.b	#$40,d7
wblit2:	btst	#14,$2(a6)
	bne.s	wblit2
	move.w	d1,$62(a6)	;d1=2*sdelta do bltbmod
	move.w	d2,d1		;d2=2*sdelta do d1
	sub.w	d0,d1		;d1-d0=2*sdelta-2*ldelta
	move.w	d1,$64(a6)	;d1=2*sdelta-2*ldelta=bltamod
	move.l	d7,$40(a6)
	move.l	a2,$48(a6)
	move.l	a2,$54(a6)
	move.w	d2,$52(a6)
	bchg	d5,(a2)
	move.w	d6,$58(a6)
exit:	rts
	
init:	lea	pomoc1(pc),a0
	moveq	#0,d0
	move.w	#120,d1
	move.l	#250,d2
abba2:	move.w	d0,(a0)+
	add.w	d1,d0
	dbf	d2,abba2
	rts

pomoc1:		blk.w	300,0
tablica:	blk.w	30,0

;*************************************************************
;*                     3d-rotations                          *
;*               coded by dr.df0 of .. atd ..                *
;*                   on  4 july 1992                         *
;*************************************************************
rotate:
	lea	sinus(pc),a0
	lea	cosin(pc),a1
	move.w	beta(pc),d7
	move.w	(a0,d7.w),d6
	move.w	(a1,d7.w),d7
	lea	wektor(pc),a2
	lea	punkty2d(pc),a4

rloop:
	movem.w	(a2)+,d0-d2
	cmp.w	#$7fff,d0	
	beq.w	exit_p

;around x
;	move.w	alfa(pc),d7
;	move.w	(a0,d7.w),d6
;	move.w	(a1,d7.w),d7
;	move.w	d2,d3
;	muls	d6,d3
;	muls	d7,d2
;	move.w	d1,d4
;	muls	d6,d4
;	muls	d7,d1
;	add.l	d1,d1
;	swap	d1
;	add.l	d2,d2
;	swap	d2
;	add.l	d3,d3
;	swap	d3
;	add.l	d4,d4
;	swap	d4
;	add.w	d4,d2
;	sub.w	d3,d1	

;around y	
	move.w	d2,d3
	muls	d6,d3
	muls	d7,d2
	move.w	d0,d4
	muls	d6,d4
	muls	d7,d0
	add.l	d0,d0
	swap	d0
	add.l	d2,d2
	swap	d2
	add.l	d3,d3
	swap	d3
	add.l	d4,d4
	swap	d4
	add.w	d3,d0
	sub.w	d4,d2	

;around z
;	move.w	gama(pc),d7
;	move.w	(a0,d7.w),d6
;	move.w	(a1,d7.w),d7
;	move.w	d0,d3
;	muls	d6,d3
;	muls	d7,d0
;	move.w	d1,d4
;	muls	d6,d4
;	muls	d7,d1
;	add.l	d0,d0
;	swap	d0
;	add.l	d1,d1
;	swap	d1
;	add.l	d3,d3
;	swap	d3
;	add.l	d4,d4
;	swap	d4
;	add.w	d3,d1
;	sub.w	d4,d0


	add.w	#22000,d2
	ext.l	d1
	ext.l	d0
	lsl.l	#8,d0
	lsl.l	#8,d1
	divs	d2,d0
	divs	d2,d1
	add.w	#160,d0
	add.w	#128,d1

	move.w	d0,(a4)+
	move.w	d1,(a4)+

	bra rloop
exit_p:
	rts

beta:	dc.w	90
;     sinus and cosinus tables

sinus:
	dc.w	$0000,$0242,$0484,$06c6,$0907,$0b48,$0d87,$0fc6
	dc.w	$1203,$143f,$1679,$18b2,$1ae8,$1d1d,$1f4f,$217e
	dc.w	$23ab,$25d5,$27fc,$2a20,$2c41,$2e5d,$3077,$328c
	dc.w	$349d,$36ab,$38b3,$3ab8,$3cb7,$3eb2,$40a8,$4298
	dc.w	$4483,$4669,$4849,$4a24,$4bf8,$4dc7,$4f8f,$5151
	dc.w	$530c,$54c1,$566f,$5816,$59b6,$5b4f,$5ce1,$5e6b
	dc.w	$5fee,$6169,$62dc,$6448,$65ab,$6706,$6859,$69a4
	dc.w	$6ae6,$6c20,$6d51,$6e7a,$6f9a,$70b0,$71be,$72c3
	dc.w	$73be,$74b1,$759a,$7679,$774f,$781c,$78df,$7998
	dc.w	$7a48,$7aee,$7b8a,$7c1d,$7ca5,$7d24,$7d98,$7e02
	dc.w	$7e63,$7eb9,$7f05,$7f48,$7f80,$7fad,$7fd1,$7fea
	dc.w	$7ffa,$7fff
cosin:
	dc.w	$7fff,$7ff9,$7fea,$7fd1,$7fae,$7f81,$7f4a,$7f09
	dc.w	$7ebe,$7e69,$7e0a,$7da1,$7d2e,$7cb2,$7c2c,$7b9c
	dc.w	$7b02,$7a5f,$79b2,$78fb,$783b,$7772,$769f,$75c3
	dc.w	$74de,$73f0,$72f8,$71f8,$70ee,$6fdc,$6ec1,$6d9d
	dc.w	$6c70,$6b3c,$69fe,$68b9,$676b,$6615,$64b7,$6351
	dc.w	$61e3,$606e,$5ef1,$5d6d,$5be1,$5a4f,$58b5,$5714
	dc.w	$556c,$53be,$5209,$504d,$4e8c,$4cc4,$4af6,$4922
	dc.w	$4749,$456a,$4385,$419b,$3fac,$3db8,$3bbf,$39c2
	dc.w	$37c0,$35ba,$33af,$31a0,$2f8e,$2d78,$2b5e,$2940
	dc.w	$2720,$24fc,$22d6,$20ad,$1e81,$1c53,$1a23,$17f0
	dc.w	$15bc,$1386,$114f,$0f16,$0cdc,$0aa1,$0865,$0628
	dc.w	$03eb,$01ae,$ff71,$fd34,$faf7,$f8ba,$f67e,$f442
	dc.w	$f208,$efce,$ed96,$eb5f,$e92a,$e6f7,$e4c5,$e296
	dc.w	$e069,$de3e,$dc17,$d9f2,$d7d0,$d5b1,$d395,$d17d
	dc.w	$cf69,$cd58,$cb4b,$c943,$c73f,$c53f,$c344,$c14d
	dc.w	$bf5c,$bd6f,$bb88,$b9a6,$b7ca,$b5f4,$b423,$b258
	dc.w	$b093,$aed4,$ad1c,$ab6b,$a9bf,$a81b,$a67e,$a4e7
	dc.w	$a358,$a1d0,$a04f,$9ed6,$9d65,$9bfb,$9a99,$993f
	dc.w	$97ed,$96a4,$9562,$9429,$92f8,$91d0,$90b1,$8f9a
	dc.w	$8e8c,$8d87,$8c8b,$8b98,$8aae,$89ce,$88f6,$8828
	dc.w	$8763,$86a8,$85f7,$854e,$84b0,$841b,$8390,$830f
	dc.w	$8297,$8229,$81c5,$816b,$811b,$80d5,$8099,$8067
	dc.w	$803f,$8021,$800d,$8003,$8003,$800d,$8021,$803f
	dc.w	$8067,$8099,$80d5,$811b,$816b,$81c5,$8229,$8297
	dc.w	$830e,$8390,$841b,$84b0,$854e,$85f6,$86a8,$8763
	dc.w	$8828,$88f6,$89cd,$8aae,$8b98,$8c8b,$8d87,$8e8c
	dc.w	$8f9a,$90b1,$91d0,$92f8,$9429,$9562,$96a3,$97ed
	dc.w	$993f,$9a99,$9bfb,$9d64,$9ed6,$a04f,$a1d0,$a358
	dc.w	$a4e7,$a67d,$a81b,$a9bf,$ab6a,$ad1c,$aed4,$b092
	dc.w	$b257,$b422,$b5f3,$b7ca,$b9a6,$bb88,$bd6f,$bf5b
	dc.w	$c14d,$c343,$c53e,$c73e,$c942,$cb4b,$cd57,$cf68
	dc.w	$d17c,$d395,$d5b0,$d7cf,$d9f1,$dc16,$de3e,$e068
	dc.w	$e295,$e4c4,$e6f6,$e929,$eb5e,$ed95,$efcd,$f207
	dc.w	$f441,$f67d,$f8b9,$faf6,$fd33,$ff71,$01ad,$03ea
	dc.w	$0627,$0864,$0aa0,$0cdb,$0f15,$114e,$1385,$15bb
	dc.w	$17f0,$1a22,$1c52,$1e80,$20ac,$22d5,$24fc,$271f
	dc.w	$2940,$2b5d,$2d77,$2f8d,$31a0,$33ae,$35b9,$37bf
	dc.w	$39c1,$3bbf,$3db8,$3fac,$419b,$4384,$4569,$4748
	dc.w	$4921,$4af5,$4cc3,$4e8b,$504d,$5208,$53bd,$556c
	dc.w	$5713,$58b4,$5a4e,$5be1,$5d6d,$5ef1,$606e,$61e3
	dc.w	$6351,$64b6,$6614,$676a,$68b8,$69fe,$6b3b,$6c70
	dc.w	$6d9c,$6ec0,$6fdb,$70ee,$71f7,$72f8,$73ef,$74de
	dc.w	$75c3,$769f,$7772,$783b,$78fb,$79b2,$7a5f,$7b02
	dc.w	$7b9c,$7c2c,$7cb2,$7d2e,$7da1,$7e0a,$7e69,$7ebe
	dc.w	$7f09,$7f4a,$7f81,$7fae,$7fd1,$7fea,$7ff9,$7fff

;****************************************************************
;*			    wypelnianie				*
;****************************************************************
fill:
	move.l	clsadr(pc),a0
	add.l	#190*120-10,a0
	bsr.w	wblit
	move.l	a0,$dff050
	move.l	a0,$dff054
	move.w	#12,$dff064
	move.w	#12,$dff066
	move.l	#$09f0000a,$dff040
	move.l	#-1,$dff044
	move.w	#3*124*64+14,$dff058
	rts

cls:
	move.l	clsadr(pc),a0
	add.l	#8+67*120,a0
	moveq	#0,d0
	moveq	#0,d1
	moveq	#0,d2
	moveq	#0,d3
	moveq	#0,d4
	moveq	#0,d5
	moveq	#0,d6
	moveq	#0,d7

	moveq	#10,d7
.loop:
	movem.l	d0-d6,(a0)
	lea	40(a0),a0
	movem.l	d0-d6,(a0)
	lea	40(a0),a0
	movem.l	d0-d6,(a0)
	lea	40(a0),a0
	movem.l	d0-d6,(a0)
	lea	40(a0),a0
	movem.l	d0-d6,(a0)
	lea	40(a0),a0
	movem.l	d0-d6,(a0)
	lea	40(a0),a0
	movem.l	d0-d6,(a0)
	lea	40(a0),a0
	movem.l	d0-d6,(a0)
	lea	40(a0),a0
	movem.l	d0-d6,(a0)
	lea	40(a0),a0
	movem.l	d0-d6,(a0)
	lea	40(a0),a0
	movem.l	d0-d6,(a0)
	lea	40(a0),a0
	movem.l	d0-d6,(a0)
	lea	40(a0),a0
	movem.l	d0-d6,(a0)
	lea	40(a0),a0
	movem.l	d0-d6,(a0)
	lea	40(a0),a0
	movem.l	d0-d6,(a0)
	lea	40(a0),a0
	movem.l	d0-d6,(a0)
	lea	40(a0),a0
	movem.l	d0-d6,(a0)
	lea	40(a0),a0
	movem.l	d0-d6,(a0)
	lea	40(a0),a0
	movem.l	d0-d6,(a0)
	lea	40(a0),a0
	movem.l	d0-d6,(a0)
	lea	40(a0),a0
	movem.l	d0-d6,(a0)
	lea	40(a0),a0
	movem.l	d0-d6,(a0)
	lea	40(a0),a0
	movem.l	d0-d6,(a0)
	lea	40(a0),a0
	movem.l	d0-d6,(a0)
	lea	40(a0),a0
	movem.l	d0-d6,(a0)
	lea	40(a0),a0
	movem.l	d0-d6,(a0)
	lea	40(a0),a0
	movem.l	d0-d6,(a0)
	lea	40(a0),a0
	movem.l	d0-d6,(a0)
	lea	40(a0),a0
	movem.l	d0-d6,(a0)
	lea	40(a0),a0
	movem.l	d0-d6,(a0)
	lea	40(a0),a0

	movem.l	d0-d6,(a0)
	lea	40(a0),a0
	movem.l	d0-d6,(a0)
	lea	40(a0),a0
	movem.l	d0-d6,(a0)
	lea	40(a0),a0
	movem.l	d0-d6,(a0)
	lea	40(a0),a0
	movem.l	d0-d6,(a0)
	lea	40(a0),a0
	movem.l	d0-d6,(a0)
	lea	40(a0),a0

	dbf	d7,.loop
	rts

;*********************************************************
;*                       variables                       *
;*********************************************************
name:		dc.b	'graphics.library',0
	even
wart1:		dc.w	0
wart2:		dc.w	0
wart3:		dc.w	0
old:		dc.l	0
planeadr:	dc.l	screen1
clsadr:		dc.l	screen1+3*planesize
modula:		dc.l	0
wsk_sin1:	dc.l	sin1
wsk_sin2:	dc.l	sin2
wsk_sin3:	dc.l	sin3

licz:		dc.w	0
;*********************************************************
;*                     copper program                    *
;*********************************************************

copper:
planes:	dc.w	$e0,0,$e2,0,$e4,0,$e6,0
	dc.w	$e8,0,$ea,0,$ec,0,$ee,0

	dc.w	$0182,$0000,$0182,$0c00,$0184,$000c,$0186,$0c00
	dc.w	$0188,$0400,$018a,$0800,$018c,$0008,$018e,$0008
	dc.w	$0190,$0fff,$0192,$0fff,$0194,$0fff,$0196,$0fff
	dc.w	$0198,$0fff,$019a,$0fff,$019c,$0fff,$019e,$0fff

	dc.w	$ffe1,$fffe
	dc.w	$3401,$fffe
	dc.w	$8e,$2081,$90,$20c1,$92,$38,$94,$d0
	dc.w	$100,%0011001000000000,$102,0,$104,0
	dc.w	$108,80,$10a,80
	
	dc.w	$ffff,$fffe

;*********************************************************
;*                   dane wektorowe                      *
;*********************************************************
sin1:
	dc.w	$10f6,$1395,$160e,$1847,$1a2c,$1baa,$1cb2,$1d3a,$1d3d,$1cbb
	dc.w	$1bb8,$1a3f,$185f,$1629,$13b2,$1114,$0e68,$0bc8,$094d,$0711
	dc.w	$0527,$03a5,$0297,$020a,$0201,$027d,$037a,$04ed,$06ca,$08fd
	dc.w	$0b70,$0e0d
	dc.w	$1094,$1274,$1437,$15ce,$1729,$1839,$18f6,$1957,$1959,$18fc
	dc.w	$1844,$1736,$15df,$144b,$1289,$10aa,$0ec1,$0ce1,$0b1c,$0983
	dc.w	$0825,$0711,$0651,$05eb,$05e5,$063e,$06f2,$07fc,$0950,$0ae2
	dc.w	$0ca3,$0e80
	dc.w	$1063,$11e3,$134c,$1492,$15a7,$1681,$1718,$1766,$1767,$171d
	dc.w	$1689,$15b2,$149f,$135c,$11f4,$1075,$0eee,$0d6e,$0c03,$0abc
	dc.w	$09a4,$08c7,$082d,$07dc,$07d7,$081e,$08af,$0983,$0a93,$0bd5
	dc.w	$0d3c,$0eba
	dc.w	$1033,$1152,$1261,$1355,$1425,$14c9,$153a,$1574,$1575,$153e
	dc.w	$14cf,$142d,$135f,$126d,$115f,$1040,$0f1a,$0dfa,$0cea,$0bf5
	dc.w	$0b23,$0a7e,$0a0a,$09cd,$09c9,$09ff,$0a6b,$0b0a,$0bd7,$0cc8
	dc.w	$0dd5,$0ef3
	dc.w	$1002,$10c2,$1176,$1219,$12a3,$1311,$135c,$1383,$1384,$135e
	dc.w	$1315,$12a9,$1220,$117e,$10ca,$100a,$0f47,$0e87,$0dd2,$0d2e
	dc.w	$0ca2,$0c34,$0be7,$0bbe,$0bbc,$0bdf,$0c27,$0c92,$0d1a,$0dba
	dc.w	$0e6e,$0f2d
	dc.w	$0fd1,$1031,$108b,$10dc,$1122,$1158,$117e,$1191,$1192,$117f
	dc.w	$115a,$1124,$10e0,$108f,$1035,$0fd5,$0f73,$0f13,$0eb9,$0e67
	dc.w	$0e21,$0dea,$0dc3,$0daf,$0dae,$0dc0,$0de4,$0e19,$0e5d,$0ead
	dc.w	$0f07,$0f66
	dc.w	$0faf,$0fcb,$0fe7,$0fff,$1014,$1024,$102f,$1035,$1035,$1030
	dc.w	$1025,$1015,$1000,$0fe8,$0fcd,$0fb0,$0f93,$0f76,$0f5b,$0f42
	dc.w	$0f2d,$0f1d,$0f11,$0f0b,$0f0b,$0f10,$0f1b,$0f2b,$0f3f,$0f57
	dc.w	$0f72,$0f8f

	dc.w	$f8f,$f8f,$f8f,$f8f,$f8f,$f8f,$f8f,$f8f,$f8f	
	dc.w	$f8f,$f8f,$f8f,$f8f,$f8f,$f8f,$f8f,$f8f,$f8f	
	dc.w	$f8f,$f8f,$f8f,$f8f,$f8f,$f8f,$f8f,$f8f,$f8f	
	dc.w	$f8f,$f8f,$f8f,$f8f,$f8f,$f8f,$f8f,$f8f,$f8f	
	dc.w	-1

sin2:
	dc.w	$0b56,$0a96,$09e2,$093f,$08b5,$0847,$07fc,$07d5,$07d4,$07fa
	dc.w	$0843,$08af,$0938,$09da,$0a8e,$0b4e,$0c11,$0cd1,$0d86,$0e2a

	dc.w	$0eb6,$0f24,$0f71,$0f9a,$0f9c,$0f79,$0f31,$0ec6,$0e3e,$0d9e
	dc.w	$0cea,$0c2b

	dc.w	$0b6a,$0ad0,$0a40,$09be,$094f,$08f8,$08bb,$089c,$089b,$08b9
	dc.w	$08f4,$094a,$09b8,$0a3a,$0aca,$0b63,$0bff,$0c99,$0d2a,$0dad
	dc.w	$0e1d,$0e75,$0eb3,$0ed3,$0ed5,$0eb9,$0e7f,$0e2a,$0dbd,$0d3c
	dc.w	$0cad,$0c14

	dc.w	$0b7d,$0b0a,$0a9e,$0a3c,$09e9,$09a8,$097a,$0963,$0963,$0979
	dc.w	$09a5,$09e6,$0a38,$0a99,$0b05,$0b78,$0bed,$0c61,$0ccd,$0d30
	dc.w	$0d84,$0dc6,$0df4,$0e0c,$0e0e,$0df9,$0dcd,$0d8d,$0d3c,$0cdb
	dc.w	$0c70,$0bfd

	dc.w	$0b91,$0b44,$0afc,$0abb,$0a83,$0a58,$0a3a,$0a2a,$0a2a,$0a39
	dc.w	$0a56,$0a81,$0ab8,$0af9,$0b41,$0b8d,$0bdc,$0c28,$0c71,$0cb2
	dc.w	$0cea,$0d17,$0d35,$0d46,$0d47,$0d38,$0d1b,$0cf1,$0cbb,$0c7a
	dc.w	$0c32,$0be6

	dc.w	$0ba4,$0b7e,$0b5a,$0b39,$0b1e,$0b08,$0af9,$0af1,$0af1,$0af8
	dc.w	$0b07,$0b1d,$0b38,$0b58,$0b7c,$0ba3,$0bca,$0bf0,$0c14,$0c35
	dc.w	$0c51,$0c67,$0c77,$0c7f,$0c7f,$0c78,$0c6a,$0c54,$0c39,$0c19
	dc.w	$0bf5,$0bcf

	dc.w	$0bae,$0b9b,$0b89,$0b79,$0b6b,$0b60,$0b58,$0b55,$0b54,$0b58
	dc.w	$0b60,$0b6a,$0b78,$0b88,$0b9a,$0bad,$0bc1,$0bd4,$0be6,$0bf7
	dc.w	$0c05,$0c10,$0c17,$0c1b,$0c1c,$0c18,$0c11,$0c06,$0bf9,$0be9
	dc.w	$0bd7,$0bc4

	dc.w	$0bb3,$0baa,$0ba0,$0b98,$0b91,$0b8c,$0b88,$0b86,$0b86,$0b88
	dc.w	$0b8c,$0b91,$0b98,$0ba0,$0ba9,$0bb3,$0bbc,$0bc6,$0bcf,$0bd7
	dc.w	$0bde,$0be4,$0be8,$0bea,$0bea,$0be8,$0be4,$0bdf,$0bd8,$0bd0
	dc.w	3010,3010

	dc.w	$bd0,$bd0,$bd0,$bd0,$bd0,$bd0,$bd0,$bd0,$bd0
	dc.w	$bd0,$bd0,$bd0,$bd0,$bd0,$bd0,$bd0,$bd0,$bd0
	dc.w	$bd0,$bd0,$bd0,$bd0,$bd0,$bd0,$bd0,$bd0,$bd0
	dc.w	$bd0,$bd0,$bd0,$bd0,$bd0,$bd0,$bd0,$bd0,$bd0

	dc.w	-1

sin3:
	dc.w	$0042,$0041,$003f,$003e,$003d,$003c,$003a,$0039,$0038,$0036
	dc.w	$0035,$0034,$0032,$0031,$002f,$002e,$002c,$002b,$0029,$0028
	dc.w	$0026,$0025,$0024,$0022,$0021,$0020,$001e,$001d,$001c,$001b
	dc.w	$0019,$0018,$0017,$0016,$0015,$0014,$0014,$0013,$0012,$0012
	dc.w	$0011,$0011,$0010,$0010,$000f,$000f,$000f,$000f,$000f,$000f
	dc.w	$000f,$000f,$0010,$0010,$0011,$0011,$0012,$0012,$0013,$0014
	dc.w	$0014,$0015,$0016,$0017,$0018,$0019,$001b,$001c,$001d,$001e
	dc.w	$0020,$0021,$0022,$0024,$0025,$0026,$0028,$0029,$002b,$002c
	dc.w	$002e,$002f,$0031,$0032,$0034,$0035,$0036,$0038,$0039,$003a
	dc.w	$003c,$003d,$003e,$003f,$0041,$0042,$0043,$0044,$0045,$0046
	dc.w	$0046,$0047,$0048,$0048,$0049,$0049,$004a,$004a,$004b,$004b
	dc.w	$004b,$004b,$004b,$004b,$004b,$004b,$004a,$004a,$0049,$0049
	dc.w	$0048,$0048,$0047,$0046,$0046,$0045,$0044,$0043

	dc.w	-1

;*********************************************************
;*********************************************************
;*********************************************************
wektor:
	dc.w	-4000,4000,4000		;0
	dc.w	4000,4000,4000		;4
zm1:	dc.w	4000,0,4000		;8
ch1:	dc.w	4000,-4000,4000		;12
ch2:	dc.w	-4000,-4000,4000	;16
zm2:	dc.w	-4000,0,4000		;20

	dc.w	-4000,4000,-4000	;24
	dc.w	4000,4000,-4000		;28
zm3:	dc.w	4000,0,-4000		;32
ch3:	dc.w	4000,-4000,-4000	;36
ch4	dc.w	-4000,-4000,-4000	;40
zm4:	dc.w	-4000,0,-4000		;44

	dc.w	$7fff


line:
	dc.w	80
	dc.w	0,4,8		;pierwsza sciana
	dc.w	1,4,8,12
	dc.w	0,16,0
	dc.w	0,0,4
	dc.w	0,12,16

	dc.w	0
	dc.w	32,28,24	;pierwsza sciana
	dc.w	0,24,28
	dc.w	1,28,32,36
	dc.w	0,36,40
	dc.w	1,40,44,24

	dc.w	40
	dc.w	4,28,32
	dc.w	0,4,28
	dc.w	1,28,32,36
	dc.w	0,36,12
	dc.w	1,12,8,4

	dc.w	80
	dc.w	24,0,20
	dc.w	0,0,24
	dc.w	0,0,16
	dc.w	0,16,40
	dc.w	1,40,44,24

	dc.w	$7fff

punkty2d:
	blk.l	20,0

;*********************************************************
;*                        screen                          *
;*********************************************************
screen1:
	blk.b	6*planesize,0
