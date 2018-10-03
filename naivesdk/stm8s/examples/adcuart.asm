stm8
	segment at 0-FF 'reg'
	segment at 100-17FF 'ram'
	segment at 4000-45FF 'eeprom'
	segment at 8000-17FFF 'flash'
	segment at 0-17FFF 'unify'

unify	segment byte at: 5000	'unify'
gpioaodr:
	ds.b 
gpioaidr:
	ds.b 
gpioaddr:
	ds.b 
gpioacr1:
	ds.b 
gpioacr2:
	ds.b 
gpiobodr:
	ds.b 
gpiobidr:
	ds.b 
gpiobddr:
	ds.b 
gpiobcr1:
	ds.b 
gpiobcr2:
	ds.b 
gpiocodr:
	ds.b 
gpiocidr:
	ds.b 
gpiocddr:
	ds.b 
gpioccr1:
	ds.b 
gpioccr2:
	ds.b 
gpiododr:
	ds.b 
gpiodidr:
	ds.b 
gpiodddr:
	ds.b 
gpiodcr1:
	ds.b 
gpiodcr2:
	ds.b 
gpioeodr:
	ds.b 
gpioeidr:
	ds.b 
gpioeddr:
	ds.b 
gpioecr1:
	ds.b 
gpioecr2:
	ds.b 
gpiofodr:
	ds.b 
gpiofidr:
	ds.b 
gpiofddr:
	ds.b 
gpiofcr1:
	ds.b 
gpiofcr2:
	ds.b 
gpiogodr:
	ds.b 
gpiogidr:
	ds.b 
gpiogddr:
	ds.b 
gpiogcr1:
	ds.b 
gpiogcr2:
	ds.b 
gpiohodr:
	ds.b 
gpiohidr:
	ds.b 
gpiohddr:
	ds.b 
gpiohcr1:
	ds.b 
gpiohcr2:
	ds.b 
gpioiodr:
	ds.b 
gpioiidr:
	ds.b 
gpioiddr:
	ds.b 
gpioicr1:
	ds.b 
gpioicr2:
	ds.b 
unify	segment byte at: 505A	'unify'
flashcr1:
	ds.b 
flashcr2:
	ds.b 
flashncr2:
	ds.b 
flashfpr:
	ds.b 
flashnfpr:
	ds.b 
flashiapsr:
	ds.b 
unify	segment byte at: 5062	'unify'
flashpukr:
	ds.b 
unify	segment byte at: 5064	'unify'
flashdukr:
	ds.b 
unify	segment byte at: 50A0	'unify'
itcexticr1:
	ds.b 
itcexticr2:
	ds.b 
unify	segment byte at: 50B3	'unify'
rstsr:
	ds.b 
unify	segment byte at: 50C0	'unify'
clkickr:
	ds.b 
clkeckr:
	ds.b 
unify	segment byte at: 50C3	'unify'
clkcmsr:
	ds.b 
clkswr:
	ds.b 
clkswcr:
	ds.b 
clkckdivr:
	ds.b 
clkpckenr1:
	ds.b 
clkcssr:
	ds.b 
clkccor:
	ds.b 
clkpckenr2:
	ds.b 
clkcanccr:
	ds.b 
clkhsitrimr:
	ds.b 
clkswimccr:
	ds.b 
unify	segment byte at: 50D1	'unify'
wwdgcr:
	ds.b 
wwdgwr:
	ds.b 
unify	segment byte at: 50E0	'unify'
iwdgkr:
	ds.b 
iwdgpr:
	ds.b 
iwdgrlr:
	ds.b 
unify	segment byte at: 50F0	'unify'
awucsr1:
	ds.b 
awuapr:
	ds.b 
awutbr:
	ds.b 
beepcsr:
	ds.b 
unify	segment byte at: 5200	'unify'
spicr1:
	ds.b 
spicr2:
	ds.b 
spiicr:
	ds.b 
spisr:
	ds.b 
spidr:
	ds.b 
spicrcpr:
	ds.b 
spirxcrcr:
	ds.b 
spitxcrcr:
	ds.b 
unify	segment byte at: 5210	'unify'
i2ccr1:
	ds.b 
i2ccr2:
	ds.b 
i2cfreqr:
	ds.b 
i2coarl:
	ds.b 
i2coarh:
	ds.b 
unify	segment byte at: 5216	'unify'
i2cdr:
	ds.b 
i2csr1:
	ds.b 
i2csr2:
	ds.b 
i2csr3:
	ds.b 
i2citr:
	ds.b 
i2cccrl:
	ds.b 
i2cccrh:
	ds.b 
i2ctriser:
	ds.b 
unify	segment byte at: 5230	'unify'
uart1sr:
	ds.b 
uart1dr:
	ds.b 
uart1brr1:
	ds.b 
uart1brr2:
	ds.b 
uart1cr1:
	ds.b 
uart1cr2:
	ds.b 
uart1cr3:
	ds.b 
uart1cr4:
	ds.b 
uart1cr5:
	ds.b 
uart1gtr:
	ds.b 
uart1pstr:
	ds.b 
unify	segment byte at: 5240	'unify'
uart3sr:
	ds.b 
uart3dr:
	ds.b 
uart3brr1:
	ds.b 
uart3brr2:
	ds.b 
uart3cr1:
	ds.b 
uart3cr2:
	ds.b 
uart3cr3:
	ds.b 
uart3cr4:
	ds.b 
unify	segment byte at: 5249	'unify'
uart3cr6:
	ds.b 
unify	segment byte at: 5250	'unify'
tim1cr1:
	ds.b 
tim1cr2:
	ds.b 
tim1smcr:
	ds.b 
tim1etr:
	ds.b 
tim1ier:
	ds.b 
tim1sr1:
	ds.b 
tim1sr2:
	ds.b 
tim1egr:
	ds.b 
tim1ccmr1:
	ds.b 
tim1ccmr2:
	ds.b 
tim1ccmr3:
	ds.b 
tim1ccmr4:
	ds.b 
tim1ccer1:
	ds.b 
tim1ccer2:
	ds.b 
tim1cntrh:
	ds.b 
tim1cntrl:
	ds.b 
tim1pscrh:
	ds.b 
tim1pscrl:
	ds.b 
tim1arrh:
	ds.b 
tim1arrl:
	ds.b 
tim1rcr:
	ds.b 
tim1ccr1h:
	ds.b 
tim1ccr1l:
	ds.b 
tim1ccr2h:
	ds.b 
tim1ccr2l:
	ds.b 
tim1ccr3h:
	ds.b 
tim1ccr3l:
	ds.b 
tim1ccr4h:
	ds.b 
tim1ccr4l:
	ds.b 
tim1bkr:
	ds.b 
tim1dtr:
	ds.b 
tim1oisr:
	ds.b 
unify	segment byte at: 5300	'unify'
tim2cr1:
	ds.b 
tim2ier:
	ds.b 
tim2sr1:
	ds.b 
tim2sr2:
	ds.b 
tim2egr:
	ds.b 
tim2ccmr1:
	ds.b 
tim2ccmr2:
	ds.b 
tim2ccmr3:
	ds.b 
tim2ccer1:
	ds.b 
tim2ccer2:
	ds.b 
tim2cntrh:
	ds.b 
tim2cntrl:
	ds.b 
tim2pscr:
	ds.b 
tim2arrh:
	ds.b 
tim2arrl:
	ds.b 
tim2ccr1h:
	ds.b 
tim2ccr1l:
	ds.b 
tim2ccr2h:
	ds.b 
tim2ccr2l:
	ds.b 
tim2ccr3h:
	ds.b 
tim2ccr3l:
	ds.b 
unify	segment byte at: 5320	'unify'
tim3cr1:
	ds.b 
tim3ier:
	ds.b 
tim3sr1:
	ds.b 
tim3sr2:
	ds.b 
tim3egr:
	ds.b 
tim3ccmr1:
	ds.b 
tim3ccmr2:
	ds.b 
tim3ccer1:
	ds.b 
tim3cntrh:
	ds.b 
tim3cntrl:
	ds.b 
tim3pscr:
	ds.b 
tim3arrh:
	ds.b 
tim3arrl:
	ds.b 
tim3ccr1h:
	ds.b 
tim3ccr1l:
	ds.b 
tim3ccr2h:
	ds.b 
tim3ccr2l:
	ds.b 
unify	segment byte at: 5340	'unify'
tim4cr1:
	ds.b 
tim4ier:
	ds.b 
tim4sr:
	ds.b 
tim4egr:
	ds.b 
tim4cntr:
	ds.b 
tim4pscr:
	ds.b 
tim4arr:
	ds.b 
unify	segment byte at: 5400	'unify'
adccsr:
	ds.b 
adccr1:
	ds.b 
adccr2:
	ds.b 
adccr3:
	ds.b 
adcdrh:
	ds.b 
adcdrl:
	ds.b 
adctdrh:
	ds.b 
adctdrl:
	ds.b 
unify	segment byte at: 48CD	'unify'
unicidx:
	ds.w 
unicidy:
	ds.w 
unicidwafer:
	ds.b 
unicidlot0:
	ds.b 
unicidlot1:
	ds.b 
unicidlot2:
	ds.b 
unicidlot3:
	ds.b 
unicidlot4:
	ds.b 
unicidlot5:
	ds.b 
unicidlot6:
	ds.b 
unify	segment byte at: 4800	'unify'
optionopt0:
	ds.b 
optionopt1:
	ds.b 
optionnopt1:
	ds.b 
optionopt2:
	ds.b 
optionnopt2:
	ds.b 
optionopt3:
	ds.b 
optionnopt3:
	ds.b 
optionopt4:
	ds.b 
optionnopt4:
	ds.b 
optionopt5:
	ds.b 
optionnopt5:
	ds.b 
optionopt6:
	ds.b 
optionnopt6:
	ds.b 
optionopt7:
	ds.b 
optionnopt7:
	ds.b 
unify	segment byte at: 487E	'unify'
bootloaderoptbl:
	ds.b 
bootloadernoptbl:
	ds.b 
unify	segment byte at: 7F00	'unify'
cpua:
	ds.b 
cpupce:
	ds.b 
cpupch:
	ds.b 
cpupcl:
	ds.b 
cpuxh:
	ds.b 
cpuxl:
	ds.b 
cpuyh:
	ds.b 
cpuyl:
	ds.b 
cpusph:
	ds.b 
cpuspl:
	ds.b 
cpuccr:
	ds.b 
unify	segment byte at: 7F60	'unify'
cpugcr:
	ds.b 
unify	segment byte at: 7F70	'unify'
itcspr1:
	ds.b 
itcspr2:
	ds.b 
itcspr3:
	ds.b 
itcspr4:
	ds.b 
itcspr5:
	ds.b 
itcspr6:
	ds.b 
itcspr7:
	ds.b 
itcspr8:
	ds.b 
unify	segment byte at: 7F80	'unify'
swimcsr:
	ds.b 
unify	segment byte at: 7F90	'unify'
dmbk1re:
	ds.b 
dmbk1rh:
	ds.b 
dmbk1rl:
	ds.b 
dmbk2re:
	ds.b 
dmbk2rh:
	ds.b 
dmbk2rl:
	ds.b 
dmcr1:
	ds.b 
dmcr2:
	ds.b 
dmcsr1:
	ds.b 
dmcsr2:
	ds.b 
dmenfctr:
	ds.b 
ram	segment	'ram'
temp:
	ds.w 
flash	segment byte at: 8000	'flash'
interruptreset:
	dc.l	{$82000000+start}
interrupttrap:
	dc.l	{$82000000+dumb}
interrupttli:
	dc.l	{$82000000+dumb}
interruptawu:
	dc.l	{$82000000+dumb}
interruptclk:
	dc.l	{$82000000+dumb}
interruptexti0:
	dc.l	{$82000000+dumb}
interruptexti1:
	dc.l	{$82000000+dumb}
interruptexti2:
	dc.l	{$82000000+dumb}
interruptexti3:
	dc.l	{$82000000+dumb}
interruptexti4:
	dc.l	{$82000000+dumb}
interruptcanrx:
	dc.l	{$82000000+dumb}
interruptcantx:
	dc.l	{$82000000+dumb}
interruptspi:
	dc.l	{$82000000+dumb}
interrupttim1u:
	dc.l	{$82000000+ontim1u}
interrupttim2u:
	dc.l	{$82000000+dumb}
interrupttim2c:
	dc.l	{$82000000+dumb}
interrupttim3u:
	dc.l	{$82000000+dumb}
interrupttim3c:
	dc.l	{$82000000+dumb}
interruptuart1rx:
	dc.l	{$82000000+dumb}
interruptuart1tx:
	dc.l	{$82000000+dumb}
interrupti2c:
	dc.l	{$82000000+dumb}
interruptuart3rx:
	dc.l	{$82000000+dumb}
interruptuart3tx:
	dc.l	{$82000000+dumb}
interruptadc:
	dc.l	{$82000000+onadc}
interrupttim4u:
	dc.l	{$82000000+dumb}
interruptflash:
	dc.l	{$82000000+dumb}
dumb:
	iret
testline:
	dc.b	"testline..."
	dc.b	{32}
	dc.b	{0}
onadc:
	iret
ontim1u:
	btjf tim1sr1,#0,otherint
	bres tim1sr1,#0
	bcpl gpiododr,#6
	push a
	ld a,adccsr
	and a,#$F0
	or a,#1
	ld adccsr,a
	bset adccr1,#0
	bset adccr1,#0
local_0_34_1:
	btjt adccsr,#6,local_0_34_1
	ld a,adcdrl
	ld xl,a
	ld a,adcdrh
	ld xh,a
	pop a
	call c2s
	ld a,#32
local__37_1:
	btjf uart1sr,#7,local__37_1
	ld uart1dr,a
	ldw temp,x
	pushw x
	push a
	clrw x
local_0_39_2:
	ld a,(temp,x)
	tnz a
	jreq local_0_39_1
local_0_39_3:
	btjf uart1sr,#7,local_0_39_3
	ld uart1dr,a
	incw x
	jra local_0_39_2
local_0_39_1:
	pop a
	popw x
otherint:
	iret
	jp local__42_1
ram	segment	'ram'
local__42_21:
	ds.b 
local__42_22:
	ds.b 
local__42_23:
	ds.b 
local__42_24:
	ds.b 
local__42_25:
	ds.b 
local__42_3:
	ds.b 
flash	segment	'flash'
c2s:
	push a
	pushw y
	mov local__42_21,#32
	mov local__42_22,#32
	mov local__42_23,#32
	mov local__42_24,#32
	clr local__42_25
	ldw y,#10
	clrw x
	ld xl,a
	divw x,y
	ld a,xl
	ld local__42_3,a
	ld a,yl
	add a,#33
	ld local__42_24,a
	ld a,local__42_3
	ldw y,#10
	clrw x
	ld xl,a
	divw x,y
	ld a,xl
	ld local__42_3,a
	ld a,yl
	add a,#33
	ld local__42_23,a
	ld a,local__42_3
	ldw y,#10
	clrw x
	ld xl,a
	divw x,y
	ld a,xl
	ld local__42_3,a
	ld a,yl
	add a,#33
	ld local__42_22,a
	ld a,local__42_3
	ldw y,#10
	clrw x
	ld xl,a
	divw x,y
	ld a,xl
	ld local__42_3,a
	ld a,yl
	add a,#33
	ld local__42_21,a
	ldw x,#local__42_21
	popw y
	pop a
	ret
local__42_1:
	nop
start:
	ldw x,#$17ff
	ldw sp,x
	clr tim1cr2
	clr tim1smcr
	clr tim1etr
	bset tim1ier,#0
	mov tim1pscrh,#{high {2000000/1000}}
	mov tim1pscrl,#{low {2000000/1000}}
	mov tim1arrh,#{high 500}
	mov tim1arrl,#{low 500}
	bset tim1cr1,#2
	bset tim1cr1,#0
	clr uart1cr1
	clr uart1cr2
	clr uart1cr3
	clr uart1cr4
	clr uart1cr5
	bres uart1cr1,#4
	bres uart1cr1,#2
	bres uart1cr3,#4
	bres uart1cr3,#5
	mov uart1brr2,#{{2000000/9600} and $0f}
	mov uart1brr1,#{{{2000000/9600} shr 4} and $ff}
	bres uart1cr2,#3
	bres uart1cr2,#2
	bset uart1cr3,#2
	bset uart1cr3,#1
	bset uart1cr3,#0
	bset uart1cr2,#3
	bset uart1cr2,#2
	bset uart1cr3,#3
	bset gpioaddr,#5
	bset gpioacr1,#5
	bres gpioaddr,#4
	bres gpioacr1,#4
	bres gpioacr2,#4
	mov adctdrl,#{1 shl 1}
	push a
	ld a,#7
	swap a
	ld adccr1,a
	ld a,adccsr
	and a,#$F0
	or a,#1
	ld adccsr,a
	pop a
	bset adccr2,#3
	bset adccr1,#0
	bset gpiodddr,#6
	bset gpiodcr1,#6
	rim
loop:
	wfi
	jra loop

	END

File composed by PCEUDO