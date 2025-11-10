[org 0x0100]
jmp start

buffer: times 4000 dw 0		; TO COPY ALL COLUMNS IN A ROW EXCEPT THE 1ST ONE
buffer1:times 1 dw 0		; TO COPY 1ST COLUMNS IN A ROW

forssbuffer: times 4000 db 0
msg1: db ' --- WELCOME --- '
msg2: db ' Umaima Shahid '
msg3: db ' 23L-0832 '
msg4:db ' Haadiyah Zafar'
msg5: db ' 23L-0744 '

; FLAGS AND CHECKS
scoring:dw 0
seconds: dw 0
timerflag: dw 1
exitflag: dw 0
collisionflag: dw 0
flagbird:dw 'D'

currentRow: dw 0
currentCol: dw 0

; PILLAR VARIABLES
pillarSpace: dw 6
pillarWidth: dw 4

StartingCol1: dw 37
EndingRow1: dw 8
EndingCol1: dw 41
endingCol1: dw 41

StartingCol2: dw 74
EndingRow2: dw 4
EndingCol2: dw 78
endingCol2: dw 78

birdrowstart: dw 10 
birdrowend: dw 11
birdcolstart: dw 14
birdcolend: dw 20   ; 23

oldkb: dd 0 
oldisr: dd 0
message: db 10, 13, 'Exitting the game.... $'
message1: db '--- GAME PAUSED ---'
message2: db 'Press P to Resume '
message3: db 'Press R to Restart '
message4:db 'Press Q to Quit'

; RULES
rule1: db '--- Flappy Bird Rules ---'
rule2: db '1. Keep Pressing the SPACE BAR to make the bird fly upwards.'
rule3: db '2. Avoid the obstacles (pipes) by navigating through the gaps.'
rule4: db '3. Stay in the air! If the bird hits the ground or a pipe, the game is over.'
rule5: db '4. Earn SCORE by successfully passing through the gaps.'
rule6: db '5. Keep it PRESSED! The bird will fall if you stop tapping.'
outMessage: db '--- GAME OVER! ---'
defaultmsg: db 'Press Enter Key To Continue...' 
score: db ' Score:  ' 
Time: db ' Time: '

; ----------------------------------------------------------------------------------------------
; ----------------------------------------------------------------------------------------------

printbird:
    push bp
    mov bp, sp
    pusha

calculate_di:
    ; Calculate start position
    mov ax, word [bp + 4]        ; Starting row
    mov bx, 80                   ; Number of columns in a row
    mul bx                       ; Multiply row by 80 to get row offset
    add ax, word [bp + 6]		 ; Add starting column
    shl ax, 1                    ; Multiply by 2 (each char is 2 bytes)
    mov di, ax                   ; Set source index

    mov ax, 0xb800
    mov es, ax

    add di, 6
    mov word [es:di], 0x4800
    add di, 2
    mov word [es:di], 0x4800
    add di, 2
    mov word [es:di], 0x3425    ; Print '%'

calculate_di_next_row:
    ; Calculate next row position
    mov ax, word [bp + 4]
    add ax, 1
    mov bx, 80
    mul bx
    add ax, word [bp + 6]
    shl ax, 1
    mov di, ax

    add di, 4
    mov word [es:di], 0x4800
    add di, 2
    mov word [es:di], 0x4800
    add di, 2
    mov word [es:di], 0x4800
    add di, 2
    mov word [es:di], 0x3000

    popa
    pop bp
    ret 4

; ----------------------------------------------------------------------------------------------
; ----------------------------------------------------------------------------------------------

MoveBirdForClearing:
    push bp
    mov bp, sp
    pusha

calculate_di_for_clearing:
    mov ax, word [bp + 4]
    mov bx, 80
    mul bx
    add ax, word [bp + 6]
    shl ax, 1
    mov di, ax

    mov ax, 0xb800
    mov es, ax

    add di, 6
    mov word [es:di], 0x3000
    add di, 2
    mov word [es:di], 0x3000
    add di, 2
    mov word [es:di], 0x3000

calculate_di_next_row_for_clearing:
    mov ax, word [bp + 4]
    add ax, 1
    mov bx, 80
    mul bx
    add ax, word [bp + 6]
    shl ax, 1
    mov di, ax

    add di, 4
    mov word [es:di], 0x3000
    add di, 2
    mov word [es:di], 0x3000
    add di, 2
    mov word [es:di], 0x3000
    add di, 2
    mov word [es:di], 0x3000

    popa
    pop bp
    ret 4
	

; ----------------------------------------------------------------------------------------------
; ----------------------------------------------------------------------------------------------

moveBird:
	pusha
	
	cmp word[flagbird], 'U'
	je movebirdup	
	jmp movebirddown
		
movebirdup:
    cmp word [birdrowstart], 0
    jle printCollision1

	push word [birdcolstart]
    push word [birdrowstart]
    call MoveBirdForClearing
	
	sub word [birdrowstart], 1
	sub word [birdrowend], 1
	
    push word [birdcolstart]
    push word [birdrowstart]
    call printbird
	jmp return

movebirddown:
    cmp word [birdrowstart], 18
    jge printCollision1

	push word [birdcolstart]
    push word [birdrowstart]
    call MoveBirdForClearing
		
    add word [birdrowstart], 1
    add word [birdrowend], 1
	
    push word [birdcolstart]
    push word [birdrowstart]
    call printbird
return:
	popa
    ret 4

; ----------------------------------------------------------------------------------------------
; ----------------------------------------------------------------------------------------------

kbisr:	
	push es
	pusha
	
	mov ax, 0xb800
	mov es, ax 
	
	in al, 0x60
	; UP
    cmp al, 0x39
    je bird_up
	
	; DOWN
    cmp al, 0xB9
    je bird_down
	
	; QUIT
	cmp al,0x10
	je exitting
	
	; PAUSE / RESTART
	cmp al,0x01
	je printingscreen
	
    jmp nomatch
	
bird_up:
	mov ax, 'U'
	mov [flagbird], ax
    jmp nomatch
	
bird_down:
    mov ax, 'D'
	mov [flagbird], ax
	jmp nomatch

; ----------------------------------------------------------------------------------------------
; ----------------------------------------------------------------------------------------------

clearPatch:
	mov ax, 0xb800
    mov es, ax
    mov di, 0
	mov cx, 20
row:
    push cx
    mov cx, 60
col:
	mov word [es:di], 0x3000
	add di, 2
	loop col
	
    pop cx
    dec cx
    loop row
	
nomatch:
	popa
	pop es
	jmp far [cs:oldisr]

exit:
	mov al, 0x20
	out 0x20, al
	iret

exitting: 
	mov word [exitflag],1
	jmp nomatch

; ----------------------------------------------------------------------------------------------
; ----------------------------------------------------------------------------------------------

clrscr:
    push es
    push ax
    push di

    mov ax, 0xb800
    mov es, ax
    mov di, 0

nextloc:
    mov word [es:di], 0x0720
    add di, 2
    cmp di, 4000
    jne nextloc

    pop di
    pop ax
    pop es
    ret

; ----------------------------------------------------------------------------------------------
; ----------------------------------------------------------------------------------------------

softwareintforexitting:
    pusha
	push es 
	push ds
    mov dx, message              ; Load address of message
    mov ah, 9                    ; Service 9 - Write String
    int 21h 
	pop ds
	pop es
	
	popa  
	ret
	
exit_program:
    call clrscr
    call softwareintforexitting
	mov word [cs:timerflag], 0	
	
    mov ax, 0x4c00
    int 0x21

; ----------------------------------------------------------------------------------------------
; ----------------------------------------------------------------------------------------------

printingscreen:
    call screendraw
    
wait_loop:
	
	in al, 0x60 
	
	cmp al, 0x19
	je clearPatch						; CLEAR BACKGROUND
	
	cmp al, 0x13						; RESTART
	je movevariables
	
	cmp al,0x10							; QUIT
	je exitting
	
	jmp wait_loop
	ret


; ----------------------------------------------------------------------------------------------
; ----------------------------------------------------------------------------------------------

screendraw:
	pusha
	push ds
	pop es
	
	mov ah, 0x13
	mov al, 1
	mov bh, 0

	; PRINTING 1ST MESSAGE
	mov bl, 00100000B
	mov cx, 19
	mov dx, 0x0103
	mov bp, message1
	INT 0x10

	; PRINTING 2ND MESSAGE
	mov dx, 0x0303
	mov cx, 18
	mov bl, 01010000B
	mov bp, message2
	INT 0x10
	
	; PRINTING 3RD message
	mov dx, 0x0503
	mov cx, 18
	mov bl, 01010000B
	mov bp, message3
	INT 0x10
	
	; PRINTING 4th message
	mov dx, 0x0703 		; index
	mov cx, 18			; length of string
	mov bl, 01010000B	; normal attribute
	mov bp, message4
	INT 0x10			; call BIOS video service
	
	popa
	ret

; ----------------------------------------------------------------------------------------------
; ----------------------------------------------------------------------------------------------

movevariables:
	mov word[scoring], 0
	mov word[cs:seconds], 0
	mov word[cs:timerflag], 1
	mov word [exitflag], 0
	mov word [collisionflag], 0
	mov word[flagbird], 'D'

	mov word[currentRow], 0
	mov word[currentCol], 0
	mov word[pillarSpace], 6
	mov word[pillarWidth], 4
	mov word[StartingCol1], 74
	mov word[EndingRow1], 4
	mov word[EndingCol1], 78
	mov word[endingCol1], 78
	
	mov word[StartingCol2], 37
	mov word[EndingRow2], 8
	mov word[EndingCol2], 41
	mov word[endingCol2], 41
	
	mov word[birdrowstart], 10
	mov word[birdrowend], 11
	mov word[birdcolstart], 20
	mov word[birdcolend], 25

	call PrintStartScreen
	jmp nomatch
	
; ----------------------------------------------------------------------------------------------
; ----------------------------------------------------------------------------------------------

RESTARTGAMEAGAIN:
	mov word[scoring], 0
	mov word[cs:seconds], 0
	mov word[cs:timerflag], 1
	mov word [exitflag], 0
	mov word [collisionflag], 0
	mov word[flagbird], 'D'

	mov word[currentRow], 0
	mov word[currentCol], 0
	mov word[pillarSpace], 6
	mov word[pillarWidth], 4
	mov word[StartingCol1], 74
	mov word[EndingRow1], 4
	mov word[EndingCol1], 78
	mov word[endingCol1], 78
	
	mov word[StartingCol2], 37
	mov word[EndingRow2], 8
	mov word[EndingCol2], 41
	mov word[endingCol2], 41
	
	mov word[birdrowstart], 10
	mov word[birdrowend], 11
	mov word[birdcolstart], 20
	mov word[birdcolend], 25
	call PrintStartScreen
	jmp kk

; ----------------------------------------------------------------------------------------------
; ----------------------------------------------------------------------------------------------

PrintStartScreen:
	pusha
	
	call background
	
	push word[StartingCol1]      
	push word[EndingRow1]        
	call printRectangle

	push word[StartingCol2]      
	push word[EndingRow2]        
	call printRectangle

	push word [birdcolstart]
	push word[birdrowstart]
    call printbird
	
	
	mov ah, 0x13		; service 13 - print string
	mov al, 1			; subservice 01 – update cursor 
	mov bh, 0			; output on page 0	
	;es:bp = ds:message
	push ds
	pop es				; es=ds segment of string
	
	mov dx, 0x0002 		; index
	mov cx, 9		; length of string
	mov bl, 0x0F	; normal attribute
	mov bp, score
	INT 0x10			; call BIOS video service
	
	popa
	ret

; ----------------------------------------------------------------------------------------------
; ----------------------------------------------------------------------------------------------

PlayAnimation:
	pusha
	animationLoop:
		cmp word [exitflag], 1
		je exit_program
		jmp move
		
	updateloop1:
		add word [StartingCol1], 77
		mov ax, word [StartingCol1]
		add ax, word[pillarWidth]
		mov word[EndingCol1], ax
		add word [endingCol1], 81
		jmp move
	
	updateloop2:
		add word [StartingCol2], 77
		mov ax, word [StartingCol2]
		add ax, word[pillarWidth]
		mov word[EndingCol2], ax
		add word [endingCol2], 81
		jmp move
	
	
	move:
	    cmp word [exitflag], 1
	    je exit_program

		push word[scoring]
		call printnumforscoring
		
		call write
		call writeAgain
		
		push word [birdcolstart]
		push word[birdrowstart]
		call moveBird

		push word[StartingCol1]      
		push word[EndingRow1]        
		call printRectangle
		
		push word[StartingCol2]      
		push word[EndingRow2]        
		call printRectangle
	
		call Delay
		call Delay
		call Delay
		
		call checkCollision1
		cmp word[collisionflag], 1
		je printCollision1
		
		call checkCollision2
		cmp word[collisionflag], 1
		je printCollision2		
		
		push 0      
		push word[StartingCol1]      
		push word[EndingRow1]  
		call printRectangleforclearing

		push 0      
		push word[StartingCol2]      
		push word[EndingRow2]  
		call printRectangleforclearing


		sub word[StartingCol1], 1
		sub word[EndingCol1], 1
		sub word[endingCol1], 1
		
		sub word[StartingCol2], 1
		sub word[EndingCol2], 1
		sub word[endingCol2], 1
		
		mov ax,  word[StartingCol1]
		add ax, word[pillarWidth]
		cmp ax, 0
		jle updateloop1	
		
		mov ax,  word[StartingCol2]
		add ax, word[pillarWidth]
		cmp ax, 0
		jle updateloop2		
		
		call moveGround
		
		mov ax,word[endingCol1]
		cmp word[birdcolend],ax
		je addscore
		
		mov ax,word[endingCol2]
		cmp word[birdcolend],ax
		je addscore
		
	    popa
	    ret

; ----------------------------------------------------------------------------------------------
; ----------------------------------------------------------------------------------------------

writeAgain:
	pusha
	; PRINTING "SCORE: "
	mov ah, 0x13		; service 13 - print string
	mov al, 1			; subservice 01 – update cursor 
	mov bh, 0			; output on page 0	
	;es:bp = ds:message
	push ds
	pop es				; es=ds segment of string
	
	mov dx, 0x003F 		; index
	mov cx, 7		; length of string
	mov bl, 0x0F	; normal attribute
	mov bp, Time
	INT 0x10			; call BIOS video service	
	popa
	ret

; ----------------------------------------------------------------------------------------------
; ----------------------------------------------------------------------------------------------

write:
	pusha
	; PRINTING "SCORE: "
	mov ah, 0x13		; service 13 - print string
	mov al, 1			; subservice 01 – update cursor 
	mov bh, 0			; output on page 0	
	;es:bp = ds:message
	push ds
	pop es				; es=ds segment of string
	
	mov dx, 0x0002 		; index
	mov cx, 8		; length of string
	mov bl, 0x0F	; normal attribute
	mov bp, score
	INT 0x10			; call BIOS video service
	popa
	ret

; ----------------------------------------------------------------------------------------------
; ----------------------------------------------------------------------------------------------
	
addscore:
	add word[scoring],1
	push word[scoring]
	call printnumforscoring
	jmp ll
	
; ----------------------------------------------------------------------------------------------
; ----------------------------------------------------------------------------------------------
	
timer:		
	push ax

	cmp word [cs:timerflag], 1 ; is the printing flag set
	jne skipall ; no, leave the ISR

	inc word [cs:seconds] ; increment tick count
	push word [cs:seconds]

	call printnumfortimer ; print tick count

skipall:	
	mov al, 0x20
	out 0x20, al ; send EOI to PIC
	pop ax
	iret ; return from interrupt

; ----------------------------------------------------------------------------------------------
; ----------------------------------------------------------------------------------------------

printnumfortimer: 
	push bp
	mov bp, sp
	pusha
	
	mov ax, 0xb800
	mov es, ax
	mov ax, [bp+4]
	mov bx, 10
	mov cx, 0
	
nextdigit: 
	mov dx, 0
	div bx
	add dl, 0x30
	push dx
	inc cx
	cmp ax, 0
	jnz nextdigit
	mov di, 140
	
nextpos: 
	pop dx
	mov dh, 0x0F
	mov [es:di], dx
	add di, 2
	loop nextpos
	
	popa
	pop bp
	ret 2

; ----------------------------------------------------------------------------------------------
; ----------------------------------------------------------------------------------------------

printnumforscoring: 
	push bp
	mov bp, sp
	pusha
	
	mov ax, 0xb800
	mov es, ax
	mov ax, [bp+4]
	mov bx, 10
	mov cx, 0
	
nextdigit1: 
	mov dx, 0
	div bx
	add dl, 0x30
	push dx
	inc cx
	cmp ax, 0
	jnz nextdigit1
	mov di, 20
	
nextpos1: 
	pop dx
	mov dh, 0x0F
	mov [es:di], dx
	add di, 2
	loop nextpos1
	
	popa
	pop bp
	ret 2

; ----------------------------------------------------------------------------------------------
; ----------------------------------------------------------------------------------------------

start:
	call clrscr
	call introscreen
	
pauseformainscreen1:
	call PrintStartScreen
	
pauseformainscreen:	
	mov ah, 0x13		; service 13 - print string
	mov al, 1			; subservice 01 – update cursor 
	mov bh, 0			; output on page 0	
	;es:bp = ds:message
	push ds
	pop es				; es=ds segment of string
	
	mov dx, 0x1203 		; index
	mov cx, 30		; length of string
	mov bl, 0xB0	; normal attribute
	mov bp, defaultmsg
	INT 0x10			; call BIOS video service
	
	
	mov ah, 0x00     ; Wait for a key press
    int 0x16         ; BIOS keyboard interrupt
	mov ah, 0x00     ; Wait for a key press
    int 0x16         ; BIOS keyboard interrupt
	mov ah, 0x00     ; Wait for a key press
    int 0x16         ; BIOS keyboard interrupt
	
llst:
	xor ax, ax
	mov es, ax
	
	mov ax, [es:9*4]
	mov [oldisr], ax
	mov ax, [es:9*4+2]
	mov [oldisr+2], ax
	
	cli
	mov word [es:9*4], kbisr
	mov [es:9*4+2], cs
	sti
	   
    xor ax, ax
	mov es, ax

	cli
	mov word [es:8*4], timer
	mov [es:8*4+2], cs
	sti

; ----------------------------------------------------------------------------------------------
; ----------------------------------------------------------------------------------------------

kk:
	call PrintStartScreen
	
ll:
	call PlayAnimation
	jmp ll

; ----------------------------------------------------------------------------------------------
; ----------------------------------------------------------------------------------------------

endProgram:	
	mov ax, 0x4c00 
	int 0x21

endProgram1:	
	push ds
	pop es
	
		mov ah, 0x13
	mov al, 1
	mov bh, 0
	;es:bp = ds:message
	push ds
	pop es

	; PRINTING 3RD message
	mov dx, 0x0E1F
	mov cx, 18
	mov bl, 01010000B
	mov bp, message3
	INT 0x10
	
	; PRINTING 4th message
	mov dx, 0x101F 		; index
	mov cx, 18			; length of string
	mov bl, 01010000B	; normal attribute
	mov bp, message4
	INT 0x10			; call BIOS video service
	
repeatEndProgram1:	
	in al, 0x60
	cmp al, 0x13
	je RESTARTGAMEAGAIN
	
	cmp al,0x10
	je endProgram
	
	jmp repeatEndProgram1

mov ax, 0x4c00 
int 0x21 
	
; ----------------------------------------------------------------------------------------------
; ----------------------------------------------------------------------------------------------


Delay:	
	push cx
	mov cx, 0xFFFF
Loop1:	
	loop Loop1
	mov cx, 0xFFFF
Loop2:	
	loop Loop2
	pop cx
	ret
	
; ----------------------------------------------------------------------------------------------
; ----------------------------------------------------------------------------------------------

printRectangle:	
	push bp
	mov bp, sp
	pusha

    mov ax, 0xb800
    mov es, ax
	
	; [bp + 4] = ending row 
	; [bp + 6] = starting col
	
	mov word [currentRow], 0	; starting row
	
	mov ax, [bp + 6]			; starting col	
	mov word [currentCol], ax
	
	jmp nextRow
	
update:
	mov ax, word[pillarSpace]			; next starting row for lower pillar
	add word[currentRow], ax
	
nextRow:
    ; Calculate di
    mov ax, word [currentRow]
    mov bx, 80
	mul bx
    add ax, word [currentCol]
    shl ax, 1
    mov di, ax                    ; DI = (row * 80 + col) * 2

	mov cx, word[pillarWidth]
printLine:
    mov word [es:di], 0x6000
    inc word [currentCol]
    add di, 2
    loop printLine

	; start from col 60
	mov ax,  word[bp + 6]
    mov word [currentCol], ax			; restore starting column
    inc word [currentRow]				; update to next row
	
	mov ax, word[bp + 4]
    cmp word [currentRow], ax
	je update
	
    cmp word [currentRow], 20
    jnz nextRow            

finish:
    pop bp
    popa
    ret 4	

; ----------------------------------------------------------------------------------------------
; ----------------------------------------------------------------------------------------------

moveGround:	
	pusha
	push es
	push ds
    
	; ROW 20
	mov ax, 0x19F5
	mov es, ax
	mov di, buffer
	mov ax, 0xb800
	mov ds, ax
	mov si, 3202
	mov cx, 79
	rep movsw

	; Save the first column into buffer1
	mov ax, 0xb800
	mov es, ax
	mov di, 3200
	mov ax, [es:di]
	mov word [buffer1], ax

	; Copy the first column to the last column
	mov ax, 0xb800
	mov es, ax
	mov di, 3358
	mov ax, [buffer1]
	mov word [es:di], ax

	; Move everything else one column back
	mov ax, 0xb800
	mov es, ax
	mov di, 3200
	mov ax, 0x19F5
	mov ds, ax
	mov si, buffer
	mov cx, 79
	rep movsw
	
	; 21 ROW
	; for the second row
	mov ax, 0x19F5
	mov es, ax
	mov di, buffer
	mov ax, 0xb800
	mov ds, ax
	mov si, 3362
	mov cx, 79
	rep movsw

	; Save the first column into buffer1
	mov ax, 0xb800
	mov es, ax
	mov di, 3360
	mov ax, [es:di]
	mov word [buffer1], ax

	; Copy the first column to the last column
	mov ax, 0xb800
	mov es, ax
	mov di, 3518
	mov ax, [buffer1]
	mov word [es:di], ax

	; Move everything else one column back
	mov ax, 0xb800
	mov es, ax
	mov di, 3360
	mov ax, 0x19F5
	mov ds, ax
	mov si, buffer
	mov cx, 79
	rep movsw

	; for the third row
	mov ax, 0x19F5
	mov es, ax
	mov di, buffer
	mov ax, 0xb800
	mov ds, ax
	mov si, 3522
	mov cx, 79
	rep movsw

	; Save the first column into buffer1
	mov ax, 0xb800
	mov es, ax
	mov di, 3520
	mov ax, [es:di]
	mov word [buffer1], ax

	; Copy the first column to the last column
	mov ax, 0xb800
	mov es, ax
	mov di, 3678
	mov ax, [buffer1]
	mov word [es:di], ax

	; Move everything else one column back
	mov ax, 0xb800
	mov es, ax
	mov di, 3520
	mov ax, 0x19F5
	mov ds, ax
	mov si, buffer
	mov cx, 79
	rep movsw
	; for the fourth row
	mov ax, 0x19F5
	mov es, ax
	mov di, buffer
	mov ax, 0xb800
	mov ds, ax
	mov si, 3682
	mov cx, 79
	rep movsw

	; Save the first column into buffer1
	mov ax, 0xb800
	mov es, ax
	mov di, 3680
	mov ax, [es:di]
	mov word [buffer1], ax

	; Copy the first column to the last column
	mov ax, 0xb800
	mov es, ax
	mov di, 3838
	mov ax, [buffer1]
	mov word [es:di], ax

	; Move everything else one column back
	mov ax, 0xb800
	mov es, ax
	mov di, 3680
	mov ax, 0x19F5
	mov ds, ax
	mov si, buffer
	mov cx, 79
	rep movsw


	; for the fifth row
	mov ax, 0x19F5
	mov es, ax
	mov di, buffer
	mov ax, 0xb800
	mov ds, ax
	mov si, 3842
	mov cx, 79
	rep movsw

	; Save the first column into buffer1
	mov ax, 0xb800
	mov es, ax
	mov di, 3840
	mov ax, [es:di]
	mov word [buffer1], ax

	; Copy the first column to the last column
	mov ax, 0xb800
	mov es, ax
	mov di, 3998
	mov ax, [buffer1]
	mov word [es:di], ax

	; Move everything else one column back
	mov ax, 0xb800
	mov es, ax
	mov di, 3840
	mov ax, 0x19F5
	mov ds, ax
	mov si, buffer
	mov cx, 79
	rep movsw
		
	pop ds
	pop es
	popa
	ret

; ----------------------------------------------------------------------------------------------
; ----------------------------------------------------------------------------------------------

printRectangleforclearing:	
	push bp
	mov bp, sp
	pusha

    mov ax, 0xb800
    mov es, ax
	
	; [bp + 4] = ending row 
	; [bp + 6] = starting col
	; [bp + 8] = starting row
	
	mov ax, [bp + 8]
	mov word [currentRow], ax
	
	mov ax, [bp + 6]
	mov word [currentCol], ax
	
	jmp nextRow1
	
update1:
	mov ax, word[pillarSpace]
	add word[currentRow], ax
	
nextRow1:
    ; Calculate di
    mov ax, word [currentRow]
    mov bx, 80
	mul bx
    add ax, word [currentCol]
    shl ax, 1
    mov di, ax

	mov cx, word[pillarWidth]
	
printLine1:
    mov word [es:di], 0x3000
    inc word [currentCol]
    add di, 2
    loop printLine1

	mov ax,  word[bp + 6]
    mov word [currentCol], ax
    inc word [currentRow]
	
	mov ax, word[bp + 4]
    cmp word [currentRow], ax
	je update1
	
    cmp word [currentRow], 20
    jnz nextRow1            	

finish1:
    popa
	pop bp
    ret 6

; ----------------------------------------------------------------------------------------------
; ----------------------------------------------------------------------------------------------
	
background:
    push ax
    push es
    push di

    mov ax, 0xb800
    mov es, ax
    mov di, 0
	
l2:
    mov word [es:di], 0x3000
    add di, 2
    cmp di, 3198
    jle l2 
	
l3:
	mov word [es:di], 0x2078
    add di, 2
	; mov word [es:di], 0x6078
    ; add di, 2
    cmp di, 3358
	jle l3
	
l4:
    mov word [es:di], 0x2000 
    add di, 2
    cmp di, 3998
    jle l4

	mov di, 3760
	mov word [es:di], 0x245E
	mov di, 3602
	mov word [es:di], 0x242F
    mov di, 3598
	mov word [es:di], 0x245C

	; mov cx, 4
	; mov di, 3360
; rowl5:
	; push cx
	; mov cx, 40
; coll5:
	; mov word [es:di], 0x60B0
    ; add di, 4
    ; loop coll5
		
	; add di, 2
	; pop cx
	; loop rowl5

    pop di
    pop es
    pop ax
    ret

; ----------------------------------------------------------------------------------------------
; ----------------------------------------------------------------------------------------------

introscreen:
	pusha
	push es
	push ds
	
	mov ah, 0x13
	mov al, 1
	mov bh, 0
	;es:bp = ds:message
	push ds
	pop es
	
	; PRINTING 1ST MESSAGE
	mov bl, 00100000B
	mov cx, 17
	mov dx, 0x0103
	mov bp, msg1
	INT 0x10

	; PRINTING 2ND MESSAGE
	mov dx, 0x0303
	mov cx, 15
	mov bl, 01010000B
	mov bp, msg2
	INT 0x10
	
	; PRINTING 3RD message
	mov dx, 0x0503
	mov cx, 10
	mov bl, 01010000B
	mov bp, msg3
	INT 0x10
	
	; PRINTING 4TH message
	mov dx, 0x0703
	mov cx, 16
	mov bl, 01010000B
	mov bp, msg4
	INT 0x10
	
	; PRINTING 5TH message
	mov dx, 0x0903
	mov cx, 10
	mov bl, 01010000B
	mov bp, msg5
	INT 0x10
	
	mov dx, 0x0F03
	mov cx, 30
	mov bl, 0x0F
	mov bp, defaultmsg
	INT 0x10
	
	pop ds
	pop es
	popa
	
waitting:
		in al, 0x60 
		cmp al, 0x1C					; PRESS ENTER KEY
		je rulesscreen
		jmp waitting

; ----------------------------------------------------------------------------------------------
; ----------------------------------------------------------------------------------------------

rulesscreen:
	pusha
	push es
	push ds

	call clrscr
	call Delay
	call Delay

	mov ah, 0x13
	mov al, 1
	mov bh, 0
	;es:bp = ds:message
	push ds
	pop es

	; PRINTING 2ND MESSAGE
	mov dx, 0x0303
	mov cx, 25
	mov bl, 01010000B
	mov bp, rule1
	INT 0x10

	call Delay
	call Delay

	; PRINTING 3RD message
	mov dx, 0x0503
	mov cx, 60
	mov bl, 01010000B
	mov bp, rule2
	INT 0x10

	call Delay
	call Delay

	; PRINTING 4th message
	mov dx, 0x0703
	mov cx, 62
	mov bl, 01010000B
	mov bp, rule3
	INT 0x10

	call Delay
	call Delay

	; PRINTING 5th message
	mov dx, 0x0903
	mov cx, 76
	mov bl, 01010000B
	mov bp, rule4
	INT 0x10

	call Delay
	call Delay

	; PRINTING 5th message
	mov dx, 0x0B03
	mov cx, 55
	mov bl, 01010000B
	mov bp, rule5
	INT 0x10

	call Delay
	call Delay

	; PRINTING 5th message
	mov dx, 0x0D03
	mov cx, 59
	mov bl, 01010000B
	mov bp, rule6
	INT 0x10

	; PRINTING 5th message
	mov dx, 0x1003
	mov cx, 30
	mov bl, 0x0F
	mov bp, defaultmsg
	INT 0x10


	pop ds
	pop es
	popa

waiting:
	in al, 0x60 
	cmp al,0x1C				; PRESS ENTER KEY
	je pauseformainscreen1
	jmp waiting

; ----------------------------------------------------------------------------------------------
; ----------------------------------------------------------------------------------------------

printCollision1:
	pusha
	mov word [cs:timerflag], 0

	mov ah, 0x13
	mov al, 1
	mov bh, 0
	;es:bp = ds:message
	push ds
	pop es
	
	; PRINTING 1ST MESSAGE
	mov bl, 00100000B
	mov cx, 18
	mov dx, 0x0C1F
	mov bp, outMessage
	INT 0x10
	popa
	
	mov cx, 25
down:
	mov ax, word [birdcolend]
	cmp ax, word [StartingCol1]  ; Check if bird is in pillar column range
	jle fulldown

	mov ax, word [birdcolstart]
	cmp ax, word [EndingCol1]    ; Check if bird is in pillar column range
	jge fulldown
	
halfdown:
	cmp word[birdrowend], 13
	jge endProgram1
	call Delay
	call Delay
	call Delay
	
	push word [birdcolstart]
	push word [birdrowstart]
	call MoveBirdForClearing
		
	add word [birdrowstart], 1
	add word [birdrowend], 1
	
	push word [birdcolstart]
	push word [birdrowstart]
	call printbird
	loop halfdown
		
fulldown:	
	cmp word[birdrowend], 19
	jge endProgram1

	call Delay
	call Delay
	call Delay
	
	push word [birdcolstart]
	push word [birdrowstart]
	call MoveBirdForClearing
		
	add word [birdrowstart], 1
	add word [birdrowend], 1
	
	push word [birdcolstart]
	push word [birdrowstart]
	call printbird
	loop fulldown


	ret

; ----------------------------------------------------------------------------------------------
; ----------------------------------------------------------------------------------------------

printCollision2:
	pusha
	mov word [cs:timerflag], 0

	mov ah, 0x13
	mov al, 1
	mov bh, 0
	;es:bp = ds:message
	push ds
	pop es
	
	; PRINTING 1ST MESSAGE
	mov bl, 00100000B
	mov cx, 18
	mov dx, 0x0C1F
	mov bp, outMessage
	INT 0x10
	popa
	
	mov cx, 25
down2:
	mov ax, word [birdcolend]
	cmp ax, word [StartingCol2]  ; Check if bird is in pillar column range
	jle fulldown2

	mov ax, word [birdcolstart]
	cmp ax, word [EndingCol2]    ; Check if bird is in pillar column range
	jge fulldown2
	
halfdown2:
	cmp word[birdrowend], 7
	jge endProgram1
	call Delay
	call Delay
	call Delay
	
	push word [birdcolstart]
	push word [birdrowstart]
	call MoveBirdForClearing
		
	add word [birdrowstart], 1
	add word [birdrowend], 1
	
	push word [birdcolstart]
	push word [birdrowstart]
	call printbird
	loop halfdown2
		
fulldown2:	
	cmp word[birdrowend], 19
	jge endProgram1

	call Delay
	call Delay
	call Delay
	
	push word [birdcolstart]
	push word [birdrowstart]
	call MoveBirdForClearing
		
	add word [birdrowstart], 1
	add word [birdrowend], 1
	
	push word [birdcolstart]
	push word [birdrowstart]
	call printbird
	loop fulldown2


	ret

; ----------------------------------------------------------------------------------------------
; ----------------------------------------------------------------------------------------------

checkCollision1: 
    ; Check if bird is within pillar horizontally
    mov ax, word [birdcolstart]
    cmp ax, word [EndingCol1]
    jg noCollision ; If bird is completely to the right, no collision
    
    mov ax, word [birdcolend]
    cmp ax, word [StartingCol1]
    jl noCollision ; If bird is completely to the left, no collision

    ; Check if bird is within pillar vertically
    mov ax, word [birdrowstart]
    cmp ax, word [EndingRow1]
    jg checkPillarSpace1 ; Check lower pillar space if above top pillar

    mov ax, word [birdcolend]
    cmp ax, word [StartingCol1]
    je setCollision ; Collision with the top pillar


	mov ax, word [birdrowstart]
    cmp ax, word [EndingRow1]
    je setCollision

checkPillarSpace1:
    ; Check for collision with the lower pillar (space included)
    mov ax, word [EndingRow1]
    add ax, word [pillarSpace]  ; Lower pillar starts after pillar space
    cmp word [birdrowend], ax
    jl noCollision ; Bird is above the lower pillar, no collision
    jmp setCollision
	
setCollision:
    mov word [collisionflag], 1
    ret

noCollision:
    mov word [collisionflag], 0
    ret
; ----------------------------------------------------------------------------------------------
; ----------------------------------------------------------------------------------------------

checkCollision2: 
    ; Check if bird is within pillar horizontally
    mov ax, word [birdcolstart]
    cmp ax, word [EndingCol2]
    jg noCollision ; If bird is completely to the right, no collision
    
    mov ax, word [birdcolend]
    cmp ax, word [StartingCol2]
    jl noCollision ; If bird is completely to the left, no collision

    ; Check if bird is within pillar vertically
    mov ax, word [birdrowstart]
    cmp ax, word [EndingRow2]
    jg checkPillarSpace2 ; Check lower pillar space if above top pillar

    mov ax, word [birdcolend]
    cmp ax, word [StartingCol2]
    je setCollision ; Collision with the top pillar


	mov ax, word [birdrowstart]
    cmp ax, word [EndingRow2]
    je setCollision

checkPillarSpace2:
    ; Check for collision with the lower pillar (space included)
    mov ax, word [EndingRow2]
    add ax, word [pillarSpace]  ; Lower pillar starts after pillar space
    cmp word [birdrowend], ax
    jl noCollision ; Bird is above the lower pillar, no collision
    jmp setCollision
	