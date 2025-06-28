[org 0x100]
    jmp start
oldIsr: dd 0x0              
playerChar: equ 0x4050          ;0x2450              ;0x0d50     ;0x50 is ascii of 'P'     
winPos: equ 2878
wall: equ 0x70DB                ;0x04DB
path: equ 0x702e
playerPos: dw 320

start:
    call draw_maze
    call draw_intials
    call set_Isr
    infloop:    
        mov ah, 0
        int 0x16            ; get keystroke in al

        cmp al, 27          ; cmp it with escape key code           ;  ;0x01        ; 0x11B ??? 
        jne infloop
        ; restore oldisr at its position (unhookin interrupt)
        push 0
        pop es
        mov ax, [oldIsr]
        mov bx, [oldIsr+2]
        cli
        mov [es:9*4], ax
        mov [es:9*4+2], bx
        sti
        ;terminate prog
        mov ax, 0x4c00
        int 0x21
myisr:
    pusha
    mov dx, 0x60                ; to read word 
    in ax, dx                   ; read from keyboard
    push 0xb800
    pop es
    up:
    cmp ax, 0x3048
    jne down
    push handle_up
    call handle_cursor
    jmp checkWinStatus

    down:
    cmp ax, 0x3050
    jne left
    push handle_down
    call handle_cursor

    left:
    cmp ax, 0x304b
    jne right
    push handle_left
    call handle_cursor

    right:
    cmp ax, 0x304d
    jne isR
    push handle_right
    call handle_cursor

    isR:
    cmp ax, 0x0013
    jne checkWinStatus
    call reset

    checkWinStatus:
    mov ax, winPos
    cmp ax, [playerPos]
    jne exit
    ;if won:
    call clrscr
    call win_display

    exit:
    popa
    jmp far [cs:oldIsr]          ; giving control to org ISR (it will clean up stack so no need of iret) 

handle_cursor:
    ; pushes address of func to call
    push bp
    mov bp, sp
    pusha

    mov bx, [playerPos]
    call word [bp+4]          ;only chnages bx
    push 0x0
    push bx
    call is_out_of_bounds
    pop cx
    cmp cx, 1           ;is out of bounds true
    je upexit

    push 0xb800
    pop es
    ; is new pos hitting wall
    cmp word [es:bx], wall
    je upexit 

    ;not out of bounds
    mov di, [playerPos]
    mov word [es:di], path               ; prev char will always be path duhh             ;prevChar    ; restor prev char at playerpos whixh isnt updated yyet
    mov word [es:bx], playerChar         ; move player to new place (bx)
    mov word [playerPos], bx             ; update player pos

    upexit:
    popa
    mov sp, bp
    pop bp
    ret 2
handle_up:
    sub bx, 160
    ret
handle_down:
    add bx, 160
    ret
handle_left:
    sub bx, 2
    ret 
handle_right:
    add bx, 2
    ret
reset:
    mov word [playerPos], 320
    call draw_maze
    call draw_intials
    ret
win_display:
    push es
    push 0xB800
    pop es
    mov byte [es:0], 'W'
    mov byte [es:1], 0xFA
    mov byte [es:2], 'O'
    mov byte [es:3], 0xFA
    mov byte [es:4], 'N'
    mov byte [es:5], 0xFA
    mov byte [es:6], '!'
    mov byte [es:7], 0xFA
    pop es
    ret
clrscr: 
    push ax
    push es
    push di

    mov ax, 0xb800
    mov es, ax
    mov di, 0                     ;location indexer

    nextposition:
        mov word[es:di], 0x0720   ;black ;space  character
        add di, 2                 ;next cell
        cmp di, 4000              ;total cells - 80*25= 2000 (2 byte cells) so 4000
        jnz nextposition

    pop di
    pop es
    pop ax
    ret
is_out_of_bounds:
    ; recieves res and location
    push bp
    mov bp, sp
    cmp word [bp+4], 0
    jl exit_out_of_bound
    cmp word [bp+4], 4000
    jg exit_out_of_bound

    exit_within_bounds:
    mov word [bp+6], 0x0        ;true ==> boundary hi hai paeen
    jmp b1
    exit_out_of_bound:
        mov word [bp+6], 0x1
    b1:
    mov sp, bp
    pop bp
    ret 2               ; not 4 cause parameter return krna paeen

set_Isr:
    push es
    push ax
    push bx
    xor ax, ax
    mov es, ax
    mov ax, [es:9*4]
    mov [oldIsr], ax
    mov ax, [es:9*4+2]
    mov [oldIsr+2], ax          ; oldisr saved
    cli                         ; disabling interrupts to avoid program crash 
    mov word [es:9*4], myisr
    mov word [es:9*4+2], cs
    sti                         ; interrupts enabled agaim
    pop bx
    pop ax
    pop es
    ret
draw_intials:
    push es
    push 0xb800
    pop es
    mov di, [playerPos]
    mov word [es:di], playerChar               ;0x0250    ;23          ;5e      ;3c    ;3e
    mov word [es:winPos], 0x02db        ;57 ais ascii of W
    pop es
    ret
draw_maze:
    pusha
    ; using bits
    push 0xb800
    pop es
    mov cx, 125             ; totak 125 words
    xor di, di
    xor si, si
    outerloop:
        push cx
        ; deal with onw word
        mov cx, 16
        mov dx, word [maze+di]
        CLC
        here:
            shl dx, 1
            jc draw_wall                    ;if 1, draw wall.        else ==> path
            draw_path:
                mov word [es:si], path          
                jmp updates
            draw_wall:
                mov word [es:si], wall         
        updates:
            add si, 2
            loop here

        pop cx
        add di, 2          ;move to next word
        loop outerloop
    popa
    ret
maze:
    dw 1110000111011000b, 0101110011101011b, 1101100011111100b, 0111000110111110b, 1111011011110111b,
    dw 1110110001011000b, 0000000001101011b, 1101101000111101b, 0101111110111101b, 1000011011110111b,
    dw 0000111110011011b, 1011111101000000b, 1111111110111001b, 1111011110000000b, 1111000000110110b,
    dw 1110110110001010b, 0000111101011110b, 1101100010100000b, 1111010001111110b, 1111011110000110b,
    dw 0010111110101010b, 1110111101011000b, 1111101010101110b, 1111000100000110b, 1100011110111110b,
    dw 1010000000000010b, 1110111100010011b, 0000000000101110b, 1111101110111110b, 1111011110000000b,
    dw 1011111011111111b, 1010001111111011b, 1101011111111110b, 1111101110111110b, 0000001111011111b,
    dw 1110111011111010b, 1010111011111110b, 1110111111011101b, 1100001110010111b, 1011000001111110b,
    dw 1000111011011000b, 1010000011111000b, 1100011110000000b, 0011101110110000b, 1011101011110001b,
    dw 1110111000000000b, 0010111011000000b, 1111111110111001b, 1101111111000000b, 0111101111100011b,
    dw 1000000001110111b, 1111111011011111b, 1011011000101000b, 0011110111011011b, 1011101111000111b,
    dw 1011111100110110b, 0000000011011000b, 0001111101010101b, 1100011111011000b, 0111110111011011b,
    dw 0011111000110111b, 0111011010011011b, 0111000000000001b, 0111000100011111b, 0110000110001011b,
    dw 1011000000110001b, 0111011010100011b, 0111101111101111b, 0111010101111010b, 0000010000011100b,
    dw 1000011011000101b, 0000000000011111b, 0111101111101111b, 0111010100000000b, 1001010000011111b,
    dw 1111011111010101b, 0111000111110001b, 0111100010001110b, 0000110110111111b, 1101101111010111b,
    dw 0010000000000000b, 0111011111110101b, 0001111010111011b, 1110110000100001b, 1100001101000111b,
    dw 1010110111110110b, 1111000000110101b, 1011000000011110b, 0000000001101101b, 1111110101100000b,
    dw 1010100111110110b, 0100010100000101b, 1101011111011110b, 0011101100101101b, 0100000010101111b,
    dw 1010101111100001b, 0010001111101001b, 1010101010000000b, 1111111101101100b, 0111010000100011b,
    dw 0000100001111101b, 1101111101111101b, 0000000100010111b, 0111111000001011b, 0001010000011000b,
    dw 1101111101110111b, 0111110001000101b, 1011101111011111b, 0000101101011101b, 0101011101100011b,
    dw 1100001100010100b, 0001110101010101b, 1011101111010111b, 0110100000000000b, 0001011100000011b,
    dw 1101111101010101b, 1100010100010000b, 0011100000011111b, 0110111101111111b, 1100011000111000b,
    dw 1100011100000010b, 1110111011111111b, 1111110111101011b, 1011011110111111b, 1111000011000111b
