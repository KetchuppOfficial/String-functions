;--------------------------------------------------------------------
;                           DOCUMENTATION
;--------------------------------------------------------------------
;strlen:
;    Input: stack
;    Output: AX
;    Registers that change values:
;       DI - offset of the string
;       AL - contains end of line symbol
;--------------------------------------------------------------------
;itoa:
;   Input: stack
;   Output: AX
;   Registers that change values:
;       BX - offset of the string
;       CX - is used to put values from one part of RAM to another
;       DI - radix
;       DX - constains the remainder of division after using mul ()
;           than contains offset of the string
;       SI - constains the remainder of 
;--------------------------------------------------------------------
;strchr:
;   Input: stack
;   Output: AX
;   Registers that change values:
;       AH - is used to contain EOL-symbol
;       DI - offset of the string
;       AL - symbol
;--------------------------------------------------------------------
;strncmp:
;   Input: stack
;   Output: AX
;   Registers that change values:
;       SI - offest of the first string
;       BX - offset of the second string
;       CX - symbols counter
;       DL, DH - are used to compare symbols
;--------------------------------------------------------------------
;strcpy:
;   Input: stack
;   Output: AX
;   Registers that change values:
;       BX - destinatuion string
;       SI - source string
;       DL - is used to put values from one part of RAM to another
;--------------------------------------------------------------------
;atoi:
;   Input: stack
;   Output: AX
;   Registers that change values:
;       BX - offset of the string
;       CH - is used to perform math
;       DH - is used to compare symbols
;       DL - flag that shows if the number is negative
;--------------------------------------------------------------------

.model tiny
.code
org 100h

zero_symb   equ 30h
nine_symb   equ 39h

symbol      equ 006Ch  ; 'l'

n_symbs     equ 0009h

CMD_LINE    equ 0081h
EOL         equ 0Dh
zero        equ 0000h
base        equ 000Ah

Start: jmp main
locals @@

;--------------------------------------------------------------------
strlen      proc

            push bp
            mov bp, sp

            mov al, 0Dh
            mov di, [bp + 4]
            mov bx, di
@@while:
            scasb
            jnz @@while

            sub di, bx
            mov ax, di
            dec ax

            pop bp
            ret

strlen      endp
;--------------------------------------------------------------------

;--------------------------------------------------------------------
itoa        proc

num         equ [bp + 8]
str         equ [bp + 6]
radix       equ [bp + 4]

            push bp
            mov bp, sp

            mov ax, num
            mov bx, str
            mov di, radix

            cmp ax, 0000h
            je  @@zero

            cmp ax, 0000h
            jb  @@negative

            jmp @@positive

@@zero:     mov ch, numbers[0]
            mov [bx], ch
            inc bx

            mov ch, dollar
            mov [bx], ch

            jmp @@return

@@negative: 
            mov ch, minus
            mov [bx], ch
            inc bx

            neg ax

@@positive: 
            xor dx, dx
            div di
            mov si, dx
            mov ch, numbers[si]
            mov [bx], ch
            xor si, si
            inc bx

            cmp ax, 0000h
            jne @@positive
            
            mov ch, dollar
            mov [bx], ch

            mov di, str
@@change:
            dec bx
            mov ch, [bx]

            mov ah, [di]
            mov [bx], ah

            mov [di], ch
            inc di

            cmp bx, di
            ja @@change
        
@@return:
            mov ax, str
            
            pop bp
            ret

dollar      db '$'
minus       db '-'

itoa        endp
;--------------------------------------------------------------------


;--------------------------------------------------------------------
strchr  proc

str     equ [bp + 6]
symb    equ [bp + 4]

        push bp
        mov bp, sp

        mov di, str
        mov al, symb
        mov ah, EOL
        
@@while:
        cmp [di], ah
        je @@check_return
        scasb
        jnz @@while
        jmp @@return 

@@check_return:
        cmp al, ah
        je @@return
        xor ax, ax
        jmp @@exit

@@return:
        dec di
        mov ax, di
@@exit:
        pop bp
        ret

strchr  endp
;--------------------------------------------------------------------

;--------------------------------------------------------------------
strncmp proc

str1    equ [bp + 8]
str2    equ [bp + 6]
len     equ [bp + 4]

        push bp
        mov bp, sp

        mov si, str1
        mov bx, str2

        xor cx, cx

        mov dl, EOL
        cmp [si], dl
        je @@after_while

        cmp [bx], dl
        je @@after_while

        cmp cx, len
        jge @@after_while

@@circle:
        mov dl, [si]
        mov dh, [bx]
        cmp dl, dh
        jl @@less
        ja @@greater

        inc si
        inc bx
        inc cx
        
        mov dl, EOL
        cmp [si], dl
        je @@after_while

        cmp [bx], dl
        je @@after_while

        cmp cx, len
        jge @@after_while

        jmp @@circle

@@after_while:

        mov dl, EOL
        cmp [si], dl
        je @@fisrt_zero

        mov dl, EOL
        cmp [bx], dl
        je @@greater

        jmp @@equal

@@fisrt_zero:
        mov dl, EOL
        cmp [bx], dl
        je @@equal

@@less:
        mov ax, 0001h
        neg ax
        jmp @@return

@@greater:
        mov ax, 0001h
        jmp @@return

@@equal:
        mov ax, 0000h
        jmp @@return

@@return:
        pop bp
        ret

strncmp endp
;--------------------------------------------------------------------

;--------------------------------------------------------------------
strcpy  proc

dest_str    equ [bp + 6]
src_str     equ [bp + 4]

            push bp
            mov bp, sp

            mov bx, dest_str
            mov si, src_str

            mov dl, EOL
            cmp [si], dl
            je @@after_while

@@while:
            mov dl, [si]
            mov [bx], dl
            inc bx
            inc si

            mov dl, EOL
            cmp [si], dl
            jne @@while

@@after_while:
            mov dl, dollar
            mov [bx], dl

            mov ax, dest_str
            pop bp
            ret

strcpy  endp
;--------------------------------------------------------------------

;--------------------------------------------------------------------
atoi    proc

str     equ [bp + 6]
basis   equ [bp + 4]

        push bp
        mov bp, sp

        mov bx, str

        xor ax, ax
        xor dl, dl
        
        dec bx
@@while:
        inc bx

        mov dh, zero_symb
        cmp [bx], dh
        jge @@then
        jmp @@not_digit

@@then:
        mov dh, nine_symb
        cmp [bx], dh
        jle @@after_while

@@not_digit:
        mov dh, minus
        cmp [bx], dh
        je @@after_while

        xor dh, dh
        cmp [bx], dh
        je @@after_while

        jmp @@while

@@after_while:

        mov dh, minus
        cmp [bx], dh
        jne @@positive

        mov dl, 01h
        inc bx

@@positive:

        mov dh, zero_symb
        cmp [bx], dh
        jl @@bad_return

        mov dh, nine_symb
        cmp [bx], dh
        jg @@bad_return

@@process_num:

        mov cl, [bx]
        sub cl, zero_symb
        xor dx, dx
        mul word ptr basis
        add ax, cx

        inc bx

        mov dh, zero_symb
        cmp [bx], dh
        jl @@good_return

        mov dh, nine_symb
        cmp [bx], dh
        jg @@good_return

        jmp @@process_num

@@good_return:        
        xor dh, dh
        cmp dl, dh
        je @@return

        neg ax
        jmp @@return

@@bad_return:
        xor ax, ax
        jmp @@return

@@return:
        pop bp
        ret

atoi    endp
;--------------------------------------------------------------------

;--------------------------------------------------------------------
main:
    if 0
        ;CALLING strlen ()
        push CMD_LINE
        call strlen
        pop cx

        cmp ax, zero
        je @@no_dec
        dec ax
    endif

    if 0
        ;CALLING strncmp ()
        push offset string_1
        push offset string_2
        push n_symbs
        call strncmp
        pop cx
        pop cx
        pop cx
    endif

    if 1
        ;CALLING strchr ()
        push CMD_LINE
        push symbol
        call strchr
        pop cx
        pop cx
    endif

    if 0
        ;CALLING strcpy ()
        push offset empty_string
        push offset string_1
        call strcpy
        pop cx
        pop cx

        mov dx, ax
        mov ax, 0900h
        int 21h
    endif

    if 0
        ;CALLING atoi ()
        push offset atoi_str
        push base
        call atoi
        pop cx
        pop cx
    endif

@@no_dec:
    if 1
        ;CALLING itoa ()
        push ax
        push offset num_string
        push base
        call itoa
        pop cx 
        pop cx
        pop cx

        mov dx, ax
        mov ax, 0900h
        int 21h
    endif

        mov ax, 4C00h
        int 21h

;--------------------------------------------------------------------
numbers         db "0123456789ABCDEF"
num_string      db 32 dup (0)

string_1        db "Hello, World", 0Dh, '$'
string_2        db "Hello, my friend", 0Dh
empty_string    db 32 dup (0)

atoi_str        db "hj1234njkj", 0Dh
;--------------------------------------------------------------------

end     Start
