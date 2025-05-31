org 100h

start:
    ; Print prompt
    mov     ah, 9
    lea     dx, prompt
    int     21h

    ; Read up to 6 chars (0Ah) into inbuf
    lea     dx, inbuf
    mov     ah, 0Ah
    int     21h

    lea     dx, newline
    mov     ah, 09h
    int     21h
    
    ; Convert ASCII string in inbuf to signed 16-bit in AX
    call    str_to_num
    ; Now AX = user-entered value (signed). 

    ; If AX == 0, exit
    cmp     ax, 0
    je      done

    ; Copy AX -> BX (we'll test bits in BX)
    mov     bx, ax

    ; Prepare mask in CX = 8000h (bit 15)
    mov     cx, 08000h

print_bits_loop:
    ; Test next bit: if (BX & CX) != 0 to bit is 1 else 0
    mov     dx, bx
    and     dx, cx
    jz      print_zero
    ; Bit is 1
    mov     dl, '1'
    jmp     print_one_or_zero

print_zero:
    mov     dl, '0'

print_one_or_zero:
    mov     ah, 2
    int     21h

    ; Shift mask right one bit for next iteration
    shr     cx, 1
    ; If CX ? 0, keep looping (16 bits total)
    cmp     cx, 0
    jne     print_bits_loop

    ; After 16 bits, print CR+LF
    mov     ah, 2
    mov     dl, 0Dh
    int     21h
    mov     dl, 0Ah
    int     21h

    ; Loop back to prompt for next decimal
    jmp     start


;   Converts ASCII decimal string in inbuf to signed 16-bit in AX.
;   inbuf[1] = character count; inbuf[2..] = actual chars.
;   Returns: AX = signed integer (two's complement).

str_to_num:
    push    bp
    mov     bp, sp
    ; We'll use [BPÅE] as a scratch byte to hold Negative Flag
    
    ; Initialize AX = 0
    xor     ax, ax

    ; Point SI at first character in buffer (inbuf+2)
    lea     si, [inbuf + 2]

    ; Check if first char is 'i'. If so, mark negative and skip it.
    mov     bl, [si]
    cmp     bl, '-'
    jne     just_digits
    ; if it is negative
    mov     byte ptr [bp - 1], 1   ; isNegative = 1
    inc     si                     ; skip

just_digits:
    mov     byte ptr [bp - 1], 0   ; isNegative = 0

digits_loop_start:
    ; Load how many chars remain (inbuf+1) to CL
    mov     cl, [inbuf + 1]
    cmp     cl, 0
    je      done_str_to_num           ; if count = 0, treat as zero

convert_loop:
    ; AL = ASCII code of next digit
    mov     al, [si]
    ; Multiply current AX by 10
    mov     dx, ax
    mov     ax, dx
    shl     ax, 1
    shl     dx, 1
    add     dx, ax      ; dx = oldAX * 2
    mov     ax, dx
    shl     dx, 2
    add     ax, dx      ; AX = oldAX * 10

    ; Convert ASCII digit to numeric (AL - '0')
    sub     al, '0'
    ; Add that digit into AX
    add     ax, ax      ; temporarily aligned in AL
    adc     ah, 0       ; propagate carry if any

    ; Advance to next character
    inc     si
    dec     cl
    jnz     convert_loop

done_str_to_num:
    ; If negative flag set, negate AX
    cmp     byte ptr [bp - 1], 0
    je      no_negate
    neg     ax

no_negate:
    pop     bp
    ret

done:
    mov ax, 4C00h
    int 21h        

; data definition
prompt      db  'Enter decimal (Enter with empty spaces to quit): $'
newline     db  0Dh, 0Ah, '$'
inbuf       db  6, 0, 6 dup(0)