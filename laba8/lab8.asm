.model tiny
.286
.code 
org 100h
start:
    jmp setResident

    NoArgsException db "Usage [Hours] [Minutes] [Delay]$"
    HoursBuffer db 8 dup('$')
    MinutesBuffer db 8 dup('$')
    DelayBuffer db 24 dup('$')
    alarm db " Alarm clock!" 
    errorOnSet db  "Error on set realtime clock$"
    Hours db 0
    Minutes db 0
    Delay dw 0
    old_interrupt dd 0


puts proc far
    mov     ah, 9
    int     21h
    ret
endp
exit proc far 
    mov ax, 4c00h
    int 21h
endp

SetUpAlarm proc far
    pusha
    mov ah, 07h
    int 1ah
    
    mov ch, Hours
    mov cl, Minutes
    mov dh, 00000000b
    mov ah, 06h
    int 1ah

    jc _error
    jmp __exit
    _error:
        mov dx, offset errorOnSet
        call puts
        call exit
    __exit:
    popa
    ret
endp

atoi proc far   ;si - source, di - target
    pusha
    xor     bx, bx
    xor     ax, ax
    start_converting:
        cmp byte ptr[si], '0'
        jb _exit_atoi
        cmp byte ptr[si], '9'
        jg _exit_atoi
        mov     bl, 10
        mul     bx 
        mov     bl, byte ptr [si]
        sub     bl, '0'
        add     ax, bx
        inc     si
        jmp     start_converting
    _exit_atoi:
    mov word ptr[di], ax
    popa
    ret
atoi endp
enable_sound proc near          
    push ax
    push bx
    push dx
    mov bx,ax
    in al,61h
    or al,3
    out 61h,al
    mov al,00001011b
    pop dx
    pop bx
    pop ax
    ret    
enable_sound endp
disable_sound proc near
    push ax
    in al,61h
    mov al,0
    out 61h,al
    pop ax
    ret
disable_sound endp
alarm_clock proc far
    push ds
    push cs
    pop ds
    pusha
    
    pusha
    lea dx, NoArgsException
    call puts
    ;call exit
    popa

    ;mov ax,@data
    ;mov ds,ax 
    pushf                 
    call dword ptr cs:old_interrupt        
    ;popf 
    ;;;;main part of procedure
    ;interrupt handler
    ;setting the tonality of the sound
    mov al, 11110000b     
    out 42h, al
    mov al, 00000101b
    out 42h, al
      
    ;duration of the sound
    call enable_sound
    mov bx, Delay
    timer_cycle:
        xor cx,cx
        mov dx,500
        mov ah,86h
        int 15h
        dec bx
        cmp bx,0
        jne timer_cycle
        call disable_sound 

        mov ah,03h
        mov bh,0
        int 10h 
        xor si,si
        mov dl,-1
    PrintAll:
        mov ah,0ah
        mov al,alarm[si]
        mov cx,1
        int 10h
        inc si
        mov ah,02h
        mov bh,0
        inc dl
        int 10h
    cmp si,12
    jne PrintAll
    
    popa
    pop ds       
    iret     
alarm_clock endp
setNewInterraptHandler proc far
    cli
    mov ah,35h ;saving old interrupt
    mov al,4ah
    int 21h
    mov word ptr cs:old_interrupt,bx
    mov word ptr cs:old_interrupt+2,es
    
    mov ah,25h ;set new interrupt handler
    mov al,4ah  ;4ah
    mov dx, offset cs:alarm_clock
    int 21h
    sti
    ret
endp
 
ConvertTimeAndDelay proc far
    pusha
    pusha
        lea dx, HoursBuffer
        call puts

        lea dx, MinutesBuffer
        call puts

        lea dx, DelayBuffer
        call puts
    popa







    mov si, offset HoursBuffer
    mov di, offset Hours
    call atoi
    mov si, offset MinutesBuffer
    mov di, offset Minutes
    call atoi
    mov si, offset DelayBuffer
    mov di, offset Delay
    call atoi
    convert_to_bcd:
        xor ax, ax
        mov al, Hours
        mov bl, 10
        div bl  
        mov ch, al
        mov cl, ah
        xor ax, ax
        mov al, ch
        mov bl, 16
        mul bl  
        add al, cl
        mov Hours,al
        xor ax, ax
        mov al, Minutes
        mov bl, 10
        div bl  
        mov ch, al
        mov cl, ah
        xor ax, ax
        mov al, ch
        mov bl, 16
        mul bl  
        add al, cl
        mov Minutes,  al
    popa
    ret
endp
getTimeAndDelay proc far
    pusha

    xor cx, cx
    xor di, di
    mov si, 82h
    mov bl, es:[80h]
    add bx, 80h

    cmp si, bx
    ja  NoArgsExc

    mov di, offset HoursBuffer

    getHours:
        mov     al, es:[si]
        mov     [di], al

        inc     di
        inc     si
;;;;;;;;;;;;;LOG;;;;;;;;;;;;;;;;;
        ;push    ax
        ;lea     dx, log_zero
        ;call    puts
        ;pop     ax
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        cmp byte ptr es:[si], byte ptr '$'
        je NoArgsExc

        cmp byte ptr es:[si], byte ptr ' '
        jne getHours

    inc si
    mov di, offset MinutesBuffer
    getMinutes:
        mov     al, es:[si]
        mov     [di], al

        inc     di
        inc     si
;;;;;;;;;;;;;LOG;;;;;;;;;;;;;;;;;
        ;push    ax
        ;lea     dx, log_zero
        ;call    puts
        ;pop     ax
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        cmp byte ptr es:[si], byte ptr '$'
        je NoArgsExc

        cmp byte ptr es:[si], byte ptr ' '
        jne getMinutes
    
    inc si
    mov di, offset DelayBuffer
    getDelay:
         mov     al, es:[si]
        mov     [di], al

        inc     di
        inc     si
;;;;;;;;;;;;;LOG;;;;;;;;;;;;;;;;;
        ;push    ax
        ;lea     dx, log_zero
        ;call    puts
        ;pop     ax
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        cmp byte ptr es:[si], byte ptr 0dh
        jne getDelay
  popa
  ret
endp

NoArgsExc:
    lea     dx, NoArgsException
    call    puts
    call    exit
setResident:
    call getTimeAndDelay
    call ConvertTimeAndDelay
    call SetUpAlarm
    call setNewInterraptHandler
    mov  dx, offset setResident
    int  27h
end start