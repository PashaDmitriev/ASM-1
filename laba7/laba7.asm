.286
.model  small
.stack  100h

empty_segment SEGMENT
empty_segment ENDS


.data
    NoArgsException     db "Usage [fileName] [NumberOfStarts]", 0Dh, 0Ah, '$'
    ;fileName            db "C:\test.txt", 0
    fileName            db 256 dup(0)
    tempString          db 256 dup('$')    
    tempVar             db 256 dup('$')    
    NumberOfStarts      dw 0
    log_zero            db "0", 0Dh, 0Ah, '$'
    log_non_zero        db "1", 0Dh, 0Ah, '$'
    endl                db 13, 10, '$'
    spacePos            dw 0
    base                dw 10
    parsingStep         dw 1
    negativeExit        db "Enter correct number!", 0Dh, 0Ah, '$'
    memoryError         db "Memory allocatingMemory", 0Dh, 0Ah, '$'
    fileNotFound        db "File not found", 0Dh, 0Ah, '$'
    badPrivilege        db "Access is denied for this file", 0Dh, 0Ah, '$'
    memoryAllocation    db "Memory allocation failed", 0Dh, 0Ah, '$'
    wrongEnviroment     db "Wrong environment", 0Dh, 0Ah, '$'
    wrongFormat         db "Wrong file format", 0Dh, 0Ah, '$'
    EPB                 dw 0000
                        dw offset commandLine, 0
                        dw 005Ch, 0, 006Ch, 0
    commandLine         db 125
                        db "/?"
    command_text        db 122 dup(?)
    
.code

puts proc
    mov     ah, 9
    int     21h
    ret
endp

exit proc
    mov     ax, 4c00h
    int     21h
endp
 
reverse_string proc
    pusha
    xor bx, bx
    xor ax, ax
    mov di, si
    searching_end_string:
        cmp byte ptr[si], '$'
        je end_string
        inc si
        jmp searching_end_string
    end_string:
        dec si
    swap_loop:
        cmp si, di
        jle end_swap_loop
        mov al, byte ptr[si]
        mov bl, byte ptr[di]
        xchg ax, bx
        mov byte ptr[si], al
        mov byte ptr[di], bl
        dec si
        inc di
        jmp swap_loop
    end_swap_loop:
    _exit_reverse_string:
        popa
        ret
reverse_string endp

itoa proc
    pusha
    xor bx, bx
    mov di, si
    _outer_loop_:
       mov bx, 10
       xor dx, dx
       div bx
       add dx, '0'
       mov byte ptr[si], dl
       inc si 
       cmp ax, 0
       je _ret_itoa
       jmp _outer_loop_
    _ret_itoa:
        mov byte ptr[si], '$'
        mov si, di
        call reverse_string       
    	popa
    	ret
itoa endp

atoi proc    ;si - source, di - target
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



getPathAndNumberOfStarts proc
    pusha
    getFileName:
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
        jne getFileName
    convertToASCIIZ:
        mov byte ptr [di], '$'
    skipSpace:
        inc si
    
    mov di, offset tempString
    getNumberOfStarts:
        cmp byte ptr es:[si], '0'
        jb wrongNumbeerException
        cmp byte ptr es:[si], '9'
        jg wrongNumbeerException

        mov     al, es:[si]
        mov     [di], al

        inc     di
        inc     si

        cmp byte ptr es:[si], byte ptr 0dh
        jne getNumberOfStarts
;;;;;;;;;;;;;LOG;;;;;;;;;;;;;;;;;
    ;push    ax
    ;lea     dx, log_non_zero
    ;call    puts
    ;pop     ax
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;cmp byte ptr es:[si], byte ptr '$'
    ;jne NoArgsExc

    popa
    ret
endp

wrongNumbeerException:
    lea     dx, negativeExit
    call    puts
    call    exit
NoArgsExc:
    lea     dx, NoArgsException
    call    puts
    call    exit

MemoryAllocException:
    lea     dx, memoryError  
    call    puts
    call    exit

start proc
    mov ax,es
    mov bx,empty_segment               
    sub bx,ax

    mov ax, 4a00h
    int 21h
    jc MemoryAllocException

    mov     ax, @data
    mov     ds, ax

    mov     bl, es:[80h]        ;size of args
    add     bx, 80h 
    mov     si, 82h
    mov     di, offset fileName 

    cmp     si, bx
    ja      NoArgsExc

    call    getPathAndNumberOfStarts
    toInteger:
        push    si
        push    di
        mov     si, offset tempString
        mov     di, offset NumberOfStarts
        call    atoi 
        pop     di
        pop     si
    checkInput:
        cmp     word ptr [NumberOfStarts], 0    
        jle     wrongNumbeerException
        
        cmp     word ptr [NumberOfStarts], 255
        jg      wrongNumbeerException 

    mov ax,cs
    mov word ptr EPB+4,ax
    mov word ptr EPB+8,ax
    mov word ptr EPB+0Ch,ax
    
    mov     cx, NumberOfStarts
    runProgramm:
        mov     ax, 4b00h
        mov     dx, offset fileName
        mov     bx, offset EPB
        int     21h
        jc _error

    loop runProgramm
    call    exit
    _error:
        cmp ax, 02h
        call fileNotFoundException
        cmp ax, 05h
        call badPrivilegeException
        cmp ax, 08h
        call memoryAllocationException
        cmp ax, 0Ah
        call wrongEnviromentException
        cmp ax, 0Bh
        call wrongFormatException
        ;mov si, offset tempVar
        ;call itoa
        ;mov dx, offset tempVar
        ;call puts
        ;call exit   
    
;;;;;;;;;;;;;;;;;;;;;;;;;LOGS;;;;;;;;;;;;;;;;;;;;;;;;;
    ;push    dx
    ;lea     dx, fileName
    ;call    puts
    ;pop     dx
    ;push    dx
    ;lea     dx, tempString
    ;call    puts
    ;call    exit
    ;pop     dx
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

endp  
fileNotFoundException proc
    lea     dx, fileNotFound
    call    puts
    call    exit
endp
badPrivilegeException proc
    lea     dx, badPrivilege
    call    puts
    call    exit
endp
memoryAllocationException proc
    lea     dx, memoryAllocation
    call    puts
    call    exit
endp
wrongEnviromentException proc
    lea     dx, wrongEnviroment
    call    puts
    call    exit
endp
wrongFormatException proc
    lea     dx, wrongFormat
    call    puts
    call    exit
endp
end start