.model tiny  
.code
.386 
org 100h  

start: 
    jmp main

cmdString		db 80 dup('$') 
cmd_len			db 0
num			dw 0
counter			dw 0 
delay 			dw 0  
symbol			db 003h, 00000100b
   
screen			dw 2000 dup('$')
emptyLine 		db 13, 10, 'No parametrs in cmd$'
tooMany                 db 0Ah, 0Dh, 'Too many arguments.You should input one string$'   
wrongArgs         	db 0Ah, 0Dh, 'Bad parametrs in comand line$'
overflowString          db 0Ah, 0Dh, 'Too big a number$' 

oldTimerOffset  dw ?
oldTimerSegment dw ? 
        
oldKeyboardOffset dw ?
oldKeyboardSegment dw ?


newKeyboardInterrupt proc far
    pusha
    pushf
; настройка регистра ds на данные резидентной программы
    push cs
    pop ds 
    
    call dword ptr cs:oldKeyboardOffset ; вызов старого обработчика прерываний для клавиатуры

    cli  ; разрешение прерываний
    
    mov ah, 01h ; проверка наличия символа в буфере клавы
    int 16h
    jz showScreen

    cmp ah, 01h ; проверяем не нажата ли esc
    je reinstallInterrupt ; если нажата - тогда устанавливаем старые обработчики
    
    showScreen:
        sti 			; запретить прерывания
        mov ax, delay
        cmp counter, ax
        jl endOfKeyboardInterrupt
        
       push es

        mov ax , 0B800h ; видеопамять
        mov es , ax 
    	mov di, 0
    	mov cx, 4000   	;запись в видеопамять сохраненного экрана
    	lea si, screen  
    	rep movsb 

       pop es    

    endOfKeyboardInterrupt:
        mov counter, 0
        popa 
    iret  

reinstallInterrupt:

    mov ds,  cs:oldTimerSegment
    mov dx,  cs:oldTimerOffset
    mov ax , 2508h
    int 21h 

    mov ds, cs:oldKeyboardSegment
    mov dx, cs:oldKeyboardOffset
    mov ax , 2509h
    int 21h 

    push cs
    pop ds
    jmp ShowScreen

endp  

newTimerInterrupt proc far 
    cli
    pusha  
; настройка регистра ds на данные резидентной программы
    push cs
    pop ds 

    inc counter
    mov ax, delay  
    cmp counter, ax
    jne endOfTimerInterrupt

    push es
     	mov ax , 0B800h
        mov es , ax
     	xor bx , bx
        xor di , di
    saveScreen: 
        mov ax, es:[di]
        mov [screen+di], ax      
        add di, 2   
        cmp di, 4000                     
    jl saveScreen
	mov di, 0
	mov cx, 2000
    fillScreen: 
        push cx   
        mov cx,2 
        mov si,offset symbol
        rep movsb 
        pop cx
    loop fillScreen 
    pop es

endOfTimerInterrupt:  
    popa 
    sti
   jmp dword ptr cs:oldTimerOffset 

endp


main: 

    call getCmd
    call checkCmd
    call makeNum
    call set_interrupt
    
printString macro string  
    lea dx, string
    mov ah, 09h
    int 21h
endm 

set_interrupt proc

    mov ax, 3508h 
    int 21h
    mov oldTimerOffset, bx ; запоминаем смещение и сегментный адрес старого обработчика
    mov oldTimerSegment, es
    
    mov ax, 2508h ; установка нового обработчика времени
    mov dx, offset newTimerInterrupt
    int 21h  
    
    mov ax, 3509h
    int 21h
    mov oldKeyboardOffset, bx
    mov oldKeyboardSegment, es
    
    mov ax, 2509h
    mov dx, offset newKeyboardInterrupt
    int 21h
    
    mov dx, offset main ; main  - адрес первого байта за резідентным участком
    int 27h  
    ret
endp 

exit proc  
    mov ax, 4C00h
    int 21h 
    ret
endp

makeNum  proc 
  push di
  push dx

  lea     si, cmdString; устанавливаем строку из командной строки
  lea     di, num  

  xor dx  , dx
  xor cx , cx
  xor ax , ax
  mov cl , cmd_len
  sub cx , 1

loop_:
    mul bx ; умножаем ax на 10(если произошло переполнение, то лишняя часть занесется в dx)
    mov [di] , ax
    cmp  dx, 0
    jnz error 

    mov al , [si] 
    cmp al , '0'
    jb error
    
    cmp al , '9'
    ja error
    
    sub al , '0' ; вычитаем символ нуля для получения числа из символа
    xor ah , ah
    add ax , [di]
    jc error; если флаг переноса установлен в 1
    cmp ax , 8000h 
    ja error
    jmp endloop

endloop:
    inc si

loop loop_

StoreRes:
    mov [di] , ax
    mov bx, 18 
    mul bx  
    jo checkOverflow
    mov delay, ax
    pop dx
    pop di
    ret

error:
    printString wrongArgs
    call exit

checkOverflow:
   printString overflowString
   jmp exit

endp makeNum  

getCmd proc

         mov ax, @data           
         mov es , ax

		    
	    xor ch, ch	
	    mov cl, ds:[80h]	
	    cmp cl, 0 
            je emptyCommandLine	; Количество символов строки, переданной через командную строку
	    mov bl, cl
            dec cl  ;первый символ - пробел
	    mov cmd_len, cl		; В cmd_len загружаем длину командной строки
	  	
                  		        ; Уменьшаем значение количества символов в строке на 1, т.к. первый символ пробел
	    mov si, 82h		        ; Смещение на параметр, переданный через командную строки
	    lea di, cmdString
            rep movsb

            mov ds, ax		        ; Загружаем в ds данные  
	    mov cmd_len, bl
           ret

emptyCommandLine:
     mov ds, ax	
     printString emptyLine 
     call exit

getCmd endp
 
checkCmd proc 
    lea si, cmdString 
    cmp byte ptr [si], 30h
    je checkFailed
  startOfCheck:
    cmp byte ptr[si] , ' '
    je TooManyArgs
    cmp byte ptr [si], '$'
    je endOfCheck
    cmp byte ptr [si], 30h
    jl checkFailed  
    cmp byte ptr [si], 39h
    jg checkFailed 
    jmp checkPassed

  TooManyArgs:
    printString TooMany
    call exit
  checkFailed:
    printString wrongArgs
    call exit  
  checkPassed:
    inc si
    jmp startOfCheck  
    endOfCheck:  
    ret
endp 
end start