.model tiny  
.286
Code SEGMENT
Assume CS: Code, DS: Code , SS:Code , ES :Code
org 100h  
resprog :jmp main
     
key dw 			5555h


num			dw 0
counter			dw 0 
delay 			dw 0  
symbol			db 003h, 00000100b
   
screen			dw 2000 dup('$')

oldTimerOffset  dw ?  
oldTimerSegment dw ?    
oldKeyboardOffset dw ?
oldKeyboardSegment dw ?

flag 		db 0

newKeyboard proc far
       
   push ax
   push es
   push bx
   push cx
   push dx
   push si
   push di
  ; push ds
    
    push cs
    pop ds
    pushf
    call dword ptr cs:oldKeyboardOffset; вызов старого обработчика прерываний для клавиатуры

    mov ah, 01h
    int 16h
    jz showScreen ;проверка наличия чего-либо в буфере 

    cmp ah, 01h ; проверяем не нажата ли esc
    je reinstallInterrupt;  

    showScreen:
 
        mov ax, delay
        cmp counter, ax
        jl endOfKeyboard

        mov ax , 0B800h ; видеопамять
        mov es , ax 
	mov di, 0000h
        mov ax , 0500h
        int 10h	
        mov di, 0
	mov cx, 4000 ;запись в видеопамять сохраненного экрана
	lea si, screen
	rep movsb
        cmp flag , 1
        jne endOfKeyboard
    


	push cs
	pop es
	mov ah , 49h
	int 21h

    endOfKeyboard:
        mov counter, 0 
	;pop ds
        pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop es 
        pop ax 
        iret 
 
 reinstallInterrupt:

    mov flag , 1
    mov ds, word ptr cs:oldTimerSegment
    mov dx, word ptr cs:oldTimerOffset
    mov ax , 2508h
    int 21h 

    mov ds, word ptr cs:oldKeyboardSegment
    mov dx, word ptr cs:oldKeyboardOffset
    mov ax , 2509h
    int 21h

    push cs
    pop ds

    jmp showScreen 
         
endp  

newTimer proc far 
   push ax
   push es
   push bx
   push cx
   push dx
   push si
   push di
   ;push ds

    push cs
    pop ds
  
; настройка регистра ds на данные резидентной программы

    inc counter
    mov ax, delay  
    cmp counter, ax
    je cont
    jmp endOfTimer

cont:
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
 
         mov ax , 0B900h
         mov es , ax
   	 mov ax, 0501h   
    	 int 10h
	 xor di , di
	 mov cx, 2000

fillScreen: 

        push cx   
        mov cx,2 
        mov si,offset symbol
        rep movsb 
        pop cx
	sub cx , 1
        cmp cx , 0
        jne fillScreen

endOfTimer:  
        ;pop ds
        pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop es  
        pop ax  
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

exit proc  
    mov ax, 4C00h
    int 21h 
    ret
endp

set_interrupt proc near
; pusha
 push es 
 push ds

 push cs
 pop ds
 
;производим проверку (сравнивая с ключом) на ;повторную установку  программы.
    mov ax, 3508h 
    int 21h
   
    cmp word ptr es:[103h],5555h
    jz inst

    mov word ptr oldTimerOffset, bx ; запоминаем смещение и сегментный адрес старого обработчика
    mov word ptr oldTimerSegment, es

   
    
    mov ax, 2508h ; установка нового обработчика времени
    mov dx, offset newTimer
    int 21h  
    
    mov ax, 3509h
    int 21h
    cmp word ptr es:[103h],5555h
    jz inst
    mov word ptr oldKeyboardOffset, bx
    mov word ptr oldKeyboardSegment, es

    
    mov ax, 2509h
    mov dx, offset newKeyboard
    int 21h
    
    mov dx, offset main ; main  - адрес первого байта за резідентным участком
    int 27h 

    pop ds
    pop es
    ret

inst: 

    pop ds
    pop es
    mov ax , 0B800h
    mov es , ax
    mov ax, 0500h   
    int 10h
    printString bye
    mov ax,4C00h
    int 21h 
    
endp

makeNum  proc near 
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
    mov ax, 4C00h
      int 21h 
    

checkOverflow:
   printString overflowString
   mov ax, 4C00h
      int 21h 
   

endp makeNum  

getCmd proc near
         mov ax, @code         
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
     printString emptyLine 
     mov ax, 4C00h
     int 21h
    
getCmd endp
 
checkCmd proc near 
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
      mov ax, 4C00h
      int 21h
  checkFailed:
      printString wrongArgs
      mov ax, 4C00h
      int 21h 
  checkPassed:
    inc si
    jmp startOfCheck  
   endOfCheck:  
    ret
endp 

cmdString		db 80 dup('$') 
cmd_len			db 0
emptyLine 		db 0Ah, 0Dh, 'No arguments in command line$'
tooMany                 db 0Ah, 0Dh, 'Too many arguments$'   
wrongArgs         	db 0Ah, 0Dh, 'Bad arguments in comand line$'
overflowString          db 0Ah, 0Dh, 'Too big a number$' 
bye			db 0Ah, 0Dh, 'Already loaded$'

Code ENDS
END resprog 
end start
