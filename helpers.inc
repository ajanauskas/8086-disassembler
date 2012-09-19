writeSymbol macro buffer
     push ax dx
     mov dh, 0
     mov dl, buffer
     mov ax, 0200h 
     int 21h
     pop dx ax
endm

mJbe macro mem
     local no_jump
         jnbe no_jump
         jmp mem
     no_jump:
endm

mJe macro mem
     local no_jump
         jne no_jump
         jmp mem
     no_jump:
endm

mJb macro mem
     local no_jump
         jge no_jump
         jmp mem
     no_jump:
endm

mJge macro mem
     local no_jump
         jb no_jump
         jmp mem
     no_jump:
endm

writeOffsetBuffer macro buffer
     push ax dx
     mov ax, 0900h
     mov dx, offset buffer
     int 21h
     pop dx ax
endm   

writeBuffer macro buffer
     push ax dx
     mov dx, buffer
     mov ax, 0900h
     int 21h
     pop dx ax
endm   

numberToHex proc
     ; registro AX analizei
     ; naudojama su funkcija printNumber
     cmp al, 10
     JGE __HEX
     add al, '0'
     jmp __endNumberToHex
   
     __HEX:
   
         sub al, 10
         add al, 'A'
   
     __endNumberToHex:   
   
     ret
   
numberToHex endp
	
printByteNumber proc
     ; Isspausdint baito dydzio reiksme registre al
     push bx dx
     mov dx, 0
     mov ah, 0
     mov bx, 16
     div bx
   
     call numberToHex
     writeSymbol al
     mov ax, dx
   
     call numberToHex
     writeSymbol al
   
     pop dx bx
     ret
printByteNumber endp

printWordNumber proc
   ; Isspausdint baito dydzio reiksme registre ax
     push bx dx
     mov dx, 0
   
     mov bx, 4096  ; 16^3
     div bx
   
     call numberToHex
     writeSymbol al
     mov ax, dx
   
     mov dx, 0
     mov bx, 256
     div bx
   
     call numberToHex
     writeSymbol al
     mov ax, dx
   
     mov dx, 0
     mov bx, 16
     div bx
   
     call numberToHex
     writeSymbol al
     mov ax, dx
   
     call numberToHex
     writeSymbol al
     pop dx bx 
     ret

printWordNumber endp

readByte proc
   
     push ax dx bx cx
     ; si - dabartine pozicija fileBufferyje
     ; di - baitu perskaityta
     cmp si, di
     jge __readNewBuffer
	 jmp __readByte
   
     __readNewBuffer:
	     
		 cmp [fileEnd], 1
		 je __endProgram
		 mov ax, 3F00h
         mov dx, offset fileBuffer
         mov bx, fileHandle
         mov cx, 250
         int 21h		 
		 cmp [firstRead], 0
		 jne __notFirstTimeReading
		 mov [firstRead], 1
		 cmp word ptr [fileBuffer], 'ZM'
		 jne __dontskip200Bytes
		 mov ax, 4200h
		 mov bx, fileHandle
		 mov cx, 0
		 mov dx, 200h
		 int 21h
		 jmp __readNewBuffer
		 __dontskip200Bytes:
		 __notFirstTimeReading:
		     mov di, ax
		     mov si, 0
	     	 cmp di, 250
	    	 jb __EOF
	    	 jmp __readByte

	 __endProgram:
	     call endProgram
	 
     __EOF:
         
		 mov [fileEnd], 1
         call closeFile
	 
     __readByte:
         
         inc [IPc]         ; IPc pakyla 1 kiekvienam bitui
		 mov al, [fileBuffer + si]
		 inc si
		 mov [bufByte], al
     
     pop cx bx dx ax
     ret
	
readByte endp	

prepareFile proc 
     mov bx, 0081h  ;programos paleidimo parametrai rasomi segmente es pradedant 129 (arba 81h) baitu
     mov di, 0

     __parSeek:  
         mov al, es:[bx]  
         inc bx
   
         cmp al, 0Dh     ; pabaigos simbolis
         je __parEnd
         cmp al, ' '
         je __parSeek    ; praleidziam tarpus
   
         mov [fileName + di], al
         inc di
         jmp __parSeek

     __parEnd:

         mov byte ptr [fileName + di], 0

         mov ax, 3D00h  ; atidaryt faila (al- 0 - read)
         mov dx, offset fileName
         int 21h
         jc __fileError
   
     jmp __noErrors

     __fileError:
        
		 call fileError

     __noErrors:
         
		 mov fileHandle, ax
     ret
  	 
prepareFile endp	

closeFile proc 
     mov ax, 3E00h
     mov bx, fileHandle
     int 21h
     ret
closeFile endp   

fileError proc 
     writeOffsetBuffer fileErrorMsg
     call endProgram
fileError endp
