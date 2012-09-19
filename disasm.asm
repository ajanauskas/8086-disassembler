.model small

.stack 100h

.data
  
  fileErrorMsg db "Klaida skaitant faila$"	
 
  ; w - 0registrai
  
  r_000     db "AL$"
  r_001     db "CL$"
  r_010     db "DL$"
  r_011     db "BL$"
  r_100     db "AH$"
  r_101     db "CH$"
  r_110     db "DH$"
  r_111     db "BH$"
  
  ; w - 1 registrai
  
  rw_000    db "AX$"
  rw_001    db "CX$"
  rw_010    db "DX$"
  rw_011    db "BX$"
  rw_100    db "SP$"
  rw_101    db "BP$"
  rw_110    db "SI$"
  rw_111    db "DI$"

  ; segmento registrai

  s_regES   db "ES$"
  s_regCS   db "CS$"
  s_regSS   db "SS$"
  s_regDS   db "DS$"  
  
  ; r/m adresavimo budai
  rm_000    db "BX + SI$"
  rm_001    db "BX + DI$"
  rm_010    db "BP + SI$"
  rm_011    db "BP + DI$"
  rm_100    db "SI$"
  rm_101    db "DI$"
  rm_110    db "BP$"
  rm_111    db "BX$"
  
  word_ptr  db "word ptr $"
  byte_ptr  db "byte ptr $"
      
  label r_array word  ; w-0 registrai
     dw  r_000, r_001, r_010, r_011
     dw  r_100, r_101, r_110, r_111
   
  label rw_array word ; w-1 registrai
     dw  rw_000, rw_001, rw_010, rw_011
     dw  rw_100, rw_101, rw_110, rw_111 

  label sr_array word
     dw s_regES, s_regCS, s_regSS, s_regDS
    
  label rm_array word ; lauko r/m reiksmes
     dw rm_000, rm_001, rm_010, rm_011
     dw rm_100, rm_101, rm_110, rm_111

  opStruct struc 
    opNameTxt  dw 0
    typeOp     db 0
    arg1       db 0
    arg2       db 0
  ends

  include opcodes.inc
  
  fileHandle dw ?
  fileName   db 200 DUP (?)
  fileBuffer db 250, 250 DUP (?)
  bufByte    db ?
  fileEnd    db 0
  firstRead  db 0

  tmp        dw 0

  opName     dw 0
  opType     db 0
  arg1type   db 0
  arg2type   db 0
  argtype    db 0
  ptrUsed    db 0
  
  addrMode   db 0  ; 0 - komanda nereikalauja adresavimo baito
  
  prefix     dw 0
  prefixType db 0
  IPc        dw 0
  
  md         db 0   ;mod
  rm         db 0
  regOpk     db 0   ;reg arba opk pletinys

  seek       db 2 DUP (?)           ; poslinkis
  hasSeek    db 0                     ; 0- nera poslinkio, 1 - 1 baito poslinkis, 2 - 2 baitu poslinkis
  imm        db 4 DUP (?)           ; imminant 
  hasImm     db 0                     ; 0 - nera; 1 -1 baito, 2 - 2 baitu imminant reiksme
  
.code 

     include helpers.inc      ; failo tvarkymas/isspausdinimo funkcijos
       
     cleanAllArguments proc
         mov [md], 0
         mov [rm], 0
         mov [regOpk], 0
         mov [hasSeek], 0
         mov [hasImm], 0
         mov [addrMode], 0 
		 mov [ptrUsed], 0
         ret
     cleanAllArguments endp
         
     analyzeAdressingMode proc
      ; analizuoja bufByte ir grazina md, rm, regOpk reiksmes
         push ax dx
		 
		 cmp [addrMode], 1
		 je __skipAnalyze
		 
		 call readByte
         mov ah, 0
         mov al, [bufByte]
		 mov [addrMode], 1
        
         mov dl, 01000000b
         div dl
         mov [md], al
         mov al, ah
         mov ah, 0
      
         mov dl, 00001000b
         div dl
         mov [regOpk], al
         mov [rm], ah
		 
		 __skipAnalyze:
      
         pop dx ax
         ret
		 
     analyzeAdressingMode endp
      
     recognizeOp proc
      ; grazina opName, opType, arg1, arg2 pagal reiksme esancia
      ; dx registre
	  
         mov al, size opStruct
         mul dl
      
         mov bx, offset [opCodes]
         add bx, ax
      
         mov ax, [bx].opNameTxt
         mov [opName], ax
        
         mov al, [bx].typeOp
         mov [opType], al
      
         mov al, [bx].arg1
         mov [arg1type], al
      
         mov al, [bx].arg2
         mov [arg2type], al
      
         ret
       
     recognizeOp endp  
   
     bytesNeeded proc
      ; kiek baitu uzima komanda
       
         mov ah, 0
       
         cmp [argType], 22
         jbe __noBytesNeeded
      
         cmp [argType], 31
         jge __addrModeNeeded
      
         jmp __analyzeImm
		 
		 __noBytesNeeded:
		     ret
		 
		 __addrModeNeeded:
		 
		     call analyzeAdressingMode
			 jmp __analyzeSeek
			 
		 __analyzeImm:
		 
		     cmp [argType], 25
			 jbe __1ByteImm
			 
			 cmp [argType], 29
			 jbe __2byteImm
			 
			 __4ByteImm:
			     mov [hasImm], 4
				 ret
			 __2ByteImm:
			     mov [hasImm], 2
				 ret
			 __1ByteImm:
			     mov [hasImm], 1
				 ret
				 
	     __analyzeSeek:
		 
		     cmp [md], 00b
			 je __analyze00mod
			 cmp [md], 01b
			 je __analyze01mod
			 cmp [md], 10b
			 je __analyze10mod
			 
			 __analyze11mod:
			     ret
				 
			 __analyze00mod:
			     cmp [rm], 110b
				 je __EAadressing
				 ret
				 
				 __EAadressing:
				     mov [hasSeek], 2
					 ret
					 
			 __analyze01mod:
			     mov [hasSeek], 1
				 ret
				 
			 __analyze10mod:
			     mov [hasSeek], 2
				 ret
     
         ret
   
     bytesNeeded endp
	 
	 setupSeekAndImm proc
	      		 
		 cmp [hasSeek], 1
		 je __setup1ByteSeek
			 
	     cmp [hasSeek], 2
	     je __setup2ByteSeek
		 jmp __setupImm
			 
		 __setup1ByteSeek:
             call readByte
             mov al, [bufByte]
             mov [seek + 1], al
			 jmp __setupImm
				 
         __setup2ByteSeek:
			 call readByte
             mov al, [bufByte]				 
             call readByte
             mov ah, [bufByte]
	         mov word ptr [seek], ax
				 
		 __setupImm:		 
	         cmp [hasImm], 1
     		 je __setup1ByteImm
    		 cmp [hasImm], 2
    		 jge __setup2ByteImm
    		 ret
		 
	    	 __setup1ByteImm:
		         call readByte
		    	 mov al, [bufByte]
	    		 mov [imm + 1], al
	     		 ret
		 
	    	 __setup2ByteImm:
	    	     call readByte
	    		 mov al, [bufByte]
	    		 call readByte
	    		 mov ah, [bufByte]
		    	 mov word ptr [imm], ax 
		    	 cmp [hasImm], 2
		    	 jg __setup4ByteImm
		     	 ret
			 
			 __setup4ByteImm: 
                 call readByte
			     mov al, [bufByte]
     			 call readByte
	    		 mov ah, [bufByte]
		     	 mov word ptr [imm + 2], ax 	
				 ret
		 
	 setupSeekAndImm endp
   
     analyzeAndPrintArg proc
      ;analizuoja [argtype] ir isspausdina argumenta      
         mov bh, 0
         mov ah, 0
         mov bl, [argType]
		 mov dl, 3
      
         cmp bl, argNone
         jne __dontfinishAnalyzeAndPrintOP
               
	     ret
      
	     __dontfinishAnalyzeAndPrintOP:
      
         cmp bl, 8
         mJbe __simple0Reg
      
         cmp bl, 16
         mJbe __simple1Reg
      
         cmp bl, 20
         mJbe __simpleSegReg
      
         cmp bl, argConst1
         mJe __argConstant1
      
         cmp bl, argConst3
         mJe __argConstant3
      
         cmp bl, argEImm8
         mJbe __Imminent
      
         cmp bl, argImm16
         mJe __Imminent
      
         cmp bl, argShort
         mJe __Relative
      
         cmp bl, argNear
         mJe __Relative
	  
	     cmp bl, argFar
	     mJe __Relative
      
         cmp bl, argOffs8
         mJe __offset
      
         cmp bl, argOffs16
         mJe __offset      
      
         cmp bl, argRegMem8
         mJe __argRegMem
      
         cmp bl, argRegMem16
         mJe __argRegMem
      
         cmp bl, argReg8
         mJe __argReg8
      
         cmp bl, argReg16
         mJe __argReg16
      
         cmp bl, argSegReg
         mJe __argSegReg
      
         ret
	
         __Offset:
      
	         call argOffsetPrint   
	         ret
	 
         __argRegMem:        
	      
	         call argRegMemPrint   
        	 ret
	 
	     __simple0reg:
		     dec bl
	         mov al, bl 
			 mul dl
			 add ax, r_array
		     writeBuffer ax
		     ret
	 
	     __simple1reg:
		 
	         sub bl, 9
	         mov al, bl 
			 mul dl
			 add ax, rw_array
	         writeBuffer ax
             ret
	 
         __simpleSegReg:
        
	         sub bl, 17
			 mov al, bl 
			 mul dl
			 add ax, sr_array
	         writeBuffer ax
             ret	 	 
	 
         __argConstant1:
       
             writeSymbol '1'
             ret      
   
         __argConstant3:
      
             writeSymbol '3'
	         ret
     
         __Imminent:
      
	         call argImminentPrint
			 ret
			
         __Relative:
     
             call argRelPrint
             ret	
  
         __argReg8:
   		     
			 mov bl, [regOpk]
	         mov al, bl 
			 mul dl
			 add ax, r_array
	         writeBuffer ax
		     ret
			 
	     __argReg16:
		 
			 mov bl, [regOpk]
	         mov al, bl 
			 mul dl
			 add ax, rw_array
	         writeBuffer ax
		     ret
		
		 __argSegReg:
		 
		     mov bl, [regOpk]
			 mov al, bl 
			 mul dl
			 add ax, sr_array
	         writeBuffer ax
			 ret 
			 
         ret
	 
     analyzeAndPrintArg endp
   
     argRelPrint proc
         cmp bl, argShort
         je __relShort
         cmp bl, argNear
         je __relNear
         jmp __relFar
 
         __relShort:
             mov al, [imm + 1]
             cbw
             add ax, [IPc]
             call printWordNumber	
             writeSymbol 'h'
             ret
		 
         __relNear:
             mov ax, word ptr [imm]
             add ax, [IPc]
             call printWordNumber
             writeSymbol 'h'
             ret
         __relFar:
             mov ah, [imm]
             call printWordNumber
             writeSymbol 'h'
             writeSymbol ':'
             mov ax, word ptr [imm + 2]
             call printWordNumber
             writeSymbol 'h'
             ret		 
    
     argRelPrint endp
   
     argImminentPrint proc   
         cmp bl, argImm8
		 je __Imm8
		 cmp bl, argImm16
		 je __Imm16
		 
		 __extendedImm:
		     mov al, [imm + 1]
			 cbw
			 call printWordNumber
			 writeSymbol 'h'
			 ret
		 __Imm8:
		     mov al, [imm + 1]
			 call printByteNumber
			 writeSymbol 'h'
			 ret
	     __Imm16:
		     mov ax, word ptr [imm]
			 call printWordNumber
			 writeSymbol 'h'
			 ret
	
     argImminentPrint endp
	 
	 checkPrefix proc
	 ; Ar Argumentas tures prefix'a
	     cmp [prefix], 0
		 je __noPrefixUsed
		 writeBuffer prefix
		 writeSymbol ':'
		 mov [prefix], 0
		 __noPrefixUsed:
	     ret
		 
	 checkPrefix endp
   
     argOffsetPrint proc  
   
         mov ax, word ptr [seek]
         cmp bl, argoffs8
         je __argoffs8
         jmp __argoffs16
	 
         __argoffs8:
	         
			 writeOffsetBuffer byte_ptr
			 call checkPrefix
		     writeSymbol '['
     	     call printWordNumber
	    	 writeSymbol 'h'
		     writeSymbol ']'
		     ret
	 
	     __argoffs16:
	     
			 writeOffsetBuffer word_ptr
			 call checkPrefix
		     writeSymbol '['
    		 mov ah, [seek]
	    	 call printWordNumber
		     writeSymbol 'h'
		     writeSymbol ']'
	    
         ret
	
     argOffsetPrint endp
	 
	 insertPtr proc
	     cmp [ptrUsed], 1
		 je __dontUse
	     cmp [argType], argRegMem8
		 je __byteptr
		 cmp [argType], argRegMem16
		 je __wordptr
		 ret
		 __byteptr:
		     writeOffsetBuffer byte_ptr
			 mov [ptrUsed], 1
			 ret
	     __wordptr:
		     writeOffsetBuffer word_ptr
			 mov [ptrUsed], 1
			 ret
		 __dontuse:
		     ret
	 insertPtr endp

     argRegMemPrint proc
	     mov bh, 0
		 cmp [md], 11b
		 mJe __11mod
		 cmp [md], 01b
		 mJge __01mod   ; arba 10 mod
               
         __00mod:
             call insertPtr		     
			 call checkPrefix
             writeSymbol '[' 
             mov bl, [rm]
			 cmp [rm], 110b
			 mJe __mod2ByteSeek
			 add bl, bl
		     mov bx, [rm_array + bx]
             writeBuffer bx
			 writeSymbol ']'
             ret
			 
		 __01mod: 
		     
             call insertPtr
			 call checkPrefix
			 writeSymbol '['
			 mov bl, [rm]
			 add bl, bl
			 mov bx, [rm_array + bx]
			 writeBuffer bx
			 
         __seek:
             
			 writeSymbol ' '
			 writeSymbol '+'
			 writeSymbol ' '
             cmp [hasSeek], 2
             je __mod2ByteSeek
             mov al, [seek + 1]
			 cbw
             call printWordNumber
			 writeSymbol 'h'
			 writeSymbol ']'
			 ret
      
         __mod2ByteSeek:
        	            
             call insertPtr	   			
		     call checkPrefix
     	     mov ax, word ptr [seek]
     	     call printWordNumber	
			 writeSymbol 'h'
             writeSymbol ']'
			 ret
	     
		 __11mod:
		     
			 cmp bl, argRegMem8
			 je __r_array
			 
			 __rw_array:
			     mov bl, [rm]
				 add bl, bl
				 mov bx, [rw_array + bx]
				 writeBuffer bx
				 ret
			 
			 __r_array:
			     mov bl, [rm]
				 add bl, bl
				 mov bx, [r_array + bx]
				 writeBuffer bx
				 ret
      
         ret
      
     argRegMemPrint endp
	 
	 analyzeExtraOpk proc
	     mov bh, 0
		 mov al, [bufByte]  ; komandos kodas
	     call analyzeAdressingMode
		 mov bl, [regOpk]
		 add bl, bl
		 cmp al, 0FFh
		 je __FFextra
		 cmp al, 0FEh
		 je __FEextra
		 cmp al, 0F6h
		 jge __F6F7Extra
		 cmp al, 0D0h
		 jge __D0D3Extra
		 
		 __8083Extra:
             mov bx, [grp1opk + bx]
             mov [opName], bx
             ret
			 
         __D0D3Extra:		
             mov bx, [grp2opk + bx]
             mov [opName], bx
             ret
			 
         __F6F7Extra:
             mov bx, [grp3opk + bx]
             mov [opName], bx
             ret
			 
         __FEextra:		
             mov bx, [grp4opk + bx]
             mov [opName], bx
             ret 
         
		 __FFextra:
             mov bx, [grp5opk + bx]
             mov [opName], bx
             ret			 
		 
	 analyzeExtraOpk endp

     endProgram proc 
         mov ax, 4C00h
         int 21h
     endProgram endp   
    
progStart:  

   mov ax, @data
   mov ds, ax
   
   call prepareFile
   
   mov si, 0
   mov di, 0
   
   __analyzeNewOp:
      call cleanAllArguments
      call readByte
      mov dh, 0
	  mov dl, [bufByte] 
      call recognizeOp       ; analizuot dl (bufByte)
      
      cmp [opType], kPrefix
      mJb __noPrefix
      
      ; PREFIXAS
      mov ax, opName
      mov [prefix], ax
	  mov al, [opType]
	  mov [prefixType], al
      jmp __analyzeNewOp
      
      __noPrefix:
      
         ; isspausdinam IPc
	     mov ax, [IPc]
      	 dec ax              ; nes padidejo IP nuskaitant komanda
	     cmp [prefix], 0     ; jei buvo prefixas tai IP turi buti dar 1 reiksme mazesne
	     je __skipDecForPrefix
	     dec ax
       
         __skipDecForPrefix:
	 
	         call printWordNumber    ; IPc print
	         writeSymbol ':'
	         writeSymbol ' '
	    
             cmp opType, kEmpty  ; arba kUnknown, kUnsupported
             jne __commandKnown
	    
	         writeBuffer opName  ; isspausdina kad nesupranta komandos
			 writeSymbol 13
	         writeSymbol 10
	         jmp __analyzeNewOp
      
     __commandknown:        ; ne prefixas ir ne nezinoma komanda
	 	 
	     cmp opType, kExtraOpk
	     je __extraOpk
	 
	     jmp __printCommand
		 
		 __extraOpk:
		    
			 call analyzeExtraOpk
		 
		 __printCommand:
		     
			 cmp [prefixType], kPrefixOp
			 jne __skipPrefixBeforeOp
			 
			 writeBuffer prefix
			 writeSymbol ' '
			 mov [prefix], 0
			 mov [prefixType], 0
			 
		 __skipPrefixBeforeOp:
	 
             writeBuffer opName
	         writeSymbol ' '
     	     mov al, [arg1Type]
    	     mov [argtype], al
    	     call bytesNeeded
	 
		   	 mov bl, [arg2Type]
	         mov [argtype], bl
	         call bytesNeeded
		 
		     call setupSeekAndImm
	 
	         mov al, [arg1Type]
	     	 mov [argType], al
		 
	         call analyzeAndPrintArg
	         cmp [arg2Type], argNone
	         je __NoComma
	 
         	 writeSymbol ','
	         writeSymbol ' '
		 
	    	 mov bl, [arg2Type]
	    	 mov [argType], bl
	         call analyzeAndPrintArg
	 
	     __NoComma:
	 	     
	         writeSymbol 13
	         writeSymbol 10
			 
	 
	     jmp __analyzeNewOp
	  
end progStart

