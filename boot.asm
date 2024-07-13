[org 7c00h]

; for now (will very likely change later)
; calling conv: normal numbers: bx,cx,dx,ax
;				pointers:		si,di
; 
; return value: [return_value] (memory)

start:

mov [boot_disk],dl ; dl is initalized to the current disk number, we want to save that now


mov si,str_welcome ; address to string
call print_string


mov si, str_disk
call print_string

xor bx,bx
mov bl,dl
call print_num

; new line
mov ah,0eh
mov al,0dh
int 10h
mov al,0ah
int 10h


; read in more code from disk (10 more sectors for now)
mov ah,2 ; interupt code for: Read Disk Sectors
mov al,10	;AL = number of sectors to read	(1-128 dec.)
mov ch,0	;CH = track/cylinder number  (0-1023 dec., see below)
mov cl,2	;CL = sector number  (1-17 dec.)
mov dh,0	;DH = head number  (0-15 dec.)
mov dl,[boot_disk]	;DL = drive number (0=A:, 1=2nd floppy, 80h=drive 0, 81h=drive 1)
mov bx,7e00h	;ES:BX = pointer to buffer
int 13h ;Read Disk Sectors
	;AL = number of sectors read
	;CF = 0 if successful
	;   = 1 if error
	
; check for errors
jnc .first_check_done ; cary flag must not be set!
jmp .failed
.first_check_done:

cmp al,10 ; al must be 10!
je .two_checks_done


.failed:
	mov si,str_bootdisk
	jmp panic

.two_checks_done:

; check if the hdd has our data
mov ax,[new_sector_signature]
cmp ax,1298h
jne .failed



jmp main


;subroutines

;si = cause string
panic:

	; print panic
	mov di,si
	mov si,str_panic
	call print_string

	; print cause
	mov si,di
	call print_string

	.loop:
jmp .loop




; reads string into string_input
read_string:
pusha

	;setup
	mov ch,39 ; max chars that can be read (places null-terminator after)
	mov ah,0 ; KEYBOARD WAIT AND READ
	mov si,string_input

mov cl,0

	.read:

		mov ah,00h
		int 16h ; KEYBOARD WAIT AND READ (result in al)
		
		cmp al,0dh ; if enter key
		je .end
			
		cmp al,08h ; if backspace
		je .backspace

		cmp ch,0
		je .read ; if we cant write more chars, then user must delete charts or commit with enter

		; if (maybe) lower case
		cmp al,'a' ; if al >= 'a'
		jnc .force_upper

		.save:		
		call .print_char; print char on screen

		mov BYTE [si],al ; save char
		inc si ; inc pointer
		dec ch ; dec max allowed chars

		jmp .read ; skip the sub routines and go to .end

	.backspace:
		dec si ; decrement pointer
		
		; if si is smaller than the start of the buffer
		cmp si, string_input
		jc .backspace_fix	


		inc ch ; inc max allowed chars		

		; the block removes the last character from the screen

		;print backspace (WE NEED TO PRINT SOMETHING, else it breaks)
		call .print_char
		;print space
		mov al,20h
		call .print_char
		;print backspace
		mov al,08
		call .print_char
	
		mov BYTE [si],0 ; write null-terminator at the new string end
		
		jmp .read

	

	.backspace_fix: ; user tried backspace emptry string
	
		inc si ;undo changes
		jmp .read


	.force_upper:
		
		; check if letter
		cmp al,'z'+1 ; if al > z
		jnc .save ; not a letter, return
		
		; make upper case!
		sub al,20h ; difference between lower and upper case
	jmp .save

	.print_char:
		mov ah,0eh
		int 10h ; BIOS PRINT CHAR
	ret


	.end:
		mov BYTE [si],0 ; put null terminator
popa
ret













; expects si to be a pointer to the string
print_string:
pusha

	mov ah, 0eh ; BIOS PRINT CHAR
	
		.print_loop:
			mov al,[si]
			cmp al,0
		je .print_done

		int 10h ; BIOS PRINT CHAR
		inc si
	jmp .print_loop

.print_done:
popa
ret



; bx = number to print
print_num:
pusha

	; load bx into the bcd scratch
	mov WORD [dcb_scratch_original],bx

	; clean scratch (set the last 4 bytes to 0 and then the first byte to 0
	mov WORD [dcb_scratch +1],0
	mov BYTE [dcb_scratch],0
	mov dl, 16 ; counter for how many bcd loops
		
		

		
	; till the orginal number was shifted into all bcd digits
	.bcd_loop:

		call print_num_bcd_check ; check and adjust bcd digits
		call print_num_bcd_shift ; shift all 
			
	dec dl
	cmp dl,0
	jne .bcd_loop

	; num is in bcd_scratch now

	;unpack bcd
	
	mov ah, BYTE [dcb_scratch+1]
	mov bh, BYTE [dcb_scratch+2]
		
	shr BYTE [dcb_scratch+1],4
	
	mov BYTE [dcb_scratch+2],ah
	and BYTE [dcb_scratch+2],0x0F
	
	mov BYTE [dcb_scratch+3],bh ; overwrites most significant byte of dcb_scratch_original
	shr BYTE [dcb_scratch+3],4
	
	mov BYTE [dcb_scratch+4],bh ; overwrites last significant byte of dcb_scratch_original
	and BYTE [dcb_scratch+4],0x0F
	
	
	; bcd_scratch fixed up

	mov ah,0eh ; BIOS PRINT CHAR
	mov si,dcb_scratch	
	mov ch,0 ; flag to avoid printing unessesary zeros

	; print all digits (ignore leading zero)
	.print_loop:
		mov al,[si]

		
		cmp ch,0 ; if the flag is set, skip next code
		jne .bios_print

		cmp al,0 ; if al is (leading) zero, skip this itteration
		je .next
		
		; its not zero, so set the flag
		mov ch,0xff ;set flag, to allow leading zeros



		;print character
		.bios_print:
		add al,30h ; make ASCII
		int 10h ; BIOS PRINT CHAR (from AL)

		.next:
	inc si
	cmp si,dcb_scratch+5
	jne .print_loop
	



	; if flag is stil unset (means that we have not yet printed anything)
	cmp ch,0
	jne .end

		mov al,30h
		int 10h ; BIOS PRINT CHAR '0'
	
	.end:	

popa
ret

print_num_bcd_shift:

	; shift the entire scratch to the left by 1 bit
	shl WORD [dcb_scratch_original], 1 ;shifts the orginal value left by 1 (msb gets into the carry)

	rcl BYTE [dcb_scratch+2],1
	rcl BYTE [dcb_scratch+1],1
	rcl BYTE [dcb_scratch],1 

ret

; expects si and cl to be usable
print_num_bcd_check:
push ax

	lea si,dcb_scratch+2
	mov cl, 3

	clc ; clear carry
	pushf 
	
	.loop:


		
		; ah = upper nibble of [si], al = lower nibble
		mov ah,[si]
		mov al,[si]
		and ah,0xF0
		and al,0x0F
				
		; bcd digit < 5?
		cmp BYTE ah,0x50
		jc .below5_upper
		
		; its above 5, add 3
		popf
		adc BYTE [si],0x30
		pushf
	
		.below5_upper:

		
		; bcd digit < 5?
		cmp BYTE al,0x05
		jc .below5
		
		; its above 5, add 3
		popf
		add BYTE [si],0x03
		pushf
	
		.below5:
	dec si
	dec cl
	jnz .loop

popf
pop ax
ret


; si, di = memory to compare (null terminated)
; 0 = not the same
; !0 =  the same
cmp_mem:
pusha



	mov WORD [return_value],0
	
	dec si
	dec di
	
	.loop:
		inc si
		inc di
	
		; if si==di
		mov al, BYTE [si]
		cmp al, BYTE [di]
		jne .end ; if not, return false
		
		cmp BYTE [di],0 ; if not zero, keep looping
		jne .loop
	
		; both are the same
		mov WORD [return_value],1 ; return true
		
	.end:
popa
ret

; boot disk data

return_value:
dw 0

boot_disk:
dw 0

str_welcome:
db "TESTOS BOOT",0dh,0ah,0
str_disk:
db "DISK: ",0
str_ready:
db 0xd,0xa,'RDY',0dh,0ah,0


;panic strings
str_panic:
db "PANIC!",0xd,0xa,0
str_bootdisk: ; used as panic cause
db "BOOT DISK ERR",0


dcb_scratch:
times 3 db 0 ; scratch and result of double dabble 
dcb_scratch_original:
dw 0 ;hold the orginal 16 bit value that will use double dabble on

binary_conv:
dw 0

string_input:
times 40 db 0 ; for storeing strings (null terminated)

; reserve rest of block and add the signature at the end
times 510-($-$$) db 0
db 55h, 0aah



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Code after this will be on "disk" and needs to be loaded first
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; more data:

new_sector_signature:
dw 1298h

str_colon:
db ": ",0
str_shutdown:
db "SHUTDOWN",0
str_reboot:
db "REBOOT",0
str_echo:
db "ECHO",0
str_help:
db "HELP",0
str_unknown:
db "UNKOWN COMMAND! TYPE HELP FOR HELP.",0

str_help_text:
db "HELP - THIS MESSAGE",0ah,0dh,
db "ECHO - ECHOS TEXT (NO COMMAND LINE ARGUMENTS!!)",0ah,0dh,
db "SHUTDOWN - SHUTS DOWN PC",0ah,0dh,
db "REBOOT - REBOOTS PC",0ah,0dh,0

str_old_apm:
db "NO APM >= 1.1",0

; more code


main:



;mov si,str_help
;mov di,str_help ; help command
;call cmp_mem
;cmp WORD [return_value],1
;jz .no_init
;call cmd_help
;
;.no_init
;jmp panic


;print RDY
call new_line
mov si,str_ready
call print_string
call new_line


call read_string
call new_line

; pare commands
mov si,string_input
mov di,str_help ; help command
call cmp_mem
cmp WORD [return_value],0
jz .no_help
call cmd_help
jmp main
.no_help:

mov di,str_echo ; echo command
mov si,string_input
call cmp_mem
cmp WORD [return_value],0
jz .no_echo
call cmd_echo
jmp main
.no_echo:


mov di,str_shutdown ; shutdown command
call cmp_mem
cmp WORD [return_value],0
jz .no_shutdown
call cmd_shutdown
.no_shutdown:


mov di,str_reboot; reeboot command
call cmp_mem
cmp WORD [return_value],0
jz .no_reboot
call cmd_reboot
.no_reboot:


mov si,str_unknown
call print_string

jmp main




cmd_echo:
pusha

mov si,str_colon
call print_string

call read_string

call new_line

mov si,string_input
call print_string


popa
ret

cmd_help:
	mov si,str_help_text
	call print_string
ret

cmd_shutdown:
	;https://wiki.osdev.org/APM

	;perform an installation check
	mov ah,53h            ;this is an APM command
	mov al,00h            ;installation check command
	xor bx,bx             ;device id (0 = APM BIOS)
	int 15h               ;call the BIOS function through interrupt 15h
	jc .error          ;if the carry flag is set there was an error
	
	;connect to an APM interface
	mov ah,53h               ;this is an APM command
	mov al,0                 ; real mode
	xor bx,bx                ;device id (0 = APM BIOS)
	int 15h                  ;call the BIOS function through interrupt 15h
	jc .error             ;if the carry flag is set there was an error
	
	mov ah,53h               ;this is an APM command
	mov al,0eh               ;set driver supported version command
	mov bx,0000h             ;device ID of system BIOS
	mov ch,01h               ;APM driver major version
	mov cl,01h               ;APM driver minor version (can be 01h or 02h if the latter one is supported)
	int 15h
	jc .error
	
	;Enable power management for all devices
	mov ah,53h              ;this is an APM command
	mov al,08h              ;Change the state of power management...
	mov bx,0001h            ;...on all devices to...
	mov cx,0001h            ;...power management on.
	int 15h                 ;call the BIOS function through interrupt 15h
	jc .error            ;if the carry flag is set there was an error
	
	;Set the power state for all devices
	mov ah,53h              ;this is an APM command
	mov al,07h              ;Set the power state...
	mov bx,0001h            ;...on all devices to...
	mov cx,3                ; off
	int 15h                 ;call the BIOS function through interrupt 15h

	
	.error:
	mov si, str_old_apm
	jmp panic
	

cmd_reboot:
	jmp 0xFFFF:0000
ret


new_line:
push ax

	mov ah,0eh
	mov al,0ah
	int 10h
	mov al,0dh
	int 10h

pop ax
ret





; bx : number to print
print_bin:
pusha

mov di,bx ; orignal routine was written to expect bx as parameter

mov ah,0eh ; code for BIOS PRINT CHAR


mov al,0dh
int 10h
mov al,0ah
int 10h


mov WORD [binary_conv],1000_0000_0000_0000b

.loop:
	mov bx,di
	and bx,WORD [binary_conv]
	call .toBin
	int 10h
	
	shr WORD [binary_conv],1
	jnz .loop


mov al,0dh
int 10h
mov al,0ah
int 10h

popa
ret

; local function. if bx is 0 set al to '0', if di is not zero, set al to '1'
.toBin:
	; the instruction before the call is an and
	jz .zero
		mov al,31h
		ret
	.zero:
		mov al,30h
ret




; reserve rest of block
times 11*512-($-$$) db 0xFF ; 10 more sectors
