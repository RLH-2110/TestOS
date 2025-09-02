[org 7c00h]

; most functions use CDECL

start:

; set data segment to 0
xor ax,ax
mov ds,ax
mov es,ax

mov [boot_disk],dl ; dl is initalized to the current disk number, we want to save that now

mov ax,str_welcome
push ax
call puts
add sp, 2


; read in more code from disk (10 more sectors for now)
mov ah,2 ; interupt code for: Read Disk Sectors
mov al,2	;AL = number of sectors to read	(1-128 dec.)
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

	mov ax, str_bootdisk_read_fail
	push ax
	call puts
	add sp, 2
	jmp $

.first_check_done:

cmp al,2 ; al must be 10!
je .two_checks_done

	mov ax, str_bootdisk_num_wrong
	push ax
	call puts
	add sp, 2
	jmp $

.two_checks_done:

; check if the hdd has our data
mov ax,[new_sector_signature]
cmp ax,1298h
je goto_main

	mov ax, str_bootdisk_checksum_fail
	push ax
	call puts
	add sp, 2
	jmp $

goto_main:
	jmp main
	
;subroutines

; prints out a string and adds a new line
; arg1: string pointer
; returns: 0E00h
puts:
	push bp      ; Preserve current frame pointer
	mov  bp, sp  ; Create new frame pointer pointing to current stack top
	
	pushf
	push si
	
		mov si,[bp+4] ; get first argument
		cld
		
		mov ah, 0eh ; BIOS PRINT CHAR
		
		.print_loop:
			lodsb ; load al with [si] and inc si
			cmp al,0 ; is null terminator?
			jz .print_done 

			int 10h ; BIOS PRINT CHAR
		jmp .print_loop

	.print_done:
	
	; print newline
	mov al,0dh
	int 10h
	mov al, 0ah
	int 10h
	
	popf
	pop si
	pop bp
	ret


; reads string into buffer
; arg1: buffer to read into
; arg2: buffer size - 1
; returns: ??? just ignore return value
read_string:
	push bp	
	mov bp,sp

	push si

	;setup
	mov ch,[bp+4] ; max chars that can be read (places null-terminator after)
	mov ah,0 ; KEYBOARD WAIT AND READ
	mov si,[bp+6] ; get address of buffer to write to

	mov cl,0

	.read:

		mov ah,00h
		int 16h ; KEYBOARD WAIT AND READ (result in al)
		
		cmp al,0dh ; if enter key
		je .end
			
		cmp al,08h ; if backspace
		je .backspace

		cmp ch,0
		je .read ; if we cant write more chars, then user must delete chars or commit with enter

		; if (maybe) lower case
		cmp al,'a' ; if al >= 'a'
		jnc .force_upper

		.save:		
		call .print_char; print char on screen (not cdecl)

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
		call .print_char ;(not cdecl)
		;print space
		mov al,20h
		call .print_char ;(not cdecl)
		;print backspace
		mov al,08
		call .print_char ;(not cdecl)
	
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

	.print_char: ;(not cdecl)
		mov ah,0eh
		int 10h ; BIOS PRINT CHAR
	ret


	.end:
		mov BYTE [si],0 ; put null terminator

	pop si
	pop bp
ret	
	

; compares two memory regions for arg3 bytes
; arg1: start of memory to compare
; arg2: start of the other memory we will compare to
; arg3: amount of characters to compare
; returns: 0 if equal | < 0 if str1 is less than str2 | > 0 if str2 is less than str1
memcmp:
	push bp
	mov bp,sp

	push si
	push di
	pushf
	
	mov si,[bp+8] ; get arg1
	mov di,[bp+6] ; get arg2
	mov cx,[bp+4] ; get arg3
	xor ax,ax ; set return value to 0
	
	cmp cx,0 ; if size == 0, return 0
	je .end
	
	cld
	
	.loop:
		cmpsb ; cmp [si],[di]
		jc .str2_bigger
		jnz .str1_bigger
	loop .loop
	
	jmp .end
	.str2_bigger:
		mov ax,-1
		jmp .end
		
	.str1_bigger:
		mov ax,1
		
	.end:
	
	popf 
	pop di
	pop si
	pop bp
ret

; boot disk data

boot_disk:
dw 0

str_welcome:
db "TESTOS BOOT",0
str_ready:
db 0dh,0ah,'RDY',0

;panic strings
str_bootdisk_read_fail:
db "DISK READ ERR",0
str_bootdisk_num_wrong:
db "DISK READ SIZE ERR",0
str_bootdisk_checksum_fail:
db "DISK CHK FAIL",0


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
db "REBOOT - REBOOTS PC",0ah,0dh,0

string_input:
times 40 db 0 ; for storeing strings (null terminated)

; main entry
main:


mov ax, str_ready
push ax
call puts
add sp, 2


mov ax,string_input
push ax
mov ax,39
push ax
call read_string
add sp, 4

; print newline
mov ax,0e0dh
int 10h
mov al, 0ah
int 10h

; pare commands

	; help 
	mov ax, string_input
	push ax
	mov ax, str_help
	push ax
	mov ax, 5
	push ax
	call memcmp
	add sp,6
	cmp ax,0
	je cmd_help
	
	
	; echo 
	mov ax, string_input
	push ax
	mov ax, str_echo
	push ax
	mov ax, 5
	push ax
	call memcmp
	add sp,6
	cmp ax,0
	je cmd_echo

	; reboot 
	mov ax, string_input
	push ax
	mov ax, str_reboot
	push ax
	mov ax, 7
	push ax
	call memcmp
	add sp,6
	cmp ax,0
	je cmd_reboot
	
	mov ax,str_unknown
	push ax
	call puts
	add sp,2
	
jmp main


cmd_echo:
	mov ax,str_colon
	push ax
	call puts
	add sp, 2

	mov ax, string_input
	push ax
	mov ax, 39
	push ax
	call read_string
	add sp, 4
	
	; print newline
	mov ax,0e0dh
	int 10h
	mov al, 0ah
	int 10h

	mov ax,string_input
	push ax 
	call puts
	add sp,2
jmp main

cmd_help:
	mov ax,str_help_text
	push ax
	call puts
	add sp,2
jmp main

cmd_reboot:
	; https://delorie.com/djgpp/doc/rbinter/id/79/22.html#:~:text=disk%20have%20a%20valid%20boot,0000h%20at%200040h%3A0072h%20before%20jumping
	mov ax, 0040h
	mov ds, ax
	mov word [0072h], 1234h

	jmp 0xFFFF:0000
jmp main





; reserve rest of block
times 3*512-($-$$) db 0xFF ; 2 more sectors