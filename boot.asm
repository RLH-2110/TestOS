[org 7c00h]

; max size: 9 sectors/track × 40 * 2 tracks

start:
jmp start_real
nop

; BIOS Parameter Block for FAT12

; OEM identifier - 8 bytes
db "TstOs0.1"

; Bytes per sector - 2 bytes
dw 512

; Number of sectors per cluster - 1 byte
db 1

; Number of reserved sectors - 2 bytes
dw 1

; Number of File Allocation Tables - 1 byte
db 2

; Number of root directory entries - 2 bytes  
dw 16

; The total sectors in the logical volume - 2 bytes
dw 9*80

; media descriptor type - 1 byte
db 0FDh

; Number of sectors per FAT - 2 bytes
dw 2 ; not sure. I think its (80*9*1.5)/512

; Number of sectors per track - 2 bytes
dw 9

; Number of heads or sides on the storage media - 2 bytes
dw 2

; Number of hidden sectors - 4 bytes
dd 0

; Large sector count (only set if  more than 65535 sectors)- 4 bytes
dd 0

; fat 12 Extended Boot Record

; Drive number - 1 byte
db 0

; Reserved by windows NT - 1 byte
db 0

; Signature - 1 byte (0x28 or 0x29)
db 0x29

; VolumeID - 4 bytes
db "TsOs"

; Volume label string. - 11 bytes (padded with space)
db "TestOs Disk"

; System identifier string.  - 8 bytes
db "FAT12   "



start_real:
; set data segment to 0
xor ax,ax
mov ds,ax
mov es,ax

mov ah,09
mov al,'H'
int 10h
mov al,'i'
int 10h


jmp $

mov [boot_disk],dl ; dl is initalized to the current disk number, we want to save that now

mov si,str_welcome
call puts


; read the root directory
mov ah,2 ; interupt code for: Read Disk Sectors
mov al,1	;AL = number of sectors to read	(1-128 dec.)
mov ch,0	;CH = track/cylinder number  (0-39)
mov cl,5	;CL = sector number  (1-9 dec.)
mov dh,0	;DH = head number  (0-1 dec.)
mov dl,[boot_disk]	;DL = drive number (0=A:, 1=2nd floppy, 80h=drive 0, 81h=drive 1)
mov bx,9000h	;ES:BX = pointer to buffer  (this one points to the next sector, so we can access stuff as if they where continuous)
int 13h ;Read Disk Sectors
	;AL = number of sectors read
	;CF = 0 if successful
	;   = 1 if error

; check for errors
jnc .first_check_done ; cary flag must not be set!

	mov si, str_bootdisk_read_fail
	call puts

.first_check_done:

cmp al,1 ; al must be 1!
je .two_checks_done

	mov si, str_bootdisk_num_wrong
	call puts
	jmp $

.two_checks_done:

; search for stage 2
mov si,0E00h
mov dx,0

.find:
	inc dx
	call memcmp
	jz found

	add si,32
	cmp dx,16
	jnz .find

	stage2_not_found:
	mov si,str_bootdisk_stage2_404
	call puts
	jmp $

found:

add si,1Ah ; starting cluster offset
mov  cl,6 ; first cluster
add cl,[si]

cmp cl,9
jg stage2_not_found

mov si,str_welcome
call puts
jmp $

; read the root directory
mov ah,2 ; interupt code for: Read Disk Sectors
mov al,1	;AL = number of sectors to read	(1-128 dec.)
mov ch,0	;CH = track/cylinder number  (0-39)
mov dh,0	;DH = head number  (0-1 dec.)
mov dl,[boot_disk]	;DL = drive number (0=A:, 1=2nd floppy, 80h=drive 0, 81h=drive 1)
mov bx,9000h	;ES:BX = pointer to buffer  (this one points to the next sector, so we can access stuff as if they where continuous)
int 13h ;Read Disk Sectors
	;AL = number of sectors read
	;CF = 0 if successful
	;   = 1 if error

; check for errors
jnc .first_check_done ; cary flag must not be set!

	mov si, str_bootdisk_read_fail
	call puts

.first_check_done:

cmp al,1 ; al must be 1!
je .two_checks_done

	mov si, str_bootdisk_num_wrong
	call puts
	jmp $

.two_checks_done:

; check if the hdd has our data
mov ax,[7e00h] ; 7e00h is the 2 byte signature
cmp ax,1298h
je goto_main

	mov si, str_bootdisk_checksum_fail
	call puts
	jmp $

goto_main:
	jmp 7E02h ; 7e02h is the entry point for the second stage
	
;subroutines

; prints out a string and adds a new line
; arg1: string pointer
; returns: 0E00h
puts:
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
	
	ret


; si: string to compare against
; will always comapare against str_stage2
; will always compare 11 bytes
; Z if equal NZ if not equal
memcmp:
	
	mov di,str_stage2
	mov cx,11

	cld
	
	.loop:
		cmpsb ; cmp [si],[di]
		jnz .end
	loop .loop
	
	.end:
	
ret



; boot disk data

boot_disk:
dw 0

str_welcome:
db "TESTOS",0
str_stage2:
db "STAGE2  BIN"


;panic strings
str_bootdisk_read_fail:
db "DSK RD ERR",0
str_bootdisk_num_wrong:
db "DSK RD SIZ ERR",0
str_bootdisk_checksum_fail:
db "DSK CHK FAIL",0
str_bootdisk_stage2_404:
db "ST2 404",0

; SECTOR 1 | reserve rest of block and add the signature at the end
times 510-($-$$) db 0
db 55h, 0aah