# Makefile for assembling boot.asm
#
# Author: OpenAI ChatGPT
# Date: July 13, 2024
# Description: This Makefile compiles boot.asm using NASM assembler.
#              It creates boot.bin and provides a cleanup target.

# Variables
NASM = nasm
NASM_FLAGS = -f bin
OUTPUT = boot.img

# Targets
all: $(OUTPUT)

$(OUTPUT): boot.bin stage2.bin README.MD
	rm -f $(OUTPUT)
	# create empty 360k image and put a FAT12 filesystem with 16 root entries
	mkfs.fat -F 12 -n "TESTOS DISK" -r 16 -s 1 -S 512 -g 9/2 -C $@ 720
	# write boot sector
	dd if=boot.bin of=$@ conv=notrunc

	mkdir -p ./tmp
	sudo mount -o loop -t vfat boot.img ./tmp

	sudo cp stage2.bin ./tmp
	sudo cp README.MD ./tmp

	sudo umount ./tmp

boot.bin: boot.asm
	$(NASM) $(NASM_FLAGS) -o $@ $^ -l boot.lst

stage2.bin: stage2.asm
	$(NASM) $(NASM_FLAGS) -o $@ $^ -l stage2.lst

clean:
	rm -rf ./tmp
	rm -f $(OUTPUT) boot.lst boot.bin stage2.bin

# Phony targets
.PHONY: all clean
