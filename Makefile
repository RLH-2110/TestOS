# Makefile for assembling boot.asm
#
# Author: OpenAI ChatGPT
# Date: July 13, 2024
# Description: This Makefile compiles boot.asm using NASM assembler.
#              It creates boot.bin and provides a cleanup target.

# Variables
NASM = nasm
NASM_FLAGS = -f bin -l boot.lst
OUTPUT = boot.bin

# Targets
all: $(OUTPUT)

$(OUTPUT): boot.asm
	$(NASM) $(NASM_FLAGS) -o $(OUTPUT) boot.asm

clean:
	rm -f $(OUTPUT) boot.lst

# Phony targets
.PHONY: all clean
