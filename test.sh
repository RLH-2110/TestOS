#! /usr/bin/bash

#nasm boot.asm -f bin -l boot.lst -o boot.bin
make
qemu-system-x86_64 boot.bin
