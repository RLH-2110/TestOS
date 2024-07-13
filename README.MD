# TestOS

This will be a very simple test OS (it won't be able to do much beyond minimal IO and running programs)

# Running

Either run `qemu-system-x86_64 boot.bin` with the assembled boot.bin (boot.img also works)  
You can also use virtual box and boot via floppy disk.

# Assembling

run the Makefile with `make`  
You  will need to have nasm installed!

# Progress

Right now, there is a very minimal interface with 4 commands (help, echo, shutdown, reboot)  
currently, shutdown does not work everywhere. You might get an error about APM.  
  
The plan is to expand the OS to allow running of programs from disks (probably floppy)  
I might decide to support fat 12