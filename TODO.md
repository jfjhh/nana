TODO
====

Custom Bootloader
-----------------

- Information Gathering

	+ High memory mapping
	+ http://wiki.osdev.org/How_Do_I_Determine_The_Amount_Of_RAM

	+ Detect video modes
	+ http://wiki.osdev.org/Getting_VBE_Mode_Info

	+ Setup table (similar to BDA) or other data structure to give found info to
	  kernel.

- Bootloader Options?

	+ Text UI, kbd reading, etc. to choose disk, fs, or to do user config.

- Load more bootloader code or load kernel (FAT FS).

	+ Chain into FAT Volume Boot Record: Have it load more bootloader code from
	  the hidden sectors, as well as a list of blocks that the kernel image is
	  on, within the actual FS (FAT for now). It should be chain-loadable, i.e.
	  a third-party MBR can load it or another partition.
	+ http://wiki.osdev.org/FAT

	+ Enable Protected Mode and A20 Line.
	+ http://wiki.osdev.org/Protected_Mode
	+ http://wiki.osdev.org/A20_Line

- Enable paging in bootloader or kernel?

