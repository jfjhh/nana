TODO
====

Custom Bootloader
-----------------

- Information Gathering

	+ High memory mapping
	+ http://wiki.osdev.org/How_Do_I_Determine_The_Amount_Of_RAM

	+ Setup table (similar to BDA) or other data structure to give found info to
	  kernel.

- Bootloader Options?

	+ Text UI, kbd reading, etc. to choose disk, fs, or to do user config.

- Load more bootloader code or load kernel (FAT FS).

	+ Enable Protected Mode and A20 Line.
	+ http://wiki.osdev.org/Protected_Mode
	+ http://wiki.osdev.org/A20_Line

- Enable paging in bootloader or kernel?

