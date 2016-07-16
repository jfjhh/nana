# Nana: Momo's Bootloader

A two-stage bootloader, written from scratch, that loads the kernel (`momo`)
from a FAT 12 FS, and creates tables with information for the kernel to use.

This bootloader is licensed under the GPLv3.

## Build Dependencies

### Required

- Root permissions (e.g. for `fdisk` and the `make drive` target).
- `fdisk`.
- `nasm`.
- `gcc`.
- GNU `make`.
- `sed`.
- `ed`.
- POSIX `sh` and utilities.
- `dd`.
- MTools (`mformat`, `mcopy`, etc.).

### Optional

- `bochs`.
- `qemu`.
- `git`.
- `pv`.

## Building

The bootloader uses a GNU `make` `VPATH`-based top-level *Makefile*, as well as
some configuration files and scripts as its build system.

Building is as easy as:

```sh
./bootstrap.sh [OPTION=VALUE]
make -j$(nproc)
```

Where OPTION=VALUE can be any number of arguments to set variables for make,
e.g. `PREFIX=..` or `TARGET_ARCH=i686-elf`.

Before invoking `make`, you can also edit the `config.mk` file that
`bootstrap.sh` generates.

Disk images are placed in the build directory, and you can emulate them by
running `make bochs` or `make qemu` for whichever emulator you prefer.

