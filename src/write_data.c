/**
 * Simple tool to write files or an MBR at an offset to a disk image.
 * Copyright (C) 2016  Alex Striff
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <stdlib.h>
#include <stdio.h>

int main(int argc, const char **argv)
{
	FILE *mbr, *img;
	int c, wmbr;
	unsigned int arg;
	size_t n, max, start;

	if (argc < 3)
		return EXIT_FAILURE;

	if (!(mbr = (argv[1][0] == '-') ? stdin : fopen(argv[1], "r"))) {
		perror("could not open mbr binary");
		return EXIT_FAILURE;
	} else if (!(img = fopen(argv[2], "r+"))) {
		perror("could not open disk image");
		return EXIT_FAILURE;
	}

	wmbr = (argc == 4) ? 0 : 1;

	/* start of the sector */
	if (wmbr) {
		start = 0;
	} else {
		if (!sscanf(argv[3], "%u", &arg)) {
			fclose(img);
			fclose(mbr);
			fprintf(stderr,
					"Invalid starting block argument: '%s'.\n", argv[3]);
			return EXIT_FAILURE;
		} else {
			start = arg * 512;
		}
	}
	fseek(img, start, SEEK_SET);

	/* byte 446 is where the disk id and partition table starts. */
	max = wmbr ? 446 : -1;
	n   = 0;
	while ((c = getc(mbr)) != EOF && n++ < max)
		putc(c, img);

	if (wmbr) { /* Put bootable magic word. */
		fseek(img, 510, SEEK_SET);
		putc('\x55', img);
		putc('\xaa', img);
	}

	fclose(img);
	fclose(mbr);

	return EXIT_SUCCESS;
}

