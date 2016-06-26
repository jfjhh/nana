#include <stdlib.h>
#include <stdio.h>

int main(int argc, const char **argv)
{
	FILE *mbr, *img;
	int c, wmbr;
	size_t n, max, start;

	if (argc < 3)
		return EXIT_FAILURE;

	if (!(mbr = fopen(argv[1], "r"))) {
		perror("could not open mbr binary");
		return EXIT_FAILURE;
	} else if (!(img = fopen(argv[2], "r+"))) {
		perror("could not open disk image");
		return EXIT_FAILURE;
	}

	wmbr = (argc == 4) ? 0 : 1;

	/* start of the sector */
	start = wmbr ? 0 : 2048 * 512; /* MBR or VBR. */
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

