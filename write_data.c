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

	if (!(mbr = fopen(argv[1], "r"))) {
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
