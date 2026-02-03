/*
 * Hamilton-Wright, Andrew (2011)
 *
 * Memory Map Block Inspection
 *
 */
 
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define	HALF_BYTES_PER_LINE	8
#define	BYTES_PER_LINE	(HALF_BYTES_PER_LINE * 2)


/**
 * Print out a region of memory in map format to the screen,
 * arranging the data in a form reminicent of hexdump -C
 *
 * You probably won't need to edit this file.
 *
 * This function gets called from the "dump" functions, but
 * you can also call it yourself. 
 *
 * If you pass it the same pointer for "base" and "start", it will
 * begin its numbering from zero.  If you pass "((void *) 0)"
 * frequently known as "NULL", then it will tell you the absolute
 * memory addresses of the bytes it prints.
 *
 * The value "start" should be the real data you want to print;
 * nBytesToPrint is the number of bytes starting at that location,
 * and then "indent" simply shifts things over a bit on the screen
 * for reading.
 *
 * If you pass the same value for "start" and "base", then your
 * offsets will be numbered relative to the start of the block.
 */
int
memdbg_dump_map(FILE* fp,
		void *base, void *start,
		size_t nBytesToPrint, int indent)
{
	unsigned char *cBase, *cStart;
	unsigned char buffer[BYTES_PER_LINE];
	size_t bytesThisLine, bytesRemain;
	size_t i, j;

	/**
	 * We convert the void * addresses to char * for byte based
	 * pointer arithmetic
	 */
	cBase = (unsigned char *) base;
	cStart = (unsigned char *) start;

	i = 0;
	while (i < nBytesToPrint)
	{
		bytesRemain = nBytesToPrint - i;
		/** figure out how many bytes to print */
		bytesThisLine = (bytesRemain < BYTES_PER_LINE)
					? bytesRemain : BYTES_PER_LINE;

		/** copy the data into our working buffer */
		memcpy(buffer, &cStart[i], bytesThisLine);

		fprintf(fp, "%*s", indent, "");

		fprintf(fp, "0x%04lx", ((unsigned long) (cStart - cBase)) + i);

		/** print the hex values */
		for (j = 0; j < bytesThisLine; j++)
		{
			if (j == HALF_BYTES_PER_LINE)
				fprintf(fp, " ");
			fprintf(fp, " %02x", cStart[i+j]);
		}

		/** pad if we are short */
		for ( ; j < BYTES_PER_LINE; j++)
		{
			if (j == HALF_BYTES_PER_LINE)
				fprintf(fp, " ");
			fprintf(fp, "   ");
		}

		/** print as chars */
		fprintf(fp, " ");
		for (j = 0; j < bytesThisLine; j++)
			fprintf(fp, "%c", isprint(cStart[i+j]) ? cStart[i+j] : '.');

		fprintf(fp, "\n");

		/** update i by the amount we have printed */
		i += bytesThisLine;
	}

	if (ferror(fp)) return -1;

	return 1;
}