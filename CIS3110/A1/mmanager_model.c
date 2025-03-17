#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

#include "mmanager.h"

int runModel(FILE *outputfp, FILE *inputfp,
		long totalMemorySize, int fitStrategy,
		int verbosity
	)
{
	char *memoryBlock = NULL;
	int nSuccessfulActions = 0;
	mmgr_action action;

	fprintf(outputfp,
			"Running a %s-fit model in %ld (0x%lx) bytes of memory.\n",
			(fitStrategy == STRAT_FIRST ? "first" :
				fitStrategy == STRAT_BEST ? "best" :
					fitStrategy == STRAT_WORST ? "worst" : "unknown"),
			totalMemorySize, totalMemorySize);

	/**
	 * this is the only allocation you should need -- all requests
	 * from your model should come from this allocated block
	 */
	memoryBlock = (char *) malloc(totalMemorySize);
	if (memoryBlock == NULL) {
		perror("allocation failed!");
		return -1;
	}

	/**
	 *	+++ Set up anything else you will need for your memory management
	 */

	while (getAction(&action, inputfp, outputfp, verbosity) > 0)
	{
		if (action.type == ACTION_ALLOCATE)
		{
			/* +++ do an allocation */
		} else
		{
			/* +++ do a release */
		}

		/** increment our count of work we did */
		nSuccessfulActions++;
	}


	/** +++ Clean up your memory management */
	free(memoryBlock);


	return nSuccessfulActions;
}
