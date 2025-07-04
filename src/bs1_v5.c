#include <string.h>


void bs1_v5_(int *n, double *a, double *b) 
{
	memcpy((void *) b, (void *) a, (size_t)(*n)*8 );
}