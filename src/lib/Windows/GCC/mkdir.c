/*******************************************************************************
** C Wrapper for mkdir() Function Call
**
** Language: C
**
** Platform: Windows/GCC
**
** Author: Andrew Orndorff
**
** Date: 1999/08/20
**
** Notes:
** . Uses the users default file permissions creation mask since the
**   wrapper doesn't accept a permission mask as an argument.
** . Uses the EPC argument passing convention to recover the string length.
*******************************************************************************/

#include "uds.h"

#include <string.h>
#include <sys/stat.h>

/* Forward */
F77_INT f77_return( int );

F77_INT
MKDIR(
 char * directory,
 long int dir_len
)
{
	char * dir;
	char * cp;
	int stat;

	dir = (char*)calloc( 1, dir_len+1 );
	if ( dir != (char*)NULL ) {

		/* Copy in the directory name to create */
		(void) memcpy( (MEMTYP*)dir, (MEMTYP*)directory, dir_len );

		/* Remove any whitespace */
		cp = (char*)&dir[ dir_len ];
		while ( ( *cp == ' ' ) || ( *cp == '\t' ) || ( *cp == '\0' ) ) {
			--cp;
		}
		++cp;
		*cp = '\0';

		/* Create the directory with the default permissions */
		stat = mkdir( dir );
	} else {
		stat = -1;
	}

	(void) free( (MEMTYP*)dir );

	return f77_return( stat );
}
