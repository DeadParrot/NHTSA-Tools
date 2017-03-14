/*******************************************************************************
** C wrappers for opendir, readdir, closedir
**
** Language: C
**
** Platform: Linux/GCC
**
** Author: Andrew Orndorff
**
** Date: 1999/08/20
**
** Notes:
** . Wildcard filename matching handled externally via POSIX regex calls
**   in the WILD_MATCH() subroutine.
** . Uses the EPC argument passing convention to recover the string length.
** . Uses a statically allocated DIR pointer, and dirent structure
**   which means that only a single directory stream may be open
**   at any one time. In particular, recursive calls of these
**   functions are not currently supported. To support recursive calls
**   or multiple directory streams the calling interface would have to
**   be modified to accept a context argument, or would have to return
**   points to dynamically allocated DIR/dirent elements to the parent
**   Fortran routine.
*******************************************************************************/

#include "uds.h"

#include <string.h>

/* Forward */
F77_INT f77_return( int );

static DIR * dirp;
static struct dirent * direntp;

F77_INT
OPEN_DIR(
 char * FILE_SPEC,
 long int FSPEC_LEN
)
{
	int stat;
	char * filename;
	char * cp;

	/* Generate the directory name as a normal C string */

	filename = (char *) calloc( 1, FSPEC_LEN+1 );
	if ( filename != (char *)NULL ) {
		(void) memcpy( (MEMTYP*)filename, (MEMTYP*)FILE_SPEC, FSPEC_LEN );

		/* Eliminate any whitespace */

		cp = (char *) &filename[FSPEC_LEN];
		while ( ( *cp == ' ' ) || ( *cp == '\t' ) || ( *cp == '\0' ) ) {
			--cp;
		}
		++cp;
		*cp = '\0';

	/* Now terminate the directory spec at the last "/" character */

		cp = strrchr( filename, '/' );
		if ( cp ) filename[ (int)( cp - filename ) ] = '\0';
		dirp = opendir( filename );
	} else {
		dirp = (DIR*)NULL;
	}

	(void) free((MEMTYP *) filename);

	/* Error condition */

	if ( dirp == (DIR*)NULL ) {
		stat = 1;
	} else {
		stat = 0;
	}

	return f77_return( stat );
}

F77_INT
READ_DIR(
 char * FILE_NAME,
 long int FNAME_LEN
)
{
	int stat;

	/* Initialize the return string to be all blanks to clear garbage from the string */

	(void) memset( (MEMTYP*)FILE_NAME, ' ', FNAME_LEN );

	/* If pointing to a valid directory structure */

	stat = 1;

	if ( dirp != (DIR*)NULL ) {
		direntp = readdir( dirp );

		/* Skip past the "." and ".." directories */

		while ( ( direntp != (struct dirent *)NULL ) && (
		 ( strcmp( direntp->d_name, "." ) == 0 ) ||
		 ( strcmp( direntp->d_name, ".." ) == 0 ) ) ) {
			direntp = readdir( dirp );
		}

		/* Error condition */

		if ( direntp != (struct dirent *)NULL ) {
			(void) memcpy( (MEMTYP*)FILE_NAME, (MEMTYP*)direntp->d_name, min( FNAME_LEN, strlen( direntp->d_name ) ) );
			stat = 0;
		}
	}

	return f77_return( stat );
}

F77_INT
CLOSE_DIR( void )
{
	int stat = closedir( (DIR *)dirp );
	return f77_return( stat );
}
