/*******************************************************************************
** C Wildcard Filename Matching Routine
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
** . Use the POSIX regex calls to process a modified filename pattern.
** . Filenames are passed in DOS/VMS wildcard format which is then rewritten
**   into a POSIX regular expression.
** . Uses the EPC argument passing convention to recover the string length.
*******************************************************************************/

#include "uds.h"

#include <string.h>

/* Forward */
F77_INT f77_return( int );

F77_INT
WILD_MATCH(
 char * p,
 char * text,
 long int p_len,
 long int text_len
)
{
	char * pc;
	char * textc;
	char * cp;
	char * tstr;

	int stat;
	int p_flags;

	regex_t p_buf;

	/* Load the Fortran string into the C string */

	pc = (char*)calloc( 1, p_len+1 );
	textc = (char*)calloc( 1, text_len+1 );
	if ( ( pc == (char*)NULL ) || ( textc == (char*)NULL ) ) {
		return f77_return( (int)FALSE );
	}

	(void) memcpy( (MEMTYP*)pc, (MEMTYP*)p, p_len );
	(void) memcpy( (MEMTYP*)textc, (MEMTYP*)text, text_len );

	/* Eliminate whitespace */

	cp = (char*)&pc[ p_len ];
	while ( ( *cp == ' ' ) || ( *cp == '\t' ) || ( *cp == '\0' ) ) {
		--cp;
	}
	++cp;
	*cp = '\0';
	cp = (char*)&textc[ text_len ];
	while ( ( *cp == ' ' ) || ( *cp == '\t' ) || ( *cp == '\0' ) ) {
		--cp;
	}
	++cp;
	*cp = '\0';

	/* Process a DOS/VMS style pattern into a regular expression
	for expansion - somewhat more limiting but more familiar
	to users. Replace occurrences of '.' with '\.'; replace
	'*' with '.*'; and prepend '^' to the beginning of the
	pattern to force a match with beginning-of-line */

	stat = 1;
	tstr = (char*)calloc( 1, strlen( pc ) + 1 );
	if ( tstr ) {
		cp = pc;

		/* First add the BOL tag to the new pattern */
		tstr = (char*)realloc( (MEMTYP*)tstr, strlen( pc ) + 2 );
		if ( tstr ) {
			tstr[ 0 ] = '\0';
			(void) strcat( tstr, "^\0" );

			/* Now loop through original pattern, substituting in
			appropriate regex patterns for DOS/VMS wildcards */
			stat = 0;
			while ( ( *cp != '\0' ) && ( stat == 0 ) ) {
				switch( *cp ) {
				case '.':
					tstr = (char*)realloc( (MEMTYP*)tstr, strlen( tstr ) + 3 );
					if ( tstr ) {
						(void) strcat( tstr, "\\." );
					} else {
						stat = 1;
					}
					break;
				case '~':
					tstr = (char*)realloc( (MEMTYP*)tstr, strlen( tstr ) + 3 );
					if ( tstr ) {
						(void) strcat( tstr, "\\~" );
					} else {
						stat = 1;
					}
					break;
				case '?':
					tstr = (char*)realloc( (MEMTYP*)tstr, strlen( tstr ) + 2 );
					if ( tstr ) {
						(void) strcat( tstr, "." );
					} else {
						stat = 1;
					}
					break;
				case '*':
					tstr = (char*)realloc( (MEMTYP*)tstr, strlen( tstr ) + 3 );
					if ( tstr ) {
						(void) strcat( tstr, ".*" );
					} else {
						stat = 1;
					}
					break;
				default:
					tstr = (char*)realloc( (MEMTYP*)tstr, strlen( tstr ) + 2 );
					if ( tstr ) {
						(void) strncat( tstr, cp, 1 );
					} else {
						stat = 1;
					}
					break;
				}
				++cp;
			}

		/* And finally add an end-of-line tag to avoid matching unwanted
		characters at the end of the text line */
			tstr = (char*) realloc( (MEMTYP*)tstr, strlen( tstr ) + 2 );
			if ( tstr ) {
				(void) strcat( tstr, "$" );
			} else {
				stat = 1;
			}
		}

		/* Pass the strings to the matching routine and return */
		if ( stat == 0 ) {
			p_flags = REG_NOSUB | REG_NEWLINE;
			stat = regcomp( &p_buf, tstr, p_flags );
			if ( stat == 0 ) {
				p_flags = 0;
				stat = regexec( &p_buf, textc, 0, (regmatch_t*)NULL, p_flags );
			}
		}
	}

	if ( tstr ) {
		(void) free( (MEMTYP*)tstr );
	}
	(void) regfree( (regex_t*)&p_buf );
	(void) free( (MEMTYP*)pc );
	(void) free( (MEMTYP*)textc );

	return f77_return( stat );
}