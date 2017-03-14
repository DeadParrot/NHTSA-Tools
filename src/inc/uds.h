#ifndef uds_h_INCLUDED
#define uds_h_INCLUDED

/*******************************************************************************
** uds.h: UDS include file for C I/O interface
**
** Language: C
**
** Author: Andrew Orndorff
**
** Date: 1999/08/20
*******************************************************************************/

#include <stdlib.h>
#include <sys/types.h>
#include <stdio.h>
#include <ctype.h>
#include <strings.h>
#include <dirent.h>
#include <regex.h>

#define MAXLEN 255

#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

#ifndef max
# define max(a,b) ((a)<(b) ? (b) : (a))
#endif

#ifndef min
# define min(a,b) ((a)>(b) ? (b) : (a))
#endif

#ifndef FOPEN_MAX
#define FOPEN_MAX 8
#endif

#define F77_INT int
#define MEMTYP void

/* Global symbol name processing */
#define OPEN_DIR open_dir_
#define READ_DIR read_dir_
#define CLOSE_DIR close_dir_
#define WILD_MATCH wild_match_
#define MKDIR mkdir_

/* Prototypes */
F77_INT OPEN_DIR( char * dirname, long int dir_len );
F77_INT READ_DIR( char * fname, long int fn_len );
F77_INT CLOSE_DIR( void );
F77_INT WILD_MATCH( char * p, char * text, long int p_len, long int text_len );
F77_INT MKDIR( char * directory, long int dir_len );

#endif
