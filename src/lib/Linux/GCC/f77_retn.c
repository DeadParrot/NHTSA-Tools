/*******************************************************************************
** Wrapper function to return the appropriate integer status value for a C
** function to a Fortran caller
**
** Language: C
**
** Platform: Linux/GCC
**
** Author: Andrew Orndorff
**
** Date: 1999/08/20
*******************************************************************************/

#include "uds.h"

F77_INT
f77_return( int stat )
{
	int new_status;
	new_status = stat;
	return (F77_INT)new_status;
}
