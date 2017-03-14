      SUBROUTINE FILE_INIT( FILE_OBJ )

!***********************************************************************
!* Initializes a FILE Object
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************

      ! Headers
      INCLUDE 'file.fi'


      ! Arguments ______________________________________________________

      RECORD /FILE/  FILE_OBJ ! File object


      ! Initializations
      FILE_OBJ.NAME = ' '
      FILE_OBJ.UNIT = 0
      FILE_OBJ.N_REC = 0
      FILE_OBJ.EXISTS = .FALSE.
      FILE_OBJ.OPEN = .FALSE.

      RETURN
      END
