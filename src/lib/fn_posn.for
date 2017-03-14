      INTEGER FUNCTION FN_POSN( FILE_NAME )

!***********************************************************************
!* Finds Start Position of Path-Free Part of File Name
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************

      ! Headers
      INCLUDE 'platform.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) FILE_NAME ! File name


      ! Variables ______________________________________________________

      INTEGER P


      ! Functions ______________________________________________________

      LOGICAL ANY_CHARS

      EXTERNAL ANY_CHARS


      ! Find start of path-free file name
      FN_POSN = LEN_TRIM( FILE_NAME )
      IF ( FN_POSN .EQ. 0 ) RETURN
      IF ( ANY_CHARS( FILE_NAME(FN_POSN:FN_POSN), PATH_CHARS ) ) THEN
        FN_POSN = 0
        RETURN
      END IF
      DO WHILE ( FN_POSN .GT. 1 )
        P = FN_POSN - 1
        IF ( ANY_CHARS( FILE_NAME(P:P), PATH_CHARS ) ) RETURN
        FN_POSN = P
      END DO

      RETURN
      END
