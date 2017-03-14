      LOGICAL FUNCTION ANY_CHARS( STRING, CHARS )

!***********************************************************************
!* Checks for Any of the Characters of CHARS in STRING
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER*(*) STRING ! String to check

      CHARACTER*(*) CHARS ! Characters to check for


      ! Variables ______________________________________________________

      INTEGER I, J


      ! Check for any of CHARS in STRING
      DO I = 1, LEN( STRING )
        DO J = 1, LEN( CHARS )
          IF ( STRING(I:I) .EQ. CHARS(J:J) ) THEN
            ANY_CHARS = .TRUE.
            RETURN
          END IF
        END DO
      END DO
      ANY_CHARS = .FALSE.

      RETURN
      END
