      INTEGER FUNCTION FIRST_CHAR( STRING, CHARS )

!***********************************************************************
!* Finds Index of First Character of CHARS in STRING
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER*(*) STRING ! String to search

      CHARACTER*(*) CHARS ! Characters to look for


      ! Variables ______________________________________________________

      INTEGER I, J


      ! Check for any of CHARS in STRING
      DO I = 1, LEN( STRING )
        DO J = 1, LEN( CHARS )
          IF ( STRING(I:I) .EQ. CHARS(J:J) ) THEN
            FIRST_CHAR = I
            RETURN
          END IF
        END DO
      END DO
      FIRST_CHAR = 0

      RETURN
      END
