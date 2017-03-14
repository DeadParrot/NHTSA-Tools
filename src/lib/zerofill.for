      SUBROUTINE ZERO_FILL( STRING )

!***********************************************************************
!* Zero-Fills a String from Beginning to Last Non-Blank Character
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************

      ! Headers
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) STRING ! String to zero-fill


      ! Variables ______________________________________________________

      INTEGER I

      CHARACTER NULL

      PARAMETER ( NULL = CHAR(0) )


      ! Zero-fill the string
      DO I = 1, LEN_TRIM( STRING )
        IF ( ( STRING(I:I) .EQ. ' ' ) .OR.
     &     ( STRING(I:I) .EQ. NULL ) ) STRING(I:I) = '0'
      END DO

      RETURN
      END
