      CHARACTER*(*) FUNCTION UPCASE( STRING )

!***********************************************************************
!* Converts to Uppercase String
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER*(*) STRING ! String


      ! Variables ______________________________________________________

      INTEGER I

      CHARACTER C


      ! Convert to uppercase
      UPCASE = STRING
      DO I = 1, LEN_TRIM( UPCASE )
        C = UPCASE(I:I)
        IF ( ( C .GE. 'a' ) .AND. ( C .LE. 'z' ) )
     &   UPCASE(I:I) = CHAR( ICHAR( C ) - 32 )
      END DO

      RETURN
      END
