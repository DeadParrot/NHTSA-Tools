      CHARACTER*(*) FUNCTION DNCASE( STRING )

!***********************************************************************
!* Converts to Lowercase String
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


      ! Convert to lowercase
      DNCASE = STRING
      DO I = 1, LEN_TRIM( DNCASE )
        C = DNCASE(I:I)
        IF ( ( C .GE. 'A' ) .AND. ( C .LE. 'Z' ) )
     &     DNCASE(I:I) = CHAR( ICHAR( C ) + 32 )
      END DO

      RETURN
      END
