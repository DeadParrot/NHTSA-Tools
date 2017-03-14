      SUBROUTINE STR_UP( STRING )

!***********************************************************************
!* Converts Argument to Uppercase String
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

      CHARACTER*(*) STRING ! String to convert


      ! Variables ______________________________________________________

      INTEGER I

      CHARACTER C


      ! Convert to upper case
      DO I = 1, LEN_TRIM( STRING )
        C = STRING(I:I)
        IF ( ( C .GE. 'a' ) .AND. ( C .LE. 'z' ) )
     &     STRING(I:I) = CHAR( ICHAR( C ) - 32 )
      END DO

      RETURN
      END



      SUBROUTINE STR_UPCASE( STR_OUT, STR_IN )

!***********************************************************************
!* Converts to Uppercase Output String
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

      CHARACTER*(*) STR_OUT ! Converted string

      CHARACTER*(*) STR_IN ! String to convert


      ! Variables ______________________________________________________

      INTEGER I

      CHARACTER C


      ! Convert to upper case
      STR_OUT = STR_IN
      DO I = 1, LEN_TRIM( STR_OUT )
        C = STR_OUT(I:I)
        IF ( ( C .GE. 'a' ) .AND. ( C .LE. 'z' ) )
     &     STR_OUT(I:I) = CHAR( ICHAR( C ) - 32 )
      END DO

      RETURN
      END
