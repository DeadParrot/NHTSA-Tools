      SUBROUTINE STR_DN( STRING )

!***********************************************************************
!* Converts Argument to Lowercase String
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
        IF ( ( C .GE. 'A' ) .AND. ( C .LE. 'Z' ) )
     &     STRING(I:I) = CHAR( ICHAR( C ) + 32 )
      END DO

      RETURN
      END



      SUBROUTINE STR_DNCASE( STR_OUT, STR_IN )

!***********************************************************************
!* Converts to Lowercase Output String
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
        IF ( ( C .GE. 'A' ) .AND. ( C .LE. 'Z' ) )
     &     STR_OUT(I:I) = CHAR( ICHAR( C ) + 32 )
      END DO

      RETURN
      END
