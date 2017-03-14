      INTEGER FUNCTION LN_TRIM( STRING )

!***********************************************************************
!* Finds the Length of a String Excluding Trailing Spaces, Nulls, & Tabs
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER*(*) STRING ! String to examine


      ! Variables ______________________________________________________

      INTEGER I

      CHARACTER C, NUL, TAB

      PARAMETER ( NUL = CHAR(0), TAB = CHAR(9) )


      ! Find the length excluding trailing spaces and tabs
      DO I = LEN( STRING ), 1, -1
        C = STRING(I:I)
        IF ( ( C .NE. ' ' ) .AND.
     &   ( C .NE. NUL ) .AND.
     &   ( C .NE. TAB ) ) THEN
          LN_TRIM = I
          RETURN
        END IF
      END DO
      LN_TRIM = 0

      RETURN
      END



      INTEGER FUNCTION L_TRIM( STRING )

!***********************************************************************
!* Finds the Length of a String Excluding Trailing Spaces, Nulls & Tabs
!* . L_TRIM >= 1 to allow safe use of string(:L_TRIM(string)) in F77/90
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER*(*) STRING ! String to examine


      ! Variables ______________________________________________________

      INTEGER I

      CHARACTER C, NUL, TAB

      PARAMETER ( NUL = CHAR(0), TAB = CHAR(9) )


      ! Find the length excluding trailing spaces and tabs
      DO I = LEN( STRING ), 1, -1
        C = STRING(I:I)
        IF ( ( C .NE. ' ' ) .AND.
     &   ( C .NE. NUL ) .AND.
     &   ( C .NE. TAB ) ) THEN
          L_TRIM = I
          RETURN
        END IF
      END DO
      L_TRIM = 1

      RETURN
      END
