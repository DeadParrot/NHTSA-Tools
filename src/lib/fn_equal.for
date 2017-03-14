      LOGICAL FUNCTION FN_EQUAL( FILE_NAME_1, FILE_NAME_2 )

!***********************************************************************
!* Checks Equality of File Names or Parts of Names
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

      CHARACTER*(*) FILE_NAME_1 ! First file name to compare

      CHARACTER*(*) FILE_NAME_2 ! Second file name to compare


      ! Functions ______________________________________________________

      CHARACTER UPCASE*256

      EXTERNAL UPCASE


      ! Check file name/portion equality wrt platform case-sensitivity
      IF ( CASE_SENSITIVE_FN ) THEN ! Compare as is
        FN_EQUAL = ( FILE_NAME_1 .EQ. FILE_NAME_2 )
      ELSE ! Compare case-insensitively
        FN_EQUAL =
     &     ( UPCASE( FILE_NAME_1 ) .EQ. UPCASE( FILE_NAME_2 ) )
      END IF

      RETURN
      END
