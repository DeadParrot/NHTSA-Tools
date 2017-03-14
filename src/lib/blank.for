      LOGICAL FUNCTION BLANK( STRING )

!***********************************************************************
!* Checks if String is Blank or NULL
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER*(*) STRING ! String to check


      ! Check string
      IF ( STRING .EQ. ' ' ) THEN
        BLANK = .TRUE.
      ELSE IF ( STRING(1:1) .EQ. CHAR(0) ) THEN
        BLANK = .TRUE.
      ELSE
        BLANK = .FALSE.
      END IF

      RETURN
      END
