      INTEGER FUNCTION LAST_INDEX( STRING, SUBSTRING )

!***********************************************************************
!* Finds the Last Index Position of a Substring in a String
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER*(*) STRING ! String to search

      CHARACTER*(*) SUBSTRING ! Substring to find


      ! Variables ______________________________________________________

      INTEGER I, LSUB


      ! Search string backwards for substring (trailing space matters)
      LAST_INDEX = 0
      LSUB = LEN( SUBSTRING )
      IF ( LSUB .EQ. 0 ) RETURN
      DO I = LEN( STRING ) - LSUB + 1, 1, -1
        IF ( STRING(I:I+LSUB-1) .EQ. SUBSTRING ) THEN
          LAST_INDEX = I
          RETURN
        END IF
      END DO

      RETURN
      END
