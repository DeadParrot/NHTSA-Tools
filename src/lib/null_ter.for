      CHARACTER*(*) FUNCTION NULL_TERM( STRING )

!***********************************************************************
!* Returns a Null-Terminated Copy of a String
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER*(*) STRING ! String to null-terminate


      ! Variables ______________________________________________________

      INTEGER LS


      ! Null-terminate the string
      NULL_TERM = STRING
      LS = MIN( LEN( NULL_TERM ), LEN( STRING ) + 1 )
      NULL_TERM(LS:LS) = CHAR(0)

      RETURN
      END



      CHARACTER*(*) FUNCTION NULL_TRIM( STRING )

!***********************************************************************
!* Returns a Null-Trimmed Copy of a String
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER*(*) STRING ! String to null-terminate


      ! Variables ______________________________________________________

      INTEGER LS


      ! Null-terminate the string
      NULL_TRIM = STRING
      LS = MIN( LEN( NULL_TRIM ), LEN_TRIM( STRING ) + 1 )
      NULL_TRIM(LS:LS) = CHAR(0)

      RETURN
      END
