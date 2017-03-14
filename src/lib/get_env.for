      SUBROUTINE GET_ENV( VAR_NAME, VAR_VALUE )

!***********************************************************************
!* Gets the Value of a Specified Environment Variable
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER*(*) VAR_NAME ! Environment variable name

      CHARACTER*(*) VAR_VALUE ! Environment variable value returned


      ! Get the variable value
      CALL GET_ENVIRONMENT_VARIABLE( VAR_NAME, VAR_VALUE )

      RETURN
      END
