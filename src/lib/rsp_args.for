      SUBROUTINE RSP_ARGS( L_NAM, R_NAM )

!***********************************************************************
!* Gets the Response Program Inputs
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER*(*) L_NAM ! List file name

      CHARACTER*(*) R_NAM ! Response file name


      ! Get response program inputs
      CALL GET_COMMAND_ARGUMENT( 1, L_NAM )
      CALL GET_COMMAND_ARGUMENT( 2, R_NAM )

      RETURN
      END
