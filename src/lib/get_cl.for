      SUBROUTINE GET_CL( CL_ARG, NUM_ARGS, MAX_ARGS )

!***********************************************************************
!* Gets the Command Line Arguments
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*         Andrew Orndorff
!*
!* Date: 1999/08/20
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER*(*) CL_ARG(0:*) ! Command line argument array

      INTEGER NUM_ARGS ! Number of arguments

      INTEGER MAX_ARGS ! Max number of arguments accepted


      ! Variables ______________________________________________________

      INTEGER I


      ! Get command line arguments
      NUM_ARGS = MIN( COMMAND_ARGUMENT_COUNT(), MAX_ARGS )
      DO I = 0, NUM_ARGS
        CALL GETARG( I, CL_ARG(I) )
      END DO

      RETURN
      END
