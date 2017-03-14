      SUBROUTINE UDSIO( ROW, SOF, FILE_NAME, LUN, Y, IOS )

!***********************************************************************
!* UDSIO - UDS File Read/Write Utility Routine
!*
!* Parameters in header COMMON:
!*  NFP_L       - Array index lower bound
!*  NLP_U       - Array index upper bound
!*  NUMFORM_R   - Number format request
!*  DIMSYS_R    - Dimensional system request
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************

      ! Headers
      INCLUDE 'udsio.fi'


      ! Arguments ______________________________________________________

      CHARACTER ROW ! Read Or Write flag

      CHARACTER SOF ! Specs Or Full read flag

      CHARACTER*(*) FILE_NAME ! UDS file name

      INTEGER LUN ! Logical unit number for UDS i/o

      REAL Y(NFP_L:NLP_U) ! UDS Y data array

      INTEGER IOS ! Message suppression if -99 / Status flag (returned)


      ! Variables ______________________________________________________

      LOGICAL MSG


      ! Initializations
      IF ( IOS .EQ. -99 ) THEN
        MSG = .FALSE.
      ELSE
        MSG = .TRUE.
      END IF
      IOS = 0

      ! Call UDSIOY to do the actual I/O
      CALL UDSIOY( ROW, SOF, FILE_NAME, LUN, Y,
     & NFP_L, NLP_U, NUMFORM_R, DIMSYS_R, MSG, IOS )

      RETURN
      END
