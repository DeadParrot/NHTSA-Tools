      SUBROUTINE UDS_CLOSE( LUN )

!***********************************************************************
!* Closes a UDS File
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/09/10
!***********************************************************************


      ! Arguments ______________________________________________________

      INTEGER LUN ! Logical i/o unit


      ! Variables ______________________________________________________

      INTEGER IOS


      ! Close the file
      CLOSE( UNIT=LUN, IOSTAT=IOS )

      RETURN
      END
