      SUBROUTINE BUF_READ( LUN, BUFF, IOS )

!***********************************************************************
!* Reads to a Buffer
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************


      ! Arguments ______________________________________________________

      INTEGER LUN ! Logical i/o unit

      CHARACTER*(*) BUFF ! Buffer

      INTEGER IOS ! Status flag


      ! Read buffer
      READ( LUN, IOSTAT=IOS ) BUFF

      RETURN
      END
