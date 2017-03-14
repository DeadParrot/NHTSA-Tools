      SUBROUTINE BUF_WRITE( LUN, BUFF, IOS )

!***********************************************************************
!* Writes from a Buffer
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


      ! Write buffer
      WRITE( LUN, IOSTAT=IOS ) BUFF

      RETURN
      END
