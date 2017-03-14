      SUBROUTINE WRITE_DEV( LUN, BUFF, IOS )

!***********************************************************************
!* Writes from a Buffer to a File or Screen Device
!* . LUN < 0 => screen output
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************

      ! Headers
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      INTEGER LUN ! Logical i/o unit

      CHARACTER*(*) BUFF ! Buffer

      INTEGER IOS ! Status flag


      ! Write buffer
      IF ( LUN .GE. 0 ) THEN ! File write
        WRITE( LUN, '(A)', IOSTAT=IOS ) BUFF(:L_TRIM(BUFF))
      ELSE ! Screen write
        WRITE( *, '(1X,A)', IOSTAT=IOS ) BUFF(:L_TRIM(BUFF))
      END IF

      RETURN
      END
