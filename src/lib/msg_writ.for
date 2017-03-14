      SUBROUTINE MSG_WRITE( MSG, REPORT )

!***********************************************************************
!* Writes a Message to the Screen and an Open Report File
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/08/20
!***********************************************************************

      ! Headers
      INCLUDE 'file.fi'
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) MSG ! Message to display

      RECORD /FILE/  REPORT ! Report file object


      ! Variables ______________________________________________________

      INTEGER IOS


      ! Write the message
      WRITE( *, *, IOSTAT=IOS )
      WRITE( *, *, IOSTAT=IOS ) MSG(:L_TRIM(MSG))
      IF ( REPORT.OPEN ) THEN
        WRITE( REPORT.UNIT, *, IOSTAT=IOS )
        WRITE( REPORT.UNIT, '(A)', IOSTAT=IOS ) MSG(:L_TRIM(MSG))
      END IF

      RETURN
      END
