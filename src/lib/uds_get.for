      SUBROUTINE UDS_GET( FILE_NAME, U, C, REPORT, IOS )

!***********************************************************************
!* Performs Standard UDS File Read & Report
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2001/03/03
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'
      INCLUDE 'file.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) FILE_NAME ! UDS file name

      RECORD /UDS/  U ! UDS object

      RECORD /UDS_CONTROL/  C ! UDS_CONTROL object

      RECORD /FILE/  REPORT ! Report file object

      INTEGER IOS ! Status flag returned


      ! Variables ______________________________________________________

      CHARACTER F_NAME*256


      ! Read UDS file
      F_NAME = FILE_NAME
      CALL MSG_WRITE( '  Reading: '//F_NAME, REPORT )
      CALL UDS_READ( FILE_NAME, U, C, IOS )
      IF ( IOS .NE. 0 ) THEN ! UDS read error
        CALL MSG_WRITE( '*** ERROR - UDS file read failed', REPORT )
      ELSE
        CALL REP_ENTRY( U, REPORT )
      END IF

      RETURN
      END
