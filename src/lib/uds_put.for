      SUBROUTINE UDS_PUT( OUT_FILE, U, C, REPORT, OUTPUT_LIST,
     & DIR_OUT, OUT_REP, IOS )

!***********************************************************************
!* Performs Standard UDS File Write & Report
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

      CHARACTER*(*) OUT_FILE ! UDS file name

      RECORD /UDS/  U ! UDS object

      RECORD /UDS_CONTROL/  C ! UDS_CONTROL object

      RECORD /FILE/  REPORT ! Report file object

      RECORD /FILE/  OUTPUT_LIST ! Output list file object

      CHARACTER*(*) DIR_OUT ! Output directory

      LOGICAL OUT_REP ! Specifies report file is active

      INTEGER IOS ! Status flag returned


      ! Variables ______________________________________________________

      CHARACTER F_NAME*256


      ! Write UDS file
      CALL ADD_PATH( DIR_OUT, OUT_FILE )
      CALL UDS_WRITE( OUT_FILE, U, C, IOS )
      IF ( IOS .EQ. 0 ) THEN
        INQUIRE( FILE=OUT_FILE, NAME=OUT_FILE, IOSTAT=IOS )
      END IF
      F_NAME = OUT_FILE
      CALL MSG_WRITE( '  Writing: '//F_NAME, REPORT )
      IF ( IOS .NE. 0 ) THEN ! UDS read error
        CALL MSG_WRITE( '*** ERROR - UDS file write failed', REPORT )
      ELSE
        IF ( OUT_REP ) CALL REP_ENTRY( U, REPORT )
        CALL OUT_ENTRY( OUT_FILE, OUTPUT_LIST )
      END IF

      RETURN
      END
