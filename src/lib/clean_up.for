      SUBROUTINE CLEAN_UP( HIT, REPORT, OUTPUT_LIST, DIR_OUT )

!***********************************************************************
!* Performs Standard Program Clean-Up Operations Before Termination
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 1999/10/18
!***********************************************************************

      ! Headers
      INCLUDE 'file.fi'
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      RECORD /FILE/  HIT ! Hit file

      RECORD /FILE/  REPORT ! Report file

      RECORD /FILE/  OUTPUT_LIST ! Output list file

      CHARACTER*(*) DIR_OUT ! Output directory


      ! Variables ______________________________________________________

      INTEGER IOS_W


      ! Clean up and stop
      CALL FILE_DEL( HIT )
      CALL FILE_CLOSE( OUTPUT_LIST )
      IF ( .NOT. BLANK( DIR_OUT ) ) THEN
        WRITE( *, '(/2A)', IOSTAT=IOS_W )
     &     ' *** Outputs redirected to:  ',
     &     DIR_OUT(:L_TRIM(DIR_OUT))
      END IF
      IF ( REPORT.OPEN ) THEN
        CALL FILE_CLOSE( REPORT )
        WRITE( *, '(/2A)', IOSTAT=IOS_W )
     &     ' *** Report file written to: ',
     &     REPORT.NAME(:L_TRIM(REPORT.NAME))
      END IF

      RETURN
      END
