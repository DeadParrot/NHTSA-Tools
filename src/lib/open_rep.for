      SUBROUTINE OPEN_REP( REPORT, PROG_NAME, DIR_OUT, NO_INCR )

!***********************************************************************
!* Opens the Report File and Writes Report Header
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

      RECORD /FILE/  REPORT ! Report file object

      CHARACTER*(*) PROG_NAME ! Program name

      CHARACTER*(*) DIR_OUT ! Output directory

      LOGICAL NO_INCR ! Specifies not to increment the report file name


      ! Variables ______________________________________________________

      INTEGER IOS, LN


      ! Open report file
      IF ( REPORT.EXISTS ) THEN
        CALL FILE_INIT( REPORT )
        REPORT.NAME = PROG_NAME
        LN = LEN_TRIM( REPORT.NAME )
        REPORT.NAME(LN+1:) = '.rep'
        CALL ADD_PATH( DIR_OUT, REPORT.NAME )
        IF ( .NOT. NO_INCR ) CALL FN_NINC( REPORT.NAME )
        CALL OPEN_FS( REPORT.UNIT, REPORT.NAME, 'W', IOS )
        IF ( IOS .EQ. 0 ) THEN ! Write report header
          REPORT.EXISTS = .TRUE.
          REPORT.OPEN = .TRUE.
          CALL REP_HEAD( REPORT.UNIT, PROG_NAME )
          INQUIRE( REPORT.UNIT, NAME=REPORT.NAME, IOSTAT=IOS )
        ELSE
          WRITE( *, '(/A/)' ) ' *** Report file open failed'
          CALL FILE_INIT( REPORT )
        END IF
      END IF

      RETURN
      END
