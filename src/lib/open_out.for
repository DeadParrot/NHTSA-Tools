      SUBROUTINE OPEN_OUT( OUTPUT_LIST, PROG_NAME, DIR_OUT )

!***********************************************************************
!* Opens the Output List File
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

      RECORD /FILE/  OUTPUT_LIST ! Output list file object

      CHARACTER*(*) PROG_NAME ! Program name

      CHARACTER*(*) DIR_OUT ! Output directory


      ! Variables ______________________________________________________

      INTEGER IOS, LN


      ! Open output list file
      IF ( OUTPUT_LIST.EXISTS ) THEN
        IF ( OUTPUT_LIST.NAME .EQ. ' ' ) THEN ! Use default list file
          OUTPUT_LIST.NAME = PROG_NAME
          LN = LEN_TRIM( OUTPUT_LIST.NAME )
          OUTPUT_LIST.NAME(LN+1:) = '.out'
          CALL ADD_PATH( DIR_OUT, OUTPUT_LIST.NAME )
          CALL FN_NINC( OUTPUT_LIST.NAME )
        ELSE ! Add DIR_OUT path to path-free name
          CALL ADD_PATH( DIR_OUT, OUTPUT_LIST.NAME )
        END IF
        CALL OPEN_FS( OUTPUT_LIST.UNIT, OUTPUT_LIST.NAME, 'W', IOS )
        IF ( IOS .NE. 0 ) ! Try overwrite mode
     &     CALL OPEN_FS( OUTPUT_LIST.UNIT, OUTPUT_LIST.NAME, ' ', IOS )
        IF ( IOS .EQ. 0 ) THEN
          OUTPUT_LIST.N_REC = 0
          OUTPUT_LIST.EXISTS = .TRUE.
          OUTPUT_LIST.OPEN = .TRUE.
          INQUIRE( OUTPUT_LIST.UNIT, NAME=OUTPUT_LIST.NAME,
     &       IOSTAT=IOS )
        ELSE
          WRITE( *, '(/A/)' ) ' *** Output list file open failed'
          CALL FILE_INIT( OUTPUT_LIST )
        END IF
      END IF

      RETURN
      END
