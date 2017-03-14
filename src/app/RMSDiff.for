      PROGRAM RMSDIFF

!***********************************************************************
!* Computes the RMS Difference of Two UDS Y-Series Files
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2001/03/04
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'
      INCLUDE 'uds_fxn.fi'
      INCLUDE 'file.fi'


      ! Variables ______________________________________________________

      RECORD /UDS_Y/  S, U
      RECORD /UDS_CONTROL/  C
      RECORD /FILE/  HIT, REPORT, OUTPUT_LIST

      LOGICAL ALL_BATCH, NO_BATCH, NO_INCR, ONE_PASS, NEW_HIT, EOF,
     & DONE

      INTEGER NUM_ARGS, MAX_ARGS, IOS, I

      PARAMETER ( MAX_ARGS = 12 )

      DOUBLE PRECISION RMS

      CHARACTER CL_ARG(0:MAX_ARGS)*255, PROG_NAME*8,
     & TEMPLATE_STR*255, DIR_OUT*255,
     & FILE_NAME*255,
     & PROMPT_1*79, PROMPT_2*79


      ! Initializations
      PROG_NAME = 'RMSDiff'
      CALL FILE_INIT( HIT )
      CALL GET_CL( CL_ARG, NUM_ARGS, MAX_ARGS )
      CALL PROC_CL( CL_ARG, NUM_ARGS, PROG_NAME, TEMPLATE_STR,
     & ALL_BATCH, NO_BATCH, NO_INCR, REPORT, OUTPUT_LIST, DIR_OUT,
     & ONE_PASS, .TRUE., .TRUE. )
      PROMPT_1 = 'First UDS file name/template/hitlist?'
      PROMPT_1(74:) = '[done]'
      PROMPT_2 = 'Second UDS file name/template/hitlist?'
      CALL UDS_CONTROL_INIT( C )
      C.CHANFORM = 'Y' ! Only accept Y-series file input

      WRITE( *, '(//24X,A//)' ) '**  RMS Difference of UDS Files  **'

      ! Read the first file name/template/hitlist
  100 CALL FILE_PROMPT( TEMPLATE_STR, PROMPT_1, DIR_OUT,
     & ONE_PASS, HIT, NEW_HIT, FILE_NAME, EOF )
      IF ( ( FILE_NAME .EQ. ' ' ) .OR. ( EOF ) ) GO TO 199 ! Done

      ! Read first UDS file
      CALL REP_COLS( REPORT )
      C.DIMSYS = ' '
      CALL UDS_GET( FILE_NAME, S, C, REPORT, IOS )
      IF ( IOS .NE. 0 ) THEN ! UDS read error
        IF ( HIT.OPEN ) CALL FILE_SKIP( HIT ) ! Skip file
        GO TO 190
      END IF

      ! Process first UDS file
      C.DIMSYS = S.DIMSYS

      ! Read/process the other UDS files
      DONE = .FALSE.
      DO WHILE ( .NOT. DONE )

        ! Read another file name/template/hitlist
  110   CALL FILE_PROMPT( TEMPLATE_STR, PROMPT_2, DIR_OUT,
     &     ONE_PASS, HIT, NEW_HIT, FILE_NAME, EOF )

        IF ( EOF ) THEN
          GO TO 190
        ELSE IF ( FILE_NAME .EQ. ' ' ) THEN ! No more files
          IF ( ONE_PASS ) THEN
            CALL MSG_WRITE(
     &         '*** Operation skipped - No second file', REPORT )
            GO TO 190
          ELSE
            GO TO 110
          END IF
        ELSE ! Read/process a file

          ! Read next UDS file
          CALL UDS_GET( FILE_NAME, U, C, REPORT, IOS )
          IF ( IOS .NE. 0 ) THEN ! UDS read error
            IF ( ( HIT.OPEN ) .OR. ( ONE_PASS ) ) THEN ! Skip to next
              GO TO 190
            ELSE
              GO TO 110
            END IF
          END IF

          ! Check UDS fields for consistency
          IF ( .NOT. EQUAL_SP( U.DEL, S.DEL, 2.E-7 ) ) THEN
            CALL MSG_WRITE( '*** ERROR - Inconsistent DEL step',
     &         REPORT )
            IF ( ( HIT.OPEN ) .OR. ( ONE_PASS ) ) THEN ! Skip to next
              GO TO 190
            ELSE
              GO TO 110
            END IF
          END IF
          IF ( U.YUNITS .NE. S.YUNITS ) CALL MSG_WRITE(
     &       '*** WARNING - Inconsistent Y-units', REPORT )
          IF ( U.XUNITS .NE. S.XUNITS ) CALL MSG_WRITE(
     &       '*** WARNING - Inconsistent X-units', REPORT )

          ! Merge UDS fields
          CALL UDS_MERGE( U, S )

          DONE = .TRUE.
        END IF

      END DO

      ! Compute RMS difference
      RMS = 0.D0
      DO I = S.NFP, S.NLP
        RMS = RMS + ( DBLE( S.Y(I) ) - U.Y(I) )**2
      END DO
      RMS = SQRT( RMS / ( S.NLP - S.NFP + 1 ) )

      ! Report results
      WRITE( *, '(/5X,A,1PG15.6,A)' )
     & 'RMS Difference = ', RMS,
     & ' ('//S.YUNITS(:L_TRIM(S.YUNITS))//')'
      IF ( REPORT.OPEN ) WRITE( REPORT.UNIT, '(/5X,A,1PG15.6,A)' )
     & 'RMS Difference = ', RMS,
     & ' ('//S.YUNITS(:L_TRIM(S.YUNITS))//')'

      ! Loop for next operation
  190 WRITE( *, '(80(''_''))' )
      GO TO 100

      ! Clean up and stop
  199 CALL CLEAN_UP( HIT, REPORT, OUTPUT_LIST, DIR_OUT )

      END
