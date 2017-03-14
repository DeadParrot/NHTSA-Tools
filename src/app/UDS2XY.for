      PROGRAM UDS2XY

!***********************************************************************
!* Combines Two UDS Y-Series Files into a UDS X-Y File
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2003/07/18
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'
      INCLUDE 'uds_fxn.fi'
      INCLUDE 'file.fi'


      ! Variables ______________________________________________________

      RECORD /UDS/  U
      RECORD /UDS_SPEC/  S
      RECORD /UDS_CONTROL/  CR, CW
      RECORD /FILE/  HIT, REPORT, OUTPUT_LIST

      LOGICAL ALL_BATCH, NO_BATCH, NO_INCR, ONE_PASS, NEW_HIT, EOF,
     & BATCH, DONE

      INTEGER NUM_ARGS, MAX_ARGS, IOS, I

      PARAMETER ( MAX_ARGS = 12 )

      CHARACTER CL_ARG(0:MAX_ARGS)*255, PROG_NAME*8,
     & TEMPLATE_STR*255, DIR_OUT*255,
     & FILE_NAME*255, OUT_FILE*255, DEF_FILE*255,
     & PROMPT_1*79, PROMPT_2*79


      ! Initializations
      PROG_NAME = 'UDS2XY'
      CALL FILE_INIT( HIT )
      CALL GET_CL( CL_ARG, NUM_ARGS, MAX_ARGS )
      CALL PROC_CL( CL_ARG, NUM_ARGS, PROG_NAME, TEMPLATE_STR,
     & ALL_BATCH, NO_BATCH, NO_INCR, REPORT, OUTPUT_LIST, DIR_OUT,
     & ONE_PASS, .TRUE., .TRUE. )
      PROMPT_1 = 'X-axis UDS file name/template/hitlist?'
      PROMPT_1(74:) = '[done]'
      PROMPT_2 = 'Y-axis UDS file name/template/hitlist?'
      CALL UDS_CONTROL_INIT( CR )
      CALL UDS_CONTROL_INIT( CW )
      CR.CHANFORM = 'Y' ! Only accept Y-series file input

      WRITE( *, '(//20X,A//)' )
     & '**  Merge UDS Y Files into X-Y Files  **'

      ! Read the first file name/template/hitlist
  100 CALL FILE_PROMPT( TEMPLATE_STR, PROMPT_1, DIR_OUT,
     & ONE_PASS, HIT, NEW_HIT, FILE_NAME, EOF )
      IF ( ( FILE_NAME .EQ. ' ' ) .OR. ( EOF ) ) GO TO 199 ! Done

      ! Batch setup
      CALL BAT_SET( ALL_BATCH, NO_BATCH, NEW_HIT, HIT, BATCH )

      ! Read first UDS file
      CALL REP_COLS( REPORT )
      CR.DIMSYS = ' '
      CALL UDS_GET( FILE_NAME, U, CR, REPORT, IOS )
      IF ( IOS .NE. 0 ) THEN ! UDS read error
        IF ( HIT.OPEN ) CALL FILE_SKIP( HIT ) ! Skip file
        GO TO 190
      END IF

      ! Process first UDS file
      CR.DIMSYS = U.DIMSYS
      CALL UDS_SPEC_COPY( S, U )
      DO I = U.NFP, U.NLP ! Load X array data
        U.X(I) = U.Y(I)
      END DO

      ! Read/process the other UDS files
      DONE = .FALSE.
      DO WHILE ( .NOT. DONE )

        ! Read another file name/template/hitlist
  110   CALL FILE_PROMPT( TEMPLATE_STR, PROMPT_2, DIR_OUT,
     &     ONE_PASS, HIT, NEW_HIT, FILE_NAME, EOF )

        IF ( EOF ) THEN
          GO TO 190
        ELSE IF ( FILE_NAME .EQ. ' ' ) THEN
          IF ( ONE_PASS ) THEN
            CALL MSG_WRITE(
     &         '*** Operation skipped - No Y-axis file', REPORT )
            GO TO 190
          ELSE
            GO TO 110
          END IF
        ELSE ! Read/process a file

          ! Read next UDS file
          CALL UDS_GET( FILE_NAME, U, CR, REPORT, IOS )
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
          IF ( U.XUNITS .NE. S.XUNITS ) CALL MSG_WRITE(
     &       '*** WARNING - Inconsistent X-units', REPORT )

          ! Merge UDS fields
          S.XTYP = S.YTYP
          S.XUNITS = S.YUNITS
          S.YTYP = U.YTYP
          S.YUNITS = U.YUNITS
          CALL UDS_MERGE( S, U )
          U.XTYP = S.XTYP
          U.XUNITS = S.XUNITS

          DONE = .TRUE.
        END IF

      END DO

      ! Prepare output fields
      U.FILEFORM = 'X-Y'
      U.CHANFORM = 'X-Y'
      IF ( U.CURNAM .EQ. ' ' ) U.CURNAM = 'X-Y'
      U.CURTYP = 'X-Y'
      U.HIC = 0.
      U.T1 = 0.
      U.T2 = 0.
      U.HICDTUP = 0.
      U.CLIP3M = 0.
      U.CSI = 0.

      ! Set default output file name
      CALL FN_DEF( U, OUT_FILE )
      CALL FN_COMBO( U, OUT_FILE )
      CALL FN_FLAG( OUT_FILE, FN_TYPE( U.XTYP ), 2, I )
      CALL ADD_PATH( DIR_OUT, OUT_FILE )
      IF ( .NOT. NO_INCR ) CALL FN_INCR( OUT_FILE )
      CALL FN_NAME_EXTRACT( OUT_FILE )
      DEF_FILE = OUT_FILE

      ! Set the output UDS file name
      IF ( .NOT. BATCH ) THEN
  120   WRITE( *, '(/A/3A/'' >> '',$)' )
     &     ' Output UDS file name?',
     &     ' [', DEF_FILE(:L_TRIM(DEF_FILE)), ']'
        READ( *, '(A)', END=190 ) OUT_FILE
        IF ( OUT_FILE .EQ. ' ' ) THEN ! Use default file name
          OUT_FILE = DEF_FILE
        ELSE ! Check file name validity
          CALL ADD_PATH( DIR_OUT, OUT_FILE )
          IF ( .NOT. FN_VALID( OUT_FILE ) ) THEN
            WRITE( *, * ) '*** Invalid file name'
            GO TO 120
          END IF
        END IF
      ELSE ! Batch
        OUT_FILE = DEF_FILE
      END IF

      ! Write the output UDS file
      CALL UDS_PUT( OUT_FILE, U, CW, REPORT, OUTPUT_LIST,
     & DIR_OUT, .TRUE., IOS )

      ! Loop for next operation
  190 WRITE( *, '(80(''_''))' )
      GO TO 100

      ! Clean up and stop
  199 CALL CLEAN_UP( HIT, REPORT, OUTPUT_LIST, DIR_OUT )

      END
