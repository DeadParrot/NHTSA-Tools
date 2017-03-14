      PROGRAM RESULT

!***********************************************************************
!* Resultant of UDS Y-Series Files
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

      RECORD /UDS_Y/  S, U
      RECORD /UDS_CONTROL/  C
      RECORD /FILE/  HIT, REPORT, OUTPUT_LIST

      LOGICAL ALL_BATCH, NO_BATCH, NO_INCR, ONE_PASS, NEW_HIT, EOF,
     & BATCH, DONE

      INTEGER NUM_ARGS, MAX_ARGS, IOS, I, IX, IY,
     & NUM_FILES, NPER_CL, NPER

      PARAMETER ( MAX_ARGS = 12 )

      CHARACTER CL_ARG(0:MAX_ARGS)*255, PROG_NAME*8,
     & TEMPLATE_STR*255, DIR_OUT*255,
     & FILE_NAME*255, OUT_FILE*255, DEF_FILE*255,
     & PROMPT_1*79, PROMPT_2*79, PROMPT_3*79, PROMPT_O*79,
     & XUNITS_1*20, YUNITS_1*20, NPER_C, AXES*3, AXIS, CSYS


      ! Initializations
      PROG_NAME = 'Result'
      CALL FILE_INIT( HIT )
      CALL GET_CL( CL_ARG, NUM_ARGS, MAX_ARGS )
      CALL RESULT_CL( CL_ARG, NUM_ARGS, PROG_NAME, NPER_CL )
      CALL PROC_CL( CL_ARG, NUM_ARGS, PROG_NAME, TEMPLATE_STR,
     & ALL_BATCH, NO_BATCH, NO_INCR, REPORT, OUTPUT_LIST, DIR_OUT,
     & ONE_PASS, .TRUE., .TRUE. )
      PROMPT_1 = 'First UDS file name/template/hitlist?'
      PROMPT_1(74:) = '[done]'
      PROMPT_2 = 'Second UDS file name/template/hitlist?'
      PROMPT_2(71:) = '[no more]'
      PROMPT_3 = 'Third UDS file name/template/hitlist?'
      PROMPT_3(71:) = '[no more]'
      CALL UDS_CONTROL_INIT( C )
      C.CHANFORM = 'Y' ! Only accept Y-series file input

      WRITE( *, '(//25X,A//)' ) '**  Resultant of UDS Files  **'

      ! Read the first file name/template/hitlist
  100 CALL FILE_PROMPT( TEMPLATE_STR, PROMPT_1, DIR_OUT,
     & ONE_PASS, HIT, NEW_HIT, FILE_NAME, EOF )
      IF ( ( FILE_NAME .EQ. ' ' ) .OR. ( EOF ) ) GO TO 199 ! Done

      ! Batch setup
      CALL BAT_SET( ALL_BATCH, NO_BATCH, NEW_HIT, HIT, BATCH )

      ! Set number of files per resultant
      IF ( NPER_CL .NE. 0 ) THEN ! Use command line value
        NPER = NPER_CL
      ELSE IF ( ( NEW_HIT ) .AND. ( HIT.OPEN ) ) THEN ! Prompt for value
        IF ( HIT.N_REC .LE. 0 ) THEN
          NPER = 1
        ELSE
  101     IF ( ( ONE_PASS ) .AND. ( HIT.N_REC .EQ. 1 ) ) THEN
            WRITE( *, '(/A,38X,A/'' >> '',$)' )
     &         ' Number of files per resultant?   (1/2)', '[2]'
          ELSE
            WRITE( *, '(/A,36X,A/'' >> '',$)' )
     &         ' Number of files per resultant?   (1/2/3)', '[3]'
          END IF
          READ( *, '(A)', IOSTAT=IOS ) NPER_C
          IF ( IOS .NE. 0 ) THEN
            CALL MSG_WRITE( '*** Operation skipped', REPORT )
            CALL FILE_DEL( HIT )
            GO TO 190
          END IF
          IF ( NPER_C .EQ. ' ' ) THEN
            NPER = 3
            IF ( ONE_PASS ) NPER = MIN( 3, HIT.N_REC + 1 )
          ELSE
            READ( NPER_C, '(BN,I1)', IOSTAT=IOS ) NPER
            IF ( ( IOS .NE. 0 ) .OR. ( NPER .LE. 0 ) .OR.
     &         ( NPER .GT. 3 ) .OR.
     &         ( ( ONE_PASS ) .AND. ( NPER .GT. HIT.N_REC + 1 ) ) )
     &         THEN
              WRITE( *, * ) '*** Unacceptable value'
              GO TO 101
            END IF
          END IF
        END IF
      ELSE IF ( .NOT. HIT.OPEN ) THEN ! Allow up to 3 for interactive
        IF ( ONE_PASS ) THEN ! Only 1 file entered
          NPER = 1
        ELSE
          NPER = 3
        END IF
      END IF

      ! Read first UDS file
      CALL REP_COLS( REPORT )
      C.DIMSYS = ' '
      CALL UDS_GET( FILE_NAME, S, C, REPORT, IOS )
      IF ( IOS .NE. 0 ) THEN ! UDS read error
        IF ( HIT.OPEN ) THEN
          DO I = 1, NPER-1
            CALL FILE_SKIP( HIT )
          END DO
        END IF
        GO TO 190
      END IF

      ! Process first UDS file
      XUNITS_1 = S.XUNITS
      YUNITS_1 = S.YUNITS
      C.DIMSYS = S.DIMSYS
      NUM_FILES = 1
      AXIS = S.AXIS(1:1)
      IF ( .NOT. ANY_CHARS( AXIS, 'XYZ' ) ) CALL MSG_WRITE(
     & '*** WARNING - Unrecognized axis: '//AXIS, REPORT )
      AXES = AXIS
      CSYS = S.AXIS(2:2)

      ! Square first curve
      DO I = S.NFP, S.NLP
        S.Y(I) = S.Y(I)**2
      END DO

      ! Read/process the other UDS files
      DONE = .FALSE.
      PROMPT_O = PROMPT_2
      DO WHILE ( ( .NOT. DONE ) .AND. ( NUM_FILES .LT. NPER ) )

        ! Read another file name/template/hitlist
  110   CALL FILE_PROMPT( TEMPLATE_STR, PROMPT_O, DIR_OUT,
     &     ONE_PASS, HIT, NEW_HIT, FILE_NAME, EOF )

        IF ( EOF ) THEN
          GO TO 190
        ELSE IF ( FILE_NAME .EQ. ' ' ) THEN ! No more files
          IF ( ONE_PASS ) THEN
            CALL MSG_WRITE(
     &         '*** Operation skipped - Not enough files', REPORT )
            GO TO 190
          ELSE ! Accept < NPER files at user request
            DONE=.TRUE.
          END IF
        ELSE ! Read/process a file

          ! Read next UDS file
          CALL UDS_GET( FILE_NAME, U, C, REPORT, IOS )
          IF ( IOS .NE. 0 ) THEN ! UDS read error
            IF ( ( HIT.OPEN ) .OR. ( ONE_PASS ) ) THEN ! Skip to next
              DO I = 1, NPER-NUM_FILES-1
                CALL FILE_SKIP( HIT )
              END DO
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
              DO I = 1, NPER-NUM_FILES-1
                CALL FILE_SKIP( HIT )
              END DO
              GO TO 190
            ELSE
              GO TO 110
            END IF
          END IF
          IF ( U.YUNITS .NE. YUNITS_1 ) CALL MSG_WRITE(
     &       '*** WARNING - Inconsistent Y-units', REPORT )
          IF ( U.XUNITS .NE. XUNITS_1 ) CALL MSG_WRITE(
     &       '*** WARNING - Inconsistent X-units', REPORT )
          AXIS = U.AXIS(1:1)
          IF ( .NOT. ANY_CHARS( AXIS, 'XYZ' ) ) CALL MSG_WRITE(
     &       '*** WARNING - Unrecognized axis: '//U.AXIS, REPORT )
          IF ( U.AXIS(2:2) .NE. CSYS ) CALL MSG_WRITE(
     &       '*** WARNING - Inconsistent axis coordinate system',
     &       REPORT )
          IF ( INDEX( AXES, AXIS ) .NE. 0 ) CALL MSG_WRITE(
     &       '*** WARNING - Repeat axis: '//AXIS, REPORT )

          ! Merge UDS fields
          CALL UDS_MERGE( U, S )

          ! Add curve to squared sum
          DO I = S.NFP, S.NLP
            S.Y(I) = S.Y(I) + U.Y(I)**2
          END DO

          ! Update for this file
          NUM_FILES = NUM_FILES + 1
          AXES(NUM_FILES:NUM_FILES) = U.AXIS(1:1)

          ! Set up for next file
          IF ( NUM_FILES .EQ. 2 ) THEN
            PROMPT_O = PROMPT_3
          ELSE
            DONE = .TRUE.
          END IF
        END IF

      END DO

      ! Take square root to get resultant
      DO I = S.NFP, S.NLP
        S.Y(I) = SQRT( S.Y(I) )
      END DO

      ! Prepare output fields
      S.CURNAM = 'RES'
      S.STATUS = 'COMPUTED'
      S.HIC = 0.
      S.T1 = 0.
      S.T2 = 0.
      S.HICDTUP = 0.
      S.CLIP3M = 0.
      S.CSI = 0.
      IF ( S.AXIS .EQ. ' ' ) S.AXIS = 'RS'
      IX = INDEX( AXES, 'X' )
      IF ( IX .GT. 1 ) THEN
        AXES(IX:IX) = AXES(1:1)
        AXES(1:1) = 'X'
      END IF
      IY = INDEX( AXES, 'Y' )
      IF ( IY .GT. 2 ) THEN
        AXES(IY:IY) = AXES(2:2)
        AXES(2:2) = 'Y'
      END IF
      S.CD2 = 'Resultant of '//AXES//' axis curves'

      ! Set default output file name
      CALL FN_DEF( S, OUT_FILE )
      CALL FN_COMBO( S, OUT_FILE )
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
      CALL UDS_PUT( OUT_FILE, S, C, REPORT, OUTPUT_LIST,
     & DIR_OUT, .TRUE., IOS )

      ! Loop for next operation
  190 WRITE( *, '(80(''_''))' )
      GO TO 100

      ! Clean up and stop
  199 CALL CLEAN_UP( HIT, REPORT, OUTPUT_LIST, DIR_OUT )

      END



      SUBROUTINE RESULT_CL( CL_ARG, NUM_ARGS, PROG_NAME, NPER_CL )

!***********************************************************************
!* Processes the Program-Specific Command Line Arguments
!***********************************************************************

      ! Headers
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) CL_ARG(0:*) ! Command line argument array

      INTEGER NUM_ARGS ! Number of arguments

      CHARACTER*(*) PROG_NAME ! Program name

      INTEGER NPER_CL ! Number of files per resultant (1-3) [3]


      ! Variables ______________________________________________________

      INTEGER I, NS, IOS

      CHARACTER ARG*255, C


      ! Process command line arguments
      NPER_CL = 0
      DO I = 1, NUM_ARGS
        CALL STR_UPCASE( ARG, CL_ARG(I) )
        C = ARG(1:1)
        IF ( ( C .EQ. '-' ) .OR. ( C .EQ. '/' ) ) THEN
          NS = 2
        ELSE
          NS = 1
        END IF
        IF ( ( ARG(NS:) .EQ. '?' ) .OR.
     &     ( ARG(NS:) .EQ. '??' ) .OR.
     &     ( ARG(NS:) .EQ. 'HELP' ) ) THEN ! Show command line syntax
          CALL SYNTAX_CL( PROG_NAME )
          WRITE( *, '(10X,A)' ) '[NPER=<num_per>]'
          IF ( ( ARG(NS:) .EQ. '??' ) .OR.
     &       ( ARG(NS:) .EQ. 'HELP' ) ) THEN ! Show command line help
            CALL HELP_CL( PROG_NAME )
            WRITE( *, '(10X,A)' )
     &         '<num_per> :  Number of files per resultant (1-3) [3]'
          END IF
          STOP ' '
        ELSE IF ( ( ARG(NS:NS+4) .EQ. 'NPER=' ) .OR.
     &     ( ARG(NS:NS+4) .EQ. 'NPER#' ) ) THEN
          READ( ARG(NS+5:), '(BN,I25)', IOSTAT=IOS ) NPER_CL
          IF ( ( IOS .NE. 0 ) .OR. ( NPER_CL .LE. 0 ) .OR.
     &       ( NPER_CL .GT. 3 ) ) THEN
            WRITE( *, '(/2A)', IOSTAT=IOS )
     &         ' *** Illegal number of files per resultant: ',
     &         ARG(NS+5:L_TRIM(ARG))
            NPER_CL = 0
          END IF
          CL_ARG(I) = ' '
        ELSE IF ( ( ARG(NS:NS+2) .EQ. 'NF=' ) .OR.
     &     ( ARG(NS:NS+2) .EQ. 'NF#' ) ) THEN ! Support old switch
          READ( ARG(NS+3:), '(BN,I5)', IOSTAT=IOS ) NPER_CL
          IF ( ( IOS .NE. 0 ) .OR. ( NPER_CL .LE. 0 ) .OR.
     &       ( NPER_CL .GT. 3 ) ) THEN
            WRITE( *, '(/2A)', IOSTAT=IOS )
     &         ' *** Illegal number of files per resultant: ',
     &         ARG(NS+3:L_TRIM(ARG))
            NPER_CL = 0
          END IF
          CL_ARG(I) = ' '
        END IF
      END DO

      RETURN
      END
