      PROGRAM SUM

!***********************************************************************
!* Sums UDS Y-Series Files
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

      INTEGER NUM_ARGS, MAX_ARGS, IOS, IOS_W, I, IP,
     & NUM_FILES, NPER_CL

      PARAMETER ( MAX_ARGS = 12 )

      CHARACTER CL_ARG(0:MAX_ARGS)*255, PROG_NAME*8,
     & TEMPLATE_STR*255, DIR_OUT*255,
     & FILE_NAME*255, OUT_FILE*255, DEF_FILE*255,
     & PROMPT_1*79, PROMPT_2*79,
     & XUNITS_1*20, YUNITS_1*20, LCROW, LCCOL*2, RC_FLAG


      ! Initializations
      PROG_NAME = 'Sum'
      CALL FILE_INIT( HIT )
      CALL GET_CL( CL_ARG, NUM_ARGS, MAX_ARGS )
      CALL SUM_CL( CL_ARG, NUM_ARGS, PROG_NAME, NPER_CL )
      CALL PROC_CL( CL_ARG, NUM_ARGS, PROG_NAME, TEMPLATE_STR,
     & ALL_BATCH, NO_BATCH, NO_INCR, REPORT, OUTPUT_LIST, DIR_OUT,
     & ONE_PASS, .TRUE., .TRUE. )
      PROMPT_1 = 'First UDS file name/template/hitlist?'
      PROMPT_1(74:) = '[done]'
      PROMPT_2 = 'Next UDS file name/template/hitlist?'
      PROMPT_2(71:) = '[no more]'
      CALL UDS_CONTROL_INIT( C )
      C.CHANFORM = 'Y' ! Only accept Y-series file input

      WRITE( *, '(//29X,A//)' ) '**  Sum UDS Files  **'

      ! Read the first file name/template/hitlist
  100 CALL FILE_PROMPT( TEMPLATE_STR, PROMPT_1, DIR_OUT,
     & ONE_PASS, HIT, NEW_HIT, FILE_NAME, EOF )
      IF ( ( FILE_NAME .EQ. ' ' ) .OR. ( EOF ) ) GO TO 199 ! Done

      ! Batch setup
      CALL BAT_SET( ALL_BATCH, NO_BATCH, NEW_HIT, HIT, BATCH )

      ! Read first UDS file
      CALL REP_COLS( REPORT )
      C.DIMSYS = ' '
      CALL UDS_GET( FILE_NAME, S, C, REPORT, IOS )
      IF ( IOS .NE. 0 ) THEN ! UDS read error
        IF ( HIT.OPEN ) CALL FILE_DEL( HIT )
        GO TO 190
      END IF

      ! Process first UDS file
      XUNITS_1 = S.XUNITS
      YUNITS_1 = S.YUNITS
      C.DIMSYS = S.DIMSYS
      NUM_FILES = 1
      IF ( S.SENATT(:10) .EQ. 'LOAD CELL ' ) THEN
        LCROW = S.SENATT(11:11)
        LCCOL = S.SENATT(12:13)
      END IF

      ! Read/process the other UDS files
      DONE = .FALSE.
      DO WHILE ( ( .NOT. DONE ) .AND.
     & ( ( NUM_FILES .LT. NPER_CL ) .OR. ( NPER_CL .EQ. 0 ) ) )

        ! Read another file name/template/hitlist
  110   CALL FILE_PROMPT( TEMPLATE_STR, PROMPT_2, DIR_OUT,
     &     ( ONE_PASS .OR. HIT.OPEN ), HIT, NEW_HIT, FILE_NAME, EOF )

        IF ( EOF ) THEN
          GO TO 190
        ELSE IF ( FILE_NAME .EQ. ' ' ) THEN ! No more files
          DONE = .TRUE.
        ELSE ! Read/process a file

          ! Read next UDS file
          CALL UDS_GET( FILE_NAME, U, C, REPORT, IOS )
          IF ( IOS .NE. 0 ) THEN ! UDS read error
            IF ( ( HIT.OPEN ) .OR. ( ONE_PASS ) ) THEN ! Skip to next
              CALL FILE_DEL( HIT )
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
              CALL FILE_DEL( HIT )
              GO TO 190
            ELSE
              GO TO 110
            END IF
          END IF
          IF ( U.YUNITS .NE. YUNITS_1 ) CALL MSG_WRITE(
     &       '*** WARNING - Inconsistent Y-units', REPORT )
          IF ( U.XUNITS .NE. XUNITS_1 ) CALL MSG_WRITE(
     &       '*** WARNING - Inconsistent X-units', REPORT )

          ! Set load cell flags
          IF ( U.SENATT(:10) .EQ. 'LOAD CELL ' ) THEN
            IF ( U.SENATT(11:11) .NE. LCROW ) LCROW = ' '
            IF ( U.SENATT(12:13) .NE. LCCOL ) LCCOL = ' '
          END IF

          ! Merge UDS fields
          CALL UDS_MERGE( U, S )

          ! Add curve
          DO I = S.NFP, S.NLP
            S.Y(I) = S.Y(I) + U.Y(I)
          END DO

          ! Update for this file
          NUM_FILES = NUM_FILES + 1
        END IF

      END DO

      ! Prepare output fields
      S.CURNAM = 'SUM'
      S.STATUS = 'COMPUTED'
      S.HIC = 0.
      S.T1 = 0.
      S.T2 = 0.
      S.HICDTUP = 0.
      S.CLIP3M = 0.
      S.CSI = 0.
      S.CD2 = 'Sum of       curves'
      WRITE( S.CD2(8:12), '(I5)', IOSTAT=IOS_W ) NUM_FILES

      ! Set default output file name
      CALL FN_DEF( S, OUT_FILE )
      CALL FN_COMBO( S, OUT_FILE )
      IP = FE_POSN( OUT_FILE )
      IF ( S.SENATT .EQ. 'LOAD CELL' ) THEN
        OUT_FILE(IP+2:IP+2) = 'l'
        IF ( LCROW .NE. ' ' ) THEN
          RC_FLAG = LCROW
        ELSE IF ( LCCOL .NE. ' ' ) THEN
          RC_FLAG = LCCOL(1:1)
          IF ( LCCOL(2:2) .EQ. '0' ) RC_FLAG = '0'
        ELSE
          RC_FLAG = 't'
        END IF
        OUT_FILE(IP+3:IP+3) = RC_FLAG
      END IF
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
      IF ( IOS .NE. 0 ) GO TO 190
      WRITE( *, '(/A,I6,A)', IOSTAT=IOS )
     & ' *** ', NUM_FILES, ' files summed'

      ! Loop for next operation
  190 WRITE( *, '(80(''_''))' )
      GO TO 100

      ! Clean up and stop
  199 CALL CLEAN_UP( HIT, REPORT, OUTPUT_LIST, DIR_OUT )

      END



      SUBROUTINE SUM_CL( CL_ARG, NUM_ARGS, PROG_NAME, NPER_CL )

!***********************************************************************
!* Processes the Program-Specific Command Line Arguments
!***********************************************************************

      ! Headers
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) CL_ARG(0:*) ! Command line argument array

      INTEGER NUM_ARGS ! Number of arguments

      CHARACTER*(*) PROG_NAME ! Program name

      INTEGER NPER_CL ! Number of files per sum [all]


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
     &         '<num_per> :  Number of files per sum [all]'
          END IF
          STOP ' '
        ELSE IF ( ( ARG(NS:NS+4) .EQ. 'NPER=' ) .OR.
     &     ( ARG(NS:NS+4) .EQ. 'NPER#' ) ) THEN
          READ( ARG(NS+5:), '(BN,I25)', IOSTAT=IOS ) NPER_CL
          IF ( ( IOS .NE. 0 ) .OR. ( NPER_CL .LE. 0 ) ) THEN
            WRITE( *, '(/2A)', IOSTAT=IOS )
     &         ' *** Illegal number of files per sum: ',
     &         ARG(NS+5:L_TRIM(ARG))
            NPER_CL = 0
          END IF
          CL_ARG(I) = ' '
        END IF
      END DO

      RETURN
      END
