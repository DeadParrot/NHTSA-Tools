      PROGRAM RMBIAS

!***********************************************************************
!* Removes Bias from UDS Files Based on Either Signal Tail
!*
!* Language: Fortran
!*
!* Author: (unknown)
!*         Stuart G. Mentzer
!*
!* Date: 2003/07/18
!*
!* History:
!*  1999/07/19 Andrew Orndorff
!*             Changed to support bias removal algorithm from FIR100
!*  1999/08/05 Andrew Orndorff
!*             Changed to support both bias removal algorithms with the option
!*             to choose based on command line arguments or interactive inputs
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'
      INCLUDE 'uds_fxn.fi'
      INCLUDE 'file.fi'


      ! Variables ______________________________________________________

      RECORD /UDS/  U
      RECORD /UDS_CONTROL/  C
      RECORD /FILE/  HIT, REPORT, OUTPUT_LIST

      LOGICAL ALL_BATCH, NO_BATCH, NO_INCR, ONE_PASS, NEW_HIT, EOF,
     & BATCH, GROUP, NEW_GROUP, NO_FLAG, DEF_PERCENT,
     & DEF_ALGORITHM, DEF_TAIL

      INTEGER NUM_ARGS, MAX_ARGS, IOS, I, NPTS

      PARAMETER ( MAX_ARGS = 12 )

      REAL S(NFP_L-25:NLP_U+25), PERCENT, TAIL_SUM,
     & FIRST_DATA_POINT, BIAS_REMOVED

      CHARACTER CL_ARG(0:MAX_ARGS)*255, PROG_NAME*8,
     & TEMPLATE_STR*255, DIR_OUT*255,
     & FILE_NAME*255, OUT_FILE*255, DEF_FILE*255,
     & PROMPT*79, TAIL, ALGORITHM, PERCENT_C*25,
     & MESSAGE*132


      ! Initializations
      PROG_NAME = 'RmBias'
      CALL FILE_INIT( HIT )
      CALL GET_CL( CL_ARG, NUM_ARGS, MAX_ARGS )
      CALL RMBIAS_CL( CL_ARG, NUM_ARGS, PROG_NAME,
     & NO_FLAG, DEF_ALGORITHM, ALGORITHM, DEF_TAIL, TAIL,
     & DEF_PERCENT, PERCENT )
      CALL PROC_CL( CL_ARG, NUM_ARGS, PROG_NAME, TEMPLATE_STR,
     & ALL_BATCH, NO_BATCH, NO_INCR, REPORT, OUTPUT_LIST, DIR_OUT,
     & ONE_PASS, .TRUE., .TRUE. )
      PROMPT = 'UDS file name/template/hitlist?'
      PROMPT(74:) = '[done]'
      CALL UDS_CONTROL_INIT( C )

      WRITE( *, '(//23X,A//)' ) '**  UDS Signal Bias Removal  **'

      ! Read the file name/template/hitlist
  100 CALL FILE_PROMPT( TEMPLATE_STR, PROMPT, DIR_OUT,
     & ONE_PASS, HIT, NEW_HIT, FILE_NAME, EOF )
      IF ( ( FILE_NAME .EQ. ' ' ) .OR. ( EOF ) ) GO TO 199 ! Done

      ! Batch setup
      CALL BAT_SET( ALL_BATCH, NO_BATCH, NEW_HIT, HIT, BATCH )
      CALL GROUP_SET( HIT, NEW_HIT, BATCH, 2, GROUP, NEW_GROUP )

      ! Group prompts
      IF ( NEW_GROUP ) THEN

        ! Choose algorithm to use
  101   IF ( DEF_ALGORITHM ) THEN
          ALGORITHM = 'B'
        ELSE IF ( .NOT. ANY_CHARS( ALGORITHM, ' AB' ) ) THEN
          WRITE( *, '(/2A,7X,A/'' >> '',$)' )
     &       ' Algorithm to use for calculating bias?   ',
     &       '( B - Basic,  A - Auto )', '[Basic]'
          READ( *, '(A)', IOSTAT=IOS ) ALGORITHM
          IF ( IOS .NE. 0 ) THEN
            CALL MSG_WRITE( '*** Operation skipped', REPORT )
            CALL FILE_DEL( HIT )
            GO TO 190
          END IF
          CALL STR_UP( ALGORITHM )
          IF ( ALGORITHM .EQ. ' ' ) THEN
            ALGORITHM = 'B'
          ELSE IF ( .NOT. ANY_CHARS( ALGORITHM, 'AB' ) ) THEN
            WRITE( *, * ) '*** Invalid response'
            GO TO 101
          END IF
        END IF

        IF ( ALGORITHM .EQ. 'B' ) THEN

          ! Choose tail to use
  102     IF ( DEF_TAIL ) THEN
            TAIL = 'S'
          ELSE IF ( .NOT. ANY_CHARS( TAIL, ' SE' ) ) THEN
            WRITE( *, '(/2A,15X,A/'' >> '',$)' )
     &         ' Signal tail for computing bias?   ',
     &         '( S - Start,  E - End )', '[Start]'
            READ( *, '(A)', IOSTAT=IOS ) TAIL
            IF ( IOS .NE. 0 ) THEN
              CALL MSG_WRITE( '*** Operation skipped', REPORT )
              CALL FILE_DEL( HIT )
              GO TO 190
            END IF
            CALL STR_UP( TAIL )
            IF ( TAIL .EQ. ' ' ) THEN
              TAIL = 'S'
            ELSE IF ( .NOT. ANY_CHARS( TAIL, 'SE' ) ) THEN
              WRITE( *, * ) '*** Invalid response'
              GO TO 102
            END IF
          END IF

          ! Choose signal percent
  103     IF ( DEF_PERCENT ) THEN
            PERCENT = 5.0
          ELSE IF ( ( PERCENT .LE. 0.0 ) .OR.
     &       ( PERCENT .GT. 100.0 ) ) THEN
            WRITE( *, '(/A,47X,A/'' >> '',$)' )
     &         ' Percent of signal to examine?', '[5]'
            READ( *, '(A)', END=190 ) PERCENT_C
            IF ( PERCENT_C .EQ. ' ' ) THEN
              PERCENT = 5.0
            ELSE
              READ( PERCENT_C, '(BN,F25.0)', IOSTAT=IOS ) PERCENT
              IF ( ( IOS .NE. 0 ) .OR. ( PERCENT .LE. 0. ) .OR.
     &           ( PERCENT .GT. 100. ) ) THEN
                WRITE( *, * ) '*** Invalid percent'
                GO TO 103
              END IF
            END IF
          END IF
        END IF

      END IF

      ! Read UDS file
      CALL REP_COLS( REPORT )
      CALL UDS_GET( FILE_NAME, U, C, REPORT, IOS )
      IF ( IOS .NE. 0 ) GO TO 190

      ! Check file fields
      IF ( U.CHANFORM .NE. 'Y' ) CALL MSG_WRITE(
     & '*** WARNING - Not a Y-series data channel', REPORT )
      IF ( U.XTYP .NE. 'TIME' ) CALL MSG_WRITE(
     & '*** WARNING - X axis data type is not TIME', REPORT )
      IF ( ( U.YTYP .NE. 'ACCELERATION' ) .AND.
     & ( U.YTYP .NE. 'ANGULAR ACCELERATION' ) .AND.
     & ( U.YTYP .NE. 'FORCE' ) .AND.
     & ( U.YTYP .NE. 'TORQUE' ) .AND.
     & ( U.YTYP .NE. 'PRESSURE' ) ) CALL MSG_WRITE(
     & '*** WARNING - Y axis data type is not typically transient: '//
     & U.YTYP, REPORT )

      IF ( .NOT. GROUP ) THEN ! Interactive prompts

  104   IF ( DEF_ALGORITHM ) THEN
          ALGORITHM = 'B'
        ELSE IF ( .NOT. ANY_CHARS( ALGORITHM, ' AB') ) THEN
          WRITE( *, '(/2A,6X,A/'' >> '',$)' )
     &       ' Algorithm to use for calculating bias ?   ',
     &       '( B - Basic,  A - Auto )', '[Basic]'
          READ( *, '(A)', END=190 ) ALGORITHM
          CALL STR_UP( ALGORITHM )
          IF ( ALGORITHM .EQ. ' ' ) THEN
            ALGORITHM = 'B'
          ELSE IF ( .NOT. ANY_CHARS( ALGORITHM, 'AB' ) ) THEN
            WRITE( *, * ) '*** Invalid response'
            GO TO 104
          END IF
        END IF

        IF ( ALGORITHM .EQ. 'B' ) THEN

          ! Choose tail to use
  105     IF ( DEF_TAIL ) THEN
            TAIL = 'S'
          ELSE IF ( .NOT. ANY_CHARS( TAIL, ' SE') ) THEN
            WRITE( *, '(/2A,15X,A/'' >> '',$)' )
     &         ' Signal tail for computing bias?   ',
     &         '( S - Start,  E - End )', '[Start]'
            READ( *, '(A)', END=190 ) TAIL
            CALL STR_UP( TAIL )
            IF ( TAIL .EQ. ' ' ) THEN
              TAIL = 'S'
            ELSE IF ( .NOT. ANY_CHARS( TAIL, 'SE' ) ) THEN
              WRITE( *, * ) '*** Invalid response'
              GO TO 105
            END IF
          END IF
        END IF

        IF ( U.CHANFORM .EQ. 'Y' ) THEN
          WRITE( MESSAGE, '(5X,3A,F12.6,A,F12.6,3A)', IOSTAT=IOS )
     &       'Signal ', U.XTYP(:L_TRIM(U.XTYP)),
     &       ' range is ', U.NFP * U.DEL, ' to ', U.NLP * U.DEL,
     &       ' (',U.XUNITS(:L_TRIM(U.XUNITS)),')'
        ELSE
          WRITE( MESSAGE, '(5X,A,I7,A,I7)', IOSTAT=IOS )
     &       'Signal index range is ', U.NFP, ' to ', U.NLP
        END IF
        CALL MSG_WRITE( MESSAGE, REPORT )

        IF ( ALGORITHM .EQ. 'B' ) THEN

          ! Choose signal percent
  106     IF ( DEF_PERCENT ) THEN
            PERCENT = 5.0
          ELSE IF ( ( PERCENT .LE. 0.0 ) .OR.
     &       ( PERCENT .GT. 100.0 ) ) THEN
            WRITE( *, '(/A,47X,A/'' >> '',$)' )
     &         ' Percent of signal to examine?', '[5]'
            READ( *, '(A)', END=190 ) PERCENT_C
            IF ( PERCENT_C .EQ. ' ' ) THEN
              PERCENT = 5.0
            ELSE
              READ( PERCENT_C, '(BN,F25.0)', IOSTAT=IOS ) PERCENT
              IF ( ( IOS .NE. 0 ) .OR. ( PERCENT .LE. 0. ) .OR.
     &           ( PERCENT .GT. 100. ) ) THEN
                WRITE( *, * ) '*** Invalid percent'
                GO TO 106
              END IF
            END IF
          END IF

        END IF

      END IF

      ! Perform operation
      IF ( ALGORITHM .EQ. 'B' ) THEN
        NPTS = NINT( ( U.NLP - U.NFP ) * PERCENT / 100. )
        TAIL_SUM = 0.
        IF ( ( TAIL .EQ. 'S' ) .OR. ( TAIL .EQ. ' ' ) )
     &   THEN ! Use start tail

          DO I = U.NFP, U.NFP+NPTS-1
            TAIL_SUM = TAIL_SUM + U.Y(I)
          END DO

        ELSE IF ( TAIL .EQ. 'E' ) THEN ! Use end tail

          DO I = U.NLP, U.NLP-NPTS+1, -1
            TAIL_SUM = TAIL_SUM + U.Y(I)
          END DO

        END IF
        BIAS_REMOVED  = TAIL_SUM / NPTS
        DO I = U.NFP, U.NLP
          U.Y(I) = U.Y(I) - BIAS_REMOVED
        END DO

      ELSE IF ( ALGORITHM .EQ. 'A' ) THEN

        FIRST_DATA_POINT = U.Y(U.NFP)
        CALL BIAS( U.Y(U.NFP), S(U.NFP), ( U.NLP - U.NFP + 1 ) )

        BIAS_REMOVED = FIRST_DATA_POINT - U.Y(U.NFP)

      END IF

      IF ( ALGORITHM .EQ. 'B' ) THEN
        WRITE( MESSAGE, '(A,G20.6,2X,2A)', IOSTAT=IOS )
     &    '*** Basic Bias of ', BIAS_REMOVED,
     &    U.YTYP(:L_TRIM(U.YTYP)), ' removed'
      ELSE IF ( ALGORITHM .EQ. 'A' ) THEN
        WRITE( MESSAGE, '(A,G20.6,2X,2A)', IOSTAT=IOS )
     &    '*** Auto/FIR100 Bias of ', BIAS_REMOVED,
     &    U.YTYP(:L_TRIM(U.YTYP)), ' removed'
      END IF
      CALL MSG_WRITE( MESSAGE, REPORT )

      ! Prepare output fields
      IF ( ALGORITHM .EQ. 'B' ) THEN
        U.STATUS = 'BIAS-CORRECTED-BASIC'
      ELSE IF ( ALGORITHM .EQ. 'A' ) THEN
        U.STATUS = 'BIAS-CORRECTED-AUTO'
      END IF
      U.HIC = 0.
      U.T1 = 0.
      U.T2 = 0.
      U.HICDTUP = 0.
      U.CLIP3M = 0.
      U.CSI = 0.

      ! Set default output file name
      OUT_FILE = FILE_NAME
      CALL FN_EXTRACT( OUT_FILE )
      IF ( .NOT. NO_FLAG ) CALL FN_FLAG( OUT_FILE, 'b', 2, I )
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
      CALL UDS_PUT( OUT_FILE, U, C, REPORT, OUTPUT_LIST,
     & DIR_OUT, .FALSE., IOS )
      IF ( IOS .NE. 0 ) GO TO 190

      ! Loop for next file
  190 WRITE( *, '(80(''_''))' )
      GO TO 100

      ! Clean up and stop
  199 CALL CLEAN_UP( HIT, REPORT, OUTPUT_LIST, DIR_OUT )

      END



      SUBROUTINE RMBIAS_CL( CL_ARG, NUM_ARGS, PROG_NAME,
     & NO_FLAG, DEF_ALGORITHM, ALGORITHM, DEF_TAIL,
     & TAIL, DEF_PERCENT, PERCENT )

!***********************************************************************
!* Processes the Program-Specific Command Line Arguments
!***********************************************************************

      ! Headers
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) CL_ARG(0:*) ! Command line argument array

      INTEGER NUM_ARGS ! Number of arguments

      CHARACTER*(*) PROG_NAME ! Program name

      LOGICAL NO_FLAG ! Indicates not to flag output file names

      LOGICAL DEF_ALGORITHM ! Flag use of default algorithm

      CHARACTER ALGORITHM ! Choice of algorithm to use

      LOGICAL DEF_TAIL ! Flag use of default tail

      CHARACTER TAIL ! Start or end tail for original algorithm

      LOGICAL DEF_PERCENT ! Flag use of default percentage

      REAL PERCENT ! Percent of signal to use in calculation of bias


      ! Variables ______________________________________________________

      INTEGER I, NS, IOS

      CHARACTER ARG*255, C


      ! Process command line arguments
      NO_FLAG = .FALSE.
      DEF_ALGORITHM = .FALSE.
      DEF_TAIL = .FALSE.
      DEF_PERCENT = .FALSE.
      PERCENT = 0.0

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
          WRITE( *, '(10X,A/10X,A)' )
     &       '[NO_FLAG] [ALGORITHM=<BASIC|AUTO>]',
     &       '[TAIL=<START|END>] [PERCENT=<percent>]'
          IF ( ( ARG(NS:) .EQ. '??' ) .OR.
     &       ( ARG(NS:) .EQ. 'HELP' ) ) THEN ! Show command line help
            CALL HELP_CL( PROG_NAME )
            WRITE( *, '(3(10X,A/),10X,A)' )
     &       'NO_FLAG :  Don''t flag output file names',
     &       '<BASIC|AUTO> :  Basic or FIR100 style auto bias removal',
     &       '<START|END> :  Start or End tail of the signal',
     &       '<percent> :  '//
     &       'Percentage of signal to use,( blank => default )'
          END IF
          STOP ' '
        ELSE IF ( ( ARG(NS:) .EQ. 'NO_FLAG' ) .OR.
     &     ( ARG(NS:) .EQ. 'NOFLAG' ) ) THEN
          NO_FLAG = .TRUE.
          CL_ARG(I) = ' '
        ELSE IF ( ( ARG(NS:NS+7) .EQ. 'PERCENT=' ) .OR.
     &     ( ARG(NS:NS+7) .EQ. 'PERCENT#' ) ) THEN
          IF ( ARG(NS+8:) .EQ. ' ' ) THEN ! Default
            DEF_PERCENT = .TRUE.
            PERCENT = 5.D0
          ELSE
            READ( ARG(NS+8:), '(BN,F25.0)', IOSTAT=IOS ) PERCENT
            IF ( ( IOS .NE. 0 ) .OR. ( PERCENT .LE. 0.D0 ) .OR.
     &         ( PERCENT .GT. 100.D0 ) ) THEN
              WRITE( *, '(/2A)', IOSTAT=IOS )
     &           ' Illegal signal percentage: ',
     &           ARG(NS+8:L_TRIM(ARG))
              PERCENT = 5.D0
            ELSE
              DEF_PERCENT = .FALSE.
            END IF
          END IF
          CL_ARG(I) = ' '
        ELSE IF ( ( ARG(NS:NS+9) .EQ. 'ALGORITHM=' ) .OR.
     &     ( ARG(NS:NS+9) .EQ. 'ALGORITHM#' ) ) THEN
          IF ( ARG(NS+10:) .EQ. ' ' ) THEN ! Default
            DEF_ALGORITHM = .TRUE.
            ALGORITHM = 'B'
          ELSE
            ALGORITHM = ARG(NS+10:NS+10)
            IF ( ( ALGORITHM .NE. 'B' ) .AND.
     &         ( ALGORITHM .NE. 'A' ) ) THEN
                WRITE( *, '(/2A)', IOSTAT=IOS )
     &             ' Illegal algorithm: ',
     &             ARG(NS+10:L_TRIM(ARG))
                DEF_ALGORITHM = .TRUE.
                ALGORITHM = 'B'
            ELSE
                DEF_ALGORITHM = .FALSE.
            END IF
          END IF
          CL_ARG(I) = ' '
        ELSE IF ( ( ARG(NS:NS+4) .EQ. 'TAIL=' ) .OR.
     &     ( ARG(NS:NS+4) .EQ. 'TAIL#' ) ) THEN
          IF ( ARG(NS+5:) .EQ. ' ' ) THEN ! Default
            DEF_TAIL = .TRUE.
            TAIL = 'S'
          ELSE
            TAIL = ARG(NS+5:NS+6)
            IF ( ( TAIL(1:1) .NE. 'S' ) .AND.
     &         ( TAIL(1:1) .NE. 'E' ) ) THEN
                WRITE( *, '(/2A)', IOSTAT=IOS )
     &             ' Illegal signal tail: ',
     &             ARG(NS+5:L_TRIM(ARG))
                DEF_TAIL = .TRUE.
                TAIL = 'S'
            ELSE
                DEF_TAIL = .FALSE.
            END IF
          END IF
          CL_ARG(I) = ' '
        ELSE IF ( ARG(NS:) .EQ. 'PERCENT' ) THEN ! Default
          DEF_PERCENT = .TRUE.
          PERCENT = 5.D0
          CL_ARG(I) = ' '
        ELSE IF ( ARG(NS:) .EQ. 'ALGORITHM' ) THEN ! Default
          DEF_ALGORITHM = .TRUE.
          ALGORITHM = 'B'
          CL_ARG(I) = ' '
        ELSE IF ( ARG(NS:) .EQ. 'TAIL' ) THEN ! Default
          DEF_TAIL = .TRUE.
          TAIL = 'S'
          CL_ARG(I) = ' '
        END IF
      END DO

      RETURN
      END
