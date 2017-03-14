      PROGRAM UDS2Y

!***********************************************************************
!* Converts UDS X-Y Files into UDS Y-Series Files
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
      RECORD /UDS_Y/  S
      RECORD /UDS_CONTROL/  CR, CW
      RECORD /FILE/  HIT, REPORT, OUTPUT_LIST

      LOGICAL ALL_BATCH, NO_BATCH, NO_INCR, ONE_PASS, NEW_HIT, EOF,
     & BATCH, GROUP, NEW_GROUP

      INTEGER NUM_ARGS, MAX_ARGS, IOS, I

      PARAMETER ( MAX_ARGS = 12 )

      DOUBLE PRECISION STEP_CL, STEP

      CHARACTER CL_ARG(0:MAX_ARGS)*255, PROG_NAME*8,
     & TEMPLATE_STR*255, DIR_OUT*255,
     & FILE_NAME*255, OUT_FILE*255, DEF_FILE*255,
     & PROMPT*79,
     & Y_OUT_CL, X_OUT_CL, Y_OUT, X_OUT, OTHER_AXIS,
     & INDEP*4, FLAG


      ! Initializations
      PROG_NAME = 'UDS2Y'
      CALL FILE_INIT( HIT )
      CALL GET_CL( CL_ARG, NUM_ARGS, MAX_ARGS )
      CALL UDS2Y_CL( CL_ARG, NUM_ARGS, PROG_NAME,
     & Y_OUT_CL, X_OUT_CL, STEP_CL )
      CALL PROC_CL( CL_ARG, NUM_ARGS, PROG_NAME, TEMPLATE_STR,
     & ALL_BATCH, NO_BATCH, NO_INCR, REPORT, OUTPUT_LIST, DIR_OUT,
     & ONE_PASS, .TRUE., .TRUE. )
      PROMPT = 'UDS X-Y file name/template/hitlist?'
      PROMPT(74:) = '[done]'
      CALL UDS_CONTROL_INIT( CR )
      CALL UDS_CONTROL_INIT( CW )
      CR.CHANFORM = 'X-Y'

      WRITE( *, '(//16X,A//)' )
     & '**  Converts UDS X-Y Files to Y-Series Files  **'

      ! Read the file name/template/hitlist
  100 CALL FILE_PROMPT( TEMPLATE_STR, PROMPT, DIR_OUT,
     & ONE_PASS, HIT, NEW_HIT, FILE_NAME, EOF )
      IF ( ( FILE_NAME .EQ. ' ' ) .OR. ( EOF ) ) GO TO 199 ! Done

      ! Batch/group setup
      CALL BAT_SET( ALL_BATCH, NO_BATCH, NEW_HIT, HIT, BATCH )
      CALL GROUP_SET( HIT, NEW_HIT, BATCH, 2, GROUP, NEW_GROUP )

      ! Group prompts
      IF ( NEW_GROUP ) THEN

        ! Get output type
  101   IF ( Y_OUT_CL .EQ. ' ' ) THEN
          WRITE( *, '(/A,49X,A/'' >> '',$)' )
     &       ' Dependent (y) axis?   (Y|X)', '[Y]'
          READ( *, '(A)', IOSTAT=IOS ) Y_OUT
          IF ( IOS .NE. 0 ) THEN
            CALL MSG_WRITE( '*** Operation skipped', REPORT )
            CALL FILE_DEL( HIT )
            GO TO 190
          END IF
          CALL STR_UP( Y_OUT )
          IF ( Y_OUT .EQ. ' ' ) Y_OUT = 'Y'
          IF ( .NOT. ANY_CHARS( Y_OUT, 'YX' ) ) THEN
            WRITE( *, * ) '*** Invalid response'
            GO TO 101
          END IF
          IF ( Y_OUT .EQ. 'Y' ) THEN
            OTHER_AXIS = 'x'
          ELSE
            OTHER_AXIS = 'y'
          END IF
  102     WRITE( *, '(/A,43X,A/'' >> '',$)' )
     &       ' Independent (x) axis?   ('//OTHER_AXIS//'|t|f|i)',
     &       '[t]'
          READ( *, '(A)', IOSTAT=IOS ) X_OUT
          IF ( IOS .NE. 0 ) THEN
            GO TO 101
          END IF
          CALL STR_UP( X_OUT )
          IF ( X_OUT .EQ. ' ' ) X_OUT = 'T'
          CALL STR_UPCASE( INDEP, OTHER_AXIS//'TFI' )
          IF ( .NOT. ANY_CHARS( X_OUT, INDEP ) ) THEN
            WRITE( *, * ) '*** Invalid response'
            GO TO 102
          END IF
        ELSE
          Y_OUT = Y_OUT_CL
          X_OUT = X_OUT_CL
        END IF

        ! Get output step
  103   IF ( STEP_CL .EQ. 0.D0 ) THEN
          IF ( ANY_CHARS( X_OUT, 'XY' ) ) THEN
            WRITE( *, '(/A/'' >> '',$)' ) ' Step value?'
            READ( *, '(BN,F25.0)', IOSTAT=IOS ) STEP
            IF ( IOS .EQ. -1 ) THEN
              CALL MSG_WRITE( '*** Operation skipped', REPORT )
              CALL FILE_DEL( HIT )
              GO TO 190
            END IF
            IF ( ( IOS .NE. 0 ) .OR. ( STEP .LE. 0.D0 ) ) THEN
              WRITE( *, '(/2A)' ) ' *** Illegal step value'
              GO TO 103
            END IF
          END IF
        ELSE IF ( .NOT. ANY_CHARS( X_OUT, 'XY' ) ) THEN
          WRITE( *, '(A)' )
     &       ' *** Step value ignored for index-based output'
        ELSE
          STEP = STEP_CL
        END IF

      END IF

      ! Read UDS file
      CALL REP_COLS( REPORT )
      CALL UDS_GET( FILE_NAME, U, CR, REPORT, IOS )
      IF ( IOS .NE. 0 ) GO TO 190
      CALL UDS_SPEC_COPY( S, U ) ! Copy input file specs to S

      ! Get output axis
      IF ( .NOT. GROUP ) THEN

        ! Output type
  111   IF ( Y_OUT_CL .EQ. ' ' ) THEN
          WRITE( *, '(/A,49X,A/'' >> '',$)' )
     &       ' Dependent (y) axis?   (Y|X)', '[Y]'
          READ( *, '(A)', IOSTAT=IOS ) Y_OUT
          IF ( IOS .NE. 0 ) THEN
            CALL MSG_WRITE( '*** Operation skipped', REPORT )
            GO TO 190
          END IF
          CALL STR_UP( Y_OUT )
          IF ( Y_OUT .EQ. ' ' ) Y_OUT = 'Y'
          IF ( .NOT. ANY_CHARS( Y_OUT, 'YX' ) ) THEN
            WRITE( *, * ) '*** Invalid response'
            GO TO 111
          END IF
          IF ( Y_OUT .EQ. 'Y' ) THEN
            OTHER_AXIS = 'x'
          ELSE
            OTHER_AXIS = 'y'
          END IF
  112     WRITE( *, '(/A,43X,A/'' >> '',$)' )
     &       ' Independent (x) axis?   ('//OTHER_AXIS//'|t|f|i)',
     &       '[t]'
          READ( *, '(A)', IOSTAT=IOS ) X_OUT
          IF ( IOS .NE. 0 ) THEN
            GO TO 111
          END IF
          CALL STR_UP( X_OUT )
          IF ( X_OUT .EQ. ' ' ) X_OUT = 'T'
          CALL STR_UPCASE( INDEP, OTHER_AXIS//'TFI' )
          IF ( .NOT. ANY_CHARS( X_OUT, INDEP ) ) THEN
            WRITE( *, * ) '*** Invalid response'
            GO TO 112
          END IF
        ELSE
          Y_OUT = Y_OUT_CL
          X_OUT = X_OUT_CL
        END IF

        ! Get output step
  113   IF ( STEP_CL .EQ. 0.D0 ) THEN
          IF ( ANY_CHARS( X_OUT, 'XY' ) ) THEN
            WRITE( *, '(/A/'' >> '',$)' ) ' Step value?'
            READ( *, '(BN,F25.0)', IOSTAT=IOS ) STEP
            IF ( IOS .EQ. -1 ) THEN
              CALL MSG_WRITE( '*** Operation skipped', REPORT )
              GO TO 190
            END IF
            IF ( ( IOS .NE. 0 ) .OR. ( STEP .LE. 0.D0 ) ) THEN
              WRITE( *, '(/2A)' ) ' *** Illegal step value'
              GO TO 113
            END IF
          END IF
        ELSE IF ( .NOT. ANY_CHARS( X_OUT, 'XY' ) ) THEN
          WRITE( *, '(A)' )
     &       ' *** Step value ignored for index-based output'
        ELSE
          STEP = STEP_CL
        END IF

      END IF

      ! Set default output file name
      OUT_FILE = FILE_NAME
      CALL FN_EXTRACT( OUT_FILE )
      IF ( Y_OUT .EQ. 'Y' ) THEN
        FLAG = FN_TYPE( U.YTYP )
      ELSE ! X-axis output
        FLAG = FN_TYPE( U.XTYP )
      END IF
      CALL FN_FLAG( OUT_FILE, FLAG//'c', 3, I )
      CALL ADD_PATH( DIR_OUT, OUT_FILE )
      IF ( .NOT. NO_INCR ) CALL FN_INCR( OUT_FILE )
      CALL FN_NAME_EXTRACT( OUT_FILE )
      DEF_FILE = OUT_FILE

      ! Prepare output fields
      S.HIC = 0.
      S.T1 = 0.
      S.T2 = 0.
      S.HICDTUP = 0.
      S.CLIP3M = 0.
      S.CSI = 0.
      S.FILEFORM = 'Y'
      S.CHANFORM = 'Y'
      IF ( Y_OUT .EQ. 'X' ) THEN
        S.YTYP = U.XTYP
        S.YUNITS = U.XUNITS
        DO I = S.NFP, S.NLP
          S.Y(I) = U.X(I)
        END DO
      ELSE IF ( Y_OUT .EQ. 'Y' ) THEN
        DO I = S.NFP, S.NLP
          S.Y(I) = U.Y(I)
        END DO
      END IF
      IF ( X_OUT .EQ. 'Y' ) THEN
        S.XTYP = U.YTYP
        S.XUNITS = U.YUNITS
      ELSE IF ( X_OUT .EQ. 'T' ) THEN
        S.XTYP = 'TIME'
        S.XUNITS = 'SECONDS'
        S.CURTYP = 'TIME SERIES'
      ELSE IF ( X_OUT .EQ. 'F' ) THEN
        S.XTYP = 'FREQUENCY'
        S.XUNITS = 'HZ'
        S.CURTYP = 'DFT'
      ELSE IF ( X_OUT .EQ. 'I' ) THEN
        S.XTYP = 'INDEX'
        S.XUNITS = 'INDEX UNITS'
      END IF
      IF ( ANY_CHARS( X_OUT, 'XYI' ) ) THEN ! Set generic curve type
        IF ( S.XTYP .EQ. 'TIME' ) THEN
          S.CURTYP = 'TIME SERIES'
        ELSE IF ( S.XTYP .EQ. 'FREQUENCY' ) THEN
          S.CURTYP = 'DFT'
        ELSE
          S.CURTYP = S.YTYP
          S.CURTYP(LEN_TRIM(S.CURTYP)+1:) = '-'//S.XTYP
        END IF
      END IF

      ! Interpolate
      IF ( X_OUT .EQ. 'X' ) THEN
        CALL INTERP_XY_S( U.X, U.Y, NFP_L, NLP_U, U.NFP, U.NLP,
     &     STEP, S.Y, S.NFP, S.NLP, IOS )
        IF ( IOS .NE. 0 ) GO TO 190
        S.DEL = REAL( STEP )
      ELSE IF ( X_OUT .EQ. 'Y' ) THEN
        CALL INTERP_XY_S( U.Y, U.X, NFP_L, NLP_U, U.NFP, U.NLP,
     &     STEP, S.Y, S.NFP, S.NLP, IOS )
        IF ( IOS .NE. 0 ) GO TO 190
        S.DEL = REAL( STEP )
      END IF

      ! Get the UDS file name
      IF ( .NOT. BATCH ) THEN
  120   WRITE( *, '(/1X,2A/3A/'' >> '',$)' )
     &     S.YTYP, ' file name?',
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

      ! Output the file
      CALL UDS_PUT( OUT_FILE, S, CW, REPORT, OUTPUT_LIST,
     & DIR_OUT, .TRUE., IOS )
      IF ( IOS .NE. 0 ) GO TO 190

      ! Loop for next file
  190 WRITE( *, '(80(''_''))' )
      GO TO 100

      ! Clean up and stop
  199 CALL CLEAN_UP( HIT, REPORT, OUTPUT_LIST, DIR_OUT )

      END



      SUBROUTINE UDS2Y_CL( CL_ARG, NUM_ARGS, PROG_NAME,
     & Y_OUT_CL, X_OUT_CL, STEP_CL )

!***********************************************************************
!* Processes the Program-Specific Command Line Arguments
!***********************************************************************

      ! Headers
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) CL_ARG(0:*) ! Command line argument array

      INTEGER NUM_ARGS ! Number of arguments

      CHARACTER*(*) PROG_NAME ! Program name

      CHARACTER Y_OUT_CL ! Indicates axis for output Y axis (Y or X)

      CHARACTER X_OUT_CL ! Indicates data for output X axis (X,Y,T,F,I)

      DOUBLE PRECISION STEP_CL ! Step value for interpolated outputs


      ! Variables ______________________________________________________

      INTEGER I, NS, IOS

      CHARACTER ARG*255, C


      ! Process command line arguments
      Y_OUT_CL = ' '
      X_OUT_CL = ' '
      STEP_CL = 0.D0
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
          WRITE( *, '(10X,A)' )
     &       '[Y|X(x|y|t|f|i)] [STEP=<step>]'
          IF ( ( ARG(NS:) .EQ. '??' ) .OR.
     &       ( ARG(NS:) .EQ. 'HELP' ) ) THEN ! Show command line help
            CALL HELP_CL( PROG_NAME )
            WRITE( *, '(6(10X,A/),10X,A)' )
     &         'Y|X(x|y|t|f|i) :  Output type, such as Y(t), where:',
     &         '   (x)  =>  interpolate into x-axis',
     &         '   (y)  =>  interpolate into y-axis',
     &         '   (t)  =>  time (index-based)',
     &         '   (f)  =>  frequency (index-based)',
     &         '   (i)  =>  index-based (other than time or freq.)',
     &         '<step> :  Step value for interpolated output series'
          END IF
          STOP ' '
        ELSE IF ( ( ANY_CHARS( ARG(NS:NS), 'YX' ) ) .AND.
     &     ( ARG(NS+1:NS+1) .EQ. '(' ) .AND.
     &     ( ANY_CHARS( ARG(NS+2:NS+2), 'XYTFI' ) ) .AND.
     &     ( ARG(NS+3:NS+3) .EQ. ')' ) .AND.
     &     ( ARG(NS+4:) .EQ. ' ' ) ) THEN
          Y_OUT_CL = ARG(NS:NS)
          X_OUT_CL = ARG(NS+2:NS+2)
          IF ( Y_OUT_CL .EQ. X_OUT_CL ) THEN
            WRITE( *, '(/2A)', IOSTAT=IOS )
     &         ' *** Illegal output type: ',
     &         CL_ARG(I)(NS:L_TRIM(CL_ARG(I)))
            Y_OUT_CL = ' '
            X_OUT_CL = ' '
          END IF
          CL_ARG(I) = ' '
        ELSE IF ( ( ARG(NS:NS+4) .EQ. 'STEP=' ) .OR.
     &     ( ARG(NS:NS+4) .EQ. 'STEP#' ) ) THEN
          READ( ARG(NS+5:), '(BN,F25.0)', IOSTAT=IOS ) STEP_CL
          IF ( ( IOS .NE. 0 ) .OR. ( STEP_CL .LE. 0.D0 ) )
     &       THEN
            WRITE( *, '(/2A)', IOSTAT=IOS )
     &         ' *** Illegal step value: ',
     &         ARG(NS+5:L_TRIM(ARG))
            STEP_CL = 0.D0
          END IF
          CL_ARG(I) = ' '
        END IF
      END DO

      RETURN
      END
