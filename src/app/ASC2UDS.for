      PROGRAM ASC2UDS

!***********************************************************************
!* Converts ASCII Files to UDS Files
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2003/07/27
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'
      INCLUDE 'uds_fxn.fi'
      INCLUDE 'file.fi'


      ! Variables ______________________________________________________

      RECORD /UDS/  U
      RECORD /UDS_SPEC/  S
      RECORD /UDS_CONTROL/  C
      RECORD /FILE/  HIT, REPORT, OUTPUT_LIST

      LOGICAL ALL_BATCH, NO_BATCH, NO_INCR, ONE_PASS, NEW_HIT, EOF,
     & BATCH, NO_FLAG,
     & WARN_FC, WARN_IC, S_NFP, S_NLP, S_DEL, X_FORMULA

      INTEGER NUM_ARGS, MAX_ARGS,
     & IOS, IOS_W, I, ID, IR, IT, J, L, LD, ASCII_UNIT,
     & NCOL, NNCOL, NRCOL, NNRCOL, IICOL, IXCOL, IYCOL, ICOL,
     & ISTART, NSTEP, NLP_STOP

      PARAMETER ( MAX_ARGS = 12 )

      DOUBLE PRECISION X_A, X_B, X_SCL, Y_SCL, DDREC(99),
     & DI, DIP, DIS, DEL_T

      CHARACTER CL_ARG(0:MAX_ARGS)*255, PROG_NAME*8,
     & TEMPLATE_STR*255, DIR_OUT*255,
     & FILE_NAME*255, OUT_FILE*255, DEF_FILE*255,
     & PROMPT*79,
     & DATA_REC*256, IBUF*8


      ! Initializations
      PROG_NAME = 'ASC2UDS'
      CALL FILE_INIT( HIT )
      CALL GET_CL( CL_ARG, NUM_ARGS, MAX_ARGS )
      CALL ASC2UDS_CL( CL_ARG, NUM_ARGS, PROG_NAME, NO_FLAG )
      CALL PROC_CL( CL_ARG, NUM_ARGS, PROG_NAME, TEMPLATE_STR,
     & ALL_BATCH, NO_BATCH, NO_INCR, REPORT, OUTPUT_LIST, DIR_OUT,
     & ONE_PASS, .TRUE., .TRUE. )
      PROMPT = 'ASCII file name/template/hitlist?'
      PROMPT(74:) = '[done]'
      CALL UDS_CONTROL_INIT( C )
      DIS = 0.0D0
      DEL_T = 0.0D0

      WRITE( *, '(//22X,A//)' ) '**  ASCII To UDS File Conversion  **'

      ! Read the file name/template/hitlist
  100 CALL FILE_PROMPT( TEMPLATE_STR, PROMPT, DIR_OUT,
     & ONE_PASS, HIT, NEW_HIT, FILE_NAME, EOF )
      IF ( ( FILE_NAME .EQ. ' ' ) .OR. ( EOF ) ) GO TO 199 ! Done

      ! Batch setup
      CALL BAT_SET( ALL_BATCH, NO_BATCH, NEW_HIT, HIT, BATCH )
      IF ( ( NEW_HIT ) .AND. ( BATCH ) ) THEN ! Batch prompts

        ! Get basic UDS file spec's
  101   CALL GET_SPECS( S, S_NFP, S_NLP, S_DEL, EOF )
        IF ( EOF ) THEN
          CALL MSG_WRITE( '*** Operation skipped', REPORT )
          CALL FILE_DEL( HIT )
          GO TO 190
        END IF
        CALL UDS_SPEC_COPY( U, S )

        ! Get column numbers
        CALL GET_COL_INFO( S_DEL, IICOL, X_FORMULA, X_A, X_B,
     &   IXCOL, X_SCL, IYCOL, Y_SCL, U.CHANFORM, 0, EOF )
        IF ( EOF ) GO TO 101

      ELSE IF ( BATCH ) THEN ! Reset UDS spec's

        CALL UDS_SPEC_COPY( U, S )

      END IF

      ! Open ASCII file
      CALL REP_COLS( REPORT )
      CALL MSG_WRITE( '  Reading: '//FILE_NAME, REPORT )
      CALL OPEN_FS( ASCII_UNIT, FILE_NAME, 'R', IOS )
      IF ( IOS .NE. 0 ) THEN
        CALL MSG_WRITE( '*** ASCII file open failed', REPORT )
        GO TO 190
      END IF

      ! Find first numerical record
      NNCOL = 0
      DO WHILE ( NNCOL .EQ. 0 )
        READ( ASCII_UNIT, '(A)', IOSTAT=IOS ) DATA_REC
        IF ( IOS .NE. 0 ) THEN ! File end or error
          CALL MSG_WRITE( '*** Unacceptable input file', REPORT )
          GO TO 190
        END IF
        L = 1
        NCOL = 0
        CALL GET_COL( DATA_REC, L, NCOL, NNCOL, DDREC )
      END DO

      ! Determine file column structure
      L = 1
      LD = LEN_TRIM( DATA_REC )
      NCOL = 0
      NNCOL = 0
      DO WHILE ( L .LE. LD )
        CALL GET_COL( DATA_REC, L, NCOL, NNCOL, DDREC )
      END DO

      IF ( .NOT. BATCH ) THEN ! Interactive prompts

        ! Get basic UDS file spec's
  102   CALL GET_SPECS( S, S_NFP, S_NLP, S_DEL, EOF )
        IF ( EOF ) THEN
          CALL MSG_WRITE( '*** Operation skipped', REPORT )
          GO TO 190
        END IF
        CALL UDS_SPEC_COPY( U, S )

        ! Show first numeric record
        WRITE( *, '(/I4,2A/)', IOSTAT=IOS_W ) NNCOL,
     &   ' numeric columns detected - ',
     &   'First record (col)=values are:'
        WRITE( *, '(4(A,I2,A,1PG14.7,0P))', IOSTAT=IOS )
     &   ( ' (', J, ')=', DDREC(J), J = 1, NNCOL )

        ! Get column numbers
        CALL GET_COL_INFO( S_DEL, IICOL, X_FORMULA, X_A, X_B,
     &   IXCOL, X_SCL, IYCOL, Y_SCL, U.CHANFORM, NNCOL, EOF )
        IF ( EOF ) GO TO 102

      ELSE ! Batch

        IF ( MAX( IICOL, IXCOL, IYCOL ) .GT. NNCOL ) THEN
          CALL MSG_WRITE(
     &     '*** ERROR - Specified column no. > no. of columns',
     &     REPORT )
          GO TO 190
        END IF

      END IF

      ! Assign/check starting index
      IF ( ( IICOL .GT. 0 ) .OR.
     & ( ( IXCOL .GT. 0 ) .AND. ( U.CHANFORM .EQ. 'Y' ) ) )
     & THEN ! Compute index
        IF ( IICOL .GT. 0 ) THEN
          ICOL = IICOL
          DEL_T = 1.D0
        ELSE
          ICOL = IXCOL
        END IF
        DI = DDREC( ICOL )
        READ( ASCII_UNIT, '(A)', IOSTAT=IOS ) DATA_REC
        BACKSPACE( ASCII_UNIT )
        IF ( IOS .EQ. 0 ) THEN ! Compute index from first 2 values
          L = 1
          LD = LEN_TRIM( DATA_REC )
          NRCOL = 0
          NNRCOL = 0
          DO WHILE ( L .LE. LD )
            CALL GET_COL( DATA_REC, L, NRCOL, NNRCOL, DDREC )
          END DO
          DIP = DDREC( ICOL )
          IF ( DIP .GT. DI ) THEN
            IF ( IICOL .EQ. 0 ) THEN
              IF ( S_DEL ) THEN
                DEL_T = DP_TRAN( U.DEL )
              ELSE
                DEL_T = DIP - DI
              END IF
            END IF
            I = NINT( DI / ( DIP - DI ) )
          ELSE
            CALL MSG_WRITE(
     &       '*** ERROR - Index cannot be computed', REPORT )
            GO TO 190
          END IF
        ELSE ! Only 1 numeric record
          IF ( ( IICOL .EQ. 0 ) .AND.
     &     ( IXCOL .GT. 0 ) .AND. ( U.CHANFORM .EQ. 'Y' ) .AND.
     &     ( S_DEL ) .AND. ( U.DEL .GT. 0. ) ) THEN ! Index from X
            I = NINT( ( DI * X_SCL ) / U.DEL )
          ELSE IF ( S_NFP ) THEN ! Use specified index
            I = U.NFP
          ELSE ! Use zero
            I = MIN( MAX( 0, NFP_L ), NLP_U )
          END IF
        END IF
        IF ( .NOT. S_NFP ) THEN ! Use first index as NFP
          IF ( ( I .LT. NFP_L ) .OR. ( I .GT. NLP_U ) ) THEN
            CALL MSG_WRITE(
     &       '*** ERROR - Index column violates array bounds',
     &       REPORT )
            GO TO 190
          END IF
          U.NFP = I
        ELSE IF ( I .LT. U.NFP ) THEN ! Skip to NFP
          DO IR = I+1, U.NFP
            READ( ASCII_UNIT, *, IOSTAT=IOS )
            IF ( IOS .NE. 0 ) THEN
              CALL MSG_WRITE(
     &         '*** ERROR - End of file occurs before NFP index',
     &         REPORT )
              GO TO 190
            END IF
          END DO
          I = U.NFP
        ELSE IF ( I .GT. U.NFP ) THEN ! Zero-fill up to I
          IF ( I .GT. NLP_U ) THEN
            CALL MSG_WRITE(
     &       '*** ERROR - Index column violates array bounds',
     &       REPORT )
            GO TO 190
          END IF
          CALL MSG_WRITE(
     &     '*** Zero-filling missing data above NFP index',
     &     REPORT )
          DO ID = U.NFP, I-1
            U.Y(I) = 0.
            U.X(I) = 0.
          END DO
        END IF
      ELSE ! Use specified value or 0
        IF ( S_NFP ) THEN ! Use specified value
          I = U.NFP
        ELSE ! Use zero
          I = MIN( MAX( 0, NFP_L ), NLP_U )
          U.NFP = I
        END IF
      END IF
      IF ( ( S_NLP ) .AND. ( I .GT. U.NLP ) ) THEN
        CALL MSG_WRITE(
     &   '*** ERROR - Starting index exceeds specified NLP', REPORT )
        GO TO 190
      ELSE IF ( I .GT. NLP_U ) THEN
        CALL MSG_WRITE(
     &   '*** ERROR - Starting index exceeds array upper bound',
     &   REPORT )
        GO TO 190
      END IF

      ! Read data
      BACKSPACE( ASCII_UNIT ) ! Move back to first numeric record
      WARN_FC = .FALSE.
      WARN_IC = .FALSE.
      IF ( S_NLP ) THEN
        NLP_STOP = MIN( U.NLP, NLP_U )
      ELSE
        NLP_STOP = NLP_U
      END IF
      ISTART = I
      I = I - 1
      DO WHILE ( I .LT. NLP_STOP )

        ! Read/process ASCII record
        DATA_REC = ' '
        DO WHILE ( DATA_REC .EQ. ' ' )
          READ( ASCII_UNIT, '(A)', ERR=104, END=104, IOSTAT=IOS )
     &     DATA_REC
        END DO
        L = 1
        LD = LEN_TRIM( DATA_REC )
        NRCOL = 0
        NNRCOL = 0
        DO WHILE ( L .LE. LD )
          CALL GET_COL( DATA_REC, L, NRCOL, NNRCOL, DDREC )
        END DO
        IF ( ( .NOT. WARN_FC ) .AND. ( NNRCOL .NE. NNCOL ) ) THEN
          CALL MSG_WRITE(
     &     '*** WARNING - File column structure varies', REPORT )
          WARN_FC = .TRUE.
        END IF
        I = I + 1

        ! Check index
        IF ( ( IICOL .GT. 0 ) .OR.
     &   ( ( IXCOL .GT. 0 ) .AND. ( U.CHANFORM .EQ. 'Y' ) ) )
     &   THEN ! Compute index
          IF ( IICOL .GT. 0 ) THEN
            ICOL = IICOL
          ELSE
            ICOL = IXCOL
          END IF
          DI = DDREC( ICOL )
          IF ( I .EQ. ISTART ) THEN ! Set lower index value
            DIS = DI
          ELSE ! Check index
            NSTEP = NINT( ( DI - DIS ) / DEL_T )
            IF ( ( NSTEP .NE. I-ISTART ) .AND. ( .NOT. WARN_IC ) ) THEN
              WRITE( IBUF, '(I8)' ) I
              CALL MSG_WRITE(
     &         '*** WARNING - Index/x data problems '//
     &         'starting at index: '//ibuf, REPORT )
              WARN_IC = .TRUE.
            END IF
            IF ( ( IICOL .EQ. 0 ) .AND. ( .NOT. S_DEL ) )
     &       DEL_T = ( DI - DIS ) / ( I - ISTART ) ! Update DEL est
          END IF
        END IF

        ! Assign X
        IF ( ( U.CHANFORM .EQ. 'X-Y' ) .OR. ( .NOT. S_DEL ) ) THEN
          IF ( X_FORMULA ) THEN
            U.X(I) = REAL( X_A * I + X_B )
          ELSE
            U.X(I) = REAL( DDREC( IXCOL ) * X_SCL )
          END IF
        END IF

        ! Assign Y
        U.Y(I) = REAL( DDREC( IYCOL ) * Y_SCL )

      END DO

      ! Check/set upper index
  104 IF ( ( I .LT. U.NFP ) .OR. ( ( IOS .NE. 0 ) .AND.
     & ( IOS .NE. -1 ) ) ) THEN
        CALL MSG_WRITE( '*** Unacceptable input file', REPORT )
        GO TO 190
      END IF
      IF ( ( S_NLP ) .AND. ( I .LT. U.NLP ) ) THEN
        CALL MSG_WRITE(
     &   '*** Zero-filling missing data up to NLP index', REPORT )
        DO ID = I+1, U.NLP
          U.Y(I) = 0.
          U.X(I) = 0.
        END DO
      ELSE IF ( .NOT. S_NLP ) THEN ! Set upper index
        U.NLP = I
      END IF

      ! Set computed X step
      IF ( ( U.CHANFORM .EQ. 'Y' ) .AND. ( .NOT. S_DEL ) ) THEN
        IF ( U.NLP .GT. U.NFP ) THEN
          U.DEL = ( U.X(U.NLP) - U.X(U.NFP) ) / ( U.NLP - U.NFP )
        END IF
      END IF

      ! Set default output file name
      OUT_FILE = FILE_NAME
      CALL FN_EXTRACT( OUT_FILE )
      IT = FT_POSN( OUT_FILE )
      IF ( IT .EQ. 0 ) IT = LEN_TRIM( OUT_FILE ) + 1
      OUT_FILE(IT:) = '.uds'
      CALL ADD_PATH( DIR_OUT, OUT_FILE )
      IF ( .NOT. NO_INCR ) CALL FN_INCR( OUT_FILE )
      CALL FN_NAME_EXTRACT( OUT_FILE )
      DEF_FILE = OUT_FILE

      ! Set the output UDS file name
      IF ( .NOT. BATCH ) THEN
  120   WRITE( *, '(/A/3A/'' >> '',$)' )
     &   ' Output UDS file name?',
     &   ' [', DEF_FILE(:L_TRIM(DEF_FILE)), ']'
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
     & DIR_OUT, .TRUE., IOS )

      ! Loop for next file
  190 WRITE( *, '(80(''_''))' )
      CLOSE( ASCII_UNIT, IOSTAT=IOS )
      GO TO 100

      ! Clean up and stop
  199 CALL CLEAN_UP( HIT, REPORT, OUTPUT_LIST, DIR_OUT )

      END



      SUBROUTINE ASC2UDS_CL( CL_ARG, NUM_ARGS, PROG_NAME, NO_FLAG )

!***********************************************************************
!* Processes the Program-Specific Command Line Arguments
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER*(*) CL_ARG(0:*) ! Command line argument array

      INTEGER NUM_ARGS ! Number of arguments

      CHARACTER*(*) PROG_NAME ! Program name

      LOGICAL NO_FLAG ! Indicates not to flag output file names


      ! Variables ______________________________________________________

      INTEGER I, NS

      CHARACTER ARG*255, C


      ! Process command line arguments
      NO_FLAG = .FALSE.
      DO I = 1, NUM_ARGS
        CALL STR_UPCASE( ARG, CL_ARG(I) )
        C = ARG(1:1)
        IF ( ( C .EQ. '-' ) .OR. ( C .EQ. '/' ) ) THEN
          NS = 2
        ELSE
          NS = 1
        END IF
        IF ( ( ARG(NS:) .EQ. '?' ) .OR.
     &   ( ARG(NS:) .EQ. '??' ) .OR.
     &   ( ARG(NS:) .EQ. 'HELP' ) ) THEN ! Show command line syntax
          CALL SYNTAX_CL( PROG_NAME )
          WRITE( *, '(10X,A)' ) '[NO_FLAG]'
          IF ( ( ARG(NS:) .EQ. '??' ) .OR.
     &     ( ARG(NS:) .EQ. 'HELP' ) ) THEN ! Show command line help
            CALL HELP_CL( PROG_NAME )
            WRITE( *, '(10X,A)' )
     &       'NO_FLAG :  Don''t flag output file names'
          END IF
          STOP ' '
        ELSE IF ( ( ARG(NS:) .EQ. 'NO_FLAG' ) .OR.
     &   ( ARG(NS:) .EQ. 'NOFLAG' ) ) THEN
          NO_FLAG = .TRUE.
          CL_ARG(I) = ' '
        END IF
      END DO

      RETURN
      END



      SUBROUTINE GET_SPECS( S, S_NFP, S_NLP, S_DEL, EOF )

!***********************************************************************
!* Gets Basic UDS File Specifications
!***********************************************************************

      ! Headers
      INCLUDE 'platform.fi'
      INCLUDE 'uds_str.fi'
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      RECORD /UDS_SPEC/  S

      LOGICAL S_NFP

      LOGICAL S_NLP

      LOGICAL S_DEL

      LOGICAL EOF


      ! Variables ______________________________________________________

      RECORD /UDS_CONTROL/  CR

      LOGICAL MSG

      PARAMETER ( MSG = .TRUE. )

      INTEGER IOS

      CHARACTER IN_LINE*255, NAME*25, VALUE*132,
     & SPEC_FILE*255, RESP, YUNITS*20, XUNITS*20


      ! Functions ______________________________________________________

      CHARACTER DS_UNITS*20

      EXTERNAL DS_UNITS


      ! Initialize UDS fields
  101 CALL UDS_INIT( S )
      CALL UDS_CONTROL_INIT( CR )

      ! Get UDS file for spec's
      WRITE( *, '(/A,39X,A/'' >> '',$)' )
     & ' UDS file with base specifications?', '[none]'
      READ( *, '(A)', END=199 ) SPEC_FILE
      IF ( SPEC_FILE .NE. ' ' ) THEN ! Read spec file

        CR.SPECS_ONLY = .TRUE.
        CALL UDS_READ( SPEC_FILE, S, CR, IOS )
        IF ( IOS .NE. 0 ) GO TO 101
        WRITE( *, '(/A,12X,A/'' >> '',$)' )
     &   ' Use index range (NFP-NLP) and step (DEL) from base file?'//
     &   '  (Y/N)', '[no]'
        READ( *, '(A)', END=101 ) RESP
        CALL STR_UP( RESP )
        IF ( RESP .EQ. 'Y' ) THEN
          S_NFP = .TRUE.
          S_NLP = .TRUE.
          S_DEL = .TRUE.
        ELSE
          S_NFP = .FALSE.
          S_NLP = .FALSE.
          S_DEL = .FALSE.
          S.NFP = 0
          S.NLP = 0
          S.DEL = 0.
        END IF

      ELSE ! Prompt for basic spec's

        ! Set key field defaults
        S.FILEVER = 'UDS-1992'
        S.AXIS = 'XG'
        S_NFP = .FALSE.
        S_NLP = .FALSE.
        S_DEL = .FALSE.

        ! Dimensional system
  102   WRITE( *, '(/A,12X,A/'' >> '',$)' )
     &   ' Dimensional system?   ( M - Metric,  S - SI,  E - English )',
     &   '[Metric]'
        READ( *, '(A)', END=101 ) RESP
        CALL STR_UP( RESP )
        IF ( .NOT. ANY_CHARS( RESP, ' MSE' ) ) THEN
          WRITE( *, * ) '*** Unacceptable response'
          GO TO 102
        END IF
        IF ( RESP .EQ. 'E' ) THEN
          S.DIMSYS = 'ENG'
        ELSE IF ( RESP .EQ. 'S' ) THEN
          S.DIMSYS = 'SI'
        ELSE
          S.DIMSYS = 'MET'
        END IF

        ! Channel format
  103   WRITE( *, '(/A,51X,A/'' >> '',$)' )
     &   ' Channel format?   (Y/X-Y)', '[Y]'
        READ( *, '(A)', END=101 ) S.CHANFORM
        CALL STR_UP( S.CHANFORM )
        IF ( ( S.CHANFORM .EQ. 'Y' ) .OR. ( S.CHANFORM .EQ. ' ' ) ) THEN
          S.CHANFORM = 'Y'
        ELSE IF ( S.CHANFORM .NE. 'X-Y' ) THEN
          WRITE( *, * ) '*** Unacceptable response'
          GO TO 103
        END IF

        ! Y data type
        WRITE( *, '(/A,53X,A/'' >> '',$)' )
     &   ' Y data type?', '[ACCELERATION]'
        READ( *, '(A)', END=101 ) S.YTYP
        CALL STR_UP( S.YTYP )
        IF ( S.YTYP .EQ. ' ' ) S.YTYP = 'ACCELERATION'
        YUNITS = DS_UNITS( S.YTYP, S.DIMSYS )

        ! Y units
        WRITE( *, '(/A,49X,A/'' >> '',$)' )
     &   ' Y units?', '['//YUNITS//']'
        READ( *, '(A)', END=101 ) S.YUNITS
        CALL STR_UP( S.YUNITS )
        IF ( S.YUNITS .EQ. ' ' ) S.YUNITS = YUNITS

        ! X data type
        WRITE( *, '(/A,61X,A/'' >> '',$)' )
     &   ' X data type?', '[TIME]'
        READ( *, '(A)', END=101 ) S.XTYP
        CALL STR_UP( S.XTYP )
        IF ( S.XTYP .EQ. ' ' ) S.XTYP = 'TIME'
        XUNITS = DS_UNITS( S.XTYP, S.DIMSYS )

        ! X units
        WRITE( *, '(/A,49X,A/'' >> '',$)' )
     &   ' X units?', '['//XUNITS//']'
        READ( *, '(A)', END=101 ) S.XUNITS
        CALL STR_UP( S.XUNITS )
        IF ( S.XUNITS .EQ. ' ' ) S.XUNITS = XUNITS

        ! Set curve type
        IF ( ( S.CHANFORM .EQ. 'Y' ) .AND. ( S.XTYP .EQ. 'TIME' ) ) THEN
          S.CURTYP = 'TIME SERIES'
        ELSE IF ( S.CHANFORM .EQ. 'X-Y' ) THEN
          S.CURTYP = 'X-Y'
        ELSE
          S.CURTYP = ' '
        END IF

        ! X data step
        IF ( S.CHANFORM .EQ. 'Y' ) THEN
          WRITE( *, '(/A,48X,A/'' >> '',$)' )
     &     ' X data step (DEL)?', '[from X data]'
          READ( *, '(A)', END=101 ) VALUE
          IF ( VALUE .NE. ' ' ) THEN
            CALL REAL_ASSIGN( VALUE, S.DEL, .TRUE., IOS )
            IF ( ( IOS .NE. 0 ) .OR. ( S.DEL .LE. 0. ) )
     &       WRITE( *, '(/A)' ) ' *** WARNING - Nonstandard field value'
            S_DEL = .TRUE.
          END IF
        ELSE ! Use DEL=0 for X-Y channels
          S_DEL = .TRUE.
        END IF

      END IF

      ! Display key UDS fields
      WRITE( *, * )
  104 WRITE( *, '(/A/)' ) ' Key UDS fields:'
      WRITE( *, * ) '  FILEVER = ', S.FILEVER(:L_TRIM(S.FILEVER)),
     & '   (UDS file version)'
      WRITE( *, * ) '  DIMSYS = ', S.DIMSYS,
     & '   (dimensional system: MET - Metric, SI - SI, ENG - English)'
      WRITE( *, * ) '  TSTNO = ', S.TSTNO,
     & '   (test number)'
      WRITE( *, * ) '  TSTNAM = ', S.TSTNAM,
     & '   (test name)'
      WRITE( *, * ) '  CHANFORM = ', S.CHANFORM(:L_TRIM(S.CHANFORM)),
     & '   (channel format: Y or X-Y)'
      WRITE( *, * ) '  CURNO = ', S.CURNO,
     & '   (curve number)'
      WRITE( *, * ) '  CURNAM = ', S.CURNAM,
     & '   (curve name)'
      WRITE( *, * ) '  SENATT = ', S.SENATT(:L_TRIM(S.SENATT)),
     & '   (sensor attachment)'
      WRITE( *, * ) '  AXIS = ', S.AXIS,
     & '   (data axis: XG for X-Global, YL for Y-Local, etc.)'
      WRITE( *, * ) '  YTYP = ', S.YTYP(:L_TRIM(S.YTYP)),
     & '   (Y data type)'
      WRITE( *, * ) '  YUNITS = ', S.YUNITS(:L_TRIM(S.YUNITS)),
     & '   (Y units)'
      WRITE( *, * ) '  XTYP = ', S.XTYP(:L_TRIM(S.XTYP)),
     & '   (X data type)'
      WRITE( *, * ) '  XUNITS = ', S.XUNITS(:L_TRIM(S.XUNITS)),
     & '   (X units)'
      WRITE( *, * ) '  CURTYP = ', S.CURTYP(:L_TRIM(S.CURTYP))
      IF ( S_NFP ) THEN
        WRITE( *, * ) '  NFP = ', S.NFP, '   (first data index)'
      ELSE
        WRITE( *, * ) '  NFP = <from data>', '   (first data index)'
      END IF
      IF ( S_NLP ) THEN
        WRITE( *, * ) '  NLP = ', S.NLP, '   (last data index)'
      ELSE
        WRITE( *, * ) '  NLP = <end of data>   (last data index)'
      END IF
      IF ( S_DEL ) THEN
        WRITE( *, * ) '  DEL = ', S.DEL, '   (data step)'
      ELSE IF ( S.CHANFORM .EQ. 'Y' ) THEN
        WRITE( *, * ) '  DEL = <from X data>   (X data step)'
      ELSE IF ( S.CHANFORM .EQ. 'X-Y' ) THEN
        WRITE( *, * ) '  DEL = <unspecified>   (data step)'
      END IF
      WRITE( *, * ) '  INIVEL = ', S.INIVEL,
     & '   (initial velocity along axis)'

      ! Prompt for field change
  105 WRITE( *, '(/A,31X,A/'' >> '',$)' )
     & ' To change a field enter:  <name> = <value>', '[done]'
      READ( *, '(A)', END=199 ) IN_LINE
      IF ( IN_LINE .NE. ' ' ) THEN

        ! Parse input for field NAME and VALUE
        CALL PARSE_NV( IN_LINE, NAME, VALUE )

        ! Change field value
        CALL UDS_SPEC_SET( S, NAME, VALUE, MSG, IOS )
        IF ( IOS .NE. 0 ) GO TO 105

        ! Update specified value flags
        IF ( NAME .EQ. 'NFP' ) THEN
          S_NFP = .TRUE.
        ELSE IF ( NAME .EQ. 'NLP' ) THEN
          S_NLP = .TRUE.
        ELSE IF ( NAME .EQ. 'DEL' ) THEN
          S_DEL = .TRUE.
        END IF

        ! Display key UDS fields again
        GO TO 104

      END IF

      ! Set other UDS fields
      S.FILEFORM = S.CHANFORM
      S.NUMFORM = NUMFORM_P
      S.NCHAN = 1
      S.ICHAN = 1

      EOF = .FALSE.
      RETURN

      ! EOF handler
  199 EOF = .TRUE.
      RETURN

      END



      SUBROUTINE GET_COL_INFO( S_DEL, IICOL, X_FORMULA, X_A, X_B,
     & IXCOL, X_SCL, IYCOL, Y_SCL, CHANFORM, NNCOL, EOF )

!***********************************************************************
!* Gets Data Column Numbers From User
!***********************************************************************


      ! Arguments ______________________________________________________

      LOGICAL S_DEL

      INTEGER IICOL

      LOGICAL X_FORMULA

      DOUBLE PRECISION X_A

      DOUBLE PRECISION X_B

      INTEGER IXCOL

      DOUBLE PRECISION X_SCL

      INTEGER IYCOL

      DOUBLE PRECISION Y_SCL

      CHARACTER*(*) CHANFORM

      INTEGER NNCOL

      LOGICAL EOF


      ! Variables ______________________________________________________

      INTEGER IOS_R

      CHARACTER RESP*25


      ! Index column
  101 WRITE( *, '(/A,42X,A/'' >> '',$)' )
     & ' Array index data column number?', '[none]'
      READ( *, '(A)', END=199 ) RESP
      IF ( RESP .NE. ' ' ) THEN
        READ( RESP, '(BN,I25)', END=199, IOSTAT=IOS_R ) IICOL
        IF ( ( IOS_R .NE. 0 ) .OR. ( IICOL .LT. 1 ) .OR.
     &   ( ( NNCOL .GT. 0 ) .AND. ( IICOL .GT. NNCOL ) ) ) THEN
          IICOL = 0
          WRITE( *, * ) '*** Unacceptable value'
          GO TO 101
        END IF
      ELSE
        IICOL = 0
      END IF

      ! X data column/formula
      IF ( ( CHANFORM .EQ. 'X-Y' ) .OR. ( .NOT. S_DEL ) .OR.
     & ( IICOL .EQ. 0 ) ) THEN
  102   IF ( ( CHANFORM .EQ. 'X-Y' ) .OR. ( .NOT. S_DEL ) ) THEN
          WRITE( *, '(/A/'' >> '',$)' )
     &     ' X data column number?   ( F - Formula-based X data )'
        ELSE
          WRITE( *, '(/A,21X,A/'' >> '',$)' )
     &     ' X data column number?   ( F - Formula-based X data )',
     &     '[none]'
        END IF
        READ( *, '(A)', END=101 ) RESP
        CALL STR_UP( RESP )
        IF ( ( RESP .EQ. ' ' ) .AND. ( CHANFORM .NE. 'X-Y' ) .AND.
     &   ( S_DEL ) ) THEN ! No optional X data column/formula
          X_FORMULA = .FALSE.
          IXCOL = 0
        ELSE IF ( RESP .EQ. 'F' ) THEN ! Construct X data from formula
          X_FORMULA = .TRUE.
          IXCOL = 0
          WRITE( *, * ) '   X data formula:   x = a*i + b',
     &     '     (i = array index)'
  103     WRITE( *, '(26X,A,$)' ) 'a='
          READ( *, '(BN,F25.0)', END=102, ERR=103 ) X_A
  104     WRITE( *, '(32X,A,$)' ) 'b='
          READ( *, '(BN,F25.0)', END=102, ERR=103 ) X_B
        ELSE ! Column number specified
          X_FORMULA = .FALSE.
          READ( RESP, '(BN,I25)', IOSTAT=IOS_R ) IXCOL
          IF ( ( IOS_R .NE. 0 ) .OR. ( IXCOL .LT. 1 ) .OR.
     &     ( ( NNCOL .GT. 0 ) .AND. ( IXCOL .GT. NNCOL ) ) ) THEN
            WRITE( *, * ) '*** Unacceptable value'
            GO TO 102
          END IF
  105     WRITE( *, '(/A,53X,A/'' >> '',$)' )
     &     ' X data scale factor?', '[none]'
          READ( *, '(A)', END=101 ) RESP
          IF ( RESP .EQ. ' ' ) THEN ! No X scaling
            X_SCL = 1.D0
          ELSE
            READ( RESP, '(BN,F25.0)', ERR=105 ) X_SCL
          END IF
        END IF
      ELSE ! No X data column/formula
        X_FORMULA = .FALSE.
        IXCOL = 0
      END IF

      ! Y data column
  106 WRITE( *, '(/A/'' >> '',$)' ) ' Y data column number?'
      READ( *, '(A)', END=101 ) RESP
      READ( RESP, '(BN,I25)', END=101, IOSTAT=IOS_R ) IYCOL
      IF ( ( IOS_R .NE. 0 ) .OR. ( IYCOL .LT. 1 ) .OR.
     & ( ( NNCOL .GT. 0 ) .AND. ( IYCOL .GT. NNCOL ) ) ) THEN
        WRITE( *, * ) '*** Unacceptable value'
        GO TO 106
      END IF
  107 WRITE( *, '(/A,53X,A/'' >> '',$)' ) ' Y data scale factor?',
     & '[none]'
      READ( *, '(A)', END=101 ) RESP
      IF ( RESP .EQ. ' ' ) THEN ! No Y scaling
        Y_SCL = 1.D0
      ELSE
        READ( RESP, '(BN,F25.0)', ERR=107 ) Y_SCL
      END IF

      EOF = .FALSE.
      RETURN

      ! EOF handler
  199 EOF = .TRUE.
      RETURN

      END



      SUBROUTINE GET_COL( DATA_REC, L, NCOL, NNCOL, DDREC )

!***********************************************************************
!* Extracts Next Data Column from String
!***********************************************************************

      ! Headers
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) DATA_REC

      INTEGER L

      INTEGER NCOL

      INTEGER NNCOL

      DOUBLE PRECISION DDREC(99)


      ! Variables ______________________________________________________

      INTEGER I, LD, IS, IE, IC, IOS_R

      DOUBLE PRECISION DD

      CHARACTER NSTR*25


      ! Initializations
      I = L
      LD = LEN_TRIM( DATA_REC )

      ! Read through column separator
      IC = ICHAR( DATA_REC(I:I) )
      DO WHILE ( ( I .LE. LD ) .AND. ( ( IC .LT. 43 ) .OR.
     & ( IC .GT. 122 ) .OR. ( IC .EQ. 44 ) ) )
        I = I + 1
        IF ( I .LE. LD ) IC = ICHAR( DATA_REC(I:I) )
      END DO

      ! Read the column
      IF ( I .LE. LD ) THEN
        ! Increment number of columns counter
        NCOL = NCOL + 1

        ! Find the end of the column
        IS = I
        DO WHILE ( ( I .LE. LD ) .AND. ( IC .GE. 43 ) .AND.
     &   ( IC .LE. 122 ) .AND. ( IC .NE. 44 ) )
          I = I + 1
          IF ( I .LE. LD ) IC = ICHAR( DATA_REC(I:I) )
        END DO

        ! Assign column if numeric
        IE = I - 1
        NSTR = DATA_REC(IS:IE)
        READ( NSTR, '(BN,F25.0)', IOSTAT=IOS_R ) DD
        IF ( IOS_R .EQ. 0 ) THEN ! Numeric
          NNCOL = NNCOL + 1
          IF ( NNCOL .LE. 99 ) THEN
            DDREC( NNCOL ) = DD
          ELSE
            WRITE( *, * ) '*** Numeric columns past 99 ignored'
          END IF
        END IF

        L = IE + 1
      ELSE
        L = I
      END IF

      RETURN
      END



      SUBROUTINE PARSE_NV( IN_LINE, NAME, VALUE )

!***********************************************************************
!* Extracts the NAME and VALUE Fields from IN_LINE
!***********************************************************************

      ! Headers
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) IN_LINE

      CHARACTER*(*) NAME

      CHARACTER*(*) VALUE


      ! Variables ______________________________________________________

      INTEGER I, LI


      ! Functions ______________________________________________________

      CHARACTER LJUST*255

      EXTERNAL LJUST


      ! Assign first field to NAME and second field to VALUE
      NAME = ' '
      VALUE = ' '
      IN_LINE = LJUST( IN_LINE )
      IF ( IN_LINE .NE. ' ' ) THEN
        IF ( IN_LINE(1:1) .EQ. '=' ) RETURN ! Blank NAME

        ! Scan for end of lead string and assign to NAME
        I = 2
        LI = LEN_TRIM( IN_LINE )
        DO WHILE ( ( I .LE. LI ) .AND.
     &   ( .NOT. ANY_CHARS( IN_LINE(I:I), ' =' ) ) )
          I = I + 1
        END DO
        NAME = IN_LINE(:I-1)
        CALL STR_UP( NAME )

        ! Find and assign VALUE
        DO WHILE ( ( I .LE. LI ) .AND.
     &   ( ANY_CHARS( IN_LINE(I:I), ' =' ) ) )
          I = I + 1
        END DO
        IF ( I .LE. LI ) VALUE = LJUST( IN_LINE(I:) )
      END IF

      RETURN
      END
