      PROGRAM UDSCONV

!***********************************************************************
!* Converts UDS File Type, Number Format, and Dimensional System
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
      INCLUDE 'dimsys.fi'
      INCLUDE 'file.fi'


      ! Variables ______________________________________________________

      RECORD /UDS/  U
      RECORD /UDS_CONTROL/  CL, CS, CR, CW
      RECORD /FILE/  HIT, REPORT, OUTPUT_LIST

      LOGICAL ALL_BATCH, NO_BATCH, NO_INCR, ONE_PASS, NEW_HIT, EOF,
     & BATCH, GROUP, NEW_GROUP, NO_FLAG

      INTEGER NUM_ARGS, MAX_ARGS, IOS, I, IT, ASCII_UNIT

      PARAMETER ( MAX_ARGS = 12 )

      CHARACTER CL_ARG(0:MAX_ARGS)*255, PROG_NAME*8,
     & TEMPLATE_STR*255, DIR_OUT*255,
     & FILE_NAME*255, OUT_FILE*255, DEF_FILE*255,
     & PROMPT*79,
     & ANGLE_CL*3, ANGLE*3, FLAG_CL, FLAG_N, FLAG_D, FLAG_A,
     & FLAG, TAB

      PARAMETER ( TAB = CHAR(9) )


      ! Initializations
      PROG_NAME = 'UDSConv'
      CALL FILE_INIT( HIT )
      CALL GET_CL( CL_ARG, NUM_ARGS, MAX_ARGS )
      CALL UDS_CONTROL_INIT( CL )
      CALL UDSCONV_CL( CL_ARG, NUM_ARGS, PROG_NAME,
     & CL, ANGLE_CL, FLAG_CL, NO_FLAG )
      CALL PROC_CL( CL_ARG, NUM_ARGS, PROG_NAME, TEMPLATE_STR,
     & ALL_BATCH, NO_BATCH, NO_INCR, REPORT, OUTPUT_LIST, DIR_OUT,
     & ONE_PASS, .TRUE., .TRUE. )
      PROMPT = 'UDS file name/template/hitlist?'
      PROMPT(74:) = '[done]'
      CALL UDS_CONTROL_INIT( CS )
      CALL UDS_CONTROL_INIT( CR )
      CALL UDS_CONTROL_INIT( CW )
      CS.NUMFORM = 'NONE'
      CS.SPECS_ONLY = .TRUE.
      CR.FILL_X = .TRUE.
      CW.NUMFORM = 'NONE'

      WRITE( *, '(//26X,A//)' ) '**  UDS File Conversions  **'

      ! Read the file name/template/hitlist
  100 CALL FILE_PROMPT( TEMPLATE_STR, PROMPT, DIR_OUT,
     & ONE_PASS, HIT, NEW_HIT, FILE_NAME, EOF )
      IF ( ( FILE_NAME .EQ. ' ' ) .OR. ( EOF ) ) GO TO 199 ! Done

      ! Batch setup
      CALL BAT_SET( ALL_BATCH, NO_BATCH, NEW_HIT, HIT, BATCH )
      CALL GROUP_SET( HIT, NEW_HIT, BATCH, 2, GROUP, NEW_GROUP )

      ! Group prompts
      IF ( NEW_GROUP ) THEN

        ! Get conversion profile
        CALL CONV_PROF( CL, CR, CW, ANGLE_CL, ANGLE,
     &     FLAG_N, FLAG_D, FLAG_A, EOF )
        IF ( EOF ) THEN
          CALL MSG_WRITE( '*** Operation skipped', REPORT )
          CALL FILE_DEL( HIT )
          GO TO 190
        END IF

      END IF

      ! Read UDS file specs
      CALL REP_COLS( REPORT )
      CALL UDS_GET( FILE_NAME, U, CS, REPORT, IOS )
      IF ( IOS .NE. 0 ) GO TO 190

      ! Check file type
      IF ( ( U.DIMSYS .NE. 'MET' ) .AND.
     & ( U.DIMSYS .NE. 'SI' ) .AND.
     & ( U.DIMSYS .NE. 'ENG' ) ) THEN
        WRITE( *, '(/3A)' ) ' *** ERROR - ',
     &     'Unsupported dimensional system: ', U.DIMSYS
        GO TO 190
      ELSE IF ( ( U.CHANFORM .NE. 'Y' ) .AND.
     & ( U.CHANFORM .NE. 'X-Y' ) ) THEN
        WRITE( *, '(/3A)' ) ' *** ERROR - ',
     &     'Unsupported channel format: ', U.CHANFORM
        GO TO 190
      END IF

      IF ( .NOT. GROUP ) THEN ! Interactive prompts

        ! Display file info
        WRITE( *, '(/4A)' )
     &     '  File Number Format: ', U.NUMFORM,
     &     '  Dimensional System: ', U.DIMSYS

        ! Get conversion profile
        CALL CONV_PROF( CL, CR, CW, ANGLE_CL, ANGLE,
     &     FLAG_N, FLAG_D, FLAG_A, EOF )
        IF ( EOF ) GO TO 190

      END IF

      ! Check if conversion(s) required / Set file name flag letter
      IF ( ( .NOT. BLANK( CW.FILEVER ) ) .AND.
     & ( CW.FILEVER .NE. U.FILEVER ) ) THEN
        ! Ok
      ELSE IF ( ( .NOT. BLANK( CR.DIMSYS ) ) .AND.
     & ( CR.DIMSYS .NE. U.DIMSYS ) ) THEN
        FLAG = FLAG_D
      ELSE IF ( ( CR.NUMFORM .NE. 'NONE' ) .AND.
     & ( CR.NUMFORM .NE. U.NUMFORM ) ) THEN
        FLAG = FLAG_N
      ELSE IF ( ANGLE .NE. ' ' ) THEN
        FLAG = FLAG_A
      ELSE
        CALL MSG_WRITE( '*** No conversion required to file',
     &     REPORT )
        GO TO 190
      END IF
      IF ( .NOT. BLANK( FLAG_CL ) ) FLAG = FLAG_CL

      ! Read UDS file performing read conversions
      CALL UDS_READ( FILE_NAME, U, CR, IOS )
      IF ( IOS .NE. 0 ) GO TO 190
      IF ( ANGLE .EQ. 'DEG' ) THEN ! Convert radians->degrees
        CALL DS_RD_CONV( U.Y( U.NFP ), U.NLP-U.NFP+1, U.YUNITS,
     &     U.NUMFORM )
      ELSE IF ( ANGLE .EQ. 'RAD' ) THEN ! Convert degrees->radians
        CALL DS_DR_CONV( U.Y( U.NFP ), U.NLP-U.NFP+1, U.YUNITS,
     &     U.NUMFORM )
      END IF

      ! Prepare output fields
      IF ( .NOT. BLANK( CW.FILEVER ) ) U.FILEVER = CW.FILEVER

      ! Set default output file name
      OUT_FILE = FILE_NAME
      CALL FN_EXTRACT( OUT_FILE )
      IF ( CW.FILEVER .EQ. 'ASCII' ) THEN ! ASCII output file
        IT = FT_POSN_S( OUT_FILE, '.uds' )
        IF ( IT .EQ. 0 ) IT = LEN_TRIM( OUT_FILE ) + 1
        OUT_FILE(IT:) = '.asc'
      ELSE ! UDS output file
        IT = FT_POSN_S( OUT_FILE, '.uds' )
        IF ( IT .EQ. 0 ) IT = LEN_TRIM( OUT_FILE ) + 1
        OUT_FILE(IT:) = '.uds'
      END IF
      IF ( .NOT. NO_FLAG ) CALL FN_FLAG( OUT_FILE, FLAG, 2, I )
      CALL ADD_PATH( DIR_OUT, OUT_FILE )
      IF ( .NOT. NO_INCR ) CALL FN_INCR( OUT_FILE )
      CALL FN_NAME_EXTRACT( OUT_FILE )
      DEF_FILE = OUT_FILE

      ! Set the output UDS file name
      IF ( .NOT. BATCH ) THEN
  120   WRITE( *, '(/A/3A/'' >> '',$)' )
     &     ' Output file name?',
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

      ! Write the output file
      IF ( CW.FILEVER .EQ. 'ASCII' ) THEN ! Write ASCII file
        CALL ADD_PATH( DIR_OUT, OUT_FILE )
        CALL OPEN_FS( ASCII_UNIT, OUT_FILE, 'W', IOS )
        IF ( IOS .EQ. 0 )
     &     INQUIRE( FILE=OUT_FILE, NAME=OUT_FILE, IOSTAT=IOS )
        CALL MSG_WRITE( '  Writing: '//OUT_FILE, REPORT )
        IF ( IOS .NE. 0 ) THEN
          CALL MSG_WRITE( '*** ASCII output file open failed',
     &       REPORT )
          GO TO 190
        END IF
        IF ( CW.FILEFORM .EQ. 'X-Y' ) THEN
          DO I = U.NFP, U.NLP
            WRITE( ASCII_UNIT, * ) U.X(I), TAB, U.Y(I)
          END DO
        ELSE IF ( CW.FILEFORM .EQ. 'Y' ) THEN
          DO I = U.NFP, U.NLP
            WRITE( ASCII_UNIT, * ) U.Y(I)
          END DO
        ELSE IF ( CW.FILEFORM .EQ. 'X' ) THEN
          DO I = U.NFP, U.NLP
            WRITE( ASCII_UNIT, * ) U.X(I)
          END DO
        END IF
        CALL CLOSE_FILE( ASCII_UNIT )
        IF ( REPORT.OPEN ) WRITE( REPORT.UNIT, '(5X,A)' )
     &     CW.FILEFORM//' data ASCII file'
        CALL OUT_ENTRY( OUT_FILE, OUTPUT_LIST )
      ELSE ! Write UDS file
        CALL UDS_PUT( OUT_FILE, U, CW, REPORT, OUTPUT_LIST,
     &     DIR_OUT, .TRUE., IOS )
      END IF

      ! Loop for next file
  190 WRITE( *, '(80(''_''))' )
      GO TO 100

      ! Clean up and stop
  199 CALL CLEAN_UP( HIT, REPORT, OUTPUT_LIST, DIR_OUT )

      END



      SUBROUTINE UDSCONV_CL( CL_ARG, NUM_ARGS, PROG_NAME,
     & CL, ANGLE_CL, FLAG_CL, NO_FLAG )

!***********************************************************************
!* Processes the Program-Specific Command Line Arguments
!***********************************************************************

      ! Headers
      INCLUDE 'platform.fi'
      INCLUDE 'uds_str.fi'
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      CHARACTER*(*) CL_ARG(0:*) ! Command line argument array

      INTEGER NUM_ARGS ! Number of arguments

      CHARACTER*(*) PROG_NAME ! Program name

      RECORD /UDS_CONTROL/  CL ! UDS_CONTROL object for read processing

      CHARACTER*3 ANGLE_CL ! Angle conversion

      CHARACTER FLAG_CL ! Flag character for output file names

      LOGICAL NO_FLAG ! Indicates not to flag output file names


      ! Variables ______________________________________________________

      INTEGER I, NS

      CHARACTER ARG*255, C


      ! Process command line arguments
      CL.FILEVER = ' '
      CL.FILEFORM = ' '
      CL.NUMFORM = ' '
      CL.DIMSYS = ' '
      ANGLE_CL = ' '
      FLAG_CL = ' '
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
     &     ( ARG(NS:) .EQ. '??' ) .OR.
     &     ( ARG(NS:) .EQ. 'HELP' ) ) THEN ! Show command line syntax
          CALL SYNTAX_CL( PROG_NAME )
          WRITE( *, '(10X,A/10X,A)' )
     &     '[UDS|ASCII[=X|Y|X-Y]] [VAX|IEEE|SUN] [METRIC|SI|ENGLISH] ',
     &     '[DEGREES|RADIANS] [FLAG[=<flag>]|NO_FLAG]'
          IF ( ( ARG(NS:) .EQ. '??' ) .OR.
     &       ( ARG(NS:) .EQ. 'HELP' ) ) THEN ! Show command line help
            CALL HELP_CL( PROG_NAME )
            WRITE( *, '(10X,A)' )
     &       'UDS|ASCII :  UDS or ASCII output',
     &       '        [=X|Y|X-Y] :  '//
     &       'ASCII output in X, Y, or X-Y format [X-Y]',
     &       'VAX|IEEE|SUN|NONE : '//
     &       'VAX, IEEE, Sun, or don''t change UDS number format',
     &       'METRIC|SI|ENGLISH :  '//
     &       'Metric, SI, or English units system',
     &       'DEGREES|RADIANS :  '//
     &       'Degree or radians based units',
     &       'FLAG[=<flag>] : '//
     &       'Output files flagged with <flag> [conversion-based]',
     &       'NO_FLAG :  Don''t flag output file names'
          END IF
          STOP ' '
        ELSE IF ( ARG(NS:) .EQ. 'UDS' ) THEN
          CL.FILEVER = 'UDS-1992'
          CL.FILEFORM = ' '
          CL_ARG(I) = ' '
        ELSE IF ( ARG(NS:) .EQ. 'ASCII' ) THEN
          CL.FILEVER = 'ASCII'
          CL_ARG(I) = ' '
        ELSE IF ( ARG(NS:NS+5) .EQ. 'ASCII=' ) THEN
          CL.FILEVER = 'ASCII'
          CL.FILEFORM = ARG(NS+6:)
          IF ( ( CL.FILEFORM .NE. 'X' ) .AND.
     &       ( CL.FILEFORM .NE. 'Y' ) .AND.
     &       ( CL.FILEFORM .NE. 'X-Y' ) ) THEN
            WRITE( *, '(/2A/)' )
     &         '*** Unrecognized ASCII format ignored: ', CL.FILEFORM
            CL.FILEFORM = ' '
          END IF
          CL_ARG(I) = ' '
        ELSE IF ( ( ARG(NS:) .EQ. 'VAX' ) .OR.
     &     ( ARG(NS:) .EQ. 'IEEE' ) .OR.
     &     ( ARG(NS:) .EQ. 'SUN' ) .OR.
     &     ( ARG(NS:) .EQ. 'NONE'  ) ) THEN
          CL.NUMFORM = ARG(NS:)
          CL_ARG(I) = ' '
        ELSE IF ( ( ARG(NS:) .EQ. 'METRIC' ) .OR.
     &     ( ARG(NS:) .EQ. 'SI' ) .OR.
     &     ( ARG(NS:) .EQ. 'ENGLISH' ) ) THEN
          CL.DIMSYS = ARG(NS:)
          CL_ARG(I) = ' '
        ELSE IF ( ( ARG(NS:) .EQ. 'DEGREES' ) .OR.
     &     ( ARG(NS:) .EQ. 'DEG' ) .OR.
     &     ( ARG(NS:) .EQ. 'RADIANS' ) .OR.
     &     ( ARG(NS:) .EQ. 'RAD' ) ) THEN
          ANGLE_CL = ARG(NS:)
          CL_ARG(I) = ' '
        ELSE IF ( ARG(NS:) .EQ. 'FLAG' ) THEN
          FLAG_CL = ' '
          CL_ARG(I) = ' '
        ELSE IF ( ARG(NS:NS+4) .EQ. 'FLAG=' ) THEN
          FLAG_CL = CL_ARG(I)(NS+5:)
        ELSE IF ( ( ARG(NS:) .EQ. 'NO_FLAG' ) .OR.
     &     ( ARG(NS:) .EQ. 'NOFLAG' ) ) THEN
          NO_FLAG = .TRUE.
          FLAG_CL = ' '
          CL_ARG(I) = ' '
        END IF
      END DO

      ! Warn about incompatible switches
      IF ( ( CL.FILEVER .EQ. 'ASCII' ) .AND.
     & ( .NOT. BLANK( CL.NUMFORM ) ) .AND.
     & ( CL.NUMFORM .NE. NUMFORM_P ) ) THEN
        WRITE( *, '(/A/)' )
     &     '*** Number format switch ignored for ASCII files'
      END IF

      RETURN
      END



      SUBROUTINE CONV_PROF( CL, CR, CW, ANGLE_CL, ANGLE,
     & FLAG_N, FLAG_D, FLAG_A, EOF )

!***********************************************************************
!* Gets Conversion Profile for UDSConv
!***********************************************************************

      ! Headers
      INCLUDE 'platform.fi'
      INCLUDE 'uds_str.fi'
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      RECORD /UDS_CONTROL/  CL, CR, CW

      CHARACTER*3 ANGLE_CL

      CHARACTER*3 ANGLE

      CHARACTER FLAG_N ! Numeric format flag

      CHARACTER FLAG_D ! Dimensional system flag

      CHARACTER FLAG_A ! Angular format flag

      LOGICAL EOF


      ! Variables ______________________________________________________

      LOGICAL CONV_CL

      CHARACTER RESP


      ! Functions ______________________________________________________

      CHARACTER DNCASE

      EXTERNAL DNCASE


      ! See if conversions specified on command line
      IF ( ( .NOT. BLANK( CL.FILEVER ) ) .OR.
     & ( .NOT. BLANK( CL.NUMFORM ) ) .OR.
     & ( .NOT. BLANK( CL.DIMSYS ) ) .OR.
     & ( .NOT. BLANK( ANGLE_CL ) ) ) THEN ! Command line conversions
        CONV_CL = .TRUE.
      ELSE ! Prompt for conversions
        CONV_CL = .FALSE.
      END IF

      ! File type
  100 IF ( .NOT. CONV_CL ) THEN
  101   WRITE( *, '(/2A,31X,A/'' >> '',$)' ) ' Target file type?',
     &     '   ( U - UDS,  A - ASCII )', '[UDS]'
        READ( *, '(A)', END=199 ) RESP
        CALL STR_UP( RESP )
        IF ( RESP .EQ. 'U' ) THEN ! UDS
          CW.FILEVER = 'UDS-1992'
        ELSE IF ( RESP .EQ. ' ' ) THEN ! UDS
          CW.FILEVER = ' '
        ELSE IF ( RESP .EQ. 'A' ) THEN ! ASCII
          CW.FILEVER = 'ASCII'
        ELSE
          WRITE( *, * ) '*** Invalid response'
          GO TO 101
        END IF
      ELSE ! Use command line value
        CW.FILEVER = CL.FILEVER
      END IF

      ! ASCII output format
      IF ( CW.FILEVER .EQ. 'ASCII' ) THEN
        IF ( ( BLANK( CL.FILEFORM ) ) .AND. ( .NOT. CONV_CL ) ) THEN
  102     WRITE( *, '(/A,44X,A/'' >> '',$)' )
     &       ' ASCII file format?   (X/Y/X-Y)', '[X-Y]'
          READ( *, '(A)', END=100 ) CW.FILEFORM
          CALL STR_UP( CW.FILEFORM )
          IF ( ( CW.FILEFORM .EQ. ' ' ) .OR.
     &       ( CW.FILEFORM .EQ. 'X-Y' ) .OR.
     &       ( CW.FILEFORM .EQ. 'XY' ) ) THEN
            CW.FILEFORM='X-Y'
          ELSE IF ( ( CW.FILEFORM .NE. 'X' ) .AND.
     &       ( CW.FILEFORM .NE. 'Y' ) ) THEN
            WRITE( *, * ) '*** Invalid response'
            GO TO 102
          END IF
        ELSE IF ( BLANK( CL.FILEFORM ) ) THEN
          CW.FILEFORM = 'X-Y'
        ELSE ! Use command line value
          CW.FILEFORM = CL.FILEFORM
        END IF
      END IF

      ! Number format
      IF ( ( CW.FILEVER .EQ. 'UDS-1992' ) .OR.
     & ( BLANK( CW.FILEVER ) ) ) THEN
        IF ( .NOT. CONV_CL ) THEN
  103     WRITE( *, '(/3A/'' >> '',$)' )
     &       ' Target number format?',
     &       '   ( V - VAX,  I - IEEE,  S - Sun,  N - No change )',
     &       '['//NUMFORM_P//']'
          READ( *, '(A)', END=100 ) RESP
          CALL STR_UP( RESP )
          IF ( .NOT. ANY_CHARS( RESP, ' VISN' ) ) THEN
            WRITE( *, * ) '*** Invalid response'
            GO TO 103
          END IF
          IF ( RESP .EQ. 'V' ) THEN
            CR.NUMFORM = 'VAX'
            FLAG_N = 'v'
          ELSE IF ( RESP .EQ. 'I' ) THEN
            CR.NUMFORM = 'IEEE'
            FLAG_N = 'i'
          ELSE IF ( RESP .EQ. 'S' ) THEN
            CR.NUMFORM = 'SUN'
            FLAG_N = 's'
          ELSE IF ( RESP .EQ. 'N' ) THEN
            CR.NUMFORM = 'NONE'
            FLAG_N = ' '
          ELSE
            CR.NUMFORM = NUMFORM_P
            FLAG_N = DNCASE( CR.NUMFORM(1:1) )
          END IF
        ELSE IF ( CL.NUMFORM .EQ. ' ' ) THEN ! Convert to local format
          CR.NUMFORM = NUMFORM_P
          FLAG_N = DNCASE( CR.NUMFORM(1:1) )
        ELSE ! Use command line value
          CR.NUMFORM = CL.NUMFORM
          FLAG_N = DNCASE( CR.NUMFORM(1:1) )
        END IF
      ELSE ! Local format for ASCII files
        CR.NUMFORM = NUMFORM_P
        FLAG_N = DNCASE( CR.NUMFORM(1:1) )
      END IF

      ! Dimensional system
      IF ( .NOT. CONV_CL ) THEN
  104   WRITE( *, '(/2A,2X,A/'' >> '',$)' )
     &     ' Target dimensional system?',
     &     '   ( M - Metric,  S - SI,  E - English )', '[No change]'
        READ( *, '(A)', END=100 ) RESP
        CALL STR_UP( RESP )
        IF ( .NOT. ANY_CHARS( RESP, ' MSE' ) ) THEN
          WRITE( *, * ) '*** Invalid response'
          GO TO 104
        END IF
        IF ( RESP .EQ. 'M' ) THEN
          CR.DIMSYS = 'MET'
        ELSE IF ( RESP .EQ. 'S' ) THEN
          CR.DIMSYS = 'SI'
        ELSE IF ( RESP .EQ. 'E' ) THEN
          CR.DIMSYS = 'ENG'
        ELSE
          CR.DIMSYS = ' '
        END IF
      ELSE ! Use command line value
        CR.DIMSYS = CL.DIMSYS
      END IF
      FLAG_D = DNCASE( CR.DIMSYS(1:1) )

      ! Angular units base
      IF ( .NOT. CONV_CL ) THEN
  105   WRITE( *, '(/2A,10X,A/'' >> '',$)' )
     &     ' Target angular units base?',
     &     '   ( D - Degrees,  R - Radians )', '[No change]'
        READ( *, '(A)', END=100 ) RESP
        CALL STR_UP( RESP )
        IF ( .NOT. ANY_CHARS( RESP, ' DR' ) ) THEN
          WRITE( *, * ) '*** Invalid response'
          GO TO 105
        END IF
        IF ( RESP .EQ. 'D' ) THEN
          ANGLE = 'DEG'
        ELSE IF ( RESP .EQ. 'R' ) THEN
          ANGLE = 'RAD'
        ELSE
          ANGLE = ' '
        END IF
      ELSE ! Use command line value
        ANGLE = ANGLE_CL
      END IF
      FLAG_A = DNCASE( ANGLE )

      ! Check for no conversions specified
      IF ( ( ( BLANK( CW.FILEVER ) ) .OR.
     & ( CW.FILEVER .EQ. 'UDS-1992' ) ) .AND.
     & ( CR.NUMFORM .EQ. 'NONE' ) .AND.
     & ( CR.DIMSYS .EQ. ' ' ) .AND.
     & ( ANGLE .EQ. ' ' ) ) THEN
        WRITE( *, '(/A)' ) ' *** No conversions specified'
        CONV_CL = .FALSE.
        GO TO 100
      END IF

      ! Normal return
      EOF = .FALSE.
      RETURN

      ! Ctrl-Z handler
  199 EOF = .TRUE.
      RETURN

      END
