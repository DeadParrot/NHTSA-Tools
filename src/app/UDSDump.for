      PROGRAM UDSDUMP

!***********************************************************************
!* UDS File Specification and Measurement Display to Screen or File
!*
!* Language: Fortran
!*
!* Author: Stuart G. Mentzer
!*
!* Date: 2003/07/18
!***********************************************************************

      ! Headers
      INCLUDE 'platform.fi'
      INCLUDE 'uds_str.fi'
      INCLUDE 'uds_fxn.fi'
      INCLUDE 'file.fi'


      ! Variables ______________________________________________________

      RECORD /UDS/  U
      RECORD /UDS_CONTROL/  CS, C
      RECORD /FILE/  HIT, REPORT, OUTPUT_LIST

      LOGICAL ALL_BATCH, NO_BATCH, NO_INCR, ONE_PASS, NEW_HIT, EOF,
     & BATCH, NO_SHOW, MSG, SCREEN_OUT

      PARAMETER ( NO_SHOW = .FALSE., MSG = .TRUE. )

      INTEGER NUM_ARGS, MAX_ARGS, IOS, I,
     & NUM_FIELDS, MAX_FIELDS, DUMP_UNIT, VAL_INT

      PARAMETER ( MAX_ARGS = 12, MAX_FIELDS = 100 )

      REAL VAL_REAL

      CHARACTER CL_ARG(0:MAX_ARGS)*255, PROG_NAME*8,
     & TEMPLATE_STR*255, DIR_OUT*255,
     & FILE_NAME*255, OUT_FILE*255,
     & PROMPT*79,
     & OUT_TYP_CL, FIELD_NAME_CL*25, OUT_DEV_CL*255, FLAG_LET,
     & OUT_TYP*25, FIELD_NAME*25, OUT_DEV*255, OUT_MODE,
     & FIELD_NAMES(MAX_FIELDS)*25,
     & NUMFORM_F*5, VAL_TYP, VAL_STR*512


      ! Functions ______________________________________________________

      CHARACTER UPCASE*255

      EXTERNAL UPCASE


      ! Initializations
      PROG_NAME = 'UDSDump'
      CALL FILE_INIT( HIT )
      CALL GET_CL( CL_ARG, NUM_ARGS, MAX_ARGS )
      CALL UDSDUMP_CL( CL_ARG, NUM_ARGS, PROG_NAME,
     & OUT_TYP_CL, FIELD_NAME_CL, OUT_DEV_CL, FLAG_LET )
      CALL PROC_CL( CL_ARG, NUM_ARGS, PROG_NAME, TEMPLATE_STR,
     & ALL_BATCH, NO_BATCH, NO_INCR, REPORT, OUTPUT_LIST, DIR_OUT,
     & ONE_PASS, .TRUE., .TRUE. )
      PROMPT = 'UDS file name/template/hitlist?'
      PROMPT(74:) = '[done]'
      CALL UDS_CONTROL_INIT( CS )
      CALL UDS_CONTROL_INIT( C )
      CS.SPECS_ONLY = .TRUE.
      CS.NUMFORM = 'NONE'
      OUT_MODE = ' '
      DUMP_UNIT = -1
      SCREEN_OUT = .TRUE.

      WRITE( *, '(//27X,A//)' ) '**  Display UDS Files  **'

      ! Read the file name/template/hitlist
  100 CALL FILE_PROMPT( TEMPLATE_STR, PROMPT, DIR_OUT,
     & ONE_PASS, HIT, NEW_HIT, FILE_NAME, EOF )
      IF ( ( FILE_NAME .EQ. ' ' ) .OR. ( EOF ) ) GO TO 199 ! Done

      ! Batch setup
      CALL BAT_SET( ALL_BATCH, NO_BATCH, NEW_HIT, HIT, BATCH )

      ! Batch prompts
      IF ( ( NEW_HIT ) .AND. ( BATCH ) ) THEN

        ! Get output type
  101   IF ( OUT_TYP_CL .EQ. ' ' ) THEN
          WRITE( *, '(/2A,9X,A/'' >> '',$)' )
     &       ' Output type/field?   ',
     &       '( S - Specs,  F - Full,  ? - list fields )',
     &       '[Specs]'
          READ( *, '(A)', IOSTAT=IOS ) OUT_TYP
          IF ( IOS .NE. 0 ) THEN
            CALL MSG_WRITE( '*** Operation skipped', REPORT )
            CALL FILE_DEL( HIT )
            GO TO 190
          END IF
          CALL STR_UP( OUT_TYP )
          IF ( ( .NOT. ANY_CHARS( OUT_TYP(1:1), ' SF' ) ) .OR.
     &       ( OUT_TYP(2:) .NE. ' ' ) ) THEN
            FIELD_NAME = OUT_TYP
            OUT_TYP = 's'
          END IF
        ELSE
          OUT_TYP = OUT_TYP_CL
          FIELD_NAME = FIELD_NAME_CL
        END IF

        ! Check field name / Get more fields
        IF ( OUT_TYP .EQ. 's' ) THEN

          ! Check field name
          CALL UDS_SPEC_CHK( FIELD_NAME, MSG, IOS )
          IF ( IOS .NE. 0 ) THEN
            IF ( ( FIELD_NAME_CL .EQ. ' ' ) .OR.
     &         ( .NOT. ONE_PASS ) ) THEN
              OUT_TYP_CL = ' '
              FIELD_NAME_CL = ' '
              GO TO 101
            ELSE
              CALL MSG_WRITE( '*** Operation skipped', REPORT )
              CALL FILE_DEL( HIT )
              GO TO 190
            END IF
          END IF

          ! Set first field array elements
          NUM_FIELDS = 1
          FIELD_NAMES(1) = FIELD_NAME

          ! Get more fields
  102     IF ( ( FIELD_NAME_CL .EQ. ' ' ) .AND.
     &       ( NUM_FIELDS .LT. MAX_FIELDS ) ) THEN
            WRITE( *, '(/A,33X,A/'' >> '',$)' )
     &         ' Next output field?   ( ? - list fields )',
     &         '[done]'
            READ( *, '(A)', IOSTAT=IOS ) FIELD_NAME
            CALL STR_UP( FIELD_NAME )
            IF ( IOS .NE. 0 ) THEN
              CALL MSG_WRITE( '*** Operation skipped', REPORT )
              CALL FILE_DEL( HIT )
              GO TO 190
            END IF
            IF ( FIELD_NAME .NE. ' ' ) THEN
              ! Check field name
              CALL UDS_SPEC_CHK( FIELD_NAME, MSG, IOS )
              IF ( IOS .NE. 0 ) GO TO 102
              ! Set field array elements
              NUM_FIELDS = NUM_FIELDS + 1
              FIELD_NAMES(NUM_FIELDS) = FIELD_NAME
              GO TO 102
            END IF
          END IF

        END IF

        ! Get output device/file
  103   IF ( OUT_DEV_CL .EQ. ' ' ) THEN
          WRITE( *, '(/2A,9X,A/'' >> '',$)' )
     &       ' Output device/file name?   ',
     &       '( S - Screen,  F - Flagged files ) ', '[Screen]'
          READ( *, '(A)', IOSTAT=IOS ) OUT_DEV
          IF ( IOS .NE. 0 ) THEN
            CALL MSG_WRITE( '*** Operation skipped', REPORT )
            CALL FILE_DEL( HIT )
            GO TO 190
          END IF
          FLAG_LET = ' '
        ELSE
          OUT_DEV = OUT_DEV_CL
        END IF
        IF ( ( OUT_DEV .EQ. ' ' ) .OR.
     &     ( UPCASE( OUT_DEV ) .EQ. 'S' ) ) THEN ! Screen
          SCREEN_OUT = .TRUE.
          IF ( DUMP_UNIT .GE. 0 ) CALL CLOSE_FILE( DUMP_UNIT )
          DUMP_UNIT = -1
          OUT_MODE = 'S'
        ELSE IF ( UPCASE( OUT_DEV ) .EQ. 'F' ) THEN ! Flagged files
          SCREEN_OUT = .FALSE.
          OUT_DEV = 'F'
          IF ( FLAG_LET .EQ. ' ' ) THEN
            WRITE( *, '(/A,64X,A/'' >> '',$)' )
     &         ' Flag letter?', '[v]'
            READ( *, '(A)', END=103 ) FLAG_LET
            IF ( FLAG_LET .EQ. ' ' ) FLAG_LET = 'v'
          END IF
          OUT_MODE = 'I'
        ELSE ! Open output file
          SCREEN_OUT = .FALSE.
          OUT_FILE = OUT_DEV
          CALL ADD_PATH( DIR_OUT, OUT_FILE )
          IF ( ( FN_EQUAL( OUT_DEV, SCREEN_DEV ) ) .OR.
     &       ( .NOT. FN_VALID( OUT_FILE ) ) ) THEN
            WRITE( *, * ) '*** Invalid file name'
            OUT_DEV_CL = ' '
            GO TO 103
          END IF
          IF ( .NOT. NO_INCR ) CALL FN_INCR( OUT_FILE )
          IF ( DUMP_UNIT .GE. 0 ) CALL CLOSE_FILE( DUMP_UNIT )
          CALL OPEN_FS( DUMP_UNIT, OUT_FILE, 'W', IOS )
          IF ( IOS .NE. 0 ) THEN
            WRITE( *, * ) '*** Output file open failed'
            OUT_DEV_CL = ' '
            GO TO 103
          END IF
          OUT_MODE = 'A'
        END IF

      END IF

      ! Read UDS file
      CALL REP_COLS( REPORT )
      CALL UDS_GET( FILE_NAME, U, CS, REPORT, IOS )
      IF ( IOS .NE. 0 ) GO TO 190
      NUMFORM_F = U.NUMFORM ! Store file NUMFORM
      CALL UDS_READ( FILE_NAME, U, C, IOS )
      U.NUMFORM = NUMFORM_F ! Set to file NUMFORM for display
      IF ( IOS .NE. 0 ) GO TO 190

      IF ( .NOT. BATCH ) THEN ! Interactive prompts

        ! Get output type
  104   IF ( OUT_TYP_CL .EQ. ' ' ) THEN
          WRITE( *, '(/2A,9X,A/'' >> '',$)' )
     &       ' Output type/field?   ',
     &       '( S - Specs,  F - Full,  ? - list fields )',
     &       '[Specs]'
          READ( *, '(A)', END=190 ) OUT_TYP
          CALL STR_UP( OUT_TYP )
          IF ( ( .NOT. ANY_CHARS( OUT_TYP(1:1), ' SF' ) ) .OR.
     &       ( OUT_TYP(2:) .NE. ' ' ) ) THEN
            FIELD_NAME = OUT_TYP
            OUT_TYP = 's'
          END IF
        ELSE
          OUT_TYP = OUT_TYP_CL
          FIELD_NAME = FIELD_NAME_CL
        END IF

        ! Check field name / Get more fields
        IF ( OUT_TYP .EQ. 's' ) THEN

          ! Check field name
          CALL UDS_SPEC_CHK( FIELD_NAME, MSG, IOS )
          IF ( IOS .NE. 0 ) THEN
            OUT_TYP_CL = ' '
            FIELD_NAME_CL = ' '
            GO TO 104
          END IF

          ! Set first field array elements
          NUM_FIELDS = 1
          FIELD_NAMES(1) = FIELD_NAME

          ! Get more fields
  105     IF ( ( FIELD_NAME_CL .EQ. ' ' ) .AND.
     &       ( NUM_FIELDS .LT. MAX_FIELDS ) ) THEN
            WRITE( *, '(/A,33X,A/'' >> '',$)' )
     &         ' Next output field?   ( ? - list fields )',
     &         '[done]'
            READ( *, '(A)', IOSTAT=IOS ) FIELD_NAME
            CALL STR_UP( FIELD_NAME )
            IF ( IOS .NE. 0 ) THEN
              CALL MSG_WRITE( '*** Operation skipped', REPORT )
              CALL FILE_DEL( HIT )
              GO TO 190
            END IF
            IF ( FIELD_NAME .NE. ' ' ) THEN
              ! Check field name
              CALL UDS_SPEC_CHK( FIELD_NAME, MSG, IOS )
              IF ( IOS .NE. 0 ) GO TO 105
              ! Set field array elements
              NUM_FIELDS = NUM_FIELDS + 1
              FIELD_NAMES(NUM_FIELDS) = FIELD_NAME
              GO TO 105
            END IF
          END IF

        END IF

        ! Get output device/file
  106   IF ( OUT_DEV_CL .EQ. ' ' ) THEN
          IF ( OUT_MODE .EQ. 'A' ) THEN ! Output open for pot append
            WRITE( *, '(/2A/'' >> '',$)' )
     &         ' Output device/file name?   ',
     &         '( S - Screen,  F - Flagged,  A - Append )   [Screen]'
          ELSE ! Not currently appending
            WRITE( *, '(/2A,10X,A/'' >> '',$)' )
     &         ' Output device/file name?   ',
     &         '( S - Screen,  F - Flagged file ) ', '[Screen]'
          END IF
          READ( *, '(A)', END=190 ) OUT_DEV
          FLAG_LET = ' '
        ELSE
          OUT_DEV = OUT_DEV_CL
        END IF
        IF ( ( OUT_MODE .EQ. 'A' ) .AND.
     &   ( UPCASE( OUT_DEV ) .EQ. 'A' ) ) THEN ! Append to current file
          OUT_DEV = 'A'
        ELSE IF ( ( OUT_DEV .EQ. ' ' ) .OR.
     &     ( UPCASE( OUT_DEV ) .EQ. 'S' ) ) THEN ! Screen
          SCREEN_OUT = .TRUE.
          OUT_MODE = 's'
        ELSE IF ( UPCASE( OUT_DEV ) .EQ. 'F' ) THEN ! Flagged files
          SCREEN_OUT = .FALSE.
          OUT_DEV = 'F'
          IF ( FLAG_LET .EQ. ' ' ) THEN
            WRITE( *, '(/A,64X,A/'' >> '',$)' )
     &         ' Flag letter?', '[v]'
            READ( *, '(A)', END=106 ) FLAG_LET
            IF ( FLAG_LET .EQ. ' ' ) FLAG_LET = 'v'
          END IF
          OUT_MODE = 'I'
        ELSE ! Specified file
          SCREEN_OUT = .FALSE.
          OUT_FILE = OUT_DEV
          CALL ADD_PATH( DIR_OUT, OUT_FILE )
          IF ( ( FN_EQUAL( OUT_DEV, SCREEN_DEV ) ) .OR.
     &       ( .NOT. FN_VALID( OUT_FILE ) ) ) THEN
            WRITE( *, * ) '*** Invalid file name'
            OUT_DEV_CL = ' '
            GO TO 106
          END IF
          IF ( .NOT. NO_INCR ) CALL FN_INCR( OUT_FILE )
          OUT_MODE = 'a'
        END IF

        ! Open non-individual device
        IF ( ( OUT_MODE .EQ. 'a' ) .OR.
     &     ( OUT_MODE .EQ. 's' ) ) THEN ! Open device
          IF ( DUMP_UNIT .GE. 0 ) CALL CLOSE_FILE( DUMP_UNIT )
          IF ( SCREEN_OUT ) THEN
            DUMP_UNIT = -1
          ELSE ! File output
            CALL OPEN_FS( DUMP_UNIT, OUT_FILE, 'W', IOS )
            IF ( IOS .NE. 0 ) THEN
              WRITE( *, * ) '*** Output device open failed'
              OUT_DEV_CL = ' '
              GO TO 106
            END IF
          END IF
          OUT_MODE = UPCASE( OUT_MODE )
        END IF

      END IF

      ! Set up flagged output file name
      IF ( OUT_DEV .EQ. 'F' ) THEN ! Flagged output file name
        OUT_FILE = FILE_NAME
        CALL FN_EXTRACT( OUT_FILE )
        CALL FN_FLAG( OUT_FILE, FLAG_LET, 2, I )
        CALL ADD_PATH( DIR_OUT, OUT_FILE )
        IF ( .NOT. NO_INCR ) CALL FN_INCR( OUT_FILE )
      END IF

      ! Open individual output device
      IF ( OUT_MODE .EQ. 'I' ) THEN ! Open new output file
        IF ( DUMP_UNIT .GE. 0 ) CALL CLOSE_FILE( DUMP_UNIT )
        IF ( .NOT. NO_INCR ) CALL FN_INCR( OUT_FILE )
        CALL OPEN_FS( DUMP_UNIT, OUT_FILE, 'W', IOS )
        IF ( IOS .NE. 0 ) THEN
          CALL MSG_WRITE( '*** Output device open failed', REPORT )
          GO TO 190
        END IF
      END IF

      ! Report the file write
      IF ( .NOT. SCREEN_OUT ) THEN
        INQUIRE( FILE=OUT_FILE, NAME=OUT_FILE, IOSTAT=IOS )
        CALL MSG_WRITE( '  Writing: '//OUT_FILE, REPORT )
        CALL OUT_ENTRY( OUT_FILE, OUTPUT_LIST )
      END IF

      ! Dump the UDS file/field
      IF ( OUT_TYP .NE. 's' ) THEN
        IF ( SCREEN_OUT ) THEN
          WRITE( *, '(/1X,79(''*''))' )
          WRITE( *, '(1X,A)', IOSTAT=IOS )
     &       FILE_NAME(:L_TRIM(FILE_NAME))
        ELSE ! File output
          WRITE( DUMP_UNIT, '(/79(''*''))' )
          WRITE( DUMP_UNIT, '(A)', IOSTAT=IOS )
     &       FILE_NAME(:L_TRIM(FILE_NAME))
        END IF
        CALL SPEC_DUMP( DUMP_UNIT, U )
        IF ( OUT_TYP .EQ. 'F' ) CALL MEAS_DUMP( DUMP_UNIT, U )
      ELSE ! Dump selected specification fields
        IF ( .NOT. SCREEN_OUT ) THEN
          WRITE( DUMP_UNIT, '(//A)', IOSTAT=IOS )
     &       FILE_NAME(:L_TRIM(FILE_NAME))
        END IF
        DO I = 1, NUM_FIELDS
          CALL UDS_SPEC_GET( U, FIELD_NAMES(I), VAL_TYP,
     &       VAL_INT, VAL_REAL, VAL_STR, NO_SHOW, MSG, IOS )
          IF ( SCREEN_OUT ) THEN
            WRITE( *, '(/1X,5X,3A)', IOSTAT=IOS )
     &         FIELD_NAMES(I)(:L_TRIM(FIELD_NAMES(I))), ' = ',
     &         VAL_STR(:L_TRIM(VAL_STR))
          ELSE ! File output
            WRITE( DUMP_UNIT, '(/5X,3A)', IOSTAT=IOS )
     &         FIELD_NAMES(I)(:L_TRIM(FIELD_NAMES(I))), ' = ',
     &         VAL_STR(:L_TRIM(VAL_STR))
          END IF
          IF ( REPORT.OPEN )
     &       WRITE( REPORT.UNIT, '(5X,3A)', IOSTAT=IOS )
     &       FIELD_NAMES(I)(:L_TRIM(FIELD_NAMES(I))), ' = ',
     &       VAL_STR(:L_TRIM(VAL_STR))
        END DO
      END IF

      ! Loop for next file
      IF ( ( SCREEN_OUT ) .AND. ( OUT_TYP .NE. 's' ) .AND.
     & ( HIT.OPEN ) ) THEN
        WRITE( *, '(1X,50(''_''),A,$)' )
     &     'Press [Return] to continue >>'
        READ( *, '(A)', IOSTAT=IOS )
        IF ( IOS .EQ. -1 ) THEN ! Ctrl-Z entered
          CALL MSG_WRITE( '*** Operation terminated', REPORT )
          CALL FILE_DEL( HIT )
          GO TO 100
        END IF
        GO TO 100
      END IF
  190 WRITE( *, '(80(''_''))' )
      GO TO 100

      ! Clean up and stop
  199 CALL CLEAN_UP( HIT, REPORT, OUTPUT_LIST, DIR_OUT )

      END



      SUBROUTINE UDSDUMP_CL( CL_ARG, NUM_ARGS, PROG_NAME,
     & OUT_TYP_CL, FIELD_NAME_CL, OUT_DEV_CL, FLAG_LET )

!***********************************************************************
!* Processes the Program-Specific Command Line Arguments
!***********************************************************************


      ! Arguments ______________________________________________________

      CHARACTER*(*) CL_ARG(0:*) ! Command line argument array

      INTEGER NUM_ARGS ! Number of arguments

      CHARACTER*(*) PROG_NAME ! Program name

      CHARACTER OUT_TYP_CL ! Output type code

      CHARACTER*(*) FIELD_NAME_CL ! Field to output

      CHARACTER*(*) OUT_DEV_CL ! Output device/file code

      CHARACTER FLAG_LET ! Output file name flag character


      ! Variables ______________________________________________________

      INTEGER I, NS

      CHARACTER ARG*255, C


      ! Process command line arguments
      OUT_TYP_CL = ' '
      FIELD_NAME_CL = ' '
      OUT_DEV_CL = ' '
      FLAG_LET = ' '
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
     &       '[SPECS|FULL|FIELD=<field>] '//
     &       '[SCREEN|FLAG[=<flag>]|FILE=<file>]'
          IF ( ( ARG(NS:) .EQ. '??' ) .OR.
     &       ( ARG(NS:) .EQ. 'HELP' ) ) THEN ! Show command line help
            CALL HELP_CL( PROG_NAME )
            WRITE( *, '(5(10X,A/),10X,A)' )
     &         'SPECS :  Dump specifications only',
     &         'FULL :  Dump specifications and data',
     &         '<field> :  Dump this specification field',
     &         'SCREEN :  Dump to screen',
     &         'FLAG[=<flag>] :  Dump to file named with <flag> [v]',
     &         '<file> :  Dump to file with this name [UDSDump.dmp]'
          END IF
          STOP ' '
        ELSE IF ( ARG(NS:) .EQ. 'SPECS' ) THEN
          OUT_TYP_CL = 'S'
          CL_ARG(I) = ' '
        ELSE IF ( ARG(NS:) .EQ. 'FULL' ) THEN
          OUT_TYP_CL = 'F'
          CL_ARG(I) = ' '
        ELSE IF ( ( ARG(NS:NS+5) .EQ. 'FIELD=' ) .OR.
     &     ( ARG(NS:NS+5) .EQ. 'FIELD#' ) ) THEN
          OUT_TYP_CL = 's'
          FIELD_NAME_CL = ARG(NS+6:)
          CL_ARG(I) = ' '
        ELSE IF ( ARG(NS:) .EQ. 'SCREEN' ) THEN
          OUT_DEV_CL = 'S'
          CL_ARG(I) = ' '
        ELSE IF ( ( ARG(NS:NS+4) .EQ. 'FLAG=' ) .OR.
     &     ( ARG(NS:NS+4) .EQ. 'FLAG#' ) ) THEN
          OUT_DEV_CL = 'F'
          FLAG_LET = CL_ARG(I)(NS+5:NS+5)
          IF ( FLAG_LET .EQ. ' ' ) FLAG_LET = 'v'
          CL_ARG(I) = ' '
        ELSE IF ( ARG(NS:) .EQ. 'FLAG' ) THEN
          OUT_DEV_CL = 'F'
          FLAG_LET = 'v'
          CL_ARG(I) = ' '
        ELSE IF ( ( ARG(NS:NS+4) .EQ. 'FILE=' ) .OR.
     &     ( ARG(NS:NS+4) .EQ. 'FILE#' ) ) THEN
          OUT_DEV_CL = CL_ARG(I)(NS+5:)
          IF ( OUT_DEV_CL .EQ. ' ' )
     &       OUT_DEV_CL = 'UDSDump.dmp'
          CL_ARG(I) = ' '
        ELSE IF ( ARG(NS:) .EQ. 'FILE' ) THEN
          OUT_DEV_CL = 'UDSDump.dmp'
          CL_ARG(I) = ' '
        END IF
      END DO

      RETURN
      END



      SUBROUTINE SPEC_DUMP( DUMP_UNIT, U )

!***********************************************************************
!* Output UDS File Specification Data
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'
      INCLUDE 'uds_fxn.fi'


      ! Arguments ______________________________________________________

      INTEGER DUMP_UNIT ! Logical i/o unit for output

      RECORD /UDS/  U ! UDS object


      ! Variables ______________________________________________________

      LOGICAL SUPP_HDR

      INTEGER I, IOS, LN, LXT, LXU, NFILL

      REAL XMIN, XMAX, YMIN, YMAX

      CHARACTER BUFF*132, SPUNITS*6, WTUNITS*6, HICNAM*7,
     & HICDT*3, NAMO*12, VEHDSC*41, FMT*30


      ! Functions ______________________________________________________

      CHARACTER LJUST*12

      EXTERNAL LJUST


      ! Set variables used to display speed and weight units.
      IF ( U.DIMSYS .EQ. 'MET' ) THEN
        SPUNITS = ' (kph)'
        WTUNITS = ' (kgs)'
      ELSE IF ( U.DIMSYS .EQ. 'SI' ) THEN
        SPUNITS = ' (m/s)'
        WTUNITS = ' (kgs)'
      ELSE IF ( U.DIMSYS .EQ. 'ENG' ) THEN
        SPUNITS = ' (mph)'
        WTUNITS = ' (lbs)'
      ELSE
        SPUNITS = ' '
        WTUNITS = ' '
      END IF


      ! File information; exclude NCHAN (number of channels)
      WRITE( BUFF, '(20(''-''),A,41(''-''))' )
     & ' FILE INFORMATION '
      CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
      WRITE( BUFF, '(8A)' ) 'Version: ', U.FILEVER(:10),
     & '  Form: ', U.FILEFORM, '  Number Format: ', U.NUMFORM,
     & '   Units System: ', U.DIMSYS
      CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )


      ! Test information
      NAMO = LJUST( U.TSTNAM )
      LN = L_TRIM( NAMO )
      NFILL = 40 - LN
      FMT = '(  20(''-''),3A,  (''-''))'
      WRITE( FMT(15:16), '(I2)', IOSTAT=IOS ) NFILL
      WRITE( BUFF, FMT ) ' TEST ', NAMO(:LN), ' INFORMATION '
      CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
      WRITE( BUFF, '(3A,I5,3A)' ) 'Source: ', U.TSTSRC,
     & '   No: ', U.TSTNO, '   Performer: ', U.TSTPRF
      CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
      WRITE( BUFF, '(5A,1PG14.7,A)', IOSTAT=IOS )
     & 'Ref No: ', U.TSTREF, '    Configuration: ', U.TSTCFN,
     & '    Closing Speed: ', U.CLSSPD, SPUNITS
      CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
      IF ( U.IMPANG .NE. 0 ) THEN
        WRITE( BUFF, '(A,I3,A)', IOSTAT=IOS )
     &     'Impact Angle: ', U.IMPANG, ' (deg)'
        CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
      END IF
      IF ( .NOT. BLANK( U.TITLE ) ) THEN
        WRITE( BUFF, '(2A)' ) 'Title: ', U.TITLE
        CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
      END IF


      ! Vehicle/component information
      IF ( U.TSTSRC .NE. 'COMPONENT DB' ) THEN

        ! Vehicle/barrier information
        IF ( .NOT. BLANK( U.MAKE ) ) THEN
          IF ( U.VEHNO .NE. 0 ) THEN ! Assume it is a vehicle
            WRITE( BUFF, '(20(''-''),A,I1,A,36(''-''))',
     &         IOSTAT=IOS ) ' VEHICLE ', U.VEHNO, ' INFORMATION '
            CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
            VEHDSC = U.MAKE
            VEHDSC(LEN_TRIM(VEHDSC)+2:) = U.MODEL
            WRITE( BUFF, '(A,I4,1X,A,T58,A,F9.3,A)', IOSTAT=IOS )
     &         'Vehicle: ', U.YEAR, VEHDSC,
     &         'Speed: ', U.VEHSPD, SPUNITS
            CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
            WRITE( BUFF, '(5A,F9.3,A)', IOSTAT=IOS )
     &         'Body: ', U.BODY, ' Engine Type: ', U.ENGINE,
     &         '  Test Weight: ', U.VEHTWT, WTUNITS
            CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
          ELSE ! Assume it is a barrier
            WRITE( BUFF, '(20(''-''),A,38(''-''))' )
     &         ' BARRIER INFORMATION '
            CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
            WRITE( BUFF, '(4A)' )
     &         'Barrier: ', U.MAKE, ' ', U.MODEL
            CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
          END IF
        END IF

        ! Occupant information
        IF ( .NOT. BLANK( U.OCCTYP ) ) THEN
          WRITE( BUFF, '(20(''-''),A,37(''-''))' )
     &       ' OCCUPANT INFORMATION '
          CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
          WRITE( BUFF, '(3A,I2,3A,F8.3,3A)', IOSTAT=IOS )
     &       'Type: ', U.OCCTYP, '    Age: ', U.OCCAGE,
     &       '    Sex: ', U.OCCSEX, '    Weight: ', U.OCCWT, WTUNITS,
     &       '    Dummy Size: ', U.DUMSIZ
          CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
          IF ( ( .NOT. BLANK( U.RESTR1 ) ) .OR.
     &       ( .NOT. BLANK( U.RESTR2 ) ) ) THEN
            WRITE( BUFF, '(4A)' )
     &         'Primary Restraint: ', U.RESTR1,
     &         ' Secondary Restraint: ', U.RESTR2(:L_TRIM(U.RESTR2))
            CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
          END IF
          IF ( U.SENATT .EQ. 'HEAD CG' ) THEN
            IF ( U.HICDTUP .EQ. 0. ) THEN
              HICNAM = 'HIC:'
            ELSE
              HICNAM = 'HIC'
              WRITE( HICDT, '(I3)', IOSTAT=IOS ) NINT( U.HICDTUP )
              HICNAM(4:) = LJUST( HICDT )
              HICNAM(LEN_TRIM(HICNAM)+1:) = ':'
            END IF
            WRITE( BUFF, '(A,F9.3,A,F9.3,A,F9.3,A,15X,2A)',
     &         IOSTAT=IOS ) HICNAM, U.HIC, '    T1: ', U.T1,
     &         '  T2: ', U.T2, ' (msec)', 'AIS: ', U.AIS
            CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
          ELSE IF ( U.SENATT(:6) .EQ. 'CHEST' ) THEN
            WRITE( BUFF, '(A,F8.2,7X,A,F9.2,22X,2A)',
     &         IOSTAT=IOS )
     &         'Chest 3 Msec Clip: ', U.CLIP3M, 'CSI: ', U.CSI,
     &         'AIS: ', U.AIS
            CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
          ELSE IF ( .NOT. BLANK( U.AIS ) ) THEN
            WRITE( BUFF, '(70X,2A)' ) 'AIS: ', U.AIS
            CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
          END IF
        END IF

      ELSE IF ( U.TSTSRC .EQ. 'COMPONENT DB' ) THEN ! Component info

        ! Component 1 information
        IF ( U.CMPTYP .EQ. 'VE' ) THEN
          WRITE( BUFF, '(20(''-''),A,I1,A,19(''-''))',
     &       IOSTAT=IOS )
     &       ' COMPONENT ', U.CMPNO, ' INFORMATION (Vehicle Part) '
          CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
          WRITE( BUFF, '(5A,F9.3,A)' ) 'Type: ', U.CMPTYP,
     &       '   Description: ', U.CMPDSC,
     &       '   Speed: ', U.CMPSPD, SPUNITS
          CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
          WRITE( BUFF, '(A,I4,4A)', IOSTAT=IOS ) 'Vehicle: ',
     &       U.YEAR, ' ', U.MAKE(:L_TRIM(U.MAKE)), ' ', U.MODEL
          CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
          WRITE( BUFF, '(5A,F9.3,A)', IOSTAT=IOS )
     &       'Body: ', U.BODY, ' Engine Type: ', U.ENGINE,
     &       '  Test Weight: ', U.VEHTWT, WTUNITS
          CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
        ELSE IF ( U.CMPTYP .EQ. 'OC' ) THEN
          WRITE( BUFF, '(20(''-''),A,I1,A,21(''-''))',
     &       IOSTAT=IOS )
     &       ' COMPONENT ', U.CMPNO, ' INFORMATION (Dummy Part) '
          CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
          WRITE( BUFF, '(5A,F9.3,A)' ) 'Type: ', U.CMPTYP,
     &       '   Description: ', U.CMPDSC,
     &       '   Speed: ', U.CMPSPD, SPUNITS
          CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
          WRITE( BUFF, '(5A,F9.3,A)', IOSTAT=IOS )
     &       'Occupant Type: ', U.OCCTYP, '     Dummy Size: ',
     &       U.DUMSIZ, '      Component Weight: ', U.CMPWGT, WTUNITS
          CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
        ELSE
          WRITE( BUFF, '(20(''-''),A,I1,A,34(''-''))',
     &       IOSTAT=IOS )
     &       ' COMPONENT ', U.CMPNO, ' INFORMATION '
          CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
          WRITE( BUFF, '(5A,F9.3,A)' ) 'Type: ', U.CMPTYP,
     &       '   Description: ', U.CMPDSC,
     &       '   Speed: ', U.CMPSPD, SPUNITS
          CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
          WRITE( BUFF, '(46X,A,F9.3,A)', IOSTAT=IOS )
     &       'Component Weight: ', U.CMPWGT, WTUNITS
          CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
        END IF

        ! Component 2 information
        IF ( U.CMPTYP2 .EQ. 'VE' ) THEN
          WRITE( BUFF, '(20(''-''),A,I1,A,19(''-''))',
     &       IOSTAT=IOS )
     &       ' COMPONENT ', U.CMPNO2, ' INFORMATION (Vehicle Part) '
          CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
          WRITE( BUFF, '(5A,F9.3,A)' ) 'Type: ', U.CMPTYP2,
     &       '   Description: ', U.CMPDSC2,
     &       '   Speed: ', U.CMPSPD2, SPUNITS
          CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
          WRITE( BUFF, '(A,I4,4A)', IOSTAT=IOS ) 'Vehicle: ',
     &       U.YEAR2, ' ',U.MAKE2(:L_TRIM(U.MAKE2)), ' ', U.MODEL2
          CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
          WRITE( BUFF, '(5A,F9.3,A)', IOSTAT=IOS )
     &       'Body: ', U.BODY2, ' Engine Type: ', U.ENGINE2,
     &       '  Test Weight: ', U.VEHTWT2, WTUNITS
          CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
        ELSE IF ( U.CMPTYP2 .EQ. 'OC' ) THEN
          WRITE( BUFF, '(20(''-''),A,I1,A,21(''-''))',
     &       IOSTAT=IOS )
     &       ' COMPONENT ', U.CMPNO2, ' INFORMATION (Dummy Part) '
          CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
          WRITE( BUFF, '(5A,F9.3,A)' ) 'Type: ', U.CMPTYP2,
     &       '   Description: ', U.CMPDSC2,
     &       '   Speed: ', U.CMPSPD2, SPUNITS
          CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
          WRITE( BUFF, '(5A,F9.3,A)', IOSTAT=IOS )
     &       'Occupant Type: ', U.OCCTYP2, '     Dummy Size: ',
     &       U.DUMSIZ2, '      Component Weight: ', U.CMPWGT2, WTUNITS
          CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
        ELSE IF ( .NOT. BLANK( U.CMPTYP2 ) ) THEN
          WRITE( BUFF, '(20(''-''),A,I1,A,34(''-''))',
     &       IOSTAT=IOS )
     &       ' COMPONENT ', U.CMPNO2, ' INFORMATION '
          CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
          WRITE( BUFF, '(5A,F9.3,A)' ) 'Type: ', U.CMPTYP2,
     &       '   Description: ', U.CMPDSC2,
     &       '   Speed: ', U.CMPSPD2, SPUNITS
          CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
          WRITE( BUFF, '(46X,A,F9.3,A)', IOSTAT=IOS )
     &       'Component Weight: ', U.CMPWGT2, WTUNITS
          CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
        END IF

      END IF


      ! Curve information ______________________________________________

      NAMO = LJUST( U.CURNAM )
      LN = L_TRIM( NAMO )
      NFILL = 39 - LN
      FMT = '(  20(''-''),3A,  (''-''))'
      WRITE( FMT(15:16), '(I2)', IOSTAT=IOS ) NFILL
      WRITE( BUFF, FMT ) ' CURVE ', NAMO(:LN), ' INFORMATION '
      CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
      WRITE( BUFF, '(A,I3,6A)', IOSTAT=IOS ) 'No: ', U.CURNO,
     & '  Type: ', U.CURTYP, '  Form: ', U.CHANFORM(:6),
     & ' Status: ', U.STATUS
      CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
      IF ( .NOT. BLANK( U.CURDSC ) ) THEN
        WRITE( BUFF, '(2A)' ) 'Desc: ', U.CURDSC
        CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
      END IF
      IF ( BLANK( U.SENLOC ) ) THEN
        WRITE( BUFF, '(2A,21X,2A)' )
     &     'Sensor Attachment: ', U.SENATT, ' Axis: ', U.AXIS
        CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
      ELSE
        WRITE( BUFF, '(4A,7X,2A)' )
     &     'Sensor Attachment: ', U.SENATT, '  Location: ', U.SENLOC,
     &     ' Axis: ', U.AXIS
        CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
      END IF
      IF ( ( U.SENNUM .NE. 0 ) .OR. ( U.BANDNO .NE. 0 ) .OR.
     & ( U.GAGNO .NE. 0 ) ) THEN
        WRITE( BUFF, '(3(A,I5))', IOSTAT=IOS )
     &     'Sensor No: ', U.SENNUM,
     &     '              Chest Band No: ', U.BANDNO,
     &     '   Gauge No: ', U.GAGNO
        CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
      END IF

      IF ( U.CHANFORM .EQ. 'Y' ) THEN
        WRITE( BUFF, '(4A)' ) 'Data Type: ', U.YTYP,
     &     '    Units: ', U.YUNITS
        CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
      ELSE IF ( U.CHANFORM .EQ. 'X-Y' ) THEN
        WRITE( BUFF, '(4A)' ) 'Y Type: ', U.YTYP,
     &     '    Y Units: ', U.YUNITS
        CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
        WRITE( BUFF, '(4A)' ) 'X Type: ', U.XTYP,
     &     '    X Units: ', U.XUNITS
        CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
      END IF

      WRITE( BUFF, '(A,1PG14.7,0P,A,9X,A,F9.3,A)', IOSTAT=IOS )
     & 'Initial Velocity: ', U.INIVEL, SPUNITS,
     & 'Prefilter Cutoff: ', U.PREF, ' (Hz)'
      CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
      IF ( U.FCUT .NE. 0. ) THEN
        WRITE( BUFF, '(A,3(F9.3,A))', IOSTAT=IOS )
     &     'Filter Cutoff: ', U.FCUT, ' (Hz)    Corner: ', U.FCOR,
     &     ' (Hz)    Stop: ', U.FSTP, ' (Hz)'
        CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
      END IF

      WRITE( BUFF, '(3(A,I5))', IOSTAT=IOS )
     & 'Index of First Point: ', U.NFP,
     & '     Last Point: ', U.NLP,
     & '       Number of Points: ', U.NLP - U.NFP + 1
      CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )

      ! Compute X and Y min and max values
      XMAX = U.X(U.NFP)
      XMIN = XMAX
      YMAX = U.Y(U.NFP)
      YMIN = YMAX
      DO I = U.NFP+1, U.NLP
        XMAX = MAX( XMAX, U.X(I) )
        XMIN = MIN( XMIN, U.X(I) )
        YMAX = MAX( YMAX, U.Y(I) )
        YMIN = MIN( YMIN, U.Y(I) )
      END DO

      IF ( U.CHANFORM .EQ. 'Y' ) THEN
        IF ( ( U.XTYP .EQ. 'TIME' ) .AND.
     &     ( U.XUNITS .EQ. 'SECONDS' ) ) THEN
          WRITE( BUFF, '(A,F10.3,A,F10.3,A,8X,A,F10.6,A)',
     &       IOSTAT=IOS )
     &       'Time Range: ', U.NFP * U.DEL * 1000, ' to ',
     &       U.NLP * U.DEL * 1000, ' (msec)',
     &       'Time Step: ', U.DEL * 1000, ' (msec)'
          CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
        ELSE IF ( ( U.XTYP .EQ. 'FREQUENCY' ) .AND.
     &     ( U.XUNITS .EQ. 'HZ' ) ) THEN
          WRITE( BUFF, '(A,F10.3,A,F10.3,A,12X,A,F10.6,A)',
     &       IOSTAT=IOS )
     &       'Freq Range: ', U.NFP * U.DEL, ' to ',
     &       U.NLP * U.DEL, ' (Hz)',
     &       'Freq Step: ', U.DEL, ' (Hz)'
          CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
        ELSE
          LXT = L_TRIM( U.XTYP )
          LXU = L_TRIM( U.XUNITS )
          WRITE( BUFF, '(A,F10.3,A,F10.3,A)', IOSTAT=IOS )
     &       U.XTYP(:LXT)//' Range: ', U.NFP * U.DEL, ' to ',
     &       U.NLP * U.DEL,' ('//U.XUNITS(:LXU)//')  '
          CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
          WRITE( BUFF, '(A,F12.6,A)', IOSTAT=IOS )
     &       U.XTYP(:LXT)//' Step: ', U.DEL, ' ('//U.XUNITS(:LXU)//')'
          CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
        END IF
      ELSE IF ( U.DEL .NE. 0. ) THEN
        WRITE( BUFF, '(2(A,1PG14.7),0P,1X,A,F10.6,A)',
     &     IOSTAT=IOS )
     &     'X Min:    ', XMIN, '  X Max:    ', XMAX,
     &     'Time Step: ', U.DEL * 1000, ' (msec)'
        CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
      ELSE
        WRITE( BUFF, '(2(A,1PG14.7))', IOSTAT=IOS )
     &     'X Min:    ', XMIN, '  X Max:    ', XMAX
        CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
      END IF

      IF ( U.CHANFORM .EQ. 'Y' ) THEN
        WRITE( BUFF, '(2(A,1PG14.7),1X,A,1PG14.7)', IOSTAT=IOS )
     &     'Data Min: ', YMIN, '  Data Max: ', YMAX,
     &     'Scale Factor: ', U.SCLFAC
        CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
      ELSE
        WRITE( BUFF, '(2(A,1PG14.7),1X,A,1PG14.7)', IOSTAT=IOS )
     &     'Y Min:    ', YMIN, '  Y Max:    ', YMAX,
     &     'Scale Factor: ', U.SCLFAC
        CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
      END IF


      ! Supplementary information
      SUPP_HDR = .FALSE.
      IF ( ( U.ID1 .NE. 0 ) .OR.
     & ( U.ID2 .NE. 0 ) .OR.
     & ( U.ID3 .NE. 0 ) .OR.
     & ( U.ID4 .NE. 0 ) .OR.
     & ( U.ID5 .NE. 0 ) ) THEN
        WRITE( BUFF, 601 )
        CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
        SUPP_HDR = .TRUE.
        WRITE( BUFF, '(5I15)', IOSTAT=IOS )
     &     U.ID1, U.ID2, U.ID3, U.ID4, U.ID5
        CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
      END IF
      IF ( ( ( U.RD1 .NE. 0.0 ) .AND.
     & ( ( U.RD1 .NE. 1.0 ) .OR. ( U.RD1 .NE. U.SCLFAC ) ) ) .OR.
     & ( U.RD2 .NE. 0.0 ) .OR.
     & ( U.RD3 .NE. 0.0 ) .OR.
     & ( U.RD4 .NE. 0.0 ) .OR.
     & ( U.RD5 .NE. 0.0 ) ) THEN
        IF ( .NOT. SUPP_HDR ) THEN
          WRITE( BUFF, 601 )
          CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
        END IF
        SUPP_HDR = .TRUE.
        WRITE( BUFF, '(5(1PG15.7))', IOSTAT=IOS )
     &     U.RD1, U.RD2, U.RD3, U.RD4, U.RD5
        CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
      END IF
      IF ( ( .NOT. BLANK( U.CD1 ) ) .AND.
     & ( U.CD1 .NE. 'NO COMMENTS' ) .AND.
     & ( U.CD1 .NE. 'NO COMMENTS.' ) ) THEN
        IF ( .NOT. SUPP_HDR ) THEN
          WRITE( BUFF, 601 )
          CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
        END IF
        SUPP_HDR = .TRUE.
        WRITE( BUFF, '(A)' ) U.CD1
        CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
      END IF
      IF ( ( .NOT. BLANK( U.CD2 ) ) .AND.
     & ( U.CD2 .NE. 'NO COMMENTS' ) .AND.
     & ( U.CD2 .NE. 'NO COMMENTS.' ) ) THEN
        IF ( .NOT. SUPP_HDR ) THEN
          WRITE( BUFF, 601 )
          CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
        END IF
        SUPP_HDR = .TRUE.
        WRITE( BUFF, '(A)' ) U.CD2
        CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
      END IF
  601 FORMAT(20('-'),' SUPPLEMENTARY INFORMATION ',32('-'))

      RETURN
      END



      SUBROUTINE MEAS_DUMP( DUMP_UNIT, U )

!***********************************************************************
!* Outputs Measurement Data to a User-Specified Device
!***********************************************************************

      ! Headers
      INCLUDE 'uds_str.fi'


      ! Arguments ______________________________________________________

      INTEGER DUMP_UNIT ! Logical i/o unit for output

      RECORD /UDS/  U ! UDS object


      ! Variables ______________________________________________________

      INTEGER I, IOS, J

      CHARACTER BUFF*132


      ! Data output
      WRITE( BUFF, '(79(''-''))' )
      CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
      IF ( U.CHANFORM .EQ. 'Y' ) THEN
        IF ( U.NFP .LT. 0 ) THEN
          BUFF = ' '
          CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
          WRITE( BUFF, '(A)' ) 'Pre-Zero Index Data:'
          CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
          BUFF = ' '
          CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
          DO I = U.NFP, MIN( -1, U.NLP ), 5
            WRITE( BUFF, 601 )
     &         ( U.Y(J), J = I, MIN( I+4, -1, U.NLP ) )
            CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
          END DO
        END IF
        IF ( U.NLP .GE. 0 ) THEN
          BUFF = ' '
          CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
          WRITE( BUFF, '(A)' ) 'Post-Zero Index Data:'
          CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
          BUFF = ' '
          CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
          DO I = MAX( 0, U.NFP ), U.NLP, 5
            WRITE( BUFF, 601 )
     &         ( U.Y(J), J = I, MIN( I+4, U.NLP ) )
            CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
          END DO
        END IF
      ELSE IF ( U.CHANFORM .EQ. 'X-Y' ) THEN
        IF ( U.NFP .LT. 0 ) THEN
          BUFF = ' '
          CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
          WRITE( BUFF, '(A)' ) 'Pre-Zero Index (X,Y) Data:'
          CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
          BUFF = ' '
          CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
          DO I = U.NFP, MIN( -1, U.NLP ), 2
            WRITE( BUFF, 602 )
     &         ( U.X(J), U.Y(J), J = I, MIN( I+1, -1, U.NLP ) )
            CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
          END DO
        END IF
        IF ( U.NLP .GE. 0 ) THEN
          BUFF = ' '
          CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
          WRITE( BUFF, '(A)' ) 'Post-Zero Index (X,Y) Data:'
          CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
          BUFF = ' '
          CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
          DO I = MAX( 0, U.NFP ), U.NLP, 2
            WRITE( BUFF, 602 )
     &         ( U.X(J), U.Y(J), J = I, MIN( I+1, U.NLP ) )
            CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
          END DO
        END IF
      ELSE
        WRITE( BUFF, '(/2A)' )
     &     '*** Unsupported data channel format: ', U.CHANFORM
        CALL WRITE_DEV( DUMP_UNIT, BUFF, IOS )
      END IF

  601 FORMAT(5(1PG15.7))
  602 FORMAT(2(:,'( ',1PG15.7,' , ',1PG15.7,' )  '))

      RETURN
      END
